import help.Sheet

case class FieldConfig(
  instName: String,
  regName: String,
  fieldName: Option[String],
  address: BigInt,
  regType: String,
  msb: Int,
  lsb: Int,
  initValue: Option[BigInt]
) {
  def width: Int = msb - lsb + 1
  def readable: Boolean = regType != "wotrg"
  def writable: Boolean = regType == "rw" || regType == "wotrg"
}

object CsrDescription {
  private val types = Set("rw", "ro", "wotrg", "rotrg", "const")

  // POI renders numeric Excel cells as decimal strings such as "10.0".
  def number(text: String): BigInt = {
    val value = text.trim
    if (value.toLowerCase.startsWith("0x")) BigInt(value.drop(2), 16)
    else BigDecimal(value).toBigIntExact.getOrElse(
      throw new IllegalArgumentException(s"Expected an integer, got '$text'")
    )
  }

  private def identifier(value: String): String = {
    require(value.matches("[A-Za-z_][A-Za-z0-9_]*"), s"Invalid CSR name '$value'")
    value
  }

  def load(path: String): Seq[FieldConfig] = parse(Sheet.load(path))

  def parse(sheets: Map[String, Sheet]): Seq[FieldConfig] = {
    def records(sheet: Sheet): Seq[Map[String, String]] =
      sheet.rows.map(row => sheet.header.zip(row.map(_.trim)).toMap)

    val instances = records(sheets("Map"))
    require(instances.map(_("Name")).distinct.size == instances.size, "Duplicate block instance name")
    val fields = instances.flatMap { instance =>
      val name = identifier(instance("Name"))
      val base = number(instance("Base Address"))
      val end = number(instance("End Address"))
      require(base >= 0 && base <= end && end < (BigInt(1) << 32), s"Invalid address range for $name")
      records(sheets(instance("Block"))).map { row =>
        val register = identifier(row("Register"))
        val fieldName = Option(row("Field")).filter(_.nonEmpty).map(identifier)
        val typ = row("Type")
        require(types(typ), s"Unknown register type '$typ' in $name.$register")
        val range = row("Range").split(":")
        require(range.length == 2, s"Invalid bit range '${row("Range")}'")
        val msb = range(0).trim.toInt
        val lsb = range(1).trim.toInt
        require(lsb >= 0 && msb >= lsb && msb < 32, s"Invalid bit range '${row("Range")}'")
        val offset = number(row("Offset"))
        val address = base + offset
        require(offset >= 0 && address % 4 == 0 && address + 3 <= end,
          s"Register $name.$register is unaligned or outside its block address range")
        val init = Option(row("Init")).filter(v => v.nonEmpty && v != "?").map(number)
        require(typ != "const" || init.nonEmpty, s"Constant $name.$register needs an initial value")
        require(init.forall(v => v >= 0 && v < (BigInt(1) << (msb - lsb + 1))),
          s"Initial value does not fit $name.$register")
        FieldConfig(name, register, fieldName, address, typ, msb, lsb, init)
      }
    }

    fields.groupBy(f => (f.instName, f.regName)).foreach { case (name, registerFields) =>
      require(registerFields.map(_.address).distinct.size == 1, s"Inconsistent offsets in $name")
      require(registerFields.map(_.fieldName).distinct.size == registerFields.size, s"Duplicate fields in $name")
      require(registerFields.size == 1 || registerFields.forall(_.fieldName.nonEmpty),
        s"Cannot mix whole-register and named fields in $name")
    }
    fields.groupBy(_.address).foreach { case (address, registerFields) =>
      require(registerFields.map(f => (f.instName, f.regName)).distinct.size == 1,
        s"Multiple registers mapped to 0x${address.toString(16)}")
      // Read and write views may overlap, e.g. UART RX and TX at the same address.
      for (view <- Seq(registerFields.filter(_.readable), registerFields.filter(_.writable))) {
        val bits = view.flatMap(f => f.lsb to f.msb)
        require(bits.distinct.size == bits.size, s"Overlapping fields at 0x${address.toString(16)}")
      }
    }
    fields
  }
}
