import help.Sheet
import org.scalatest.flatspec.AnyFlatSpec

class CsrDescriptionTest extends AnyFlatSpec {
  private def description(rows: Seq[Seq[String]]): Map[String, Sheet] = Map(
    "Map" -> new Sheet(CsrTestFixture.mapHeader, CsrTestFixture.mapRows),
    "Example" -> new Sheet(CsrTestFixture.registerHeader, rows)
  )

  "CSR description" should "read decimal and hexadecimal integers without losing field positions" in {
    assert(CsrDescription.number("10.0") == 10)
    assert(CsrDescription.number("10") == 10)
    assert(CsrDescription.number("0x10") == 16)
    assert(CsrDescription.number("0XDEADBEEF") == BigInt("deadbeef", 16))
    intercept[IllegalArgumentException](CsrDescription.number("1.5"))
    CsrTestFixture.withWorkbook { path =>
      val fields = CsrDescription.load(path)
      val control = fields.find(f => f.instName == "block0" && f.fieldName.contains("control")).get
      assert(control.initValue.contains(BigInt(10)))
      assert(control.lsb == 4 && control.msb == 11 && control.width == 8)
      assert(fields.exists(f => f.instName == "block1" && f.address == 0x2000))
      assert(fields.find(_.regName == "scratch").get.initValue.isEmpty)
    }
  }

  it should "allow separate read and write views of the same bits" in {
    val fields = CsrDescription.load("soc.xlsx")
    val data = fields.filter(f => f.instName == "uart0" && f.regName == "data")
    assert(data.map(_.regType).toSet == Set("wotrg", "rotrg"))
    assert(data.map(_.address).distinct.size == 1)
  }

  private val invalidRows = Seq(
    "unknown types" -> Seq("bad", "0x0", "", "invalid", "7:0", "0"),
    "reversed ranges" -> Seq("bad", "0x0", "", "rw", "0:7", "0"),
    "oversized ranges" -> Seq("bad", "0x0", "", "rw", "32:0", "0"),
    "overflowing reset values" -> Seq("bad", "0x0", "", "rw", "7:0", "256"),
    "unspecified constants" -> Seq("bad", "0x0", "", "const", "7:0", "?"),
    "unaligned addresses" -> Seq("bad", "0x1", "", "rw", "7:0", "0"),
    "out-of-range addresses" -> Seq("bad", "0x100", "", "rw", "7:0", "0")
  )
  for ((name, row) <- invalidRows) {
    it should s"reject $name" in {
      intercept[IllegalArgumentException](CsrDescription.parse(description(Seq(row))))
    }
  }

  it should "reject overlapping readable fields and duplicate port names" in {
    val first = Seq("mixed", "0x0", "a", "rw", "7:0", "0")
    val overlap = Seq("mixed", "0x0", "b", "ro", "11:4", "?")
    intercept[IllegalArgumentException](CsrDescription.parse(description(Seq(first, overlap))))
    intercept[IllegalArgumentException](CsrDescription.parse(description(Seq(first, first))))
  }
}
