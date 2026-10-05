import help._

import chisel3._
import circt.stage.ChiselStage

class ApbPort extends Bundle {
  val psel = Input(Bool())
  val penable = Input(Bool())
  val pwrite = Input(Bool())
  val paddr = Input(UInt(32.W))
  val pwdata = Input(UInt(32.W))
  val prdata = Output(UInt(32.W))
  val pready = Output(Bool())
  val pslverr = Output(Bool())
}

class CsrAdapter(descriptionSheetPath: String) extends Module {
  val fieldConfigs = CsrDescription.load(descriptionSheetPath)
  val apb = IO(new ApbPort)

  private def fieldPort(field: FieldConfig): Data = field.regType match {
    case "rw" => Output(UInt(field.width.W))
    case "ro" => Input(UInt(field.width.W))
    case "wotrg" => new DynamicBundle(Seq(
      "data" -> Output(UInt(field.width.W)), "trg" -> Output(Bool())
    ))
    case "rotrg" => new DynamicBundle(Seq(
      "data" -> Input(UInt(field.width.W)), "trg" -> Output(Bool())
    ))
  }

  // Preserve spreadsheet order and the block.register[.field][.data/.trg] hierarchy.
  private val connectedFields = fieldConfigs.filterNot(_.regType == "const")
  val csr = IO(new DynamicBundle(connectedFields.map(_.instName).distinct.map { instance =>
    val fields = connectedFields.filter(_.instName == instance)
    instance -> new DynamicBundle(fields.map(_.regName).distinct.map { register =>
      val registerFields = fields.filter(_.regName == register)
      val port = if (registerFields.head.fieldName.isEmpty) fieldPort(registerFields.head)
      else new DynamicBundle(registerFields.map(field => field.fieldName.get -> fieldPort(field)))
      register -> port
    })
  }))

  private def port(field: FieldConfig): Data = {
    val register = csr(field.instName).asInstanceOf[DynamicBundle](field.regName)
    field.fieldName.fold(register)(name => register.asInstanceOf[DynamicBundle](name))
  }

  // Every access completes in one access cycle. Setup and idle never change state.
  private val access = apb.psel && apb.penable && !reset.asBool
  private val readAccess = access && !apb.pwrite
  private val writeAccess = access && apb.pwrite
  apb.pready := !reset.asBool

  private val readValues = fieldConfigs.filter(_.readable).map { field =>
    val value = field.regType match {
      case "const" => field.initValue.get.U(field.width.W)
      case "ro" => port(field).asInstanceOf[UInt]
      case "rotrg" => port(field).asInstanceOf[DynamicBundle]("data").asInstanceOf[UInt]
      case "rw" => Wire(UInt(field.width.W))
    }
    field -> value
  }.toMap

  fieldConfigs.foreach { field =>
    val selected = apb.paddr === field.address.U(32.W)
    if (field.writable) {
      // '?' means unspecified state after reset, matching the Python reference.
      val value = field.initValue match {
        case Some(init) => RegInit(init.U(field.width.W))
        case None => Reg(UInt(field.width.W))
      }
      when(writeAccess && selected) {
        value := apb.pwdata(field.msb, field.lsb)
      }
      if (field.regType == "rw") {
        port(field).asInstanceOf[UInt] := value
        readValues(field) := value
      } else {
        port(field).asInstanceOf[DynamicBundle]("data") := value
      }
    }
    if (field.regType == "wotrg" || field.regType == "rotrg") {
      val direction = if (field.regType == "wotrg") writeAccess else readAccess
      port(field).asInstanceOf[DynamicBundle]("trg") := direction && selected
    }
  }

  private def matches(fields: Seq[FieldConfig]): Bool =
    fields.map(_.address).distinct.map(address => apb.paddr === address.U(32.W))
      .foldLeft(false.B)(_ || _)

  private val canRead = matches(fieldConfigs.filter(_.readable))
  private val canWrite = matches(fieldConfigs.filter(_.writable))
  apb.pslverr := access && Mux(apb.pwrite, !canWrite, !canRead)
  apb.prdata := 0.U
  fieldConfigs.filter(_.readable).groupBy(_.address).foreach { case (address, fields) =>
    val packed = fields.map { field =>
      (readValues(field).pad(32) << field.lsb)(31, 0)
    }.reduce(_ | _)
    when(readAccess && apb.paddr === address.U(32.W)) {
      apb.prdata := packed
    }
  }
}

object CsrAdapter extends App {
  val spreadsheet = args.headOption.getOrElse("soc.xlsx")
  val outputDirectory = args.lift(1).getOrElse("generated")
  ChiselStage.emitSystemVerilogFile(
    new CsrAdapter(spreadsheet),
    Array("--target-dir", outputDirectory)
  )
}
