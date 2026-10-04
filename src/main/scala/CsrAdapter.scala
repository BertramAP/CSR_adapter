
import help._

import chisel3._
import chisel3.util._
// TODO: Handle the internal hardware logic with the APB interface
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

case class FieldConfig(
  instName: String,
  portName: String,
  address: BigInt,
  regType: String,
  width: Int,
  initValue: Option[BigInt]
)

class CsrAdapter(descriptionSheetPath: String) extends Module {

  val sheets = Sheet.load(descriptionSheetPath)
  // Map is sheet page that contains the list of hardware blocks that are present in the design
  val map = sheets("Map")

  println(map)
  // Stores the given registers associated with each hardware block in a hash map
  val fieldConfigs: Seq[FieldConfig] = for {
    row <- map.rows
    blockType = row(0)
    name = row(1)
    interface = row(2)
    baseAddress = BigInt(row(3).replace("0x", ""), 16)
    endAddress = BigInt(row(4).replace("0x", ""), 16)
    cacheable = row(5)
    executable = row(6)
    description = row(7)
    blockSheet = sheets(blockType)

    regRow <- blockSheet.rows
    regName = regRow(0)
    regOffset = BigInt(regRow(1).replace("0x", ""), 16)
    regField = regRow(2)
    regType = regRow(3)
    regRange = regRow(4)
    regInit = if (regRow(5).trim.isEmpty || regRow(5).trim == "?") None else Some(BigInt(regRow(5).replace("0x", "").split('.').head, 16))

  } yield {
    val portName = if (regField.trim.isEmpty) {
      s"${name}_${regName}"
    } else {
      s"${name}_${regName}_${regField}"
    }
    val width = regRange.split(":").headOption.map(_.toInt).getOrElse(0) - regRange.split(":").lastOption.map(_.toInt).getOrElse(0) + 1
    FieldConfig(name, portName, baseAddress + regOffset, regType, width, regInit)
  }

  val apb = IO(new ApbPort)
  //Dynamically load in the each row, this stores all of the IO interfaces from the sheet into a map of DynamicBundles
  /* Old code
    val elements = for { // Loop through each hardware block in the map sheet
    row <- map.rows
    blockType = row(0)
    name = row(1)
    interface = row(2)
    baseAddress = row(3)
    endAddress = row(4)
    cacheable = row(5)
    executable = row(6)
    description = row(7)
    blockSheet = sheets(blockType)
    registers = for {
      regRow <- blockSheet.rows
      regName = regRow(0)
      regOffset = regRow(1)
      regField = regRow(2)
      regType = regRow(3)
      regRange = regRow(4)
      regInit = regRow(5)
    } yield {
      (regName, regOffset, regField, regType, regRange, regInit)
    }
  } yield { 
    (name, registers)
  }
  val myBundle = new DynamicBundle(elements.map { case (name, registers) =>
    (name, new DynamicBundle(
      registers.flatMap { case (regName, regOffset, regField, regType, regRange, regInit) =>
        val portName = if (regField.trim.isEmpty) {
          s"${name}_${regName}"
        } else {
          s"${name}_${regName}_${regField}"
        }
        val width = regRange.split(":").headOption.map(_.toInt).getOrElse(0) - regRange.split(":").lastOption.map(_.toInt).getOrElse(0) + 1
        regType match {
          case "rw" => Seq(portName -> Output(UInt(width.W)))
          case "ro" => Seq(portName -> Input(UInt(width.W)))
          case "wotrg" => Seq(portName -> Output(UInt(width.W)), s"${portName}_trg" -> Output(Bool()))
          case "rotrg" => Seq(portName -> Input(UInt(width.W)), s"${portName}_trg" -> Output(Bool()))
          case "const" => Seq.empty
          case _ => throw new Exception(s"Unknown register type: $regType")
        }
      }
    ))
  })
  */
  val ioPorts = fieldConfigs.flatMap { config =>
    config.regType match {
          case "rw" => Seq(config.portName -> Output(UInt( config.width.W)))
          case "ro" => Seq(config.portName -> Input(UInt(config.width.W)))
          case "wotrg" => Seq(config.portName -> Output(UInt(config.width.W)), s"${config.portName}_trg" -> Output(Bool()))
          case "rotrg" => Seq(config.portName -> Input(UInt(config.width.W)), s"${config.portName}_trg" -> Output(Bool()))
          case "const" => Seq.empty
          case _ => throw new Exception(s"Unknown register type: $config.regType")
    }
  }

  val myBundle = new DynamicBundle(ioPorts)  
  myBundle.elements.foreach { case (name, data) =>
    println(s"$name: ${data.getWidth} bits")
  } // Just a debug print to show the allocated bits for each register field

  val block = fieldConfigs.groupBy(_.instName) // Group the field configs by their instance name to create a map of hardware blocks
  block.foreach { case (blockName, configs) =>
    println(s"Block: $blockName")
    configs.foreach { config =>
      println(s"  Port: ${config.portName}, Address: 0x${config.address.toString(16)}, Type: ${config.regType}, Width: ${config.width}, Init: ${config.initValue.getOrElse("None")}")
    }
  } // Just a debug print to show the allocated bits for each register field
  S
  // TODO: Load in the APB interface and connect the appropriate signals from myBundle
  val csr = IO(new DynamicBundle(
    Seq(sheets(map.column("Block").head).column("Register").head -> Output(UInt(32.W)))
  ))
  

  apb := DontCare
  apb.pready := 1.B
  apb.pslverr := 1.B
  csr := DontCare
}

object CsrAdapter extends App {
  emitVerilog(
    new CsrAdapter("soc.xlsx"),
    Array("--target-dir", "generated")
  )
}