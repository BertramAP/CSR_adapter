
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

class CsrAdapter(descriptionSheetPath: String) extends Module {

  val sheets = Sheet.load(descriptionSheetPath)
  val map = sheets("Map")

  println(map)
  // Stores the given registers associated with each hardware block in a hash map
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
  val apb = IO(new ApbPort)
  //Dynamically load in the each row, this stores all of the IO interfaces from the sheet into a map of DynamicBundles
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

  myBundle.elements.foreach { case (name, data) =>
    println(s"$name: ${data.getWidth} bits")
  }

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