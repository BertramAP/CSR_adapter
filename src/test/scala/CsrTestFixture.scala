import java.nio.file.Files
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import scala.util.Using

object CsrTestFixture {
  val registerHeader = Seq("Register", "Offset", "Field", "Type", "Range", "Init")
  val mapHeader = Seq("Block", "Name", "Interface", "Base Address", "End Address")
  val mapRows = Seq(
    Seq("Example", "block0", "APB", "0x1000", "0x10ff"),
    Seq("Example", "block1", "APB", "0x2000", "0x20ff")
  )
  val rows = Seq(
    Seq("mixed", "0x0", "control", "rw", "11:4", "10"),
    Seq("mixed", "0x0", "status", "ro", "19:16", "?"),
    Seq("mixed", "0x0", "version", "const", "31:28", "0xa"),
    Seq("send", "0x4", "", "wotrg", "15:8", "0x12"),
    Seq("receive", "0x8", "", "rotrg", "23:16", "?"),
    Seq("constant", "0xc", "", "const", "31:0", "0xcafebabe"),
    Seq("scratch", "0x10", "", "rw", "31:0", "?"),
    Seq("fixed", "0x14", "", "rw", "7:0", "0x7")
  )

  // Generate a real workbook so numeric-cell parsing is exercised end to end.
  def withWorkbook[A](body: String => A): A = {
    val path = Files.createTempFile("csr-test-", ".xlsx")
    try {
      Using.resource(new XSSFWorkbook()) { workbook =>
        for ((name, header, data) <- Seq(
          ("Map", mapHeader, mapRows), ("Example", registerHeader, rows)
        )) {
          val sheet = workbook.createSheet(name)
          (header +: data).zipWithIndex.foreach { case (values, rowIndex) =>
            val row = sheet.createRow(rowIndex)
            values.zipWithIndex.foreach { case (value, column) =>
              val cell = row.createCell(column)
              if (value == "10") cell.setCellValue(10.0)
              else cell.setCellValue(value)
            }
          }
        }
        Using.resource(Files.newOutputStream(path))(workbook.write)
      }
      body(path.toString)
    } finally Files.deleteIfExists(path)
  }
}
