import chisel3._
import chiseltest._
import help.DynamicBundle
import org.scalatest.flatspec.AnyFlatSpec

/** Compare observable behavior during legal APB transfers, not unspecified idle outputs. */
class CsrComparison extends Module {
  private val adapter = Module(new CsrAdapter("soc.xlsx"))
  private val reference = Module(new PythonSocAdapterWrapper)
  val apb = IO(new ApbPort)
  val csr = IO(chiselTypeOf(adapter.csr))
  val outputsAgree = IO(Output(Bool()))
  apb <> adapter.apb
  csr <> adapter.csr
  reference.io.psel := apb.psel
  reference.io.penable := apb.penable
  reference.io.paddr := apb.paddr
  reference.io.pwrite := apb.pwrite
  reference.io.pwdata := apb.pwdata

  private val active = apb.psel && apb.penable && !reset.asBool
  private val comparisons = adapter.fieldConfigs.filterNot(_.regType == "const").map { field =>
    val register = csr(field.instName).asInstanceOf[DynamicBundle](field.regName)
    val port = field.fieldName.fold(register)(name => register.asInstanceOf[DynamicBundle](name))
    val legacyName = (Seq(field.instName, field.regName) ++ field.fieldName.toSeq).mkString("_")
    val legacy = reference.io.elements(legacyName)
    val data = if (field.regType.endsWith("trg")) port.asInstanceOf[DynamicBundle]("data") else port
    if (!field.writable) {
      legacy := data.asTypeOf(chiselTypeOf(legacy))
    }
    if (field.regType.endsWith("trg")) {
      when(active) {
        assert(port.asInstanceOf[DynamicBundle]("trg").asUInt ===
          reference.io.elements(s"${legacyName}_trg").asUInt, s"Trigger mismatch: $legacyName")
      }
    }
    if (field.writable) data.asUInt === legacy.asUInt else true.B
  }
  outputsAgree := comparisons.reduce(_ && _)
  when(active) {
    assert(apb.pready && reference.io.pready, "Adapters must complete each access cycle")
    assert(apb.pslverr === reference.io.pslverr, "APB error response mismatch")
    when(!apb.pwrite && !apb.pslverr) {
      assert(apb.prdata === reference.io.prdata, "APB read data mismatch")
    }
  }
}

class CsrEquivalenceTest extends AnyFlatSpec with ChiselScalatestTester {
  "Chisel and Python adapters" should "agree on randomized legal APB transactions" in {
    test(new CsrComparison).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      def input(path: String): UInt = path.split('.').foldLeft(dut.csr: Data) {
        (data, name) => data.asInstanceOf[DynamicBundle](name)
      }.asInstanceOf[UInt]
      val bfm = new ApbMasterBfm(
        dut.clock, dut.reset, dut.apb.psel, dut.apb.penable, dut.apb.paddr,
        dut.apb.pwrite, dut.apb.pwdata, dut.apb.prdata, dut.apb.pready, dut.apb.pslverr
      )
      input("uart0.status.txEmpty").poke(0.U)
      input("uart0.status.rxReady").poke(0.U)
      input("uart0.data.rxData.data").poke(0.U)
      input("gpio0.dataIn").poke(0.U)
      input("gpio1.dataIn").poke(0.U)
      bfm.reset()
      // Initialize the reference's unspecified TX register before comparing outputs.
      assert(bfm.write(0x41003008L, 0x5a).nonEmpty)
      dut.outputsAgree.expect(true.B)
      val random = new scala.util.Random(2026)
      val addresses = Seq(
        0x41003000L, 0x41003004L, 0x41003008L,
        0x41004000L, 0x41004004L, 0x41004008L, 0x4100400cL,
        0x41004010L, 0x41004014L, 0x41004018L, 0x4100401cL,
        0x80000000L, 0x50000000L, 0x41003001L
      )
      // Include both access directions at every address, then randomize their order and data.
      for (_ <- 0 until 5; address <- random.shuffle(addresses)) {
        input("uart0.status.txEmpty").poke(random.nextInt(2).U)
        input("uart0.status.rxReady").poke(random.nextInt(2).U)
        input("uart0.data.rxData.data").poke(random.nextInt(256).U)
        input("gpio0.dataIn").poke(BigInt(32, random).U)
        input("gpio1.dataIn").poke(BigInt(32, random).U)
        bfm.write(address, BigInt(32, random))
        dut.outputsAgree.expect(true.B)
        bfm.read(address)
        dut.outputsAgree.expect(true.B)
      }
      bfm.reset()
      dut.outputsAgree.expect(true.B)
    }
  }
}
