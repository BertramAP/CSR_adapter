import chisel3._
import chiseltest._
import help.DynamicBundle
import org.scalatest.flatspec.AnyFlatSpec

class CsrAdapterTest extends AnyFlatSpec with ChiselScalatestTester {
  private val backend = if (sys.env.get("CSR_TEST_BACKEND").contains("verilator"))
    Seq(VerilatorBackendAnnotation) else Seq.empty

  private def signal(dut: CsrAdapter, path: String): Data =
    path.split('.').foldLeft(dut.csr: Data)((data, name) => data.asInstanceOf[DynamicBundle](name))
  private def uint(dut: CsrAdapter, path: String): UInt = signal(dut, path).asInstanceOf[UInt]
  private def trigger(dut: CsrAdapter, path: String): Bool = signal(dut, path).asInstanceOf[Bool]
  private def bus(dut: CsrAdapter): ApbMasterBfm = new ApbMasterBfm(
    dut.clock, dut.reset, dut.apb.psel, dut.apb.penable, dut.apb.paddr,
    dut.apb.pwrite, dut.apb.pwdata, dut.apb.prdata, dut.apb.pready, dut.apb.pslverr
  )
  private def fixtureInputs(dut: CsrAdapter): Unit = {
    for (block <- Seq("block0", "block1")) {
      uint(dut, s"$block.mixed.status").poke(5.U)
      uint(dut, s"$block.receive.data").poke(0xbc.U)
    }
  }

  "Chisel CSR adapter" should "implement the supplied SoC map and independent GPIO instances" in {
    test(new CsrAdapter("soc.xlsx")).withAnnotations(backend) { dut =>
      val bfm = bus(dut)
      uint(dut, "uart0.status.txEmpty").poke(1.U)
      uint(dut, "uart0.status.rxReady").poke(0.U)
      uint(dut, "uart0.data.rxData.data").poke(0xaa.U)
      uint(dut, "gpio0.dataIn").poke("hdeadbeef".U)
      uint(dut, "gpio1.dataIn").poke("h12345678".U)
      bfm.reset()
      assert(!dut.csr.elements.contains("sysInfo"), "Constants must not create block ports")
      bfm.readExpect(0x80000000L, Some(0xdeadbeefL))
      bfm.readExpect(0x41003000L, Some(0))
      bfm.readExpect(0x41003004L, Some(1))
      bfm.readExpect(0x41003008L, Some(0xaa))
      bfm.readExpect(0x41004008L, Some(0xdeadbeefL))
      bfm.readExpect(0x41004018L, Some(0x12345678L))
      assert(bfm.write(0x41003000L, 0xdeadbeefL).nonEmpty)
      bfm.readExpect(0x41003000L, Some(3))
      uint(dut, "uart0.ctrl.en").expect(1.U)
      uint(dut, "uart0.ctrl.loopback").expect(1.U)
      uint(dut, "uart0.status.rxReady").poke(1.U)
      bfm.readExpect(0x41003004L, Some(3))

      val random = new scala.util.Random(42)
      for (_ <- 0 until 40) {
        val first = BigInt(32, random)
        val second = BigInt(32, random)
        assert(bfm.write(0x4100400cL, first).nonEmpty)
        assert(bfm.write(0x4100401cL, second).nonEmpty)
        bfm.readExpect(0x4100400cL, Some(first))
        bfm.readExpect(0x4100401cL, Some(second))
        uint(dut, "gpio0.dataOut").expect(first.U)
        uint(dut, "gpio1.dataOut").expect(second.U)
      }
      for (address <- Seq(0x50000000L, 0x41003001L, 0x4100300cL)) {
        bfm.readExpect(address, None)
        assert(bfm.write(address, 1).isEmpty)
      }
      for (address <- Seq(0x41003004L, 0x41004008L, 0x80000000L)) {
        assert(bfm.write(address, 0).isEmpty)
      }
      bfm.readExpect(0x80000000L, Some(0xdeadbeefL))
      bfm.reset()
      bfm.readExpect(0x41003000L, Some(0))
      bfm.readExpect(0x4100400cL, Some(0))
      bfm.readExpect(0x4100401cL, Some(0))
    }
  }

  it should "pack mixed fields, parse decimal reset values, and enforce access permissions" in {
    CsrTestFixture.withWorkbook { path =>
      test(new CsrAdapter(path)).withAnnotations(backend) { dut =>
        fixtureInputs(dut)
        val bfm = bus(dut)
        bfm.reset()
        bfm.readExpect(0x1000, Some(0xa00500a0L))
        bfm.readExpect(0x1014, Some(7))
        uint(dut, "block0.send.data").expect(0x12.U)
        assert(bfm.write(0x1000, 0xffffffffL).nonEmpty)
        bfm.readExpect(0x1000, Some(0xa0050ff0L))
        bfm.readExpect(0x2000, Some(0xa00500a0L))
        uint(dut, "block0.mixed.control").expect(255.U)
        bfm.readExpect(0x1008, Some(0x00bc0000))
        bfm.readExpect(0x100c, Some(0xcafebabeL))
        bfm.readExpect(0x1004, None)
        assert(bfm.write(0x1008, 0).isEmpty)
        assert(bfm.write(0x100c, 0).isEmpty)
        assert(bfm.write(0x1010, 0x87654321L).nonEmpty)
        bfm.readExpect(0x1010, Some(0x87654321L))
        assert(bfm.write(0x1004, 0xffffabffL).nonEmpty)
        uint(dut, "block0.send.data").expect(0xab.U)
        uint(dut, "block1.send.data").expect(0x12.U)
        bfm.reset()
        bfm.readExpect(0x1000, Some(0xa00500a0L))
        uint(dut, "block0.send.data").expect(0x12.U)
      }
    }
  }

  it should "gate triggers and writes to access cycles including back-to-back transfers and reset" in {
    CsrTestFixture.withWorkbook { path =>
      test(new CsrAdapter(path)).withAnnotations(backend) { dut =>
        fixtureInputs(dut)
        bus(dut).reset()
        val writeTrigger = trigger(dut, "block0.send.trg")
        val readTrigger = trigger(dut, "block0.receive.trg")
        def noTriggers(): Unit = {
          writeTrigger.expect(false.B)
          readTrigger.expect(false.B)
          trigger(dut, "block1.send.trg").expect(false.B)
          trigger(dut, "block1.receive.trg").expect(false.B)
        }
        dut.apb.paddr.poke(0x1004.U)
        dut.apb.pwdata.poke(0x5500.U)
        dut.apb.pwrite.poke(true.B)
        // Idle and setup must have no side effects, even when held for multiple cycles.
        dut.apb.penable.poke(true.B)
        dut.clock.step(2)
        noTriggers()
        uint(dut, "block0.send.data").expect(0x12.U)
        dut.apb.psel.poke(true.B)
        dut.apb.penable.poke(false.B)
        dut.clock.step(3)
        noTriggers()
        uint(dut, "block0.send.data").expect(0x12.U)
        dut.apb.penable.poke(true.B)
        dut.apb.pready.expect(true.B)
        dut.apb.pslverr.expect(false.B)
        writeTrigger.expect(true.B)
        readTrigger.expect(false.B)
        dut.clock.step()
        uint(dut, "block0.send.data").expect(0x55.U)

        // Keep psel high and insert the required setup phase for the next transfer.
        dut.apb.penable.poke(false.B)
        dut.apb.paddr.poke(0x1008.U)
        dut.apb.pwrite.poke(false.B)
        noTriggers()
        dut.clock.step()
        dut.apb.penable.poke(true.B)
        readTrigger.expect(true.B)
        writeTrigger.expect(false.B)
        dut.apb.prdata.expect(0x00bc0000.U)
        dut.apb.pslverr.expect(false.B)
        dut.clock.step()
        dut.apb.psel.poke(false.B)
        dut.apb.penable.poke(false.B)
        noTriggers()
        dut.apb.pslverr.expect(false.B)

        // Abort a setup phase without asserting penable.
        dut.apb.psel.poke(true.B)
        dut.apb.paddr.poke(0x1004.U)
        dut.apb.pwdata.poke(0xaa00.U)
        dut.apb.pwrite.poke(true.B)
        dut.clock.step()
        dut.apb.psel.poke(false.B)
        dut.clock.step()
        uint(dut, "block0.send.data").expect(0x55.U)
        noTriggers()

        // Reset suppresses an otherwise valid write and restores specified values.
        dut.apb.psel.poke(true.B)
        dut.apb.penable.poke(true.B)
        dut.reset.poke(true.B)
        noTriggers()
        dut.apb.pready.expect(false.B)
        dut.clock.step()
        uint(dut, "block0.send.data").expect(0x12.U)
        dut.apb.psel.poke(false.B)
        dut.apb.penable.poke(false.B)
        dut.reset.poke(false.B)
        noTriggers()
      }
    }
  }
}
