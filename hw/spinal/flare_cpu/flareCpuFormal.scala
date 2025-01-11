package flare_cpu
import spinal.core._
import spinal.core.formal._
//import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.misc.pipeline._
//import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bmb._
import spinal.lib.bus.simple._
//import spinal.lib.bus.avalon._
//import spinal.lib.bus.tilelink
//import spinal.core.fiber.Fiber
import scala.collection.mutable.ArrayBuffer
import libcheesevoyage.general._
//import libcheesevoyage.general.PipeSkidBuf
//import libcheesevoyage.general.PipeSkidBufIo
////import libcheesevoyage.general.PipeSimpleDualPortMem
//import libcheesevoyage.general.FpgacpuRamSimpleDualPort
import libcheesevoyage.general.{
  PipeMemRmw, PipeMemRmwSimDut, PipeMemRmwFormal
}
import libcheesevoyage.math.LongDivMultiCycle
import libcheesevoyage.formal.LcvFormal

//case class FlareCpuFormalTestIoIbusIo(
//  cfg: FlareCpuConfig,
//) extends Bundle {
//}
//object FlareCpuFormal {
//  def mkStopAnyseqCnt
//}
object FlareCpuFormal {
  //def defaultDelayCntWidth = 8
  //def delayStopAnyseq[
  //  WordT <: Data,
  //](
  //  signal: WordT,
  //  reset: WordT,
  //  delay: UInt,
  //  cntWidth: Int=defaultDelayCntWidth,
  //) = (
  //  LcvFormal.DelayStopAnyseq(
  //    signal=signal,
  //    reset=reset,
  //    delay=delay,
  //    cntWidth=cntWidth,
  //  )
  //)
  //case class DelayStopAnyseq[
  //  WordT <: Data,
  //](
  //  signal: WordT,
  //  reset: WordT,
  //  delay: UInt,
  //  cntWidth: Int=defaultDelayCntWidth,
  //) extends Area {
  //  val rCnt = KeepAttribute(
  //    Reg(UInt(cntWidth bits))
  //    init(0x0)
  //  )
  //  when (rCnt < delay) {
  //    rCnt := rCnt + 1
  //  } otherwise {
  //    signal := reset
  //  }
  //}
  //def delayStopAnyseq(
  //  cfg: FlareCpuConfig,
  //  signal: UInt,
  //  resetValue: Int,
  //) = {
  //  delayStopAnyseq[
  //    UInt
  //  ](
  //    cfg=cfg,
  //    signal=signal,
  //    resetValue=resetValue
  //  )
  //}
}
//case class FlareCpuFormalTestMain(
//  cfg: FlareCpuConfig,
//) extends Component {
//}
//object FlareCpuFormalTestMain extends App {
//  //val cfg = new FlareCpuConfig(
//  //  optFormalTest=FlareCpuConfig.enumFormalTestMain
//  //)
//  val cfg = FlareCpuConfig()
//  case class FlareCpuTesterMain() extends Component {
//    val dut = FormalDut(FlareCpu(
//      cfg=cfg,
//      optFormalTest=FlareCpuFormalTest.Main,
//    ))
//
//    //val ibus = dut.io.ibus
//    //anyseq(ibus.ready)
//    //anyseq(ibus.devData)
//    ////assumeInitial(
//    ////  ClockDomain.isResetActive
//    ////)
//    ////when (pastValid) {
//    ////  assume(!ClockDomain.isResetActive)
//    ////}
//    ////
//    //////val rDidReset = RegNextWhen()
//    //////assumeInitial(!rDidReset)
//    ////when (RegNext(ClockDomain.isResetActive)) {
//    ////  assume(!ClockDomain.isResetActive)
//    ////}
//    //////when (ClockDomain.isResetActive) {
//    //////  assume(
//    //////    RegNext(Clo
//    //////  )
//    //////}
//    //////ClockDomain.isResetActive := RegNext(False) init(True)
//    //assumeInitial(ClockDomain.current.isResetActive)
//  }
//  new SpinalFormalConfig(
//    //.withConfig(config=SpinalConfig(
//    //),
//      _spinalConfig=SpinalConfig(
//        defaultConfigForClockDomains=ClockDomainConfig(
//          resetActiveLevel=HIGH,
//          resetKind=SYNC,
//        ),
//        formalAsserts=true,
//      ),
//      _keepDebugInfo=true
//    )
//    //.withBMC(10)
//    .withCover(10)
//    .withProve(10)
//    .doVerify({
//      //val dut=FormalDut(FlareCpuFormalTestIoIbus(
//      //  cfg=cfg,
//      //))
//      FlareCpuTesterMain()
//    })
//}
