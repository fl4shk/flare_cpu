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
import libcheesevoyage.general.PipeMemRmw
import libcheesevoyage.general.PipeMemRmwSimDut
import libcheesevoyage.math.LongDivMultiCycle
import libcheesevoyage.formal.LcvFormal

//case class FlareCpuFormalTestIoIbusIo(
//  params: FlareCpuParams,
//) extends Bundle {
//}
//object FlareCpuFormal {
//  def mkStopAnyseqCnt
//}
object FlareCpuFormal {
  def defaultDelayCntWidth = 8
  def delayStopAnyseq[
    WordT <: Data,
  ](
    signal: WordT,
    reset: WordT,
    delay: UInt,
    cntWidth: Int=defaultDelayCntWidth,
  ) = (
    LcvFormal.DelayStopAnyseq(
      signal=signal,
      reset=reset,
      delay=delay,
      cntWidth=cntWidth,
    )
  )
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
  //  params: FlareCpuParams,
  //  signal: UInt,
  //  resetValue: Int,
  //) = {
  //  delayStopAnyseq[
  //    UInt
  //  ](
  //    params=params,
  //    signal=signal,
  //    resetValue=resetValue
  //  )
  //}
}
case class FlareCpuFormalTestIoIbus(
  params: FlareCpuParams,
) extends Component {
  //assert(params.optFormalTest == FlareCpuParams.enumFormalTestMain)
  //--------
  val io = FlareCpuIo(
    params=params,
    haveDbus=false,
  )
  //--------
  def optFormalTestNoJumps = true
  def optFormalTest = FlareCpuParams.enumFormalTestMain
  //--------
  val linkArr = PipeHelper.mkLinkArr()
  //--------
  val pIf = Payload(FlareCpuPipeMemModExtType(
    params=params,
    optFormalTest=optFormalTest,
  ))
  val cIf = CtrlLink(
    up=Node(),
    down=Node(),
  )
  linkArr += cIf

  val sIf = StageLink(
    up=cIf.down,
    down=Node(),
  )
  linkArr += sIf
  //--------
  //val pId = Payload(FlareCpuPipeMemModExtType(params=params))
  //val pId = Payload(FlareCpuPipeMemModExtType(
  //  params=params,
  //  optFormalTest=optFormalTest,
  //))
  val cId = CtrlLink(
    up=sIf.down,
    down=Node(),
  )
  linkArr += cId
  val sId = StageLink(
    up=cId.down,
    down=Node(),
  )
  linkArr += sId
  //--------
  val psExSetPc = Flow(
    UInt(params.mainWidth bits)
    //FlareCpuPsExSetPcPayload(
    //  params=params,
    //  optFormalTest=optFormalTest,
    //)
  )
  if (optFormalTestNoJumps) {
    psExSetPc := (
      RegNext(psExSetPc)
      init(psExSetPc.getZero)
    )
  } else { // if (!optFormalTestNoJumps)
    anyseq(psExSetPc)
  }
  //when (
  //  psExSetPc.fire
  //) {
  //  assert(
  //    
  //  )
  //}
  val psIdHaltIt = Bool()
  psIdHaltIt := False
  cIf.up.valid := True
  val cIfArea = FlareCpuPipeStageIf(
    params=params,
    cIf=cIf,
    pIf=pIf,
    io=io,
    psIdHaltIt=psIdHaltIt,
    psExSetPc=psExSetPc,
    optFormalTestNoJumps=optFormalTestNoJumps,
    optFormalTest=optFormalTest,
  ).setName("cIfArea")
  val cIdArea = FlareCpuPipeStageId(
    params=params,
    cId=cId,
    pIf=pIf,
    //pId=pId,
    //pId=pId,
    io=io,
    regFile=None,
    psIdHaltIt=psIdHaltIt,
    psExSetPc=psExSetPc,
    optFormalTest=optFormalTest,
  ).setName("cIdArea")
  //val rSIdDownReadyCnt = Reg(UInt(8 bits)) init(0x0)
  //if (params.formal) {
  //  //anyseq(sId.down.valid)
  //  //when (rSIdDownReadyCnt < 32) {
  //  //  anyseq(sId.down.ready)
  //  //  rSIdDownReadyCnt
  //  //} otherwise {
  //  //}
  //  //anyseq(sId.down.cancel)
  //}
  anyseq(sId.down.ready)
  //val rSIdDownReadyDelayStopAnyseq = (params.formal) generate (
  //  FlareCpuFormal.delayStopAnyseq(
  //    signal=sId.down.ready,
  //    reset=True,
  //    delay=8,
  //  )
  //)
  //--------
  Builder(linkArr.toSeq)
  //--------
}
object FlareCpuFormalTestIoIbus extends App {
  //val params = new FlareCpuParams(
  //  optFormalTest=FlareCpuParams.enumFormalTestMain
  //)
  val params = FlareCpuParams()
  case class FlareCpuTesterIoIbus() extends Component {
    val dut = FormalDut(FlareCpuFormalTestIoIbus(params=params))

    val ibus = dut.io.ibus
    anyseq(ibus.ready)
    anyseq(ibus.devData)
    //assumeInitial(
    //  ClockDomain.isResetActive
    //)
    //when (pastValid) {
    //  assume(!ClockDomain.isResetActive)
    //}
    //
    ////val rDidReset = RegNextWhen()
    ////assumeInitial(!rDidReset)
    //when (RegNext(ClockDomain.isResetActive)) {
    //  assume(!ClockDomain.isResetActive)
    //}
    ////when (ClockDomain.isResetActive) {
    ////  assume(
    ////    RegNext(Clo
    ////  )
    ////}
    ////ClockDomain.isResetActive := RegNext(False) init(True)
    assumeInitial(ClockDomain.current.isResetActive)
  }
  new SpinalFormalConfig(
    //.withConfig(config=SpinalConfig(
    //),
      _spinalConfig=SpinalConfig(
        defaultConfigForClockDomains=ClockDomainConfig(
          resetActiveLevel=HIGH,
          resetKind=SYNC,
        ),
        formalAsserts=true,
      ),
      _keepDebugInfo=true
    )
    //.withBMC(10)
    .withCover(10)
    .withProve(10)
    .doVerify({
      //val dut=FormalDut(FlareCpuFormalTestIoIbus(
      //  params=params,
      //))
      FlareCpuTesterIoIbus()
    })
}
