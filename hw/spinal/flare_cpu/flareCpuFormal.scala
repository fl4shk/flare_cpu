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

case class FlareCpuFormalTestIoIbusIo(
  params: FlareCpuParams,
) extends Bundle {
}
case class FlareCpuFormalTestIoIbus(
  params: FlareCpuParams,
) extends Component {
  //assert(params.optFormalTest == FlareCpuParams.enumFormalTestIoIbus)
  //--------
  val io = FlareCpuIo(
    params=params,
    haveDbus=false,
  )
  //--------
  val linkArr = PipeHelper.mkLinkArr()
  //--------
  val cIf = CtrlLink(
    up=Node(),
    down=Node(),
  )
  cIf.up.valid := True
  linkArr += cIf

  val sIf = StageLink(
    up=cIf.down,
    down=Node(),
  )
  linkArr += sIf
  //--------
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
  def optFormalTest = FlareCpuParams.enumFormalTestIoIbus
  val psExSetPc = Flow(UInt(params.mainWidth bits))
  psExSetPc := (
    RegNext(psExSetPc)
    init(psExSetPc.getZero)
  )
  val psIdHaltIt = Bool()
  psIdHaltIt := False
  val cIfArea = FlareCpuPipeStageIf(
    params=params,
    cIf=cIf,
    io=io,
    psExSetPc=psExSetPc,
    psIdHaltIt=psIdHaltIt,
    optFormalTest=optFormalTest,
  ).setName("cIfArea")
  val cIdArea = FlareCpuPipeStageId(
    params=params,
    cId=cId,
    io=io,
    regFile=None,
    psIdHaltIt=psIdHaltIt,
    optFormalTest=optFormalTest,
  ).setName("cIdArea")
  GenerationFlags.formal {
    //anyseq(sId.down.valid)
    anyseq(sId.down.ready)
    //anyseq(sId.down.cancel)
  }
  //--------
  Builder(linkArr.toSeq)
  //--------
}
object FlareCpuFormalTestIoIbus extends App {
  //val params = new FlareCpuParams(
  //  optFormalTest=FlareCpuParams.enumFormalTestIoIbus
  //)
  val params = FlareCpuParams()
  case class FlareCpuTesterIoBus() extends Component {
    val dut = FormalDut(FlareCpuFormalTestIoIbus(params=params))

    val ibus = dut.io.ibus
    anyseq(ibus.ready)
    anyseq(ibus.devData)
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
    .withBMC(10)
    //.withProve(10)
    .doVerify({
      //val dut=FormalDut(FlareCpuFormalTestIoIbus(
      //  params=params,
      //))
      FlareCpuTesterIoBus()
    })
}
