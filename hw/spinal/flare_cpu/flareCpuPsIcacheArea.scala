package flare_cpu
import spinal.core._
//import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.misc.pipeline._
//import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bmb._
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
import libcheesevoyage.general.PipeHelper
import libcheesevoyage.math.LongDivPipelined

case class FlareCpuIcacheEntryPayload(
  params: FlareCpuParams,
) extends Bundle {
  //--------
  //val valid = Bool()
  //val data = UInt(params.instrMainWidth bits)
  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
  val data = params.icacheLineMemWordType()
  //--------
}

case class FlareCpuPipePayloadIcache(
  params: FlareCpuParams,
) extends Bundle {
  //--------
  val instr = UInt(params.instrMainWidth bits) 
  //--------
}

case class FlareCpuPsIcacheIo(
  params: FlareCpuParams
) extends Area /*with IMasterSlave*/ {
  //val pop = /*master*/(Stream(UInt(params.instrMainWidth bits)))
  //val front = Node()
  //val frontPayload = Payload(FlareCpuPipePayload(params=params))
  //val back = Node()
  //val backPayload = Payload(FlareCpuPipePayload(params=params))
  val ibus = /*master*/(Bmb(p=params.ibusParams))
  val clear = /*in*/(Bool())
  //val currPayload = Payload(FlareCpuPipePayload(params=params))

  //def asMaster(): Unit = {
  //  master(pop)
  //}
}
// for now, this is a direct-mapped instruction cache
case class FlareCpuPsIcache(
  params: FlareCpuParams,
  currPayload: Payload[FlareCpuPipePayload],
  linkArr: ArrayBuffer[Link],
) extends Area {
  val io = /*master*/(FlareCpuPsIcacheIo(
    params=params
  ))
  //val linkArr = PipeHelper.mkLinkArr()

  def pipeMemModStageCnt = (
    //0
    1
  )
  def pipeMemModType() = SamplePipeMemRmwModType(
    wordType=/*Flow*/(FlareCpuIcacheEntryPayload(params=params)),
    wordCount=params.icacheLineMemWordCount,
    hazardCmpType=Bool(),
    modStageCnt=pipeMemModStageCnt,
    optReorder=false,
  )
  def pipeMemMemArrIdx = 0
  val pipeMem = PipeMemRmw[
    /*Flow[*/FlareCpuIcacheEntryPayload/*]*/,  // WordT
    Bool,                   // HazardCmpT
    SamplePipeMemRmwModType[/*Flow[*/FlareCpuIcacheEntryPayload/*]*/, Bool],
    SamplePipeMemRmwModType[/*Flow[*/FlareCpuIcacheEntryPayload/*]*/, Bool],
    //PipeMemRmwDualRdTypeDisabled[FlareCpuIcacheEntry, Bool],
  ](
    wordType=/*Flow*/(FlareCpuIcacheEntryPayload(params=params)),
    wordCount=params.icacheLineMemWordCount,
    hazardCmpType=Bool(),
    modType=pipeMemModType(),
    modStageCnt=pipeMemModStageCnt,
    pipeName="FlareCpuPsIcache",
    linkArr=Some(linkArr),
    memArrIdx=pipeMemMemArrIdx,
    dualRdType=(
      //PipeMemRmwDualRdTypeDisabled[FlareCpuIcacheEntry, Bool]()
      pipeMemModType()
      // `pipeMem.io.dualRdFront` and `pipeMem.io.dualRdBack` will be
      // hooked up to the rest of the CPU pipeline
    ),
    optDualRd=(
      //false
      true
    ),
    optReorder=false,
    initBigInt=Some(
      Array.fill(params.icacheLineMemWordCount)(BigInt(0)).toSeq
    ),
    optEnableModDuplicate=(
      true
    ),
    optEnableClear=(
      //true
      false
    ),
    memRamStyle="block",
    vivadoDebug=false,
  )(
    doHazardCmpFunc=None,
    doPrevHazardCmpFunc=false,
    //doModSingleStageFunc=Some(
    //  (
    //    outp,
    //    inp,
    //  ) => {
    //  }
    //),
  )
}
