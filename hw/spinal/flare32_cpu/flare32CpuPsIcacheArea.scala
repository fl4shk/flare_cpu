package flare32_cpu
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

case class Flare32CpuIcacheEntryPayload(
  params: Flare32CpuParams,
) extends Bundle {
  //--------
  //val valid = Bool()
  //val data = UInt(params.instrMainWidth bits)
  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
  val data = params.icacheLineMemWordType()
  //--------
}

case class Flare32CpuPipePayloadIcache(
  params: Flare32CpuParams,
) extends Bundle {
  //--------
  val instr = UInt(params.instrMainWidth bits) 
  //--------
}

case class Flare32CpuIcacheIo(
  params: Flare32CpuParams
) extends Area /*with IMasterSlave*/ {
  //val pop = /*master*/(Stream(UInt(params.instrMainWidth bits)))
  //val front = Node()
  //val frontPayload = Payload(Flare32CpuPipePayload(params=params))
  //val back = Node()
  //val backPayload = Payload(Flare32CpuPipePayload(params=params))
  val ibus = /*master*/(Bmb(p=params.ibusParams))
  val clear = /*in*/(Bool())
  //val currPayload = Payload(Flare32CpuPipePayload(params=params))

  //def asMaster(): Unit = {
  //  master(pop)
  //}
}
// for now, this is a direct-mapped instruction cache
case class Flare32CpuPsIcache(
  params: Flare32CpuParams,
  currPayload: Payload[Flare32CpuPipePayload],
  linkArr: ArrayBuffer[Link],
) extends Area {
  val io = /*master*/(Flare32CpuIcacheIo(
    params=params
  ))
  //val linkArr = PipeHelper.mkLinkArr()

  def pipeMemModStageCnt = (
    //0
    1
  )
  def pipeMemModType() = SamplePipeMemRmwModType(
    wordType=/*Flow*/(Flare32CpuIcacheEntryPayload(params=params)),
    wordCount=params.icacheLineMemWordCount,
    hazardCmpType=Bool(),
    modStageCnt=pipeMemModStageCnt,
    optReorder=false,
  )
  def pipeMemMemArrIdx = 0
  val pipeMem = PipeMemRmw[
    /*Flow[*/Flare32CpuIcacheEntryPayload/*]*/,  // WordT
    Bool,                   // HazardCmpT
    SamplePipeMemRmwModType[/*Flow[*/Flare32CpuIcacheEntryPayload/*]*/, Bool],
    SamplePipeMemRmwModType[/*Flow[*/Flare32CpuIcacheEntryPayload/*]*/, Bool],
    //PipeMemRmwDualRdTypeDisabled[Flare32CpuIcacheEntry, Bool],
  ](
    wordType=/*Flow*/(Flare32CpuIcacheEntryPayload(params=params)),
    wordCount=params.icacheLineMemWordCount,
    hazardCmpType=Bool(),
    modType=pipeMemModType(),
    modStageCnt=pipeMemModStageCnt,
    pipeName="Flare32CpuPsIcache",
    linkArr=Some(linkArr),
    memArrIdx=pipeMemMemArrIdx,
    dualRdType=(
      //PipeMemRmwDualRdTypeDisabled[Flare32CpuIcacheEntry, Bool]()
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
