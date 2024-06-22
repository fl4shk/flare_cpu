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
import libcheesevoyage.math.LongDivPipelined

object Flare32CpuDcacheDataSize
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    SZ8,
    SZ16,
    SZ32
    = newElement();
}
case class Flare32CpuPipePayloadDcache(
  params: Flare32CpuParams,
) extends Bundle {
  val data = UInt(params.mainWidth bits)
  val size = Flare32CpuDcacheDataSize()
  val isWr = Bool()
}

case class Flare32CpuDcacheEntryPayload(
  params: Flare32CpuParams
) extends Bundle {
  //val valid = Bool()
  val dirty = Bool()
  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
  val data = params.dcacheLineMemWordType()
}
//case class Flare32CpuDcacheIoPushPayload(
//  params: Flare32CpuParams
//) extends Bundle {
//  //val 
//}
case class Flare32CpuDcacheIo(
  params: Flare32CpuParams,
) extends Area {
  //--------
  val dbus = /*master*/(/*Stream*/(Bmb(p=params.dbusParams)))
  //val clear = /*in*/(/*Stream*/(Bool()))
  //val currPayload = Payload(Flare32CpuPipePayload(params=params))
  //--------
}
case class Flare32CpuPsDcache(
  params: Flare32CpuParams,
  prevPayload: Payload[Flare32CpuPipePayload],
  currPayload: Payload[Flare32CpuPipePayload],
  cPrevCurr: CtrlLink,
  linkArr: ArrayBuffer[Link],
) extends Area {
  //--------
  val io = Flare32CpuDcacheIo(params=params)
  //--------
  def pipeMemModStageCnt = (
    //0
    0
  )
  case class PipeMemModType(
  ) extends Bundle
    with PipeMemRmwPayloadBase[
      Flare32CpuDcacheEntryPayload,
      Flare32CpuPipePayloadExec,
    ]
  {
    val myExt = PipeMemRmwPayloadExt(
      wordType=Flare32CpuDcacheEntryPayload(params=params),
      wordCount=params.dcacheLineMemWordCount,
      hazardCmpType=Flare32CpuPipePayloadExec(params=params),
      modStageCnt=pipeMemModStageCnt,
      optEnableModDuplicate=true,
      optReorder=false,
    )
    val pipePayload = Flare32CpuPipePayload(params=params)
    def setPipeMemRmwExt(
      ext: PipeMemRmwPayloadExt[
        Flare32CpuDcacheEntryPayload,
        Flare32CpuPipePayloadExec,
      ],
      memArrIdx: Int,
    ): Unit = {
      myExt := ext
    }
    def getPipeMemRmwExt(
      ext: PipeMemRmwPayloadExt[
        Flare32CpuDcacheEntryPayload,
        Flare32CpuPipePayloadExec,
      ],
      memArrIdx: Int,
    ): Unit = {
      ext := myExt
    }
  }

  def pipeMemModType() = SamplePipeMemRmwModType(
    wordType=/*Flow*/(Flare32CpuDcacheEntryPayload(params=params)),
    wordCount=params.dcacheLineMemWordCount,
    hazardCmpType=Bool(),
    modStageCnt=pipeMemModStageCnt,
    optReorder=false,
  )
  def pipeMemMemArrIdx = 0

  val doClear = Bool()
  doClear := False // temporary!
  def validVecElemWidth = params.dcacheValidVecElemWidth
  val validVecSize = (
    (1 << params.dcacheNumLinesPow) / params.dcacheValidVecElemWidth
  )

  val rValidVec = KeepAttribute(
    Vec.fill(
      (1 << params.dcacheNumLinesPow) / validVecElemWidth
    )(
      Reg(UInt(validVecElemWidth bits)) init(0x0)
    )
    .setName("dcache_rValidVec")
  )
  val rValidClearCnt = KeepAttribute(
    (
      Reg(SInt(log2Up(rValidVec.size) + 2 bits)) init(-1)
    )
    .setName("dcache_rValidClearCnt")
  )
  val rValidClearCntPlus1 = KeepAttribute(
    (
      Reg(cloneOf(rValidClearCnt)) init(0x0)
    )
    .setName("dcache_rValidClearCntPlus1")
  )
  val pipeMem = PipeMemRmw[
    Flare32CpuDcacheEntryPayload, // WordT
    Bool,                               // HazardCmpT
    SamplePipeMemRmwModType[Flare32CpuDcacheEntryPayload, Bool],
    PipeMemRmwDualRdTypeDisabled[Flare32CpuDcacheEntryPayload, Bool],
  ](
    wordType=Flare32CpuDcacheEntryPayload(params=params),
    wordCount=params.dcacheLineMemWordCount,
    hazardCmpType=Bool(),
    modType=pipeMemModType(),
    modStageCnt=0,
    pipeName="Dcache",
    linkArr=Some(linkArr),
    memArrIdx=pipeMemMemArrIdx,
    dualRdType=PipeMemRmwDualRdTypeDisabled[
      Flare32CpuDcacheEntryPayload, Bool
    ](),
    optDualRd=false,
    optReorder=false,
    initBigInt=Some(
      Array.fill(params.dcacheLineMemWordCount)(BigInt(0)).toSeq
    ),
    optEnableModDuplicate=(
      true
    ),
    optEnableClear=(
      false,
    ),
    memRamStyle="block",
    vivadoDebug=false,
  )(
    doHazardCmpFunc=None,
    doPrevHazardCmpFunc=false,
    doModSingleStageFunc=Some(
      (
        outp,
        inp,
        cMid0Front,
      ) => {
        outp := (
          RegNext(outp) init(outp.getZero)
        )
        when (rValidClearCnt.msb) {
          when (doClear) {
            rValidClearCnt := rValidVec.size - 2
            def myCntPlus1 = rValidVec.size - 1
            rValidClearCntPlus1 := myCntPlus1
            rValidVec(myCntPlus1) := 0x0
          } otherwise { // when (!io.clear)
            when (cMid0Front.up.isFiring) {
              for (idx <- 0 until validVecElemWidth) {
                //--------
                //when (
                //  //rValidVec(inp.myExt.memAddr(validVecElemWidth))
                //  //=== cMid0Front.up(prevPayload).exec.addr
                //  //rValidVec(
                //    cMid0Front.up(prevPayload).exec.addr(
                //      params.dcacheValidVecRange
                //    )
                //  //)
                //  //=== 0
                //) {
                //}
                //--------

                //when (
                //  inp.myExt.memAddr(myRange)(
                //  === 0
                //) {
                //}
              }
              //when (
              //  rValidVec(inp.myExt.rdMemWord.baseAddr(0 downto 0)) === 0
              //) {
              //}
              //when (~rValidVec(0)) {
              //}
            }
          }
        } otherwise {
          rValidClearCnt := rValidClearCnt - 1
          rValidClearCntPlus1 := rValidClearCntPlus1 - 1
          rValidVec(rValidClearCntPlus1.asUInt) := 0x0
        }
      }
    ),
  )
}
