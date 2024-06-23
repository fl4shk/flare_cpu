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
object Flare32CpuPipePayloadDcache {
  def apply(
    params: Flare32CpuParams
  ) = (
    Flow(Flare32CpuPipePayloadFlowPayloadDcache(params=params))
  )
}

case class Flare32CpuPipePayloadFlowPayloadDcache(
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
case class Flare32CpuPsDcacheIo(
  params: Flare32CpuParams,
  //optMainMemBram: Boolean,
) extends Area {
  //--------
  val dbus = /*(!optMainMemBram) generate*/ (
    /*master*/(/*Stream*/(Bmb(p=params.dbusParams)))
  )
  //val dBramBus = (optMainMemBram) generate (
  //)
  //val clear = /*in*/(/*Stream*/(Bool()))
  //val currPayload = Payload(Flare32CpuPipePayload(params=params))
  //--------
}
case class Flare32CpuPsDcache(
  params: Flare32CpuParams,
  prevPayload: Payload[Flare32CpuPipePayload],
  currPayload: Payload[Flare32CpuPipePayload],
  //cPrevCurr: CtrlLink,
  linkArr: ArrayBuffer[Link],
  //optMainMemBram: Boolean=true,
) extends Area {
  //--------
  val io = Flare32CpuPsDcacheIo(
    params=params,
    //optMainMemBram=optMainMemBram,
  )
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

  //def pipeMemModType() = SamplePipeMemRmwModType(
  //  wordType=/*Flow*/(Flare32CpuDcacheEntryPayload(params=params)),
  //  wordCount=params.dcacheLineMemWordCount,
  //  hazardCmpType=Bool(),
  //  modStageCnt=pipeMemModStageCnt,
  //  optReorder=false,
  //)
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
      Reg(UInt(validVecElemWidth bits))
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
  object State extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      CHECK_FOR_DCACHE_HIT,
      //CMD_READ_INIT,
      //BMB_CACHED_CMD_READ_WAIT_READY,
      //BMB_CACHED_CMD_WRITE_WAIT_READY,
      //BMB_NONCACHED_READ_CMD_WAIT_READY,
      //BMB_NONCACHED_WRITE_CMD_WAIT_READY
      DBUS_BMB_CMD_WAIT_READY,
      DBUS_BMB_RSP_WAIT_VALID
      //BMB_RSP_DEASSERT_READY
      //RD_NONCACHED_BMB_CMD_WAIT_READY,
      //WR_NONCACHED_BMB_CMD_WAIT_READY
      //CMD_WRITE_INIT
      = newElement();
  }
  val nextState = State()
  val rState = RegNext(nextState) init(State.CHECK_FOR_DCACHE_HIT)

  val pipeMem = PipeMemRmw[
    Flare32CpuDcacheEntryPayload, // WordT
    Flare32CpuPipePayloadExec,                               // HazardCmpT
    PipeMemModType,
    PipeMemRmwDualRdTypeDisabled[
      Flare32CpuDcacheEntryPayload,
      Flare32CpuPipePayloadExec,
    ],
  ](
    wordType=Flare32CpuDcacheEntryPayload(params=params),
    wordCount=params.dcacheLineMemWordCount,
    hazardCmpType=Flare32CpuPipePayloadExec(params=params),
    modType=PipeMemModType(),
    modStageCnt=0,
    pipeName="Dcache",
    linkArr=Some(linkArr),
    memArrIdx=pipeMemMemArrIdx,
    dualRdType=PipeMemRmwDualRdTypeDisabled[
      Flare32CpuDcacheEntryPayload, Flare32CpuPipePayloadExec
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
        when (cMid0Front.up.isValid) {
          switch (rState) {
            is (State.CHECK_FOR_DCACHE_HIT) {
              val memAddr = inp.myExt.memAddr 
            }
            is (State.DBUS_BMB_CMD_WAIT_READY) {
            }
            is (State.DBUS_BMB_RSP_WAIT_VALID) {
            }
            //is (State.BMB_RSP_DEASSERT_READY) {
            //}
          }
        }
        //when (rValidClearCnt.msb) {
        //  when (doClear) {
        //    rValidClearCnt := rValidVec.size - 2
        //    def myCntPlus1 = rValidVec.size - 1
        //    rValidClearCntPlus1 := myCntPlus1
        //    //rValidVec(myCntPlus1) := U(validVecElemWidth, U"1'b0")
        //    rValidVec(myCntPlus1) := U"1'b0".resized
        //  } otherwise { // when (!doClear)
        //    when (cMid0Front.up.isValid) {
        //      switch (rState) {
        //        
        //      }
        //      //for (idx <- 0 until validVecElemWidth) {
        //      //  //--------
        //      //  //when (
        //      //  //  //rValidVec(inp.myExt.memAddr(validVecElemWidth))
        //      //  //  //=== cMid0Front.up(prevPayload).exec.addr
        //      //  //  //rValidVec(
        //      //  //    cMid0Front.up(prevPayload).exec.addr(
        //      //  //      params.dcacheValidVecRange
        //      //  //    )
        //      //  //  //)
        //      //  //  //=== 0
        //      //  //) {
        //      //  //}
        //      //  //--------

        //      //  //when (
        //      //  //  inp.myExt.memAddr(myRange)(
        //      //  //  === 0
        //      //  //) {
        //      //  //}
        //      //}
        //      //when (
        //      //  rValidVec(inp.myExt.rdMemWord.baseAddr(0 downto 0)) === 0
        //      //) {
        //      //}
        //      //when (~rValidVec(0)) {
        //      //}
        //    }
        //  }
        //} otherwise {
        //  rValidClearCnt := rValidClearCnt - 1
        //  rValidClearCntPlus1 := rValidClearCntPlus1 - 1
        //  //rValidVec(rValidClearCntPlus1.asUInt.resized) := (
        //  //  (params.dcacheValidVecElemWidth bits, 0x0)
        //  //)
        //  rValidVec(rValidClearCntPlus1.asUInt.resized) := U"1'b0".resized
        //}
      }
    ),
  )
}
