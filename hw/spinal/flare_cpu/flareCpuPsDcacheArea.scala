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
import libcheesevoyage.math.LongDivPipelined

object FlareCpuDcacheDataSize
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    SZ8,
    SZ16,
    SZ32
    = newElement();
}
object FlareCpuPipePayloadDcache {
  def apply(
    params: FlareCpuParams
  ) = (
    Flow(FlareCpuPipePayloadFlowPayloadDcache(params=params))
  )
}

case class FlareCpuPipePayloadFlowPayloadDcache(
  params: FlareCpuParams,
) extends Bundle {
  val data = UInt(params.mainWidth bits)
  val size = FlareCpuDcacheDataSize()
  val isWr = Bool()
}

case class FlareCpuDcacheEntryPayload(
  params: FlareCpuParams
) extends Bundle {
  //val valid = Bool()
  //val dirty = Bool()
  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
  val data = params.dcacheLineMemWordType()
}
//case class FlareCpuDcacheIoPushPayload(
//  params: FlareCpuParams
//) extends Bundle {
//  //val 
//}
case class FlareCpuPsDcacheIo(
  params: FlareCpuParams,
  //optMainMemBram: Boolean,
) extends Area {
  //--------
  val dbus = /*(!optMainMemBram) generate*/ (
    /*master*/(/*Stream*/(Bmb(p=params.dbusParams)))
  )
  //val dBramBus = (optMainMemBram) generate (
  //)
  //val clear = /*in*/(/*Stream*/(Bool()))
  //val currPayload = Payload(FlareCpuPipePayload(params=params))
  //--------
}
case class FlareCpuPsDcache(
  params: FlareCpuParams,
  prevPayload: Payload[FlareCpuPipePayload],
  currPayload: Payload[FlareCpuPipePayload],
  //cPrevCurr: CtrlLink,
  linkArr: ArrayBuffer[Link],
  //optMainMemBram: Boolean=true,
) extends Area {
  //--------
  val io = FlareCpuPsDcacheIo(
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
      FlareCpuDcacheEntryPayload,
      FlareCpuPipePayloadExec,
    ]
  {
    val myExt = PipeMemRmwPayloadExt(
      wordType=FlareCpuDcacheEntryPayload(params=params),
      wordCount=params.dcacheLineMemWordCount,
      hazardCmpType=FlareCpuPipePayloadExec(params=params),
      modStageCnt=pipeMemModStageCnt,
      optEnableModDuplicate=true,
      optReorder=false,
    )
    val pipePayload = FlareCpuPipePayload(params=params)
    def setPipeMemRmwExt(
      ext: PipeMemRmwPayloadExt[
        FlareCpuDcacheEntryPayload,
        FlareCpuPipePayloadExec,
      ],
      memArrIdx: Int,
    ): Unit = {
      myExt := ext
    }
    def getPipeMemRmwExt(
      ext: PipeMemRmwPayloadExt[
        FlareCpuDcacheEntryPayload,
        FlareCpuPipePayloadExec,
      ],
      memArrIdx: Int,
    ): Unit = {
      ext := myExt
    }
  }

  //def pipeMemModType() = SamplePipeMemRmwModType(
  //  wordType=/*Flow*/(FlareCpuDcacheEntryPayload(params=params)),
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
      MAYBE_NO_BMB_ACCESS,
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
  val rState = RegNext(nextState) init(State.MAYBE_NO_BMB_ACCESS)

  val pipeMem = PipeMemRmw[
    FlareCpuDcacheEntryPayload, // WordT
    FlareCpuPipePayloadExec,    // HazardCmpT
    PipeMemModType,
    PipeMemRmwDualRdTypeDisabled[
      FlareCpuDcacheEntryPayload,
      FlareCpuPipePayloadExec,
    ],
  ](
    wordType=FlareCpuDcacheEntryPayload(params=params),
    wordCount=params.dcacheLineMemWordCount,
    hazardCmpType=FlareCpuPipePayloadExec(params=params),
    modType=PipeMemModType(),
    modStageCnt=0,
    pipeName="Dcache",
    linkArr=Some(linkArr),
    memArrIdx=pipeMemMemArrIdx,
    dualRdType=PipeMemRmwDualRdTypeDisabled[
      FlareCpuDcacheEntryPayload, FlareCpuPipePayloadExec
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
    doHazardCmpFunc=Some(
      (
        curr,
        prev,
        isPostDelay,
      ) => {
        val currLdst = curr.hazardCmp.ldst
        val prevLdst = prev.hazardCmp.ldst
        (
          currLdst.valid
          && prevLdst.valid
          && (
            // For higher fmax purposes, test only 32-bit chunks, as that's
            // the largest size of a load or store.
            // We could speed this up further in terms of number of clock
            // cycles, but as my fmax is already probably not good, I
            // choose to use this.
            // For program speed purposes, it may be wiser to use 32-bit
            // data anyway?
            // Oh, but for 8-bit and 16-bit loads and stores, they are
            // likely to not be consecutive instructions anyway, so this
            // fmax increase may be worth it.
            // `memset()`, `memcpy()`, and `memmove()` not withstanding...
            // Apparently those functions can be implemented in a C
            // standard conforming manner with 32-bit loads/stores, so
            // I'll just do that.
            currLdst.addr(
              params.dcacheLineDataIdxRange(params.rawElemNumBytesPow32)
            ) === prevLdst.addr(
              params.dcacheLineDataIdxRange(params.rawElemNumBytesPow32)
            )
          )
        )
      }
    ),
    doPrevHazardCmpFunc=(
      //false
      true
    ),
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
          when (nextState =/= State.MAYBE_NO_BMB_ACCESS) {
            cMid0Front.haltIt()
          }
          switch (rState) {
            is (State.MAYBE_NO_BMB_ACCESS) {
              //val memAddr = inp.myExt.memAddr 
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
