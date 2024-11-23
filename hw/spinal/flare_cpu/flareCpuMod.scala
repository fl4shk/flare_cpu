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
import libcheesevoyage.bus.lcvStall.{
  LcvStallIo, LcvStallHost, LcvStallHostSaved
}

object FlareCpuPsExHaltItState
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    IDLE,
    HALT_IT
    = newElement();
}
object FlareCpuInnerBusAccSz
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    SZ_8,
    SZ_16,
    SZ_32
    = newElement();
}
// the `Bundle` to connect the main CPU core to icache/dcache (eventually
// also the TLB), though there is one of these for each of the
// icache/dcache/etc.
case class FlareCpuInnerBus(
  params: FlareCpuParams,
  isIcache: Boolean,
) extends Bundle with IMasterSlave {
  val valid = in(Bool())
  val ready = out(Bool())
  def fire = (valid && ready)

  val addr = in(UInt(params.mainWidth bits))

  // device/peripheral data (typically from cache!)
  val devData = out(
    UInt(
      (if (isIcache) (params.instrMainWidth) else (params.mainWidth))
      bits
    )
  )
  val accSz = (!isIcache) generate (
    in(FlareCpuInnerBusAccSz())
  )
  val hostData = (!isIcache) generate (
    in(UInt(params.mainWidth bits))
  )
  val lock = (!isIcache) generate (
    in(Bool())
  )

  def asMaster(): Unit = {
    out(valid)
    in(ready)
    out(addr)
    in(devData)
    if (!isIcache) {
      out(accSz)
      out(hostData)
      out(lock)
    }
  }
}
case class FlareCpuIo(
  params: FlareCpuParams,
  haveDbus: Boolean=true,
) extends Bundle {
  //--------
  //val bus = master(Bmb(p=params.busParams))
  //val ibus = AsyncMemoryBus(config=AsyncMemoryBusConfig(
  //))
  val ibus = master(FlareCpuInnerBus(
    params=params,
    isIcache=true,
  ))
  val dbus = (haveDbus) generate (
    master(FlareCpuInnerBus(
      params=params,
      isIcache=false,
    ))
  )
  val irq = in(Bool())
  //--------
}

object FlareCpuFormalInstrCnt {
  def cntWidth = 8
}
case class FlareCpuFormalInstrCnt(
  params: FlareCpuParams,
) extends Bundle {
  val any = UInt(FlareCpuFormalInstrCnt.cntWidth bits)
  val fwd = UInt(FlareCpuFormalInstrCnt.cntWidth bits)
  val jmp = UInt(FlareCpuFormalInstrCnt.cntWidth bits)
}
case class FlareCpuPipeMemModExtType(
  params: FlareCpuParams,
  optFormalTest: Int,
  //=(
  //  FlareCpuParams.enumFormalTestNone
  //),
) extends Bundle {
  val regPc = UInt(params.mainWidth bits)
  val instrCnt = (
    optFormalTest != FlareCpuParams.enumFormalTestNone
  ) generate (
    //UInt(8 bits)
    FlareCpuFormalInstrCnt(params=params)
  )
  val instrEnc = FlareCpuInstrEnc(params=params)
  val instrDecEtc = FlareCpuInstrDecEtc(params=params)
  //val icache = FlareCpuIcachePipePayload(params=params)
  //val dcache = FlareCpuDcachePipePayload(params=params)
}

case class FlareCpuPipeMemModType[
  WordT <: Data,
  HazardCmpT <: Data,
  ModExtT <: Data,
](
  params: FlareCpuParams,
  wordType: HardType[WordT],
  wordCountMax: Int,
  hazardCmpType: HardType[HazardCmpT],
  modRdPortCnt: Int,
  modStageCnt: Int,
  optModHazardKind: Int,
  modExtType: HardType[ModExtT],
) extends Bundle with PipeMemRmwPayloadBase[WordT, HazardCmpT] {
  //--------
  val modExt = modExtType()
  //--------
  //println(s"FlareCpuPipeMemModType: ${modRdPortCnt}")
  val myExt = PipeMemRmwPayloadExt(
    wordType=wordType(),
    wordCount=wordCountMax,
    hazardCmpType=hazardCmpType(),
    modRdPortCnt=modRdPortCnt,
    modStageCnt=modStageCnt,
    optModHazardKind=optModHazardKind,
    optReorder=false,
  )
  //--------
  def setPipeMemRmwExt(
    ext: PipeMemRmwPayloadExt[WordT, HazardCmpT],
    memArrIdx: Int,
  ): Unit = {
    myExt := ext
  }
  def getPipeMemRmwExt(
    ext: PipeMemRmwPayloadExt[WordT, HazardCmpT],
    memArrIdx: Int,
  ): Unit = {
    ext := myExt
  }
  //--------
}
case class FlareCpuIcacheWordType(
  params: FlareCpuParams
) extends Bundle {
  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
  val data = params.icacheLineMemWordType()
}
case class FlareCpuIcachePipePayload(
  params: FlareCpuParams
) extends Bundle {
  val hit = Bool()
  val valid = Bool()
  val word = FlareCpuIcacheWordType(params=params)
}
case class FlareCpuDcacheWordType(
  params: FlareCpuParams
) extends Bundle {
  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
  val data = params.dcacheLineMemWordType()
}
case class FlareCpuDcachePipePayload(
  params: FlareCpuParams
) extends Bundle {
  val hit = Bool()

  val word = FlareCpuDcacheWordType(params=params)

  val valid = Bool()
  val dirty = Bool()
}
//case class FlareCpuPsExSetPcPayload(
//  params: FlareCpuParams,
//  optFormalTest: Int=(
//    FlareCpuParams.enumFormalTestNone
//  ),
//) extends Bundle {
//  //--------
//  val regPc = UInt(params.mainWidth bits)
//  //--------
//  val instrCnt = (
//    optFormalTest != FlareCpuParams.enumFormalTestNone
//  ) generate (
//    UInt(8 bits)
//  )
//  //--------
//}
//case class FlareCpuCalcNextPc(
//  params: FlareCpuParams,
//  cl: CtrlLink,
//  //io: FlareCpuIo,
//  psExSetPc: Flow[FlareCpuPsExSetPcPayload],
//  psIdHaltIt: Bool,
//  optFormalTest: Int=(
//    FlareCpuParams.enumFormalTestNone
//  ),
//) extends Area {
//  //--------
//  val up = cl.up
//  val down = cl.down
//  //--------
//  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
//  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
//  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
//  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
//  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
//  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
//  def enumRegFileLim = FlareCpuParams.enumRegFileLim
//  //--------
//  def enumFormalTestNone = FlareCpuParams.enumFormalTestNone
//  def enumFormalTestMain = FlareCpuParams.enumFormalTestMain
//  //--------
//  val nextRegPc = UInt(params.mainWidth bits)
//  nextRegPc := (
//    RegNext(nextRegPc)
//    init(nextRegPc.getZero)
//  )
//  val rSavedExSetPc = (
//    //Reg(Flow(UInt(params.mainWidth bits)))
//    Reg(
//      Flow(FlareCpuPsExSetPcPayload(
//        params=params,
//        optFormalTest=optFormalTest,
//      ))
//    )
//  )
//  rSavedExSetPc.init(rSavedExSetPc.getZero)
//
//  when (psExSetPc.fire) {
//    rSavedExSetPc := psExSetPc
//  }
//  val rPrevRegPc = (
//    RegNextWhen(
//      //upModExt.regPc,
//      nextRegPc,
//      up.isFiring,
//    )
//    init(nextRegPc.getZero)
//  )
//
//  when (up.isFiring) {
//    //--------
//    rSavedExSetPc := rSavedExSetPc.getZero
//    //--------
//    when (psExSetPc.fire) {
//      nextRegPc := psExSetPc.regPc //+ (params.instrMainWidth / 8)
//    } elsewhen (rSavedExSetPc.fire) {
//      nextRegPc := (
//        rSavedExSetPc.regPc //+ (params.instrMainWidth / 8)
//      )
//    } otherwise {
//      nextRegPc := rPrevRegPc + (params.instrMainWidth / 8)
//    }
//  }
//}
case class FlareCpuPipeStageIf(
  params: FlareCpuParams,
  cIf: CtrlLink,
  pIf: Payload[FlareCpuPipeMemModExtType],
  io: FlareCpuIo,
  psIdHaltIt: Bool,
  psExSetPc: Flow[UInt],
  optFormalTestNoJumps: Boolean=(
    false
  ),
  optFormalTest: Int=(
    FlareCpuParams.enumFormalTestNone
  ),
) extends Area {
  //--------
  val up = cIf.up
  val down = cIf.down
  //--------
  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
  def enumRegFileLim = FlareCpuParams.enumRegFileLim
  //--------
  def enumFormalTestNone = FlareCpuParams.enumFormalTestNone
  def enumFormalTestMain = FlareCpuParams.enumFormalTestMain
  //--------
  def myFormal = (
    optFormalTest != FlareCpuParams.enumFormalTestNone
  )
  def myFormalMain = (
    optFormalTest == FlareCpuParams.enumFormalTestMain
  )
  //--------
  val upModExt = FlareCpuPipeMemModExtType(
    params=params,
    optFormalTest=optFormalTest,
  )
  up(pIf) := upModExt
  upModExt := (
    RegNext(upModExt)
    init(upModExt.getZero)
  )
  def nextRegPc = upModExt.regPc
  //val nextRegPc = UInt(params.mainWidth bits)
  //nextRegPc := (
  //  RegNext(nextRegPc)
  //  init(nextRegPc.getZero)
  //)
  val rSavedExSetPc = (
    Reg(Flow(UInt(params.mainWidth bits)))
    //Reg(
    //  Flow(FlareCpuPsExSetPcPayload(
    //    params=params,
    //    optFormalTest=optFormalTest,
    //  ))
    //)
  )
  rSavedExSetPc.init(rSavedExSetPc.getZero)

  if (
    myFormal
    && optFormalTestNoJumps
  ) {
    rSavedExSetPc := rSavedExSetPc.getZero
  } else {
    when (psExSetPc.fire) {
      rSavedExSetPc := psExSetPc
    }
  }
  val rPrevRegPc = (
    RegNextWhen(
      //upModExt.regPc,
      nextRegPc,
      up.isFiring,
    )
    init(nextRegPc.getZero)
  )
  val rPrevInstrCnt = (myFormal) generate (
    RegNextWhen(
      upModExt.instrCnt,
      up.isFiring
    )
    init(upModExt.instrCnt.getZero)
  )

  when (up.isFiring) {
    //--------
    rSavedExSetPc := rSavedExSetPc.getZero
    if (myFormal) {
      upModExt.instrCnt.any := rPrevInstrCnt.any + 1
    }
    //--------
    when (psExSetPc.fire) {
      if (myFormal) {
        upModExt.instrCnt.jmp := rPrevInstrCnt.jmp + 1
      }
      nextRegPc := psExSetPc.payload //+ (params.instrMainWidth / 8)
    } elsewhen (rSavedExSetPc.fire) {
      nextRegPc := (
        rSavedExSetPc.payload //+ (params.instrMainWidth / 8)
      )
      if (myFormal) {
        upModExt.instrCnt.jmp := rPrevInstrCnt.jmp + 1
      }
    } otherwise {
      nextRegPc := rPrevRegPc + (params.instrMainWidth / 8)
      if (myFormal) {
        upModExt.instrCnt.fwd := rPrevInstrCnt.fwd + 1
      }
    }
  }
  //--------
  //when (exSetPc.fire) {
  //  upModExt.regPc := exSetPc.payload + (params.instrMainWidth / 8)
  //}
  //--------
  io.ibus.valid := True
  io.ibus.addr := upModExt.regPc
  //--------
  val myDoHaltIt = (myFormal) generate (
    Bool()
  )
  if (myFormal) {
    myDoHaltIt := False
  }
  def doStallMain(): Unit = {
    io.ibus.addr := (
      RegNext(io.ibus.addr)
      init(io.ibus.addr.getZero)
    )
  }
  def doHaltItEtc(): Unit = {
    ////io.ibus.valid := False
    //io.ibus.addr := (
    //  RegNext(io.ibus.addr)
    //  init(io.ibus.addr.getZero)
    //)
    io.ibus.valid := False
    doStallMain()
    cIf.haltIt()
    if (myFormal) {
      myDoHaltIt := True
    }
  }
  when (
    //(RegNext(io.ibus.valid) init(False))
    //&&
    !io.ibus.ready
  ) {
    //cIf.duplicateIt()
    doHaltItEtc()
  }
  //when (psIdHaltIt) {
  //  //haltIt()
  //  //io.ibus.valid := False
  //  doHaltItEtc()
  //}
  //--------
  //if (params.formal()) {
  //println("testificate")
  if (myFormalMain) {
    //when (!psExSetPc.fire) {
    //  assert(
    //    upModExt.regPc === psExSetPc.payload
    //  )
    //}
    //when (
    //  pastValidAfterReset()
    //  //&& (
    //  //  RegNextWhen(True, !ClockDomain.isResetActive) init(False)
    //  //)
    //) {
      when (
        !io.ibus.ready //|| psIdHaltIt
      ) {
        assert(
          !cIf.up.isReady
        )
        assert(
          !cIf.down.isValid
        )
        assert(
          myDoHaltIt
        )
      }
      when (pastValidAfterReset) {
        when (!io.ibus.ready) {
          assert(
            stable(io.ibus.addr)
          )
        }
      }
      //when (psIdHaltIt) {
      //  assert(
      //    !io.ibus.valid
      //  )
      //} otherwise {
      //  assert(
      //    io.ibus.valid
      //  )
      //}
      when (pastValidAfterReset) {
        //when (!past(io.ibus.valid)) {
        //  assume(
        //    !io.ibus.ready
        //  )
        //}
        when (past(io.ibus.valid)) {
          when (io.ibus.ready) {
            when (!io.ibus.valid) {
              assume(!RegNext(io.ibus.ready))
            }
          }
        }
        //when (past(io.ibus.valid)) {
        //}
        //cover(
        //  io.ibus.ready
        //)
      }
    //}
    //  when (past(up.isFiring)) {
    //    assert(
    //      rSavedExSetPc === rSavedExSetPc.getZero
    //    )
    //    when (past(psExSetPc.fire)) {
    //      assert(
    //        past(upModExt.regPc) === past(psExSetPc.payload)
    //      )
    //    } elsewhen (past(rSavedExSetPc).fire) {
    //      assert(
    //        past(upModExt.regPc) === past(rSavedExSetPc.payload)
    //      )
    //    } otherwise {
    //      assert(
    //        past(upModExt.regPc)
    //        === past(rPrevRegPc) + (params.instrMainWidth / 8)
    //      )
    //    }
    //  }
    //}
    when (
      up.isFiring
    ) {
      assert(
        !myDoHaltIt
      )
    }
    when (
      pastValidAfterReset
    ) {
      when (past(up.isFiring)) {
        assert(
          rSavedExSetPc === rSavedExSetPc.getZero
        )
      }
      when (
        up.isFiring
      ) {
        when (
          !psExSetPc.fire
          && !rSavedExSetPc.fire
        ) {
          assert(
            nextRegPc
            === rPrevRegPc + (params.instrMainWidth / 8)
          )
          assert(
            upModExt.instrCnt.fwd
            === rPrevInstrCnt.fwd + 1
          )
          assert(
            stable(upModExt.instrCnt.jmp)
          )
          //when (!past(up.isFiring)) {
          //  assert(
          //    stable(psExSetPc.fire)
          //  )
          //  //assert(
          //  //  stable(rSavedExSetPc.fire)
          //  //)
          //}
          //cover(
          //  !past(up.isFiring)
          //  && stable(psExSetPc.fire)
          //)
          ////cover(
          ////  !past(up.isFiring)
          ////  && stable(rSavedExSetPc.fire)
          ////)
        } elsewhen (
          psExSetPc.fire
        ) {
          assert(
            nextRegPc === psExSetPc.payload
          )
          assert(
            stable(upModExt.instrCnt.fwd)
          )
          assert(
            upModExt.instrCnt.jmp
            === rPrevInstrCnt.jmp + 1
          )
        } otherwise { // when (rSavedExSetPc.fire)
          //assert(
          //  rSavedExSetPc.fire
          //)
          assert(
            nextRegPc === rSavedExSetPc.payload
          )
          assert(
            stable(upModExt.instrCnt.fwd)
          )
          assert(
            upModExt.instrCnt.jmp
            === rPrevInstrCnt.jmp + 1
          )
        }
        //when (
        //  RegNextWhen(True, up.isFiring) init(False)
        //) {
        //  when (
        //    !psExSetPc.fire
        //    && !rSavedExSetPc.fire
        //  ) {
        //    //assert(
        //    //  nextRegPc
        //    //  === rPrevRegPc + (params.instrMainWidth / 8)
        //    //)
        //    //assert(
        //    //  upModExt.instrCnt.fwd
        //    //  === rPrevInstrCnt.fwd + 1
        //    //)
        //    cover(
        //      !past(up.isFiring)
        //      && stable(psExSetPc.fire)
        //    )
        //    cover(
        //      !past(up.isFiring)
        //      && stable(rSavedExSetPc.fire)
        //    )
        //    when (!past(up.isFiring)) {
        //      assert(
        //        stable(psExSetPc.fire)
        //      )
        //      assert(
        //        stable(rSavedExSetPc.fire)
        //      )
        //    }
        //  }
        //}
        assert(
          upModExt.instrCnt.any
          === rPrevInstrCnt.any + 1
        )
      } otherwise {
        assert(
          stable(nextRegPc)
        )
        assert(
          stable(upModExt.instrCnt.fwd)
        )
        assert(
          stable(upModExt.instrCnt.jmp)
        )
        assert(
          stable(upModExt.instrCnt.any)
        )
      }
    }
  }
  //}
  //--------
  //when (io.ibus.ready) {
  //  when (exSetPc.fire) {
  //    upModExt.regPc := exSetPc.payload
  //  } elsewhen (rSavedExSetPc.fire) {
  //    upModExt.regPc := rSavedExSetPc.payload
  //  } 
  //  //otherwise { // no `exSetPc.fire` or `rSavedExSetPc.fire`
  //  //}
  //} otherwise { //when (!io.ibus.ready) 
  //  haltIt()
  //}

  //object IbusState extends SpinalEnum(defaultEncoding=binarySequential) {
  //  val
  //    INIT_OR_NOT_READY,
  //    READY
  //    = newElement();
  //}
  //val rIbusState = (
  //  Reg(IbusState()) init(IbusState.INIT_OR_NOT_READY)
  //)
  //switch (rIbusState) {
  //  is (IbusState.INIT_OR_NOT_READY) {
  //    io.ibus.valid := True
  //  }
  //  is (IbusState.READY) {
  //  }
  //}
  //--------
}
case class FlareCpuPipeStageId(
  params: FlareCpuParams,
  cId: CtrlLink,
  pIf: Payload[FlareCpuPipeMemModExtType],
  //pId: Payload[FlareCpuPipeMemModExtType],
  ////pId: Payload[FlareCpuPipeMemModExtType],
  io: FlareCpuIo,
  regFile: Option[PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      FlareCpuPipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool]
  ]],
  //mkRegFileModType: () => FlareCpuPipeMemModType[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModExtType,
  //],
  psIdHaltIt: Bool,
  psExSetPc: Flow[UInt],
  //up: NodeApi,
  //down: NodeApi,
  optFormalTest: Int=(
    FlareCpuParams.enumFormalTestNone
  ),
) extends Area {
  //--------
  val up = cId.up
  val down = cId.down
  //--------
  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
  def enumRegFileLim = FlareCpuParams.enumRegFileLim
  //--------
  def mkRegFileModType() = (
    FlareCpuParams.mkRegFileModType(
      params=params,
      optFormalTest=optFormalTest,
    )
    //FlareCpuPipeMemModType(
    //  params=params,
    //  wordType=params.regWordType(),
    //  wordCountMax=params.sprFileEvenWordCount,
    //  hazardCmpType=params.regFileHazardCmpType(),
    //  modRdPortCnt=params.regFileModRdPortCnt,
    //  modStageCnt=params.regFileModStageCnt,
    //  optModHazardKind=params.regFileOptModHazardKind,
    //  modExtType=FlareCpuPipeMemModExtType(params=params),
    //)
  )
  //--------
  def enumFormalTestNone = FlareCpuParams.enumFormalTestNone
  def enumFormalTestMain = FlareCpuParams.enumFormalTestMain
  //--------
  def myFormal = (
    optFormalTest != FlareCpuParams.enumFormalTestNone
  )
  def myFormalMain = (
    optFormalTest == FlareCpuParams.enumFormalTestMain
  )
  val upModExt = FlareCpuPipeMemModExtType(
    params=params,
    optFormalTest=optFormalTest,
  )
  //up(pId) := upModExt
  upModExt := (
    RegNext(upModExt)
    init(upModExt.getZero)
  )
  upModExt.allowOverride
  when (up.isValid) {
    upModExt.regPc := cId.up(pIf).regPc
    if (myFormal) {
      upModExt.instrCnt := cId.up(pIf).instrCnt
    }
  }
  //cId.bypass(pIf) := upModExt
  //--------
  def upInstrEnc = upModExt.instrEnc
  def upInstrDecEtc = upModExt.instrDecEtc
  upInstrDecEtc.allowOverride
  val rSavedUpInstrDecEtc = (
    RegNextWhen(
      upInstrDecEtc,
      up.isFiring,
    ) init(upInstrDecEtc.getZero)
  )
  //val canIrq = Bool()
  //canIrq := True

  //val myFrontPayloadGprEven = mkRegFileModType()
  val myFrontPayloadGprEvenNonFp = mkRegFileModType()
  val myFrontPayloadGprFp = mkRegFileModType()
  val myFrontPayloadGprOddNonSp = mkRegFileModType()
  val myFrontPayloadGprSp = mkRegFileModType()
  val myFrontPayloadSprEven = mkRegFileModType()
  val myFrontPayloadSprOdd = mkRegFileModType()

  myFrontPayloadGprEvenNonFp := (
    RegNext(myFrontPayloadGprEvenNonFp)
    init(myFrontPayloadGprEvenNonFp.getZero)
  )
  myFrontPayloadGprFp := (
    RegNext(myFrontPayloadGprFp)
    init(myFrontPayloadGprFp.getZero)
  )
  myFrontPayloadGprOddNonSp := (
    RegNext(myFrontPayloadGprOddNonSp)
    init(myFrontPayloadGprOddNonSp.getZero)
  )
  myFrontPayloadGprSp := (
    RegNext(myFrontPayloadGprSp)
    init(myFrontPayloadGprSp.getZero)
  )
  myFrontPayloadSprEven := (
    RegNext(myFrontPayloadSprEven)
    init(myFrontPayloadSprEven.getZero)
  )
  myFrontPayloadSprOdd := (
    RegNext(myFrontPayloadSprOdd)
    init(myFrontPayloadSprOdd.getZero)
  )

  regFile match {
    case Some(myRegFile) => {
      up(myRegFile.io.frontPayloadArr(enumRegFileGprEvenNonFp)) := (
        myFrontPayloadGprEvenNonFp
      )
      up(myRegFile.io.frontPayloadArr(enumRegFileGprFp)) := (
        myFrontPayloadGprFp
      )
      up(myRegFile.io.frontPayloadArr(enumRegFileGprOddNonSp)) := (
        myFrontPayloadGprOddNonSp
      )
      up(myRegFile.io.frontPayloadArr(enumRegFileGprSp)) := (
        myFrontPayloadGprSp
      )
      up(myRegFile.io.frontPayloadArr(enumRegFileSprEven)) := (
        myFrontPayloadSprEven
      )
      up(myRegFile.io.frontPayloadArr(enumRegFileSprOdd)) := (
        myFrontPayloadSprOdd
      )
    }
    case None => {
    }
  }
  object MultiCycleState
  extends SpinalEnum(defaultEncoding=binarySequential) {
    val
      PRIMARY,
      LPRE_SIMM_LO,
      G7_SUB_DECODE
      = newElement()
  }

  val rMultiCycleState = (
    Reg(MultiCycleState())
    init(MultiCycleState.PRIMARY)
  )
  val rDidHandleG7SubDecode = Reg(Bool()) init(False)
  def myDoHaltIt(): Unit = {
    //psIdHaltIt := True
    cId.haltIt()
      // this `haltIt()` call prevents `up.isFiring` and prevents
      // deassertion of `rDidHandleG7SubDecode`
  }
  when (up.isValid) {
    // Take one extra cycle to decode group 7 instructions to help with
    // fmax
    when (
      rMultiCycleState === MultiCycleState.PRIMARY
      && upInstrEnc.g0Pre.grp === FlareCpuInstrEncConst.g7Grp
      && !rDidHandleG7SubDecode
    ) {
      myDoHaltIt()
      rMultiCycleState := MultiCycleState.G7_SUB_DECODE
      rDidHandleG7SubDecode := True
    }
  }

  //up(pId) := upModExt
  when (
    up.isFiring
    //up.isValid
  ) {
    rDidHandleG7SubDecode := False
    //upInstrDecEtc := upInstrDecEtc.getZero
    //upInstrDecEtc.isInvalid := False
    //upInstrDecEtc.haveFullInstr := True

    //def clearRegsMain(): Unit = {
    //}
    //--------
    //myFrontPayloadGprEvenNonFp.myExt.modMemWordValid := (
    //  upInstrDecEtc.gprEvenNonFpRaIdx.valid
    //)
    myFrontPayloadGprEvenNonFp.modExt := upModExt
    myFrontPayloadGprEvenNonFp.myExt.memAddr(0) := (
      upInstrDecEtc.gprEvenNonFpRaIdx.payload(
        myFrontPayloadGprEvenNonFp.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadGprEvenNonFp.myExt.memAddr(1) := (
      upInstrDecEtc.gprEvenNonFpRbIdx.payload(
        myFrontPayloadGprEvenNonFp.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    myFrontPayloadGprFp.modExt := upModExt
    //myFrontPayloadGprFp.myExt.modMemWordValid := (
    //  upInstrDecEtc.gprFpRaIdx.valid
    //)
    myFrontPayloadGprFp.myExt.memAddr(0) := (
      upInstrDecEtc.gprFpRaIdx.payload(
        myFrontPayloadGprFp.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadGprFp.myExt.memAddr(1) := (
      upInstrDecEtc.gprFpRbIdx.payload(
        myFrontPayloadGprFp.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    //myFrontPayloadGprOddNonSp.myExt.modMemWordValid := (
    //  upInstrDecEtc.gprOddNonSpRaIdx.valid
    //)
    myFrontPayloadGprOddNonSp.modExt := upModExt
    myFrontPayloadGprOddNonSp.myExt.memAddr(0) := (
      upInstrDecEtc.gprOddNonSpRaIdx.payload(
        myFrontPayloadGprOddNonSp.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadGprOddNonSp.myExt.memAddr(1) := (
      upInstrDecEtc.gprOddNonSpRbIdx.payload(
        myFrontPayloadGprOddNonSp.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    //myFrontPayloadGprSp.myExt.modMemWordValid := (
    //  upInstrDecEtc.gprSpRaIdx.valid
    //)
    myFrontPayloadGprSp.modExt := upModExt
    myFrontPayloadGprSp.myExt.memAddr(0) := (
      upInstrDecEtc.gprSpRaIdx.payload(
        myFrontPayloadGprSp.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadGprSp.myExt.memAddr(1) := (
      upInstrDecEtc.gprSpRbIdx.payload(
        myFrontPayloadGprSp.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    //myFrontPayloadSprEven.myExt.modMemWordValid := (
    //  upInstrDecEtc.sprEvenSaIdx.valid
    //)
    myFrontPayloadSprEven.modExt := upModExt
    myFrontPayloadSprEven.myExt.memAddr(0) := (
      upInstrDecEtc.sprEvenSaIdx.payload(
        myFrontPayloadSprEven.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadSprEven.myExt.memAddr(1) := (
      upInstrDecEtc.sprEvenSbIdx.payload(
        myFrontPayloadSprEven.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    //myFrontPayloadSprOdd.myExt.modMemWordValid := (
    //  upInstrDecEtc.sprOddSaIdx.valid
    //)
    myFrontPayloadSprOdd.modExt := upModExt
    myFrontPayloadSprOdd.myExt.memAddr(0) := (
      upInstrDecEtc.sprOddSaIdx.payload(
        myFrontPayloadSprOdd.myExt.memAddr(0).bitsRange
      )
    )
    myFrontPayloadSprOdd.myExt.memAddr(1) := (
      upInstrDecEtc.sprOddSbIdx.payload(
        myFrontPayloadSprOdd.myExt.memAddr(1).bitsRange
      )
    )
    //--------
    //case class DecodeGpr(
    //  gprRaIdx: UInt,
    //  wrGprRa: Boolean,
    //  dual64: Boolean=false
    //) {
    //}

    def finishInstr(
      //isBlJl: Boolean=false,
      ////writeSprFlags: Option[Bool]=None,
      //writeGpr: Bool=True,
      //writeGpr: Option[(UInt, Boolean, Boolean)]=(
      //  Some((U"1'd0", false, false))
      //),
      writeGpr: Option[(UInt, Boolean)]=Some((U"1'd0", false)),
      gprDual64: Boolean=false,
      readGprRaAsRb: Boolean=false,
      //decodeGpr: Option[DecodeGpr]=(
      //  Some(
      //    DecodeGpr(
      //      U"1'd0",
      //      false,
      //      //false,
      //    )
      //  )
      //),
      rdWrSpr0: Option[(UInt, Bool, Boolean)]=None,
      rdWrSpr1: Option[(UInt, Bool, Boolean)]=None,
    ): Unit = {
      //upInstrDecEtc.decodeTemp.indexRaRbValid := False
      //upInstrDecEtc.decodeTemp.indexRaSimmValid := False
      //upInstrDecEtc.decodeTemp.preLpreValid := False
      //upInstrDecEtc.decodeTemp.preValid := False
      //upInstrDecEtc.decodeTemp.lpreValid := False
      upInstrDecEtc.decodeTemp := upInstrDecEtc.decodeTemp.getZero

      upInstrDecEtc.isInvalid := False
      upInstrDecEtc.haveFullInstr := True
      upInstrDecEtc.raIdx := upInstrEnc.g2.raIdx
      upInstrDecEtc.rbIdx := upInstrEnc.g2.rbIdx

      //upInstrDecEtc.haveFullInstr := True

      //--------
      // BEGIN: Old design for `finishInstr()`'s writing rA
      //if (!isBlJl) {
      //  upInstrDecEtc.raIdx := upInstrEnc.g2.raIdx
      //} else { // if (isBlSimm)
      //  upInstrDecEtc.raIdx := FlareCpuInstrEncConst.gprLrIdx
      //}
      // END: Old design for `finishInstr()`'s writing rA
      //def setGprA(
      //): Unit = {
      //}
      //--------
      // BEGIN: New design for `finishInstr()`'s writing rA
      def setGpr(
        tempGprIdx: UInt,
        isGprRa: Boolean,
      ): Unit = {
        //val tempRaIdx = (
        //  if (!myWriteGpr._2) (
        //    upInstrDecEtc.raIdx
        //  ) else (
        //    myWriteGpr._1
        //  )
        //) //--------
        val myGprEvenNonFpRegIdx = (
          if (isGprRa) (
            upInstrDecEtc.gprEvenNonFpRaIdx
          ) else (
            upInstrDecEtc.gprEvenNonFpRbIdx
          )
        )
        val myGprFpRegIdx = (
          if (isGprRa) (
            upInstrDecEtc.gprFpRaIdx
          ) else (
            upInstrDecEtc.gprFpRbIdx
          )
        )
        val myGprOddNonSpRegIdx = (
          if (isGprRa) (
            upInstrDecEtc.gprOddNonSpRaIdx
          ) else (
            upInstrDecEtc.gprOddNonSpRbIdx
          )
        )
        val myGprSpRegIdx = (
          if (isGprRa) (
            upInstrDecEtc.gprSpRaIdx
          ) else (
            upInstrDecEtc.gprSpRbIdx
          )
        )
        if (!gprDual64) {
          myGprEvenNonFpRegIdx.valid := (
            !tempGprIdx(0)
            && (
              tempGprIdx =/= FlareCpuInstrEncConst.gprFpIdx
            )
          )
          //--------
          myGprFpRegIdx.valid := (
            tempGprIdx === FlareCpuInstrEncConst.gprFpIdx
          )
          //--------
          myGprOddNonSpRegIdx.valid := (
            tempGprIdx(0)
            && (
              tempGprIdx =/= FlareCpuInstrEncConst.gprSpIdx
            )
          )
          //--------
          myGprSpRegIdx.valid := (
            tempGprIdx === FlareCpuInstrEncConst.gprSpIdx
          )
          //--------
        } else { // if (gprDual64)
          myGprEvenNonFpRegIdx.valid := (
            Cat(tempGprIdx(tempGprIdx.high downto 1), False).asUInt
            =/= FlareCpuInstrEncConst.gprFpIdx
          )
          myGprFpRegIdx.valid := (
            Cat(tempGprIdx(tempGprIdx.high downto 1), False).asUInt
            === FlareCpuInstrEncConst.gprFpIdx
          )
          myGprOddNonSpRegIdx.valid := (
            Cat(tempGprIdx(tempGprIdx.high downto 1), True).asUInt
            =/= FlareCpuInstrEncConst.gprSpIdx
          )
          myGprSpRegIdx.valid := (
            Cat(tempGprIdx(tempGprIdx.high downto 1), True).asUInt
            === FlareCpuInstrEncConst.gprSpIdx
          )
        }
        //--------
        //upInstrDecEtc.wrGprSpRaIdx := (
        //  tempGprIdx === FlareCpuInstrEncConst.gprSpIdx
        //)
        //--------
        val myGprSel = (
          if (isGprRa) (
            upInstrDecEtc.gprRaSel
          ) else (
            upInstrDecEtc.gprRbSel
          )
        )
        //if (!gprDual64) {
          when (
            //upInstrDecEtc.gprEvenNonFpRaIdx.valid
            if (isGprRa) (
              upInstrDecEtc.gprEvenNonFpRaIdx.fire
            ) else (
              upInstrDecEtc.gprEvenNonFpRbIdx.fire
            )
          ) {
            myGprSel := FlareCpuGprSelect.gprEvenNonFp
          } elsewhen (
            if (isGprRa) (
              upInstrDecEtc.gprFpRaIdx.fire
            ) else (
              upInstrDecEtc.gprFpRbIdx.fire
            )
          ) {
            myGprSel := FlareCpuGprSelect.gprFp
          } elsewhen (
            if (isGprRa) (
              upInstrDecEtc.gprOddNonSpRaIdx.fire
            ) else (
              upInstrDecEtc.gprOddNonSpRbIdx.fire
            )
          ) {
            myGprSel := FlareCpuGprSelect.gprOddNonSp
          } otherwise {
            myGprSel := FlareCpuGprSelect.gprSp
          }
        //} else { // if (gprDual64)
        //  //myGprSel := FlareCpuGprSelect.gpr64
        //}
        val myGpr64IsNonFpSp = (
          if (isGprRa) (
            upInstrDecEtc.gprRa64IsNonFpSp
          ) else (
            upInstrDecEtc.gprRb64IsNonFpSp
          )
        )
        myGpr64IsNonFpSp := (
          if (isGprRa) (
            upInstrDecEtc.gprEvenNonFpRaIdx.fire
          ) else (
            upInstrDecEtc.gprEvenNonFpRbIdx.fire
          )
        )
        //upInstrDecEtc.gprRa64IsNonFpSp := (
        //  upInstrDecEtc.gprEvenNonFprR
        //)
      }

      writeGpr match {
        case Some(myWriteGpr) => {
          val tempRaIdx = (
            if (!myWriteGpr._2) (
              upInstrEnc.g2.raIdx
            ) else (
              myWriteGpr._1
            )
          )
          setGpr(
            tempGprIdx=tempRaIdx,
            isGprRa=true,
          )
          upInstrDecEtc.wrGprEvenNonFpRaIdx := (
            upInstrDecEtc.gprEvenNonFpRaIdx.fire
          )
          upInstrDecEtc.wrGprFpRaIdx := (
            upInstrDecEtc.gprFpRaIdx.fire
          )
          upInstrDecEtc.wrGprOddNonSpRaIdx := (
            upInstrDecEtc.gprOddNonSpRaIdx.fire
          )
          upInstrDecEtc.wrGprSpRaIdx := (
            upInstrDecEtc.gprSpRaIdx.fire
          )
        }
        case None => {
          ////upInstrDecEtc.wrGprEvenNonFpRaIdx := False
          //upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
          //upInstrDecEtc.gprFpRaIdx.valid := False
          //upInstrDecEtc.gprOddNonSpRaIdx.valid := False
          //upInstrDecEtc.gprSpRaIdx.valid := False
          val tempRaIdx = (
            //if (!myWriteGpr._2) (
              upInstrDecEtc.raIdx
            //) else (
            //  myWriteGpr._1
            //)
          )
          setGpr(
            tempGprIdx=tempRaIdx,
            isGprRa=true,
          )
          upInstrDecEtc.wrGprEvenNonFpRaIdx := (
            False
          )
          upInstrDecEtc.wrGprFpRaIdx := (
            False
          )
          upInstrDecEtc.wrGprOddNonSpRaIdx := (
            False
          )
          upInstrDecEtc.wrGprSpRaIdx := (
            False
          )
        }
      }
      setGpr(
        tempGprIdx=(
          if (readGprRaAsRb) (
            upInstrDecEtc.raIdx
          ) else (
            upInstrDecEtc.rbIdx
          )
        ),
        isGprRa=false,
      )
      //if (true) {
      //  //--------
      //  val tempRbIdx = (
      //    //if (!myWriteGpr._2) (
      //      upInstrDecEtc.rbIdx
      //    //) else (
      //    //  myWriteGpr._1
      //    //)
      //  )
      //  upInstrDecEtc.gprEvenNonFpRbIdx.valid := (
      //    !tempRbIdx(0)
      //    && (
      //      tempRbIdx =/= FlareCpuInstrEncConst.gprFpIdx
      //    )
      //  )
      //  upInstrDecEtc.gprFpRbIdx.valid := (
      //    tempRbIdx === FlareCpuInstrEncConst.gprFpIdx
      //  )
      //  upInstrDecEtc.gprOddNonSpRbIdx.valid := (
      //    tempRbIdx(0)
      //    && (
      //      tempRbIdx =/= FlareCpuInstrEncConst.gprSpIdx
      //    )
      //  )
      //  upInstrDecEtc.gprSpRbIdx.valid := (
      //    tempRbIdx === FlareCpuInstrEncConst.gprSpIdx
      //  )
      //  when (upInstrDecEtc.gprEvenNonFpRbIdx.valid) {
      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprEvenNonFp
      //  } elsewhen (upInstrDecEtc.gprFpRbIdx.valid) {
      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprFp
      //  } elsewhen (upInstrDecEtc.gprOddNonSpRbIdx.valid) {
      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprOddNonSp
      //  } otherwise {
      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprSp
      //  }
      //}
      // END: New design for `finishInstr()`'s writing rA
      //--------

      upInstrDecEtc.rbIdx := upInstrEnc.g2.rbIdx
      //upInstrDecEtc.saIdx := upInstrEnc.g2.raIdx
      //upInstrDecEtc.sbIdx := upInstrEnc.g2.rbIdx


      //when (upInstrDecEtc.gprEvenRaIdx.valid) {
      //} otherwise {
      //}

      //--------
      upInstrDecEtc.gprEvenNonFpRaIdx.payload := (
        Cat(
          U"1'd0",
          upInstrDecEtc.raIdx(upInstrDecEtc.raIdx.high downto 1),
        ).asUInt
      )
      upInstrDecEtc.gprEvenNonFpRbIdx.payload := (
        Cat(
          U"1'd0",
          upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
        ).asUInt
      )
      //--------
      upInstrDecEtc.gprFpRaIdx.payload := (
        0x0
      )
      upInstrDecEtc.gprFpRbIdx.payload := 0x0
      //--------
      upInstrDecEtc.gprOddNonSpRaIdx.payload := (
        Cat(
          U"1'd0",
          upInstrDecEtc.raIdx(upInstrDecEtc.raIdx.high downto 1),
        ).asUInt
      )
      upInstrDecEtc.gprOddNonSpRbIdx.payload := (
        Cat(
          U"1'd0",
          upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
        ).asUInt
      )
      //--------
      upInstrDecEtc.gprSpRaIdx.payload := (
        0x0
      )
      upInstrDecEtc.gprSpRbIdx.payload := 0x0
      //--------
      //if (!writeSprFlags) {
      //  upInstrDecEtc.sprSaIdx.payload := upInstrDecEtc.saIdx
      //} else 
      //if (writeSprFlags)
      //writeSprFlags match {
      //  case Some(myWriteSprFlags) => {
      //    when (myWriteSprFlags) {
      //      upInstrDecEtc.sprEvenSaIdx.valid := True
      //      upInstrDecEtc.sprEvenSaIdx.payload := (
      //        FlareCpuInstrEncConst.sprFlagsIdx
      //      )
      //    }
      //  }
      //  case None => {
      //    //upInstrDecE
      //  }
      //}
      rdWrSpr0 match {
        case Some(myWriteSpr0) => {
          when (myWriteSpr0._2) {
            //if (myWriteSpr0._1 % 2 == 0) {
            //} else {
            //}
            when (!myWriteSpr0._1.lsb) {
              // even
              upInstrDecEtc.sprSaSel := FlareCpuSprSelect.sprEven
              if (myWriteSpr0._3) {
                upInstrDecEtc.wrSprEvenSaIdx := True
              }
              upInstrDecEtc.sprEvenSaIdx.valid := True
              upInstrDecEtc.sprEvenSaIdx.payload := (
                Cat(
                  False,
                  myWriteSpr0._1(myWriteSpr0._1.high downto 1)
                ).asUInt
              )
              rdWrSpr1 match {
                case Some(myWriteSpr1) => {
                }
                case None => {
                  //upInstrDecEtc.sprEvenSaIdx.valid := False
                  disableSprOddWrite()
                }
              }
            } otherwise { // when (myWriteSpr0._1.lsb)
              upInstrDecEtc.sprSaSel := FlareCpuSprSelect.sprOdd
              // odd
              if (myWriteSpr0._3) {
                upInstrDecEtc.wrSprOddSaIdx := True
              }
              upInstrDecEtc.sprOddSaIdx.valid := True
              upInstrDecEtc.sprOddSaIdx.payload := (
                Cat(
                  False,
                  myWriteSpr0._1(myWriteSpr0._1.high downto 1)
                ).asUInt
              )
              rdWrSpr1 match {
                case Some(myWriteSpr1) => {
                }
                case None => {
                  //upInstrDecEtc.sprEvenSaIdx.valid := False
                  disableSprEvenWrite()
                }
              }
            }
          }
        }
        case None => {
          rdWrSpr1 match {
            case Some(myWriteSpr1) => {
            }
            case None => {
              disableSprWrites()
            }
          }
        }
      }
      rdWrSpr1 match {
        case Some(myWriteSpr1) => {
          when (myWriteSpr1._2) {
            //if (myWriteSpr1._1 % 2 == 0) {
            //} else {
            //}
            when (!myWriteSpr1._1.lsb) {
              if (myWriteSpr1._3) {
                upInstrDecEtc.wrSprEvenSaIdx := True
              }
              upInstrDecEtc.sprEvenSaIdx.valid := True
              upInstrDecEtc.sprEvenSaIdx.payload := (
                Cat(
                  False,
                  myWriteSpr1._1(myWriteSpr1._1.high downto 1)
                ).asUInt
              )
            } otherwise { // when (myWriteSpr1._1.lsb)
              if (myWriteSpr1._3) {
                upInstrDecEtc.wrSprOddSaIdx := True
              }
              upInstrDecEtc.sprOddSaIdx.valid := True
              upInstrDecEtc.sprOddSaIdx.payload := (
                Cat(
                  False,
                  myWriteSpr1._1(myWriteSpr1._1.high downto 1),
                ).asUInt
              )
            }
          }
        }
        case None => {
        }
      }
      when (!upInstrDecEtc.rbIdx.lsb) {
        upInstrDecEtc.sprSbSel := FlareCpuSprSelect.sprEven
      } otherwise { // when (upInstrDecEtc.rbIdx.lsb)
        upInstrDecEtc.sprSbSel := FlareCpuSprSelect.sprOdd
      }
      //when (!upInstrDecEtc.rbIdx.lsb) {
        upInstrDecEtc.sprEvenSbIdx.valid := True
        upInstrDecEtc.sprEvenSbIdx.payload := (
          Cat(
            False,
            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1)
          ).asUInt
        )
      //} otherwise {
        // it doesn't matter (from a logic perspective) if we read from
        // both
        upInstrDecEtc.sprOddSbIdx.valid := True
        upInstrDecEtc.sprOddSbIdx.payload := (
          Cat(
            False,
            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1)
          ).asUInt
        )
      //}
    }
    //def writeSpr(
    //  sprIdx: UInt,
    //  cond: Bool,
    //): Option[(UInt, Bool, Boolean)] = (
    //  Some(
    //    (sprIdx, cond, true)
    //  )
    //)
    def writeSprFlags(cond: Bool): Option[(UInt, Bool, Boolean)] = (
      Some(
        (FlareCpuInstrEncConst.sprFlagsIdx, cond, true)
      )
    )
    def markInstrInvalid(): Unit = {
      upInstrDecEtc.haveFullInstr := True 
      upInstrDecEtc.isInvalid := True
      upInstrDecEtc.decOp := FlareCpuInstrDecOp.bubble

      //upInstrDecEtc.decodeTemp.indexRaRbValid := False
      //upInstrDecEtc.decodeTemp.indexRaSimmValid := False
      //upInstrDecEtc.decodeTemp.preLpreValid := False
      //upInstrDecEtc.decodeTemp.preValid := False
      //upInstrDecEtc.decodeTemp.lpreValid := False
      //upInstrDecEtc.decodeTemp.assignFromBits(B"5'd0")

      upInstrDecEtc.decodeTemp := upInstrDecEtc.decodeTemp.getZero
      disableRegWrites()
    }
    def disableGprWrites(): Unit = {
      //--------
      upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
      upInstrDecEtc.gprFpRaIdx.valid := False
      upInstrDecEtc.gprOddNonSpRaIdx.valid := False
      upInstrDecEtc.gprSpRaIdx.valid := False
      //--------
      upInstrDecEtc.wrGprEvenNonFpRaIdx := False
      upInstrDecEtc.wrGprFpRaIdx := False
      upInstrDecEtc.wrGprOddNonSpRaIdx := False
      upInstrDecEtc.wrGprSpRaIdx := False
      //--------
    }
    def disableSprEvenWrite(): Unit = {
      //--------
      upInstrDecEtc.sprEvenSaIdx.valid := False
      //--------
      upInstrDecEtc.wrSprEvenSaIdx := False
      //--------
    }
    def disableSprOddWrite(): Unit = {
      //--------
      upInstrDecEtc.sprOddSaIdx.valid := False
      //--------
      upInstrDecEtc.wrSprOddSaIdx := False
      //--------
    }
    def disableSprWrites(): Unit = {
      disableSprEvenWrite()
      disableSprOddWrite()
    }
    def disableRegWrites(): Unit = {
      disableGprWrites()
      disableSprWrites()
    }
    //def markInstrNotFull(): Unit = {
    //}
    //setRegsMain()
    //if (optFormalTest == FlareCpuParams.enumFormalTestMain) {
    //  //when (pastValidAfterReset) {
    //    when (past(up.isFiring)) {
    //      cover(
    //        past(
    //          rMultiCycleState
    //          === MultiCycleState.PRIMARY
    //        ) && past(
    //          upInstrEnc.g0Pre.grp
    //          === FlareCpuInstrEncConst.g0Grp
    //        ) && past(
    //          upInstrEnc.g0LpreHi.subgrp
    //          === FlareCpuInstrEncConst.g0LpreSubgrp
    //        ) && (
    //          rMultiCycleState
    //          === MultiCycleState.LPRE_SIMM_LO
    //        )
    //      )
    //    }
    //  //}
    //}

    switch (rMultiCycleState) {
      is (MultiCycleState.PRIMARY) {
    //when (rSavedUpInstrDecEtc.decOp =/= FlareCpuInstrDecOp.lpreSimmHi) {
        disableRegWrites() // just do this to start with
        switch (upInstrEnc.g0Pre.grp) {
          is (FlareCpuInstrEncConst.g0Grp) {
            when (
              upInstrEnc.g0Pre.subgrp
              === FlareCpuInstrEncConst.g0PreSubgrp
            ) {
              when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
                upInstrDecEtc.decodeTemp.preLpreValid := True
                upInstrDecEtc.decodeTemp.preValid := True
                upInstrDecEtc.isInvalid := False
                upInstrDecEtc.haveFullInstr := False
                //upInstrDecEtc.fullgrp := (
                //  FlareCpuInstrFullgrpDec.g0Pre
                //)
                upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0Pre
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.preSimm
                //upInstrDecEtc.fullSimm := (
                //  upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
                //)
                val tempSimm = SInt(params.mainWidth bits)
                tempSimm := (
                  upInstrEnc.g0Pre.simm.asSInt.resized
                )
                upInstrDecEtc.fullSimm := (
                  tempSimm.asUInt
                )
                // I believe the Binutils port handles unsigned
                // immediates as signed when there's `pre`
                // TODO: check whether that's the case
                //upInstrDecEtc.fullImm := (
                //  upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
                //)
                upInstrDecEtc.fullImm := (
                  //upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
                  tempSimm.asUInt
                )
                disableRegWrites()
              } otherwise {
                markInstrInvalid()
              }
            } elsewhen (
              upInstrEnc.g0LpreHi.subgrp
              === FlareCpuInstrEncConst.g0LpreSubgrp
            ) {
              rMultiCycleState := MultiCycleState.LPRE_SIMM_LO
              upInstrDecEtc.isInvalid := False
              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0LpreHi
              upInstrDecEtc.haveFullInstr := False
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lpreSimmHi
              disableRegWrites()
              val tempSimm = SInt(params.mainWidth bits)
              tempSimm := (
                upInstrEnc.g0LpreHi.simmHi.asSInt.resized
              )
              upInstrDecEtc.fullSimm := (
                tempSimm.asUInt
              )
              upInstrDecEtc.fullImm := (
                //upInstrEnc.g0LpreHi.simmHi.asSInt.resized.asUInt
                tempSimm.asUInt
              )
            } elsewhen (
              upInstrEnc.g0Atomic.subgrp
              === FlareCpuInstrEncConst.g0AtomicSubgrp
            ) {
              upInstrDecEtc.fwl := upInstrEnc.g0Atomic.l
              finishInstr()
              disableRegWrites()
              switch (Cat(
                upInstrDecEtc.fwl,
                rSavedUpInstrDecEtc.decodeTemp.indexRaRbValid,
              )) {
                is (B"00") {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchg
                }
                is (B"01") {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchgLock
                }
                is (B"10") {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchg
                }
                is (B"11") {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchgLock
                }
              }
              //when (
              //  rSavedUpInstrDecEtc.indexRaRbValid
              //  //|| rSavedUpInstrDecEtc.indexRaSimmValid
              //) {
              //  when (upInstrDecEtc.fwl) {
              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchgLock
              //  } otherwise {
              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchg
              //  }
              //} otherwise {
              //  when (upInstrDecEtc.fwl) {
              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchgLock
              //  } otherwise {
              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchg
              //  }
              //}
              //when (upInstrDecEtc.) {
              //}
            } otherwise {
              markInstrInvalid()
              //canIrq := False
            }
          }
          is (FlareCpuInstrEncConst.g1Grp) {
            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g1
            when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
              //upInstrDecEtc.fullSimm := (
              //  upInstrEnc.g1.imm.asSInt.resized.asUInt
              //)
              val tempSimm = SInt(params.mainWidth bits)
              tempSimm := (
                upInstrEnc.g1.imm.asSInt.resized
              )
              upInstrDecEtc.fullSimm := (
                tempSimm.asUInt
              )
              upInstrDecEtc.fullImm := (
                upInstrEnc.g1.imm.resized
              )
            } otherwise {
              upInstrDecEtc.fullSimm := Cat(
                rSavedUpInstrDecEtc.fullSimm,
                upInstrEnc.g1.imm,
              ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
              upInstrDecEtc.fullImm := Cat(
                rSavedUpInstrDecEtc.fullImm,
                upInstrEnc.g1.imm,
              ).asUInt(upInstrDecEtc.fullImm.bitsRange)
            }
            upInstrDecEtc.fwl := False
            switch (upInstrEnc.g1.op) {
              is (FlareCpuInstrG1EncOp.addRaS5) {
                // Opcode 0x0: add rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.addRaPcS5) {
                // Opcode 0x1: add rA, pc, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaPcSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.addRaSpS5) {
                // Opcode 0x2: add rA, sp, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.addRaFpS5) {
                // Opcode 0x3: add rA, fp, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.cmpRaS5) {
                // Opcode 0x4: cmp rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpRaSimm
                //disableGprWrites()
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=writeSprFlags(True)
                )
              }
              is (FlareCpuInstrG1EncOp.cpyRaS5) {
                // Opcode 0x5: cpy rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.lslRaI5) {
                // Opcode 0x6: lsl rA, #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaImm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.lsrRaI5) {
                // Opcode 0x7: lsr rA, #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaImm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.asrRaI5) {
                // Opcode 0x8: asr rA, #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaImm
              }
              is (FlareCpuInstrG1EncOp.andRaS5) {
                // Opcode 0x9: and rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.orrRaS5) {
                // Opcode 0xa: orr rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.xorRaS5) {
                // Opcode 0xb: xor rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaSimm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.zeRaI5) {
                // Opcode 0xc: ze rA, #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.zeRaImm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.seRaI5) {
                // Opcode 0xd: se rA, #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.seRaImm
                finishInstr()
              }
              is (FlareCpuInstrG1EncOp.swiRaS5) {
                // Opcode 0xe: swi rA, #simm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.swiRaSimm
                finishInstr(writeGpr=None)
                //disableRegWrites()
              }
              is (FlareCpuInstrG1EncOp.swiI5) {
                // Opcode 0xf: swi #imm5
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.swiImm
                finishInstr(writeGpr=None)
                //disableRegWrites()
              }
            }
          }
          is (FlareCpuInstrEncConst.g2Grp) {
            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g2
            upInstrDecEtc.fwl := upInstrEnc.g2.f
            switch (upInstrEnc.g2.op) {
              is (FlareCpuInstrG2EncOp.addRaRb) {
                // Opcode 0x0: add rA, rB
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaRbFlags
                //}
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
              }
              is (FlareCpuInstrG2EncOp.subRaRb) {
                // Opcode 0x1: sub rA, rB
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.subRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.subRaRbFlags
                //}
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
              }
              is (FlareCpuInstrG2EncOp.addRaSpRb) {
                //--------
                // Opcode 0x2: add rA, sp, rB
                // we can always read `sp` since it's in its own
                // `regFile` chunk
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpRbFlags
                //}
                //--------
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
                //--------
              }
              is (FlareCpuInstrG2EncOp.addRaFpRb) {
                //--------
                // Opcode 0x3: add rA, fp, rB
                // we can always read `fp` since it's in its own
                // `regFile` chunk
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpRbFlags
                //}
                //--------
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
                //--------
              }
              is (FlareCpuInstrG2EncOp.cmpRaRb) {
                //--------
                // Opcode 0x4: cmp rA, rB
                //--------
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpRaRb
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=writeSprFlags(True)
                )
                //disableGprWrites()
                //--------
              }
              is (FlareCpuInstrG2EncOp.cpyRaRb) {
                //--------
                // Opcode 0x5: cpy rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaRbFlags
                //}
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
              }
              is (FlareCpuInstrG2EncOp.lslRaRb) {
                //--------
                // Opcode 0x6: lsl rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaRbFlags
                //}
                finishInstr(
                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
                )
              }
              is (FlareCpuInstrG2EncOp.lsrRaRb) {
                //--------
                // Opcode 0x7: lsr rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
                //--------
              }
              is (FlareCpuInstrG2EncOp.asrRaRb) {
                //--------
                // Opcode 0x8: asr rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
                //--------
              }
              is (FlareCpuInstrG2EncOp.andRaRb) {
                //--------
                // Opcode 0x9: and rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
                //--------
              }
              is (FlareCpuInstrG2EncOp.orrRaRb) {
                //--------
                // Opcode 0xa: orr rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
                //--------
              }
              is (FlareCpuInstrG2EncOp.xorRaRb) {
                //--------
                // Opcode 0xb: xor rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
                //--------
              }
              is (FlareCpuInstrG2EncOp.adcRaRb) {
                //--------
                // Opcode 0xc: adc rA, rB
                //--------
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.adcRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.adcRaRbFlags
                //}
                //--------
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
              }
              is (FlareCpuInstrG2EncOp.sbcRaRb) {
                // Opcode 0xd: sbc rA, rB
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRbFlags
                //}
                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
              }
              is (FlareCpuInstrG2EncOp.cmpbcRaRb) {
                // Opcode 0xe: cmpbc rA, rB
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbcRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbcRaRbFlags
                //}
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=writeSprFlags(True)
                )
                //disableGprWrites()
              }
              default {
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bubble
                markInstrInvalid()
              }
            }
          }
          is (FlareCpuInstrEncConst.g3Grp) {
            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g3
            when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
              val tempSimm = SInt(params.mainWidth bits)
              tempSimm := (
                upInstrEnc.g3.simm.resized
              )
              upInstrDecEtc.fullSimm := (
                tempSimm.asUInt
              )
              //upInstrDecEtc.fullImm := (
              //  upInstrEnc.g3.simm.resized
              //)
            } otherwise {
              upInstrDecEtc.fullSimm := Cat(
                rSavedUpInstrDecEtc.fullSimm,
                upInstrEnc.g3.simm,
              ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
            }
            upInstrDecEtc.fullPcrelSimm := (
              upModExt.regPc
              + upInstrDecEtc.fullSimm
            )
            switch (upInstrEnc.g3.op) {
              is (FlareCpuInstrG3EncOp.blS9) {
                // Opcode 0x0: bl simm
                finishInstr(
                  //isBlJl=true
                  writeGpr=Some((FlareCpuInstrEncConst.gprLrIdx, true))
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.blSimm
              }
              is (FlareCpuInstrG3EncOp.braS9) {
                // Opcode 0x1: bra simm
                finishInstr(
                  writeGpr=None,
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.braSimm
              }
              is (FlareCpuInstrG3EncOp.beqS9) {
                // Opcode 0x2: beq simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.beqSimm
              }
              is (FlareCpuInstrG3EncOp.bneS9) {
                // Opcode 0x3: bne simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bneSimm
              }
              is (FlareCpuInstrG3EncOp.bmiS9) {
                // Opcode 0x4: bmi simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bmiSimm
              }
              is (FlareCpuInstrG3EncOp.bplS9) {
                // Opcode 0x5: bpl simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bplSimm
              }
              is (FlareCpuInstrG3EncOp.bvsS9) {
                // Opcode 0x6: bvs simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvsSimm
              }
              is (FlareCpuInstrG3EncOp.bvcS9) {
                // Opcode 0x7: bvc simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvcSimm
              }
              is (FlareCpuInstrG3EncOp.bgeuS9) {
                // Opcode 0x8: bgeu simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgeuSimm
              }
              is (FlareCpuInstrG3EncOp.bltuS9) {
                // Opcode 0x9: bltu simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltuSimm
              }
              is (FlareCpuInstrG3EncOp.bgtuS9) {
                // Opcode 0xa: bgtu simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtuSimm
              }
              is (FlareCpuInstrG3EncOp.bleuS9) {
                // Opcode 0xb: bleu simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bleuSimm
              }
              is (FlareCpuInstrG3EncOp.bgesS9) {
                // Opcode 0xc: bges simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgesSimm
              }
              is (FlareCpuInstrG3EncOp.bltsS9) {
                // Opcode 0xd: blts simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltsSimm
              }
              is (FlareCpuInstrG3EncOp.bgtsS9) {
                // Opcode 0xe: bgts simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtsSimm
              }
              is (FlareCpuInstrG3EncOp.blesS9) {
                // Opcode 0xf: bles simm
                finishInstr(
                  writeGpr=None,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.blesSimm
              }
            }
          }
          is (FlareCpuInstrEncConst.g4Grp) {
            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g4
            switch (upInstrEnc.g4.op) {
              is (FlareCpuInstrG4EncOp.jlRa) {
                finishInstr(
                  //isBlJl=true
                  writeGpr=Some((FlareCpuInstrEncConst.gprLrIdx, true)),
                  readGprRaAsRb=true,
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jlRa
              }
              is (FlareCpuInstrG4EncOp.jmpRa) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpRa
              }
              is (FlareCpuInstrG4EncOp.jmpIra) {
                finishInstr(rdWrSpr0=Some(
                  FlareCpuInstrEncConst.sprIraIdx, True, false
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpIra
              }
              is (FlareCpuInstrG4EncOp.reti) {
                finishInstr(
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprIraIdx, True, false,
                  ),
                  rdWrSpr1=Some(
                    FlareCpuInstrEncConst.sprIeIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.reti
              }
              is (FlareCpuInstrG4EncOp.ei) {
                finishInstr(rdWrSpr0=Some(
                  FlareCpuInstrEncConst.sprIeIdx, True, true
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ei
              }
              is (FlareCpuInstrG4EncOp.di) {
                finishInstr(rdWrSpr0=Some(
                  FlareCpuInstrEncConst.sprIeIdx, True, true
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.di
              }
              is (FlareCpuInstrG4EncOp.pushRaRb) {
                // Opcode 0x6: push rA, rB
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.pushRaRb
              }
              is (FlareCpuInstrG4EncOp.pushSaRb) {
                // Opcode 0x7: push sA, rB
                finishInstr(rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, false
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.pushSaRb
              }
              is (FlareCpuInstrG4EncOp.popRaRb) {
                // Opcode 0x8: pop rA, rB
                when (!(
                  (
                    upInstrEnc.g2.raIdx.lsb === upInstrEnc.g2.rbIdx.lsb
                    && (
                      upInstrEnc.g2.rbIdx
                      =/= FlareCpuInstrEncConst.gprSpIdx
                    )
                  ) || (
                    upInstrEnc.g2.raIdx === upInstrEnc.g2.rbIdx
                  )
                )) {
                  finishInstr()
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.popRaRb
                } otherwise {
                  markInstrInvalid()
                }
              }
              is (FlareCpuInstrG4EncOp.popSaRb) {
                // Opcode 0x9: pop sA, rB
                finishInstr(rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, true
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.popSaRb
              }
              is (FlareCpuInstrG4EncOp.popPcRb) {
                // Opcode 0xa: pop pc, rB
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.popPcRb
              }
              is (FlareCpuInstrG4EncOp.mulRaRb) {
                // Opcode 0xb: mul rA, rB
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.mulRaRb
              }
              is (FlareCpuInstrG4EncOp.udivmodRaRb) {
                // Opcode 0xc: udivmod rA, rB
                finishInstr(rdWrSpr0=Some(
                  FlareCpuInstrEncConst.sprLoIdx, True, true
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
              }
              is (FlareCpuInstrG4EncOp.sdivmodRaRb) {
                // Opcode 0xd: sdivmod rA, rB
                finishInstr(rdWrSpr0=Some(
                  FlareCpuInstrEncConst.sprLoIdx, True, true
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
              }
              is (FlareCpuInstrG4EncOp.lumulRaRb) {
                // Opcode 0xe: lumul rA, rB
                finishInstr(
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True, true
                  ),
                  rdWrSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lumulRaRb
              }
              is (FlareCpuInstrG4EncOp.lsmulRaRb) {
                // Opcode 0xf: lsmul rA, rB
                finishInstr(
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True, true
                  ),
                  rdWrSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsmulRaRb
              }
              is (FlareCpuInstrG4EncOp.udivmod64RaRb) {
                // Opcode 0x10: udivmod64 rA, rB
                finishInstr(
                  gprDual64=true,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True, true
                  ),
                  rdWrSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmod64RaRb
              }
              is (FlareCpuInstrG4EncOp.sdivmod64RaRb) {
                // Opcode 0x11: sdivmod64 rA, rB
                finishInstr(
                  gprDual64=true,
                  rdWrSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True, true,
                  ),
                  rdWrSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True, true,
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.sdivmod64RaRb
              }
              is (FlareCpuInstrG4EncOp.ldubRaRb) {
                // Opcode 0x12: ldub rA, [rB]
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldubRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.ldsbRaRb) {
                // Opcode 0x13: ldsb rA, [rB]
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldsbRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.lduhRaRb) {
                // Opcode 0x14: lduh rA, [rB]
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lduhRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.ldshRaRb) {
                // Opcode 0x15: ldsh rA, [rB]
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldshRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.ldrRaRb) {
                // Opcode 0x16: ldr rA, [rB]
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
              }
              //is (FlareCpuInstrG4EncOp.reserved17) {
              //  // Opcode 0x17: reserved
              //  //finishInstr()
              //  //upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
              //  markInstrInvalid()
              //}
              is (FlareCpuInstrG4EncOp.stbRaRb) {
                // Opcode 0x18: stb rA, [rB]
                finishInstr(writeGpr=None)
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.stbRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.sthRaRb) {
                // Opcode 0x19: sth rA, [rB]
                finishInstr(writeGpr=None)
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.sthRaRbLdst
              }
              is (FlareCpuInstrG4EncOp.strRaRb) {
                // Opcode 0x1a: str rA, [rB]
                finishInstr(writeGpr=None)
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.strRaRbLdst
              }
              //is (FlareCpuInstrG4EncOp.reserved1b) {
              //  // Opcode 0x1b: reserved
              //  //finishInstr()
              //  //upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
              //  markInstrInvalid()
              //}
              is (FlareCpuInstrG4EncOp.cpyRaSb) {
                // Opcode 0x1c: cpy rA, sB
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaSb
              }
              is (FlareCpuInstrG4EncOp.cpySaRb) {
                // Opcode 0x1d: cpy sA, rB
                finishInstr(
                  rdWrSpr0=Some(
                    upInstrEnc.g2.raIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpySaRb
              }
              is (FlareCpuInstrG4EncOp.cpySaSb) {
                // Opcode 0x1e: cpy sA, sB
                finishInstr(
                  rdWrSpr0=Some(
                    upInstrEnc.g2.raIdx, True, true
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpySaSb
              }
              default {
                markInstrInvalid()
              }
            }
          }
          is (FlareCpuInstrEncConst.g5Grp) {
            when (
              upInstrEnc.g5Sg0.subgrp
              === FlareCpuInstrEncConst.g5Sg0Subgrp
            ) {
              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g5Sg0
              when (!rSavedUpInstrDecEtc.decodeTemp.indexRaRbValid) {
                upInstrDecEtc.decodeTemp.indexRaRbValid := True
                upInstrDecEtc.haveFullInstr := False
              } otherwise {
                markInstrInvalid()
              }
            } elsewhen (
              upInstrEnc.g5Sg1.subgrp
              === FlareCpuInstrEncConst.g5Sg1Subgrp
            ) {
              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g5Sg1
              upInstrDecEtc.haveFullInstr := False
              when (!rSavedUpInstrDecEtc.decodeTemp.indexRaSimmValid) {
                upInstrDecEtc.decodeTemp.indexRaSimmValid := True
                upInstrDecEtc.haveFullInstr := False
              } otherwise {
                markInstrInvalid()
              }
            } otherwise {
              //upInstrDecEtc.isInvalid := True
              markInstrInvalid()
            }
          }
          is (FlareCpuInstrEncConst.g6Grp) {
            //upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g6
            markInstrInvalid()
          }
          is (FlareCpuInstrEncConst.g7Grp) {
            ////psIdHaltIt := True
            //cId.haltIt()
            //myDoHaltIt()
            //rMultiCycleState := MultiCycleState.G7_SUB_DECODE
          }
        }
      }
    //} otherwise { // when (the previous instruction was lpreSimmHi)
      is (MultiCycleState.LPRE_SIMM_LO) {
        rMultiCycleState := MultiCycleState.PRIMARY
        upInstrDecEtc.isInvalid := False
        upInstrDecEtc.haveFullInstr := False
        upInstrDecEtc.fullgrp := (
          FlareCpuInstrFullgrpDec.g0LpreLo
        )
        upInstrDecEtc.fwl := False
        upInstrDecEtc.decOp := FlareCpuInstrDecOp.lpreSimmLo
        upInstrDecEtc.fullSimm := Cat(
          rSavedUpInstrDecEtc.fullSimm,
          upInstrEnc.g0LpreLo,
        ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
        upInstrDecEtc.fullImm := Cat(
          rSavedUpInstrDecEtc.fullImm,
          upInstrEnc.g0LpreLo,
        ).asUInt(upInstrDecEtc.fullImm.bitsRange)
      }
      is (MultiCycleState.G7_SUB_DECODE) {
        rMultiCycleState := MultiCycleState.PRIMARY
        when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
          //upInstrDecEtc.fullSimm := (
          //  upInstrEnc.g7Sg0110.imm.asSInt.resized.asUInt
          //)
          val tempSimm = SInt(params.mainWidth bits)
          tempSimm := (
            upInstrEnc.g7Sg0110.imm.asSInt.resized
          )
          upInstrDecEtc.fullSimm := (
            tempSimm.asUInt
          )
          upInstrDecEtc.fullImm := (
            upInstrEnc.g7Sg0110.imm.resized
          )
        } otherwise {
          upInstrDecEtc.fullSimm := Cat(
            rSavedUpInstrDecEtc.fullSimm,
            upInstrEnc.g7Sg0110.imm,
          ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
          upInstrDecEtc.fullImm := Cat(
            rSavedUpInstrDecEtc.fullImm,
            upInstrEnc.g7Sg0110.imm,
          ).asUInt(upInstrDecEtc.fullImm.bitsRange)
        }
        when (
          upInstrEnc.g7Sg00.subgrp
          === FlareCpuInstrEncConst.g7Sg00Subgrp
        ) {
          upInstrDecEtc.fwl := upInstrEnc.g7Sg00.w
          switch (upInstrEnc.g7Sg00.op) {
            is (FlareCpuInstrG7Sg00FullOpEnc.cmpbRaRb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=writeSprFlags(True),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.lsrbRaRb) {
              finishInstr()
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrbRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.asrbRaRb) {
              finishInstr()
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrbRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.invalid0) {
              markInstrInvalid()
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.cmphRaRb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=writeSprFlags(True),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmphRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.lsrhRaRb) {
              finishInstr()
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrhRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.asrhRaRb) {
              finishInstr()
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrhRaRb
            }
            is (FlareCpuInstrG7Sg00FullOpEnc.invalid1) {
              markInstrInvalid()
            }
          }
        } elsewhen (
          upInstrEnc.g7Sg010.subgrp
          === FlareCpuInstrEncConst.g7Sg010Subgrp
        ) {
          switch (upInstrEnc.g7Sg010.op) {
            is (FlareCpuInstrG7Sg010EncOp.ldrSaRb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, true
                ),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrSaRbLdst
            }
            is (FlareCpuInstrG7Sg010EncOp.ldrSaSb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, true
                ),
                //rdWrSpr1=Some(
                //  upInstrEnc.g2.rbIdx, True, false
                //),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrSaSbLdst
            }
            is (FlareCpuInstrG7Sg010EncOp.strSaRb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, false
                ),
                //rdWrSpr1=Some(
                //  upInstrEnc.g2.rbIdx, True, false
                //),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.strSaRbLdst
            }
            is (FlareCpuInstrG7Sg010EncOp.strSaSb) {
              finishInstr(
                writeGpr=None,
                rdWrSpr0=Some(
                  upInstrEnc.g2.raIdx, True, false
                ),
                //rdWrSpr1=Some(
                //  upInstrEnc.g2.rbIdx, True, false
                //),
              )
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.strSaSbLdst
            }
          }
        } elsewhen (
          upInstrEnc.g7Sg0110.subgrp
          === FlareCpuInstrEncConst.g7Sg0110Subgrp
        ) {
          upInstrDecEtc.decOp := FlareCpuInstrDecOp.icreloadRaSimm
          finishInstr(
            writeGpr=None
          )
          //disableRegWrites()
        } elsewhen (
          upInstrEnc.g7Sg01110.subgrp
          === FlareCpuInstrEncConst.g7Sg01110Subgrp
        ) {
          upInstrDecEtc.decOp := FlareCpuInstrDecOp.icflush
          finishInstr(writeGpr=None)
          //disableRegWrites()
        } otherwise {
          markInstrInvalid()
        }
      }
    //}
    }
  }
  //}
  when (io.ibus.ready) {
    upInstrEnc.g0Pre.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g0LpreHi.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g0LpreLo.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g0Atomic.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g1.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g2.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g3.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g4.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g5Sg0.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g5Sg1.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g7Sg00.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g7Sg010.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g7Sg0110.assignFromBits(io.ibus.devData.asBits)
    upInstrEnc.g7Sg01110.assignFromBits(io.ibus.devData.asBits)
  } otherwise { // when (!io.ibus.ready)
    //duplicateIt()
    // `cId.haltIt()` could be wrong unless EX captures values when EX has
    // `up.isValid`. So I think that means EX needs to capture the
    // outputs of ID upon an `EX.up.isValid` if EX is going to stall.
    ////psIdHaltIt := True
    //cId.haltIt()
    myDoHaltIt()
    //cId.haltIt()
    //psIdHaltIt := True
    //cId.terminateIt()
  }
  //when (psExSetPc.fire) {
  //}
  //--------
  //if (params.formal) {
  //val myDidReset = Bool()
  ////assumeInitial(!myDidReset)
  ////assume(!myDidReset)
  ////when (pastValid) {
  ////  //when (ClockDomain.isResetActive) {
  ////  //  //rDidReset := True
  ////  //}
  ////}
  //val rDidReset = RegNext(myDidReset)
  //myDidReset := rDidReset
  //GenerationFlags.formal {
    //println("testificate\n")
  if (myFormalMain) {
    //when (!pastValid) {
    //  //assume(!myDidReset)
    //  myDidReset := False
    //} otherwise {
    //  //when (ClockDomain.isResetActive) {
    //  //}
    //  when (
    //    past(ClockDomain.current.isResetActive)
    //    && !ClockDomain.current.isResetActive
    //  ) {
    //    myDidReset := True
    //    //assume(myDidReset)
    //  }
    //  //myDidReset := True
    //}
    //assumeInitial(
    //  rMultiCycleState === MultiCycleState.PRIMARY
    //)
    //assumeInitial(
    //  !rDidHandleG7SubDecode
    //)
    //assumeInitial(
    //  ClockDomain.isResetActive
    //)
    //when (ClockDomain.isResetActive) {
    //  assume(!RegNext(ClockDomain.isResetActive))
    //}
    //cover(
    //  pastValidAfterReset
    //  && (
    //    RegNextWhen(True, ClockDomain.isResetActive) init(False)
    //  )
    //  && !ClockDomain.isResetActive
    //)
    //cover(
    //  
    //  //pastValidAfterReset
    //  ////&& (
    //  ////  RegNextWhen(True, ClockDomain.isResetActive) init(False)
    //  ////)
    //  //pastValid
    //  //&& 
    //  myDidReset
    //  //&& !ClockDomain.isResetActive
    //)
    cover(rMultiCycleState === MultiCycleState.LPRE_SIMM_LO)
    when (
      !pastValidAfterReset()
    ) {
      //assume(!rDidHandleG7SubDecode)
    } elsewhen (
      pastValidAfterReset()
      ////&& (
      ////  RegNextWhen(True, ClockDomain.isResetActive) init(False)
      ////)
      //pastValid
      //&& 
      //myDidReset
      //&& !ClockDomain.isResetActive
    ) {
      ////cover(rMultiCycleState === MultiCycleState.LPRE_SIMM_LO)
      //cover(
      //  up.isFiring
      //  && io.ibus.ready
      //)
      ////cover(
      ////  up.isFiring
      ////  && !psIdHaltIt
      ////)
      ////when (up.isFiring) {
      ////  assume(
      ////    io.ibus.ready
      ////  )
      ////  //assert(
      ////  //  !psIdHaltIt
      ////  //)
      ////}
      when (!io.ibus.ready) {
        assert(
          !up.isFiring
        )
      }
      when (
        //up.isValid
        //&& 
        !past(up.isFiring)
        && io.ibus.ready
      ) {
        //assert(stable(io.ibus.ready))
        assume(stable(io.ibus.ready))
      }
      assert(
        rMultiCycleState.asBits.asUInt =/= 0x3
      )
      when (past(io.ibus.valid)) {
        when (io.ibus.ready) {
          //cover(
          //  up.isValid
          //)
          cover(
            up.isFiring
          )
          assert(
            //up.isFiring
            up.isValid
          )
          when (!io.ibus.valid) {
            assume(!RegNext(io.ibus.ready))
          }
        }
      }
      when (
        !past(up.isFiring)
      ) {
        //cover(io.ibus.ready)
      }
      //when (
      //  up.isValid
      //  && io.ibus.ready
      //  //&& 
      //) {
      //  assert(
      //  )
      //}
      //otherwise { // when (!up.isFiring)
      //}
      //cover(
      //  past(up.isFiring)
      //  && (
      //    past(rMultiCycleState) === MultiCycleState.G7_SUB_DECODE
      //  ) && (
      //    rMultiCycleState === MultiCycleState.PRIMARY
      //  )
      //)
      when (
        //(
        //  RegNextWhen(True, up.isFiring) init(False)
        //) && (
          //RegNext(up.isFiring) init(False)
        //)
        past(up.isFiring)
      ) {
        assert(
          !rDidHandleG7SubDecode
        )
        when (
          past(rMultiCycleState)
          === MultiCycleState.G7_SUB_DECODE
        ) {
          assert(
            rMultiCycleState === MultiCycleState.PRIMARY
          )
        }
      }
      when (up.isValid) {
        switch (rMultiCycleState) {
          is (MultiCycleState.PRIMARY) {
            assert(
              !rDidHandleG7SubDecode
            )
            when (
              upInstrEnc.g0Pre.grp === FlareCpuInstrEncConst.g7Grp
              && !rDidHandleG7SubDecode
            ) {
              //assert(
              //  psIdHaltIt
              //)
              assert(
                !cId.up.isReady
              )
              assert(
                !cId.down.isValid
              )
            }
          }
          is (MultiCycleState.LPRE_SIMM_LO) {
            when (RegNextWhen(True, up.isFiring) init(False)) {
              assert(
                !rDidHandleG7SubDecode
              )
            }
          }
          is (MultiCycleState.G7_SUB_DECODE) {
            when (RegNextWhen(True, up.isFiring) init(False)) {
              assert(
                rDidHandleG7SubDecode
              )
            }
          }
        }
      }
      //cover(
      //  past(up.isFiring)
      //  && (
      //    rMultiCycleState === MultiCycleState.LPRE_SIMM_LO
      //  ) && (
      //    past(rMultiCycleState) === MultiCycleState.PRIMARY
      //  )
      //)
      when (
        past(up.isFiring)
        && (
          rMultiCycleState === MultiCycleState.LPRE_SIMM_LO
        )
      ) {
        assert(
          past(rMultiCycleState) === MultiCycleState.PRIMARY
        )
      }
    }
    //when (
    //  !(
    //    RegNextWhen(psExSetPc.fire, up.isFiring) init(False)
    //  )
    //) {
    //}
    //cover (
    //  upModExt.regPc
    //  === (
    //    RegNextWhen(upModExt.regPc, up.isFiring)
    //    + (params.instrMainWidth / 8)
    //  )
    //)
  }
  //}
  //}
}
//case class FlareCpuExSetRegExtFuncArgs(
//  someIdx: Int,
//) {
//}
case class FlareCpuPipeStageEx(
  params: FlareCpuParams,
  io: FlareCpuIo,
  psExSetPc: Flow[UInt],
  regFileWordCountArr: ArrayBuffer[Int],
  //nextPrevTxnWasHazardVec: Vec[Bool], // nextPrevTxnWasHazardVec
  //rPrevTxnWasHazardVec: Vec[Bool],  // rPrevTxnWasHazardVec
  //rPrevTxnWasHazardAny: Bool,       // rPrevTxnWasHazardAny
  //outpVec: Vec[FlareCpuPipeMemModType[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModExtType,
  //]], // outp
  //inpVec: Vec[FlareCpuPipeMemModType[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModExtType,
  //]], // inp
  //cMid0Front: CtrlLink, // mod.front.cMid0Front
  //modFront: Node,     // io.modFront
  //tempModFrontPayloadVec: Vec[FlareCpuPipeMemModType[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModExtType,
  //]], // io.tempModFrontPayload
  //getMyModMemWordFunc: (Int) => UInt,
  //ydxArg: Int,    // ydx
  doModParams: PipeMemRmwDoModInModFrontFuncParams[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      FlareCpuPipeMemModExtType,
    ],
  ],
  optFormalTest: Int=(
    FlareCpuParams.enumFormalTestNone
  ),
) extends Area {
  //--------
  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
  def enumRegFileLim = FlareCpuParams.enumRegFileLim
  //--------
  def enumFormalTestNone = FlareCpuParams.enumFormalTestNone
  def enumFormalTestMain = FlareCpuParams.enumFormalTestMain
  //--------
  //def nextPrevTxnWasHazardVec = doModParams.nextPrevTxnWasHazardVec
  //def rPrevTxnWasHazardVec = doModParams.rPrevTxnWasHazardVec
  def nextPrevTxnWasHazard = doModParams.nextPrevTxnWasHazardVec(0)
  def rPrevTxnWasHazard = doModParams.rPrevTxnWasHazardVec(0)
  //def rPrevTxnWasHazardAny = doModParams.rPrevTxnWasHazardAny
  def outpVec = doModParams.outpVec
  def inpVec = doModParams.inpVec
  def cMid0Front = doModParams.cMid0Front
  def modFront = doModParams.modFront
  def getMyRdMemWord(ydx: Int) = (
    doModParams.getMyRdMemWordFunc(ydx)
  )
  //--------
  def myFormal = (
    optFormalTest != FlareCpuParams.enumFormalTestNone
  )
  //def myFormalPipeMain = (
  //  optFormalTest == FlareCpuParams.enumFormalTestPipeMain
  //)
  if (doModParams.ydx == 0) {
    def memArrSize = regFileWordCountArr.size
    for (ydx <- 0 until memArrSize) {
      //--------
      outpVec(ydx) := inpVec(ydx)
      outpVec(ydx).allowOverride
      //--------
      // Set every `modMemWordValid` to `False` so that it can be set back
      // to `True` upon a register being written.
      //--------
      // TODO: In `PipeMemRmw`, right before the call to
      // `doModInModFrontFunc()`, `modMemWordValid` is set to `True`, which
      // is what we're working around here.
      // We need to verify that we can do things this way!
      // I suppose I'll find out with help from the formal verification 
      // tools.
      //--------
      // NOTE: DON'T default to setting `outpVec(ydx).myExt.valid := False`
      //--------
      outpVec(ydx).myExt.modMemWordValid := False
      //--------
    }
    val nextHaltItState = KeepAttribute(
      FlareCpuPsExHaltItState()
    )//.setName("psEx_nextHaltItState")
    val rHaltItState = KeepAttribute(
      RegNext(nextHaltItState)
      init(FlareCpuPsExHaltItState.IDLE)
    )//.setName("psEx_rHaltItState")
    nextHaltItState := rHaltItState

    //val rSavedInpVec = Reg(Flow(cloneOf(inpVec)))
    //rSavedInpVec.init(rSavedInpVec.getZero)
    //when (cMid0Front.up.isValid) {
    //  rSavedInpVec.valid := True
    //  //rSavedInpVec.payload := 
    //}
    //when (cMid0Front.up.isFiring) {
    //  rSavedInpVec := rSavedInpVec.getZero
    //}
    ////val rSavedInpModExt = Reg(Flow(cloneOf(inpVec(0).modExt)))
    ////rSavedInpModExt.init(rSavedInpModExt.getZero)
    //val myInpVec = cloneOf(inpVec)
    ////def inpModExt = inpVec(0).modExt
    ////def inpInstrDecEtc = inpModExt.instrDecEtc
    ////val nextHaltItState = KeepAttribute(
    ////)
    //val inpModExt = cloneOf(inpVec(0).modExt)
    //val inpInstrDecEtc = inpModExt.instrDecEtc

    //object MultiCycleState
    //extends SpinalEnum(defaultEncoding=binarySequential) {
    //  val
    //    PRIMARY,
    //    PREV_INSTR_MEM_ACCESS
    //    = newElement();
    //}
    //val rMultiCycleState = (
    //  KeepAttribute(
    //    //Reg(Bool())
    //    //init(False)
    //    Reg(MultiCycleState())
    //    init(MultiCycleState.PRIMARY)
    //  )
    //)
    def myInpModExt = inpVec(0).modExt
    def myInstrEnc = myInpModExt.instrEnc
    def myInstrDecEtc = myInpModExt.instrDecEtc
    def setGprRa32(
      value: UInt,
      doAssertValid: Boolean,
      //retYdx: UInt,
      //optExtraFunc: Option[(Int) => Unit],
    ): Unit = {
      //--------
      //var ret = new ArrayBuffer[UInt]()
      //--------
      switch (
        //inpInstrDecEtc.gprRaSel
        myInstrDecEtc.gprRaSel
      ) {
        //def outerMyModMemWord(ydx: Int) = (
        //  upExt(idx=1, regFileSlice=ydx).modMemWord
        //)
        def outerMyValid(ydx: Int) = (
          outpVec(ydx).myExt.valid
        )
        def outerMyModMemWordValid(ydx: Int) = (
          outpVec(ydx).myExt.modMemWordValid
        )
        def outerMyModMemWord(ydx: Int) = (
          outpVec(ydx).myExt.modMemWord
        )
        val myZero = U(s"${params.mainWidth}'d0")
        is (FlareCpuGprSelect.gprEvenNonFp) {
          for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
            //def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
            def myValid = outerMyValid(ydx=ydx)
            def myModMemWordValid = outerMyModMemWordValid(ydx=ydx)
            def myModMemWord = outerMyModMemWord(ydx=ydx)
            if (ydx == enumRegFileGprEvenNonFp) {
              if (doAssertValid) {
                myValid := True
              }
              myModMemWordValid := True
              myModMemWord := value
              //ret += ydx
            } else {
              //myModMemWordValid := False
              myModMemWord := myZero
            }
          }
          //retYdx := enumRegFileGprEvenNonFp
        }
        is (FlareCpuGprSelect.gprFp) {
          for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
            //def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
            def myValid = outerMyValid(ydx=ydx)
            def myModMemWordValid = outerMyModMemWordValid(ydx=ydx)
            def myModMemWord = outerMyModMemWord(ydx=ydx)
            if (ydx == enumRegFileGprFp) {
              if (doAssertValid) {
                myValid := True
              }
              myModMemWordValid := True
              myModMemWord := value
              //ret += ydx
            } else {
              //myModMemWordValid := False
              myModMemWord := myZero
            }
          }
          //retYdx := enumRegFileGprFp
        }
        is (FlareCpuGprSelect.gprOddNonSp) {
          for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
            //def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
            def myValid = outerMyValid(ydx=ydx)
            def myModMemWordValid = outerMyModMemWordValid(ydx=ydx)
            def myModMemWord = outerMyModMemWord(ydx=ydx)
            if (ydx == enumRegFileGprOddNonSp) {
              if (doAssertValid) {
                myValid := True
              }
              myModMemWordValid := True
              myModMemWord := value
              //ret += ydx
            } else {
              //myModMemWordValid := False
              myModMemWord := myZero
            }
          }
          //retYdx := enumRegFileGprOddNonSp
        }
        is (FlareCpuGprSelect.gprSp) {
          for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
            def myValid = outerMyValid(ydx=ydx)
            def myModMemWordValid = outerMyModMemWordValid(ydx=ydx)
            def myModMemWord = outerMyModMemWord(ydx=ydx)
            if (ydx == enumRegFileGprSp) {
              if (doAssertValid) {
                myValid := True
              }
              myModMemWordValid := True
              myModMemWord := value
              //ret += ydx
            } else {
              //myModMemWordValid := False
              myModMemWord := myZero
            }
          }
          //retYdx := enumRegFileGprSp
        }
      }
      //--------
      //ret
      //--------
    }
    def setGprRa64(
      valueHi: UInt,
      valueLo: UInt,
      doAssertValid: Boolean,
      //optExtraFunc: Option[(Int) => Unit],
    ): Unit = {
      //--------
      //var ret = new ArrayBuffer[Int]()
      //--------
      switch (myInstrDecEtc.gprRa64IsNonFpSp) {
        //for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
        //  def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
          def myOutpExt(
            ydx: Int
          ) = (
            outpVec(ydx).myExt
          )
          def myModMemWord(
            ydx: Int,
          ) = (
            //upExt(idx=1, regFileSlice=ydx).modMemWord
            myOutpExt(ydx=ydx).modMemWord
          )
          val myZero = U(s"${params.mainWidth}'d0")

          is (False) { // {non-fp, non-sp} 64-bit register pair
            for (
              zdx <- List[Int](
                enumRegFileGprEvenNonFp,
                enumRegFileGprOddNonSp,
              )
            ) {
              //myOutpExt(enumRegFileGprEvenNonFp).valid := True
              //myOutpExt(enumRegFileGprOddNonSp).valid := True
              if (doAssertValid) {
                myOutpExt(zdx).valid := True
              }
              myOutpExt(zdx).modMemWordValid := True
            }
            //ret ++= List[Int](
            //  enumRegFileGprEvenNonFp,
            //  enumRegFileGprOddNonSp
            //)
            //outpVec(enumRegFileGprFp).myExt.modMemWordValid := (
            //  False
            //)
            //outpVec(enumRegFileGprSp).myExt.modMemWordValid := (
            //  False
            //)
            myModMemWord(ydx=enumRegFileGprEvenNonFp) := valueHi
            myModMemWord(ydx=enumRegFileGprOddNonSp) := valueLo
            myModMemWord(ydx=enumRegFileGprFp) := 0x0
            myModMemWord(ydx=enumRegFileGprSp) := 0x0
          }
          is (True) { // {fp, sp} 64-bit register pair
            ////outpVec(enumRegFileGprEvenNonFp).myExt.modMemWordValid := (
            ////  False
            ////)
            ////outpVec(enumRegFileGprOddNonSp).myExt.modMemWordValid := (
            ////  False
            ////)
            ////myOutpExt(enumRegFileGprEvenNonFp).valid := False
            ////myOutpExt(enumRegFileGprEvenNonFp).modMemWordValid := False
            ////myOutpExt(enumRegFileGprOddNonSp).valid := False
            ////myOutpExt(enumRegFileGprOddNonSp).modMemWordValid := False
            //myOutpExt(enumRegFileGprFp).valid := True
            //myOutpExt(enumRegFileGprSp).valid := True
            ////ret ++= List[Int](
            ////  enumRegFileGprFp,
            ////  enumRegFileGprSp
            ////)
            for (
              zdx <- List[Int](
                enumRegFileGprFp,
                enumRegFileGprSp,
              )
            ) {
              if (doAssertValid) {
                myOutpExt(zdx).valid := True
              }
              myOutpExt(zdx).modMemWordValid := True
            }
            myModMemWord(ydx=enumRegFileGprEvenNonFp) := 0x0
            myModMemWord(ydx=enumRegFileGprOddNonSp) := 0x0
            myModMemWord(ydx=enumRegFileGprFp) := valueHi
            myModMemWord(ydx=enumRegFileGprSp) := valueLo
          }
        //}
      }
      //--------
      //ret
      //--------
    }

    def setSprSa32(
      value: UInt,
      doAssertValid: Boolean,
      //optExtraFunc: Option[(Int) => Unit],
    ): Unit = {
      //--------
      //var ret = new ArrayBuffer[Int]()
      //--------
      switch (myInstrDecEtc.sprSaSel) {
        def outerMyModMemWord(ydx: Int) = (
          //upExt(idx=1, regFileSlice=ydx).modMemWord
          outpVec(ydx).myExt.modMemWord
        )
        val myZero = U(s"${params.mainWidth}'d0")
        is (FlareCpuSprSelect.sprEven) {
          for (ydx <- enumRegFileSprEven until enumRegFileSprOdd) {
            //def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord

            def myModMemWord = (
              outerMyModMemWord(ydx=ydx)
              ////upExt(idx=1, regFileSlice=ydx).modMemWord
              //outpVec(ydx).myExt.modMemWord
            )
            //val myZero = U(s"${params.mainWidth}'d0")
            if (ydx == enumRegFileSprEven) {
              myModMemWord := value
              outpVec(ydx).myExt.modMemWordValid := True
              if (doAssertValid) {
                outpVec(ydx).myExt.valid := True
              }
              //optExtraFunc match {
              //  case Some(myExtraFunc) => {
              //    myExtraFunc(ydx)
              //  }
              //  case None => {
              //  }
              //}
            } else {
              if (myFormal) {
                myModMemWord := myZero
              }
            }
          }
        }
        is (FlareCpuSprSelect.sprOdd) {
          for (ydx <- enumRegFileSprEven until enumRegFileSprOdd) {
            //def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord

            def myModMemWord = (
              outerMyModMemWord(ydx=ydx)
              ////upExt(idx=1, regFileSlice=ydx).modMemWord
              //outpVec(ydx).myExt.modMemWord
            )
            //val myZero = U(s"${params.mainWidth}'d0")
            if (ydx == enumRegFileSprOdd) {
              myModMemWord := value
              outpVec(ydx).myExt.modMemWordValid := True
              if (doAssertValid) {
                outpVec(ydx).myExt.valid := True
              }
              //optExtraFunc match {
              //  case Some(myExtraFunc) => {
              //    myExtraFunc(ydx)
              //  }
              //  case None => {
              //  }
              //}
            } else {
              if (myFormal) {
                myModMemWord := myZero
              }
            }
          }
        }
      }
      //--------
      //ret
      //--------
    }
    def setSprSa64(
      valueHi: UInt,
      valueLo: UInt,
      doAssertValid: Boolean,
      //optExtraFunc: Option[(Int) => Unit],
    ): Unit = {
      //--------
      //var ret = new ArrayBuffer[Int]()
      //--------
      def myModMemWord(ydx: Int) = (
        //upExt(idx=1, regFileSlice=ydx).modMemWord
        outpVec(ydx).myExt.modMemWord
      )
      for (ydx <- enumRegFileSprEven until enumRegFileSprOdd) {
        //--------
        outpVec(ydx).myExt.modMemWordValid := True
        if (doAssertValid) {
          outpVec(ydx).myExt.valid := True
        }
        //outpVec(ydx).myExt.valid := True
        //--------
        //optExtraFunc match {
        //  case Some(myExtraFunc) => {
        //    myExtraFunc(ydx)
        //  }
        //  case None => {
        //  }
        //}
        //--------
      }
      myModMemWord(ydx=enumRegFileSprEven) := valueHi
      myModMemWord(ydx=enumRegFileSprOdd) := valueLo
      //--------
      //ret += enumRegFileSprEven
      //ret += enumRegFileSprOdd
      //ret
      //--------
    }
    //--------
    //for (ydx <- 0 until outpVec.size) {
    //  //outpVec(ydx).myExt.modMemWordValid := False
    //  outpVec(ydx).myExt.valid := False
    //}
    when (
      //rPrevTxnWasHazardAny
      //rPrevTxnWasHazardVec(0)
      rPrevTxnWasHazard
    ) {
      doTestModOpMain(doCheckHazard=true)
    } elsewhen (cMid0Front.up.isValid) {
      doTestModOpMain(doCheckHazard=false)
    }
    //object MyDcacheMissState
    //extends SpinalEnum(defaultEncoding=binarySequential) {
    //  val
    //    
    //}
    def doTestModOpMain(
      doCheckHazard: Boolean
    ): Unit = {
      def doHandleHazardWithDcacheMiss(
        haveCurrLoad: Boolean,
        //someSetModMemWordFuncArr: Seq[((UInt) => Unit, Int)],
        setModMemWordFunc: (
          //Seq[UInt]
          //--------
          (
            Int,
          ) => UInt, // someGetMyRdMemWordFunc
          Boolean,   // doAssertValid
          //--------
        ) => Unit
      ): Unit = {
        def myNonCurrFireSetModMemWord(
          someGetMyRdMemWordFunc: (Int) => UInt
        ) = (
          setModMemWordFunc(
            someGetMyRdMemWordFunc, // someGetMyRdMemWordFunc
            false, // doAssertValid
          )
        )
        def handleCurrFire(
          //someSetModMemWordFuncArr: Seq[(() => Unit, Int)],
          someGetMyRdMemWordFunc: (Int) => UInt=getMyRdMemWord
        ): Unit = {
          //--------
          //outp.myExt.valid := True
          nextPrevTxnWasHazard := False
          //--------
          setModMemWordFunc(
            someGetMyRdMemWordFunc, // someGetMyRdMemWordFunc
            true,                   // doAssertValid
          )
          //--------
        }
        def handleDuplicateIt(
          actuallyDuplicateIt: Boolean,
        ): Unit = {
          for (zdx <- 0 until outpVec.size) {
            def outp = outpVec(zdx)
            outp := (
              RegNext(outp) init(outp.getZero)
            )
            outp.myExt.valid := False
            outp.myExt.modMemWordValid := (
              False
            )
          }
          if (actuallyDuplicateIt) {
            cMid0Front.duplicateIt()
          }
        }
        val rState = KeepAttribute(
          Reg(Bool())
          init(False)
        )
          .setName(
            s"doHandleHazardWithDcacheMiss"
            + s"_${doCheckHazard}_${haveCurrLoad}"
            + s"_rState"
          )
        val rSavedRdMemWord1Valid = (
          KeepAttribute(
            Reg(Bool())
            init(False)
          )
          .setName(
            s"doModInModFrontFunc"
            + s"_${doCheckHazard}_${haveCurrLoad}"
            + s"_rSavedModMemWord1Valid"
          )
        )
        val rSavedRdMemWord1 = (
          KeepAttribute(
            Reg(
              Vec.fill(enumRegFileLim)(
              //cloneOf(myRdMemWord))
                UInt(params.mainWidth bits)
              )
            )
            //init(0x0)
            .setName(
              s"doModInModFrontFunc"
              + s"_${doCheckHazard}_${haveCurrLoad}"
              + s"_rSavedModMemWord1"
            )
          )
        )
        for (ydx <- 0 until rSavedRdMemWord1.size) {
          rSavedRdMemWord1(ydx).init(
            rSavedRdMemWord1(ydx).getZero
          )
        }
          
        switch (rState) {
          is (False) {
            //when (
            //  //!tempModFrontPayload.dcacheHit
            //  //!io.dbus.valid
            //  //|| (
            //    !io.dbus.fire
            //  //)
            //) {
            //  when (
            //    modFront.isValid
            //  ) {
            //    if (haveCurrLoad) {
            //      handleDuplicateIt(actuallyDuplicateIt=true)
            //      //rSavedRdMemWord1 := myRdMemWord
            //      for (ydx <- 0 until rSavedRdMemWord1.size) {
            //        rSavedRdMemWord1(ydx) := getMyRdMemWord(ydx=ydx)
            //      }
            //      rState := True
            //    } else {  // if (!haveCurrLoad)
            //      when (modFront.isFiring) {
            //        handleCurrFire()
            //      }
            //    }
            //  } otherwise { // when (!modFront.isFiring)
            //    handleDuplicateIt(actuallyDuplicateIt=true)
            //  }
            //} otherwise {
            //  when (cMid0Front.up.isFiring) {
            //    handleCurrFire()
            //  }
            //}
            when (
              //!tempModFrontPayload.dcacheHit
              !io.dbus.fire
            ) {
              when (
                modFront.isValid
              ) {
                //when (
                //   rTempPrevOp
                //   === (
                //    PipeMemRmwSimDut.ModOp.LDR_RA_RB
                //  )
                //) {
                if (haveCurrLoad) {
                  //cMid0Front.duplicateIt()
                  handleDuplicateIt(actuallyDuplicateIt=true)

                  GenerationFlags.formal {
                    assert(!rSavedRdMemWord1Valid)
                  }
                  rSavedRdMemWord1Valid := False
                  for (ydx <- 0 until rSavedRdMemWord1.size) {
                    rSavedRdMemWord1(ydx) := (
                      getMyRdMemWord(ydx=ydx)
                    )
                  }
                  rState := True
                } else {  // if (!haveCurrLoad)
                //} otherwise {
                  when (modFront.isFiring) {
                    handleCurrFire()
                  }
                }
              } otherwise { // when (!modFront.isFiring)
                handleDuplicateIt(actuallyDuplicateIt=true)
              }
            } otherwise {
              when (cMid0Front.up.isFiring) {
                //when (
                //  (
                //    (
                //      RegNext(rState) init(False)
                //    ) === False
                //  ) && (
                //    rTempPrevOp
                //    === PipeMemRmwSimDut.ModOp.LDR_RA_RB
                //  )
                //) {
                //  handleCurrFire()
                //} otherwise {
                //  handleCurrFire(
                //  )
                //}
                when (rSavedRdMemWord1Valid) {
                  rSavedRdMemWord1Valid := False
                  handleCurrFire(
                    //someRdMemWord=rSavedRdMemWord1
                    someGetMyRdMemWordFunc=(
                      (ydx: Int) => (rSavedRdMemWord1(ydx))
                    ),
                  )
                } otherwise {
                  handleCurrFire()
                }
              }
            }
            GenerationFlags.formal {
              when (pastValidAfterReset) {
                when (past(rState) === True) {
                  for (ydx <- 0 until rSavedRdMemWord1.size) {
                    assert(stable(rSavedRdMemWord1(ydx)))
                  }
                }
              }
            }
          }
          is (True) {
            //when (cMid0Front.up.isFiring) {
            //  //handleCurrFire(
            //  //  someModMemWord=rSavedRdMemWord1
            //  //)
            //} otherwise {
            //  handleDuplicateIt(actuallyDuplicateIt=false)
            //}
            GenerationFlags.formal {
              when (pastValidAfterReset) {
                when (past(rState) === False) {
                  assert(!past(tempModFrontPayload.dcacheHit))
                  assert(!rSavedRdMemWord1.valid)
                } otherwise {
                  assert(
                    stable(rSavedRdMemWord)
                  )
                }
              }
            }
          }
        }
      }
      //object MyState
      //extends SpinalEnum(defaultEncoding=binarySequential)
      //{
      //  //val 
      //}
      //when (cMid0Front.up.isValid) {
      //  switch (inpInstrDecEtc.decOp) {
      //  }
      //}
    }
  }
}

case class FlareCpu(
  params: FlareCpuParams,
  optFormalTest: Int=(
    FlareCpuParams.enumFormalTestNone
  ),
) extends Component {
  //--------
  val io = FlareCpuIo(params=params)
  //val instrBmb = Bmb(p=params.busParams)
  //val dataBmb = Bmb(p=params.busParams)
  //val busArb = BmbArbiter(
  //  inputsParameter=List(
  //    params.busParams,
  //    params.busParams,
  //  ),
  //  outputParameter=params.busParams,
  //  lowerFirstPriority=false,
  //)
  //busArb.io.inputs(0) << instrBmb
  //busArb.io.inputs(1) << dataBmb
  //io.bus << busArb.io.output
  //--------
  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
  def enumRegFileLim = FlareCpuParams.enumRegFileLim
  //#define FLARE_FLAGS_Z_MASK \
  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_Z_BITPOS) 
  //#define FLARE_FLAGS_C_MASK \
  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_C_BITPOS) 
  //#define FLARE_FLAGS_V_MASK \
  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_V_BITPOS) 
  //#define FLARE_FLAGS_N_MASK \
  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_N_BITPOS) 

  //#define FLARE_SIM_FLAGS_VN_MASK(bits) \
  //  ((uint64_t) 0x1ull << (uint64_t) (bits - 1))
  //#define FLARE_SIM_FLAGS_Z_MASK(bits) \
  //  (FLARE_SIM_FLAGS_VN_MASK (bits) - (int64_t) 0x1ll)
  //#define FLARE_SIM_FLAGS_C_MASK(bits) \
  //  (FLARE_SIM_FLAGS_VN_MASK (bits) << (uint64_t) 0x1ull)
  def myFlagsVnMask(bits: Int) = (
    U(s"${params.mainWidth + 1}'d1") << (bits - 1)
  )
  def myFlagsZMask(bits: Int) = (
    myFlagsVnMask(bits=bits) - 1
  )
  def myFlagsCMask(bits: Int) = (
    myFlagsVnMask(bits=bits) << 1
  )
  //def performSetFlagsZn(
  //  rawElemNumBytesPow: (Int, Int),
  //  result: UInt,
  //  //flagsOut: UInt,
  //): Unit = {
  //  //--------
  //  //def myBits = params.elemNumBytesPow(
  //  //  rawElemNumBytesPow=rawElemNumBytesPow
  //  //)._2
  //  //val outpFlags = UInt(params.mainWidth bits)
  //  //outpFlags := (
  //  //  //0x0
  //  //  flags
  //  //)
  //  //outpFlags.allowOverride
  //  //outpFlags(params.flagIdxZ) := (
  //  //  result(myBits - 1 downto 0) === 0
  //  //)
  //  //outpFlags(params.flagIdxN) := result(myBits - 1)
  //  //doWriteSpr(
  //  //  regIdx=myInstrDecEtc.enumSprFlags,
  //  //  payload=outpFlags,
  //  //)
  //  //--------
  //}


  //static INLINE void
  //flare_sim_set_flags_zn (uint32_t bits,
  //                          uint64_t result,
  //                          int32_t *flags_out)
  //{
  //  uint64_t
  //    temp_flags_z_mask = FLARE_SIM_FLAGS_Z_MASK (bits),
  //    temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);

  //  /* set the `Z` flag */
  //  if (result & temp_flags_z_mask)
  //  {
  //    *flags_out |= FLARE_FLAGS_Z_MASK;
  //  }
  //  else
  //  {
  //    *flags_out &= ~FLARE_FLAGS_Z_MASK;
  //  }

  //  /* set the `N` flag */
  //  if (result & temp_flags_vn_mask)
  //  {
  //    *flags_out |= FLARE_FLAGS_N_MASK;
  //  }
  //  else
  //  {
  //    *flags_out &= ~FLARE_FLAGS_N_MASK;
  //  }
  //}
  def setFlagsZn(
    bits: Int,
    result: UInt,
    flagsOut: UInt
  ): Unit = {
    assert(result.getWidth == params.mainWidth)
    val tempFlagsZMask = myFlagsZMask(bits=bits)
    val tempFlagsVnMask = myFlagsVnMask(bits=bits)
    // set the `Z` flag
    flagsOut(params.flagIdxZ) := (
      (result & tempFlagsZMask) =/= 0
    )
    // set the `N` flag
    flagsOut(params.flagIdxN) := (
      (result & tempFlagsVnMask) =/= 0
    )
  }
  // Returns the sum/difference of the `add`/`sub`/`cmp`/`cmpb`/`cmph`
  // Note: `NULL` `flags_out` indicates don't compute output flags
  //static INLINE int32_t
  //flare_sim_add_sub (uint32_t bits,
  //                    int32_t operand_a,
  //                    int32_t operand_b,
  //                    int32_t flags_in,
  //                    int32_t *flags_out, 
  //                    bool with_carry_in,
  //                    bool do_sub)
  //{
  //  uint64_t
  //    ret = 0,
  //    temp_operand_a = operand_a,
  //    temp_operand_b = operand_b,
  //    temp_flags_c_mask = 0,
  //    temp_flags_vn_mask = 0;

  //  if (!do_sub)
  //  {
  //    ret = temp_operand_a + temp_operand_b
  //      + (with_carry_in
  //        ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
  //        : 0x0ull);
  //  }
  //  else // if (do_sub)
  //  {
  //    /* 6502-style subtraction */
  //    ret = temp_operand_a + (~temp_operand_b)
  //      + (with_carry_in 
  //        ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
  //        : 0x1ull);
  //  }

  //  if (flags_out != NULL)
  //  {
  //    temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
  //    temp_flags_c_mask = FLARE_SIM_FLAGS_C_MASK (bits);

  //    *flags_out = 0x0;
  //    flare_sim_set_flags_zn (bits, ret, flags_out);

  //    /* set the `C` flag */
  //    if (ret & temp_flags_c_mask)
  //    {
  //      *flags_out |= FLARE_FLAGS_C_MASK;
  //    }
  //    /* set the `V` flag (6502-style) */
  //    //if (!((temp_operand_a ^ temp_operand_b) & temp_flags_vn_mask)
  //    //  && ((temp_operand_a ^ ret) & temp_flags_vn_mask))
  //    /* The above ^ commented-out method is equivalent, but slower. */
  //    if ((temp_operand_a ^ ret) & (temp_operand_b ^ ret)
  //      & temp_flags_vn_mask)
  //    {
  //      *flags_out |= FLARE_FLAGS_V_MASK;
  //    }
  //  }

  //  return (int32_t) ret;
  //}
  def performAddSub(
    bits: Int,
    operandA: UInt,
    operandB: UInt,
    flagsIn: UInt,
    withCarryIn: Boolean,
    doSub: Boolean,
    ret: UInt,
    flagsOut: (UInt, Bool)=(U"32'd0", False),
  ): Unit = {
    //--------
    //val ret = UInt(33 bits)
    val tempRet = UInt(params.mainWidth + 1 bits)
    val tempOperandA = UInt((params.mainWidth + 1) bits)
    val tempOperandB = UInt((params.mainWidth + 1) bits)
    val tempFlagsCMask = UInt(params.mainWidth bits)
    val tempFlagsVnMask = UInt(params.mainWidth bits)
    //--------
    assert(ret.getWidth == params.mainWidth)
    assert(operandA.getWidth == params.mainWidth)
    assert(operandB.getWidth == params.mainWidth)
    assert(flagsIn.getWidth == params.mainWidth)
    ret := 0x0
    tempOperandA := Cat(False, operandA).asUInt
    tempOperandB := Cat(False, operandB).asUInt
    tempFlagsCMask := 0x0
    tempFlagsVnMask := 0x0
    //--------
    if (!doSub) {
      //tempRet = temp_operand_a + temp_operand_b
      //  + (with_carry_in
      //    ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
      //    : 0x0ull);
      tempRet := (
        tempOperandA + tempOperandB
        + (
          if (withCarryIn) (
            Cat(
              U(s"${params.mainWidth}'d0"),
              flagsIn(params.flagIdxC)
            ).asUInt
          ) else ( // if (!withCarryIn)
            U(s"${params.mainWidth}'d0")
          )
        )
      )
    } else { // if (doSub)
      ///* 6502-style subtraction */
      //tempRet = temp_operand_a + (~temp_operand_b)
      //  + (with_carry_in 
      //    ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
      //    : 0x1ull);
      tempRet := (
        tempOperandA + (~tempOperandB)
        + (
          if (withCarryIn) (
            Cat(
              U(s"${params.mainWidth}'d0"),
              flagsIn(params.flagIdxC)
            ).asUInt
          ) else ( // if (!withCarryIn)
            U(s"${params.mainWidth}'d1")
          )
        )
      )
    }
    ret := tempRet(ret.bitsRange)
    when (flagsOut._2) {
      //temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
      //temp_flags_c_mask = FLARE_SIM_FLAGS_C_MASK (bits);
      tempFlagsVnMask := myFlagsVnMask(bits=bits)
      tempFlagsCMask := myFlagsCMask(bits=bits)

      //*flags_out = 0x0;
      //flare_sim_set_flags_zn (bits, tempRet, flags_out);
      setFlagsZn(
        bits=bits,
        result=tempRet,
        flagsOut=flagsOut._1,
      )

      ///* set the `C` flag */
      //if (tempRet & temp_flags_c_mask)
      //{
      //  *flags_out |= FLARE_FLAGS_C_MASK;
      //}
      flagsOut._1(params.flagIdxC) := (
        (tempRet & tempFlagsCMask) =/= 0
      )
      ///* set the `V` flag (6502-style) */
      ////if (!((temp_operand_a ^ temp_operand_b) & temp_flags_vn_mask)
      ////  && ((temp_operand_a ^ tempRet) & temp_flags_vn_mask))
      ///* The above ^ commented-out method is equivalent, but slower. */
      //if ((temp_operand_a ^ tempRet) & (temp_operand_b ^ tempRet)
      //  & temp_flags_vn_mask)
      //{
      //  *flags_out |= FLARE_FLAGS_V_MASK;
      //}
      flagsOut._1(params.flagIdxV) := (
        (
          ((tempOperandA ^ tempRet) & (tempOperandB ^ tempRet)
          & tempFlagsVnMask)
        ) =/= 0
      )
    }
  }

  //def enumPipeMemIcache = 0
  //def enumPipeMemDcache = 1
  //def enumPipeMemGprFileEven = 2
  //def enumPipeMemGprFileOddNonSp = 3
  //def enumPipeMemGprFileSp = 4
  //def enumPipeMemSprFile = 5
  //def enumPipeMemLim = 6

  val linkArr = PipeHelper.mkLinkArr()

  //case class FlareCpuPipeMemModExtType(
  //) extends Bundle {
  //  //val regPc = UInt(params.mainWidth bits)
  //  //val instrEnc = FlareCpuInstrEnc(params=params)
  //  //val instrDecEtc = FlareCpuInstrDecEtc(params=params)
  //  ////val icache = FlareCpuIcachePipePayload(params=params)
  //  ////val dcache = FlareCpuDcachePipePayload(params=params)
  //}
  val psIdHaltIt = Bool()
  psIdHaltIt := False
  // `exSetPc` is to be driven by the `EX` pipeline stage
  val psExSetPc = Flow(UInt(params.mainWidth bits))
  //val psExSetPc = Flow(FlareCpuPsExSetPcPayload(
  //  params=params,
  //  optFormalTest=optFormalTest,
  //))
  psExSetPc.allowOverride
  psExSetPc := (
    RegNext(psExSetPc)
    init(psExSetPc.getZero)
  )

  //def mkRegFileModType() = (
  //  FlareCpuPipeMemModType(
  //    params=params,
  //    wordType=params.regWordType(),
  //    wordCountMax=params.sprFileEvenWordCount,
  //    hazardCmpType=params.regFileHazardCmpType(),
  //    modRdPortCnt=params.regFileModRdPortCnt,
  //    modStageCnt=params.regFileModStageCnt,
  //    optModHazardKind=params.regFileOptModHazardKind,
  //    modExtType=FlareCpuPipeMemModExtType(params=params),
  //  )
  //)
  //def mkGprFileEvenModType() = (
  //  FlareCpuPipeMemModType(
  //    params=params,
  //    wordType=params.regWordType(),
  //    wordCount=params.gprFileEvenWordCount,
  //    hazardCmpType=params.regFileHazardCmpType(),
  //    modRdPortCnt=params.regFileNonSpModRdPortCnt,
  //    modStageCnt=params.regFileModStageCnt,
  //    optModHazardKind=params.regFileOptModHazardKind,
  //    modExtType=FlareCpuPipeMemModExtType(),
  //  )
  //)
  //val gprFileEven = PipeMemRmw[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModType[
  //    UInt,
  //    Bool,
  //    FlareCpuPipeMemModExtType,
  //  ],
  //  PipeMemRmwDualRdTypeDisabled[UInt, Bool],
  //](
  //  // `r0`, `r2`, `r4`, `r6`, `r8`, `r10`, `r12`, `fp`
  //  wordType=params.regWordType(),
  //  wordCount=params.gprFileEvenWordCount,
  //  hazardCmpType=params.regFileHazardCmpType(),
  //  modType=mkGprFileEvenModType(),
  //  modRdPortCnt=params.regFileNonSpModRdPortCnt,
  //  modStageCnt=params.regFileModStageCnt,
  //  pipeName="FlareCpu_gprFileEven",
  //  linkArr=Some(linkArr),
  //  memArrIdx=enumPipeMemGprFileEven,
  //  memArrSize=enumPipeMemLim,
  //  optDualRd=false,
  //  optReorder=false,
  //  initBigInt=Some({
  //    val tempArr = new ArrayBuffer[BigInt]()
  //    for (idx <- 0 until params.gprFileEvenWordCount) {
  //      tempArr += BigInt(0)
  //    }
  //    tempArr.toSeq
  //  }),
  //  optModHazardKind=params.regFileOptModHazardKind,
  //  optEnableClear=false,
  //  memRamStyle="auto",
  //  vivadoDebug=false,
  //  optIncludeModFrontStageLink=false,
  //)(
  //  //--------
  //  doModInModFrontFunc=Some(
  //    mkRegFilePipeMemDoModInModFrontFunc(
  //      regFileKind=enumRegFileGprEven
  //    )
  //  ),
  //  //--------
  //)

  //val psExSetPc = KeepAttribute(
  //  Flow(UInt(params.mainWidth bits))
  //)

  val regFileWordCountArr: ArrayBuffer[Int] = {
    val tempArr = (new ArrayBuffer[Int]()) ++ List[Int](
      params.gprFileEvenNonFpWordCount,
      params.gprFileFpWordCount,
      params.gprFileOddNonSpWordCount,
      params.gprFileSpWordCount,
      //params.sprFileWordCount,
      params.sprFileEvenWordCount,
      params.sprFileOddWordCount,
    )
    //println(s"tempArr.size: ${tempArr.size}")
    tempArr
  }

  val regFile = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      FlareCpuPipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool]
  ](
    wordType=params.regWordType(),
    wordCountArr=regFileWordCountArr,
    hazardCmpType=params.regFileHazardCmpType(),
    modType=FlareCpuParams.mkRegFileModType(
      params=params,
      optFormalTest=optFormalTest,
    ),
    modRdPortCnt=params.regFileModRdPortCnt,
    modStageCnt=params.regFileModStageCnt,
    pipeName="FlareCpu_pipeName",
    linkArr=Some(linkArr),
    initBigInt={
      val myInitBigInt = new ArrayBuffer[ArrayBuffer[BigInt]]()
      //myInitBigInt += ArrayBuffer.fill(params.gprFileEvenWordCount)(
      //  BigInt(0)
      //)
      myInitBigInt += ArrayBuffer.fill(params.gprFileEvenNonFpWordCount)(
        BigInt(0)
      )
      myInitBigInt += ArrayBuffer.fill(params.gprFileFpWordCount)(
        BigInt(0)
      )
      myInitBigInt += ArrayBuffer.fill(params.gprFileOddNonSpWordCount)(
        BigInt(0)
      )
      myInitBigInt += ArrayBuffer.fill(params.gprFileSpWordCount)(
        BigInt(0)
      )
      //myInitBigInt += ArrayBuffer.fill(params.sprFileWordCount)(
      //  BigInt(0)
      //)
      myInitBigInt += ArrayBuffer.fill(params.sprFileEvenWordCount)(
        BigInt(0)
      )
      myInitBigInt += ArrayBuffer.fill(params.sprFileOddWordCount)(
        BigInt(0)
      )
      Some(myInitBigInt)
    },
    optModHazardKind=PipeMemRmw.modHazardKindFwd,
    memRamStyle="block",
    //optIncludeModFrontStageLink=false,
  )(
    doModInFrontFunc=(
      None
      //Some(
      //  (
      //    outp: FlareCpuPipeMemModType[
      //      UInt,
      //      Bool,
      //      FlareCpuPipeMemModExtType,
      //    ],
      //    inp: FlareCpuPipeMemModType[
      //      UInt,
      //      Bool,
      //      FlareCpuPipeMemModExtType,
      //    ],
      //    cFront: CtrlLink,
      //    ydx: Int,
      //  ) => {
      //    outp := inp
      //  }
      //)
    ),
    doModInModFrontFunc=Some(
      //(
      //  //PipeMemRmwPayloadExt[WordT, HazardCmpT],  // inp
      //  //PipeMemRmwPayloadExt[WordT, HazardCmpT],  // outp
      //  nextPrevTxnWasHazard: Vec[Bool], // nextPrevTxnWasHazard,
      //  rPrevTxnWasHazard: Vec[Bool], // rPrevTxnWasHazard,
      //  outp: Vec[FlareCpuPipeMemModType[
      //    UInt,
      //    Bool,
      //    FlareCpuPipeMemModExtType,
      //  ]], // outp
      //  inp: Vec[FlareCpuPipeMemModType[
      //    UInt,
      //    Bool,
      //    FlareCpuPipeMemModExtType,
      //  ]], // inp
      //  cMid0Front: CtrlLink, // mod.front.cMid0Front
      //  modFront: Node,     // io.modFront
      //  tempModFrontPayload: Vec[FlareCpuPipeMemModType[
      //    UInt,
      //    Bool,
      //    FlareCpuPipeMemModExtType,
      //  ]], // io.tempModFrontPayload
      //  //myModMemWord: UInt,    // myModMemWord
      //  getMyModMemWordFunc: (Int) => UInt,
      //  //Vec[WordT],  // myRdMemWord
      //  ydx: Int,    // ydx
      //) => {
      //  outp := inp
      //},
      psExDoModInModFrontFunc
    ),
  )
  //--------
  //--------
  val pIf = Payload(FlareCpuPipeMemModExtType(
    params=params,
    optFormalTest=optFormalTest,
  ))
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
  //linkArr += DirectLink(
  //  up=sIf.down,
  //  down=regFile.io.front
  //)
  //--------
  //val pId = Payload(FlareCpuPipeMemModExtType())
  //val pId = Payload(FlareCpuPipeMemModExtType(params=params))
  //val pId = Payload(FlareCpuPipeMemModExtType(
  //  params=params,
  //  optFormalTest=optFormalTest,
  //))
  val cId = CtrlLink(
    up=sIf.down,
    down=(
      //Node()
      regFile.io.front
    ),
  )
  linkArr += cId
  // We have no `sId` because we use `regFile.mod.front.sFront` for that
  //--------
  //val pEx = Payload(Vec.fill(enumRegFileLim)(mkRegFileModType()))
  //val cEx = CtrlLink(
  //  up=regFile.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += cEx
  //val sEx = StageLink(
  //  up=cEx.down,
  //  down=regFile.io.modFront,
  //)
  //linkArr += sEx
  //--------
  val cMem = CtrlLink(
    up=regFile.io.modFront,
    down=Node(),
  )
  linkArr += cMem
  val sMem = StageLink(
    up=cMem.down,
    down=regFile.io.modBack,
  )
  linkArr += sMem
  //--------
  //val cIfArea = new cIf.Area {
  //}
  val cIfArea = FlareCpuPipeStageIf(
    params=params,
    cIf=cIf,
    pIf=pIf,
    io=io,
    psIdHaltIt=psIdHaltIt,
    psExSetPc=psExSetPc,
  )
  //val cIdArea = new cId.Area {
  //  //--------
  //  //val myMain = FlareCpuPipeStageId(
  //  //  params=params,
  //  //  up=up,
  //  //)
  //  //val myUp: NodeApi = up
  //}
  val cIdArea = FlareCpuPipeStageId(
    params=params,
    cId=cId,
    pIf=pIf,
    //pId=pId,
    io=io,
    regFile=Some(regFile),
    //mkRegFileModType=FlareCpuParams.mkRegFileModType(params=params),
    psIdHaltIt=psIdHaltIt,
    psExSetPc=psExSetPc,
  )
  //val psEx = new Area {
  //  val psExNextHaltItState = KeepAttribute(
  //    FlareCpuPsExHaltItState()
  //  )
  //  val psExRHaltItState = KeepAttribute(
  //    RegNext(psExNextHaltItState)
  //    init(FlareCpuPsExHaltItState.IDLE)
  //  )
  //}
  def psExDoModInModFrontFunc(
    //nextPrevTxnWasHazardVec: Vec[Bool], // nextPrevTxnWasHazardVec
    //rPrevTxnWasHazardVec: Vec[Bool],  // rPrevTxnWasHazardVec
    //rPrevTxnWasHazardAny: Bool,       // rPrevTxnWasHazardAny
    //outpVec: Vec[FlareCpuPipeMemModType[
    //  UInt,
    //  Bool,
    //  FlareCpuPipeMemModExtType,
    //]], // outp
    //inpVec: Vec[FlareCpuPipeMemModType[
    //  UInt,
    //  Bool,
    //  FlareCpuPipeMemModExtType,
    //]], // inp
    //cMid0Front: CtrlLink, // mod.front.cMid0Front
    //modFront: Node,     // io.modFront
    //tempModFrontPayloadVec: Vec[FlareCpuPipeMemModType[
    //  UInt,
    //  Bool,
    //  FlareCpuPipeMemModExtType,
    //]], // io.tempModFrontPayload
    //getMyModMemWordFunc: (Int) => UInt,
    //ydxArg: Int,    // ydx
    doModParams: PipeMemRmwDoModInModFrontFuncParams[
      UInt,
      Bool,
      FlareCpuPipeMemModType[
        UInt,
        Bool,
        FlareCpuPipeMemModExtType,
      ],
    ]
  ): Area = {
    FlareCpuPipeStageEx(
      params=params,
      io=io,
      psExSetPc=psExSetPc,
      regFileWordCountArr=regFileWordCountArr,
      //nextPrevTxnWasHazardVec=nextPrevTxnWasHazardVec,
      //rPrevTxnWasHazardVec=rPrevTxnWasHazardVec,
      //rPrevTxnWasHazardAny=rPrevTxnWasHazardAny,
      //outpVec=outpVec,
      //inpVec=inpVec,
      //cMid0Front=cMid0Front,
      //modFront=modFront,
      //tempModFrontPayloadVec=tempModFrontPayloadVec,
      //getMyModMemWordFunc=getMyModMemWordFunc,
      //ydxArg=ydxArg,
      doModParams=doModParams,
    ).setName("cExArea")
  }
  //val cExArea = new cEx.Area {
  //  val upMod = Vec.fill(2)(Vec.fill(enumRegFileLim)(mkRegFileModType()))
  //  for (ydx <- 0 until enumRegFileLim) {
  //    upMod(0)(ydx) := up(regFile.io.modFrontPayload(ydx))
  //    when (up.isFiring) {
  //      up(pEx)(ydx) := upMod(1)(ydx)
  //    }
  //  }
  //  def upExt(
  //    idx: Int,
  //    regFileSlice: Int,
  //  ) = (
  //    upMod(idx)(regFileSlice).myExt
  //  )
  //  def upModExt(idx: Int) = (
  //    upMod(idx)(0).modExt
  //  )
  //  def upInstrDecEtc(idx: Int) = (
  //    upModExt(idx=idx).instrDecEtc
  //  )
  //  object MultiCycleState
  //  extends SpinalEnum(defaultEncoding=binarySequential) {
  //    val
  //      PRIMARY,
  //      PREV_INSTR_MEM_ACCESS
  //      = newElement();
  //  }
  //  val rMultiCycleState = (
  //    KeepAttribute(
  //      //Reg(Bool())
  //      //init(False)
  //      Reg(MultiCycleState())
  //      init(MultiCycleState.PRIMARY)
  //    )
  //  )
  //  def setGprRa32(
  //    value: UInt
  //  ): Unit = {
  //    switch (upInstrDecEtc(0).gprRaSel) {
  //      for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
  //        def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
  //        val myZero = U(s"${params.mainWidth}'d0")
  //        is (FlareCpuGprSelect.gprEvenNonFp) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileGprEvenNonFp) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //        is (FlareCpuGprSelect.gprFp) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileGprFp) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //        is (FlareCpuGprSelect.gprOddNonSp) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileGprOddNonSp) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //        is (FlareCpuGprSelect.gprSp) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileGprSp) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //      }
  //    }
  //  }
  //  def setGprRa64(
  //    valueHi: UInt,
  //    valueLo: UInt,
  //  ): Unit = {
  //    switch (upInstrDecEtc(0).gprRa64IsNonFpSp) {
  //      //for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
  //      //  def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
  //        def myModMemWord(
  //          ydx: Int,
  //        ) = (
  //          upExt(idx=1, regFileSlice=ydx).modMemWord
  //        )
  //        val myZero = U(s"${params.mainWidth}'d0")
  //        is (False) { // {non-fp, non-sp} 64-bit register pair
  //          myModMemWord(ydx=enumRegFileGprEvenNonFp) := valueHi
  //          myModMemWord(ydx=enumRegFileGprOddNonSp) := valueLo
  //          myModMemWord(ydx=enumRegFileGprFp) := 0x0
  //          myModMemWord(ydx=enumRegFileGprSp) := 0x0
  //        }
  //        is (True) { // {fp, sp} 64-bit register pair
  //          myModMemWord(ydx=enumRegFileGprEvenNonFp) := 0x0
  //          myModMemWord(ydx=enumRegFileGprOddNonSp) := 0x0
  //          myModMemWord(ydx=enumRegFileGprFp) := valueHi
  //          myModMemWord(ydx=enumRegFileGprSp) := valueLo
  //        }
  //      //}
  //    }
  //    //for (ydx <- enumRegFileGprEvenNonFp until enumRegFileGprSp) {
  //    //  def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
  //    //  val myZero = U(s"${params.mainWidth}'d0")
  //    //  switch (upInstrDecEtc(0).gprRaSel) {
  //    //    is (FlareCpuGprSelect.gprEvenNonFp) {
  //    //      myModMemWord := (
  //    //        if (ydx == enumRegFileGprEvenNonFp) (
  //    //          value
  //    //        ) else (
  //    //          myZero
  //    //        )
  //    //      )
  //    //    }
  //    //    is (FlareCpuGprSelect.gprFp) {
  //    //      myModMemWord := (
  //    //        if (ydx == enumRegFileGprFp) (
  //    //          value
  //    //        ) else (
  //    //          myZero
  //    //        )
  //    //      )
  //    //    }
  //    //    is (FlareCpuGprSelect.gprOddNonSp) {
  //    //      myModMemWord := (
  //    //        if (ydx == enumRegFileGprOddNonSp) (
  //    //          value
  //    //        ) else (
  //    //          myZero
  //    //        )
  //    //      )
  //    //    }
  //    //    is (FlareCpuGprSelect.gprSp) {
  //    //      myModMemWord := (
  //    //        if (ydx == enumRegFileGprSp) (
  //    //          value
  //    //        ) else (
  //    //          myZero
  //    //        )
  //    //      )
  //    //    }
  //    //  }
  //    //}
  //  }

  //  def setSprSa32(
  //    value: UInt
  //  ): Unit = {
  //    switch (upInstrDecEtc(0).sprSaSel) {
  //      for (ydx <- enumRegFileSprEven until enumRegFileSprOdd) {
  //        def myModMemWord = upExt(idx=1, regFileSlice=ydx).modMemWord
  //        val myZero = U(s"${params.mainWidth}'d0")
  //        is (FlareCpuSprSelect.sprEven) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileSprEven) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //        is (FlareCpuSprSelect.sprOdd) {
  //          myModMemWord := (
  //            if (ydx == enumRegFileSprOdd) (
  //              value
  //            ) else (
  //              myZero
  //            )
  //          )
  //        }
  //      }
  //    }
  //  }
  //  def setSprSa64(
  //    valueHi: UInt,
  //    valueLo: UInt,
  //  ): Unit = {
  //  }
  //  val rPreSimm = (
  //    Reg(UInt(params.preWidth bits))
  //    init(0x0)
  //  )
  //  val rLpreSimm = (
  //    Reg(UInt(params.lpreWidth bits))
  //    init(0x0)
  //  )
  //  when (up.isValid) {
  //    switch (rMultiCycleState) {
  //      is (MultiCycleState.PRIMARY) {
  //        //when (
  //        //  regFile.io.tempModFrontPayload(0).dcacheHit
  //        //) {
  //        //}
  //        switch (upInstrDecEtc(0).decOp) {
  //          // decoded instruction opcode
  //          //--------
  //          is (FlareCpuInstrDecOp.bubble) {
  //            // fake instruction, acts as a NOP and prevents forwarding in
  //            // the `PipeMemRmw`s
  //            // used, for example, for `index`, `lpre`'s low immediate bits
  //          }
  //          is (FlareCpuInstrDecOp.lpreSimmHi) {
  //          }
  //          is (FlareCpuInstrDecOp.lpreSimmLo) {
  //            // fake instruction, acts as a bubble
  //          }
  //          is (FlareCpuInstrDecOp.preSimm) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.cmpxchg) { // without lock
  //          }
  //          is (FlareCpuInstrDecOp.cmpxchgLock) { // with lock
  //          }
  //          is (FlareCpuInstrDecOp.xchg) {     // without lock
  //          }
  //          is (FlareCpuInstrDecOp.xchgLock) {   // with lock
  //          }
  //          //--------
  //          //addRaRbSimm, // only `fp`, `sp`, or `pc` can be `rB`
  //          is (FlareCpuInstrDecOp.addRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.addRaPcSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.addRaSpSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.addRaFpSimm) {
  //          }

  //          is (FlareCpuInstrDecOp.cmpRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.cpyRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.lslRaImm) {
  //          }
  //          is (FlareCpuInstrDecOp.lsrRaImm) {
  //          }

  //          is (FlareCpuInstrDecOp.asrRaImm) {
  //          }
  //          is (FlareCpuInstrDecOp.andRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.orrRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.xorRaSimm) {
  //          }

  //          is (FlareCpuInstrDecOp.zeRaImm) {
  //          }
  //          is (FlareCpuInstrDecOp.seRaImm) {
  //          }
  //          is (FlareCpuInstrDecOp.swiRaSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.swiImm) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.addRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.subRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.addRaSpRb) {
  //          }
  //          is (FlareCpuInstrDecOp.addRaFpRb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.cmpRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.cpyRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.lslRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.lsrRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.asrRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.andRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.orrRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.xorRaRb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.adcRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.sbcRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.cmpbcRaRb) {
  //          }
  //          //--------
  //          //addRaRbFlags,
  //          //subRaRbFlags,
  //          //addRaSpRbFlags,
  //          //addRaFpRbFlags,
  //          ////--------
  //          ////cmpRaRbFlags,
  //          //cpyRaRbFlags,
  //          //lslRaRbFlags,
  //          //lsrRaRbFlags,
  //          //asrRaRbFlags,
  //          //andRaRbFlags,
  //          //orrRaRbFlags,
  //          //xorRaRbFlags,
  //          ////--------
  //          //adcRaRbFlags,
  //          //sbcRaRbFlags,
  //          //cmpbcRaRbFlags,
  //          //--------
  //          is (FlareCpuInstrDecOp.blSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.braSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.beqSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bneSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bmiSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bplSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bvsSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bvcSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bgeuSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bltuSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bgtuSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bleuSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bgesSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bltsSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.bgtsSimm) {
  //          }
  //          is (FlareCpuInstrDecOp.blesSimm) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.jlRa) {
  //          }
  //          is (FlareCpuInstrDecOp.jmpRa) {
  //          }
  //          is (FlareCpuInstrDecOp.jmpIra) {
  //          }
  //          is (FlareCpuInstrDecOp.reti) {
  //          }
  //          is (FlareCpuInstrDecOp.ei) {
  //          }
  //          is (FlareCpuInstrDecOp.di) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.pushRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.pushSaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.popRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.popSaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.popPcRb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.mulRaRb) {
  //          }
  //          //--------
  //          //udivRaRb,
  //          //sdivRaRb,
  //          //udivmodRaRbRc,
  //          //sdivmodRaRbRc,
  //          is (FlareCpuInstrDecOp.udivmodRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.sdivmodRaRb) {
  //          }
  //          //--------
  //          //lumulRcRdRaRb,
  //          //lsmulRcRdRaRb,
  //          is (FlareCpuInstrDecOp.lumulRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.lsmulRaRb) {
  //          }
  //          //--------
  //          //udiv64RaRb,
  //          //sdiv64RaRb,
  //          //udivmod64RaRbRcRd,
  //          //sdivmod64RaRbRcRd,
  //          is (FlareCpuInstrDecOp.udivmod64RaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.sdivmod64RaRb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.ldubRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.ldsbRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.lduhRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.ldshRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.ldrRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.stbRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.sthRaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.strRaRbLdst) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.cpyRaSb) {
  //          }
  //          is (FlareCpuInstrDecOp.cpySaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.cpySaSb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.indexRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.indexRaSimm) {
  //          }
  //          //--------
  //          //ldrRaRbSimmLdst,
  //          //strRaRbSimmLdst,
  //          //--------
  //          is (FlareCpuInstrDecOp.ldrSaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.ldrSaSbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.strSaRbLdst) {
  //          }
  //          is (FlareCpuInstrDecOp.strSaSbLdst) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.cmpbRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.cmphRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.lsrbRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.lsrhRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.asrbRaRb) {
  //          }
  //          is (FlareCpuInstrDecOp.asrhRaRb) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.icreloadRaSimm) {
  //          }
  //          //--------
  //          is (FlareCpuInstrDecOp.icflush) {
  //          }
  //          //--------
  //        }
  //      }
  //      is (MultiCycleState.PREV_INSTR_MEM_ACCESS) {
  //      }
  //    }
  //  }
  //  //duplicateIt()
  //  //when (up.isValid) {
  //  //  //val upModExt = Vec.fill(2)(FlareCpuPipeMemModExtType())
  //  //  //upModExt(0) := up(regFile.io.modFrontPayload(0)).modExt
  //  //  //upMod(0) := 
  //  //  //up(pEx) := upMod(1)
  //  //  //when (up.isFiring) {
  //  //    switch (upInstrDecEtc(0).decOp) {
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.bubble) {
  //  //        // fake instruction, acts as a NOP and prevents forwarding in
  //  //        // the `PipeMemRmw`s
  //  //        // used, for example, for `index`, `lpre`'s low immediate bits
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lpreSimmHi) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lpreSimmLo) {
  //  //        // fake instruction, acts as a bubble
  //  //      }
  //  //      is (FlareCpuInstrDecOp.preSimm) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.cmpxchg) { // without lock
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cmpxchgLock) { // with lock
  //  //      }
  //  //      is (FlareCpuInstrDecOp.xchg) {     // without lock
  //  //      }
  //  //      is (FlareCpuInstrDecOp.xchgLock) {   // with lock
  //  //      }
  //  //      //--------
  //  //      //addRaRbSimm, // only `fp`, `sp`, or `pc` can be `rB`
  //  //      is (FlareCpuInstrDecOp.addRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.addRaPcSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.addRaSpSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.addRaFpSimm) {
  //  //      }

  //  //      is (FlareCpuInstrDecOp.cmpRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cpyRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lslRaImm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lsrRaImm) {
  //  //      }

  //  //      is (FlareCpuInstrDecOp.asrRaImm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.andRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.orrRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.xorRaSimm) {
  //  //      }

  //  //      is (FlareCpuInstrDecOp.zeRaImm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.seRaImm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.swiRaSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.swiImm) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.addRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.subRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.addRaSpRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.addRaFpRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.cmpRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cpyRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lslRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lsrRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.asrRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.andRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.orrRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.xorRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.adcRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.sbcRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cmpbcRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.blSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.braSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.beqSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bneSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bmiSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bplSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bvsSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bvcSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bgeuSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bltuSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bgtuSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bleuSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bgesSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bltsSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.bgtsSimm) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.blesSimm) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.jlRa) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.jmpRa) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.jmpIra) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.reti) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.ei) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.di) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.pushRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.pushSaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.popRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.popSaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.popPcRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.mulRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.udivmodRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.sdivmodRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.lumulRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lsmulRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.udivmod64RaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.sdivmod64RaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.ldubRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.ldsbRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lduhRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.ldshRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.ldrRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.stbRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.sthRaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.strRaRbLdst) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.cpyRaSb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cpySaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cpySaSb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.indexRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.indexRaSimm) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.ldrSaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.ldrSaSbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.strSaRbLdst) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.strSaSbLdst) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.cmpbRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.cmphRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lsrbRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.lsrhRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.asrbRaRb) {
  //  //      }
  //  //      is (FlareCpuInstrDecOp.asrhRaRb) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.icreloadRaSimm) {
  //  //      }
  //  //      //--------
  //  //      is (FlareCpuInstrDecOp.icflush) {
  //  //      }
  //  //      //--------
  //  //      //default {
  //  //      //  haltIt()
  //  //      //}
  //  //      //--------
  //  //    }
  //  //  //}
  //  //}

  //  //exSetPc.valid := True
  //  //exSetPc.payload := 0x0
  //  //outp := inp
  //  //outp.allowOverride
  //  //val nextHaltItState = KeepAttribute(
  //  //  Bool()
  //  //).setName(s"FlareCpu_doModInModFrontFunc_nextHaltItState_${ydx}")
  //  //val rHaltItState = KeepAttribute(
  //  //  RegNext(nextHaltItState)
  //  //  init(nextHaltItState.getZero)
  //  //).setName(s"FlareCpu_doModInModFrontFunc_rHaltItState_${ydx}")
  //  //////val nextMulHaltItCnt = KeepAttribute(
  //  //////  SInt(4 bits)
  //  //////).setName("FlareCpu_doModInModFrontFunc_nextMulHaltItCnt")
  //  //val nextMulHaltItCnt = SInt(4 bits)
  //  //  .setName(s"FlareCpu_doModInModFrontFunc_nextMulHaltItCnt_${ydx}")
  //  //val rMulHaltItCnt = (
  //  //  RegNext(nextMulHaltItCnt)
  //  //  init(-1)
  //  //)
  //  //  .setName(s"FlareCpu_doModInModFrontFunc_rMulHaltItCnt_${ydx}")
  //  //nextHaltItState := rHaltItState
  //  //nextMulHaltItCnt := rMulHaltItCnt
  //  //def setOutpModMemWord(
  //  //  someModMemWord: UInt=myModMemWord
  //  //): Unit = {
  //  //  //outp.myExt.modMemWord := (
  //  //  //  someModMemWord + 0x1
  //  //  //)
  //  //  outp.myExt.modMemWordValid := True
  //  //}
  //  //val rSavedModMemWord = (
  //  //  Reg(cloneOf(myModMemWord))
  //  //  init(myModMemWord.getZero)
  //  //)
  //  //  .setName(s"FlareCpu_doModInModFrontFunc_rSavedModMemWord_${ydx}")
  //  //val rPrevOutp = KeepAttribute(
  //  //  RegNextWhen(
  //  //    outp,
  //  //    cMid0Front.up.isFiring
  //  //  )
  //  //  init(outp.getZero)
  //  //)
  //  //  .setName(s"FlareCpu_doModInModFrontFunc_rPrevOutp_${ydx}")
  //  //def doMulHaltItFsmIdleInnards(
  //  //  doDuplicateIt: Boolean
  //  //): Unit = {
  //  //  if (PipeMemRmwSimDut.doTestModOp) {
  //  //    def myInitMulHaltItCnt = 0x1
  //  //    cMid0Front.duplicateIt()
  //  //    when (
  //  //      //cMid0Front.down.isFiring
  //  //      modFront.isFiring
  //  //    ) {
  //  //      nextHaltItState := (
  //  //        //PipeMemRmwSimDutHaltItState.HALT_IT
  //  //        True
  //  //      )
  //  //      nextMulHaltItCnt := myInitMulHaltItCnt
  //  //    }
  //  //    outp.myExt.modMemWordValid := False
  //  //    rSavedModMemWord := myModMemWord
  //  //  }
  //  //}
  //  //def doMulHaltItFsmHaltItInnards(): Unit = {
  //  //  if (PipeMemRmwSimDut.doTestModOp) {
  //  //    outp := (
  //  //      RegNext(outp)
  //  //      init(outp.getZero)
  //  //    )
  //  //    when ((rMulHaltItCnt - 1).msb) {
  //  //      when (
  //  //        //cMid0Front.down.isFiring
  //  //        modFront.isFiring
  //  //      ) {
  //  //        setOutpModMemWord(rSavedModMemWord)
  //  //        nextHaltItState := False//PipeMemRmwSimDutHaltItState.IDLE
  //  //      }
  //  //    } otherwise {
  //  //      nextMulHaltItCnt := rMulHaltItCnt - 1
  //  //      //cMid0Front.haltIt()
  //  //      cMid0Front.duplicateIt()
  //  //      outp.myExt.modMemWordValid := False
  //  //    }
  //  //  }
  //  //}
  //  //def doTestModOpMain(
  //  //  doCheckHazard: Boolean=false
  //  //): Unit = {
  //  //  val myFindFirstHazardAddr = (doCheckHazard) generate (
  //  //    KeepAttribute(
  //  //      inp.myExt.memAddr.sFindFirst(
  //  //        _ === rPrevOutp.myExt.memAddr(PipeMemRmw.modWrIdx)
  //  //      )
  //  //      //(
  //  //      //  // Only check one register.
  //  //      //  // This will work fine for testing the different
  //  //      //  // categories of stalls, but the real CPU will need to
  //  //      //  /// be tested for *all* registers
  //  //      //  inp.myExt.memAddr(PipeMemRmw.modWrIdx)
  //  //      //  === rPrevOutp.myExt.memAddr(PipeMemRmw.modWrIdx)
  //  //      //)
  //  //      .setName(s"myFindFirstHazardAddr_${ydx}")
  //  //    )
  //  //  )
  //  //  def doHandleHazardWithDcacheMiss(
  //  //    haveCurrLoad: Boolean,
  //  //  ): Unit = {
  //  //    def handleCurrFire(
  //  //      someModMemWord: UInt=myModMemWord
  //  //    ): Unit = {
  //  //      outp.myExt.valid := True
  //  //      nextPrevTxnWasHazard := False
  //  //      setOutpModMemWord(
  //  //        someModMemWord=someModMemWord
  //  //      )
  //  //    }
  //  //    def handleDuplicateIt(
  //  //      actuallyDuplicateIt: Boolean=true
  //  //    ): Unit = {
  //  //      outp := (
  //  //        RegNext(outp) init(outp.getZero)
  //  //      )
  //  //      outp.myExt.valid := False
  //  //      outp.myExt.modMemWordValid := (
  //  //        False
  //  //      )
  //  //      if (actuallyDuplicateIt) {
  //  //        cMid0Front.duplicateIt()
  //  //      }
  //  //    }
  //  //    val rState = KeepAttribute(
  //  //      Reg(Bool())
  //  //      init(False)
  //  //    )
  //  //      .setName(
  //  //        s"doHandleHazardWithDcacheMiss"
  //  //        + s"_${doCheckHazard}_${haveCurrLoad}"
  //  //        + s"_rState"
  //  //        + s"${ydx}"
  //  //      )
  //  //    val rSavedModMemWord1 = (
  //  //      Reg(cloneOf(myModMemWord))
  //  //      init(myModMemWord.getZero)
  //  //      .setName(
  //  //        s"FlareCpu_doModInModFrontFunc"
  //  //        + s"_${doCheckHazard}_${haveCurrLoad}"
  //  //        + s"_rSavedModMemWord1"
  //  //        + s"${ydx}"
  //  //      )
  //  //    )
  //  //      
  //  //    switch (rState) {
  //  //      //is (False) {
  //  //      //  when (
  //  //      //    !tempModFrontPayload.dcacheHit
  //  //      //  ) {
  //  //      //    when (
  //  //      //      modFront.isValid
  //  //      //    ) {
  //  //      //      if (haveCurrLoad) {
  //  //      //        //cMid0Front.duplicateIt()
  //  //      //        handleDuplicateIt()
  //  //      //        rSavedModMemWord1 := myModMemWord
  //  //      //        rState := True
  //  //      //      } else {  // if (!haveCurrLoad)
  //  //      //        when (modFront.isFiring) {
  //  //      //          handleCurrFire()
  //  //      //        }
  //  //      //      }
  //  //      //    } otherwise { // when (!modFront.isFiring)
  //  //      //      handleDuplicateIt()
  //  //      //    }
  //  //      //  } otherwise {
  //  //      //    when (cMid0Front.up.isFiring) {
  //  //      //      handleCurrFire()
  //  //      //    }
  //  //      //  }
  //  //      //}
  //  //      //is (True) {
  //  //      //  when (cMid0Front.up.isFiring) {
  //  //      //    handleCurrFire(
  //  //      //      someModMemWord=rSavedModMemWord1
  //  //      //    )
  //  //      //  } otherwise {
  //  //      //    handleDuplicateIt(actuallyDuplicateIt=false)
  //  //      //  }
  //  //      //}
  //  //    }
  //  //  }
  //  //  when (cMid0Front.up.isValid) {
  //  //    //switch (inp.op) {
  //  //    //  is (PipeMemRmwSimDut.ModOp.ADD_RA_RB) {
  //  //    //    if (!doCheckHazard) {
  //  //    //      setOutpModMemWord()
  //  //    //    } else { // if (doCheckHazard)
  //  //    //      doHandleHazardWithDcacheMiss(
  //  //    //        haveCurrLoad=false,
  //  //    //      )
  //  //    //    }
  //  //    //  }
  //  //    //  is (PipeMemRmwSimDut.ModOp.LDR_RA_RB) {
  //  //    //    if (!doCheckHazard) {
  //  //    //      when (cMid0Front.up.isFiring) {
  //  //    //        setOutpModMemWord()
  //  //    //        nextPrevTxnWasHazard := True
  //  //    //      }
  //  //    //    } else { // if (doCheckHazard)
  //  //    //      nextPrevTxnWasHazard := True
  //  //    //      doHandleHazardWithDcacheMiss(
  //  //    //        haveCurrLoad=true,
  //  //    //      )
  //  //    //    }
  //  //    //  }
  //  //    //  is (PipeMemRmwSimDut.ModOp.MUL_RA_RB) {
  //  //    //    // we should stall `EX` in this case until the
  //  //    //    // calculation is done. The same stalling logic
  //  //    //    // will be used for `divmod`, etc.
  //  //    //    switch (rHaltItState) {
  //  //    //      is (
  //  //    //        False//PipeMemRmwSimDutHaltItState.IDLE
  //  //    //      ) {
  //  //    //        doMulHaltItFsmIdleInnards(
  //  //    //          doDuplicateIt=(
  //  //    //            //true
  //  //    //            doCheckHazard
  //  //    //          )
  //  //    //        )
  //  //    //      }
  //  //    //      is (
  //  //    //        //PipeMemRmwSimDutHaltItState.HALT_IT
  //  //    //        True
  //  //    //      ) {
  //  //    //        doMulHaltItFsmHaltItInnards()
  //  //    //        when (
  //  //    //          nextHaltItState
  //  //    //          //=== PipeMemRmwSimDutHaltItState.IDLE
  //  //    //          === False
  //  //    //        ) {
  //  //    //          nextPrevTxnWasHazard := False
  //  //    //        }
  //  //    //      }
  //  //    //    }
  //  //    //  }
  //  //    //}
  //  //  }
  //  //}
  //  //when (
  //  //  (
  //  //    //if (
  //  //    //  //PipeMemRmwSimDut.doAddrOneHaltIt
  //  //    //  PipeMemRmwSimDut.doTestModOp
  //  //    //) (
  //  //      rPrevTxnWasHazard
  //  //    //) else (
  //  //    //  False
  //  //    //)
  //  //  ) 
  //  //) {
  //  //  assert(PipeMemRmwSimDut.modRdPortCnt == 1)
  //  //  doTestModOpMain(
  //  //    doCheckHazard=true
  //  //  )
  //  //} elsewhen (
  //  //  cMid0Front.up.isValid
  //  //) {
  //  //  doTestModOpMain()
  //  //  //when (
  //  //  //  False
  //  //  //) {
  //  //  //  //cMid0Front.haltIt()
  //  //  //} elsewhen (
  //  //  //  //if (optModHazardKind == PipeMemRmw.modHazardKindDupl) (
  //  //  //  //  outp.myExt.hazardId.msb
  //  //  //  //) else (
  //  //  //    True
  //  //  //  //)
  //  //  //) {
  //  //  //  //if (
  //  //  //  //  //PipeMemRmwSimDut.doAddrOneHaltIt
  //  //  //  //  PipeMemRmwSimDut.doTestModOp
  //  //  //  //) {
  //  //  //    doTestModOpMain()
  //  //  //  //} else {
  //  //  //  //  setOutpModMemWord()
  //  //  //  //}
  //  //  //}
  //  //}
  //}
  //val cMemArea = new cMem.Area {
  //  //when (up.isFiring) {
  //  //}
  //}
  //--------
  Builder(linkArr.toSeq)
  //--------
}
object FlareCpuVerilog extends App {
  Config.spinal.generateVerilog(FlareCpu(params=FlareCpuParams()))
}

