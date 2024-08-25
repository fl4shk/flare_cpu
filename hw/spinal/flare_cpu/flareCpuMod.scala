package flare_cpu
import spinal.core._
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
  isIcache: Boolean
) extends Bundle with IMasterSlave {

  val valid = in(Bool())
  val ready = out(Bool())

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
) extends Bundle {
  //--------
  //val bus = master(Bmb(p=params.busParams))
  //val ibus = AsyncMemoryBus(config=AsyncMemoryBusConfig(
  //))
  val ibus = master(FlareCpuInnerBus(
    params=params,
    isIcache=true,
  ))
  val dbus = master(FlareCpuInnerBus(
    params=params,
    isIcache=false,
  ))
  val irq = in(Bool())
  //--------
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

case class FlareCpu(
  params: FlareCpuParams
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

  //def enumPipeMemIcache = 0
  //def enumPipeMemDcache = 1
  //def enumPipeMemGprFileEven = 2
  //def enumPipeMemGprFileOddNonSp = 3
  //def enumPipeMemGprFileSp = 4
  //def enumPipeMemSprFile = 5
  //def enumPipeMemLim = 6

  val linkArr = PipeHelper.mkLinkArr()
  //def mkRegFilePipeMemDoModInModFrontFunc(
  //  regFileKind: Int
  //): (
  //  Bool, // nextPrevTxnWasHazard
  //  Bool, // rPrevTxnWasHazard,
  //  FlareCpuPipeMemModType[ // outp
  //    UInt,
  //    Bool,
  //    PipeMemModExtType,
  //  ],
  //  FlareCpuPipeMemModType[ // inp
  //    UInt,
  //    Bool,
  //    PipeMemModExtType,
  //  ],
  //  CtrlLink,   // mod.front.cMid0Front
  //  Node,       // io.modFront
  //  FlareCpuPipeMemModType[ // io.tempModFrontPayload
  //    UInt,
  //    Bool,
  //    PipeMemModExtType,
  //  ],
  //  UInt,                 // myModMemWord
  //) => Unit = {
  //  val retFunc = (
  //    nextPrevTxnWasHazard: Bool,
  //    rPrevTxnWasHazard: Bool,
  //    outp: FlareCpuPipeMemModType[
  //      UInt,
  //      Bool,
  //      PipeMemModExtType,
  //    ],
  //    inp: FlareCpuPipeMemModType[
  //      UInt,
  //      Bool,
  //      PipeMemModExtType,
  //    ],
  //    cMid0Front: CtrlLink,
  //    modFront: Node,
  //    tempModFrontPayload: FlareCpuPipeMemModType[
  //      UInt,
  //      Bool,
  //      PipeMemModExtType,
  //    ],
  //    myModMemWord: UInt,
  //  ) => {
  //    //nextPrevTxnWasHazard := False

  //    outp := inp
  //    outp.allowOverride

  //    def instrDecEtc = inp.modExt.instrDecEtc

  //    if (regFileKind == enumRegFileGprEven) {
  //      outp.myExt.modMemWordValid := (
  //        //!instrDecEtc.raIdx.fire
  //        //|| instrDecEtc.raIdx.payload(0)
  //        instrDecEtc.regFileGprEvenModMemWordValid
  //      )
  //    } else if (regFileKind == enumRegFileGprOddNonSp) {
  //      outp.myExt.modMemWordValid := (
  //        //!instrDecEtc.raIdx.fire
  //        //|| !instrDecEtc.raIdx.payload(0)
  //        instrDecEtc.regFileGprOddNonSpModMemWordValid
  //      )
  //    } else if (regFileKind == enumRegFileGprSp) {
  //      outp.myExt.modMemWordValid := (
  //        //!instrDecEtc.raIdx.fire
  //        //|| (
  //        //  instrDecEtc.raIdx.payload
  //        //  === FlareCpuInstrEncConst.gprSpIdx
  //        //)
  //        instrDecEtc.regFileGprSpModMemWordValid
  //      )
  //    } else { // if (regFileKind == enumRegFileSpr)
  //      outp.myExt.modMemWordValid := (
  //        //!instrDecEtc.raIdx.fire
  //        instrDecEtc.regFileSprModMemWordValid
  //      )
  //    }
  //    val fakeRet = Bool()
  //    fakeRet := True // needed for the `Unit` Scala type to be inferred as
  //                    // the return value of this function
  //  }
  //  retFunc
  //}

  case class PipeMemModExtType(
  ) extends Bundle {
    val regPc = UInt(params.mainWidth bits)
    val instrEnc = FlareCpuInstrEnc(params=params)
    val instrDecEtc = FlareCpuInstrDecEtc(params=params)
    //val icache = FlareCpuIcachePipePayload(params=params)
    //val dcache = FlareCpuDcachePipePayload(params=params)
  }
  // `exSetPc` is to be driven by the `EX` pipeline stage
  val exSetPc = Flow(UInt(params.mainWidth bits))
  exSetPc.allowOverride
  exSetPc := (
    RegNext(exSetPc)
    init(exSetPc.getZero)
  )

  //def mkIcacheModType() = (
  //  FlareCpuPipeMemModType(
  //    params=params,
  //    wordType=IcacheWordType(),
  //    wordCount=params.icacheLineMemWordCount,
  //    hazardCmpType=Bool(),
  //    modRdPortCnt=params.icacheModRdPortCnt,
  //    modStageCnt=params.icacheModStageCnt,
  //    optModHazardKind=params.icacheOptModHazardKind,
  //    modExtType=PipeMemModExtType(),
  //  )
  //)
  //val icache = PipeMemRmw[
  //  IcacheWordType,
  //  Bool,
  //  FlareCpuPipeMemModType[
  //    IcacheWordType,
  //    Bool,
  //    PipeMemModExtType,
  //  ],
  //  PipeMemRmwDualRdTypeDisabled[IcacheWordType, Bool],
  //](
  //  wordType=IcacheWordType(),
  //  wordCount=params.icacheLineMemWordCount,
  //  hazardCmpType=Bool(),
  //  modType=mkIcacheModType(),
  //  modRdPortCnt=params.icacheModRdPortCnt,
  //  modStageCnt=params.icacheModStageCnt,
  //  pipeName="FlareCpu_icache",
  //  linkArr=Some(linkArr),
  //  memArrIdx=enumPipeMemIcache,
  //  memArrSize=enumPipeMemLim,
  //  optDualRd=false,
  //  optReorder=false,
  //  initBigInt=Some({
  //    val tempArr = new ArrayBuffer[BigInt]()
  //    for (idx <- 0 until params.icacheLineMemWordCount) {
  //      tempArr += BigInt(0)
  //    }
  //    tempArr.toSeq
  //  }),
  //  optModHazardKind=params.icacheOptModHazardKind,
  //  optEnableClear=false,
  //  memRamStyle="block",
  //  vivadoDebug=false,
  //  optIncludeModFrontStageLink=false,
  //)(
  //  //doModInFrontFunc=Some(
  //  //  (
  //  //    outp: FlareCpuPipeMemModType[
  //  //      IcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    inp: FlareCpuPipeMemModType[
  //  //      IcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    cFront,
  //  //  ) => {
  //  //    when (cFront.up.isFiring) {
  //  //    }
  //  //  }
  //  //)
  //  //doModInModFrontFunc=Some(
  //  //  (
  //  //    nextPrevTxnWasHazard: Bool,
  //  //    rPrevTxnWasHazard: Bool,
  //  //    outp: FlareCpuPipeMemModType[
  //  //      IcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    inp: FlareCpuPipeMemModType[
  //  //      IcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    cMid0Front: CtrlLink,
  //  //    modFront: Node,
  //  //    tempModFrontPayload: FlareCpuPipeMemModType[
  //  //      IcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    myModMemWord: IcacheWordType,
  //  //  ) => {
  //  //  }
  //  //)
  //)
  //def mkDcacheModType() = (
  //  FlareCpuPipeMemModType(
  //    params=params,
  //    wordType=DcacheWordType(),
  //    wordCount=params.dcacheLineMemWordCount,
  //    hazardCmpType=Bool(),
  //    modRdPortCnt=params.dcacheModRdPortCnt,
  //    modStageCnt=params.dcacheModStageCnt,
  //    optModHazardKind=params.dcacheOptModHazardKind,
  //    modExtType=PipeMemModExtType(),
  //  )
  //)
  //val dcache = PipeMemRmw[
  //  DcacheWordType,
  //  Bool,
  //  FlareCpuPipeMemModType[
  //    DcacheWordType,
  //    Bool,
  //    PipeMemModExtType,
  //  ],
  //  PipeMemRmwDualRdTypeDisabled[DcacheWordType, Bool],
  //](
  //  wordType=DcacheWordType(),
  //  wordCount=params.dcacheLineMemWordCount,
  //  hazardCmpType=Bool(),
  //  modType=mkDcacheModType(),
  //  modRdPortCnt=params.dcacheModRdPortCnt,
  //  modStageCnt=params.dcacheModStageCnt,
  //  pipeName="FlareCpu_dcache",
  //  linkArr=Some(linkArr),
  //  memArrIdx=enumPipeMemDcache,
  //  memArrSize=enumPipeMemLim,
  //  optDualRd=false,
  //  optReorder=false,
  //  initBigInt=Some({
  //    val tempArr = new ArrayBuffer[BigInt]()
  //    for (idx <- 0 until params.dcacheLineMemWordCount) {
  //      tempArr += BigInt(0)
  //    }
  //    tempArr.toSeq
  //  }),
  //  optModHazardKind=params.dcacheOptModHazardKind,
  //  optEnableClear=false,
  //  memRamStyle="block",
  //  vivadoDebug=false,
  //  optIncludeModFrontStageLink=false,
  //)(
  //  //doModInFrontFunc=Some(
  //  //  (
  //  //    outp: FlareCpuPipeMemModType[
  //  //      DcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    inp: FlareCpuPipeMemModType[
  //  //      DcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    cFront,
  //  //  ) => {
  //  //    when (cFront.up.isFiring) {
  //  //    }
  //  //  }
  //  //)
  //  //doModInModFrontFunc=Some(
  //  //  (
  //  //    nextPrevTxnWasHazard: Bool,
  //  //    rPrevTxnWasHazard: Bool,
  //  //    outp: FlareCpuPipeMemModType[
  //  //      DcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    inp: FlareCpuPipeMemModType[
  //  //      DcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    cMid0Front: CtrlLink,
  //  //    modFront: Node,
  //  //    tempModFrontPayload: FlareCpuPipeMemModType[
  //  //      DcacheWordType,
  //  //      Bool,
  //  //      PipeMemModExtType,
  //  //    ],
  //  //    myModMemWord: DcacheWordType,
  //  //  ) => {
  //  //  }
  //  //)
  //)
  //case class IdForkType() {
  //  val regFile = 
  //}
  def mkRegFileModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCountMax=params.sprFileEvenWordCount,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.regFileModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  //def mkGprFileEvenModType() = (
  //  FlareCpuPipeMemModType(
  //    params=params,
  //    wordType=params.regWordType(),
  //    wordCount=params.gprFileEvenWordCount,
  //    hazardCmpType=params.regFileHazardCmpType(),
  //    modRdPortCnt=params.regFileNonSpModRdPortCnt,
  //    modStageCnt=params.regFileModStageCnt,
  //    optModHazardKind=params.regFileOptModHazardKind,
  //    modExtType=PipeMemModExtType(),
  //  )
  //)
  //val gprFileEven = PipeMemRmw[
  //  UInt,
  //  Bool,
  //  FlareCpuPipeMemModType[
  //    UInt,
  //    Bool,
  //    PipeMemModExtType,
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

  val regFile = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool]
  ](
    wordType=params.regWordType(),
    wordCountArr={
      val myArr = ArrayBuffer[Int]()
      myArr += params.gprFileEvenNonFpWordCount
      myArr += params.gprFileFpWordCount
      myArr += params.gprFileOddNonSpWordCount
      myArr += params.gprFileSpWordCount
      //myArr += params.sprFileWordCount
      myArr += params.sprFileEvenWordCount
      myArr += params.sprFileOddWordCount
      myArr.toSeq
    },
    hazardCmpType=params.regFileHazardCmpType(),
    modType=mkRegFileModType(),
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
    optIncludeModFrontStageLink=false,
  )(
    doModInFrontFunc=Some(
      (
        outp: FlareCpuPipeMemModType[
          UInt,
          Bool,
          PipeMemModExtType,
        ],
        inp: FlareCpuPipeMemModType[
          UInt,
          Bool,
          PipeMemModExtType,
        ],
        cFront: CtrlLink,
        ydx: Int,
      ) => {
        outp := inp
      }
    ),
    doModInModFrontFunc=Some(
      (
        //PipeMemRmwPayloadExt[WordT, HazardCmpT],  // inp
        //PipeMemRmwPayloadExt[WordT, HazardCmpT],  // outp
        nextPrevTxnWasHazard: Bool, // nextPrevTxnWasHazard,
        rPrevTxnWasHazard: Bool, // rPrevTxnWasHazard,
        outp: FlareCpuPipeMemModType[
          UInt,
          Bool,
          PipeMemModExtType,
        ], // outp
        inp: FlareCpuPipeMemModType[
          UInt,
          Bool,
          PipeMemModExtType,
        ], // inp
        cMid0Front: CtrlLink, // mod.front.cMid0Front
        modFront: Node,     // io.modFront
        tempModFrontPayload: FlareCpuPipeMemModType[
          UInt,
          Bool,
          PipeMemModExtType,
        ], // io.tempModFrontPayload
        myModMemWord: UInt,    // myModMemWord
        //Vec[WordT],  // myRdMemWord
        ydx: Int,    // ydx
      ) => {
        outp := inp
      },
    ),
  )
  //--------
  //--------
  val pIf = Payload(PipeMemModExtType())
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
  val pId = Payload(PipeMemModExtType())
  val cId = CtrlLink(
    up=sIf.down,
    down=(
      //Node()
      regFile.io.front
    ),
  )
  linkArr += cId
  //--------
  val cEx = CtrlLink(
    up=regFile.mod.front.cMid0Front.down,
    down=Node(),
  )
  linkArr += cEx
  val sEx = StageLink(
    up=cEx.down,
    down=regFile.io.modFront,
  )
  linkArr += sEx
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
  val cIfArea = new cIf.Area {
    //--------
    val upPayload = PipeMemModExtType()
    up(pIf) := upPayload
    upPayload := (
      RegNext(upPayload)
      init(upPayload.getZero)
    )
    //--------
    val rSavedExSetPc = (
      Reg(Flow(UInt(params.mainWidth bits)))
    )
    rSavedExSetPc.init(rSavedExSetPc.getZero)

    when (exSetPc.fire) {
      rSavedExSetPc := exSetPc
    }

    when (up.isFiring) {
      //--------
      rSavedExSetPc := rSavedExSetPc.getZero
      //--------
      when (exSetPc.fire) {
        upPayload.regPc := exSetPc.payload //+ (params.instrMainWidth / 8)
      } elsewhen (rSavedExSetPc.fire) {
        upPayload.regPc := (
          rSavedExSetPc.payload //+ (params.instrMainWidth / 8)
        )
      } otherwise {
        upPayload.regPc := upPayload.regPc + (params.instrMainWidth / 8)
      }
    }
    //when (exSetPc.fire) {
    //  upPayload.regPc := exSetPc.payload + (params.instrMainWidth / 8)
    //}
    //--------
    io.ibus.valid := True
    io.ibus.addr := upPayload.regPc
    //--------
    when (!io.ibus.ready) {
      haltIt()
    }
    //--------
    //when (io.ibus.ready) {
    //  when (exSetPc.fire) {
    //    upPayload.regPc := exSetPc.payload
    //  } elsewhen (rSavedExSetPc.fire) {
    //    upPayload.regPc := rSavedExSetPc.payload
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
  val cIdArea = new cId.Area {
    //--------
    val upPayload = PipeMemModExtType()
    up(pId) := upPayload
    upPayload := (
      RegNext(upPayload)
      init(upPayload.getZero)
    )
    //--------
    def upInstrEnc = upPayload.instrEnc
    def upInstrDecEtc = upPayload.instrDecEtc
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

    up(regFile.io.frontPayloadArr(enumRegFileGprEvenNonFp)) := (
      myFrontPayloadGprEvenNonFp
    )
    up(regFile.io.frontPayloadArr(enumRegFileGprFp)) := (
      myFrontPayloadGprFp
    )
    up(regFile.io.frontPayloadArr(enumRegFileGprOddNonSp)) := (
      myFrontPayloadGprOddNonSp
    )
    up(regFile.io.frontPayloadArr(enumRegFileGprSp)) := (
      myFrontPayloadGprSp
    )
    up(regFile.io.frontPayloadArr(enumRegFileSprEven)) := (
      myFrontPayloadSprEven
    )
    up(regFile.io.frontPayloadArr(enumRegFileSprOdd)) := (
      myFrontPayloadSprOdd
    )

    when (up.isFiring) {
      //upInstrDecEtc := upInstrDecEtc.getZero
      //upInstrDecEtc.isInvalid := False
      //upInstrDecEtc.haveFullInstr := True

      //def clearRegsMain(): Unit = {
      //}
      //--------
      myFrontPayloadGprEvenNonFp.myExt.modMemWordValid := (
        upInstrDecEtc.gprEvenNonFpRaIdx.valid
      )
      myFrontPayloadGprEvenNonFp.myExt.memAddr(0) := (
        upInstrDecEtc.gprEvenNonFpRaIdx.payload(
          myFrontPayloadGprEvenNonFp.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadGprEvenNonFp.myExt.memAddr(1) := (
        upInstrDecEtc.gprEvenNonFpRbIdx(
          myFrontPayloadGprEvenNonFp.myExt.memAddr(1).bitsRange
        )
      )
      //--------
      myFrontPayloadGprFp.myExt.modMemWordValid := (
        upInstrDecEtc.gprFpRaIdx.valid
      )
      myFrontPayloadGprFp.myExt.memAddr(0) := (
        upInstrDecEtc.gprFpRaIdx.payload(
          myFrontPayloadGprFp.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadGprFp.myExt.memAddr(1) := (
        upInstrDecEtc.gprFpRbIdx(
          myFrontPayloadGprFp.myExt.memAddr(1).bitsRange
        )
      )
      //--------
      myFrontPayloadGprOddNonSp.myExt.modMemWordValid := (
        upInstrDecEtc.gprOddNonSpRaIdx.valid
      )
      myFrontPayloadGprOddNonSp.myExt.memAddr(0) := (
        upInstrDecEtc.gprOddNonSpRaIdx.payload(
          myFrontPayloadGprOddNonSp.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadGprOddNonSp.myExt.memAddr(1) := (
        upInstrDecEtc.gprOddNonSpRbIdx(
          myFrontPayloadGprOddNonSp.myExt.memAddr(1).bitsRange
        )
      )
      //--------
      myFrontPayloadGprSp.myExt.modMemWordValid := (
        upInstrDecEtc.gprSpRaIdx.valid
      )
      myFrontPayloadGprSp.myExt.memAddr(0) := (
        upInstrDecEtc.gprSpRaIdx.payload(
          myFrontPayloadGprSp.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadGprSp.myExt.memAddr(1) := (
        upInstrDecEtc.gprSpRbIdx(
          myFrontPayloadGprSp.myExt.memAddr(1).bitsRange
        )
      )
      //--------
      myFrontPayloadSprEven.myExt.modMemWordValid := (
        upInstrDecEtc.sprEvenSaIdx.valid
      )
      myFrontPayloadSprEven.myExt.memAddr(0) := (
        upInstrDecEtc.sprEvenSaIdx.payload(
          myFrontPayloadSprEven.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadSprEven.myExt.memAddr(1) := (
        upInstrDecEtc.sprEvenSbIdx(
          myFrontPayloadSprEven.myExt.memAddr(1).bitsRange
        )
      )
      //--------
      myFrontPayloadSprOdd.myExt.modMemWordValid := (
        upInstrDecEtc.sprOddSaIdx.valid
      )
      myFrontPayloadSprOdd.myExt.memAddr(0) := (
        upInstrDecEtc.sprOddSaIdx.payload(
          myFrontPayloadSprOdd.myExt.memAddr(0).bitsRange
        )
      )
      myFrontPayloadSprOdd.myExt.memAddr(1) := (
        upInstrDecEtc.sprOddSbIdx(
          myFrontPayloadSprOdd.myExt.memAddr(1).bitsRange
        )
      )
      //--------

      def finishInstr(
        //isBlJl: Boolean=false,
        ////writeSprFlags: Option[Bool]=None,
        //writeGpr: Bool=True,
        writeGpr: Option[(UInt, Boolean)]=Some((U"1'd0", false)),
        writeSpr0: Option[(UInt, Bool)]=None,
        writeSpr1: Option[(UInt, Bool)]=None,
      ): Unit = {
        upInstrDecEtc.decodeTempIndexRaRbValid := False
        upInstrDecEtc.decodeTempIndexRaSimmValid := False
        upInstrDecEtc.decodeTempPreLpreValid := False
        upInstrDecEtc.decodeTempPreValid := False
        upInstrDecEtc.decodeTempLpreValid := False

        upInstrDecEtc.isInvalid := False
        upInstrDecEtc.haveFullInstr := True

        //upInstrDecEtc.haveFullInstr := True

        //--------
        // BEGIN: Old design for `finishInstr()`'s writing rA
        //if (!isBlJl) {
        //  upInstrDecEtc.raIdx := upInstrEnc.g2.raIdx
        //} else { // if (isBlSimm)
        //  upInstrDecEtc.raIdx := FlareCpuInstrEncConst.gprLrIdx
        //}
        // END: Old design for `finishInstr()`'s writing rA
        writeGpr match {
          case Some(myWriteGpr) => {
            val tempRaIdx = (
              if (!myWriteGpr._2) (
                upInstrDecEtc.raIdx
              ) else (
                myWriteGpr._1
              )
            )
            upInstrDecEtc.gprEvenNonFpRaIdx.valid := (
              !tempRaIdx(0)
              && (
                tempRaIdx =/= FlareCpuInstrEncConst.gprFpIdx
              )
            )
            upInstrDecEtc.gprFpRaIdx.valid := (
              tempRaIdx === FlareCpuInstrEncConst.gprFpIdx
            )
            upInstrDecEtc.gprOddNonSpRaIdx.valid := (
              tempRaIdx(0)
              && (
                tempRaIdx =/= FlareCpuInstrEncConst.gprSpIdx
              )
            )
            upInstrDecEtc.gprSpRaIdx.valid := (
              tempRaIdx === FlareCpuInstrEncConst.gprSpIdx
            )
          }
          case None => {
            upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
            upInstrDecEtc.gprOddNonSpRaIdx.valid := False
            upInstrDecEtc.gprSpRaIdx.valid := False
          }
        }
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
        upInstrDecEtc.gprEvenNonFpRbIdx := (
          Cat(
            U"1'd0",
            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
          ).asUInt
        )
        //--------
        upInstrDecEtc.gprFpRaIdx.payload := (
          0x0
        )
        upInstrDecEtc.gprFpRbIdx := 0x0
        //--------
        upInstrDecEtc.gprOddNonSpRaIdx.payload := (
          Cat(
            U"1'd0",
            upInstrDecEtc.raIdx(upInstrDecEtc.raIdx.high downto 1),
          ).asUInt
        )
        upInstrDecEtc.gprOddNonSpRbIdx := (
          Cat(
            U"1'd0",
            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
          ).asUInt
        )
        //--------
        upInstrDecEtc.gprSpRaIdx.payload := (
          0x0
        )
        upInstrDecEtc.gprSpRbIdx := 0x0
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
        writeSpr0 match {
          case Some(myWriteSpr0) => {
            when (myWriteSpr0._2) {
              //if (myWriteSpr0._1 % 2 == 0) {
              //} else {
              //}
              when (!myWriteSpr0._1.lsb) {
                upInstrDecEtc.sprEvenSaIdx.valid := True
                upInstrDecEtc.sprEvenSaIdx.payload := myWriteSpr0._1
                writeSpr1 match {
                  case Some(myWriteSpr1) => {
                  }
                  case None => {
                    //upInstrDecEtc.sprEvenSaIdx.valid := False
                    disableSprOddWrite()
                  }
                }
              } otherwise {
                upInstrDecEtc.sprOddSaIdx.valid := True
                upInstrDecEtc.sprOddSaIdx.payload := myWriteSpr0._1
                writeSpr1 match {
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
            writeSpr1 match {
              case Some(myWriteSpr1) => {
              }
              case None => {
                disableSprWrites()
              }
            }
          }
        }
        writeSpr1 match {
          case Some(myWriteSpr1) => {
            when (myWriteSpr1._2) {
              //if (myWriteSpr1._1 % 2 == 0) {
              //} else {
              //}
              when (!myWriteSpr1._1.lsb) {
                upInstrDecEtc.sprEvenSaIdx.valid := True
                upInstrDecEtc.sprEvenSaIdx.payload := myWriteSpr1._1
              } otherwise {
                upInstrDecEtc.sprOddSaIdx.valid := True
                upInstrDecEtc.sprOddSaIdx.payload := myWriteSpr1._1
              }
            }
          }
          case None => {
          }
        }
      }
      def writeSprFlags(cond: Bool): Option[(UInt, Bool)] = (
        Some(
          (FlareCpuInstrEncConst.sprFlagsIdx, cond)
        )
      )
      def markInstrInvalid(): Unit = {
        upInstrDecEtc.haveFullInstr := True 
        upInstrDecEtc.isInvalid := True
        upInstrDecEtc.decOp := FlareCpuInstrDecOp.bubble
        upInstrDecEtc.decodeTempPreLpreValid := False
        upInstrDecEtc.decodeTempPreValid := False
        upInstrDecEtc.decodeTempLpreValid := False
        disableRegWrites()
      }
      def disableGprWrites(): Unit = {
        upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
        upInstrDecEtc.gprFpRaIdx.valid := False
        upInstrDecEtc.gprOddNonSpRaIdx.valid := False
        upInstrDecEtc.gprSpRaIdx.valid := False
      }
      def disableSprEvenWrite(): Unit = {
        upInstrDecEtc.sprEvenSaIdx.valid := False
      }
      def disableSprOddWrite(): Unit = {
        upInstrDecEtc.sprOddSaIdx.valid := False
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

      when (rSavedUpInstrDecEtc.decOp =/= FlareCpuInstrDecOp.lpreSimmHi) {
        switch (upInstrEnc.g0Pre.grp) {
          is (FlareCpuInstrEncConst.g0Grp) {
            when (
              upInstrEnc.g0Pre.subgrp
              === FlareCpuInstrEncConst.g0PreSubgrp
            ) {
              when (!rSavedUpInstrDecEtc.decodeTempPreLpreValid) {
                upInstrDecEtc.decodeTempPreLpreValid := True
                upInstrDecEtc.decodeTempPreValid := True
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
                rSavedUpInstrDecEtc.decodeTempIndexRaRbValid,
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
            when (!rSavedUpInstrDecEtc.decodeTempPreLpreValid) {
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
                  writeSpr0=writeSprFlags(True)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                  writeSpr0=writeSprFlags(True)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                  writeSpr0=writeSprFlags(upInstrDecEtc.fwl)
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
              }
              is (FlareCpuInstrG2EncOp.sbcRaRb) {
                // Opcode 0xd: sbc rA, rB
                //when (upInstrDecEtc.fwl) {
                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRb
                //} otherwise {
                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRbFlags
                //}
                finishInstr(writeSpr0=writeSprFlags(upInstrDecEtc.fwl))
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
                  writeSpr0=writeSprFlags(True)
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
            when (!rSavedUpInstrDecEtc.decodeTempPreLpreValid) {
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
              upPayload.regPc
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
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.braSimm
              }
              is (FlareCpuInstrG3EncOp.beqS9) {
                // Opcode 0x2: beq simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.beqSimm
              }
              is (FlareCpuInstrG3EncOp.bneS9) {
                // Opcode 0x3: bne simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bneSimm
              }
              is (FlareCpuInstrG3EncOp.bmiS9) {
                // Opcode 0x4: bmi simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bmiSimm
              }
              is (FlareCpuInstrG3EncOp.bplS9) {
                // Opcode 0x5: bpl simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bplSimm
              }
              is (FlareCpuInstrG3EncOp.bvsS9) {
                // Opcode 0x6: bvs simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvsSimm
              }
              is (FlareCpuInstrG3EncOp.bvcS9) {
                // Opcode 0x7: bvc simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvcSimm
              }
              is (FlareCpuInstrG3EncOp.bgeuS9) {
                // Opcode 0x8: bgeu simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgeuSimm
              }
              is (FlareCpuInstrG3EncOp.bltuS9) {
                // Opcode 0x9: bltu simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltuSimm
              }
              is (FlareCpuInstrG3EncOp.bgtuS9) {
                // Opcode 0xa: bgtu simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtuSimm
              }
              is (FlareCpuInstrG3EncOp.bleuS9) {
                // Opcode 0xb: bleu simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bleuSimm
              }
              is (FlareCpuInstrG3EncOp.bgesS9) {
                // Opcode 0xc: bges simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgesSimm
              }
              is (FlareCpuInstrG3EncOp.bltsS9) {
                // Opcode 0xd: blts simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltsSimm
              }
              is (FlareCpuInstrG3EncOp.bgtsS9) {
                // Opcode 0xe: bgts simm
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtsSimm
              }
              is (FlareCpuInstrG3EncOp.blesS9) {
                // Opcode 0xf: bles simm
                finishInstr()
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
                  writeGpr=Some((FlareCpuInstrEncConst.gprLrIdx, true))
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jlRa
              }
              is (FlareCpuInstrG4EncOp.jmpRa) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpRa
              }
              is (FlareCpuInstrG4EncOp.jmpIra) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpIra
              }
              is (FlareCpuInstrG4EncOp.reti) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.reti
              }
              is (FlareCpuInstrG4EncOp.ei) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ei
              }
              is (FlareCpuInstrG4EncOp.di) {
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.di
              }
              is (FlareCpuInstrG4EncOp.pushRaRb) {
                // Opcode 0x6: push rA, rB
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.pushRaRb
              }
              is (FlareCpuInstrG4EncOp.pushSaRb) {
                // Opcode 0x7: push sA, rB
                finishInstr()
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
                finishInstr()
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
                finishInstr(writeSpr0=Some(
                  FlareCpuInstrEncConst.sprLoIdx, True
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
              }
              is (FlareCpuInstrG4EncOp.sdivmodRaRb) {
                // Opcode 0xd: sdivmod rA, rB
                finishInstr(writeSpr0=Some(
                  FlareCpuInstrEncConst.sprLoIdx, True
                ))
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
              }
              is (FlareCpuInstrG4EncOp.lumulRaRb) {
                // Opcode 0xe: lumul rA, rB
                finishInstr(
                  writeSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True
                  ),
                  writeSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lumulRaRb
              }
              is (FlareCpuInstrG4EncOp.lsmulRaRb) {
                // Opcode 0xf: lsmul rA, rB
                finishInstr(
                  writeSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True
                  ),
                  writeSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsmulRaRb
              }
              is (FlareCpuInstrG4EncOp.udivmod64RaRb) {
                // Opcode 0x10: udivmod64 rA, rB
                finishInstr(
                  writeSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True
                  ),
                  writeSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True
                  ),
                )
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmod64RaRb
              }
              is (FlareCpuInstrG4EncOp.sdivmod64RaRb) {
                // Opcode 0x11: sdivmod64 rA, rB
                finishInstr(
                  writeSpr0=Some(
                    FlareCpuInstrEncConst.sprHiIdx, True
                  ),
                  writeSpr1=Some(
                    FlareCpuInstrEncConst.sprLoIdx, True
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
                finishInstr()
                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpySaRb
              }
              is (FlareCpuInstrG4EncOp.cpySaSb) {
                // Opcode 0x1e: cpy sA, sB
                finishInstr()
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
              when (!rSavedUpInstrDecEtc.decodeTempIndexRaRbValid) {
                upInstrDecEtc.decodeTempIndexRaRbValid := True
                upInstrDecEtc.haveFullInstr := False
              } otherwise {
                markInstrInvalid()
              }
            } elsewhen (
              upInstrEnc.g5Sg1.subgrp
              === FlareCpuInstrEncConst.g5Sg1Subgrp
            ) {
              upInstrDecEtc.haveFullInstr := False
              when (!rSavedUpInstrDecEtc.decodeTempIndexRaSimmValid) {
                upInstrDecEtc.decodeTempIndexRaSimmValid := True
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
            markInstrInvalid()
          }
          is (FlareCpuInstrEncConst.g7Grp) {
            when (
              upInstrEnc.g7Sg00.subgrp
              === FlareCpuInstrEncConst.g7Sg00Subgrp
            ) {
              upInstrDecEtc.fwl := upInstrEnc.g7Sg00.w
              switch (upInstrEnc.g7Sg00.op) {
                is (FlareCpuInstrG7Sg00FullOpEnc.cmpbRaRb) {
                  finishInstr(writeSpr0=writeSprFlags(True))
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
                  finishInstr(writeSpr0=writeSprFlags(True))
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
            } elsewhen (
              upInstrEnc.g7Sg0110.subgrp
              === FlareCpuInstrEncConst.g7Sg0110Subgrp
            ) {
              upInstrDecEtc.decOp := FlareCpuInstrDecOp.icreloadRaSimm
              finishInstr(writeGpr=None)
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
        }
      } otherwise { // when (the previous instruction was lpreSimmHi)
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
    }
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
      haltIt()
    }
  }
  val cExArea = new cEx.Area {
    
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
      flagsOut(params.flagIdxZ) := (result & tempFlagsZMask) =/= 0
      // set the `N` flag
      flagsOut(params.flagIdxN) := (result & tempFlagsVnMask) =/= 0
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
      flagsOut: Option[UInt]=None,
      withCarryIn: Boolean,
      doSub: Boolean,
      ret: UInt,
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
      flagsOut match {
        case Some(myFlagsOut) => {
          //temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
          //temp_flags_c_mask = FLARE_SIM_FLAGS_C_MASK (bits);
          tempFlagsVnMask := myFlagsVnMask(bits=bits)
          tempFlagsCMask := myFlagsCMask(bits=bits)

          //*flags_out = 0x0;
          //flare_sim_set_flags_zn (bits, tempRet, flags_out);
          setFlagsZn(
            bits=bits,
            result=tempRet,
            flagsOut=myFlagsOut,
          )

          ///* set the `C` flag */
          //if (tempRet & temp_flags_c_mask)
          //{
          //  *flags_out |= FLARE_FLAGS_C_MASK;
          //}
          myFlagsOut(params.flagIdxC) := (tempRet & tempFlagsCMask) =/= 0
          ///* set the `V` flag (6502-style) */
          ////if (!((temp_operand_a ^ temp_operand_b) & temp_flags_vn_mask)
          ////  && ((temp_operand_a ^ tempRet) & temp_flags_vn_mask))
          ///* The above ^ commented-out method is equivalent, but slower. */
          //if ((temp_operand_a ^ tempRet) & (temp_operand_b ^ tempRet)
          //  & temp_flags_vn_mask)
          //{
          //  *flags_out |= FLARE_FLAGS_V_MASK;
          //}
          myFlagsOut(params.flagIdxV) := (
            ((tempOperandA ^ tempRet) & (tempOperandB ^ tempRet)
            & tempFlagsVnMask)
          ) =/= 0
        }
        case None => {
        }
      }
    }
    //def performAddSub(
    //  rawElemNumBytesPow: (Int, Int),
    //  operandA: UInt,
    //  operandB: UInt,
    //  withCarryIn: Boolean,
    //  doSub: Boolean,
    //  doSetFlags: Boolean,
    //  //flagsOut: UInt
    //  result: UInt,
    //  flagsOut: Option[UInt]=None,
    //): Unit = {
    //  ////--------
    //  //def myBits = params.elemNumBytesPow(
    //  //  rawElemNumBytesPow=rawElemNumBytesPow
    //  //)._2
    //  //assert(result.getWidth == myBits + 1)
    //  ////--------
    //  ////uint64_t
    //  ////  ret = 0,
    //  ////  temp_operand_a = operand_a,
    //  ////  temp_operand_b = operand_b,
    //  ////  temp_flags_c_mask = 0,
    //  ////  temp_flags_vn_mask = 0;
    //  //val tempOperandA = UInt((myBits + 1) bits)
    //  //val tempOperandB = UInt((myBits + 1) bits)
    //  //tempOperandA := Cat(False, operandA).asUInt
    //  //tempOperandB := Cat(False, operandB).asUInt
    //  //if (!doSub) {
    //  //  //ret = temp_operand_a + temp_operand_b
    //  //  //+ (with_carry_in
    //  //  //  ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
    //  //  //  : 0x0ull);
    //  //  result := (
    //  //    tempOperandA + tempOperandB
    //  //    + (
    //  //      if (withCarryIn) {
    //  //        flags(params.flagIdxC downto params.flagIdxC)
    //  //      } else { // if (!withCarryIn)
    //  //        U"1'd0"
    //  //      }
    //  //    ).resized
    //  //  )
    //  //} else { // if (doSub)
    //  //  /* 6502-style subtraction */
    //  //  //ret = temp_operand_a + (~temp_operand_b)
    //  //  //  + (with_carry_in 
    //  //  //    ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
    //  //  //    : 0x1ull);
    //  //  result := (
    //  //    tempOperandA + (~tempOperandB)
    //  //    + (
    //  //      if (withCarryIn) {
    //  //        flags(params.flagIdxC downto params.flagIdxC)
    //  //      } else { // if (!withCarryIn)
    //  //        U"1'd1"
    //  //      }
    //  //    ).resized
    //  //  )
    //  //}

    //  //if (doSetFlags) {
    //  //  var useMyFlagsOut: Boolean = false
    //  //  val tempFlagsOut = flagsOut match {
    //  //    case Some(myFlagsOut) => {
    //  //      useMyFlagsOut = true
    //  //      myFlagsOut
    //  //    }
    //  //    case None => {
    //  //      //outpFlags
    //  //      UInt(params.mainWidth bits)
    //  //    }
    //  //  }
    //  //  val tempFlags = cloneOf(tempFlagsOut)
    //  //  tempFlagsOut := 0x0
    //  //  tempFlagsOut.allowOverride
    //  //  performSetFlagsZn(
    //  //    rawElemNumBytesPow=rawElemNumBytesPow,
    //  //    result=result,
    //  //  )
    //  //  tempFlagsOut(params.flagIdxC) := result(myBits)
    //  //  tempFlagsOut(params.flagIdxV) := (
    //  //    (tempOperandA ^ result.resized)
    //  //    & (tempOperandB ^ result.resized)
    //  //  )(myBits - 1)
    //  //  if (!useMyFlagsOut) {
    //  //    doWriteSpr(
    //  //      regIdx=myInstrDecEtc.enumSprFlags,
    //  //      payload=tempFlagsOut,
    //  //    )
    //  //  }
    //  //}
    //  ////--------
    //}
    //when (up.isFiring) {
    //}
    //exSetPc.valid := True
    //exSetPc.payload := 0x0
    //outp := inp
    //outp.allowOverride
    //val nextHaltItState = KeepAttribute(
    //  Bool()
    //).setName(s"FlareCpu_doModInModFrontFunc_nextHaltItState_${ydx}")
    //val rHaltItState = KeepAttribute(
    //  RegNext(nextHaltItState)
    //  init(nextHaltItState.getZero)
    //).setName(s"FlareCpu_doModInModFrontFunc_rHaltItState_${ydx}")
    //////val nextMulHaltItCnt = KeepAttribute(
    //////  SInt(4 bits)
    //////).setName("FlareCpu_doModInModFrontFunc_nextMulHaltItCnt")
    //val nextMulHaltItCnt = SInt(4 bits)
    //  .setName(s"FlareCpu_doModInModFrontFunc_nextMulHaltItCnt_${ydx}")
    //val rMulHaltItCnt = (
    //  RegNext(nextMulHaltItCnt)
    //  init(-1)
    //)
    //  .setName(s"FlareCpu_doModInModFrontFunc_rMulHaltItCnt_${ydx}")
    //nextHaltItState := rHaltItState
    //nextMulHaltItCnt := rMulHaltItCnt
    //def setOutpModMemWord(
    //  someModMemWord: UInt=myModMemWord
    //): Unit = {
    //  //outp.myExt.modMemWord := (
    //  //  someModMemWord + 0x1
    //  //)
    //  outp.myExt.modMemWordValid := True
    //}
    //val rSavedModMemWord = (
    //  Reg(cloneOf(myModMemWord))
    //  init(myModMemWord.getZero)
    //)
    //  .setName(s"FlareCpu_doModInModFrontFunc_rSavedModMemWord_${ydx}")
    //val rPrevOutp = KeepAttribute(
    //  RegNextWhen(
    //    outp,
    //    cMid0Front.up.isFiring
    //  )
    //  init(outp.getZero)
    //)
    //  .setName(s"FlareCpu_doModInModFrontFunc_rPrevOutp_${ydx}")
    //def doMulHaltItFsmIdleInnards(
    //  doDuplicateIt: Boolean
    //): Unit = {
    //  if (PipeMemRmwSimDut.doTestModOp) {
    //    def myInitMulHaltItCnt = 0x1
    //    cMid0Front.duplicateIt()
    //    when (
    //      //cMid0Front.down.isFiring
    //      modFront.isFiring
    //    ) {
    //      nextHaltItState := (
    //        //PipeMemRmwSimDutHaltItState.HALT_IT
    //        True
    //      )
    //      nextMulHaltItCnt := myInitMulHaltItCnt
    //    }
    //    outp.myExt.modMemWordValid := False
    //    rSavedModMemWord := myModMemWord
    //  }
    //}
    //def doMulHaltItFsmHaltItInnards(): Unit = {
    //  if (PipeMemRmwSimDut.doTestModOp) {
    //    outp := (
    //      RegNext(outp)
    //      init(outp.getZero)
    //    )
    //    when ((rMulHaltItCnt - 1).msb) {
    //      when (
    //        //cMid0Front.down.isFiring
    //        modFront.isFiring
    //      ) {
    //        setOutpModMemWord(rSavedModMemWord)
    //        nextHaltItState := False//PipeMemRmwSimDutHaltItState.IDLE
    //      }
    //    } otherwise {
    //      nextMulHaltItCnt := rMulHaltItCnt - 1
    //      //cMid0Front.haltIt()
    //      cMid0Front.duplicateIt()
    //      outp.myExt.modMemWordValid := False
    //    }
    //  }
    //}
    //def doTestModOpMain(
    //  doCheckHazard: Boolean=false
    //): Unit = {
    //  val myFindFirstHazardAddr = (doCheckHazard) generate (
    //    KeepAttribute(
    //      inp.myExt.memAddr.sFindFirst(
    //        _ === rPrevOutp.myExt.memAddr(PipeMemRmw.modWrIdx)
    //      )
    //      //(
    //      //  // Only check one register.
    //      //  // This will work fine for testing the different
    //      //  // categories of stalls, but the real CPU will need to
    //      //  /// be tested for *all* registers
    //      //  inp.myExt.memAddr(PipeMemRmw.modWrIdx)
    //      //  === rPrevOutp.myExt.memAddr(PipeMemRmw.modWrIdx)
    //      //)
    //      .setName(s"myFindFirstHazardAddr_${ydx}")
    //    )
    //  )
    //  def doHandleHazardWithDcacheMiss(
    //    haveCurrLoad: Boolean,
    //  ): Unit = {
    //    def handleCurrFire(
    //      someModMemWord: UInt=myModMemWord
    //    ): Unit = {
    //      outp.myExt.valid := True
    //      nextPrevTxnWasHazard := False
    //      setOutpModMemWord(
    //        someModMemWord=someModMemWord
    //      )
    //    }
    //    def handleDuplicateIt(
    //      actuallyDuplicateIt: Boolean=true
    //    ): Unit = {
    //      outp := (
    //        RegNext(outp) init(outp.getZero)
    //      )
    //      outp.myExt.valid := False
    //      outp.myExt.modMemWordValid := (
    //        False
    //      )
    //      if (actuallyDuplicateIt) {
    //        cMid0Front.duplicateIt()
    //      }
    //    }
    //    val rState = KeepAttribute(
    //      Reg(Bool())
    //      init(False)
    //    )
    //      .setName(
    //        s"doHandleHazardWithDcacheMiss"
    //        + s"_${doCheckHazard}_${haveCurrLoad}"
    //        + s"_rState"
    //        + s"${ydx}"
    //      )
    //    val rSavedModMemWord1 = (
    //      Reg(cloneOf(myModMemWord))
    //      init(myModMemWord.getZero)
    //      .setName(
    //        s"FlareCpu_doModInModFrontFunc"
    //        + s"_${doCheckHazard}_${haveCurrLoad}"
    //        + s"_rSavedModMemWord1"
    //        + s"${ydx}"
    //      )
    //    )
    //      
    //    switch (rState) {
    //      //is (False) {
    //      //  when (
    //      //    !tempModFrontPayload.dcacheHit
    //      //  ) {
    //      //    when (
    //      //      modFront.isValid
    //      //    ) {
    //      //      if (haveCurrLoad) {
    //      //        //cMid0Front.duplicateIt()
    //      //        handleDuplicateIt()
    //      //        rSavedModMemWord1 := myModMemWord
    //      //        rState := True
    //      //      } else {  // if (!haveCurrLoad)
    //      //        when (modFront.isFiring) {
    //      //          handleCurrFire()
    //      //        }
    //      //      }
    //      //    } otherwise { // when (!modFront.isFiring)
    //      //      handleDuplicateIt()
    //      //    }
    //      //  } otherwise {
    //      //    when (cMid0Front.up.isFiring) {
    //      //      handleCurrFire()
    //      //    }
    //      //  }
    //      //}
    //      //is (True) {
    //      //  when (cMid0Front.up.isFiring) {
    //      //    handleCurrFire(
    //      //      someModMemWord=rSavedModMemWord1
    //      //    )
    //      //  } otherwise {
    //      //    handleDuplicateIt(actuallyDuplicateIt=false)
    //      //  }
    //      //}
    //    }
    //  }
    //  when (cMid0Front.up.isValid) {
    //    //switch (inp.op) {
    //    //  is (PipeMemRmwSimDut.ModOp.ADD_RA_RB) {
    //    //    if (!doCheckHazard) {
    //    //      setOutpModMemWord()
    //    //    } else { // if (doCheckHazard)
    //    //      doHandleHazardWithDcacheMiss(
    //    //        haveCurrLoad=false,
    //    //      )
    //    //    }
    //    //  }
    //    //  is (PipeMemRmwSimDut.ModOp.LDR_RA_RB) {
    //    //    if (!doCheckHazard) {
    //    //      when (cMid0Front.up.isFiring) {
    //    //        setOutpModMemWord()
    //    //        nextPrevTxnWasHazard := True
    //    //      }
    //    //    } else { // if (doCheckHazard)
    //    //      nextPrevTxnWasHazard := True
    //    //      doHandleHazardWithDcacheMiss(
    //    //        haveCurrLoad=true,
    //    //      )
    //    //    }
    //    //  }
    //    //  is (PipeMemRmwSimDut.ModOp.MUL_RA_RB) {
    //    //    // we should stall `EX` in this case until the
    //    //    // calculation is done. The same stalling logic
    //    //    // will be used for `divmod`, etc.
    //    //    switch (rHaltItState) {
    //    //      is (
    //    //        False//PipeMemRmwSimDutHaltItState.IDLE
    //    //      ) {
    //    //        doMulHaltItFsmIdleInnards(
    //    //          doDuplicateIt=(
    //    //            //true
    //    //            doCheckHazard
    //    //          )
    //    //        )
    //    //      }
    //    //      is (
    //    //        //PipeMemRmwSimDutHaltItState.HALT_IT
    //    //        True
    //    //      ) {
    //    //        doMulHaltItFsmHaltItInnards()
    //    //        when (
    //    //          nextHaltItState
    //    //          //=== PipeMemRmwSimDutHaltItState.IDLE
    //    //          === False
    //    //        ) {
    //    //          nextPrevTxnWasHazard := False
    //    //        }
    //    //      }
    //    //    }
    //    //  }
    //    //}
    //  }
    //}
    //when (
    //  (
    //    //if (
    //    //  //PipeMemRmwSimDut.doAddrOneHaltIt
    //    //  PipeMemRmwSimDut.doTestModOp
    //    //) (
    //      rPrevTxnWasHazard
    //    //) else (
    //    //  False
    //    //)
    //  ) 
    //) {
    //  assert(PipeMemRmwSimDut.modRdPortCnt == 1)
    //  doTestModOpMain(
    //    doCheckHazard=true
    //  )
    //} elsewhen (
    //  cMid0Front.up.isValid
    //) {
    //  doTestModOpMain()
    //  //when (
    //  //  False
    //  //) {
    //  //  //cMid0Front.haltIt()
    //  //} elsewhen (
    //  //  //if (optModHazardKind == PipeMemRmw.modHazardKindDupl) (
    //  //  //  outp.myExt.hazardId.msb
    //  //  //) else (
    //  //    True
    //  //  //)
    //  //) {
    //  //  //if (
    //  //  //  //PipeMemRmwSimDut.doAddrOneHaltIt
    //  //  //  PipeMemRmwSimDut.doTestModOp
    //  //  //) {
    //  //    doTestModOpMain()
    //  //  //} else {
    //  //  //  setOutpModMemWord()
    //  //  //}
    //  //}
    //}
  }
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

////case class FlareCpuIcacheLineAttrs(
////  params: FlareCpuParams
////) extends Bundle {
////  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
////  val loaded = Bool()
////}
////case class FlareCpuDcacheLineAttrs(
////  params: FlareCpuParams
////) extends Bundle {
////  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
////  val loaded = Bool()
////  val dirty = Bool()
////}
////case class FlareCpuIcacheWordType(
////  params: FlareCpuParams
////) extends Bundle {
////  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
////  val data = UInt((params.icacheNumBytesPerLine * 8) bits)
////  val loaded = Bool()
////}
////case class FlareCpuDcacheWordType(
////  params: FlareCpuParams
////) extends Bundle {
////  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
////  val data = UInt((params.dcacheNumBytesPerLine * 8) bits)
////  val loaded = Bool()
////  val dirty = Bool()
////}
//
////case class FlareCpuIcacheIo(
////  params: FlareCpuParams
////) extends Bundle with IMasterSlave {
////  //--------
////  val rdAddrStm = slave Stream(
////    UInt(params.mainWidth bits)
////  )
////  val rdDataStm = master Stream(
////    UInt(params.instrMainWidth bits)
////  )
////  //--------
////  def asMaster(): Unit = {
////    master(rdAddrStm)
////    slave(rdDataStm)
////  }
////  //--------
////}
//
////case class FlareCpuIcachePayload(
////  params: FlareCpuParams,
////) extends Bundle {
////}
////
////case class FlareCpuIcacheCpuIo(
////  params: FlareCpuParams,
////) extends Bundle {
////}
////
////case class FlareCpuIcacheIo(
////  params: FlareCpuParams,
////) extends Bundle {
////  //--------
////  // AXI host
////  val ibus = master(Axi4(config=params.ibusConfig))
////  //--------
////  val cpuIo = FlareCpuIcacheCpuIo(params=params)
////  //--------
////}
////case class FlareCpuPsIcache(
////  params: FlareCpuParams,
////) extends Component {
////}
//
////case class FlareCpuDcacheIo(
////  params: FlareCpuParams,
////) extends Bundle {
////  //--------
////  // AXI host
////  val dbus = master(Axi4(config=params.dbusConfig))
////  //--------
////}
//
////--------
//case class FlareCpuPipePayload(
//  params: FlareCpuParams,
//) extends Bundle {
//  //--------
//  // IF
//  val icache = FlareCpuPipePayloadIcache(params=params)
//  //--------
//  // ID
//  val decode = FlareCpuPipePayloadDecode(params=params)
//  //--------
//  // EX
//  val exec = FlareCpuPipePayloadExec(params=params)
//  //--------
//  // dcache
//  val dcache = FlareCpuPipePayloadDcache(params=params)
//  //--------
//}
//
////case class FlareCpuIcacheIo(
////  params: FlareCpuParams,
////  //linkArr: ArrayBuffer[Link],
////) extends Area {
////}
//
//object FlareCpuParamsTest extends App {
//  val p = FlareCpuParams()
//  println(s"ibusParams.lengthWidth: ${p.ibusParams.access.lengthWidth}")
//  println(s"dbusParams.lengthWidth: ${p.dbusParams.access.lengthWidth}")
//  println("icache:")
//  println(s"icacheNumLines: ${p.icacheNumLines}")
//  println(s"icacheNumBytesPerLine: ${p.icacheNumBytesPerLine}")
//  println(s"icacheLineIdxRange: ${p.icacheLineIdxRange}")
//  println(s"icacheValidVecRange: ${p.icacheValidVecRange}")
//  println(s"icacheLineBaseAddrWidth: ${p.icacheLineBaseAddrWidth}")
//  val icacheLineDataIdxRangeArg = p.rawElemNumBytesPow32
//  val icacheLineDataIdxRange = (
//    p.icacheLineDataIdxRange(/*icacheLineDataIdxRangeArg*/)
//  )
//  println(s"icacheLineDataIdxRange: ${icacheLineDataIdxRange}")
//  println(s"icacheLineIdxRange: ${p.icacheLineIdxRange}")
//  println(s"icacheLineBaseAddrWidth: ${p.icacheLineBaseAddrWidth}")
//  println(s"icacheLineBaseAddRange: ${p.icacheLineBaseAddrRange}")
//  println("dcache:")
//  println(s"dcacheNumLines: ${p.dcacheNumLines}")
//  println(s"dcacheNumBytesPerLine: ${p.dcacheNumBytesPerLine}")
//  println(s"dcacheLineIdxRange: ${p.dcacheLineIdxRange}")
//  println(s"dcacheValidVecRange: ${p.dcacheValidVecRange}")
//  println(s"dcacheValidVecElemWidth: ${p.dcacheValidVecElemWidth}")
//  val dcacheValidVecSize = (
//    (1 << p.dcacheNumLinesPow) / p.dcacheValidVecElemWidth
//  )
//  println(s"dcacheValidVecSize: ${dcacheValidVecSize}")
//
//  println(s"dcacheLineBaseAddrWidth: ${p.dcacheLineBaseAddrWidth}")
//  val dcacheLineDataIdxRangeArg = p.rawElemNumBytesPow32
//  val dcacheLineDataIdxRange32 = (
//    p.dcacheLineDataIdxRange(dcacheLineDataIdxRangeArg)
//  )
//  println(s"dcacheLineDataIdxRange32: ${dcacheLineDataIdxRange32}")
//  println(s"dcacheLineBaseAddrWidth: ${p.dcacheLineBaseAddrWidth}")
//  println(s"dcacheLineBaseAddRange: ${p.dcacheLineBaseAddrRange}")
//}
//case class FlareCpuIo(
//  params: FlareCpuParams,
//) extends Bundle {
//  //--------
//  //--------
//  ////val bus = 
//  //// Instruction Cache Bus
//  ////val ibus = master(tilelink.Bus(FlareCpuParams.busParams))
//  ////val ibus = master(Axi4(config=params.ibusConfig))
//  //val ibus = master(Bmb(p=params.ibusParams))
//
//  //// Data Cache Bus
//  ////val dbus = master(tilelink.Bus(FlareCpuParams.busParams))
//  ////val dbus = master(Axi4(config=params.dbusConfig))
//  //val dbus = master(Bmb(p=params.dbusParams))
//
//  val irq = in Bool()
//  //--------
//}
//
//case class FlareCpu(
//  params: FlareCpuParams,
//) extends Component {
//  //--------
//  val io = FlareCpuIo(params=params)
//  //--------
//  val linkArr = PipeHelper.mkLinkArr()
//  //val pipe = PipeHelper(linkArr=linkArr)
//  //--------
//  ////val cIcache = pipe.addStage(
//  ////  name="Icache",
//  ////)
//  // These are named based upon the later pipeline stages
//  //val cIcacheDecode = pipe.addStage(
//  //  name="Decode",
//  //)
//  //val cDecodeExec = pipe.addStage(
//  //  name="Exec",
//  //)
//  //val cExecDcache = pipe.addStage(
//  //  name="Dcache",
//  //  optIncludeStage=false,
//  //  optIncludeS2M=false,
//  //)
//  //val cDcacheWrback = pipe.addStage(
//  //  name="Wrback",
//  //  optIncludeStage=false,
//  //  optIncludeS2M=false,
//  //)
//  //val cLast = pipe.addStage(
//  //  name="Last",
//  //  finish=true,
//  //)
//  //def addStageMain(
//  //  
//  //)
//  //def numStages = 5
//  //val nArr = Array.fill(numStages + 1)(Node())
//  //nArr(0).setName("nIcache")
//  //nArr(1).setName("nDecode")
//  //nArr(2).setName("nExec")
//  //nArr(3).setName("nDcache")
//  //nArr(4).setName("nWrback")
//  //nArr(5).setName("nLast")
//
//  //val sArr = new ArrayBuffer[StageLink]()
//  //val s2mArr = new ArrayBuffer[S2MLink]()
//  //val cArr = new ArrayBuffer[CtrlLink]()
//  val nIcache, nDecode, nExec, nDcacheModFront, nWrbackModFront = Node()
//
//  val sIcacheDecode = StageLink(up=nIcache, down=Node())
//  linkArr += sIcacheDecode
//  val s2mIcacheDecode = S2MLink(up=sIcacheDecode.down, down=Node())
//  linkArr += s2mIcacheDecode
//  val cIcacheDecode = CtrlLink(up=s2mIcacheDecode.down, down=nDecode)
//  linkArr += cIcacheDecode
//
//  val sDecodeExec = StageLink(up=nDecode, down=Node())
//  linkArr += sDecodeExec
//  val s2mDecodeExec = S2MLink(up=sDecodeExec.down, down=Node())
//  linkArr += s2mDecodeExec
//  val cDecodeExec = CtrlLink(up=s2mDecodeExec.down, down=nExec)
//  linkArr += cDecodeExec
//  //--------
//  // these are outputs of the pipeline stages
//  val icachePayload = Payload(FlareCpuPipePayload(params=params))
//  val decodePayload = Payload(FlareCpuPipePayload(params=params))
//  val execPayload = Payload(FlareCpuPipePayload(params=params))
//  val dcachePayload = Payload(FlareCpuPipePayload(params=params))
//  //val wrbackPayload = Payload(FlareCpuPipePayload(params=params))
//  //--------
//  //val icache = FlareCpuPsIcache(
//  //  params=params,
//  //  currPayload=icachePayload,
//  //  linkArr=linkArr,
//  //)
//  //io.ibus << icache.io.ibus
//  nIcache(icachePayload) := nIcache(icachePayload).getZero
//
//  val dcache = FlareCpuPsDcache(
//    params=params,
//    prevPayload=execPayload,
//    currPayload=dcachePayload,
//    //cPrevCurr=cExecDcache,
//    linkArr=linkArr,
//  )
//  //io.dbus << dcache.io.dbus
//
//  //val sExecDcache = StageLink(up=nExec, down=Node())
//  //linkArr += sExecDcache
//  //val s2mExecDcache = S2MLink(up=sExecDcache.down, down=Node())
//  //linkArr += s2mExecDcache
//  //val cExecDcache = CtrlLink(
//  //  up=(
//  //    //s2mExecDcache.down
//  //    nExec
//  //    //Node()
//  //    //nExec
//  //  ),
//  //  down=(
//  //    //nDcache
//  //    Node()
//  //  ),
//  //)
//  //val cExecDcache = dcache.pipeMem.mod.front.pipe.first
//  //val cExecDcache = CtrlLink(
//  //  up=cDecodeExec.down,
//  //  down=dcache.pipeMem.io.front,
//  //)
//  //linkArr += cExecDcache
//  val cExecDcache = dcache.pipeMem.mod.front.pipe.first
//  //val dDecodeExecDcache = DirectLink(
//  //  up=,
//  //  down=cExecDcache.up
//  //)
//  //linkArr += dDecodeExecDcache
//
//  //val cDcacheWrback = CtrlLink(
//  //  up=(
//  //    //Node()
//  //    //dcache.pipeMem.mod.front.pipe.last.down
//  //    dcache.pipeMem.io.modFront
//  //  ),
//  //  down=(
//  //    //nWrback
//  //    dcache.pipeMem.io.modBack
//  //  ),
//  //)
//  //linkArr += cDcacheWrback
//  val fDcacheWrback = ForkLink(
//    up=dcache.pipeMem.io.modFront,
//    downs=List(nDcacheModFront, nWrbackModFront),
//    synchronous=true,
//  )
//  linkArr += fDcacheWrback
//
//  val sWrbackModFront = StageLink(
//    up=nWrbackModFront,
//    down=Node(),
//  )
//  linkArr += sWrbackModFront
//  val s2mWrbackModFront = S2MLink(
//    up=sWrbackModFront.down,
//    down=Node(),
//  )
//  linkArr += s2mWrbackModFront
//  val cWrbackModFront = CtrlLink(
//    up=s2mWrbackModFront.down,
//    down=Node(),
//  )
//  linkArr += cWrbackModFront
//
//  val dDcacheMod = DirectLink(
//    up=nDcacheModFront,
//    down=dcache.pipeMem.io.modBack,
//  )
//  linkArr += dDcacheMod
//  //val sDcacheModFront = StageLink(
//  //  up=nDcacheModFront,
//  //  down=Node(),
//  //)
//  //linkArr += sDcacheModFront
//  //val s2mDcacheModFront = S2MLink(
//  //  up=sDcacheModFront.down,
//  //  down=(
//  //    //Node()
//  //    dcache.pipeMem.io.modBack
//  //  ),
//  //)
//  //linkArr += s2mDcacheModFront
//
//  dcache.pipeMem.io.back.ready := True
//
//  cWrbackModFront.down.ready := True
//
//  //val dWrbackEndDcache = DirectLink(
//  //  up=nWrback
//  //)
//
//  //val cIcacheDecode = CtrlLink(up=nIcache, down=nDecode)
//  //val cDecodeExec = CtrlLink(up=nDecode, down=nExec)
//  //--------
//  val decode = FlareCpuPsDecode(
//    params=params,
//    prevPayload=icachePayload,
//    currPayload=decodePayload,
//    lastMainPayload=(
//      //wrbackPayload
//      dcachePayload
//    ),
//    cPrevCurr=cIcacheDecode,
//    cCurrNext=cDecodeExec,
//    cLastMain=(
//      //cDcacheWrback
//      cWrbackModFront
//    ),
//  )
//  val exec = FlareCpuPsExec(
//    params=params,
//    prevPayload=decodePayload,
//    currPayload=execPayload,
//    cPrevCurr=cDecodeExec,
//    cCurrNext=(
//      cExecDcache
//    ),
//    //cNext2=(
//    //),
//    cPostCurrNext=(
//      //cDcacheWrback
//      cWrbackModFront
//    ),
//    decodeIo=decode.io,
//  )
//
//  val wrback = FlareCpuPsWrback(
//    params=params,
//    prevPayload=dcachePayload,
//    cPrevCurr=(
//      //cDcacheWrback
//      cWrbackModFront
//    ),
//    decodeIo=decode.io,
//  )
//
//  //--------
//  //val dExecDcache = DirectLink(
//  //  up=
//  //)
//  //val sDcachePmFront = StageLink(
//  //  up=dcache.pipeMem
//  //)
//  //val sDcachePmMod = StageLink(
//  //  up=dcache.pipeMem.io.modFront,
//  //  down=Node()
//  //)
//  //linkArr += sDcachePmMod
//  //val s2mDcachePmMod = S2MLink(
//  //  up=sDcachePmMod.down,
//  //  down=Node(),
//  //)
//  //linkArr += s2mDcachePmMod
//  //val cDcachePmMod = CtrlLink(
//  //  up=s2mDcachePmMod.down,
//  //  down=dcache.pipeMem.io.modBack,
//  //)
//  //linkArr += cDcachePmMod
//  //--------
//
//  //--------
//  Builder(linkArr.toSeq)
//  //--------
//}
//object FlareCpuVerilog extends App {
//  Config.spinal.generateVerilog(FlareCpu(params=FlareCpuParams()))
//}
//
////case class FlareCpu(
////  //clkRate: HertzNumber,
////  //optIncludeSimd: Boolean=false,
////  //optIncludeFpu: Boolean=false,
////  params: FlareCpuParams,
////) extends Component {
////  //--------
////  val io = FlareCpuIo(params=params)
////  def mainWidth = params.mainWidth
////  def instrMainWidth = params.instrMainWidth
////  def numGprsSprs = params.numGprsSprs
////  def numGprsSprsPow = params.numGprsSprsPow
////  def nonG3ImmWidth = params.nonG3ImmWidth
////  def g3ImmWidth = params.g3ImmWidth
////  def preWidth = params.preWidth
////  def preFullNonG3Width = params.preFullNonG3Width
////  def preFullG3Width = params.preFullG3Width
////  def lpreWidth = params.lpreWidth
////  def lpreFullWidth = params.lpreFullWidth
////  def instrEncGrpWidth = params.instrEncGrpWidth
////  //--------
////  //io.bus.a.address := 3
////  //--------
////  // Pipeline: IF -> ID -> EX -> WB
////  //--------
////  //case class InstrEncConst(
////  //  value: UInt,
////  //  rangeLo: Int,
////  //  //range: Range,
////  //) {
////  //  def range = value.high + rangeLo downto rangeLo
////  //}
////  object InstrEncConst {
////    //--------
////    // Instruction Group 0
////    val g0Grp = U"3'd0"
////    val g0PreSubgrp = U"1'b0"
////    val g0PreMaskedSubgrp = M"0-"
////    val g0LpreSubgrp = U"2'b10"
////    //--------
////    // Instruction Group 1
////    val g1Grp = U"3'd1"
////    //--------
////    // Instruction Group 2
////    val g2Grp = U"3'd2"
////    //--------
////    // Instruction Group 3
////    val g3Grp = U"3'd3"
////    //--------
////    // Instruction Group 4
////    val g4Grp = U"3'd4"
////    //--------
////    // Instruction Group 5
////    val g5Grp = U"3'd5"
////    //--------
////    // Instruction Group 6
////    val g6Grp = U"3'd6"
////    //--------
////    // Instruction Group 7
////    val g7Grp = U"3'd7"
////    val g7Sg00Subgrp = U"2'b00"
////    val g7Sg010Subgrp = U"3'b010"
////    val g7Sg0110Subgrp = U"4'b0110"
////    //--------
////    def gprR0Idx = U"4'd0"
////    def gprR1Idx = U"4'd1"
////    def gprR2Idx = U"4'd2"
////    def gprR3Idx = U"4'd3"
////    def gprR4Idx = U"4'd4"
////    def gprR5Idx = U"4'd5"
////    def gprR6Idx = U"4'd6"
////    def gprR7Idx = U"4'd7"
////    def gprR8Idx = U"4'd8"
////    def gprR9Idx = U"4'd9"
////    def gprR10Idx = U"4'd10"
////    def gprR11Idx = U"4'd11"
////    def gprR12Idx = U"4'd12"
////    def gprLrIdx = U"4'd13"
////    def gprFpIdx = U"4'd14"
////    def gprSpIdx = U"4'd15"
////
////    def sprFlagsIdx = U"4'd0"
////    def sprIdsIdx = U"4'd1"
////    def sprIraIdx = U"4'd2"
////    def sprIeIdx = U"4'd3"
////    def sprItyIdx = U"4'd4"
////    def sprStyIdx = U"4'd5"
////    def sprS6Idx = U"4'd6"
////    def sprS7Idx = U"4'd7"
////    def sprS8Idx = U"4'd8"
////    def sprS9Idx = U"4'd9"
////    def sprS10Idx = U"4'd10"
////    def sprS11Idx = U"4'd11"
////    def sprS12Idx = U"4'd12"
////    def sprS13Idx = U"4'd13"
////    def sprS14Idx = U"4'd14"
////    def sprS15Idx = U"4'd15"
////    //--------
////  }
////  case class InstrG0EncPre() extends Bundle {
////    val grp = UInt(InstrEncConst.g0Grp.getWidth bits)
////    val subgrp = UInt(InstrEncConst.g0PreSubgrp.getWidth bits)
////    def fullgrp = Cat(grp, subgrp)
////    val simm = UInt(preWidth bits)
////  }
////  case class InstrG0EncLpreHi() extends Bundle {
////    val grp = UInt(InstrEncConst.g0Grp.getWidth bits)
////    val subgrp = UInt(InstrEncConst.g0LpreSubgrp.getWidth bits)
////    def fullgrp = Cat(grp, subgrp)
////    //val simm = UInt(lpreWidth bits)
////    val simmHi = UInt((lpreWidth - instrMainWidth) bits)
////  }
////  object InstrG1EncOp extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      addRaS5,    // Opcode 0x0: add rA, #simm5
////      addRaPcS5,  // Opcode 0x1: add rA, pc, #simm5
////      addRaSpS5,  // Opcode 0x2: add rA, sp, #simm5
////      addRaFpS5,  // Opcode 0x3: add rA, fp, #simm5
////      cmpRaS5,    // Opcode 0x4: cmp rA, #simm5
////      cpyRaS5,    // Opcode 0x5: cpy rA, #simm5
////      lslRaI5,    // Opcode 0x6: lsl rA, #imm5
////      lsrRaI5,    // Opcode 0x7: lsr rA, #imm5
////      asrRaI5,    // Opcode 0x8: asr rA, #imm5
////      andRaS5,    // Opcode 0x9: and rA, #simm5
////      orrRaS5,    // Opcode 0xa: orr rA, #simm5
////      xorRaS5,    // Opcode 0xb: xor rA, #simm5
////      zeRaI5,     // Opcode 0xc: ze rA, #imm5
////      seRaI5,     // Opcode 0xd: se rA, #imm5
////      swiRaS5,    // Opcode 0xe: swi rA, #simm5
////      swiI5      // Opcode 0xf: swi #simm5
////      = newElement();
////  }
////  case class InstrG1Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g1Grp.getWidth bits)
////    // immediate or signed immediate, depending on the opcode
////    val imm = UInt(nonG3ImmWidth bits) 
////    val op = InstrG1EncOp()
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////  object InstrG2EncOp extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      addRaRb,      // Opcode 0x0: add rA, rB
////      subRaRb,      // Opcode 0x1: sub rA, rB
////      addRaSpRb,    // Opcode 0x2: add rA, sp, rB
////      addRaFpRb,    // Opcode 0x3: add rA, fp, rB
////      cmpRaRb,      // Opcode 0x4: cmp rA, rB
////      cpyRaRb,      // Opcode 0x5: cpy rA, rB
////      lslRaRb,      // Opcode 0x6: lsl rA, rB
////      lsrRaRb,      // Opcode 0x7: lsr rA, rB
////      asrRaRb,      // Opcode 0x8: asr rA, rB
////      andRaRb,      // Opcode 0x9: and rA, rB
////      orrRaRb,      // Opcode 0xa: orr rA, rB
////      xorRaRb,      // Opcode 0xb: xor rA, rB
////      adcRaRb,      // Opcode 0xc: adc rA, rB
////      sbcRaRb,      // Opcode 0xd: sbc rA, rB
////      cmpbcRaRb,    // Opcode 0xe: cmpbc rA, rB
////      invalid0      // Opcode 0xf: invalid operation 0
////      = newElement();
////  }
////  case class InstrG2Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g2Grp.getWidth bits)
////    val f = Bool()
////    val op = InstrG2EncOp()
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////  object InstrG3EncOp extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      blS9,       // Opcode 0x0: bl simm9
////      braS9,      // Opcode 0x1: bra simm9
////      beqS9,      // Opcode 0x2: beq simm9
////      bneS9,      // Opcode 0x3: bne simm9
////      bmiS9,      // Opcode 0x4: bmi simm9
////      bplS9,      // Opcode 0x5: bpl simm9
////      bvsS9,      // Opcode 0x6: bvs simm9
////      bvcS9,      // Opcode 0x7: bvc simm9
////      bgeuS9,     // Opcode 0x8: bgeu simm9
////      bltuS9,     // Opcode 0x9: bltu simm9
////      bgtuS9,     // Opcode 0xa: bgtu simm9
////      bleuS9,     // Opcode 0xb: bleu simm9
////      bgesS9,     // Opcode 0xc: bges simm9
////      bltsS9,     // Opcode 0xd: blts simm9
////      bgtsS9,     // Opcode 0xe: bgts simm9
////      blesS9      // Opcode 0xf: bles simm9
////      = newElement();
////  }
////  case class InstrG3Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g3Grp.getWidth bits)
////    val simm = SInt(g3ImmWidth bits)
////    val op = InstrG3EncOp()
////  }
////  object InstrG4EncOp extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      //--------
////      jlRa,         // Opcode 0x0: jl rA
////      jmpRa,        // Opcode 0x1: jmp rA
////      jmpIra,       // Opcode 0x2: jmp ira
////      reti,         // Opcode 0x3: reti
////      ei,           // Opcode 0x4: ei
////      di,           // Opcode 0x5: di
////      pushRaRb,     // Opcode 0x6: push rA, rB
////      pushSaRb,     // Opcode 0x7: push sA, rB
////      popRaRb,      // Opcode 0x8: pop rA, rB
////      popSaRb,      // Opcode 0x9: pop sA, rB
////      popPcRb,      // Opcode 0xa: pop pc, rB
////      mulRaRb,      // Opcode 0xb: mul rA, rB
////      udivRaRb,     // Opcode 0xc: udiv rA, rB
////      sdivRaRb,     // Opcode 0xd: sdiv rA, rB
////      umodRaRb,     // Opcode 0xe: umod rA, rB
////      smodRaRb,     // Opcode 0xf: smod rA, rB
////      //--------
////      lumulRaRb,    // Opcode 0x10: lumul rA, rB
////      lsmulRaRb,    // Opcode 0x11: lsmul rA, rB
////      udiv64RaRb,   // Opcode 0x12: udiv64 rA, rB
////      sdiv64RaRb,   // Opcode 0x13: sdiv64 rA, rB
////      umod64RaRb,   // Opcode 0x14: umod64 rA, rB
////      smod64RaRb,   // Opcode 0x15: smod64 rA, rB
////      ldubRaRb,     // Opcode 0x16: ldub rA, [rB]
////      ldsbRaRb,     // Opcode 0x17: ldsb rA, [rB]
////      lduhRaRb,     // Opcode 0x18: lduh rA, [rB]
////      ldshRaRb,     // Opcode 0x19: ldsh rA, [rB]
////      stbRaRb,      // Opcode 0x1a: stb rA, [rB]
////      sthRaRb,      // Opcode 0x1b: sth rA, [rB]
////      cpyRaSb,      // Opcode 0x1c: cpy rA, sB
////      cpySaRb,      // Opcode 0x1d: cpy sA, rB
////      cpySaSb,      // Opcode 0x1e: cpy sA, sB
////      indexRa       // Opcode 0x1f: index rA
////      //--------
////      = newElement();
////  }
////  case class InstrG4Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g4Grp.getWidth bits)
////    val op = InstrG4EncOp()
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////  case class InstrG5Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g5Grp.getWidth bits)
////    val simm = SInt(nonG3ImmWidth bits)
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////  case class InstrG6Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g6Grp.getWidth bits)
////    val simm = SInt(nonG3ImmWidth bits)
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////
////  // this includes the `w` bit (since it's contiguous with the opcode field)
////  object InstrG7Sg00FullOpEnc extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      cmpbRaRb,     // Opcode 0b000: cmpb rA, rB
////      lsrbRaRb,     // Opcode 0b001: lsrb rA, rB
////      asrbRaRb,     // Opcode 0b010: asrb rA, rB
////      invalid0,     // Opcode 0b011: invalid 0
////      cmphRaRb,     // Opcode 0b100: cmph rA, rB
////      lsrhRaRb,     // Opcode 0b101: lsrh rA, rB
////      asrhRaRb,     // Opcode 0b110: asrh rA, rB
////      invalid1      // Opcode 0b111: invalid 1
////      = newElement();
////  }
////  case class InstrG7Sg00Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
////    val subgrp = UInt(InstrEncConst.g7Sg00Subgrp.getWidth bits)
////    def fullgrp = Cat(grp, subgrp)
////    val fullop = InstrG7Sg00FullOpEnc()
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////  object InstrG7Sg010EncOp extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      ldrSaRb,      // Opcode 0x0: ldr sA, [rB]
////      ldrSaSb,      // Opcode 0x1: ldr sA, [sB]
////      strSaRb,      // Opcode 0x2: str sA, [rB]
////      strSaSb       // Opcode 0x3: str sA, [sB]
////      = newElement();
////  }
////  case class InstrG7Sg010Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
////    val subgrp = UInt(InstrEncConst.g7Sg010Subgrp.getWidth bits)
////    def fullgrp = Cat(grp, subgrp)
////    val op = InstrG7Sg010EncOp()
////    val rbIdx = UInt(numGprsSprsPow bits)
////    val raIdx = UInt(numGprsSprsPow bits)
////  }
////
////  case class InstrG7Sg0110Enc() extends Bundle {
////    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
////    val subgrp = UInt(InstrEncConst.g7Sg0110Subgrp.getWidth bits)
////    def fullgrp = Cat(grp, subgrp)
////  }
////
////  object InstrFullgrpDec extends SpinalEnum(
////    defaultEncoding=binarySequential
////  ) {
////    val
////      g0Pre,
////      g0Lpre,
////      g1,
////      g2,
////      g3,
////      g4,
////      g5,
////      g6,
////      g7Sg00,
////      g7Sg010,
////      g7Sg0110,
////      invalid
////      = newElement();
////  }
////
////  val psExOutp = Payload(PsExOutp())
////
////  case class InstrDecEtc() extends Bundle {
////    //--------
////    // This `Bundle` also includes register values read from the two
////    // register files
////    //--------
////    //val isNop = Bool()
////    val isInvalid = Bool()
////    val haveFullInstr = Bool()
////    val fullgrp = InstrFullgrpDec()
////    val fullSimm = UInt(mainWidth bits)
////    val fullImm = UInt(mainWidth bits)
////    val fullPcrelSimm = UInt(mainWidth bits)
////
////    val enumGprRa = 0 
////    val enumGprRb = 1
////    val enumGprRc = 2
////    val enumGprLr = 3
////    val enumGprFp = 4
////    val enumGprSp = 5
////    val enumSprSa = 6
////    val enumSprSb = 7
////    val enumSprFlags = 8
////    val enumSprIds = 9
////    val enumSprIra = 10
////    val enumSprIe = 11
////    val enumSprIty = 12
////    val enumSprSty = 13
////    val enumGprRa64Hi = 14
////    val enumGprRa64Lo = 15
////    val enumGprRb64Hi = 16
////    val enumGprRb64Lo = 17
////
////    val ra = UInt(mainWidth bits)         // `rA`
////    val rb = UInt(mainWidth bits)         // `rB`
////    val rc = UInt(mainWidth bits)         // `rC`
////    val gprLr = UInt(mainWidth bits)      // `lr`
////    val gprFp = UInt(mainWidth bits)      // `fp`
////    val gprSp = UInt(mainWidth bits)      // `sp`
////    //val rd = UInt(mainWidth bits)         // `rD`
////    val sa = UInt(mainWidth bits)         // `sA`
////    val sb = UInt(mainWidth bits)         // `sB`
////    val sprFlags = UInt(mainWidth bits)   // `flags`
////    val sprIds = UInt(mainWidth bits)     // `ids`
////    val sprIra = UInt(mainWidth bits)     // `ira`
////    val sprIe = UInt(mainWidth bits)      // `ie`
////    val sprIty = UInt(mainWidth bits)     // `ity`
////    val sprSty = UInt(mainWidth bits)     // `sty`
////
////
////    val raIdx = UInt(numGprsSprsPow bits) //
////    val rbIdx = UInt(numGprsSprsPow bits) //
////    val rcIdx = UInt(numGprsSprsPow bits) // 
////    //val rdIdx = UInt(numGprsSprsPow bits) // 
////
////    //val saIdx = UInt(numGprsSprsPow bits) //
////    //val sbIdx = UInt(numGprsSprsPow bits) //
////
////
////    val ra64Hi = UInt(mainWidth bits)     //
////    val ra64Lo = UInt(mainWidth bits)     //
////    val rb64Hi = UInt(mainWidth bits)     //
////    val rb64Lo = UInt(mainWidth bits)     //
////
////    //val ra64HiIdx = UInt(numGprsSprsPow bits)
////    //val ra64LoIdx = UInt(numGprsSprsPow bits)
////    //val rb64HiIdx = UInt(numGprsSprsPow bits)
////    //val rb64LoIdx = UInt(numGprsSprsPow bits)
////
////    def ra64HiIdx = Cat(
////      raIdx(raIdx.high downto 1),
////      False,
////    ).asUInt
////    def ra64LoIdx = Cat(
////      raIdx(raIdx.high downto 1),
////      True,
////    ).asUInt
////    def rb64HiIdx = Cat(
////      rbIdx(rbIdx.high downto 1),
////      False,
////    ).asUInt
////    def rb64LoIdx = Cat(
////      rbIdx(rbIdx.high downto 1),
////      True,
////    ).asUInt
////
////    //def doSet64Idxs(): Unit = {
////    //  ra64HiIdx := Cat(
////    //    raIdx(raIdx.high downto 1),
////    //    False,
////    //  ).asUInt
////    //  ra64LoIdx := Cat(
////    //    raIdx(raIdx.high downto 1),
////    //    True,
////    //  ).asUInt
////    //  rb64HiIdx := Cat(
////    //    rbIdx(rbIdx.high downto 1),
////    //    False,
////    //  ).asUInt
////    //  rb64LoIdx := Cat(
////    //    rbIdx(rbIdx.high downto 1),
////    //    True,
////    //  ).asUInt
////    //}
////
////    def doFwdAllRegs(
////      someCtrlLink: CtrlLink,
////      fwdRc: Boolean,
////    )(
////      getNonFwdRegFunc: (
////        UInt,       // `decIdx`
////        Boolean,    // `isGpr`
////        Int,        // `whichReg`
////      ) => UInt,
////    ): Unit = {
////      //--------
////      def doFwdOneReg(
////        decIdx: UInt,
////        //nonFwdReg: UInt,
////        isGpr: Boolean,
////        whichReg: Int,
////        //someCtrlLink: CtrlLink,
////      ) = {
////        def tempExOutp = someCtrlLink(psExOutp).get(isGpr)
////        def nonFwdReg = getNonFwdRegFunc(
////          decIdx,
////          isGpr,
////          whichReg,
////        )
////        //def tempRegWb = cExWb(psExOutp).get(isGpr)
////        Mux[UInt](
////          !(
////            decIdx === tempExOutp.regIdx
////            && tempExOutp.wrReg.valid
////          ),
////          nonFwdReg,
////          tempExOutp.wrReg.payload,
////        )
////      }
////      //--------
////      if (!fwdRc) {
////        ra := doFwdOneReg(
////          decIdx=raIdx,
////          isGpr=true,
////          whichReg=enumGprRa,
////        )
////        rb := doFwdOneReg(
////          decIdx=rbIdx,
////          isGpr=true,
////          whichReg=enumGprRb,
////        )
////        gprLr := doFwdOneReg(
////          decIdx=InstrEncConst.gprLrIdx,
////          isGpr=true,
////          whichReg=enumGprLr,
////        )
////        gprFp := doFwdOneReg(
////          decIdx=InstrEncConst.gprFpIdx,
////          isGpr=true,
////          whichReg=enumGprFp,
////        )
////        gprSp := doFwdOneReg(
////          decIdx=InstrEncConst.gprSpIdx,
////          isGpr=true,
////          whichReg=enumGprSp,
////        )
////        sa := doFwdOneReg(
////          decIdx=raIdx,
////          isGpr=false,
////          whichReg=enumSprSa,
////        )
////        sb := doFwdOneReg(
////          decIdx=rbIdx,
////          isGpr=false,
////          whichReg=enumSprSa,
////        )
////        sprFlags := doFwdOneReg(
////          decIdx=InstrEncConst.sprFlagsIdx,
////          isGpr=false,
////          whichReg=enumSprFlags,
////        )
////        sprIds := doFwdOneReg(
////          decIdx=InstrEncConst.sprIdsIdx,
////          isGpr=false,
////          whichReg=enumSprIds,
////        )
////        sprIra := doFwdOneReg(
////          decIdx=InstrEncConst.sprIraIdx,
////          isGpr=false,
////          whichReg=enumSprIra,
////        )
////        sprIe := doFwdOneReg(
////          decIdx=InstrEncConst.sprIeIdx,
////          isGpr=false,
////          whichReg=enumSprIe,
////        )
////        sprIty := doFwdOneReg(
////          decIdx=InstrEncConst.sprItyIdx,
////          isGpr=false,
////          whichReg=enumSprIty,
////        )
////        sprSty := doFwdOneReg(
////          decIdx=InstrEncConst.sprStyIdx,
////          isGpr=false,
////          whichReg=enumSprSty,
////        )
////
////        ra64Hi := doFwdOneReg(
////          decIdx=ra64HiIdx,
////          isGpr=true,
////          whichReg=enumGprRa64Hi
////        )
////        ra64Lo := doFwdOneReg(
////          decIdx=ra64LoIdx,
////          isGpr=true,
////          whichReg=enumGprRa64Lo,
////        )
////        rb64Hi := doFwdOneReg(
////          decIdx=rb64HiIdx,
////          isGpr=true,
////          whichReg=enumGprRb64Hi,
////        )
////        rb64Lo := doFwdOneReg(
////          decIdx=rb64LoIdx,
////          isGpr=true,
////          whichReg=enumGprRb64Lo,
////        )
////      } else { // if (fwdRc)
////        rc := doFwdOneReg(
////          decIdx=rcIdx,
////          isGpr=true,
////          whichReg=enumGprRc,
////        )
////      }
////      //--------
////    }
////    //--------
////    case class InstrEnc() extends Bundle {
////      val g0Pre = InstrG0EncPre()
////      val g0LpreHi = InstrG0EncLpreHi()
////      val g1 = InstrG1Enc()
////      val g2 = InstrG2Enc()
////      val g3 = InstrG3Enc()
////      val g4 = InstrG4Enc()
////      val g5 = InstrG5Enc()
////      val g6 = InstrG6Enc()
////      val g7Sg00 = InstrG7Sg00Enc()
////      val g7Sg010 = InstrG7Sg010Enc()
////      val g7Sg0110 = InstrG7Sg0110Enc()
////    }
////    val instrEnc = InstrEnc()
////    //--------
////  }
////  //object InstrG7Sg0110Op extends SpinalEnum(
////  //  defaultEncoding=binarySequential
////  //) {
////  //}
////  //case class PsIfPayload() extends Bundle {
////  //  val irq = Bool()
////  //  val pc = UInt(mainWidth bits)
////  //  val pcPlus2 = UInt(mainWidth bits)
////  //  val psExSetPc = Flow(UInt(mainWidth bits))
////  //}
////
////  val irq = Payload(Bool())
////  val pc = Payload(UInt(mainWidth bits))
////  val pcPlus2 = Payload(UInt(mainWidth bits))
////  val psExSetPc = Reg(Flow(UInt(mainWidth bits)))
////  val fetchInstr = Payload(UInt(instrMainWidth bits))
////  psExSetPc.init(psExSetPc.getZero)
////
////  object LpreState extends SpinalEnum(defaultEncoding=binarySequential) {
////    val
////      //noLpre,
////      haveHi,
////      haveLo
////      = newElement();
////  }
////  case class PrefixInfo(
////    dataWidth: Int,
////    isLpre: Boolean, 
////  ) extends Bundle {
////    val have = Bool()
////    val lpreState = (isLpre) generate LpreState()
////    val data = UInt(dataWidth bits)
////  }
////
////  case class AllPrefixInfo() extends Bundle {
////    val pre = PrefixInfo(dataWidth=preWidth, isLpre=false)
////    val lpre = PrefixInfo(dataWidth=lpreWidth, isLpre=true)
////    val index = PrefixInfo(dataWidth=numGprsSprsPow, isLpre=false)
////    val haveAnyPrefix = Bool()
////    val havePreOrLpre = Bool()
////    val haveLpreHiOnly = Bool()
////    val haveFullLpre = Bool()
////  }
////  val allPrefixInfo = Payload(AllPrefixInfo())
////
////  val instrDecEtc = Payload(InstrDecEtc())
////  //case class PsIdPayload() extends Bundle {
////  //  val preInfo = PrefixInfo(dataWidth=preWidth, isLpre=false)
////  //  val lpreInfo = PrefixInfo(dataWidth=lpreWidth, isLpre=true)
////  //  val indexInfo = PrefixInfo(dataWidth=numGprsPow, isLpre=false)
////  //}
////
////  def flagIdxZ = 0
////  def flagIdxC = 1
////  def flagIdxV = 2
////  def flagIdxN = 3
////  //val rRegFlags = Reg(UInt(mainWidth bits)) init(0x0)
////  val rGprVec = Vec.fill(numGprsSprs)(
////    Reg(UInt(mainWidth bits)) init(0x0)
////  )
////  val rSprVec = Vec.fill(numGprsSprs)(
////    Reg(UInt(mainWidth bits)) init(0x0)
////  )
////  case class PsExOutp() extends Bundle {
////    //--------
////    // when multiple register need to be written to by one instruction
////    // (lumul, sdiv64, etc.), serialize sending new register values to WB
////    //val gprIdx = UInt(numGprsSprsPow bits)
////    //val wrGpr = Flow(UInt(mainWidth bits))
////    ////--------
////    //val sprIdx = UInt(numGprsSprsPow bits)
////    //val wrSpr = Flow(UInt(mainWidth bits))
////    //--------
////    case class WrReg() extends Bundle {
////      val regIdx = UInt(numGprsSprsPow bits)
////      val wrReg = Flow(UInt(mainWidth bits))
////    }
////    val gpr = WrReg()
////    val spr = WrReg()
////    //--------
////    def get(
////      isGpr: Boolean
////    ) = (
////      if (isGpr) {gpr} else {spr}
////    )
////    //--------
////  }
////
////  //case class PsExPayload() extends Bundle {
////  //  def flagIdxZ = 0
////  //  def flagIdxC = 1
////  //  def flagIdxV = 2
////  //  def flagIdxN = 3
////  //  val regFlags = UInt(mainWidth bits)
////  //  val gprVec = Vec.fill(numGprs)(UInt(mainWidth bits))
////  //}
////  //case class PsMemWbPayload() extends Bundle {
////  //}
////
////  val nIf, nId, nEx, nWb = Node()
////  val sArr = new ArrayBuffer[StageLink]()
////  val s2mArr = new ArrayBuffer[S2MLink]()
////  val cArr = new ArrayBuffer[CtrlLink]()
////  val linkArr = new ArrayBuffer[Link]()
////
////  // IF -> ID
////  sArr += StageLink(
////    up=nIf,
////    down=Node(),
////  )
////  linkArr += sArr.last
////  s2mArr += S2MLink(
////    up=sArr.last.down,
////    down=Node(),
////  )
////  linkArr += s2mArr.last
////  cArr += CtrlLink(
////    up=s2mArr.last.down,
////    down=nId,
////  )
////  linkArr += cArr.last
////  val cIfId = cArr.last
////
////  // ID -> EX
////  sArr += StageLink(
////    up=cIfId.down,
////    down=Node(),
////  )
////  linkArr += sArr.last
////  s2mArr += S2MLink(
////    up=sArr.last.down,
////    down=Node(),
////  )
////  linkArr += s2mArr.last
////  cArr += CtrlLink(
////    up=s2mArr.last.down,
////    down=nEx,
////  )
////  linkArr += cArr.last
////  val cIdEx = cArr.last
////
////  // EX -> WB
////  sArr += StageLink(
////    up=cIdEx.down,
////    down=Node(),
////  )
////  linkArr += sArr.last
////  s2mArr += S2MLink(
////    up=sArr.last.down,
////    down=Node(),
////  )
////  linkArr += s2mArr.last
////  cArr += CtrlLink(
////    up=s2mArr.last.down,
////    down=nWb,
////  )
////  linkArr += cArr.last
////  val cExWb = cArr.last
////
////
////  val icache = new Area {
////    val attrsMem = Mem(
////      wordType=FlareCpuIcacheLineAttrs(params=params),
////      wordCount=params.icacheNumLines,
////    )
////      .addAttribute("ramstyle", params.icacheRamStyle)
////      .addAttribute("ram_style", params.icacheRamStyle)
////      .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
////    val lineMem = Mem(
////      wordType=params.icacheLineMemWordType(),
////      wordCount=params.icacheLineMemWordCount,
////    )
////      .addAttribute("ramstyle", params.icacheRamStyle)
////      .addAttribute("ram_style", params.icacheRamStyle)
////      .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
////    def sliceData(
////      data: UInt,
////      //elemNumBytesPow: Int=log2Up(instrMainWidth / 8),
////      rawElemNumBytesPow: (Int, Int)
////        =params.icacheParams.rawElemNumBytesPow16,
////    ) = (
////      data(params.icacheLineDataIdxRange(
////        rawElemNumBytesPow=rawElemNumBytesPow
////      ))
////    )
////    //def rawReadSyncU16(
////    //  addr: UInt,
////    //  enable: Bool,
////    //): UInt = {
////    //  mem.readSync(
////    //    address=addr(params.icacheLineIdxRange),
////    //    enable=enable,
////    //  ).data(params.icacheLineDataIdxRange())
////    //}
////    ////def flush(): Unit = {
////    ////}
////    //def rawWrite(
////    //  addr: UInt,
////    //  enable: Bool,
////    //  data: UInt,
////    //): Unit = {
////    //  //mem.write(
////    //  //  address=addr,
////    //  //  enable=enable,
////    //  //  data=data,
////    //  //)
////    //}
////  }
////
////  val dcache = new Area {
////    val attrsMem = Mem(
////      wordType=FlareCpuDcacheLineAttrs(params=params),
////      //wordType=UInt(instrMainWidth bits),
////      wordCount=params.dcacheNumLines,
////    )
////      .addAttribute("ramstyle", params.dcacheRamStyle)
////      .addAttribute("ram_style", params.dcacheRamStyle)
////      .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
////    val lineMem = Mem(
////      //wordType=UInt((params.dcacheNumBytesPerLine * 8) bits),
////      //wordCount=params.dcacheNumLines,
////      wordType=params.dcacheLineMemWordType(),
////      wordCount=params.dcacheLineMemWordCount,
////    )
////      .addAttribute("ramstyle", params.dcacheRamStyle)
////      .addAttribute("ram_style", params.dcacheRamStyle)
////      .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
////    //def read(): Unit = {
////    //}
////    def sliceData(
////      data: UInt,
////      //elemNumBytesPow: Int,
////      rawElemNumBytesPow: (Int, Int)
////    ) = {
////      data(params.dcacheLineDataIdxRange(
////        rawElemNumBytesPow=rawElemNumBytesPow
////      ))
////    }
////  }
////
////  // Pipeline Stage: Instruction Fetch
////  //when (!nIf(psExSetPc).valid) {
////  //} otherwise {
////  //}
////  //--------
////  //def doFwdReg(
////  //  decIdx: UInt,
////  //  nonFwdReg: UInt,
////  //  isGpr: Boolean,
////  //  someCtrlLink: CtrlLink,
////  //) = {
////  //  def tempExOutp = someCtrlLink(psExOutp).get(isGpr)
////  //  //def tempRegWb = cExWb(psExOutp).get(isGpr)
////  //  Mux[UInt](
////  //    !(
////  //      decIdx === tempExOutp.regIdx
////  //      && tempExOutp.wrReg.valid
////  //    ),
////  //    nonFwdReg,
////  //    tempExOutp.wrReg.payload,
////  //  )
////  //}
////  //--------
////  //def doFwdGpr64Half(
////  //  decIdx: UInt,
////  //  nonFwdGpr64Half: UInt,
////  //  isHi: Boolean,
////  //  someCtrlLink: CtrlLink,
////  //) = {
////  //  doFwdReg(
////  //    decIdx=Cat(decIdx(decIdx.high downto 1), !Bool(isHi)).asUInt,
////  //    nonFwdReg=nonFwdGpr64Half,
////  //    isGpr=true,
////  //    someCtrlLink=someCtrlLink,
////  //  )
////  //}
////
////  // Pipeline Stage: Instruction Decode
////  //def myThrowWhenCond = (
////  //  cIfId.down(instrDecEtc).isInvalid
////  //  //|| cIdEx.down(instrDecEtc).isInvalid
////  //  //|| cExWb.down(instrDecEtc).isInvalid
////  //)
////  //cIfId.throwWhen(myThrowWhenCond)
////  //cIdEx.throwWhen(myThrowWhenCond)
////  //cExWb.throwWhen(myThrowWhenCond)
////  nIf.valid := True
////  nWb.ready := True
////
////  //when (
////  //  //cIfId.down.isFiring
////  //  cIfId.down.isFiring
////  //)
////  {
////    def myInstrDecEtc = cIfId.down(instrDecEtc)
////    def myFetchInstr = cIfId.down(fetchInstr).asBits
////    //switch (
////    //  Cat(
////    //    preInfo.have,
////    //    lpreInfo.lpreState,
////    //    indexInfo.have
////    //  )
////    //) {
////    //}
////
////    def downAllPrefixInfo = cIfId(allPrefixInfo)
////    def downPreInfo = downAllPrefixInfo.pre
////    def downLpreInfo = downAllPrefixInfo.lpre
////    def downIndexInfo = downAllPrefixInfo.index
////    def downHaveAnyPrefix = downAllPrefixInfo.haveAnyPrefix
////    def downHavePreOrLpre = downAllPrefixInfo.havePreOrLpre
////    def downHaveLpreHiOnly = downAllPrefixInfo.haveLpreHiOnly
////    def downHaveFullLpre = downAllPrefixInfo.haveFullLpre
////
////    //val tempAllPrefixInfo = cloneOf(downAllPrefixInfo)
////    //def tempPreInfo = tempAllPrefixInfo.pre
////    //def tempLpreInfo = tempAllPrefixInfo.lpre
////    //def tempIndexInfo = tempAllPrefixInfo.index
////    //def tempHaveAnyPrefix = tempAllPrefixInfo.haveAnyPrefix
////    //def tempHavePreOrLpre = tempAllPrefixInfo.havePreOrLpre
////    //def tempHaveLpreHiOnly = tempAllPrefixInfo.haveLpreHiOnly
////    //def tempHaveFullLpre = tempAllPrefixInfo.haveFullLpre
////
////    myInstrDecEtc := (
////      RegNext(myInstrDecEtc) init(myInstrDecEtc.getZero)
////    )
////    downAllPrefixInfo := (
////      RegNext(downAllPrefixInfo) init(downAllPrefixInfo.getZero)
////    )
////    //tempAllPrefixInfo := (
////    //  RegNext(tempAllPrefixInfo) init(tempAllPrefixInfo.getZero)
////    //)
////
////    when (
////      //!downLpreInfo.have
////      //|| (
////      //  downLpreInfo.lpreState === LpreState.haveLo
////      //)
////      //!downHaveFullLpre
////      //downLpreInfo.lpreState =/= LpreState.haveHi
////      //&& downLpreInfo.lpreState === LpreState.haveLo
////      //&& downLpreInfo.lpreState === LpreState.have
////      !downHaveLpreHiOnly
////    ) {
////      //val tempInstrDecEtc = cloneOf(downInstrDecEtc)
////      //tempInstrDecEtc := (
////      //  RegNext(tempInstrDecEtc) init(tempInstrDecEtc.getZero)
////      //)
////      //tempInstrDecEtc.allowOverride
////
////      def downInstrEnc = myInstrDecEtc.instrEnc
////      downInstrEnc.g0Pre.assignFromBits(myFetchInstr)
////      downInstrEnc.g0LpreHi.assignFromBits(myFetchInstr)
////      downInstrEnc.g1.assignFromBits(myFetchInstr)
////      downInstrEnc.g2.assignFromBits(myFetchInstr)
////      downInstrEnc.g3.assignFromBits(myFetchInstr)
////      downInstrEnc.g4.assignFromBits(myFetchInstr)
////      downInstrEnc.g5.assignFromBits(myFetchInstr)
////      downInstrEnc.g6.assignFromBits(myFetchInstr)
////      downInstrEnc.g7Sg00.assignFromBits(myFetchInstr)
////      downInstrEnc.g7Sg010.assignFromBits(myFetchInstr)
////      downInstrEnc.g7Sg0110.assignFromBits(myFetchInstr)
////
////      myInstrDecEtc.raIdx := downInstrEnc.g2.raIdx
////      myInstrDecEtc.rbIdx := downInstrEnc.g2.rbIdx
////      //tempInstrDecEtc.saIdx := tempInstrEnc.g2.raIdx
////      //tempInstrDecEtc.sbIdx := tempInstrEnc.g2.rbIdx
////      //when (!downIndexInfo.have) {
////      //  tempInstrDecEtc.rcIdx := 0x0
////      //}
////      //tempInstrDecEtc.rdIdx := 0x0
////      def doClearPrefixes(): Unit = {
////        //tempPreInfo := tempPreInfo.getZero
////        //tempLpreInfo := tempLpreInfo.getZero
////        //tempHavePreOrLpre := tempHavePreOrLpre.getZero
////        //tempIndexInfo := tempIndexInfo.getZero
////        //tempAllPrefixInfo := tempAllPrefixInfo.getZero
////        downAllPrefixInfo := downAllPrefixInfo.getZero
////      }
////      def doInvalidInstr(): Unit = {
////        doClearPrefixes()
////        myInstrDecEtc.isInvalid := True
////        //tempInstrDecEtc.fullgrp := InstrFullgrpDec.invalid
////      }
////      def doFinishedInstr(): Unit = {
////        doClearPrefixes()
////        myInstrDecEtc.isInvalid := False
////      }
////      //def doFwdWbReg(
////      //  decIdx: UInt,
////      //  //someReg: UInt,
////      //  isGpr: Boolean,
////      //) = {
////      //  doFwdReg(
////      //    decIdx=decIdx,
////      //    nonFwdReg=(
////      //      if (isGpr) {
////      //        rGprVec(decIdx)
////      //      } else {
////      //        rSprVec(decIdx)
////      //      }
////      //    ),
////      //    isGpr=isGpr,
////      //    someCtrlLink=cExWb,
////      //  )
////      //}
////      def getNonFwdRegFunc(
////        decIdx: UInt,
////        isGpr: Boolean,
////        whichReg: Int,
////      ): UInt = {
////        if (isGpr) {
////          rGprVec(decIdx)
////        } else {
////          rSprVec(decIdx)
////        }
////      }
////      myInstrDecEtc.doFwdAllRegs(
////        someCtrlLink=cExWb,
////        fwdRc=false,
////      )(
////        getNonFwdRegFunc=getNonFwdRegFunc
////      )
////      //--------
////      //downInstrDecEtc.ra := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.raIdx,
////      //  isGpr=true,
////      //)
////      //downInstrDecEtc.rb := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.rbIdx,
////      //  isGpr=true,
////      //)
////      //downInstrDecEtc.sa := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.raIdx,
////      //  isGpr=false,
////      //)
////      //downInstrDecEtc.sb := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.rbIdx,
////      //  isGpr=false,
////      //)
////      ////--------
////      //downInstrDecEtc.ra64HiIdx := Cat(
////      //  downInstrDecEtc.raIdx(downInstrDecEtc.raIdx.high downto 1),
////      //  False,
////      //).asUInt
////      //downInstrDecEtc.ra64LoIdx := Cat(
////      //  downInstrDecEtc.raIdx(downInstrDecEtc.raIdx.high downto 1),
////      //  True,
////      //).asUInt
////      //downInstrDecEtc.rb64HiIdx := Cat(
////      //  downInstrDecEtc.rbIdx(downInstrDecEtc.rbIdx.high downto 1),
////      //  False,
////      //).asUInt
////      //downInstrDecEtc.rb64LoIdx := Cat(
////      //  downInstrDecEtc.rbIdx(downInstrDecEtc.rbIdx.high downto 1),
////      //  True,
////      //).asUInt
////
////      //downInstrDecEtc.ra64Hi := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.ra64HiIdx,
////      //  isGpr=true,
////      //)
////      //downInstrDecEtc.ra64Lo := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.ra64LoIdx,
////      //  isGpr=true,
////      //)
////      //downInstrDecEtc.rb64Hi := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.rb64HiIdx,
////      //  isGpr=true,
////      //)
////      //downInstrDecEtc.rb64Lo := doFwdWbReg(
////      //  decIdx=downInstrDecEtc.rb64LoIdx,
////      //  isGpr=true,
////      //)
////      //--------
////      //switch (
////      //  Cat(
////      //    downPreInfo.have,
////      //    downLpreInfo.lpreState,
////      //    downIndexInfo.have
////      //  )
////      //) {
////      //}
////
////      //def doFwdReg(
////      //  decIdx: UInt,
////      //  someReg: UInt,
////      //  isGpr: Boolean,
////      //) = {
////      //  def tempRegWb = cExWb(psExOutp).get(isGpr)
////      //  Mux[UInt](
////      //    !(
////      //      decIdx === tempRegWb.regIdx
////      //      && tempRegWb.wrReg.valid
////      //    ),
////      //    someReg,
////      //    tempRegWb.wrReg.payload
////      //  )
////      //}
////      //tempInstrEnc.ra := 
////      //downInstrDecEtc.isNop := False
////      switch (downInstrEnc.g0Pre.grp) {
////        is (InstrEncConst.g0Grp) {
////          when (!downHavePreOrLpre) {
////            switch (downInstrEnc.g0LpreHi.subgrp) {
////              is (InstrEncConst.g0PreMaskedSubgrp) {
////                myInstrDecEtc.fullgrp := InstrFullgrpDec.g0Pre
////                downPreInfo.have := True
////                downHavePreOrLpre := True
////              }
////              is (InstrEncConst.g0LpreSubgrp) {
////                myInstrDecEtc.fullgrp := InstrFullgrpDec.g0Lpre
////                downLpreInfo.have := True
////                downLpreInfo.lpreState := LpreState.haveHi
////                downHavePreOrLpre := True
////              }
////              default {
////                doInvalidInstr()
////              }
////            }
////          } otherwise {
////            doInvalidInstr()
////          }
////        }
////        is (InstrEncConst.g1Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g1
////          doFinishedInstr()
////        }
////        is (InstrEncConst.g2Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g2
////          doFinishedInstr()
////        }
////        is (InstrEncConst.g3Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g3
////          doFinishedInstr()
////        }
////        is (InstrEncConst.g4Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g4
////          when (
////            downInstrEnc.g4.op =/= InstrG4EncOp.indexRa
////          ) {
////            doFinishedInstr()
////          } otherwise {
////            downIndexInfo.have := True
////            myInstrDecEtc.rcIdx := downInstrEnc.g4.raIdx
////            //downInstrDecEtc.rc := doFwdWbReg(
////            //  decIdx=downInstrDecEtc.rcIdx,
////            //  isGpr=true,
////            //)
////            myInstrDecEtc.doFwdAllRegs(
////              someCtrlLink=cExWb,
////              fwdRc=true,
////            )(
////              getNonFwdRegFunc=getNonFwdRegFunc
////            )
////          }
////        }
////        is (InstrEncConst.g5Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g5
////          doFinishedInstr()
////        }
////        is (InstrEncConst.g6Grp) {
////          myInstrDecEtc.fullgrp := InstrFullgrpDec.g6
////          doFinishedInstr()
////        }
////        is (InstrEncConst.g7Grp) {
////          doFinishedInstr()
////          when (
////            downInstrEnc.g7Sg00.subgrp
////            === InstrEncConst.g7Sg00Subgrp
////          ) {
////            myInstrDecEtc.fullgrp := InstrFullgrpDec.g7Sg00
////            doFinishedInstr()
////          } elsewhen (
////            downInstrEnc.g7Sg010.subgrp
////            === InstrEncConst.g7Sg010Subgrp
////          ) {
////            myInstrDecEtc.fullgrp := InstrFullgrpDec.g7Sg010
////            doFinishedInstr()
////          } elsewhen (
////            downInstrEnc.g7Sg0110.subgrp
////            === InstrEncConst.g7Sg0110Subgrp
////          ) {
////            myInstrDecEtc.fullgrp := InstrFullgrpDec.g7Sg0110
////            doFinishedInstr()
////          } otherwise {
////            // invalid instruction, NOP
////            //downInstrDecEtc.isNop := True
////            //cIfId.haltIt()
////            //downInstrDecEtc.isInvalid := True
////            doInvalidInstr()
////          }
////        }
////      }
////      myInstrDecEtc := myInstrDecEtc
////    } otherwise {
////      downHaveFullLpre := True
////      downLpreInfo.have := True
////      downLpreInfo.lpreState := LpreState.haveLo
////      downLpreInfo.data(instrMainWidth - 1 downto 0).assignFromBits(
////        myFetchInstr
////      )
////      cIfId.throwIt()
////      //cIfId.terminateIt() // clear `cIfId.down.valid`
////    }
////  }
////
////  // Pipeline Stage: EXecute:
////  when (
////    //cIdEx.down.isFiring
////    //&&
////    //cIdEx.down(lpreInfo).lpreState =/= LpreState.haveHi
////    //&& 
////    !psExSetPc.valid
////  ) {
////    def myInstrDecEtc = cIdEx.down(instrDecEtc)
////    switch (myInstrDecEtc.fullgrp) {
////      def myPc = cIdEx.down(pc)
////      def myPcPlus2 = cIdEx.down(pcPlus2)
////      def simm = myInstrDecEtc.fullSimm
////      def imm = myInstrDecEtc.fullImm
////      //def wrGpr = cIdEx(psExOutp).get(isGpr=true)
////      //def wrSpr = cIdEx(psExOutp).get(isGpr=false)
////      def doWriteReg(
////        regIdx: UInt,
////        payload: UInt,
////        isGpr: Boolean,
////      ): Unit = {
////        def myWrReg = (
////          //if (isGpr) {
////          //  wrGpr
////          //} else { // if (!isGpr)
////          //  wrSpr
////          //}
////          cIdEx(psExOutp).get(isGpr)
////        )
////        myWrReg.regIdx := regIdx
////        myWrReg.wrReg.valid := True
////        myWrReg.wrReg.payload := payload
////      }
////      def doWriteGpr(
////        regIdx: UInt,
////        payload: UInt,
////      ): Unit = doWriteReg(
////        regIdx=regIdx,
////        payload=payload,
////        isGpr=true,
////      )
////      def doWriteSpr(
////        regIdx: UInt,
////        payload: UInt,
////      ): Unit = doWriteReg(
////        regIdx=regIdx,
////        payload=payload,
////        isGpr=false,
////      )
////
////      //def doFwdExReg(
////      //  decIdx: UInt,
////      //  nonFwdReg: UInt,
////      //  isGpr: Boolean,
////      //) = {
////      //  doFwdReg(
////      //    decIdx=decIdx,
////      //    nonFwdReg=nonFwdReg,
////      //    isGpr=isGpr,
////      //    someCtrlLink=cIdEx,
////      //  )
////      //}
////
////      //def doFwdExGpr64Half(
////      //  decIdx: UInt,
////      //  nonFwdGpr64Half: UInt,
////      //  isHi: Boolean,
////      //) = {
////      //  doFwdGpr64Half(
////      //    decIdx=decIdx,
////      //    nonFwdGpr64Half=nonFwdGpr64Half,
////      //    isHi=isHi,
////      //    someCtrlLink=cIdEx,
////      //  )
////      //}
////
////      val tempInstrDecEtc = InstrDecEtc()
////      tempInstrDecEtc := myInstrDecEtc
////      tempInstrDecEtc.allowOverride
////      def getNonFwdRegFunc(
////        decIdx: UInt,
////        isGpr: Boolean,
////        whichReg: Int,
////      ): UInt = {
////        if (isGpr) {
////          rGprVec(decIdx)
////        } else {
////          rSprVec(decIdx)
////        }
////      }
////      tempInstrDecEtc.doFwdAllRegs(
////        someCtrlLink=cIdEx,
////        fwdRc=false,
////      )(
////        getNonFwdRegFunc=getNonFwdRegFunc
////      )
////      def ra = tempInstrDecEtc.ra
////      def rb = tempInstrDecEtc.rb
////      def rc = tempInstrDecEtc.rc
////      def lr = tempInstrDecEtc.gprLr
////      def fp = tempInstrDecEtc.gprFp
////      def sp = tempInstrDecEtc.gprSp
////      //def rd = tempInstrDecEtc.rd
////      def sa = tempInstrDecEtc.sa
////      def sb = tempInstrDecEtc.sb
////      def flags = tempInstrDecEtc.sprFlags
////      def ids = tempInstrDecEtc.sprIds
////      def ira = tempInstrDecEtc.sprIra
////      def ie = tempInstrDecEtc.sprIe
////      def ity = tempInstrDecEtc.sprIty
////      def sty = tempInstrDecEtc.sprSty
////      def raIdx = tempInstrDecEtc.raIdx
////      def rbIdx = tempInstrDecEtc.rbIdx
////      def rcIdx = tempInstrDecEtc.rcIdx
////      //val rdIdx = UInt(numGprsSprsPow bits) // 
////
////      //val saIdx = UInt(numGprsSprsPow bits) //
////      //val sbIdx = UInt(numGprsSprsPow bits) //
////
////
////      def ra64Hi = tempInstrDecEtc.ra64Hi
////      def ra64Lo = tempInstrDecEtc.ra64Lo
////      def rb64Hi = tempInstrDecEtc.rb64Hi
////      def rb64Lo = tempInstrDecEtc.rb64Lo
////
////      //val ra64HiIdx = UInt(numGprsSprsPow bits)
////      //val ra64LoIdx = UInt(numGprsSprsPow bits)
////      //val rb64HiIdx = UInt(numGprsSprsPow bits)
////      //val rb64LoIdx = UInt(numGprsSprsPow bits)
////
////      def ra64HiIdx = tempInstrDecEtc.ra64HiIdx
////      def ra64LoIdx = tempInstrDecEtc.ra64LoIdx
////      def rb64HiIdx = tempInstrDecEtc.rb64HiIdx
////      def rb64LoIdx = tempInstrDecEtc.rb64LoIdx
////      //def raIdx = upInstrDecEtc.raIdx
////
////      //def ra = doFwdExReg(
////      //  decIdx=raIdx,
////      //  nonFwdReg=upInstrDecEtc.ra,
////      //  isGpr=true,
////      //)
////      ////val ra = Mux[UInt](
////      ////  !(
////      ////    raIdx === cIdEx(psExOutp).gprIdx
////      ////    && nWb(psExOutp).wrGpr.valid
////      ////  ),
////      ////  upInstrDecEtc.ra,
////      ////  nWb(psExOutp).wrGpr.payload,
////      ////)
////      //def rbIdx = upInstrDecEtc.rbIdx
////      //def rb = doFwdExReg(
////      //  decIdx=rbIdx,
////      //  nonFwdReg=upInstrDecEtc.rb,
////      //  isGpr=true,
////      //)
////      //def rcIdx = upInstrDecEtc.rcIdx
////      //def rc = doFwdExReg(
////      //  decIdx=rcIdx,
////      //  nonFwdReg=upInstrDecEtc.rc,
////      //  isGpr=true,
////      //)
////
////      ////def rdIdx = upInstrDecEtc.rdIdx
////      ////def rd = doFwdExReg(
////      ////  decIdx=rdIdx,
////      ////  someReg=upInstrDecEtc.rd,
////      ////  isGpr=true,
////      ////)
////      ////def saIdx = upInstrDecEtc.saIdx
////      //def sa = doFwdExReg(
////      //  decIdx=raIdx,
////      //  nonFwdReg=upInstrDecEtc.sa,
////      //  isGpr=false,
////      //)
////      ////def sbIdx = upInstrDecEtc.sbIdx
////      //def sb = doFwdExReg(
////      //  decIdx=rbIdx,
////      //  nonFwdReg=upInstrDecEtc.sb,
////      //  isGpr=false,
////      //)
////
////      //def ra64HiIdx = upInstrDecEtc.ra64HiIdx
////      //def ra64Hi = doFwdExReg(
////      //  decIdx=ra64HiIdx,
////      //  nonFwdReg=upInstrDecEtc.ra64Hi,
////      //  isGpr=true,
////      //)
////      //def ra64LoIdx = upInstrDecEtc.ra64LoIdx
////      //def ra64Lo = doFwdExReg(
////      //  decIdx=ra64LoIdx,
////      //  nonFwdReg=upInstrDecEtc.ra64Lo,
////      //  isGpr=true,
////      //)
////
////      //def rb64HiIdx = upInstrDecEtc.rb64HiIdx
////      //def rb64Hi = doFwdExReg(
////      //  decIdx=rb64HiIdx,
////      //  nonFwdReg=upInstrDecEtc.rb64Hi,
////      //  isGpr=true,
////      //)
////      //def rb64LoIdx = upInstrDecEtc.rb64LoIdx
////      //def rb64Lo = doFwdExReg(
////      //  decIdx=rb64LoIdx,
////      //  nonFwdReg=upInstrDecEtc.rb64Lo,
////      //  isGpr=true,
////      //)
////      //def ra64Hi = doFwdExGpr64Half(
////      //  decIdx=ra64HiIdx
////      //)
////      //def ra = rGprVec(cIdEx.down(instrDecEtc).raIdx)
////      //def rb = rGprVec(cIdEx.down(instrDecEtc).rbIdx)
////      ////def rc = rGprVec(cIdEx.down(instrDecEtc).rcIdx)
////      //def lr = rGprVec(InstrEncConst.gprLrIdx)
////      //def fp = rGprVec(InstrEncConst.gprFpIdx)
////      //def sp = rGprVec(InstrEncConst.gprSpIdx)
////      //def sa = rSprVec(cIdEx.down(instrDecEtc).raIdx)
////      //def sb = rSprVec(cIdEx.down(instrDecEtc).rbIdx)
////      ////def sc = rSprVec(cIdEx.down(instrDecEtc).rcIdx)
////      //def flags = rSprVec(InstrEncConst.sprFlagsIdx)
////      //def ids = rSprVec(InstrEncConst.sprIdsIdx)
////      //def ira = rSprVec(InstrEncConst.sprIraIdx)
////      //def ie = rSprVec(InstrEncConst.sprIeIdx)
////      //def ity = rSprVec(InstrEncConst.sprItyIdx)
////      //def sty = rSprVec(InstrEncConst.sprStyIdx)
////
////      //def performFetch(): Unit = {
////      //}
////      //def performLoad(
////      //  rawElemNumBytesPow: (Int, Int),
////      //  dst: UInt,
////      //): Unit = {
////      //}
////      //def performStore(
////      //  rawElemNumBytesPow: (Int, Int),
////      //  src: UInt,
////      //): Unit = {
////      //}
////      def performSetFlagsZn(
////        rawElemNumBytesPow: (Int, Int),
////        result: UInt,
////        //flagsOut: UInt,
////      ): Unit = {
////        //--------
////        def myBits = params.elemNumBytesPow(
////          rawElemNumBytesPow=rawElemNumBytesPow
////        )._2
////        flags(flagIdxZ) := (result(myBits - 1 downto 0) === 0)
////        flags(flagIdxN) := result(myBits - 1)
////        //--------
////      }
////      def performAddSub(
////        rawElemNumBytesPow: (Int, Int),
////        operandA: UInt,
////        operandB: UInt,
////        withCarryIn: Boolean,
////        doSub: Boolean,
////        doSetFlags: Boolean,
////        //flagsOut: UInt
////        result: UInt,
////        flagsOut: Option[UInt]=None,
////      ): Unit = {
////        //--------
////        def myBits = params.elemNumBytesPow(
////          rawElemNumBytesPow=rawElemNumBytesPow
////        )._2
////        assert(result.getWidth == myBits + 1)
////        //--------
////        //uint64_t
////        //  ret = 0,
////        //  temp_operand_a = operand_a,
////        //  temp_operand_b = operand_b,
////        //  temp_flags_c_mask = 0,
////        //  temp_flags_vn_mask = 0;
////        val tempOperandA = UInt((myBits + 1) bits)
////        val tempOperandB = UInt((myBits + 1) bits)
////        tempOperandA := operandA
////        tempOperandB := operandB
////        if (!doSub) {
////          //ret = temp_operand_a + temp_operand_b
////          //+ (with_carry_in
////          //  ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
////          //  : 0x0ull);
////          result := (
////            tempOperandA + tempOperandB
////            + (
////              if (withCarryIn) {
////                flags(flagIdxC downto flagIdxC)
////              } else { // if (!withCarryIn)
////                U"1'd0"
////              }
////            ).resized
////          )
////        } else { // if (doSub)
////          /* 6502-style subtraction */
////          //ret = temp_operand_a + (~temp_operand_b)
////          //  + (with_carry_in 
////          //    ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
////          //    : 0x1ull);
////          result := (
////            tempOperandA + (~tempOperandB)
////            + (
////              if (withCarryIn) {
////                flags(flagIdxC downto flagIdxC)
////              } else { // if (!withCarryIn)
////                U"1'd1"
////              }
////            ).resized
////          )
////        }
////
////        if (doSetFlags) {
////          val tempFlagsOut = flagsOut match {
////            case Some(myFlagsOut) => myFlagsOut;
////            case None => flags
////          }
////          val tempFlags = cloneOf(tempFlagsOut)
////          tempFlags := 0x0
////          tempFlags.allowOverride
////          performSetFlagsZn(
////            rawElemNumBytesPow=rawElemNumBytesPow,
////            result=result,
////          )
////          tempFlagsOut(flagIdxC) := result(myBits)
////          tempFlagsOut(flagIdxV) := (
////            (tempOperandA ^ result.resized)
////            & (tempOperandB ^ result.resized)
////          )(myBits - 1)
////        }
////        //--------
////      }
////      //--------
////      //is (InstrFullgrpDec.g0Pre) {
////      //}
////      //is (InstrFullgrpDec.g0Lpre) {
////      //}
////      //is (InstrFullgrpDec.g1) {
////      //  switch (cIdEx.down(instrDecEtc).instrG1Enc.op) {
////      //    is (InstrG1EncOp.addRaS5) {    // Opcode 0x0: add rA, #simm5
////      //      ra := ra + simm
////      //    }
////      //    is (InstrG1EncOp.addRaPcS5) {  // Opcode 0x1: add rA, pc, #simm5
////      //      ra := myPcPlus2 + simm
////      //    }
////      //    is (InstrG1EncOp.addRaSpS5) {  // Opcode 0x2: add rA, sp, #simm5
////      //      ra := sp + simm
////      //    }
////      //    is (InstrG1EncOp.addRaFpS5) {  // Opcode 0x3: add rA, fp, #simm5
////      //      ra := fp + simm
////      //    }
////      //    is (InstrG1EncOp.cmpRaS5) {    // Opcode 0x4: cmp rA, #simm5
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      performAddSub(
////      //        rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //        operandA=ra,
////      //        operandB=simm,
////      //        withCarryIn=false,
////      //        doSub=true,
////      //        doSetFlags=true,
////      //        result=tempResult,
////      //      )
////      //    }
////      //    is (InstrG1EncOp.cpyRaS5) {    // Opcode 0x5: cpy rA, #simm5
////      //      ra := simm
////      //    }
////      //    is (InstrG1EncOp.lslRaI5) {    // Opcode 0x6: lsl rA, #imm5
////      //      ra := ra << imm
////      //    }
////      //    is (InstrG1EncOp.lsrRaI5) {    // Opcode 0x7: lsr rA, #imm5
////      //      ra := ra >> imm
////      //    }
////      //    is (InstrG1EncOp.asrRaI5) {    // Opcode 0x8: asr rA, #imm5
////      //      ra := (ra.asSInt >> imm).asUInt
////      //    }
////      //    is (InstrG1EncOp.andRaS5) {    // Opcode 0x9: and rA, #simm5
////      //      ra := ra & simm
////      //    }
////      //    is (InstrG1EncOp.orrRaS5) {    // Opcode 0xa: orr rA, #simm5
////      //      ra := ra | simm
////      //    }
////      //    is (InstrG1EncOp.xorRaS5) {    // Opcode 0xb: xor rA, #simm5
////      //      ra := ra ^ simm
////      //    }
////      //    is (InstrG1EncOp.zeRaI5) {     // Opcode 0xc: ze rA, #imm5
////      //      //ra := ra(imm - 1 downto 0)
////      //      //switch (imm) {
////      //      //  for (value <- 0 until (1 << nonG3ImmWidth)) {
////      //      //    is (value) {
////      //      //      ra := ra(value - 1 downto 0).resized
////      //      //    }
////      //      //  }
////      //      //}
////      //      //ra := ra << (mainWidth - imm)
////      //      //ra := ra & ((1 << imm) - 1)
////      //      def tempShiftAmount = mainWidth - imm
////      //      ra := (ra << tempShiftAmount) >> tempShiftAmount
////      //    }
////      //    is (InstrG1EncOp.seRaI5) {     // Opcode 0xd: se rA, #imm5
////      //      //switch (imm) {
////      //      //  for (value <- 0 until (1 << nonG3ImmWidth)) {
////      //      //    is (value) {
////      //      //      ra := ra.asSInt(value - 1 downto 0).resized.asUInt
////      //      //    }
////      //      //  }
////      //      //}
////      //      //ra := ra & ((1 << imm) - 1)
////      //      def tempShiftAmount = mainWidth - imm
////      //      ra := (
////      //        ((ra.asSInt << tempShiftAmount) >> tempShiftAmount).asUInt
////      //      )
////      //    }
////      //    is (InstrG1EncOp.swiRaS5) {    // Opcode 0xe: swi rA, #simm5
////      //    }
////      //    is (InstrG1EncOp.swiI5) {      // Opcode 0xf: swi #simm5
////      //    }
////      //  }
////      //}
////      //is (InstrFullgrpDec.g2) {
////      //  switch (cIdEx.down(instrDecEtc).instrG2Enc.op) {
////      //    def f = cIdEx.down(instrDecEtc).instrG2Enc.f
////      //    is (InstrG2EncOp.addRaRb) {   // Opcode 0x0: add rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = false
////      //      def tempDoSub = false
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.subRaRb) {   // Opcode 0x1: sub rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = false
////      //      def tempDoSub = true
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.addRaSpRb) { // Opcode 0x2: add rA, sp, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = sp
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = false
////      //      def tempDoSub = false
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.addRaFpRb) { // Opcode 0x3: add rA, fp, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = fp
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = false
////      //      def tempDoSub = false
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.cmpRaRb) {   // Opcode 0x4: cmp rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = false
////      //      def tempDoSub = true
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        //ra := tempResult(ra.bitsRange)
////      //      }
////      //      //when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      //} otherwise { // when (f)
////      //      //  myPerformAddSub(doSetFlags=true)
////      //      //}
////      //    }
////      //    is (InstrG2EncOp.cpyRaRb) {   // Opcode 0x5: cpy rA, rB
////      //      //ra := rb
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.lslRaRb) {   // Opcode 0x6: lsl rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := ra << rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.lsrRaRb) {   // Opcode 0x7: lsr rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := ra >> rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.asrRaRb) {   // Opcode 0x8: asr rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := (ra.asSInt >> rb).asUInt
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.andRaRb) {   // Opcode 0x9: and rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := ra & rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.orrRaRb) {   // Opcode 0xa: orr rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := ra | rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.xorRaRb) {   // Opcode 0xb: xor rA, rB
////      //      val tempResult = UInt(mainWidth bits)
////      //      tempResult := ra ^ rb
////      //      ra := tempResult
////      //      when (f) {
////      //        performSetFlagsZn(
////      //          rawElemNumBytesPow=params.rawElemNumBytesPow32,
////      //          result=tempResult,
////      //        )
////      //      }
////      //    }
////      //    is (InstrG2EncOp.adcRaRb) {   // Opcode 0xc: adc rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = true
////      //      def tempDoSub = false
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.sbcRaRb) {   // Opcode 0xd: sbc rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = true
////      //      def tempDoSub = true
////
////      //      def myPerformAddSub(
////      //        doSetFlags: Boolean,
////      //      ): Unit = {
////      //        performAddSub(
////      //          rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //          operandA=tempOperandA,
////      //          operandB=tempOperandB,
////      //          withCarryIn=tempWithCarryIn,
////      //          doSub=tempDoSub,
////      //          doSetFlags=doSetFlags,
////      //          result=tempResult,
////      //        )
////      //        ra := tempResult(ra.bitsRange)
////      //      }
////      //      when (!f) {
////      //        myPerformAddSub(doSetFlags=false)
////      //      } otherwise { // when (f)
////      //        myPerformAddSub(doSetFlags=true)
////      //      }
////      //    }
////      //    is (InstrG2EncOp.cmpbcRaRb) { // Opcode 0xe: cmpbc rA, rB
////      //      val tempResult = UInt((mainWidth + 1) bits)
////      //      def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
////      //      def tempOperandA = ra
////      //      def tempOperandB = rb
////      //      def tempWithCarryIn = true
////      //      def tempDoSub = true
////      //      def tempDoSetFlags = true
////      //      val tempFlagsOut = UInt(mainWidth bits)
////
////      //      performAddSub(
////      //        rawElemNumBytesPow=tempRawEleNumBytesPow,
////      //        operandA=tempOperandA,
////      //        operandB=tempOperandB,
////      //        withCarryIn=tempWithCarryIn,
////      //        doSub=tempDoSub,
////      //        doSetFlags=tempDoSetFlags,
////      //        result=tempResult,
////      //        flagsOut=Some(tempFlagsOut),
////      //      )
////      //    }
////      //    is (InstrG2EncOp.invalid0) {  // Opcode 0xf: invalid operation 0
////      //    }
////      //  }
////      //}
////      //is (InstrFullgrpDec.g3) {
////      //  def someNextPc = Cat(
////      //    cIdEx.down(instrDecEtc).fullPcrelSimm(
////      //      params.mainWidth - 1 downto 1
////      //    ),
////      //    False,
////      //  ).asUInt
////      //  switch (cIdEx.down(instrDecEtc).instrG3Enc.op) {
////      //    def doSetPc(): Unit = {
////      //      psExSetPc.valid := True
////      //      psExSetPc.payload := someNextPc
////      //    }
////      //    is (InstrG3EncOp.blS9) {       // Opcode 0x0: bl simm9
////      //      doSetPc()
////      //      lr := cIdEx.down(pcPlus2)
////      //    }
////      //    is (InstrG3EncOp.braS9) {      // Opcode 0x1: bra simm9
////      //      doSetPc()
////      //    }
////      //    is (InstrG3EncOp.beqS9) {      // Opcode 0x2: beq simm9
////      //      when (flags(flagIdxZ)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bneS9) {      // Opcode 0x3: bne simm9
////      //      when (!flags(flagIdxZ)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bmiS9) {      // Opcode 0x4: bmi simm9
////      //      when (flags(flagIdxN)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bplS9) {      // Opcode 0x5: bpl simm9
////      //      when (!flags(flagIdxN)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bvsS9) {      // Opcode 0x6: bvs simm9
////      //      when (flags(flagIdxV)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bvcS9) {      // Opcode 0x7: bvc simm9
////      //      when (!flags(flagIdxV)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bgeuS9) {     // Opcode 0x8: bgeu simm9
////      //      when (flags(flagIdxC)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bltuS9) {     // Opcode 0x9: bltu simm9
////      //      when (!flags(flagIdxC)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bgtuS9) {     // Opcode 0xa: bgtu simm9
////      //      when (flags(flagIdxC) && !flags(flagIdxZ)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bleuS9) {     // Opcode 0xb: bleu simm9
////      //      when (!flags(flagIdxC) || flags(flagIdxZ)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bgesS9) {     // Opcode 0xc: bges simm9
////      //      when (flags(flagIdxN) === flags(flagIdxV)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bltsS9) {     // Opcode 0xd: blts simm9
////      //      when (flags(flagIdxN) =/= flags(flagIdxV)) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.bgtsS9) {     // Opcode 0xe: bgts simm9
////      //      when (
////      //        flags(flagIdxN) === flags(flagIdxV)
////      //        && !flags(flagIdxZ)
////      //      ) {
////      //        doSetPc()
////      //      }
////      //    }
////      //    is (InstrG3EncOp.blesS9) {     // Opcode 0xf: bles simm9
////      //      when (
////      //        flags(flagIdxN) =/= flags(flagIdxV)
////      //        || flags(flagIdxZ)
////      //      ) {
////      //        doSetPc()
////      //      }
////      //    }
////      //  }
////      //}
////      //is (InstrFullgrpDec.g4) {
////      //  switch (cIdEx.down(instrDecEtc).instrG4Enc.op) {
////      //    //--------
////      //    is (InstrG4EncOp.jlRa) {         // Opcode 0x0: jl rA
////      //      psExSetPc.valid := True
////      //      psExSetPc.payload := ra
////      //      lr := cIdEx.down(pcPlus2)
////      //    }
////      //    is (InstrG4EncOp.jmpRa) {        // Opcode 0x1: jmp rA
////      //      psExSetPc.valid := True
////      //      psExSetPc.payload := ra
////      //    }
////      //    is (InstrG4EncOp.jmpIra) {       // Opcode 0x2: jmp ira
////      //      psExSetPc.valid := True
////      //      psExSetPc.payload := ira
////      //    }
////      //    is (InstrG4EncOp.reti) {         // Opcode 0x3: reti
////      //      psExSetPc.valid := True
////      //      psExSetPc.payload := ira
////      //      ie := U(default -> True)
////      //    }
////      //    is (InstrG4EncOp.ei) {           // Opcode 0x4: ei
////      //      ie := U(default -> True)
////      //    }
////      //    is (InstrG4EncOp.di) {           // Opcode 0x5: di
////      //      ie := U(default -> False)
////      //    }
////      //    is (InstrG4EncOp.pushRaRb) {     // Opcode 0x6: push rA, rB
////      //    }
////      //    is (InstrG4EncOp.pushSaRb) {     // Opcode 0x7: push sA, rB
////      //    }
////      //    is (InstrG4EncOp.popRaRb) {      // Opcode 0x8: pop rA, rB
////      //    }
////      //    is (InstrG4EncOp.popSaRb) {      // Opcode 0x9: pop sA, rB
////      //    }
////      //    is (InstrG4EncOp.popPcRb) {      // Opcode 0xa: pop pc, rB
////      //    }
////      //    is (InstrG4EncOp.mulRaRb) {      // Opcode 0xb: mul rA, rB
////      //      ra := ra * rb
////      //    }
////      //    is (InstrG4EncOp.udivRaRb) {     // Opcode 0xc: udiv rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.sdivRaRb) {     // Opcode 0xd: sdiv rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.umodRaRb) {     // Opcode 0xe: umod rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.smodRaRb) {     // Opcode 0xf: smod rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    //--------
////      //    is (InstrG4EncOp.lumulRaRb) {    // Opcode 0x10: lumul rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.lsmulRaRb) {    // Opcode 0x11: lsmul rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.udiv64RaRb) {   // Opcode 0x12: udiv64 rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.sdiv64RaRb) {   // Opcode 0x13: sdiv64 rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.umod64RaRb) {   // Opcode 0x14: umod64 rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.smod64RaRb) {   // Opcode 0x15: smod64 rA, rB
////      //      //cIdEx.haltIt()
////      //    }
////      //    is (InstrG4EncOp.ldubRaRb) {     // Opcode 0x16: ldub rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.ldsbRaRb) {     // Opcode 0x17: ldsb rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.lduhRaRb) {     // Opcode 0x18: lduh rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.ldshRaRb) {     // Opcode 0x19: ldsh rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.stbRaRb) {      // Opcode 0x1a: stb rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.sthRaRb) {      // Opcode 0x1b: sth rA, [rB]
////      //    }
////      //    is (InstrG4EncOp.cpyRaSb) {      // Opcode 0x1c: cpy rA, sB
////      //      ra := sb
////      //    }
////      //    is (InstrG4EncOp.cpySaRb) {      // Opcode 0x1d: cpy sA, rB
////      //      sa := rb
////      //    }
////      //    is (InstrG4EncOp.cpySaSb) {      // Opcode 0x1e: cpy sA, sB
////      //      sa := sb
////      //    }
////      //    is (InstrG4EncOp.indexRa) {      // Opcode 0x1f: index rA
////      //    }
////      //    //--------
////      //  }
////      //}
////      //is (InstrFullgrpDec.g5) {
////      //}
////      //is (InstrFullgrpDec.g6) {
////      //}
////      //is (InstrFullgrpDec.g7Sg00) {
////      //}
////      //is (InstrFullgrpDec.g7Sg010) {
////      //}
////      //is (InstrFullgrpDec.g7Sg0110) {
////      //}
////      //default {
////      //  // eek!
////      //}
////    }
////  } otherwise {
////    //cIdEx.throwWhen(psExSetPc.valid)
////    cIdEx.throwIt() // cancel the current transaction
////    //cIdEx.terminateIt() // clear `cIfId.down.valid`
////    psExSetPc.valid := False
////  }
////
////  // Pipeline Stage: Write Back
////  //when (cExWb.down.isFiring)
////  {
////    def wrGpr = cExWb.down(psExOutp).get(isGpr=true)
////    def wrSpr = cExWb.down(psExOutp).get(isGpr=false)
////    when (wrGpr.wrReg.fire) {
////      rGprVec(wrGpr.regIdx) := wrGpr.wrReg.payload
////    }
////    when (wrSpr.wrReg.fire) {
////      rSprVec(wrSpr.regIdx) := wrSpr.wrReg.payload
////    }
////  }
////
////  //val locIcache = new Area {
////  //}
////  //val locDcache = new Area {
////  //}
////
////  //val locIf = new Area {
////  //  //val ibusAStm = cloneOf(io.ibus.a)
////  //  //io.ibus.a <-/< ibusAStm
////  //  //val ibusDStm = cloneOf(io.ibus.d)
////  //  //ibusDStm <-/< io.ibus.d
////
////  //  //def flagIdxZ = 0
////  //  //def flagIdxC = 1
////  //  //def flagIdxV = 2
////  //  //def flagIdxN = 3
////
////  //  //val regFlags = UInt(mainWidth bits)
////  //}
////  ////--------
////  //val locId = new Area {
////  //}
////  ////--------
////  //val locEx = new Area {
////  //  //val myFlags = cloneOf(locIf.regFlags)
////
////  //  //val dbusAStm = cloneOf(io.dbus.a)
////  //  //dbusAStm <-/< io.dbus.a
////  //  //val dbusDStm = cloneOf(io.dbus.d)
////  //  //dbusDStm <-/< io.dbus.d
////  //}
////  //--------
////  //--------
////  Builder(linkArr.toSeq)
////  //--------
////}
////
////object FlareCpuVerilog extends App {
////  Config.spinal.generateVerilog(FlareCpu(params=FlareCpuParams()))
////}
