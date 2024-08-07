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
import libcheesevoyage.math.LongDivMultiCycle

case class FlareCpuIo(
  params: FlareCpuParams,
) extends Bundle {
  //--------
  val bus = master(Bmb(p=params.busParams))
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
  wordCount: Int,
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
    wordCount=wordCount,
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

case class FlareCpu(
  params: FlareCpuParams
) extends Component {
  //--------
  val io = FlareCpuIo(params=params)
  val instrBmb = Bmb(p=params.busParams)
  val dataBmb = Bmb(p=params.busParams)
  val busArb = BmbArbiter(
    inputsParameter=List(
      params.busParams,
      params.busParams,
    ),
    outputParameter=params.busParams,
    lowerFirstPriority=false,
  )
  busArb.io.inputs(0) << instrBmb
  busArb.io.inputs(1) << dataBmb
  io.bus << busArb.io.output
  //--------
  def enumRegFileGprEven = FlareCpuParams.enumRegFileGprEven
  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
  def enumRegFileSpr = FlareCpuParams.enumRegFileSpr
  def enumRegFileLim = FlareCpuParams.enumRegFileLim

  def enumPipeMemIcache = 0
  def enumPipeMemDcache = 1
  def enumPipeMemGprFileEven = 2
  def enumPipeMemGprFileOddNonSp = 3
  def enumPipeMemGprFileSp = 4
  def enumPipeMemSprFile = 5
  def enumPipeMemLim = 6

  val linkArr = PipeHelper.mkLinkArr()
  def mkRegFilePipeMemDoModInModFrontFunc(
    regFileKind: Int
  ): (
    Bool, // nextPrevTxnWasHazard
    Bool, // rPrevTxnWasHazard,
    FlareCpuPipeMemModType[ // outp
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    FlareCpuPipeMemModType[ // inp
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    CtrlLink,   // mod.front.cMid0Front
    Node,       // io.modFront
    FlareCpuPipeMemModType[ // io.tempModFrontPayload
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    UInt,                 // myModMemWord
  ) => Unit = {
    val retFunc = (
      nextPrevTxnWasHazard: Bool,
      rPrevTxnWasHazard: Bool,
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
      cMid0Front: CtrlLink,
      modFront: Node,
      tempModFrontPayload: FlareCpuPipeMemModType[
        UInt,
        Bool,
        PipeMemModExtType,
      ],
      myModMemWord: UInt,
    ) => {
      //nextPrevTxnWasHazard := False

      outp := inp
      outp.allowOverride

      def instrDecEtc = inp.modExt.instrDecEtc

      if (regFileKind == enumRegFileGprEven) {
        outp.myExt.modMemWordValid := (
          //!instrDecEtc.raIdx.fire
          //|| instrDecEtc.raIdx.payload(0)
          instrDecEtc.regFileGprEvenModMemWordValid
        )
      } else if (regFileKind == enumRegFileGprOddNonSp) {
        outp.myExt.modMemWordValid := (
          //!instrDecEtc.raIdx.fire
          //|| !instrDecEtc.raIdx.payload(0)
          instrDecEtc.regFileGprOddNonSpModMemWordValid
        )
      } else if (regFileKind == enumRegFileGprSp) {
        outp.myExt.modMemWordValid := (
          //!instrDecEtc.raIdx.fire
          //|| (
          //  instrDecEtc.raIdx.payload
          //  === FlareCpuInstrEncConst.gprSpIdx
          //)
          instrDecEtc.regFileGprSpModMemWordValid
        )
      } else { // if (regFileKind == enumRegFileSpr)
        outp.myExt.modMemWordValid := (
          //!instrDecEtc.raIdx.fire
          instrDecEtc.regFileSprModMemWordValid
        )
      }
      val fakeRet = Bool()
      fakeRet := True // needed for the `Unit` Scala type to be inferred as
                      // the return value of this function
    }
    retFunc
  }

  case class IcacheWordType(
  ) extends Bundle {
    val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
    val data = params.icacheLineMemWordType()
  }
  case class IcachePipePayload(
  ) extends Bundle {
    val hit = Bool()
    val valid = Bool()
    val word = IcacheWordType()
  }
  case class DcacheWordType(
  ) extends Bundle {
    val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
    val data = params.dcacheLineMemWordType()
  }
  case class DcachePipePayload(
  ) extends Bundle {
    val hit = Bool()

    val word = DcacheWordType()

    val valid = Bool()
    val dirty = Bool()
  }
  case class PipeMemModExtType(
  ) extends Bundle {
    // this `Bundle` is the non-`PipeMemRmw` main pipeline payload type

    val instrEnc = FlareCpuInstrEnc(params=params)
    val instrDecEtc = FlareCpuInstrDecEtc(params=params)
    val icache = IcachePipePayload()
    val dcache = DcachePipePayload()
  }
  // `exSetPc` is to be driven by the `EX` pipeline stage
  val exSetPc = Flow(UInt(params.mainWidth bits))
  def mkIcacheModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=IcacheWordType(),
      wordCount=params.icacheLineMemWordCount,
      hazardCmpType=Bool(),
      modRdPortCnt=params.icacheModRdPortCnt,
      modStageCnt=params.icacheModStageCnt,
      optModHazardKind=params.icacheOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val icache = PipeMemRmw[
    IcacheWordType,
    Bool,
    FlareCpuPipeMemModType[
      IcacheWordType,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[IcacheWordType, Bool],
  ](
    wordType=IcacheWordType(),
    wordCount=params.icacheLineMemWordCount,
    hazardCmpType=Bool(),
    modType=mkIcacheModType(),
    modRdPortCnt=params.icacheModRdPortCnt,
    modStageCnt=params.icacheModStageCnt,
    pipeName="FlareCpu_icache",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemIcache,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.icacheLineMemWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.icacheOptModHazardKind,
    optEnableClear=false,
    memRamStyle="block",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //doModInFrontFunc=Some(
    //  (
    //    outp: FlareCpuPipeMemModType[
    //      IcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    inp: FlareCpuPipeMemModType[
    //      IcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    cFront,
    //  ) => {
    //    when (cFront.up.isFiring) {
    //    }
    //  }
    //)
    //doModInModFrontFunc=Some(
    //  (
    //    nextPrevTxnWasHazard: Bool,
    //    rPrevTxnWasHazard: Bool,
    //    outp: FlareCpuPipeMemModType[
    //      IcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    inp: FlareCpuPipeMemModType[
    //      IcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    cMid0Front: CtrlLink,
    //    modFront: Node,
    //    tempModFrontPayload: FlareCpuPipeMemModType[
    //      IcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    myModMemWord: IcacheWordType,
    //  ) => {
    //  }
    //)
  )
  def mkDcacheModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=DcacheWordType(),
      wordCount=params.dcacheLineMemWordCount,
      hazardCmpType=Bool(),
      modRdPortCnt=params.dcacheModRdPortCnt,
      modStageCnt=params.dcacheModStageCnt,
      optModHazardKind=params.dcacheOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val dcache = PipeMemRmw[
    DcacheWordType,
    Bool,
    FlareCpuPipeMemModType[
      DcacheWordType,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[DcacheWordType, Bool],
  ](
    wordType=DcacheWordType(),
    wordCount=params.dcacheLineMemWordCount,
    hazardCmpType=Bool(),
    modType=mkDcacheModType(),
    modRdPortCnt=params.dcacheModRdPortCnt,
    modStageCnt=params.dcacheModStageCnt,
    pipeName="FlareCpu_dcache",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemDcache,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.dcacheLineMemWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.dcacheOptModHazardKind,
    optEnableClear=false,
    memRamStyle="block",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //doModInFrontFunc=Some(
    //  (
    //    outp: FlareCpuPipeMemModType[
    //      DcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    inp: FlareCpuPipeMemModType[
    //      DcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    cFront,
    //  ) => {
    //    when (cFront.up.isFiring) {
    //    }
    //  }
    //)
    //doModInModFrontFunc=Some(
    //  (
    //    nextPrevTxnWasHazard: Bool,
    //    rPrevTxnWasHazard: Bool,
    //    outp: FlareCpuPipeMemModType[
    //      DcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    inp: FlareCpuPipeMemModType[
    //      DcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    cMid0Front: CtrlLink,
    //    modFront: Node,
    //    tempModFrontPayload: FlareCpuPipeMemModType[
    //      DcacheWordType,
    //      Bool,
    //      PipeMemModExtType,
    //    ],
    //    myModMemWord: DcacheWordType,
    //  ) => {
    //  }
    //)
  )
  //case class IdForkType() {
  //  val regFile = 
  //}
  def mkRegFileForkJoinModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCount=params.numGprsSprs,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.regFileNonSpModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  def mkGprFileEvenModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCount=params.gprFileEvenWordCount,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.regFileNonSpModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val gprFileEven = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool],
  ](
    // `r0`, `r2`, `r4`, `r6`, `r8`, `r10`, `r12`, `fp`
    wordType=params.regWordType(),
    wordCount=params.gprFileEvenWordCount,
    hazardCmpType=params.regFileHazardCmpType(),
    modType=mkGprFileEvenModType(),
    modRdPortCnt=params.regFileNonSpModRdPortCnt,
    modStageCnt=params.regFileModStageCnt,
    pipeName="FlareCpu_gprFileEven",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemGprFileEven,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.gprFileEvenWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.regFileOptModHazardKind,
    optEnableClear=false,
    memRamStyle="auto",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //--------
    doModInModFrontFunc=Some(
      mkRegFilePipeMemDoModInModFrontFunc(
        regFileKind=enumRegFileGprEven
      )
    ),
    //--------
  )
  def mkGprFileOddNonSpModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCount=params.gprFileOddNonSpWordCount,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.regFileNonSpModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val gprFileOddNonSp = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool],
  ](
    // `r0`, `r2`, `r4`, `r6`, `r8`, `r10`, `r12`, `fp`
    wordType=params.regWordType(),
    wordCount=params.gprFileOddNonSpWordCount,
    hazardCmpType=params.regFileHazardCmpType(),
    modType=mkGprFileOddNonSpModType(),
    modRdPortCnt=params.regFileNonSpModRdPortCnt,
    modStageCnt=params.regFileModStageCnt,
    pipeName="FlareCpu_gprFileOddNonSp",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemGprFileOddNonSp,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.gprFileOddNonSpWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.regFileOptModHazardKind,
    optEnableClear=false,
    memRamStyle="auto",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //--------
    doModInModFrontFunc=Some(
      mkRegFilePipeMemDoModInModFrontFunc(
        regFileKind=enumRegFileGprOddNonSp
      )
    ),
    //--------
  )

  def mkGprFileSpModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCount=params.gprFileSpWordCount,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.gprFileSpModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val gprFileSp = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool],
  ](
    // `r0`, `r2`, `r4`, `r6`, `r8`, `r10`, `r12`, `fp`
    wordType=params.regWordType(),
    wordCount=params.gprFileSpWordCount,
    hazardCmpType=params.regFileHazardCmpType(),
    modType=mkGprFileSpModType(),
    modRdPortCnt=params.gprFileSpModRdPortCnt,
    modStageCnt=params.regFileModStageCnt,
    pipeName="FlareCpu_gprFileSp",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemGprFileSp,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.gprFileSpWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.regFileOptModHazardKind,
    optEnableClear=false,
    memRamStyle="auto",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //--------
    doModInModFrontFunc=Some(
      mkRegFilePipeMemDoModInModFrontFunc(
        regFileKind=enumRegFileGprSp
      )
    ),
    //--------
  )
  def mkSprFileModType() = (
    FlareCpuPipeMemModType(
      params=params,
      wordType=params.regWordType(),
      wordCount=params.sprFileWordCount,
      hazardCmpType=params.regFileHazardCmpType(),
      modRdPortCnt=params.regFileNonSpModRdPortCnt,
      modStageCnt=params.regFileModStageCnt,
      optModHazardKind=params.regFileOptModHazardKind,
      modExtType=PipeMemModExtType(),
    )
  )
  val sprFile = PipeMemRmw[
    UInt,
    Bool,
    FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ],
    PipeMemRmwDualRdTypeDisabled[UInt, Bool],
  ](
    // `r0`, `r2`, `r4`, `r6`, `r8`, `r10`, `r12`, `fp`
    wordType=params.regWordType(),
    wordCount=params.sprFileWordCount,
    hazardCmpType=params.regFileHazardCmpType(),
    modType=mkSprFileModType(),
    modRdPortCnt=params.regFileNonSpModRdPortCnt,
    modStageCnt=params.regFileModStageCnt,
    pipeName="FlareCpu_sprFile",
    linkArr=Some(linkArr),
    memArrIdx=enumPipeMemSprFile,
    memArrSize=enumPipeMemLim,
    optDualRd=false,
    optReorder=false,
    initBigInt=Some({
      val tempArr = new ArrayBuffer[BigInt]()
      for (idx <- 0 until params.sprFileWordCount) {
        tempArr += BigInt(0)
      }
      tempArr.toSeq
    }),
    optModHazardKind=params.regFileOptModHazardKind,
    optEnableClear=false,
    memRamStyle="auto",
    vivadoDebug=false,
    optIncludeModFrontStageLink=false,
  )(
    //--------
    doModInModFrontFunc=Some(
      mkRegFilePipeMemDoModInModFrontFunc(
        regFileKind=enumRegFileSpr
      )
    ),
    //--------
  )
  //--------
  //val nIf = Node()
  // the `IF` pipeline stage
  //val pIf = Payload(mkIcacheModType())
  val pIfInpIcache, pIdOutpIcache = Payload(mkIcacheModType())
  val pMemInpDcache, pMemOutpDcache = Payload(mkDcacheModType())
  val pIdInpGprFileEven, pIdOutpGprFileEven = (
    Payload(mkGprFileEvenModType())
  )
  val pIdInpGprFileOddNonSp, pIdOutpGprFileOddNonSp = (
    Payload(mkGprFileOddNonSpModType())
  )
  val pIdInpGprFileSp, pIdOutpGprFileSp = (
    Payload(mkGprFileSpModType())
  )
  val pIdInpSprFile, pIdOutpSprFile = (
    Payload(mkSprFileModType())
  )
  //val pExInp
  //val pExInp, pExOutp = Payload(

  val cIf = CtrlLink(
    up=Node(),
    down=icache.io.front
  )
  linkArr += cIf
  //val sIf = StageLink(
  //  up=cIf.down,
  //  down=Node(),
  //)
  //linkArr += sIf

  //val pIdGprEven = Payload(gprFileEven.mkExt())

  val cId = CtrlLink(
    up=icache.mod.front.cMid0Front.down,
    down=Node(),
  )
  linkArr += cId
  //val nIdRegFileArr = Array.fill(enumRegFileLim)(Node())
  val nIdIcache = Node()
  val nIdGprFileEvenInp, nIdGprFileEvenOutp  = Node()
  val nIdGprFileOddNonSpInp, nIdGprFileOddNonSpOutp = Node()
  val nIdGprFileSpInp, nIdGprFileSpOutp = Node()
  val nIdSprFileInp, nIdSprFileOutp = Node()
  //nIdRegFileGprEven.setName("nIdRegFileGprEven")
  //nIdRegFileGprOddNonSp.setName("nIdRegFileGprOddNonSp")
  //nIdRegFileGprSp.setName("nIdRegFileGprSp")
  //nIdRegFileSpr.setName("nIdRegFileSpr")

  //val fId = ForkLink(
  //  up=cId.down,
  //  downs=(
  //    //nIdRegFileArr
  //    //List(
  //    //  gprFileEven.io.front,
  //    //  gprFileOddNonSp.io.front,
  //    //  gprFileSp.io.front,
  //    //  sprFile.io.front,
  //    //)
  //    List(
  //      nIdIcache,
  //      nIdGprFileEven,
  //      nIdGprFileOddNonSp,
  //      nIdGprFileSp,
  //      nIdSprFile,
  //    )
  //  ),
  //  synchronous=true,
  //)
  //linkArr += fId

  val fId = new Area {
    val up = Stream(mkIcacheModType())
    val downsIcache = Stream(mkIcacheModType())
    val downsGprFileEven = Stream(mkIcacheModType())
    val downsGprFileOddNonSp = Stream(mkIcacheModType())
    val downsGprFileSp = Stream(mkIcacheModType())
    val downsSprFile = Stream(mkIcacheModType())
    val downs = List(
      downsIcache,
      downsGprFileEven,
      downsGprFileOddNonSp,
      downsGprFileSp,
      downsSprFile,
    )
    val stmFork = StreamFork(
      //dataType=mkRegFileForkJoinModType(),
      input=up,
      portCount=downs.size,
      synchronous=true,
    )
    for (idx <- 0 until downs.size) {
      downs(idx) << stmFork(idx)
    }
  }
  cId.down.driveTo(fId.up)(
    con=(payload, node) => {
      //payload := node
      payload := node(pIdOutpIcache)
    }
  )
  nIdIcache.driveFrom(fId.downsIcache)(
    con=(node, payload) => {
      node(pIdOutpIcache) := payload
    }
  )

  //val fId = StreamFork(
  //)

  val sIdIcache = StageLink(
    up=nIdIcache,
    down=icache.io.modFront,
  )
  linkArr += sIdIcache

  linkArr += DirectLink(
    up=sIdIcache.down,
    down=icache.io.modBack,
  )

  def doIdSplit(
    node: Node,
    pIdInpPayload: NamedType[FlareCpuPipeMemModType[
      UInt,
      Bool,
      PipeMemModExtType,
    ]],
    fIdOutpPayload: FlareCpuPipeMemModType[
      IcacheWordType,
      Bool,
      PipeMemModExtType,
    ],
    whichRegFile: Int,
  ): Unit = {
    //--------
    assert(whichRegFile >= 0)
    assert(whichRegFile < enumRegFileLim)
    //--------
    if (whichRegFile == enumRegFileGprEven) {
      //node(pIdInpPayload).myExt.memAddr(0) := 3
    } else if (whichRegFile == enumRegFileGprOddNonSp) {
    } else if (whichRegFile == enumRegFileGprSp) {
    } else { // if (whichRegFile == enumRegFileSpr)
    }
    //--------
  }
  // I think it's okay to not give names to these `DirectLink`s as we do
  // have the nodes.
  nIdGprFileEvenInp.driveFrom(fId.downsGprFileEven)(
    con=(node, payload) => {
      //node(pIdInpGprFileEven).myExt.memAddr(0) := 3
      doIdSplit(
        node=node,
        pIdInpPayload=pIdInpGprFileEven,
        fIdOutpPayload=payload,
        whichRegFile=enumRegFileGprEven,
      )
    }
  )
  nIdGprFileOddNonSpInp.driveFrom(fId.downsGprFileOddNonSp)(
    con=(node, payload) => {
      //node(pIdInpGprFileOddNonSp) := 
      doIdSplit(
        node=node,
        pIdInpPayload=pIdInpGprFileOddNonSp,
        fIdOutpPayload=payload,
        whichRegFile=enumRegFileGprOddNonSp,
      )
    }
  )
  nIdGprFileSpInp.driveFrom(fId.downsGprFileSp)(
    con=(node, payload) => {
      //node(pIdInpGprFileSp) := 
      doIdSplit(
        node=node,
        pIdInpPayload=pIdInpGprFileSp,
        fIdOutpPayload=payload,
        whichRegFile=enumRegFileGprSp,
      )
    }
  )
  nIdSprFileInp.driveFrom(fId.downsSprFile)(
    con=(node, payload) => {
      //node(pIdInpSprFile) := 
      doIdSplit(
        node=node,
        pIdInpPayload=pIdInpSprFile,
        fIdOutpPayload=payload,
        whichRegFile=enumRegFileSpr,
      )
    }
  )
  val dIdGprFileEven = DirectLink(
    up=nIdGprFileEvenInp,
    down=(
      nIdGprFileEvenOutp
      //gprFileEven.io.front
    ),
  )
  linkArr += dIdGprFileEven
  linkArr += DirectLink(
    up=nIdGprFileEvenOutp,
    down=gprFileEven.io.front,
  )
  //val dIdGprFileOddNonSp = DirectLink(
  //  up=nIdGprFileOddNonSp,
  //  down=gprFileOddNonSp.io.front,
  //)
  //linkArr += dIdGprFileOddNonSp
  //val dIdGprFileSp = DirectLink(
  //  up=nIdGprFileSp,
  //  down=gprFileSp.io.front,
  //)
  //linkArr += dIdGprFileSp
  //val dIdSprFile = DirectLink(
  //  up=nIdSprFile,
  //  down=sprFile.io.front,
  //)
  //linkArr += dIdSprFile
  val dIdGprFileOddNonSp = DirectLink(
    up=nIdGprFileOddNonSpInp,
    down=(
      nIdGprFileOddNonSpOutp
      //gprFileOddNonSp.io.front
    ),
  )
  linkArr += dIdGprFileOddNonSp
  linkArr += DirectLink(
    up=nIdGprFileOddNonSpOutp,
    down=gprFileOddNonSp.io.front,
  )
  val dIdGprFileSp = DirectLink(
    up=nIdGprFileSpInp,
    down=(
      nIdGprFileSpOutp
      //gprFileSp.io.front
    ),
  )
  linkArr += dIdGprFileSp
  linkArr += DirectLink(
    up=nIdGprFileSpOutp,
    down=gprFileSp.io.front,
  )
  val dIdSprFile = DirectLink(
    up=nIdSprFileInp,
    down=(
      nIdSprFileOutp
      //sprFile.io.front
    ),
  )
  linkArr += dIdSprFile
  linkArr += DirectLink(
    up=nIdSprFileOutp,
    down=sprFile.io.front,
  )

  //val sExGprFileEven = StageLink(
  //  up=gprFileEven.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += sExGprFileEven
  //val sExGprFileOddNonSp = StageLink(
  //  up=gprFileOddNonSp.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += sExGprFileOddNonSp
  //val sExGprFileSp = StageLink(
  //  up=gprFileSp.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += sExGprFileSp
  //val sExSprFile = StageLink(
  //  up=sprFile.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += sExSprFile

  //val dExGprFileEven = DirectLink(
  //  up=gprFileEven.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += dExGprFileEven
  //val dExGprFileOddNonSp = DirectLink(
  //  up=gprFileOddNonSp.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += dExGprFileOddNonSp
  //val dExGprFileSp = DirectLink(
  //  up=gprFileSp.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += dExGprFileSp
  //val dExSprFile = DirectLink(
  //  up=sprFile.mod.front.cMid0Front.down,
  //  down=Node(),
  //)
  //linkArr += dExSprFile
  val jEx = JoinLink(
    ups=(
      List(
        gprFileEven.mod.front.cMid0Front.down,
        gprFileOddNonSp.mod.front.cMid0Front.down,
        gprFileSp.mod.front.cMid0Front.down,
        sprFile.mod.front.cMid0Front.down,
        //dExGprFileEven.down,
        //dExGprFileOddNonSp.down,
        //dExGprFileSp.down,
        //dExSprFile.down,
      )
    ),
    down=Node(),
  )
  linkArr += jEx
  val cEx = CtrlLink(
    up=jEx.down,
    down=Node(),
  )
  linkArr += cEx

  //val sEx = StageLink(
  //  up=cEx.down,
  //  down=(
  //    Node()
  //  ),
  //)
  //linkArr += sEx

  //val fEx = ForkLink(
  //  up=sEx.down,
  //  downs=(
  //    List(
  //      //dcache.io.front,
  //      // can't put `dcache.io.front` here
  //      gprFileEven.io.modFront,
  //      gprFileOddNonSp.io.modFront,
  //      gprFileSp.io.modFront,
  //      sprFile.io.modFront,
  //    )
  //  ),
  //  synchronous=true,
  //)
  //linkArr += fEx

  // this is a hack because I couldn't figure out how to make the
  // `JoinLink`s not give me an error!
  val fEx = new Area {
    val up = Stream(mkRegFileForkJoinModType())
    val downsGprFileEven = Stream(mkRegFileForkJoinModType())
    val downsGprFileOddNonSp = Stream(mkRegFileForkJoinModType())
    val downsGprFileSp = Stream(mkRegFileForkJoinModType())
    val downsSprFile = Stream(mkRegFileForkJoinModType())
    val downs = List(
      downsGprFileEven,
      downsGprFileOddNonSp,
      downsGprFileSp,
      downsSprFile,
    )
    val stmFork = StreamFork(
      //dataType=mkRegFileForkJoinModType(),
      input=up,
      portCount=downs.size,
      synchronous=true,
    )
    for (idx <- 0 until downs.size) {
      downs(idx) << stmFork(idx)
    }
  }
  cEx.down.driveTo(fEx.up)(
    con=(payload, node) => {
      //payload := node()
    }
  )
  gprFileEven.io.modFront.driveFrom(fEx.downsGprFileEven)(
    con=(node, payload) => {
    }
  )
  gprFileOddNonSp.io.modFront.driveFrom(fEx.downsGprFileOddNonSp)(
    con=(node, payload) => {
    }
  )
  gprFileSp.io.modFront.driveFrom(fEx.downsGprFileSp)(
    con=(node, payload) => {
    }
  )
  sprFile.io.modFront.driveFrom(fEx.downsSprFile)(
    con=(node, payload) => {
    }
  )
  val sMemGprFileEven = StageLink(
    up=gprFileEven.io.modFront,
    down=Node(),
  )
  linkArr += sMemGprFileEven
  val sMemGprFileOddNonSp = StageLink(
    up=gprFileOddNonSp.io.modFront,
    down=Node(),
  )
  linkArr += sMemGprFileOddNonSp
  val sMemGprFileSp = StageLink(
    up=gprFileSp.io.modFront,
    down=Node(),
  )
  linkArr += sMemGprFileSp
  val sMemSprFile = StageLink(
    up=sprFile.io.modFront,
    down=Node(),
  )
  linkArr += sMemSprFile
  //val sEx = StageLink(
  //  up=cEx.down,
  //  down=(
  //    Node()
  //  ),
  //)
  //linkArr += sEx

  //val cMemGprFileEven = CtrlLink(
  //  up=gprFileEven.io.modFront,
  //  down=Node(),
  //)
  //linkArr += cMemGprFileEven
  //val cMemGprFileOddNonSp = CtrlLink(
  //  up=gprFileOddNonSp.io.modFront,
  //  down=Node(),
  //)
  //linkArr += cMemGprFileOddNonSp
  //val cMemGprFileSp = CtrlLink(
  //  up=gprFileSp.io.modFront,
  //  down=Node(),
  //)
  //linkArr += cMemGprFileSp
  //val cMemSprFile = CtrlLink(
  //  up=sprFile.io.modFront,
  //  down=Node(),
  //)
  //linkArr += cMemSprFile

  //val nMemGprFileEven = Node()
  //val nMemGprFileOddNonSp = Node()
  //val nMemGprFileSp = Node()
  //val nMemSprFile = Node()

  val jMem = JoinLink(
    ups=(
      //fEx.downs
      List(
        //dcache.io.front,
        // can't put `dcache.io.front` here
        //gprFileEven.io.modFront,
        //gprFileOddNonSp.io.modFront,
        //gprFileSp.io.modFront,
        //sprFile.io.modFront,
        sMemGprFileEven.down,
        sMemGprFileOddNonSp.down,
        sMemGprFileSp.down,
        sMemSprFile.down,
      )
    ),
    down=(
      Node()
    ),
  )
  linkArr += jMem
  val cMem = CtrlLink(
    up=jMem.down,
    down=(
      Node()
    ),
  )
  linkArr += cMem

  val nMemRegFile = Node()
  val fMem = ForkLink(
    up=cMem.down,
    downs=(
      List(
        dcache.io.front,  // `dcache` has a `StageLink` inside of it, so we
                          // don't put another `StageLink` before `fMem`
        nMemRegFile,
      )
    ),
    synchronous=true,
  )
  linkArr += fMem

  val sMemRegFile = StageLink(
    up=nMemRegFile,
    down=Node(),
  )
  linkArr += sMemRegFile

  // We can't put an `S2MLink` here because there's not one between 
  // `dcache`'s `cFront` and `cMid0Front`. I guess we don't have *any*
  // `S2MLink`s in this pipeline? No registered `ready` for me I guess....
  // Unfortunate, but there's not much else that can be done about it! At
  // least stalling is partially formally verified...

  val fWbRegFile = ForkLink(
    up=sMemRegFile.down,
    downs=(
      List(
        gprFileEven.io.modBack,
        gprFileOddNonSp.io.modBack,
        gprFileSp.io.modBack,
        sprFile.io.modBack,
      ),
    ),
    synchronous=true,
  )
  val sWbDcache = StageLink(
    up=dcache.mod.front.cMid0Front.down,
    down=dcache.io.modFront,
  )
  linkArr += sWbDcache
  linkArr += DirectLink(
    up=sWbDcache.down,
    down=dcache.io.modBack,
  )

  icache.io.back.ready := True
  gprFileEven.io.back.ready := True
  gprFileOddNonSp.io.back.ready := True
  gprFileSp.io.back.ready := True
  sprFile.io.back.ready := True
  dcache.io.back.ready := True
  //--------
  // actual logic for each pipeline stage goes here
  val cIfArea = new cIf.Area {
    // the `icache` front/`IF` pipeline stage goes here
    when (up.isFiring) {
    }
  }

  val cIdArea = new cId.Area {
    // Most of `ID` pipeline stage goes here.
    when (up.isFiring) {
    }
  }
  val cExArea = new cEx.Area {
    // the `EX` pipeline stage
    when (up.isFiring) {
    }
  }
  val cMemArea = new cMem.Area {
    // The `MEM` pipeline stages
    when (up.isFiring) {
    }
  }

  val nIdGprFileEvenArea = new Area {
    //--------
    def inpPayload = nIdGprFileEvenOutp(pIdOutpGprFileEven)
    def inpModExt = inpPayload.modExt
    def inpExt = inpPayload.myExt
    //--------
    def outpPayload = nIdGprFileEvenOutp(gprFileEven.io.frontPayload)
    //def outpExt = outpPayload.myExt
    //def outpModExt = outpPayload.modExt
    //--------
    val tempOutpPayload = cloneOf(inpPayload)
    outpPayload := tempOutpPayload
    tempOutpPayload := RegNext(tempOutpPayload)
    when (nIdGprFileEvenOutp.isFiring) {
      tempOutpPayload := inpPayload
      tempOutpPayload.myExt.memAddr.allowOverride
      tempOutpPayload.myExt.memAddr(0) := (
        inpModExt.instrDecEtc.gprEvenRaIdx >> 1
      )
      tempOutpPayload.myExt.memAddr(1) := (
        inpModExt.instrDecEtc.gprEvenRbIdx >> 1
      )
    }
  }
  val nIdGprFileOddNonSpArea = new Area {
    //--------
    def inpPayload = nIdGprFileOddNonSpOutp(pIdOutpGprFileOddNonSp)
    def inpModExt = inpPayload.modExt
    def inpExt = inpPayload.myExt
    //--------
    def outpPayload = (
      nIdGprFileOddNonSpOutp(gprFileOddNonSp.io.frontPayload)
    )
    //def outpExt = outpPayload.myExt
    //def outpModExt = outpPayload.modExt
    //--------
    //outpPayload := RegNext(outpPayload)
    val tempOutpPayload = cloneOf(inpPayload)
    outpPayload := tempOutpPayload
    tempOutpPayload := RegNext(tempOutpPayload)
    when (nIdGprFileOddNonSpOutp.isFiring) {
      //--------
      tempOutpPayload := inpPayload
      tempOutpPayload.myExt.memAddr.allowOverride
      tempOutpPayload.myExt.memAddr(0) := (
        inpModExt.instrDecEtc.gprOddNonSpRaIdx >> 1
      )
      tempOutpPayload.myExt.memAddr(1) := (
        inpModExt.instrDecEtc.gprOddNonSpRbIdx >> 1
      )
    }
  }
  val nIdGprFileSpArea = new Area {
    //--------
    def inpPayload = nIdGprFileSpOutp(pIdOutpGprFileSp)
    def inpModExt = inpPayload.modExt
    def inpExt = inpPayload.myExt
    //--------
    def outpPayload = nIdGprFileSpOutp(gprFileSp.io.frontPayload)
    //def outpExt = outpPayload.myExt
    //def outpModExt = outpPayload.modExt
    //--------
    val tempOutpPayload = cloneOf(inpPayload)
    outpPayload := tempOutpPayload
    tempOutpPayload := RegNext(tempOutpPayload)
    //--------
    when (nIdGprFileSpOutp.isFiring) {
      tempOutpPayload := inpPayload
      tempOutpPayload.myExt.memAddr.allowOverride
      tempOutpPayload.myExt.memAddr(0) := 0
      //outpExt.memAddr(1) := inpModExt.instrDecEtc.gprSpRbIdx
    }
  }
  val nIdSprFileArea = new Area {
    //--------
    def inpPayload = nIdSprFileOutp(pIdOutpSprFile)
    def inpModExt = inpPayload.modExt
    def inpExt = inpPayload.myExt
    //--------
    def outpPayload = nIdSprFileOutp(sprFile.io.frontPayload)
    //def outpExt = outpPayload.myExt
    //def outpModExt = outpPayload.modExt
    //--------
    val tempOutpPayload = cloneOf(inpPayload)
    outpPayload := tempOutpPayload
    tempOutpPayload := RegNext(tempOutpPayload)
    //--------
    when (nIdSprFileOutp.isFiring) {
      //--------
      tempOutpPayload := inpPayload
      //outpPayload.myExt := inpPayload.myExt
      //println(s"${outpExt.rdMemWord.size} ${inpExt.rdMemWord.size}")
      //outpExt.rdMemWord := inpExt.rdMemWord
      tempOutpPayload.myExt.memAddr.allowOverride
      tempOutpPayload.myExt.memAddr(0) := inpModExt.instrDecEtc.sprSaIdx
      tempOutpPayload.myExt.memAddr(1) := inpModExt.instrDecEtc.sprSbIdx
      //outpExt.memAddr(1) := inpModExt.instrDecEtc.gprSpRbIdx
    }
  }

  Builder(linkArr.toSeq)
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
