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

object FlareCpuCacheParams {
  def defaultNumLinesPow = (
    //log2Up(512)        // 32 kiB (if each line is 64 bytes long)
    log2Up(256)         // 16 kiB (if each line is 64 bytes long)
  )
  def defaultNumBytesPerLinePow = log2Up(64)  //

  val rawElemNumBytesPow8 = (8, log2Up(8 / 8))
  val rawElemNumBytesPow16 = (16, log2Up(16 / 8))
  val rawElemNumBytesPow32 = (32, log2Up(32 / 8))
  val rawElemNumBytesPow64 = (64, log2Up(64 / 8))

  def elemNumBytesPow(
    rawElemNumBytesPow: (Int, Int)
  ) = {
    rawElemNumBytesPow._1 match {
      case rawElemNumBytesPow8._1 => {
        //println("test8")
        (rawElemNumBytesPow8._2, 8);
      }
      case rawElemNumBytesPow16._1 => {
        //println("test16")
        (rawElemNumBytesPow16._2, 16);
      }
      case rawElemNumBytesPow32._1 => {
        //println("test32")
        (rawElemNumBytesPow32._2, 32);
      }
      case rawElemNumBytesPow64._1 => {
        //println("test64")
        (rawElemNumBytesPow64._2, 64);
      }
    }
    //if (rawElemNumBytesPow._1 == 8) {
    //  println("test8")
    //  (rawElemNumBytesPow8._2, 8)
    //} else if (rawElemNumBytesPow._1 == 16) {
    //  println("test16")
    //  (rawElemNumBytesPow16._2, 16)
    //} else if (rawElemNumBytesPow._1 == 32) {
    //  println("test32")
    //  (rawElemNumBytesPow32._2, 32)
    //} else /*if (rawElemNumBytesPow._1 == 64)*/ {
    //  println("test64")
    //  (rawElemNumBytesPow64._2, 64)
    //}
  }
  //--------
}

case class FlareCpuCacheParams(
  mainWidth: Int,
  lineElemNumBytes: Int,
  numLinesPow: Int=FlareCpuCacheParams.defaultNumLinesPow,
  numBytesPerLinePow: Int=FlareCpuCacheParams.defaultNumBytesPerLinePow, 
) {
  def dontCacheAddrBit = mainWidth - 1
  //def lineIdxRange = log2Up(numBytesPerLine) - 1 downto 0
  def lineIdxRangePair = (
    numLinesPow + numBytesPerLinePow - 1,
    numBytesPerLinePow
  )

  def lineIdxRange = (
    //mainWidth - numLinesPow - 1
    lineIdxRangePair._1
    downto lineIdxRangePair._2
  )

  def validVecElemWidth = 32
  def validVecRange = (
    lineIdxRangePair._1
    downto (
      lineIdxRangePair._2 + log2Up(validVecElemWidth)
    )
  )
  val rawElemNumBytesPow8 = FlareCpuCacheParams.rawElemNumBytesPow8
  val rawElemNumBytesPow16 = FlareCpuCacheParams.rawElemNumBytesPow16
  val rawElemNumBytesPow32 = FlareCpuCacheParams.rawElemNumBytesPow32
  val rawElemNumBytesPow64 = FlareCpuCacheParams.rawElemNumBytesPow64

  def elemNumBytesPow(
    rawElemNumBytesPow: (Int, Int)
  ) = FlareCpuCacheParams.elemNumBytesPow(rawElemNumBytesPow)

  def lineDataIdxRange(rawElemNumBytesPow: (Int, Int)) = (
    numBytesPerLinePow - 1 downto elemNumBytesPow(rawElemNumBytesPow)._1
  )
  def lineBaseAddrWidth = (
    //mainWidth - numBytesPerLinePow
    //numLinesPow
    mainWidth - (numLinesPow + numBytesPerLinePow)
  )
  def lineBaseAddrRange = (
    mainWidth - 1
    downto numLinesPow + numBytesPerLinePow
  )
  def numLines = (1 << numLinesPow)
  def numBytesPerLine = (1 << numBytesPerLinePow)
  //def icacheLineMemWordCount = (
  //  (numLines * numBytesPerLine)
  //  / (
  //  )
  //)
  //def lineWidth = 1 << lineWidthPow
  def lineElemWidth = lineElemNumBytes * 8
  def lineMemWordType() = UInt(lineElemWidth bits)
  def lineMemWordCount = (
    (numLines * numBytesPerLine) / (lineElemWidth / 8).toInt
  )
}

//case class FlareCpuRegFileParams(
//  params: FlareCpuParams,
//  wordCount: Int,
//) {
//}
object FlareCpuRegFileInfo {
  sealed trait SliceCfg {
    def idx: Int
    def wordCount: Int
  }
  object SliceCfg {
    case object GprEvenNonFp extends SliceCfg {
      def idx: Int = 0
      def wordCount: Int = 7
    }
    case object GprFp extends SliceCfg {
      def idx: Int = 1
      def wordCount: Int = 1
    }
    case object GprOddNonSp extends SliceCfg {
      def idx: Int = 2
      def wordCount: Int = 7
    }
    case object GprSp extends SliceCfg {
      def idx: Int = 3
      def wordCount: Int = 1
    }
    case object SprEven extends SliceCfg {
      def idx: Int = 4
      def wordCount: Int = 8
    }
    case object SprOdd extends SliceCfg {
      def idx: Int = 5
      def wordCount: Int = 8
    }
    def lim = SprOdd.idx + 1
  }
  def modRdPortCnt: Int = 2
  def modStageCnt: Int = 1      // `MEM` is the only "modStage"
  def optModHazardKind: PipeMemRmw.ModHazardKind = (
    PipeMemRmw.ModHazardKind.Fwd
  )
  //--------
  def wordType(
    params: FlareCpuParams
  ) = (
    UInt(params.mainWidth bits)
  )
  def hazardCmpType(
    params: FlareCpuParams
  ) = (
    Bool()
  )
  //def gprFileEvenWordCount = 8
  //def gprFileEvenNonFpWordCount = 7
  //def gprFileFpWordCount = 1
  //def gprFileOddNonSpWordCount = 7
  //def gprFileSpWordCount = 1
  ////def sprFileWordCount = numGprsSprs
  //def sprFileEvenWordCount = 8
  //def sprFileOddWordCount = 8
  //def regFileNonSpModRdPortCnt = 2
  //def gprFileSpModRdPortCnt = 1
  //def regFileModRdPortCnt = 2
  //def regFileModStageCnt = 1      // `MEM` is the only "modStage"
  //def regFileOptModHazardKind: PipeMemRmw.ModHazardKind = (
  //  PipeMemRmw.ModHazardKind.Fwd
  //)
  val pmRmwWordCountArr: ArrayBuffer[Int] = {
    val tempArr = (new ArrayBuffer[Int]()) ++ List[Int](
      FlareCpuRegFileInfo.SliceCfg.GprEvenNonFp.wordCount,
      FlareCpuRegFileInfo.SliceCfg.GprFp.wordCount,
      FlareCpuRegFileInfo.SliceCfg.GprOddNonSp.wordCount,
      FlareCpuRegFileInfo.SliceCfg.GprSp.wordCount,
      //params.sprFileWordCount,
      FlareCpuRegFileInfo.SliceCfg.SprEven.wordCount,
      FlareCpuRegFileInfo.SliceCfg.SprOdd.wordCount,
    )
    //println(s"tempArr.size: ${tempArr.size}")
    tempArr
  }
  //--------
}
sealed trait FlareCpuFormalTest
//  def enumFormalTestNone = 0
//  def enumFormalTestMain = 1
object FlareCpuFormalTest {
  case object Dont extends FlareCpuFormalTest
  case object Main extends FlareCpuFormalTest
}
object FlareCpuParams {
  //--------
  def enumRegFileGprEvenNonFp: Int = (
    //0
    FlareCpuRegFileInfo.SliceCfg.GprEvenNonFp.idx
  )
  def enumRegFileGprFp: Int = (
    FlareCpuRegFileInfo.SliceCfg.GprFp.idx
    //1
  )
  def enumRegFileGprOddNonSp: Int = (
    //2
    FlareCpuRegFileInfo.SliceCfg.GprOddNonSp.idx
  )
  def enumRegFileGprSp: Int = (
    //3
    FlareCpuRegFileInfo.SliceCfg.GprSp.idx
  )
  def enumRegFileSprEven: Int = (
    //4
    FlareCpuRegFileInfo.SliceCfg.SprEven.idx
  )
  def enumRegFileSprOdd: Int = (
    //5
    FlareCpuRegFileInfo.SliceCfg.SprOdd.idx
  )
  def enumRegFileLim: Int = (
    //6
    FlareCpuRegFileInfo.SliceCfg.lim
  )
  //--------
  //--------
  def mkRegFilePmRmwCfg(
    params: FlareCpuParams,
    //wordCountArr: Seq[Int],
    optFormalTest: FlareCpuFormalTest,
    pipeName: String,
    linkArr: Option[ArrayBuffer[Link]]=None,
  ) = (
    PipeMemRmwConfig(
      wordType=FlareCpuRegFileInfo.wordType(params=params),
      wordCountArr=(
        //wordCountArr
        FlareCpuRegFileInfo.pmRmwWordCountArr
      ),
      hazardCmpType=FlareCpuRegFileInfo.hazardCmpType(params=params),
      modRdPortCnt=FlareCpuRegFileInfo.modRdPortCnt,
      modStageCnt=FlareCpuRegFileInfo.modStageCnt,
      pipeName=pipeName,
      linkArr=linkArr,
      optModHazardKind=PipeMemRmw.ModHazardKind.Fwd
    )
  )
  def mkRegFileModType(
    params: FlareCpuParams,
    //wordCountArr: Seq[Int],
    pmRmwCfg: PipeMemRmwConfig[
      UInt,
      Bool,
    ],
    optFormalTest: FlareCpuFormalTest,
    //pipeName: String,
    //linkArr: Option[ArrayBuffer[Link]]=None,
  ) = (
    FlareCpuPipeMemModType(
      params=params,
      pmRmwCfg=pmRmwCfg,
      //mkRegFilePmRmwCfg(
      //  params=params,
      //  wordCountArr=wordCountArr,
      //  optFormalTest=optFormalTest,
      //  pipeName=pipeName,
      //  linkArr=linkArr,
      //),
      ////wordType=params.regWordType(),
      //wordCountMax=params.sprFileEvenWordCount,
      ////hazardCmpType=params.regFileHazardCmpType(),
      ////modRdPortCnt=params.regFileModRdPortCnt,
      ////modStageCnt=params.regFileModStageCnt,
      ////optModHazardKind=params.regFileOptModHazardKind,
      modExtType=FlareCpuPipeMemModExtType(
        params=params,
        optFormalTest=optFormalTest,
      ),
      optFormalTest=optFormalTest,
    )
  )
  //--------
  def enumFormalTestNone: FlareCpuFormalTest = (
    FlareCpuFormalTest.Dont
  )
  def enumFormalTestMain: FlareCpuFormalTest = (
    FlareCpuFormalTest.Main
  )
  //def enumFormalTestPipeMain = 2
  //def enumFormalTestMain = 2
  //--------
}

case class FlareCpuParams(
  //optIncludeSimd: Boolean=false,
  //optIncludeFpu: Boolean=false,
  icacheNumLinesPow: Int=FlareCpuCacheParams.defaultNumLinesPow,
  icacheNumBytesPerLinePow: Int=(
    FlareCpuCacheParams.defaultNumBytesPerLinePow
  ),
  icacheRamStyle: String="block",
  icacheRamRwAddrCollision: String="",
  dcacheNumLinesPow: Int=FlareCpuCacheParams.defaultNumLinesPow,
  dcacheNumBytesPerLinePow: Int=(
    FlareCpuCacheParams.defaultNumBytesPerLinePow
  ),
  dcacheRamStyle: String="block",
  dcacheRamRwAddrCollision: String="",
  //icacheParams: FlareCpuCacheParams,
  //dcacheParams: FlareCpuCacheParams,
  numCpuCores: Int=(
    //2 // at the time of this writing, this does nothing!
    1
  ), 
  //optFormalTest: Int=(
  //  FlareCpuParams.enumFormalTestNone
  //),
) {
  //def formal(): Boolean = {
  //  GenerationFlags.formal {
  //    return true
  //  }
  //  return false
  //}
  //--------
  //def regWordType() = UInt(mainWidth bits)
  ////def gprFileEvenWordCount = 8
  //def gprFileEvenNonFpWordCount = 7
  //def gprFileFpWordCount = 1
  //def gprFileOddNonSpWordCount = 7
  //def gprFileSpWordCount = 1
  ////def sprFileWordCount = numGprsSprs
  //def sprFileEvenWordCount = 8
  //def sprFileOddWordCount = 8
  //def regFileHazardCmpType() = Bool()
  ////def regFileNonSpModRdPortCnt = 2
  ////def gprFileSpModRdPortCnt = 1
  //def regFileModRdPortCnt = 2
  //def regFileModStageCnt = 1      // `MEM` is the only "modStage"
  //def regFileOptModHazardKind: PipeMemRmw.ModHazardKind = (
  //  PipeMemRmw.ModHazardKind.Fwd
  //)
  def cacheOptModHazardKind: PipeMemRmw.ModHazardKind = (
    PipeMemRmw.ModHazardKind.Fwd
  )
  //--------
  def flagIdxZ = 0
  def flagIdxC = 1
  def flagIdxV = 2
  def flagIdxN = 3
  //--------
  //def busParams = tilelink.BusParameter.simple(
  //  addressWidth=32,
  //  dataWidth=32,
  //  sizeBytes=32,
  //  sourceWidth=4,
  //  //sinkWidth=0,
  //  //withBCE=true,
  //  //withDataA=false,
  //  //withDataB=true,
  //  //withDataC=false,
  //  //withDataD=true,
  //  //node=null,
  //)
  def mainWidth = 32
  def instrMainWidth = 16
  def numGprsSprs = 16
  def numGprsSprsPow = log2Up(numGprsSprs)
  def g1g7ImmWidth = 5
  def g3ImmWidth = 9
  def g5ImmWidth = 7
  def preWidth = 12
  def preFullNonG3Width = preWidth + g1g7ImmWidth
  def preFullG3Width = preWidth + g3ImmWidth
  def lpreWidth = 27
  def lpreFullWidth = mainWidth
  def instrEncGrpWidth = 3
  def g7Sg01110BlankWidth = 8
  //def instrEncG1G5G6Simm5Width = 5
  //def instrEncG3Simm9Width = 9
  //--------
  val rawElemNumBytesPow8 = FlareCpuCacheParams.rawElemNumBytesPow8
  val rawElemNumBytesPow16 = FlareCpuCacheParams.rawElemNumBytesPow16
  val rawElemNumBytesPow32 = FlareCpuCacheParams.rawElemNumBytesPow32
  val rawElemNumBytesPow64 = FlareCpuCacheParams.rawElemNumBytesPow64

  def elemNumBytesPow(
    rawElemNumBytesPow: (Int, Int)
  ) = FlareCpuCacheParams.elemNumBytesPow(rawElemNumBytesPow)
  //--------
  def icacheParams = FlareCpuCacheParams(
    mainWidth=mainWidth,
    lineElemNumBytes=(instrMainWidth / 8),
    numLinesPow=icacheNumLinesPow,
    numBytesPerLinePow=icacheNumBytesPerLinePow,
  )
  def icacheLineIdxRangePair = icacheParams.lineIdxRangePair
  def icacheLineIdxRange = icacheParams.lineIdxRange
  def icacheValidVecElemWidth = icacheParams.validVecElemWidth
  def icacheValidVecRange = icacheParams.validVecRange
  def icacheLineDataIdxRange(
    //rawElemNumBytesPow: (Int, Int)=icacheParams.rawElemNumBytesPow16
  ) = (
    icacheParams.lineDataIdxRange(
      //rawElemNumBytesPow=rawElemNumBytesPow
      rawElemNumBytesPow=icacheParams.rawElemNumBytesPow16
    )
  )
  def icacheLineBaseAddrWidth = icacheParams.lineBaseAddrWidth
  def icacheLineBaseAddrRange = icacheParams.lineBaseAddrRange
  def icacheNumLines = icacheParams.numLines
  def icacheNumBytesPerLine = icacheParams.numBytesPerLine
  def icacheLineElemWidth = icacheParams.lineElemWidth
  def icacheLineMemWordType() = icacheParams.lineMemWordType()
  def icacheLineMemWordCount = icacheParams.lineMemWordCount
  def icacheModRdPortCnt = 1
  def icacheOptModHazardKind: PipeMemRmw.ModHazardKind = (
    PipeMemRmw.ModHazardKind.Fwd
  )
  def icacheModStageCnt = 0
  //def icacheLineMemWordTypeWidth = instrMainWidth
  //def icacheLineMemWordCount = (
  //  (icacheParams.numLines * icacheParams.numBytesPerLine)
  //  / (instrMainWidth / 8).toInt
  //)
  //--------
  def dcacheParams = FlareCpuCacheParams(
    mainWidth=mainWidth,
    lineElemNumBytes=(mainWidth / 8),
    numLinesPow=dcacheNumLinesPow,
    numBytesPerLinePow=dcacheNumBytesPerLinePow,
  )
  //def dcacheValidVecRange = (
  //  dcacheLineIdxRangePair._1
  //  downto (
  //    dcacheLineIdxRangePair._2 + dcacheParams.validVecElemWidth
  //  )
  //)
  def dcacheLineIdxRangePair = dcacheParams.lineIdxRangePair
  def dcacheLineIdxRange = dcacheParams.lineIdxRange
  def dcacheValidVecElemWidth = dcacheParams.validVecElemWidth
  def dcacheValidVecRange = dcacheParams.validVecRange
  def dcacheLineDataIdxRange(
    rawElemNumBytesPow: (Int, Int)
  ) = (
    dcacheParams.lineDataIdxRange(rawElemNumBytesPow=rawElemNumBytesPow)
  )
  def dcacheLineBaseAddrWidth = dcacheParams.lineBaseAddrWidth
  def dcacheLineBaseAddrRange = dcacheParams.lineBaseAddrRange
  def dcacheNumLines = dcacheParams.numLines
  def dcacheNumBytesPerLine = dcacheParams.numBytesPerLine
  def dcacheLineElemWidth = dcacheParams.lineElemWidth
  def dcacheLineMemWordType() = dcacheParams.lineMemWordType()
  def dcacheLineMemWordCount = dcacheParams.lineMemWordCount
  def dcacheModRdPortCnt = 1
  def dcacheOptModHazardKind: PipeMemRmw.ModHazardKind = (
    PipeMemRmw.ModHazardKind.Fwd
  )
  def dcacheModStageCnt = 0
  //def dcacheLineMemWordTypeWidth = instrMainWidth
  //def dcacheLineMemWordCount = (
  //  (dcacheParams.numLines * dcacheParams.numBytesPerLine)
  //  / (instrMainWidth / 8).toInt
  //)
  //--------
  //val ibusParams = BmbParameter(
  //  addressWidth=mainWidth,
  //  dataWidth=(
  //    //instrMainWidth
  //    //icacheLineElemWidth
  //    mainWidth
  //  ),
  //  sourceWidth=0,
  //  contextWidth=0,
  //  lengthWidth=(
  //    //icacheLineMemWordCount
  //    //icacheLineMemWordCount
  //    /*log2Up*/(
  //      icacheNumBytesPerLinePow
  //      //- log2Up(mainWidth)
  //      - log2Up(mainWidth / 8)
  //    )
  //  ),
  //  alignment=BmbParameter.BurstAlignement.WORD,
  //  alignmentMin=0,
  //  accessLatencyMin=1,
  //  canRead=true,
  //  canWrite=false, // icache only reads
  //  canExclusive=false,   // I don't know what this is for yet
  //  maximumPendingTransaction=1,
  //)

  //def ibusConfig = Axi4Config(
  //  addressWidth=mainWidth,
  //  dataWidth=instrMainWidth,
  //  idWidth=0,
  //  useId=false,
  //  useRegion=false,
  //  useBurst=true,
  //  useLock=false,
  //  useCache=false,
  //  useSize=false,
  //  useQos=false,
  //  useLen=false,
  //  useLast=false,
  //  useResp=true,
  //  useProt=false,
  //  useStrb=false, // icache only reads
  //  useAllStrb=false,
  //)
  val busParams = BmbParameter(
    addressWidth=mainWidth,
    dataWidth=mainWidth,
    sourceWidth=1,
    contextWidth=0,
    lengthWidth=(
      //dcacheLineMemWordCount
      //icacheParams.numBytesPerLine / params.instrMainWidth
      /*log2Up*/(
        dcacheNumBytesPerLinePow
        - log2Up(mainWidth / 8)
      )
    ),
    alignment=BmbParameter.BurstAlignement.WORD,
    alignmentMin=0,
    accessLatencyMin=1,
    canRead=true,
    canWrite=true,
    canExclusive=false,
      // Current Note: `canExclusive` is for atomics!
      // OLD Note: I don't know what `canExclusive` is for yet
    maximumPendingTransaction=1,
  )
  //def dbusConfig = Axi4Config(
  //  addressWidth=mainWidth,
  //  dataWidth=mainWidth,
  //  idWidth=0,
  //  useId=false,
  //  useRegion=false,
  //  useBurst=true,
  //  useLock=false,
  //  useCache=false,
  //  useSize=false,
  //  useQos=false,
  //  useLen=false,
  //  useLast=false,
  //  useResp=true,
  //  useProt=false,
  //  useStrb=true,
  //  useAllStrb=false,
  //)
  //--------
  //val busConfig = AvalonMMConfig(
  //  addressWidth=mainWidth,
  //  dataWidth=mainWidth,
  //  burstCountWidth=
  //)
  //val test = AvalonMMInterconnect(
  //)
}
