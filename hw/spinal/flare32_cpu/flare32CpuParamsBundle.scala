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

object Flare32CpuCacheParams {
  def defaultNumLinesPow = log2Up(512)        // \ 32 kiB
  def defaultNumBytesPerLinePow = log2Up(64)  // /

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
}

case class Flare32CpuCacheParams(
  mainWidth: Int,
  lineElemNumBytes: Int,
  numLinesPow: Int=Flare32CpuCacheParams.defaultNumLinesPow,
  numBytesPerLinePow: Int=Flare32CpuCacheParams.defaultNumBytesPerLinePow, 
) {
  //def lineIdxRange = log2Up(numBytesPerLine) - 1 downto 0
  def lineIdxRange = (
    //mainWidth - numLinesPow - 1
    numLinesPow + numBytesPerLinePow - 1 
    downto numBytesPerLinePow
  )
  val rawElemNumBytesPow8 = Flare32CpuCacheParams.rawElemNumBytesPow8
  val rawElemNumBytesPow16 = Flare32CpuCacheParams.rawElemNumBytesPow16
  val rawElemNumBytesPow32 = Flare32CpuCacheParams.rawElemNumBytesPow32
  val rawElemNumBytesPow64 = Flare32CpuCacheParams.rawElemNumBytesPow64

  def elemNumBytesPow(
    rawElemNumBytesPow: (Int, Int)
  ) = Flare32CpuCacheParams.elemNumBytesPow(rawElemNumBytesPow)

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

case class Flare32CpuParams(
  //optIncludeSimd: Boolean=false,
  //optIncludeFpu: Boolean=false,
  icacheNumLinesPow: Int=Flare32CpuCacheParams.defaultNumLinesPow,
  icacheNumBytesPerLinePow: Int=(
    Flare32CpuCacheParams.defaultNumBytesPerLinePow
  ),
  icacheRamStyle: String="block",
  icacheRamRwAddrCollision: String="",
  dcacheNumLinesPow: Int=Flare32CpuCacheParams.defaultNumLinesPow,
  dcacheNumBytesPerLinePow: Int=(
    Flare32CpuCacheParams.defaultNumBytesPerLinePow
  ),
  dcacheRamStyle: String="block",
  dcacheRamRwAddrCollision: String="",
  //icacheParams: Flare32CpuCacheParams,
  //dcacheParams: Flare32CpuCacheParams,
  numCpuCores: Int=(
    //2 // at the time of this writing, this does nothing!
    1
  ), 
) {
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
  def nonG3ImmWidth = 5
  def g3ImmWidth = 9
  def preWidth = 12
  def preFullNonG3Width = preWidth + nonG3ImmWidth
  def preFullG3Width = preWidth + g3ImmWidth
  def lpreWidth = 27
  def lpreFullWidth = mainWidth
  def instrEncGrpWidth = 3
  //def instrEncG1G5G6Simm5Width = 5
  //def instrEncG3Simm9Width = 9
  //--------
  val rawElemNumBytesPow8 = Flare32CpuCacheParams.rawElemNumBytesPow8
  val rawElemNumBytesPow16 = Flare32CpuCacheParams.rawElemNumBytesPow16
  val rawElemNumBytesPow32 = Flare32CpuCacheParams.rawElemNumBytesPow32
  val rawElemNumBytesPow64 = Flare32CpuCacheParams.rawElemNumBytesPow64

  def elemNumBytesPow(
    rawElemNumBytesPow: (Int, Int)
  ) = Flare32CpuCacheParams.elemNumBytesPow(rawElemNumBytesPow)
  //--------
  def icacheParams = Flare32CpuCacheParams(
    mainWidth=mainWidth,
    lineElemNumBytes=(instrMainWidth / 8),
    numLinesPow=icacheNumLinesPow,
    numBytesPerLinePow=icacheNumBytesPerLinePow,
  )
  def icacheLineIdxRange = icacheParams.lineIdxRange
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
  //def icacheLineMemWordTypeWidth = instrMainWidth
  //def icacheLineMemWordCount = (
  //  (icacheParams.numLines * icacheParams.numBytesPerLine)
  //  / (instrMainWidth / 8).toInt
  //)
  //--------
  def dcacheParams = Flare32CpuCacheParams(
    mainWidth=mainWidth,
    lineElemNumBytes=(mainWidth / 8),
    numLinesPow=dcacheNumLinesPow,
    numBytesPerLinePow=dcacheNumBytesPerLinePow,
  )
  def dcacheLineIdxRange = dcacheParams.lineIdxRange
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
  //def dcacheLineMemWordTypeWidth = instrMainWidth
  //def dcacheLineMemWordCount = (
  //  (dcacheParams.numLines * dcacheParams.numBytesPerLine)
  //  / (instrMainWidth / 8).toInt
  //)
  //--------
  val ibusParams = BmbParameter(
    addressWidth=mainWidth,
    dataWidth=(
      //instrMainWidth
      //icacheLineElemWidth
      mainWidth
    ),
    sourceWidth=0,
    contextWidth=0,
    lengthWidth=(
      //icacheLineMemWordCount
      //icacheLineMemWordCount
      /*log2Up*/(
        icacheNumBytesPerLinePow
        //- log2Up(mainWidth)
        - log2Up(mainWidth / 8)
      )
    ),
    alignment=BmbParameter.BurstAlignement.WORD,
    alignmentMin=0,
    accessLatencyMin=1,
    canRead=true,
    canWrite=false, // icache only reads
    canExclusive=false,   // I don't know what this is for yet
    maximumPendingTransaction=1,
  )
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
  val dbusParams = BmbParameter(
    addressWidth=mainWidth,
    dataWidth=mainWidth,
    sourceWidth=0,
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
    canExclusive=false,   // I don't know what this is for yet
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
