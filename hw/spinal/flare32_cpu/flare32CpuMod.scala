package flare32_cpu
import spinal.core._
//import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.avalon._
//import spinal.lib.bus.tilelink
//import spinal.core.fiber.Fiber
import scala.collection.mutable.ArrayBuffer
import libcheesevoyage.general._
import libcheesevoyage.general.PipeSkidBuf
import libcheesevoyage.general.PipeSkidBufIo
//import libcheesevoyage.general.PipeSimpleDualPortMem
import libcheesevoyage.general.FpgacpuRamSimpleDualPort
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
  ) = rawElemNumBytesPow match {
    case (rawElemNumBytesPow8._1, rawElemNumBytesPow8._2) =>
      (rawElemNumBytesPow8._2, 8);
    case (rawElemNumBytesPow16._1, rawElemNumBytesPow16._2) =>
      (rawElemNumBytesPow16._2, 16);
    case (rawElemNumBytesPow32._1, rawElemNumBytesPow32._2) =>
      (rawElemNumBytesPow32._2, 32);
    case (rawElemNumBytesPow64._1, rawElemNumBytesPow64._2) =>
      (rawElemNumBytesPow64._2, 64);
  }
}

case class Flare32CpuCacheParams(
  mainWidth: Int,
  lineElemNumBytes: Int,
  numLinesPow: Int=Flare32CpuCacheParams.defaultNumLinesPow,
  numBytesPerLinePow: Int=Flare32CpuCacheParams.defaultNumBytesPerLinePow, 
) {
  //def lineIndexRange = log2Up(numBytesPerLine) - 1 downto 0
  def lineIndexRange = (
    mainWidth - numLinesPow - 1
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
  def lineBaseAddrWidth = mainWidth - numBytesPerLinePow
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
  numCpuCores: Int=2,
) {
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
  def icacheLineIndexRange = icacheParams.lineIndexRange
  def icacheLineDataIdxRange(
    rawElemNumBytesPow: (Int, Int)=icacheParams.rawElemNumBytesPow16
  ) = (
    icacheParams.lineDataIdxRange(
      //rawElemNumBytesPow=rawElemNumBytesPow
      rawElemNumBytesPow=icacheParams.rawElemNumBytesPow16
    )
  )
  def icacheLineBaseAddrWidth = icacheParams.lineBaseAddrWidth
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
  def dcacheLineIndexRange = dcacheParams.lineIndexRange
  def dcacheLineDataIdxRange(
    rawElemNumBytesPow: (Int, Int)
  ) = (
    dcacheParams.lineDataIdxRange(rawElemNumBytesPow=rawElemNumBytesPow)
  )
  def dcacheLineBaseAddrWidth = dcacheParams.lineBaseAddrWidth
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
  def ibusConfig = Axi4Config(
    addressWidth=mainWidth,
    dataWidth=instrMainWidth,
    idWidth=0,
    useId=false,
    useRegion=false,
    useBurst=true,
    useLock=false,
    useCache=false,
    useSize=false,
    useQos=false,
    useLen=false,
    useLast=false,
    useResp=true,
    useProt=false,
    useStrb=false, // icache only reads
    useAllStrb=false,
  )
  def dbusConfig = Axi4Config(
    addressWidth=mainWidth,
    dataWidth=mainWidth,
    idWidth=0,
    useId=false,
    useRegion=false,
    useBurst=true,
    useLock=false,
    useCache=false,
    useSize=false,
    useQos=false,
    useLen=false,
    useLast=false,
    useResp=true,
    useProt=false,
    useStrb=true,
    useAllStrb=false,
  )
  //--------
  //val busConfig = AvalonMMConfig(
  //  addressWidth=mainWidth,
  //  dataWidth=mainWidth,
  //  burstCountWidth=
  //)
  //val test = AvalonMMInterconnect(
  //)
}
case class Flare32CpuIcacheLineAttrs(
  params: Flare32CpuParams
) extends Bundle {
  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
  val loaded = Bool()
}
case class Flare32CpuDcacheLineAttrs(
  params: Flare32CpuParams
) extends Bundle {
  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
  val loaded = Bool()
  val dirty = Bool()
}
//case class Flare32CpuIcacheWordType(
//  params: Flare32CpuParams
//) extends Bundle {
//  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
//  val data = UInt((params.icacheNumBytesPerLine * 8) bits)
//  val loaded = Bool()
//}
//case class Flare32CpuDcacheWordType(
//  params: Flare32CpuParams
//) extends Bundle {
//  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
//  val data = UInt((params.dcacheNumBytesPerLine * 8) bits)
//  val loaded = Bool()
//  val dirty = Bool()
//}

//case class Flare32CpuIcacheIo(
//  params: Flare32CpuParams
//) extends Bundle with IMasterSlave {
//  //--------
//  val rdAddrStm = slave Stream(
//    UInt(params.mainWidth bits)
//  )
//  val rdDataStm = master Stream(
//    UInt(params.instrMainWidth bits)
//  )
//  //--------
//  def asMaster(): Unit = {
//    master(rdAddrStm)
//    slave(rdDataStm)
//  }
//  //--------
//}

//case class Flare32CpuIcachePayload(
//  params: Flare32CpuParams,
//) extends Bundle {
//}
//
//case class Flare32CpuIcacheCpuIo(
//  params: Flare32CpuParams,
//) extends Bundle {
//}
//
//case class Flare32CpuIcacheIo(
//  params: Flare32CpuParams,
//) extends Bundle {
//  //--------
//  // AXI host
//  val ibus = master(Axi4(config=params.ibusConfig))
//  //--------
//  val cpuIo = Flare32CpuIcacheCpuIo(params=params)
//  //--------
//}
//case class Flare32CpuIcache(
//  params: Flare32CpuParams,
//) extends Component {
//}

//case class Flare32CpuDcacheIo(
//  params: Flare32CpuParams,
//) extends Bundle {
//  //--------
//  // AXI host
//  val dbus = master(Axi4(config=params.dbusConfig))
//  //--------
//}
case class Flare32CpuIo(
  params: Flare32CpuParams
) extends Bundle {
  //--------
  //--------
  //val bus = 
  // Instruction Cache Bus
  //val ibus = master(tilelink.Bus(Flare32CpuParams.busParams))
  val ibus = master(Axi4(config=params.ibusConfig))

  // Data Cache Bus
  //val dbus = master(tilelink.Bus(Flare32CpuParams.busParams))
  val dbus = master(Axi4(config=params.dbusConfig))

  val irq = in Bool()
  //--------
}
case class Flare32Cpu(
  //clkRate: HertzNumber,
  //optIncludeSimd: Boolean=false,
  //optIncludeFpu: Boolean=false,
  params: Flare32CpuParams,
) extends Component {
  //--------
  val io = Flare32CpuIo(params=params)
  def mainWidth = params.mainWidth
  def numGprsSprs = params.numGprsSprs
  def numGprsSprsPow = params.numGprsSprsPow
  def nonG3ImmWidth = params.nonG3ImmWidth
  def g3ImmWidth = params.g3ImmWidth
  def preWidth = params.preWidth
  def preFullNonG3Width = params.preFullNonG3Width
  def preFullG3Width = params.preFullG3Width
  def lpreWidth = params.lpreWidth
  def lpreFullWidth = params.lpreFullWidth
  def instrEncGrpWidth = params.instrEncGrpWidth
  //--------
  //io.bus.a.address := 3
  //--------
  // Pipeline: IF -> ID -> EX
  //--------
  val icache = new Area {
    //val baseAddrMem = Mem(
    //  wordType=UInt(params.icacheLineBaseAddrWidth bits),
    //  wordCount=params.icacheNumLines
    //)
    //  .addAttribute("ramstyle", params.icacheRamStyle)
    //  .addAttribute("ram_style", params.icacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
    val attrsMem = Mem(
      wordType=Flare32CpuIcacheLineAttrs(params=params),
      wordCount=params.icacheNumLines,
    )
      .addAttribute("ramstyle", params.icacheRamStyle)
      .addAttribute("ram_style", params.icacheRamStyle)
      .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
    val lineMem = Mem(
      //wordType=UInt(params.instrMainWidth bits),
      //wordCount=params.icacheNumLines
      //wordType=UInt((params.icacheNumBytesPerLine * 8) bits),
      //wordCount=params.icacheNumLines,
      wordType=params.icacheLineMemWordType(),
      wordCount=params.icacheLineMemWordCount,
    )
      .addAttribute("ramstyle", params.icacheRamStyle)
      .addAttribute("ram_style", params.icacheRamStyle)
      .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
    //val loadedMem = Mem(
    //  wordType=Bool(),
    //  wordCount=params.icacheNumLines,
    //)
    //  .addAttribute("ramstyle", params.icacheRamStyle)
    //  .addAttribute("ram_style", params.icacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
    //val mem = Mem(
    //  wordType=Flare32CpuIcacheWordType(params=params),
    //  wordCount=params.icacheNumLines
    //)
    //  .addAttribute("ramstyle", params.icacheRamStyle)
    //  .addAttribute("ram_style", params.icacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.icacheRamRwAddrCollision)
    //val mem = FpgacpuRamSimpleDualPort(
    //  wordType=Flare32CpuIcacheWordType(params=params),
    //  depth=params.icacheNumLines,
    //  initBigInt={
    //    val tempInitBigInt = new ArrayBuffer[BigInt]()
    //    for (idx <- 0 until params.icacheNumLines) {
    //      tempInitBigInt += BigInt(0)
    //    }
    //    Some(tempInitBigInt)
    //  },
    //  arrRamStyle="block",
    //  arrRwAddrCollision=""
    //)
    //def readAsyncU16(
    //  addr: UInt,
    //  enable: Bool,
    //): UInt = {
    //  mem.readAsync(
    //    address=addr(params.icacheLineIndexRange),
    //    enable=enable,
    //  ).data(params.icacheLineDataIdxRange())
    //}
    def sliceData(
      data: UInt,
      //elemNumBytesPow: Int=log2Up(params.instrMainWidth / 8),
      rawElemNumBytesPow: (Int, Int)
        =params.icacheParams.rawElemNumBytesPow16,
    ) = (
      data(params.icacheLineDataIdxRange(
        rawElemNumBytesPow=rawElemNumBytesPow
      ))
    )
    //def rawReadSyncU16(
    //  addr: UInt,
    //  enable: Bool,
    //): UInt = {
    //  mem.readSync(
    //    address=addr(params.icacheLineIndexRange),
    //    enable=enable,
    //  ).data(params.icacheLineDataIdxRange())
    //}
    ////def flush(): Unit = {
    ////}
    //def rawWrite(
    //  addr: UInt,
    //  enable: Bool,
    //  data: UInt,
    //): Unit = {
    //  //mem.write(
    //  //  address=addr,
    //  //  enable=enable,
    //  //  data=data,
    //  //)
    //}
  }

  val dcache = new Area {
    //val mem = Mem(
    //  wordType=Flare32CpuDcacheWordType(params=params),
    //  wordCount=params.dcacheNumLines,
    //)
    //val mem = Mem(
    //  wordType=Flare32CpuDcacheWordType(params=params),
    //  wordCount=params.dcacheNumLines
    //)
    //  .addAttribute("ramstyle", params.dcacheRamStyle)
    //  .addAttribute("ram_style", params.dcacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
    //val baseAddrMem = Mem(
    //  wordType=UInt(params.dcacheLineBaseAddrWidth bits),
    //  wordCount=params.dcacheNumLines,
    //)
    //  .addAttribute("ramstyle", params.dcacheRamStyle)
    //  .addAttribute("ram_style", params.dcacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
    val attrsMem = Mem(
      wordType=Flare32CpuDcacheLineAttrs(params=params),
      //wordType=UInt(params.instrMainWidth bits),
      wordCount=params.dcacheNumLines,
    )
      .addAttribute("ramstyle", params.dcacheRamStyle)
      .addAttribute("ram_style", params.dcacheRamStyle)
      .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
    val lineMem = Mem(
      //wordType=UInt((params.dcacheNumBytesPerLine * 8) bits),
      //wordCount=params.dcacheNumLines,
      wordType=params.dcacheLineMemWordType(),
      wordCount=params.dcacheLineMemWordCount,
    )
      .addAttribute("ramstyle", params.dcacheRamStyle)
      .addAttribute("ram_style", params.dcacheRamStyle)
      .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
    //val loadedMem = Mem(
    //  wordType=Bool(),
    //  //wordType=UInt(params.instrMainWidth bits),
    //  wordCount=params.dcacheNumLines,
    //)
    //  .addAttribute("ramstyle", params.dcacheRamStyle)
    //  .addAttribute("ram_style", params.dcacheRamStyle)
    //  .addAttribute("rw_addr_collision", params.dcacheRamRwAddrCollision)
    //def read(): Unit = {
    //}
    def sliceData(
      data: UInt,
      //elemNumBytesPow: Int,
      rawElemNumBytesPow: (Int, Int)
    ) = {
      data(params.dcacheLineDataIdxRange(
        rawElemNumBytesPow=rawElemNumBytesPow
      ))
    }
  }
  //val icache = Icache()
  
  //case class PsIfPayload() extends Bundle {
  //  val irq = Bool()
  //  val pc = UInt(mainWidth bits)
  //  val pcPlus2 = UInt(mainWidth bits)
  //  val psExSetPc = Flow(UInt(mainWidth bits))
  //}
  //case class InstrEncConst(
  //  value: UInt,
  //  rangeLo: Int,
  //  //range: Range,
  //) {
  //  def range = value.high + rangeLo downto rangeLo
  //}
  object InstrEncConst {
    //--------
    // Instruction Group 0
    val g0Grp = U"3'd0"
    val g0PreSubgrp = U"1'b0"
    val g0LpreSubgrp = U"2'b10"
    //--------
    // Instruction Group 1
    val g1Grp = U"3'd1"
    //--------
    // Instruction Group 2
    val g2Grp = U"3'd2"
    //--------
    // Instruction Group 3
    val g3Grp = U"3'd3"
    //--------
    // Instruction Group 4
    val g4Grp = U"3'd4"
    //--------
    // Instruction Group 5
    val g5Grp = U"3'd5"
    //--------
    // Instruction Group 6
    val g6Grp = U"3'd6"
    //--------
    // Instruction Group 7
    val g7Grp = U"3'd7"
    val g7Sg00Subgrp = U"2'b00"
    val g7Sg010Subgrp = U"3'b010"
    val g7Sg0110Subgrp = U"4'b0110"
    //--------
    def gprIdxR0 = 0
    def gprIdxR1 = 1
    def gprIdxR2 = 2
    def gprIdxR3 = 3
    def gprIdxR4 = 4
    def gprIdxR5 = 5
    def gprIdxR6 = 6
    def gprIdxR7 = 7
    def gprIdxR8 = 8
    def gprIdxR9 = 9
    def gprIdxR10 = 10
    def gprIdxR11 = 11
    def gprIdxR12 = 12
    def gprIdxLr = 13
    def gprIdxFp = 14
    def gprIdxSp = 15

    def sprIdxFlags = 0
    def sprIdxIds = 1
    def sprIdxIra = 2
    def sprIdxIe = 3
    def sprIdxIty = 4
    def sprIdxSty = 5
    def sprIdxS6 = 6
    def sprIdxS7 = 7
    def sprIdxS8 = 8
    def sprIdxS9 = 9
    def sprIdxS10 = 10
    def sprIdxS11 = 11
    def sprIdxS12 = 12
    def sprIdxS13 = 13
    def sprIdxS14 = 14
    def sprIdxS15 = 15
    //--------
  }
  case class InstrG0EncPre() extends Bundle {
    val grp = UInt(InstrEncConst.g0Grp.getWidth bits)
    val subgrp = UInt(InstrEncConst.g0PreSubgrp.getWidth bits)
    def fullgrp = Cat(grp, subgrp)
    val simm = UInt(preWidth bits)
  }
  case class InstrG0EncLpreHi() extends Bundle {
    val grp = UInt(InstrEncConst.g0Grp.getWidth bits)
    val subgrp = UInt(InstrEncConst.g0LpreSubgrp.getWidth bits)
    def fullgrp = Cat(grp, subgrp)
    val simm = UInt(lpreWidth bits)
  }
  //case class PsIdPayload() extends Bundle {
  //  val preInfo = PrefixInfo(dataWidth=preWidth, isLpre=false)
  //  val lpreInfo = PrefixInfo(dataWidth=lpreWidth, isLpre=true)
  //  val indexInfo = PrefixInfo(dataWidth=numGprsPow, isLpre=false)
  //}
  object InstrG1EncOp extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      addRaS5,    // Opcode 0x0: add rA, #simm5
      addRaPcS5,  // Opcode 0x1: add rA, pc, #simm5
      addRaSpS5,  // Opcode 0x2: add rA, sp, #simm5
      addRaFpS5,  // Opcode 0x3: add rA, fp, #simm5
      cmpRaS5,    // Opcode 0x4: cmp rA, #simm5
      cpyRaS5,    // Opcode 0x5: cpy rA, #simm5
      lslRaI5,    // Opcode 0x6: lsl rA, #imm5
      lsrRaI5,    // Opcode 0x7: lsr rA, #imm5
      asrRaI5,    // Opcode 0x8: asr rA, #imm5
      andRaS5,    // Opcode 0x9: and rA, #simm5
      orrRaS5,    // Opcode 0xa: orr rA, #simm5
      xorRaS5,    // Opcode 0xb: xor rA, #simm5
      zeRaI5,     // Opcode 0xc: ze rA, #imm5
      seRaI5,     // Opcode 0xd: se rA, #imm5
      swiRaS5,    // Opcode 0xe: swi rA, #simm5
      swiI5      // Opcode 0xf: swi #simm5
      = newElement();
  }
  case class InstrG1Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g1Grp.getWidth bits)
    // immediate or signed immediate, depending on the opcode
    val imm = UInt(nonG3ImmWidth bits) 
    val op = InstrG1EncOp()
    val raIdx = UInt(numGprsSprsPow bits)
  }
  object InstrG2EncOp extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      addRaRb,      // Opcode 0x0: add rA, rB
      subRaRb,      // Opcode 0x1: sub rA, rB
      addRaSpRb,    // Opcode 0x2: add rA, sp, rB
      addRaFpRb,    // Opcode 0x3: add rA, fp, rB
      cmpRaRb,      // Opcode 0x4: cmp rA, rB
      cpyRaRb,      // Opcode 0x5: cpy rA, rB
      lslRaRb,      // Opcode 0x6: lsl rA, rB
      lsrRaRb,      // Opcode 0x7: lsr rA, rB
      asrRaRb,      // Opcode 0x8: asr rA, rB
      andRaRb,      // Opcode 0x9: and rA, rB
      orrRaRb,      // Opcode 0xa: orr rA, rB
      xorRaRb,      // Opcode 0xb: xor rA, rB
      adcRaRb,      // Opcode 0xc: adc rA, rB
      sbcRaRb,      // Opcode 0xd: sbc rA, rB
      cmpbcRaRb,    // Opcode 0xe: cmpbc rA, rB
      invalid0      // Opcode 0xf: invalid operation 0
      = newElement();
  }
  case class InstrG2Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g2Grp.getWidth bits)
    val f = Bool()
    val op = InstrG2EncOp()
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }
  object InstrG3EncOp extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      blS9,       // Opcode 0x0: bl simm9
      braS9,      // Opcode 0x1: bra simm9
      beqS9,      // Opcode 0x2: beq simm9
      bneS9,      // Opcode 0x3: bne simm9
      bmiS9,      // Opcode 0x4: bmi simm9
      bplS9,      // Opcode 0x5: bpl simm9
      bvsS9,      // Opcode 0x6: bvs simm9
      bvcS9,      // Opcode 0x7: bvc simm9
      bgeuS9,     // Opcode 0x8: bgeu simm9
      bltuS9,     // Opcode 0x9: bltu simm9
      bgtuS9,     // Opcode 0xa: bgtu simm9
      bleuS9,     // Opcode 0xb: bleu simm9
      bgesS9,     // Opcode 0xc: bges simm9
      bltsS9,     // Opcode 0xd: blts simm9
      bgtsS9,     // Opcode 0xe: bgts simm9
      blesS9      // Opcode 0xf: bles simm9
      = newElement();
  }
  case class InstrG3Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g3Grp.getWidth bits)
    val simm = SInt(g3ImmWidth bits)
    val op = InstrG3EncOp()
  }
  object InstrG4EncOp extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      //--------
      jlRa,         // Opcode 0x0: jl rA
      jmpRa,        // Opcode 0x1: jmp rA
      jmpIra,       // Opcode 0x2: jmp ira
      reti,         // Opcode 0x3: reti
      ei,           // Opcode 0x4: ei
      di,           // Opcode 0x5: di
      pushRaRb,     // Opcode 0x6: push rA, rB
      pushSaRb,     // Opcode 0x7: push sA, rB
      popRaRb,      // Opcode 0x8: pop rA, rB
      popSaRb,      // Opcode 0x9: pop sA, rB
      popPcRb,      // Opcode 0xa: pop pc, rB
      mulRaRb,      // Opcode 0xb: mul rA, rB
      udivRaRb,     // Opcode 0xc: udiv rA, rB
      sdivRaRb,     // Opcode 0xd: sdiv rA, rB
      umodRaRb,     // Opcode 0xe: umod rA, rB
      smodRaRb,     // Opcode 0xf: smod rA, rB
      //--------
      lumulRaRb,    // Opcode 0x10: lumul rA, rB
      lsmulRaRb,    // Opcode 0x11: lsmul rA, rB
      udiv64RaRb,   // Opcode 0x12: udiv64 rA, rB
      sdiv64RaRb,   // Opcode 0x13: sdiv64 rA, rB
      umod64RaRb,   // Opcode 0x14: umod64 rA, rB
      smod64RaRb,   // Opcode 0x15: smod64 rA, rB
      ldubRaRb,     // Opcode 0x16: ldub rA, [rB]
      ldsbRaRb,     // Opcode 0x17: ldsb rA, [rB]
      lduhRaRb,     // Opcode 0x18: lduh rA, [rB]
      ldshRaRb,     // Opcode 0x19: ldsh rA, [rB]
      stbRaRb,      // Opcode 0x1a: stb rA, [rB]
      sthRaRb,      // Opcode 0x1b: sth rA, [rB]
      cpyRaSb,      // Opcode 0x1c: cpy rA, sB
      cpySaRb,      // Opcode 0x1d: cpy sA, rB
      cpySaSb,      // Opcode 0x1e: cpy sA, sB
      indexRa       // Opcode 0x1f: index rA
      //--------
      = newElement();
  }
  case class InstrG4Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g4Grp.getWidth bits)
    val op = InstrG4EncOp()
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }
  case class InstrG5Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g5Grp.getWidth bits)
    val simm = SInt(nonG3ImmWidth bits)
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }
  case class InstrG6Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g6Grp.getWidth bits)
    val simm = SInt(nonG3ImmWidth bits)
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }

  // this includes the `w` bit (since it's contiguous with the opcode field)
  object InstrG7Sg00FullOpEnc extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      cmpbRaRb,     // Opcode 0b000: cmpb rA, rB
      lsrbRaRb,     // Opcode 0b001: lsrb rA, rB
      asrbRaRb,     // Opcode 0b010: asrb rA, rB
      invalid0,     // Opcode 0b011: invalid 0
      cmphRaRb,     // Opcode 0b100: cmph rA, rB
      lsrhRaRb,     // Opcode 0b101: lsrh rA, rB
      asrhRaRb,     // Opcode 0b110: asrh rA, rB
      invalid1      // Opcode 0b111: invalid 1
      = newElement();
  }
  case class InstrG7Sg00Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
    val subgrp = UInt(InstrEncConst.g7Sg00Subgrp.getWidth bits)
    def fullgrp = Cat(grp, subgrp)
    val fullop = InstrG7Sg00FullOpEnc()
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }
  object InstrG7Sg010EncOp extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      ldrSaRb,      // Opcode 0x0: ldr sA, [rB]
      ldrSaSb,      // Opcode 0x1: ldr sA, [sB]
      strSaRb,      // Opcode 0x2: str sA, [rB]
      strSaSb       // Opcode 0x3: str sA, [sB]
      = newElement();
  }
  case class InstrG7Sg010Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
    val subgrp = UInt(InstrEncConst.g7Sg010Subgrp.getWidth bits)
    def fullgrp = Cat(grp, subgrp)
    val op = InstrG7Sg010EncOp()
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
  }

  case class InstrG7Sg0110Enc() extends Bundle {
    val grp = UInt(InstrEncConst.g7Grp.getWidth bits)
    val subgrp = UInt(InstrEncConst.g7Sg0110Subgrp.getWidth bits)
    def fullgrp = Cat(grp, subgrp)
  }

  object InstrFullgrpDec extends SpinalEnum(
    defaultEncoding=binarySequential
  ) {
    val
      g0Pre,
      g0Lpre,
      g1,
      g2,
      g3,
      g4,
      g5,
      g6,
      g7Sg00,
      g7Sg010,
      g7Sg0110
      = newElement();
  }
  case class InstrDec() extends Bundle {
    //--------
    val fullgrp = InstrFullgrpDec()
    val fullSimm = UInt(mainWidth bits)
    val fullImm = UInt(mainWidth bits)
    val rcIdx = UInt(numGprsSprsPow bits)
    val rbIdx = UInt(numGprsSprsPow bits)
    val raIdx = UInt(numGprsSprsPow bits)
    //--------
    val instrG0PreEnc = InstrG0EncPre()
    val instrG0LpreHiEnc = InstrG0EncLpreHi()
    val instrG1Enc = InstrG1Enc()
    val instrG2Enc = InstrG2Enc()
    val instrG3Enc = InstrG3Enc()
    val instrG4Enc = InstrG4Enc()
    val instrG5Enc = InstrG5Enc()
    val instrG6Enc = InstrG6Enc()
    val instrG7Sg00Enc = InstrG7Sg00Enc()
    val instrG7Sg010Enc = InstrG7Sg010Enc()
    val instrG7Sg0110Enc = InstrG7Sg0110Enc()
    //--------
  }
  //object InstrG7Sg0110Op extends SpinalEnum(
  //  defaultEncoding=binarySequential
  //) {
  //}

  val irq = Payload(Bool())
  val pc = Payload(UInt(mainWidth bits))
  val pcPlus2 = Payload(UInt(mainWidth bits))
  val psExSetPc = Payload(Flow(UInt(mainWidth bits)))

  object LpreState extends SpinalEnum(defaultEncoding=binarySequential) {
    val
      noLpre,
      haveHi,
      haveLo
      = newElement();
  }
  case class PrefixInfo(
    dataWidth: Int,
    isLpre: Boolean, 
  ) extends Bundle {
    val have = (!isLpre) generate Bool()
    val lpreState = (isLpre) generate LpreState()
    val data = UInt(dataWidth bits)
  }

  val preInfo = Payload(PrefixInfo(dataWidth=preWidth, isLpre=false))
  val lpreInfo = Payload(PrefixInfo(dataWidth=lpreWidth, isLpre=true))
  val indexInfo = Payload(PrefixInfo(dataWidth=numGprsSprsPow, isLpre=false))
  val instrDec = Payload(InstrDec())

  def flagIdxZ = 0
  def flagIdxC = 1
  def flagIdxV = 2
  def flagIdxN = 3
  //val rRegFlags = Reg(UInt(mainWidth bits)) init(0x0)
  val rGprVec = Vec.fill(numGprsSprs)(
    Reg(UInt(mainWidth bits)) init(0x0)
  )
  val rSprVec = Vec.fill(numGprsSprs)(
    Reg(UInt(mainWidth bits)) init(0x0)
  )

  //case class PsExPayload() extends Bundle {
  //  def flagIdxZ = 0
  //  def flagIdxC = 1
  //  def flagIdxV = 2
  //  def flagIdxN = 3
  //  val regFlags = UInt(mainWidth bits)
  //  val gprVec = Vec.fill(numGprs)(UInt(mainWidth bits))
  //}
  //case class PsMemWbPayload() extends Bundle {
  //}

  val nIf, nId, nEx = Node()
  val cIfId = CtrlLink(nIf, nId)
  val cIdEx = CtrlLink(nId, nEx)

  // Pipeline Stage: Instruction Fetch
  //when (!nIf(psExSetPc).valid) {
  //} otherwise {
  //}

  // Pipeline Stage: Instruction Decode

  // Pipeline Stage: EXecute:
  when (cIdEx(lpreInfo).lpreState =/= LpreState.haveHi) {
    switch (cIdEx(instrDec).fullgrp) {
      def myPc = cIdEx(pc)
      def myPcPlus2 = cIdEx(pcPlus2)
      def simm = cIdEx(instrDec).fullSimm
      def imm = cIdEx(instrDec).fullImm
      def ra = rGprVec(cIdEx(instrDec).raIdx)
      def rb = rGprVec(cIdEx(instrDec).rbIdx)
      def rc = rGprVec(cIdEx(instrDec).rcIdx)
      def lr = rGprVec(InstrEncConst.gprIdxLr)
      def fp = rGprVec(InstrEncConst.gprIdxFp)
      def sp = rGprVec(InstrEncConst.gprIdxSp)
      def sa = rSprVec(cIdEx(instrDec).raIdx)
      def sb = rSprVec(cIdEx(instrDec).rbIdx)
      def sc = rSprVec(cIdEx(instrDec).rcIdx)
      def flags = rSprVec(InstrEncConst.sprIdxFlags)
      def ids = rSprVec(InstrEncConst.sprIdxIds)
      def ira = rSprVec(InstrEncConst.sprIdxIra)
      def ie = rSprVec(InstrEncConst.sprIdxIe)
      def ity = rSprVec(InstrEncConst.sprIdxIty)
      def sty = rSprVec(InstrEncConst.sprIdxSty)
      def performSetFlagsZn(
        rawElemNumBytesPow: (Int, Int),
        result: UInt,
        //flagsOut: UInt,
      ): Unit = {
        //--------

        def myBits = params.elemNumBytesPow(
          rawElemNumBytesPow=rawElemNumBytesPow
        )._2
        flags(flagIdxZ) := (result(myBits - 1 downto 0) === 0)
        flags(flagIdxN) := result(myBits - 1)
        //--------
      }
      def performAddSub(
        rawElemNumBytesPow: (Int, Int),
        operandA: UInt,
        operandB: UInt,
        withCarryIn: Boolean,
        doSub: Boolean,
        doSetFlags: Boolean,
        //flagsOut: UInt
        result: UInt,
        flagsOut: Option[UInt]=None,
      ): Unit = {
        //--------
        def myBits = params.elemNumBytesPow(
          rawElemNumBytesPow=rawElemNumBytesPow
        )._2
        assert(result.getWidth == myBits + 1)
        //--------
        //uint64_t
        //  ret = 0,
        //  temp_operand_a = operand_a,
        //  temp_operand_b = operand_b,
        //  temp_flags_c_mask = 0,
        //  temp_flags_vn_mask = 0;
        val tempOperandA = UInt((myBits + 1) bits)
        val tempOperandB = UInt((myBits + 1) bits)
        tempOperandA := operandA
        tempOperandB := operandB
        if (!doSub) {
          //ret = temp_operand_a + temp_operand_b
          //+ (with_carry_in
          //  ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
          //  : 0x0ull);
          result := (
            tempOperandA + tempOperandB
            + (
              if (withCarryIn) {
                flags(flagIdxC downto flagIdxC)
              } else { // if (!withCarryIn)
                U"1'd0"
              }
            ).resized
          )
        } else { // if (doSub)
          /* 6502-style subtraction */
          //ret = temp_operand_a + (~temp_operand_b)
          //  + (with_carry_in 
          //    ? ((flags_in & FLARE32_FLAGS_C_MASK) >> FLARE32_FLAGS_C_BITPOS)
          //    : 0x1ull);
          result := (
            tempOperandA + (~tempOperandB)
            + (
              if (withCarryIn) {
                flags(flagIdxC downto flagIdxC)
              } else { // if (!withCarryIn)
                U"1'd1"
              }
            ).resized
          )
        }

        if (doSetFlags) {
          val tempFlagsOut = flagsOut match {
            case Some(myFlagsOut) => myFlagsOut;
            case None => flags
          }
          val tempFlags = cloneOf(tempFlagsOut)
          tempFlags := 0x0
          tempFlags.allowOverride
          performSetFlagsZn(
            rawElemNumBytesPow=rawElemNumBytesPow,
            result=result,
          )
          tempFlagsOut(flagIdxC) := result(myBits)
          tempFlagsOut(flagIdxV) := (
            (tempOperandA ^ result.resized)
            & (tempOperandB ^ result.resized)
          )(myBits - 1)
        }
        //--------
      }
      //--------
      is (InstrFullgrpDec.g0Pre) {
      }
      is (InstrFullgrpDec.g0Lpre) {
      }
      is (InstrFullgrpDec.g1) {
        switch (cIdEx(instrDec).instrG1Enc.op) {
          is (InstrG1EncOp.addRaS5) {    // Opcode 0x0: add rA, #simm5
            ra := ra + simm
          }
          is (InstrG1EncOp.addRaPcS5) {  // Opcode 0x1: add rA, pc, #simm5
            ra := myPcPlus2 + simm
          }
          is (InstrG1EncOp.addRaSpS5) {  // Opcode 0x2: add rA, sp, #simm5
            ra := sp + simm
          }
          is (InstrG1EncOp.addRaFpS5) {  // Opcode 0x3: add rA, fp, #simm5
            ra := fp + simm
          }
          is (InstrG1EncOp.cmpRaS5) {    // Opcode 0x4: cmp rA, #simm5
            val tempResult = UInt((mainWidth + 1) bits)
            performAddSub(
              rawElemNumBytesPow=params.rawElemNumBytesPow32,
              operandA=ra,
              operandB=simm,
              withCarryIn=false,
              doSub=true,
              doSetFlags=true,
              result=tempResult,
            )
          }
          is (InstrG1EncOp.cpyRaS5) {    // Opcode 0x5: cpy rA, #simm5
            ra := simm
          }
          is (InstrG1EncOp.lslRaI5) {    // Opcode 0x6: lsl rA, #imm5
            ra := ra << imm
          }
          is (InstrG1EncOp.lsrRaI5) {    // Opcode 0x7: lsr rA, #imm5
            ra := ra >> imm
          }
          is (InstrG1EncOp.asrRaI5) {    // Opcode 0x8: asr rA, #imm5
            ra := (ra.asSInt >> imm).asUInt
          }
          is (InstrG1EncOp.andRaS5) {    // Opcode 0x9: and rA, #simm5
            ra := ra & simm
          }
          is (InstrG1EncOp.orrRaS5) {    // Opcode 0xa: orr rA, #simm5
            ra := ra | simm
          }
          is (InstrG1EncOp.xorRaS5) {    // Opcode 0xb: xor rA, #simm5
            ra := ra ^ simm
          }
          is (InstrG1EncOp.zeRaI5) {     // Opcode 0xc: ze rA, #imm5
            //ra := ra(imm - 1 downto 0)
            //switch (imm) {
            //  for (value <- 0 until (1 << nonG3ImmWidth)) {
            //    is (value) {
            //      ra := ra(value - 1 downto 0).resized
            //    }
            //  }
            //}
            //ra := ra << (mainWidth - imm)
            //ra := ra & ((1 << imm) - 1)
            def tempShiftAmount = mainWidth - imm
            ra := (ra << tempShiftAmount) >> tempShiftAmount
          }
          is (InstrG1EncOp.seRaI5) {     // Opcode 0xd: se rA, #imm5
            //switch (imm) {
            //  for (value <- 0 until (1 << nonG3ImmWidth)) {
            //    is (value) {
            //      ra := ra.asSInt(value - 1 downto 0).resized.asUInt
            //    }
            //  }
            //}
            //ra := ra & ((1 << imm) - 1)
            def tempShiftAmount = mainWidth - imm
            ra := (
              ((ra.asSInt << tempShiftAmount) >> tempShiftAmount).asUInt
            )
          }
          is (InstrG1EncOp.swiRaS5) {    // Opcode 0xe: swi rA, #simm5
          }
          is (InstrG1EncOp.swiI5) {      // Opcode 0xf: swi #simm5
          }
        }
      }
      is (InstrFullgrpDec.g2) {
        switch (cIdEx(instrDec).instrG2Enc.op) {
          def f = cIdEx(instrDec).instrG2Enc.f
          is (InstrG2EncOp.addRaRb) {   // Opcode 0x0: add rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = false
            def tempDoSub = false

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.subRaRb) {   // Opcode 0x1: sub rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = false
            def tempDoSub = true

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.addRaSpRb) { // Opcode 0x2: add rA, sp, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = sp
            def tempOperandB = rb
            def tempWithCarryIn = false
            def tempDoSub = false

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.addRaFpRb) { // Opcode 0x3: add rA, fp, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = fp
            def tempOperandB = rb
            def tempWithCarryIn = false
            def tempDoSub = false

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.cmpRaRb) {   // Opcode 0x4: cmp rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = false
            def tempDoSub = true

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              //ra := tempResult(ra.bitsRange)
            }
            //when (!f) {
              myPerformAddSub(doSetFlags=false)
            //} otherwise { // when (f)
            //  myPerformAddSub(doSetFlags=true)
            //}
          }
          is (InstrG2EncOp.cpyRaRb) {   // Opcode 0x5: cpy rA, rB
            //ra := rb
            val tempResult = UInt(mainWidth bits)
            tempResult := rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.lslRaRb) {   // Opcode 0x6: lsl rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := ra << rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.lsrRaRb) {   // Opcode 0x7: lsr rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := ra >> rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.asrRaRb) {   // Opcode 0x8: asr rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := (ra.asSInt >> rb).asUInt
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.andRaRb) {   // Opcode 0x9: and rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := ra & rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.orrRaRb) {   // Opcode 0xa: orr rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := ra | rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.xorRaRb) {   // Opcode 0xb: xor rA, rB
            val tempResult = UInt(mainWidth bits)
            tempResult := ra ^ rb
            ra := tempResult
            when (f) {
              performSetFlagsZn(
                rawElemNumBytesPow=params.rawElemNumBytesPow32,
                result=tempResult,
              )
            }
          }
          is (InstrG2EncOp.adcRaRb) {   // Opcode 0xc: adc rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = true
            def tempDoSub = false

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.sbcRaRb) {   // Opcode 0xd: sbc rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = true
            def tempDoSub = true

            def myPerformAddSub(
              doSetFlags: Boolean,
            ): Unit = {
              performAddSub(
                rawElemNumBytesPow=tempRawEleNumBytesPow,
                operandA=tempOperandA,
                operandB=tempOperandB,
                withCarryIn=tempWithCarryIn,
                doSub=tempDoSub,
                doSetFlags=doSetFlags,
                result=tempResult,
              )
              ra := tempResult(ra.bitsRange)
            }
            when (!f) {
              myPerformAddSub(doSetFlags=false)
            } otherwise { // when (f)
              myPerformAddSub(doSetFlags=true)
            }
          }
          is (InstrG2EncOp.cmpbcRaRb) { // Opcode 0xe: cmpbc rA, rB
            val tempResult = UInt((mainWidth + 1) bits)
            def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
            def tempOperandA = ra
            def tempOperandB = rb
            def tempWithCarryIn = true
            def tempDoSub = true
            def tempDoSetFlags = true
            val tempFlagsOut = UInt(mainWidth bits)

            performAddSub(
              rawElemNumBytesPow=tempRawEleNumBytesPow,
              operandA=tempOperandA,
              operandB=tempOperandB,
              withCarryIn=tempWithCarryIn,
              doSub=tempDoSub,
              doSetFlags=tempDoSetFlags,
              result=tempResult,
              flagsOut=Some(tempFlagsOut),
            )
          }
          is (InstrG2EncOp.invalid0) {  // Opcode 0xf: invalid operation 0
          }
        }
      }
      is (InstrFullgrpDec.g3) {
      }
      is (InstrFullgrpDec.g4) {
      }
      is (InstrFullgrpDec.g5) {
      }
      is (InstrFullgrpDec.g6) {
      }
      is (InstrFullgrpDec.g7Sg00) {
      }
      is (InstrFullgrpDec.g7Sg010) {
      }
      is (InstrFullgrpDec.g7Sg0110) {
      }
      default {
        // eek!
      }
    }
  }

  //val locIcache = new Area {
  //}
  //val locDcache = new Area {
  //}

  //val locIf = new Area {
  //  //val ibusAStm = cloneOf(io.ibus.a)
  //  //io.ibus.a <-/< ibusAStm
  //  //val ibusDStm = cloneOf(io.ibus.d)
  //  //ibusDStm <-/< io.ibus.d

  //  //def flagIdxZ = 0
  //  //def flagIdxC = 1
  //  //def flagIdxV = 2
  //  //def flagIdxN = 3

  //  //val regFlags = UInt(mainWidth bits)
  //}
  ////--------
  //val locId = new Area {
  //}
  ////--------
  //val locEx = new Area {
  //  //val myFlags = cloneOf(locIf.regFlags)

  //  //val dbusAStm = cloneOf(io.dbus.a)
  //  //dbusAStm <-/< io.dbus.a
  //  //val dbusDStm = cloneOf(io.dbus.d)
  //  //dbusDStm <-/< io.dbus.d
  //}
  //--------
  Builder(cIfId, cIdEx)
  //--------
}

object Flare32CpuVerilog extends App {
  Config.spinal.generateVerilog(Flare32Cpu(params=Flare32CpuParams()))
}
