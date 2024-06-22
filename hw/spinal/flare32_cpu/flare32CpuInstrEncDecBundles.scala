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

object Flare32CpuInstrEncConst {
  //--------
  // Instruction Group 0
  val g0Grp = U"3'd0"
  val g0PreSubgrp = U"1'b0"
  val g0PreMaskedSubgrp = M"0-"
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
  def gprR0Idx = U"4'd0"
  def gprR1Idx = U"4'd1"
  def gprR2Idx = U"4'd2"
  def gprR3Idx = U"4'd3"
  def gprR4Idx = U"4'd4"
  def gprR5Idx = U"4'd5"
  def gprR6Idx = U"4'd6"
  def gprR7Idx = U"4'd7"
  def gprR8Idx = U"4'd8"
  def gprR9Idx = U"4'd9"
  def gprR10Idx = U"4'd10"
  def gprR11Idx = U"4'd11"
  def gprR12Idx = U"4'd12"
  def gprLrIdx = U"4'd13"
  def gprFpIdx = U"4'd14"
  def gprSpIdx = U"4'd15"

  def sprFlagsIdx = U"4'd0"
  def sprIdsIdx = U"4'd1"
  def sprIraIdx = U"4'd2"
  def sprIeIdx = U"4'd3"
  def sprItyIdx = U"4'd4"
  def sprStyIdx = U"4'd5"
  def sprS6Idx = U"4'd6"
  def sprS7Idx = U"4'd7"
  def sprS8Idx = U"4'd8"
  def sprS9Idx = U"4'd9"
  def sprS10Idx = U"4'd10"
  def sprS11Idx = U"4'd11"
  def sprS12Idx = U"4'd12"
  def sprS13Idx = U"4'd13"
  def sprS14Idx = U"4'd14"
  def sprS15Idx = U"4'd15"
  //--------
}
case class Flare32CpuInstrG0EncPre(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g0Grp.getWidth bits)
  val subgrp = UInt(Flare32CpuInstrEncConst.g0PreSubgrp.getWidth bits)
  def fullgrp = Cat(grp, subgrp)
  val simm = UInt(params.preWidth bits)
}
case class Flare32CpuInstrG0EncLpreHi(
  params: Flare32CpuParams
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g0Grp.getWidth bits)
  val subgrp = UInt(Flare32CpuInstrEncConst.g0LpreSubgrp.getWidth bits)
  def fullgrp = Cat(grp, subgrp)
  //val simm = UInt(lpreWidth bits)
  val simmHi = UInt((params.lpreWidth - params.instrMainWidth) bits)
}
object Flare32CpuInstrG1EncOp extends SpinalEnum(
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
case class Flare32CpuInstrG1Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g1Grp.getWidth bits)
  // immediate or signed immediate, depending on the opcode
  val imm = UInt(params.nonG3ImmWidth bits) 
  val op = Flare32CpuInstrG1EncOp()
  val raIdx = UInt(params.numGprsSprsPow bits)
}
object Flare32CpuInstrG2EncOp extends SpinalEnum(
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
case class Flare32CpuInstrG2Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g2Grp.getWidth bits)
  val f = Bool()
  val op = Flare32CpuInstrG2EncOp()
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}
object Flare32CpuInstrG3EncOp extends SpinalEnum(
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
case class Flare32CpuInstrG3Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g3Grp.getWidth bits)
  val simm = SInt(params.g3ImmWidth bits)
  val op = Flare32CpuInstrG3EncOp()
}

object Flare32CpuInstrG4EncOp extends SpinalEnum(
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
case class Flare32CpuInstrG4Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g4Grp.getWidth bits)
  val op = Flare32CpuInstrG4EncOp()
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}
case class Flare32CpuInstrG5Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g5Grp.getWidth bits)
  val simm = SInt(params.nonG3ImmWidth bits)
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}
case class Flare32CpuInstrG6Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g6Grp.getWidth bits)
  val simm = SInt(params.nonG3ImmWidth bits)
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}
// this includes the `w` bit (since it's contiguous with the opcode field)
object Flare32CpuInstrG7Sg00FullOpEnc extends SpinalEnum(
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
case class Flare32CpuInstrG7Sg00Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g7Grp.getWidth bits)
  val subgrp = UInt(Flare32CpuInstrEncConst.g7Sg00Subgrp.getWidth bits)
  def fullgrp = Cat(grp, subgrp)
  val op = Flare32CpuInstrG7Sg00FullOpEnc()
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}
object Flare32CpuInstrG7Sg010EncOp extends SpinalEnum(
  defaultEncoding=binarySequential
) {
  val
    ldrSaRb,      // Opcode 0x0: ldr sA, [rB]
    ldrSaSb,      // Opcode 0x1: ldr sA, [sB]
    strSaRb,      // Opcode 0x2: str sA, [rB]
    strSaSb       // Opcode 0x3: str sA, [sB]
    = newElement();
}
case class Flare32CpuInstrG7Sg010Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g7Grp.getWidth bits)
  val subgrp = UInt(Flare32CpuInstrEncConst.g7Sg010Subgrp.getWidth bits)
  def fullgrp = Cat(grp, subgrp)
  val op = Flare32CpuInstrG7Sg010EncOp()
  val rbIdx = UInt(params.numGprsSprsPow bits)
  val raIdx = UInt(params.numGprsSprsPow bits)
}

case class Flare32CpuInstrG7Sg0110Enc(
  params: Flare32CpuParams,
) extends Bundle {
  val grp = UInt(Flare32CpuInstrEncConst.g7Grp.getWidth bits)
  val subgrp = UInt(Flare32CpuInstrEncConst.g7Sg0110Subgrp.getWidth bits)
  def fullgrp = Cat(grp, subgrp)
}

object Flare32CpuInstrFullgrpDec extends SpinalEnum(
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
    g7Sg0110,
    invalid
    = newElement();
}
case class Flare32CpuInstrDecEtc(
  params: Flare32CpuParams,
) extends Bundle {
  //--------
  // This `Bundle` also includes register values read from the two
  // register files
  //--------
  //val isNop = Bool()
  val isInvalid = Bool()
  val haveFullInstr = Bool()
  val fullgrp = Flare32CpuInstrFullgrpDec()
  val fullSimm = UInt(params.mainWidth bits)
  val fullImm = UInt(params.mainWidth bits)
  val fullPcrelSimm = UInt(params.mainWidth bits)

  val enumGprRa = 0 
  val enumGprRb = 1
  val enumGprRc = 2
  val enumGprLr = 3
  val enumGprFp = 4
  val enumGprSp = 5
  val enumSprSa = 6
  val enumSprSb = 7
  val enumSprFlags = 8
  val enumSprIds = 9
  val enumSprIra = 10
  val enumSprIe = 11
  val enumSprIty = 12
  val enumSprSty = 13
  val enumGprRa64Hi = 14
  val enumGprRa64Lo = 15
  val enumGprRb64Hi = 16
  val enumGprRb64Lo = 17

  val ra = UInt(params.mainWidth bits)         // `rA`
  val rb = UInt(params.mainWidth bits)         // `rB`
  val rc = UInt(params.mainWidth bits)         // `rC`
  val gprLr = UInt(params.mainWidth bits)      // `lr`
  val gprFp = UInt(params.mainWidth bits)      // `fp`
  val gprSp = UInt(params.mainWidth bits)      // `sp`
  //val rd = UInt(params.mainWidth bits)         // `rD`
  val sa = UInt(params.mainWidth bits)         // `sA`
  val sb = UInt(params.mainWidth bits)         // `sB`
  val sprFlags = UInt(params.mainWidth bits)   // `flags`
  val sprIds = UInt(params.mainWidth bits)     // `ids`
  val sprIra = UInt(params.mainWidth bits)     // `ira`
  val sprIe = UInt(params.mainWidth bits)      // `ie`
  val sprIty = UInt(params.mainWidth bits)     // `ity`
  val sprSty = UInt(params.mainWidth bits)     // `sty`


  val raIdx = UInt(params.numGprsSprsPow bits) //
  val rbIdx = UInt(params.numGprsSprsPow bits) //
  val rcIdx = UInt(params.numGprsSprsPow bits) // 
  //val rdIdx = UInt(params.numGprsSprsPow bits) // 

  //val saIdx = UInt(params.numGprsSprsPow bits) //
  //val sbIdx = UInt(params.numGprsSprsPow bits) //


  val ra64Hi = UInt(params.mainWidth bits)     //
  val ra64Lo = UInt(params.mainWidth bits)     //
  val rb64Hi = UInt(params.mainWidth bits)     //
  val rb64Lo = UInt(params.mainWidth bits)     //

  //val ra64HiIdx = UInt(params.numGprsSprsPow bits)
  //val ra64LoIdx = UInt(params.numGprsSprsPow bits)
  //val rb64HiIdx = UInt(params.numGprsSprsPow bits)
  //val rb64LoIdx = UInt(params.numGprsSprsPow bits)

  def ra64HiIdx = Cat(
    raIdx(raIdx.high downto 1),
    False,
  ).asUInt
  def ra64LoIdx = Cat(
    raIdx(raIdx.high downto 1),
    True,
  ).asUInt
  def rb64HiIdx = Cat(
    rbIdx(rbIdx.high downto 1),
    False,
  ).asUInt
  def rb64LoIdx = Cat(
    rbIdx(rbIdx.high downto 1),
    True,
  ).asUInt

  //def doSet64Idxs(): Unit = {
  //  ra64HiIdx := Cat(
  //    raIdx(raIdx.high downto 1),
  //    False,
  //  ).asUInt
  //  ra64LoIdx := Cat(
  //    raIdx(raIdx.high downto 1),
  //    True,
  //  ).asUInt
  //  rb64HiIdx := Cat(
  //    rbIdx(rbIdx.high downto 1),
  //    False,
  //  ).asUInt
  //  rb64LoIdx := Cat(
  //    rbIdx(rbIdx.high downto 1),
  //    True,
  //  ).asUInt
  //}

  def doFwdAllRegs(
    execPayload: /*Payload[*/Flare32CpuPipePayloadExec/*]*/,
    //someCtrlLink: CtrlLink,
    fwdRc: Boolean,
    extCond: Bool,
    second: Option[(Bool, Flare32CpuPipePayloadExec)]=None,
  )(
    getNonFwdRegFunc: (
      UInt,       // `decIdx`
      Boolean,    // `isGpr`
      //Int,        // `whichReg`
    ) => UInt,
    //getNonFwdRegFunc1: (
    //  UInt,     // `decIdx`
    //  Boolean,  // `isGpr`
    //  //Int,      // `whichReg`
    //) => UInt,
  ): Unit = {
    //--------
    def doFwdOneReg(
      decIdx: UInt,
      //nonFwdReg: UInt,
      isGpr: Boolean,
      //whichReg: Int,
      //someCtrlLink: CtrlLink,
    ) = {
      //def innerFunc(
      //  someExecPayload: Flare32CpuPipePayloadExec,
      //  someNonFwdReg: UInt,
      //) = {
      //  def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
      //  def nonFwdReg = getNonFwdRegFunc(
      //    decIdx,
      //    isGpr,
      //    //whichReg,
      //  )
      //  //def tempRegWb = cExWb(psExOutp).get(isGpr)
      //  Mux[UInt](
      //    (
      //      decIdx === tempExOutp.regIdx
      //      && tempExOutp.wrReg.fire
      //      && extCond
      //    ),
      //    tempExOutp.wrReg.payload,
      //    someNonFwdReg,
      //  )
      //}
      second match {
        case Some(mySecond) => {
          //val tempNonFwdReg = (
          //  innerFunc(
          //    someExecPayload=execPayload,
          //    someNonFwdReg=getNonFwdRegFunc(
          //      decIdx,
          //      isGpr,
          //      //whichReg,
          //    ),
          //  )
          //)
          //innerFunc(
          //  someExecPayload=myExecPayload1,
          //  someNonFwdReg=getNonFwdRegFunc(
          //    decIdx,
          //    isGpr,
          //    //whichReg,
          //  ),
          //)
          //Mux[UInt](
          //  decIdx === tempExOutp
          //)
          def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
          def nonFwdReg = getNonFwdRegFunc(
            decIdx,
            isGpr,
            //whichReg,
          )
          //def tempRegWb = cExWb(psExOutp).get(isGpr)
          val condVec = Vec(Bool(), 2)
          condVec(0) := (
            decIdx === tempExOutp.regIdx
            && tempExOutp.wrReg.fire
            && extCond
          )
          condVec(1) := (
            decIdx === mySecond._2.get(isGpr).regIdx
            && mySecond._2.get(isGpr).wrReg.fire
            && mySecond._1
          )
          val fwdVec = Vec(UInt(params.mainWidth bits), 2)
          fwdVec(0) := tempExOutp.wrReg.payload
          fwdVec(1) := mySecond._2.get(isGpr).wrReg.payload
          val myFindFirst = condVec.sFindFirst(_ === True)
          Mux[UInt](
            myFindFirst._1,
            //Mux[UInt](
            //  myFindFirst._2 === 0,
            //  tempExOutp.wrReg.payload,
            //  mySecond._2.get(isGpr).wrReg.payload,
            //),
            fwdVec(myFindFirst._2),
            nonFwdReg,
          )
          //condVec(2) := True
          //Mux[UInt](
          //  (
          //  ),
          //  Mux[UInt](
          //    tempExOutp.wrReg.payload,
          //    nonFwdReg,
          //  ),
          //  nonFwdReg
          //)
        }
        case None => {
          //innerFunc(
          //  execPayload,
          //  someNonFwdReg=getNonFwdRegFunc(
          //    decIdx,
          //    isGpr,
          //    //whichReg,
          //  ),
          //)
          def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
          def nonFwdReg = getNonFwdRegFunc(
            decIdx,
            isGpr,
            //whichReg,
          )
          //def tempRegWb = cExWb(psExOutp).get(isGpr)
          Mux[UInt](
            (
              decIdx === tempExOutp.regIdx
              && tempExOutp.wrReg.fire
              && extCond
            ),
            tempExOutp.wrReg.payload,
            nonFwdReg,
          )
        }
      }
    }
    //--------
    if (!fwdRc) {
      ra := doFwdOneReg(
        decIdx=raIdx,
        isGpr=true,
        //whichReg=enumGprRa,
      )
      rb := doFwdOneReg(
        decIdx=rbIdx,
        isGpr=true,
        //whichReg=enumGprRb,
      )
      gprLr := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.gprLrIdx,
        isGpr=true,
        //whichReg=enumGprLr,
      )
      gprFp := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.gprFpIdx,
        isGpr=true,
        //whichReg=enumGprFp,
      )
      gprSp := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.gprSpIdx,
        isGpr=true,
        //whichReg=enumGprSp,
      )
      sa := doFwdOneReg(
        decIdx=raIdx,
        isGpr=false,
        //whichReg=enumSprSa,
      )
      sb := doFwdOneReg(
        decIdx=rbIdx,
        isGpr=false,
        //whichReg=enumSprSa,
      )
      sprFlags := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprFlagsIdx,
        isGpr=false,
        //whichReg=enumSprFlags,
      )
      sprIds := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprIdsIdx,
        isGpr=false,
        //whichReg=enumSprIds,
      )
      sprIra := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprIraIdx,
        isGpr=false,
        //whichReg=enumSprIra,
      )
      sprIe := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprIeIdx,
        isGpr=false,
        //whichReg=enumSprIe,
      )
      sprIty := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprItyIdx,
        isGpr=false,
        //whichReg=enumSprIty,
      )
      sprSty := doFwdOneReg(
        decIdx=Flare32CpuInstrEncConst.sprStyIdx,
        isGpr=false,
        //whichReg=enumSprSty,
      )

      ra64Hi := doFwdOneReg(
        decIdx=ra64HiIdx,
        isGpr=true,
        //whichReg=enumGprRa64Hi
      )
      ra64Lo := doFwdOneReg(
        decIdx=ra64LoIdx,
        isGpr=true,
        //whichReg=enumGprRa64Lo,
      )
      rb64Hi := doFwdOneReg(
        decIdx=rb64HiIdx,
        isGpr=true,
        //whichReg=enumGprRb64Hi,
      )
      rb64Lo := doFwdOneReg(
        decIdx=rb64LoIdx,
        isGpr=true,
        //whichReg=enumGprRb64Lo,
      )
    } else { // if (fwdRc)
      rc := doFwdOneReg(
        decIdx=rcIdx,
        isGpr=true,
        //whichReg=enumGprRc,
      )
    }
    //--------
  }
  //--------
  case class InstrEnc(
    params: Flare32CpuParams,
  ) extends Bundle {
    val g0Pre = Flare32CpuInstrG0EncPre(params=params)
    val g0LpreHi = Flare32CpuInstrG0EncLpreHi(params=params)
    val g1 = Flare32CpuInstrG1Enc(params=params)
    val g2 = Flare32CpuInstrG2Enc(params=params)
    val g3 = Flare32CpuInstrG3Enc(params=params)
    val g4 = Flare32CpuInstrG4Enc(params=params)
    val g5 = Flare32CpuInstrG5Enc(params=params)
    val g6 = Flare32CpuInstrG6Enc(params=params)
    val g7Sg00 = Flare32CpuInstrG7Sg00Enc(params=params)
    val g7Sg010 = Flare32CpuInstrG7Sg010Enc(params=params)
    val g7Sg0110 = Flare32CpuInstrG7Sg0110Enc(params=params)
  }
  val instrEnc = InstrEnc(params=params)
  //--------
}
