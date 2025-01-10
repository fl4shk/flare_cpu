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
import libcheesevoyage.general.PipeHelper
import libcheesevoyage.math.LongDivMultiCycle

object FlareCpuInstrEncConst {
  //--------
  // Instruction Group 0
  def g0GrpWidth = 3
  def g0Grp = U"3'd0"
  def g0PreSubgrpWidth = 1
  def g0PreSubgrp = U"1'b0"
  //def g0PreMaskedSubgrp = M"0-"
  def g0LpreSubgrpWidth = 2
  def g0LpreSubgrp = U"2'b10"
  def g0AtomicSubgrpWidth = 4
  def g0AtomicSubgrp = U"4'b1100"
  //--------
  // Instruction Group 1
  def g1GrpWidth = 3
  def g1Grp = U"3'd1"
  //--------
  // Instruction Group 2
  def g2GrpWidth = 3
  def g2Grp = U"3'd2"
  //--------
  // Instruction Group 3
  def g3GrpWidth = 3
  def g3Grp = U"3'd3"
  //--------
  // Instruction Group 4
  def g4GrpWidth = 3
  def g4Grp = U"3'd4"
  //--------
  // Instruction Group 5
  def g5GrpWidth = 3
  def g5Grp = U"3'd5"
  def g5Sg0SubgrpWidth = 1
  def g5Sg0Subgrp = U"1'b0"
  def g5Sg0ReservedWidth = 4
  def g5Sg1SubgrpWidth = 1
  def g5Sg1Subgrp = U"1'b1"
  //--------
  // Instruction Group 6
  def g6GrpWidth = 3
  def g6Grp = U"3'd6"
  //--------
  // Instruction Group 7
  def g7GrpWidth = 3
  def g7Grp = U"3'd7"
  def g7Sg00SubgrpWidth = 2
  def g7Sg00Subgrp = U"2'b00"
  def g7Sg010SubgrpWidth = 3
  def g7Sg010Subgrp = U"3'b010"
  def g7Sg0110SubgrpWidth = 4
  def g7Sg0110Subgrp = U"4'b0110"
  def g7Sg01110SubgrpWidth = 5
  def g7Sg01110Subgrp = U"5'b01110"
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
  def sprHiIdx = U"4'd6"
  def sprLoIdx = U"4'd7"
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
case class FlareCpuInstrG0EncPre(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val simm = UInt(cfg.preWidth bits)
  val subgrp = UInt(FlareCpuInstrEncConst.g0PreSubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g0GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}
case class FlareCpuInstrG0EncLpreHi(
  cfg: FlareCpuConfig
) extends Bundle {
  //val simm = UInt(lpreWidth bits)
  //--------
  val simmHi = UInt((cfg.lpreWidth - cfg.instrMainWidth) bits)
  val subgrp = UInt(FlareCpuInstrEncConst.g0LpreSubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g0GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}
case class FlareCpuInstrG0EncAtomic(
  cfg: FlareCpuConfig
) extends Bundle {
  //--------
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val l = Bool()
  val subgrp = UInt(FlareCpuInstrEncConst.g0AtomicSubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g0GrpWidth bits)
}
object FlareCpuInstrG1EncOp extends SpinalEnum(
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
    swiI5      // Opcode 0xf: swi #imm5
    = newElement();
}
case class FlareCpuInstrG1Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val op = FlareCpuInstrG1EncOp()
  // immediate or signed immediate, depending on the opcode
  val imm = UInt(cfg.g1g7ImmWidth bits) 
  val grp = UInt(FlareCpuInstrEncConst.g1GrpWidth bits)
}
object FlareCpuInstrG2EncOp extends SpinalEnum(
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
case class FlareCpuInstrG2Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val op = FlareCpuInstrG2EncOp()
  val f = Bool()
  val grp = UInt(FlareCpuInstrEncConst.g2GrpWidth bits)
}
object FlareCpuInstrG3EncOp extends SpinalEnum(
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
case class FlareCpuInstrG3Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  val op = FlareCpuInstrG3EncOp()
  val simm = SInt(cfg.g3ImmWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g3GrpWidth bits)
}

object FlareCpuInstrG4EncOp extends SpinalEnum(
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
    udivmodRaRb,     // Opcode 0xc: udivmod rA, rB
    sdivmodRaRb,     // Opcode 0xd: sdivmod rA, rB
    //umodRaRb,     // Opcode 0xe: umod rA, rB
    //smodRaRb,     // Opcode 0xf: smod rA, rB
    lumulRaRb,    // Opcode 0xe: lumul rA, rB
    lsmulRaRb,    // Opcode 0xf: lsmul rA, rB
    //--------
    ////lumulRaRb,    // Opcode 0x10: lumul rA, rB
    ////lsmulRaRb,    // Opcode 0x11: lsmul rA, rB
    //udiv64RaRb,   // Opcode 0x12: udiv64 rA, rB
    //sdiv64RaRb,   // Opcode 0x13: sdiv64 rA, rB
    ////umod64RaRb,   // Opcode 0x14: umod64 rA, rB
    ////smod64RaRb,   // Opcode 0x15: smod64 rA, rB
    udivmod64RaRb,   // Opcode 0x10: udivmod64 rA, rB
    sdivmod64RaRb,   // Opcode 0x11: sdivmod64 rA, rB
    ldubRaRb,     // Opcode 0x12: ldub rA, [rB]
    ldsbRaRb,     // Opcode 0x13: ldsb rA, [rB]
    lduhRaRb,     // Opcode 0x14: lduh rA, [rB]
    ldshRaRb,     // Opcode 0x15: ldsh rA, [rB]
    ldrRaRb,      // Opcode 0x16: ldr rA, [rB]
    reserved17,   // Opcode 0x17: reserved
    stbRaRb,      // Opcode 0x18: stb rA, [rB]
    sthRaRb,      // Opcode 0x19: sth rA, [rB]
    strRaRb,      // Opcode 0x1a: str rA, [rB]
    reserved1b,   // Opcode 0x1b: reserved
    cpyRaSb,      // Opcode 0x1c: cpy rA, sB
    cpySaRb,      // Opcode 0x1d: cpy sA, rB
    cpySaSb      // Opcode 0x1e: cpy sA, sB
    //indexRaRb       // Opcode 0x1b: index rA, rB
    //--------
    = newElement();
}
case class FlareCpuInstrG4Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val op = FlareCpuInstrG4EncOp()
  val grp = UInt(FlareCpuInstrEncConst.g4GrpWidth bits)
}
case class FlareCpuInstrG5Sg0Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //val simm = SInt(cfg.g1g7ImmWidth bits)
  //val rbIdx = UInt(cfg.numGprsSprsPow bits)
  //val raIdx = UInt(cfg.numGprsSprsPow bits)
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val reserved = UInt(FlareCpuInstrEncConst.g5Sg0ReservedWidth bits)
  val subgrp = UInt(FlareCpuInstrEncConst.g5Sg0SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g5GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}
case class FlareCpuInstrG5Sg1Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //val simm = SInt(cfg.g1g7ImmWidth bits)
  //val rbIdx = UInt(cfg.numGprsSprsPow bits)
  //val raIdx = UInt(cfg.numGprsSprsPow bits)
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val simm = UInt(cfg.g5ImmWidth bits)
  val subgrp = UInt(FlareCpuInstrEncConst.g5Sg1SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g5GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}
//case class FlareCpuInstrG6Enc(
//  cfg: FlareCpuParams,
//) extends Bundle {
//  val grp = UInt(FlareCpuInstrEncConst.g6GrpWidth bits)
//  val simm = SInt(cfg.nonG3ImmWidth bits)
//  val rbIdx = UInt(cfg.numGprsSprsPow bits)
//  val raIdx = UInt(cfg.numGprsSprsPow bits)
//}
// this includes the `w` bit (since it's contiguous with the opcode field)
object FlareCpuInstrG7Sg00FullOpEnc extends SpinalEnum(
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
case class FlareCpuInstrG7Sg00Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val op = FlareCpuInstrG7Sg00FullOpEnc()
  def w = op.asBits.msb
  val subgrp = UInt(FlareCpuInstrEncConst.g7Sg00SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g7GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}
object FlareCpuInstrG7Sg010EncOp extends SpinalEnum(
  defaultEncoding=binarySequential
) {
  val
    ldrSaRb,      // Opcode 0x0: ldr sA, [rB]
    ldrSaSb,      // Opcode 0x1: ldr sA, [sB]
    strSaRb,      // Opcode 0x2: str sA, [rB]
    strSaSb       // Opcode 0x3: str sA, [sB]
    = newElement();
}
case class FlareCpuInstrG7Sg010Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val rbIdx = UInt(cfg.numGprsSprsPow bits)
  val op = FlareCpuInstrG7Sg010EncOp()
  val subgrp = UInt(FlareCpuInstrEncConst.g7Sg010SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g7GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}

case class FlareCpuInstrG7Sg0110Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val raIdx = UInt(cfg.numGprsSprsPow bits)
  val imm = UInt(cfg.g1g7ImmWidth bits) 
  val subgrp = UInt(FlareCpuInstrEncConst.g7Sg0110SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g7GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}

case class FlareCpuInstrG7Sg01110Enc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  val blank = UInt(cfg.g7Sg01110BlankWidth bits)
  val subgrp = UInt(FlareCpuInstrEncConst.g7Sg01110SubgrpWidth bits)
  val grp = UInt(FlareCpuInstrEncConst.g7GrpWidth bits)
  def fullgrp = Cat(grp, subgrp)
}

object FlareCpuInstrFullgrpDec extends SpinalEnum(
  defaultEncoding=binarySequential
) {
  val
    g0Pre,
    g0LpreHi,
    g0LpreLo,
    g0Atomic,
    g1,
    g2,
    g3,
    g4,
    g5Sg0,
    g5Sg1,
    //g6,
    g7Sg00,
    g7Sg010,
    g7Sg0110,
    g7Sg01110,
    invalid
    = newElement();
}
case class FlareCpuInstrEnc(
  cfg: FlareCpuConfig,
) extends Bundle {
  val g0Pre = FlareCpuInstrG0EncPre(cfg=cfg)
  val g0LpreHi = FlareCpuInstrG0EncLpreHi(cfg=cfg)
  val g0LpreLo = UInt(cfg.instrMainWidth bits)
  val g0Atomic = FlareCpuInstrG0EncAtomic(cfg=cfg)
  val g1 = FlareCpuInstrG1Enc(cfg=cfg)
  val g2 = FlareCpuInstrG2Enc(cfg=cfg)
  val g3 = FlareCpuInstrG3Enc(cfg=cfg)
  val g4 = FlareCpuInstrG4Enc(cfg=cfg)
  val g5Sg0 = FlareCpuInstrG5Sg0Enc(cfg=cfg)
  val g5Sg1 = FlareCpuInstrG5Sg1Enc(cfg=cfg)
  //val g6 = FlareCpuInstrG6Enc(cfg=cfg)
  val g7Sg00 = FlareCpuInstrG7Sg00Enc(cfg=cfg)
  val g7Sg010 = FlareCpuInstrG7Sg010Enc(cfg=cfg)
  val g7Sg0110 = FlareCpuInstrG7Sg0110Enc(cfg=cfg)
  val g7Sg01110 = FlareCpuInstrG7Sg01110Enc(cfg=cfg)
}
object FlareCpuInstrDecOp
extends SpinalEnum(defaultEncoding=binarySequential) {
  // decoded instruction opcode
  val
    //--------
    bubble,   // fake instruction, acts as a NOP and prevents forwarding in
              // the `PipeMemRmw`s
              // used, for example, for `index`, `lpre`'s low immediate bits
    lpreSimmHi,
    lpreSimmLo, // fake instruction, acts as a bubble
    preSimm,
    //--------
    cmpxchg, // without lock
    cmpxchgLock, // with lock
    xchg,     // without lock
    xchgLock,   // with lock
    //--------
    //addRaRbSimm, // only `fp`, `sp`, or `pc` can be `rB`
    addRaSimm,
    addRaPcSimm,
    addRaSpSimm,
    addRaFpSimm,

    cmpRaSimm,
    cpyRaSimm,
    lslRaImm,
    lsrRaImm,

    asrRaImm,
    andRaSimm,
    orrRaSimm,
    xorRaSimm,

    zeRaImm,
    seRaImm,
    swiRaSimm,
    swiImm,
    //--------
    addRaRb,
    subRaRb,
    addRaSpRb,
    addRaFpRb,
    //--------
    cmpRaRb,
    cpyRaRb,
    lslRaRb,
    lsrRaRb,
    asrRaRb,
    andRaRb,
    orrRaRb,
    xorRaRb,
    //--------
    adcRaRb,
    sbcRaRb,
    cmpbcRaRb,
    //--------
    //addRaRbFlags,
    //subRaRbFlags,
    //addRaSpRbFlags,
    //addRaFpRbFlags,
    ////--------
    ////cmpRaRbFlags,
    //cpyRaRbFlags,
    //lslRaRbFlags,
    //lsrRaRbFlags,
    //asrRaRbFlags,
    //andRaRbFlags,
    //orrRaRbFlags,
    //xorRaRbFlags,
    ////--------
    //adcRaRbFlags,
    //sbcRaRbFlags,
    //cmpbcRaRbFlags,
    //--------
    blSimm,
    braSimm,
    beqSimm,
    bneSimm,
    bmiSimm,
    bplSimm,
    bvsSimm,
    bvcSimm,
    bgeuSimm,
    bltuSimm,
    bgtuSimm,
    bleuSimm,
    bgesSimm,
    bltsSimm,
    bgtsSimm,
    blesSimm,
    //--------
    jlRa,
    jmpRa,
    jmpIra,
    reti,
    ei,
    di,
    //--------
    pushRaRb,
    pushSaRb,
    popRaRb,
    popSaRb,
    popPcRb,
    //--------
    mulRaRb,
    //--------
    //udivRaRb,
    //sdivRaRb,
    //udivmodRaRbRc,
    //sdivmodRaRbRc,
    udivmodRaRb,
    sdivmodRaRb,
    //--------
    //lumulRcRdRaRb,
    //lsmulRcRdRaRb,
    lumulRaRb,
    lsmulRaRb,
    //--------
    //udiv64RaRb,
    //sdiv64RaRb,
    //udivmod64RaRbRcRd,
    //sdivmod64RaRbRcRd,
    udivmod64RaRb,
    sdivmod64RaRb,
    //--------
    ldubRaRbLdst,
    ldsbRaRbLdst,
    lduhRaRbLdst,
    ldshRaRbLdst,
    ldrRaRbLdst,
    stbRaRbLdst,
    sthRaRbLdst,
    strRaRbLdst,
    //--------
    cpyRaSb,
    cpySaRb,
    cpySaSb,
    //--------
    indexRaRb,
    indexRaSimm,
    //--------
    //ldrRaRbSimmLdst,
    //strRaRbSimmLdst,
    //--------
    ldrSaRbLdst,
    ldrSaSbLdst,
    strSaRbLdst,
    strSaSbLdst,
    //--------
    cmpbRaRb,
    cmphRaRb,
    lsrbRaRb,
    lsrhRaRb,
    asrbRaRb,
    asrhRaRb,
    //--------
    icreloadRaSimm,
    //--------
    icflush
    //--------
    = newElement()
}
object FlareCpuGprSelect extends SpinalEnum(
  defaultEncoding=binarySequential
) {
  val
    gprEvenNonFp,
    gprFp,
    gprOddNonSp,
    gprSp
    //gpr64
    = newElement();
}
object FlareCpuSprSelect extends SpinalEnum(
  defaultEncoding=binarySequential
) {
  val
    sprEven,
    sprOdd
    = newElement();
}

case class FlareCpuDecodeTemp(
  cfg: FlareCpuConfig,
) extends Bundle {
  val indexRaRbValid = Bool()
  val indexRaSimmValid = Bool()
  val preLpreValid = Bool()
  val preValid = Bool()
  val lpreValid = Bool()
}
case class FlareCpuInstrDecEtc(
  cfg: FlareCpuConfig,
) extends Bundle {
  //--------
  //val bubble = Bool()
  val isInvalid = Bool()
  val haveFullInstr = Bool()
  val fullgrp = FlareCpuInstrFullgrpDec()
  val fullSimm = UInt(cfg.mainWidth bits)
  val fullImm = UInt(cfg.mainWidth bits)
  val fullPcrelSimm = UInt(cfg.mainWidth bits)
  //val indexRa = UInt(cfg.mainWidth bits)
  //val indexRb = UInt(cfg.mainWidth bits)
  //val indexSum = UInt(cfg.mainWidth bits)
  val fwl = Bool()
  val decOp = FlareCpuInstrDecOp()

  //val regFileGprEvenModMemWordValid = Bool()
  //val regFileGprOddNonSpModMemWordValid = Bool()
  //val regFileGprSpModMemWordValid = Bool()
  //val regFileSprModMemWordValid = Bool()

  val gprRaSel = FlareCpuGprSelect()
  //val gprRa64LoSel = FlareCpuGprSelect()
  val gprRa64IsNonFpSp = Bool() // 64-bit GPR rA pair select
  val gprRbSel = FlareCpuGprSelect()
  val gprRb64IsNonFpSp = Bool() // 64-bit GPR rB pair select
  val sprSaSel = FlareCpuSprSelect()
  val sprSbSel = FlareCpuSprSelect()
  //--------
  val wrGprEvenNonFpRaIdx = Bool()
  val gprEvenNonFpRaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val gprEvenNonFpRbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------
  val wrGprFpRaIdx = Bool()
  val gprFpRaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val gprFpRbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------
  val wrGprOddNonSpRaIdx = Bool()
  val gprOddNonSpRaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val gprOddNonSpRbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------
  //val haveGprOddNonSpRaIdx = Bool()
  //val haveGprOddNonSpRbIdx = Bool()
  val wrGprSpRaIdx = Bool()
  val gprSpRaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val gprSpRbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------
  val wrSprEvenSaIdx = Bool()
  val sprEvenSaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val sprEvenSbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------
  val wrSprOddSaIdx = Bool()
  val sprOddSaIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  val sprOddSbIdx = Flow(UInt(cfg.numGprsSprsPow bits))
  //--------

  val raIdx = /*Flow*/(UInt(cfg.numGprsSprsPow bits))
  val rbIdx = /*Flow*/(UInt(cfg.numGprsSprsPow bits))
  val indexRaIdx = UInt(cfg.numGprsSprsPow bits)
  val indexRbIdx = UInt(cfg.numGprsSprsPow bits)
  val decodeTemp = FlareCpuDecodeTemp(cfg=cfg)
  //val saIdx = /*Flow*/(UInt(cfg.numGprsSprsPow bits))
  //val sbIdx = /*Flow*/(UInt(cfg.numGprsSprsPow bits))
  //--------
}
//case class FlareCpuInstrDecEtc(
//  cfg: FlareCpuParams,
//  //decodeIo: FlareCpuPsDecodeIo,
//  //isDecode: Boolean,
//) extends Bundle {
//  //--------
//  // This `Bundle` also includes register values read from the two
//  // register files
//  //--------
//  //val isNop = Bool()
//  val isInvalid = Bool()
//  val haveFullInstr = Bool()
//  val fullgrp = FlareCpuInstrFullgrpDec()
//  val fullSimm = UInt(cfg.mainWidth bits)
//  val fullImm = UInt(cfg.mainWidth bits)
//  val fullPcrelSimm = UInt(cfg.mainWidth bits)
//
//  def enumGprRa = 0 
//  def enumGprRb = 1
//  def enumGprRc = 2
//  def enumGprLr = 3
//  def enumGprFp = 4
//  def enumGprSp = 5
//  def enumSprSa = 6
//  def enumSprSb = 7
//  def enumSprFlags = 8
//  def enumSprIds = 9
//  def enumSprIra = 10
//  def enumSprIe = 11
//  def enumSprIty = 12
//  def enumSprSty = 13
//  def enumGprRa64Hi = 14
//  def enumGprRa64Lo = 15
//  def enumGprRb64Hi = 16
//  def enumGprRb64Lo = 17
//
//  //val nonFwdGprVec = Vec.fill(cfg.numGprsSprs)(
//  //  UInt(cfg.mainWidth bits)
//  //)
//  ////val fwdGprVec = Vec.fill(cfg.numGprsSprs)(
//  ////  UInt(cfg.mainWidth bits)
//  ////)
//  //val nonFwdSprVec = Vec.fill(cfg.numGprsSprs)(
//  //  UInt(cfg.mainWidth bits)
//  //)
//  ////val fwdSprVec = Vec.fill(cfg.numGprsSprs)(
//  ////  UInt(cfg.mainWidth bits)
//  ////)
//
//  val ra = UInt(cfg.mainWidth bits)         // `rA`
//  val rb = UInt(cfg.mainWidth bits)         // `rB`
//  val rc = UInt(cfg.mainWidth bits)         // `rC`
//  val gprLr = UInt(cfg.mainWidth bits)      // `lr`
//  val gprFp = UInt(cfg.mainWidth bits)      // `fp`
//  val gprSp = UInt(cfg.mainWidth bits)      // `sp`
//  //val rd = UInt(cfg.mainWidth bits)         // `rD`
//  val sa = UInt(cfg.mainWidth bits)         // `sA`
//  val sb = UInt(cfg.mainWidth bits)         // `sB`
//  val sprFlags = UInt(cfg.mainWidth bits)   // `flags`
//  val sprIds = UInt(cfg.mainWidth bits)     // `ids`
//  val sprIra = UInt(cfg.mainWidth bits)     // `ira`
//  val sprIe = UInt(cfg.mainWidth bits)      // `ie`
//  val sprIty = UInt(cfg.mainWidth bits)     // `ity`
//  val sprSty = UInt(cfg.mainWidth bits)     // `sty`
//
//
//  val raIdx = UInt(cfg.numGprsSprsPow bits) //
//  val rbIdx = UInt(cfg.numGprsSprsPow bits) //
//  val rcIdx = UInt(cfg.numGprsSprsPow bits) // 
//  //val rdIdx = UInt(cfg.numGprsSprsPow bits) // 
//
//  //val saIdx = UInt(cfg.numGprsSprsPow bits) //
//  //val sbIdx = UInt(cfg.numGprsSprsPow bits) //
//
//
//  val ra64Hi = UInt(cfg.mainWidth bits)     //
//  val ra64Lo = UInt(cfg.mainWidth bits)     //
//  val rb64Hi = UInt(cfg.mainWidth bits)     //
//  val rb64Lo = UInt(cfg.mainWidth bits)     //
//
//  //val ra64HiIdx = UInt(cfg.numGprsSprsPow bits)
//  //val ra64LoIdx = UInt(cfg.numGprsSprsPow bits)
//  //val rb64HiIdx = UInt(cfg.numGprsSprsPow bits)
//  //val rb64LoIdx = UInt(cfg.numGprsSprsPow bits)
//
//  def ra64HiIdx = Cat(
//    raIdx(raIdx.high downto 1),
//    False,
//  ).asUInt
//  def ra64LoIdx = Cat(
//    raIdx(raIdx.high downto 1),
//    True,
//  ).asUInt
//  def rb64HiIdx = Cat(
//    rbIdx(rbIdx.high downto 1),
//    False,
//  ).asUInt
//  def rb64LoIdx = Cat(
//    rbIdx(rbIdx.high downto 1),
//    True,
//  ).asUInt
//
//  //def doSet64Idxs(): Unit = {
//  //  ra64HiIdx := Cat(
//  //    raIdx(raIdx.high downto 1),
//  //    False,
//  //  ).asUInt
//  //  ra64LoIdx := Cat(
//  //    raIdx(raIdx.high downto 1),
//  //    True,
//  //  ).asUInt
//  //  rb64HiIdx := Cat(
//  //    rbIdx(rbIdx.high downto 1),
//  //    False,
//  //  ).asUInt
//  //  rb64LoIdx := Cat(
//  //    rbIdx(rbIdx.high downto 1),
//  //    True,
//  //  ).asUInt
//  //}
//  //def getNonFwdRegFunc(
//  //  decIdx: UInt,
//  //  isGpr: Boolean,
//  //  //whichReg: Int,
//  //  //isDecode: Boolean,
//  //): UInt = {
//  //  if (isGpr) (
//  //    nonFwdGprVec(decIdx)
//  //  ) else (
//  //    //io.rSprVec(decIdx)
//  //    nonFwdSprVec(decIdx)
//  //  )
//  //}
//
//  def doFwdAllRegs(
//    execPayload: /*Payload[*/FlareCpuPipePayloadExec/*]*/,
//    //someCtrlLink: CtrlLink,
//    fwdRc: Boolean,
//    extCond: Bool,
//    //isDecode: Boolean,
//    second: Option[(Bool, FlareCpuPipePayloadExec)]=None,
//    otherInstrDecEtc: Option[FlareCpuInstrDecEtc]=None,
//  )(
//    getNonFwdRegFunc: (
//      UInt,       // `decIdx`
//      Boolean,    // `isGpr`
//      //Int,        // `whichReg`
//    ) => UInt,
//    //getNonFwdRegFunc1: (
//    //  UInt,     // `decIdx`
//    //  Boolean,  // `isGpr`
//    //  //Int,      // `whichReg`
//    //) => UInt,
//  )
//  : Unit = {
//    //--------
//    def doFwdOneReg(
//      decIdx: UInt,
//      isGpr: Boolean,
//      nonFwdRegArg: Option[UInt],
//      //whichReg: Int,
//      //someCtrlLink: CtrlLink,
//    ) = {
//      //def innerFunc(
//      //  someExecPayload: FlareCpuPipePayloadExec,
//      //  someNonFwdReg: UInt,
//      //) = {
//      //  def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
//      //  def nonFwdReg = getNonFwdRegFunc(
//      //    decIdx,
//      //    isGpr,
//      //    //whichReg,
//      //  )
//      //  //def tempRegWb = cExWb(psExOutp).get(isGpr)
//      //  Mux[UInt](
//      //    (
//      //      decIdx === tempExOutp.regIdx
//      //      && tempExOutp.wrReg.fire
//      //      && extCond
//      //    ),
//      //    tempExOutp.wrReg.payload,
//      //    someNonFwdReg,
//      //  )
//      //}
//      val nonFwdReg = nonFwdRegArg match {
//        case Some(myNonFwdReg) => {
//          myNonFwdReg
//        }
//        case None => {
//          getNonFwdRegFunc(
//            decIdx,
//            isGpr,
//            //whichReg,
//          )
//        }
//      }
//      second match {
//        case Some(mySecond) => {
//          //val tempNonFwdReg = (
//          //  innerFunc(
//          //    someExecPayload=execPayload,
//          //    someNonFwdReg=getNonFwdRegFunc(
//          //      decIdx,
//          //      isGpr,
//          //      //whichReg,
//          //    ),
//          //  )
//          //)
//          //innerFunc(
//          //  someExecPayload=myExecPayload1,
//          //  someNonFwdReg=getNonFwdRegFunc(
//          //    decIdx,
//          //    isGpr,
//          //    //whichReg,
//          //  ),
//          //)
//          //Mux[UInt](
//          //  decIdx === tempExOutp
//          //)
//          def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
//
//          //def tempRegWb = cExWb(psExOutp).get(isGpr)
//          val condVec = Vec(Bool(), 2)
//          condVec(0) := (
//            decIdx === tempExOutp.regIdx
//            && tempExOutp.wrReg.fire
//            && extCond
//          )
//          condVec(1) := (
//            decIdx === mySecond._2.get(isGpr).regIdx
//            && mySecond._2.get(isGpr).wrReg.fire
//            && mySecond._1
//          )
//          val fwdVec = Vec(UInt(cfg.mainWidth bits), 2)
//          fwdVec(0) := tempExOutp.wrReg.payload
//          fwdVec(1) := mySecond._2.get(isGpr).wrReg.payload
//          val myFindFirst = condVec.sFindFirst(_ === True)
//          Mux[UInt](
//            myFindFirst._1,
//            //Mux[UInt](
//            //  myFindFirst._2 === 0,
//            //  tempExOutp.wrReg.payload,
//            //  mySecond._2.get(isGpr).wrReg.payload,
//            //),
//            fwdVec(myFindFirst._2),
//            nonFwdReg,
//          )
//          //condVec(2) := True
//          //Mux[UInt](
//          //  (
//          //  ),
//          //  Mux[UInt](
//          //    tempExOutp.wrReg.payload,
//          //    nonFwdReg,
//          //  ),
//          //  nonFwdReg
//          //)
//        }
//        case None => {
//          //innerFunc(
//          //  execPayload,
//          //  someNonFwdReg=getNonFwdRegFunc(
//          //    decIdx,
//          //    isGpr,
//          //    //whichReg,
//          //  ),
//          //)
//          def tempExOutp = /*someCtrlLink*/(execPayload).get(isGpr)
//          def nonFwdReg = getNonFwdRegFunc(
//            decIdx,
//            isGpr,
//            //whichReg,
//          )
//          //def tempRegWb = cExWb(psExOutp).get(isGpr)
//          Mux[UInt](
//            (
//              decIdx === tempExOutp.regIdx
//              && tempExOutp.wrReg.fire
//              && extCond
//            ),
//            tempExOutp.wrReg.payload,
//            nonFwdReg,
//          )
//        }
//      }
//    }
//    //--------
//    //for (idx <- 0 until cfg.numGprsSprs) {
//    //  fwdGprVec(idx) := doFwdOneReg(
//    //    decIdx=idx,
//    //    isGpr=true,
//    //  )
//    //  fwdSprVec(idx) := doFwdOneReg(
//    //    decIdx=idx,
//    //    isGpr=false,
//    //  )
//    //}
//    if (!fwdRc) {
//      ra := doFwdOneReg(
//        decIdx=raIdx,
//        isGpr=true,
//        //whichReg=enumGprRa,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.ra)
//          case None => None
//        },
//      )
//      rb := doFwdOneReg(
//        decIdx=rbIdx,
//        isGpr=true,
//        //whichReg=enumGprRb,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.rb)
//          case None => None
//        },
//      )
//      gprLr := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.gprLrIdx,
//        isGpr=true,
//        //whichReg=enumGprLr,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.gprLr)
//          case None => None
//        },
//      )
//      gprFp := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.gprFpIdx,
//        isGpr=true,
//        //whichReg=enumGprFp,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.gprFp)
//          case None => None
//        },
//      )
//      gprSp := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.gprSpIdx,
//        isGpr=true,
//        //whichReg=enumGprSp,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.gprSp)
//          case None => None
//        },
//      )
//      sa := doFwdOneReg(
//        decIdx=raIdx,
//        isGpr=false,
//        //whichReg=enumSprSa,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sa)
//          case None => None
//        },
//      )
//      sb := doFwdOneReg(
//        decIdx=rbIdx,
//        isGpr=false,
//        //whichReg=enumSprSa,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sb)
//          case None => None
//        },
//      )
//      sprFlags := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprFlagsIdx,
//        isGpr=false,
//        //whichReg=enumSprFlags,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprFlags)
//          case None => None
//        },
//      )
//      sprIds := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprIdsIdx,
//        isGpr=false,
//        //whichReg=enumSprIds,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprIds)
//          case None => None
//        },
//      )
//      sprIra := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprIraIdx,
//        isGpr=false,
//        //whichReg=enumSprIra,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprIra)
//          case None => None
//        },
//      )
//      sprIe := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprIeIdx,
//        isGpr=false,
//        //whichReg=enumSprIe,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprIe)
//          case None => None
//        },
//      )
//      sprIty := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprItyIdx,
//        isGpr=false,
//        //whichReg=enumSprIty,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprIty)
//          case None => None
//        },
//      )
//      sprSty := doFwdOneReg(
//        decIdx=FlareCpuInstrEncConst.sprStyIdx,
//        isGpr=false,
//        //whichReg=enumSprSty,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.sprSty)
//          case None => None
//        },
//      )
//
//      ra64Hi := doFwdOneReg(
//        decIdx=ra64HiIdx,
//        isGpr=true,
//        //whichReg=enumGprRa64Hi
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.ra64Hi)
//          case None => None
//        },
//      )
//      ra64Lo := doFwdOneReg(
//        decIdx=ra64LoIdx,
//        isGpr=true,
//        //whichReg=enumGprRa64Lo,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.ra64Lo)
//          case None => None
//        },
//      )
//      rb64Hi := doFwdOneReg(
//        decIdx=rb64HiIdx,
//        isGpr=true,
//        //whichReg=enumGprRb64Hi,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.rb64Hi)
//          case None => None
//        },
//      )
//      rb64Lo := doFwdOneReg(
//        decIdx=rb64LoIdx,
//        isGpr=true,
//        //whichReg=enumGprRb64Lo,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.rb64Lo)
//          case None => None
//        },
//      )
//    } else { // if (fwdRc)
//      rc := doFwdOneReg(
//        decIdx=rcIdx,
//        isGpr=true,
//        //whichReg=enumGprRc,
//        nonFwdRegArg=otherInstrDecEtc match {
//          case Some(myOtherInstrDecEtc) => Some(myOtherInstrDecEtc.rc)
//          case None => None
//        },
//      )
//    }
//    //--------
//  }
//  //--------
//  val instrEnc = FlareCpuInstrEnc(cfg=cfg)
//  //--------
//}
