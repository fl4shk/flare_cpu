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

case class Flare32CpuPipePayloadExec(
  params: Flare32CpuParams,
) extends Bundle {
  //--------
  // when multiple register need to be written to by one instruction
  // (lumul, sdiv64, etc.), serialize sending new register values to WB
  //val gprIdx = UInt(numGprsSprsPow bits)
  //val wrGpr = Flow(UInt(mainWidth bits))
  ////--------
  //val sprIdx = UInt(numGprsSprsPow bits)
  //val wrSpr = Flow(UInt(mainWidth bits))
  //--------
  case class WrReg() extends Bundle {
    val regIdx = UInt(params.numGprsSprsPow bits)
    val wrReg = Flow(UInt(params.mainWidth bits))
  }
  val gpr = WrReg()
  val spr = WrReg()
  //--------
  def get(
    isGpr: Boolean
  ) = (
    if (isGpr) {gpr} else {spr}
  )
  //--------
  val addr = UInt(params.mainWidth bits) // for loads/stores
  //--------
}

case class Flare32CpuExecIo(
  params: Flare32CpuParams,
) extends Area {
  //--------
  ////val front = Node()
  //val frontPayload = Payload(Flare32CpuPipePayload(params=params))
  ////val back = Node()
  //val backPayload = Payload(Flare32CpuPipePayload(params=params))
  //val currPayload = Payload(Flare32CpuPipePayload(params=params))
  //--------
}
case class Flare32CpuPsExec(
  params: Flare32CpuParams,
  prevPayload: Payload[Flare32CpuPipePayload],
  currPayload: Payload[Flare32CpuPipePayload],
  cPrevCurr: CtrlLink,
  cCurrNext: CtrlLink,
  decodeIo: Flare32CpuDecodeIo,
) extends Area {
  //--------
  val io = Flare32CpuExecIo(params=params)
  //--------
  val cPrevCurrArea = new cPrevCurr.Area {
    val upPayload = Vec.fill(2)(Flare32CpuPipePayload(params=params))
    //upPayload := RegNext(upPayload) init(upPayload.getZero)
    for (idx <- 0 until upPayload.size) {
      upPayload(idx) := (
        RegNext(upPayload(idx))
        init(upPayload(idx).getZero)
      )
    }
    upPayload(1).allowOverride
    up(currPayload) := upPayload(1)

    when (up.isFiring) {
      upPayload(0) := up(prevPayload)
      upPayload(1) := upPayload(0)
      when (!decodeIo.rExecSetPc.valid) {
        //def myInstrDecEtc = cIdEx.down(instrDecEtc)
        def myInstrDecEtc = up(prevPayload).decode.instrDecEtc

        /*switch (myInstrDecEtc.fullgrp)*/ 
        if (true)
        {
          def myPc = up(prevPayload).decode.pc //cIdEx.down(pc)
          def myPcPlus2 = up(prevPayload).decode.pcPlus2 //cIdEx.down(pcPlus2)
          def simm = myInstrDecEtc.fullSimm
          def imm = myInstrDecEtc.fullImm
          //def wrGpr = cIdEx(psExOutp).get(isGpr=true)
          //def wrSpr = cIdEx(psExOutp).get(isGpr=false)
          def doWriteReg(
            regIdx: UInt,
            payload: UInt,
            isGpr: Boolean,
          ): Unit = {
            def myWrReg = (
              //if (isGpr) {
              //  wrGpr
              //} else { // if (!isGpr)
              //  wrSpr
              //}
              //cIdEx(psExOutp).get(isGpr)
              //up(prevPayload).exec.get(isGpr)
              upPayload(1).exec.get(isGpr)
              //down(currPayload).exec.get(isGpr=isGpr)
            )
            myWrReg.regIdx := regIdx
            myWrReg.wrReg.valid := True
            myWrReg.wrReg.payload := payload
          }
          def doWriteGpr(
            regIdx: UInt,
            payload: UInt,
          ): Unit = doWriteReg(
            regIdx=regIdx,
            payload=payload,
            isGpr=true,
          )
          def doWriteSpr(
            regIdx: UInt,
            payload: UInt,
          ): Unit = doWriteReg(
            regIdx=regIdx,
            payload=payload,
            isGpr=false,
          )

          //def doFwdExReg(
          //  decIdx: UInt,
          //  nonFwdReg: UInt,
          //  isGpr: Boolean,
          //) = {
          //  doFwdReg(
          //    decIdx=decIdx,
          //    nonFwdReg=nonFwdReg,
          //    isGpr=isGpr,
          //    someCtrlLink=cIdEx,
          //  )
          //}

          //def doFwdExGpr64Half(
          //  decIdx: UInt,
          //  nonFwdGpr64Half: UInt,
          //  isHi: Boolean,
          //) = {
          //  doFwdGpr64Half(
          //    decIdx=decIdx,
          //    nonFwdGpr64Half=nonFwdGpr64Half,
          //    isHi=isHi,
          //    someCtrlLink=cIdEx,
          //  )
          //}

          val tempInstrDecEtc = Flare32CpuInstrDecEtc(params=params)
          tempInstrDecEtc := myInstrDecEtc
          tempInstrDecEtc.allowOverride
          def getNonFwdRegFunc(
            decIdx: UInt,
            isGpr: Boolean,
            //whichReg: Int,
          ): UInt = {
            if (isGpr) {
              decodeIo.rGprVec(decIdx)
            } else {
              decodeIo.rSprVec(decIdx)
            }
          }
          //--------
          tempInstrDecEtc.doFwdAllRegs(
            //someCtrlLink=cIdEx,
            //execPayload=up(prevPayload)
            execPayload=upPayload(1).exec,
            fwdRc=false,
            extCond=(
              //up.isFiring
              up.isValid
              //down.isFiring
            ),
            second=Some(
              (
                //cCurrNext.up.isFiring,
                cCurrNext.up.isValid,
                cCurrNext.up(currPayload).exec,
              )
            )
          )(
            getNonFwdRegFunc=getNonFwdRegFunc
          )
          //--------
          def ra = tempInstrDecEtc.ra
          def rb = tempInstrDecEtc.rb
          def rc = tempInstrDecEtc.rc
          def lr = tempInstrDecEtc.gprLr
          def fp = tempInstrDecEtc.gprFp
          def sp = tempInstrDecEtc.gprSp
          //def rd = tempInstrDecEtc.rd
          def sa = tempInstrDecEtc.sa
          def sb = tempInstrDecEtc.sb
          def flags = tempInstrDecEtc.sprFlags
          def ids = tempInstrDecEtc.sprIds
          def ira = tempInstrDecEtc.sprIra
          def ie = tempInstrDecEtc.sprIe
          def ity = tempInstrDecEtc.sprIty
          def sty = tempInstrDecEtc.sprSty
          def raIdx = tempInstrDecEtc.raIdx
          def rbIdx = tempInstrDecEtc.rbIdx
          def rcIdx = tempInstrDecEtc.rcIdx
          //val rdIdx = UInt(numGprsSprsPow bits) // 

          //val saIdx = UInt(numGprsSprsPow bits) //
          //val sbIdx = UInt(numGprsSprsPow bits) //


          def ra64Hi = tempInstrDecEtc.ra64Hi
          def ra64Lo = tempInstrDecEtc.ra64Lo
          def rb64Hi = tempInstrDecEtc.rb64Hi
          def rb64Lo = tempInstrDecEtc.rb64Lo

          //val ra64HiIdx = UInt(numGprsSprsPow bits)
          //val ra64LoIdx = UInt(numGprsSprsPow bits)
          //val rb64HiIdx = UInt(numGprsSprsPow bits)
          //val rb64LoIdx = UInt(numGprsSprsPow bits)

          def ra64HiIdx = tempInstrDecEtc.ra64HiIdx
          def ra64LoIdx = tempInstrDecEtc.ra64LoIdx
          def rb64HiIdx = tempInstrDecEtc.rb64HiIdx
          def rb64LoIdx = tempInstrDecEtc.rb64LoIdx
          //def raIdx = upInstrDecEtc.raIdx

          //def ra = doFwdExReg(
          //  decIdx=raIdx,
          //  nonFwdReg=upInstrDecEtc.ra,
          //  isGpr=true,
          //)
          ////val ra = Mux[UInt](
          ////  !(
          ////    raIdx === cIdEx(psExOutp).gprIdx
          ////    && nWb(psExOutp).wrGpr.valid
          ////  ),
          ////  upInstrDecEtc.ra,
          ////  nWb(psExOutp).wrGpr.payload,
          ////)
          //def rbIdx = upInstrDecEtc.rbIdx
          //def rb = doFwdExReg(
          //  decIdx=rbIdx,
          //  nonFwdReg=upInstrDecEtc.rb,
          //  isGpr=true,
          //)
          //def rcIdx = upInstrDecEtc.rcIdx
          //def rc = doFwdExReg(
          //  decIdx=rcIdx,
          //  nonFwdReg=upInstrDecEtc.rc,
          //  isGpr=true,
          //)

          ////def rdIdx = upInstrDecEtc.rdIdx
          ////def rd = doFwdExReg(
          ////  decIdx=rdIdx,
          ////  someReg=upInstrDecEtc.rd,
          ////  isGpr=true,
          ////)
          ////def saIdx = upInstrDecEtc.saIdx
          //def sa = doFwdExReg(
          //  decIdx=raIdx,
          //  nonFwdReg=upInstrDecEtc.sa,
          //  isGpr=false,
          //)
          ////def sbIdx = upInstrDecEtc.sbIdx
          //def sb = doFwdExReg(
          //  decIdx=rbIdx,
          //  nonFwdReg=upInstrDecEtc.sb,
          //  isGpr=false,
          //)

          //def ra64HiIdx = upInstrDecEtc.ra64HiIdx
          //def ra64Hi = doFwdExReg(
          //  decIdx=ra64HiIdx,
          //  nonFwdReg=upInstrDecEtc.ra64Hi,
          //  isGpr=true,
          //)
          //def ra64LoIdx = upInstrDecEtc.ra64LoIdx
          //def ra64Lo = doFwdExReg(
          //  decIdx=ra64LoIdx,
          //  nonFwdReg=upInstrDecEtc.ra64Lo,
          //  isGpr=true,
          //)

          //def rb64HiIdx = upInstrDecEtc.rb64HiIdx
          //def rb64Hi = doFwdExReg(
          //  decIdx=rb64HiIdx,
          //  nonFwdReg=upInstrDecEtc.rb64Hi,
          //  isGpr=true,
          //)
          //def rb64LoIdx = upInstrDecEtc.rb64LoIdx
          //def rb64Lo = doFwdExReg(
          //  decIdx=rb64LoIdx,
          //  nonFwdReg=upInstrDecEtc.rb64Lo,
          //  isGpr=true,
          //)
          //def ra64Hi = doFwdExGpr64Half(
          //  decIdx=ra64HiIdx
          //)
          //def ra = rGprVec(cIdEx.down(instrDecEtc).raIdx)
          //def rb = rGprVec(cIdEx.down(instrDecEtc).rbIdx)
          ////def rc = rGprVec(cIdEx.down(instrDecEtc).rcIdx)
          //def lr = rGprVec(InstrEncConst.gprLrIdx)
          //def fp = rGprVec(InstrEncConst.gprFpIdx)
          //def sp = rGprVec(InstrEncConst.gprSpIdx)
          //def sa = rSprVec(cIdEx.down(instrDecEtc).raIdx)
          //def sb = rSprVec(cIdEx.down(instrDecEtc).rbIdx)
          ////def sc = rSprVec(cIdEx.down(instrDecEtc).rcIdx)
          //def flags = rSprVec(InstrEncConst.sprFlagsIdx)
          //def ids = rSprVec(InstrEncConst.sprIdsIdx)
          //def ira = rSprVec(InstrEncConst.sprIraIdx)
          //def ie = rSprVec(InstrEncConst.sprIeIdx)
          //def ity = rSprVec(InstrEncConst.sprItyIdx)
          //def sty = rSprVec(InstrEncConst.sprStyIdx)

          //def performFetch(): Unit = {
          //}
          //def performLoad(
          //  rawElemNumBytesPow: (Int, Int),
          //  dst: UInt,
          //): Unit = {
          //}
          //def performStore(
          //  rawElemNumBytesPow: (Int, Int),
          //  src: UInt,
          //): Unit = {
          //}
          def performSetFlagsZn(
            rawElemNumBytesPow: (Int, Int),
            result: UInt,
            //flagsOut: UInt,
          ): Unit = {
            //--------
            def myBits = params.elemNumBytesPow(
              rawElemNumBytesPow=rawElemNumBytesPow
            )._2
            flags(params.flagIdxZ) := (result(myBits - 1 downto 0) === 0)
            flags(params.flagIdxN) := result(myBits - 1)
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
                    flags(params.flagIdxC downto params.flagIdxC)
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
                    flags(params.flagIdxC downto params.flagIdxC)
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
              tempFlagsOut(params.flagIdxC) := result(myBits)
              tempFlagsOut(params.flagIdxV) := (
                (tempOperandA ^ result.resized)
                & (tempOperandB ^ result.resized)
              )(myBits - 1)
            }
            //--------
          }
          //--------
          switch (myInstrDecEtc.fullgrp) {
            is (Flare32CpuInstrFullgrpDec.g0Pre) {
            }
            is (Flare32CpuInstrFullgrpDec.g0Lpre) {
            }
            is (Flare32CpuInstrFullgrpDec.g1) {
              switch (
                //cIdEx.down(instrDecEtc).instrG1Enc.op
                tempInstrDecEtc.instrEnc.g1.op
              ) {
                is (Flare32CpuInstrG1EncOp.addRaS5) {    // Opcode 0x0: add rA, #simm5
                  ra := ra + simm
                }
                is (Flare32CpuInstrG1EncOp.addRaPcS5) {  // Opcode 0x1: add rA, pc, #simm5
                  ra := myPcPlus2 + simm
                }
                is (Flare32CpuInstrG1EncOp.addRaSpS5) {  // Opcode 0x2: add rA, sp, #simm5
                  ra := sp + simm
                }
                is (Flare32CpuInstrG1EncOp.addRaFpS5) {  // Opcode 0x3: add rA, fp, #simm5
                  ra := fp + simm
                }
                is (Flare32CpuInstrG1EncOp.cmpRaS5) {    // Opcode 0x4: cmp rA, #simm5
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG1EncOp.cpyRaS5) {    // Opcode 0x5: cpy rA, #simm5
                  ra := simm
                }
                is (Flare32CpuInstrG1EncOp.lslRaI5) {    // Opcode 0x6: lsl rA, #imm5
                  ra := ra << imm
                }
                is (Flare32CpuInstrG1EncOp.lsrRaI5) {    // Opcode 0x7: lsr rA, #imm5
                  ra := ra >> imm
                }
                is (Flare32CpuInstrG1EncOp.asrRaI5) {    // Opcode 0x8: asr rA, #imm5
                  ra := (ra.asSInt >> imm).asUInt
                }
                is (Flare32CpuInstrG1EncOp.andRaS5) {    // Opcode 0x9: and rA, #simm5
                  ra := ra & simm
                }
                is (Flare32CpuInstrG1EncOp.orrRaS5) {    // Opcode 0xa: orr rA, #simm5
                  ra := ra | simm
                }
                is (Flare32CpuInstrG1EncOp.xorRaS5) {    // Opcode 0xb: xor rA, #simm5
                  ra := ra ^ simm
                }
                is (Flare32CpuInstrG1EncOp.zeRaI5) {     // Opcode 0xc: ze rA, #imm5
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
                  def tempShiftAmount = params.mainWidth - imm
                  ra := (ra << tempShiftAmount) >> tempShiftAmount
                }
                is (Flare32CpuInstrG1EncOp.seRaI5) {     // Opcode 0xd: se rA, #imm5
                  //switch (imm) {
                  //  for (value <- 0 until (1 << nonG3ImmWidth)) {
                  //    is (value) {
                  //      ra := ra.asSInt(value - 1 downto 0).resized.asUInt
                  //    }
                  //  }
                  //}
                  //ra := ra & ((1 << imm) - 1)
                  def tempShiftAmount = params.mainWidth - imm
                  ra := (
                    ((ra.asSInt << tempShiftAmount)
                    >> tempShiftAmount).asUInt
                  )
                }
                is (Flare32CpuInstrG1EncOp.swiRaS5) {    // Opcode 0xe: swi rA, #simm5
                }
                is (Flare32CpuInstrG1EncOp.swiI5) {      // Opcode 0xf: swi #simm5
                }
              }
            }
            is (Flare32CpuInstrFullgrpDec.g2) {
              switch (
                //cIdEx.down(instrDecEtc).instrG2Enc.op
                tempInstrDecEtc.instrEnc.g2.op
              ) {
                def f = (
                  //cIdEx.down(instrDecEtc).instrG2Enc.f
                  tempInstrDecEtc.instrEnc.g2.f
                )
                is (Flare32CpuInstrG2EncOp.addRaRb) {   // Opcode 0x0: add rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.subRaRb) {   // Opcode 0x1: sub rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.addRaSpRb) { // Opcode 0x2: add rA, sp, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.addRaFpRb) { // Opcode 0x3: add rA, fp, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.cmpRaRb) {   // Opcode 0x4: cmp rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.cpyRaRb) {   // Opcode 0x5: cpy rA, rB
                  //ra := rb
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.lslRaRb) {   // Opcode 0x6: lsl rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := ra << rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.lsrRaRb) {   // Opcode 0x7: lsr rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := ra >> rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.asrRaRb) {   // Opcode 0x8: asr rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := (ra.asSInt >> rb).asUInt
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.andRaRb) {   // Opcode 0x9: and rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := ra & rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.orrRaRb) {   // Opcode 0xa: orr rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := ra | rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.xorRaRb) {   // Opcode 0xb: xor rA, rB
                  val tempResult = UInt(params.mainWidth bits)
                  tempResult := ra ^ rb
                  ra := tempResult
                  when (f) {
                    performSetFlagsZn(
                      rawElemNumBytesPow=params.rawElemNumBytesPow32,
                      result=tempResult,
                    )
                  }
                }
                is (Flare32CpuInstrG2EncOp.adcRaRb) {   // Opcode 0xc: adc rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.sbcRaRb) {   // Opcode 0xd: sbc rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
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
                is (Flare32CpuInstrG2EncOp.cmpbcRaRb) { // Opcode 0xe: cmpbc rA, rB
                  val tempResult = UInt((params.mainWidth + 1) bits)
                  def tempRawEleNumBytesPow = params.rawElemNumBytesPow32
                  def tempOperandA = ra
                  def tempOperandB = rb
                  def tempWithCarryIn = true
                  def tempDoSub = true
                  def tempDoSetFlags = true
                  val tempFlagsOut = UInt(params.mainWidth bits)

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
                is (Flare32CpuInstrG2EncOp.invalid0) {  // Opcode 0xf: invalid operation 0
                }
              }
            }
            is (Flare32CpuInstrFullgrpDec.g3) {
              def someNextPc = Cat(
                //cIdEx.down(instrDecEtc).fullPcrelSimm(
                //  params.mainWidth - 1 downto 1
                //),
                tempInstrDecEtc.fullPcrelSimm(
                  params.mainWidth - 1 downto 1
                ),
                False,
              ).asUInt
              switch (
                //cIdEx.down(instrDecEtc).instrG3Enc.op
                tempInstrDecEtc.instrEnc.g3.op
              ) {
                def doSetPc(): Unit = {
                  decodeIo.rExecSetPc.valid := True
                  decodeIo.rExecSetPc.payload := someNextPc
                }
                is (Flare32CpuInstrG3EncOp.blS9) {       // Opcode 0x0: bl simm9
                  doSetPc()
                  //lr := cIdEx.down(pcPlus2)
                  lr := up(prevPayload).decode.pcPlus2
                }
                is (Flare32CpuInstrG3EncOp.braS9) {      // Opcode 0x1: bra simm9
                  doSetPc()
                }
                is (Flare32CpuInstrG3EncOp.beqS9) {      // Opcode 0x2: beq simm9
                  when (flags(params.flagIdxZ)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bneS9) {      // Opcode 0x3: bne simm9
                  when (!flags(params.flagIdxZ)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bmiS9) {      // Opcode 0x4: bmi simm9
                  when (flags(params.flagIdxN)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bplS9) {      // Opcode 0x5: bpl simm9
                  when (!flags(params.flagIdxN)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bvsS9) {      // Opcode 0x6: bvs simm9
                  when (flags(params.flagIdxV)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bvcS9) {      // Opcode 0x7: bvc simm9
                  when (!flags(params.flagIdxV)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bgeuS9) {     // Opcode 0x8: bgeu simm9
                  when (flags(params.flagIdxC)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bltuS9) {     // Opcode 0x9: bltu simm9
                  when (!flags(params.flagIdxC)) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bgtuS9) {     // Opcode 0xa: bgtu simm9
                  when (
                    flags(params.flagIdxC) && !flags(params.flagIdxZ)
                  ) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bleuS9) {     // Opcode 0xb: bleu simm9
                  when (
                    !flags(params.flagIdxC) || flags(params.flagIdxZ)
                  ) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bgesS9) {     // Opcode 0xc: bges simm9
                  when (
                    flags(params.flagIdxN) === flags(params.flagIdxV)
                  ) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bltsS9) {     // Opcode 0xd: blts simm9
                  when (
                    flags(params.flagIdxN) =/= flags(params.flagIdxV)
                  ) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.bgtsS9) {     // Opcode 0xe: bgts simm9
                  when (
                    flags(params.flagIdxN) === flags(params.flagIdxV)
                    && !flags(params.flagIdxZ)
                  ) {
                    doSetPc()
                  }
                }
                is (Flare32CpuInstrG3EncOp.blesS9) {     // Opcode 0xf: bles simm9
                  when (
                    flags(params.flagIdxN) =/= flags(params.flagIdxV)
                    || flags(params.flagIdxZ)
                  ) {
                    doSetPc()
                  }
                }
              }
            }
            is (Flare32CpuInstrFullgrpDec.g4) {
              switch (tempInstrDecEtc.instrEnc.g4.op) {
                //--------
                is (Flare32CpuInstrG4EncOp.jlRa) {         // Opcode 0x0: jl rA
                  decodeIo.rExecSetPc.valid := True
                  decodeIo.rExecSetPc.payload := ra
                  //lr := cIdEx.down(pcPlus2)
                  lr := up(prevPayload).decode.pcPlus2
                }
                is (Flare32CpuInstrG4EncOp.jmpRa) {        // Opcode 0x1: jmp rA
                  decodeIo.rExecSetPc.valid := True
                  decodeIo.rExecSetPc.payload := ra
                }
                is (Flare32CpuInstrG4EncOp.jmpIra) {       // Opcode 0x2: jmp ira
                  decodeIo.rExecSetPc.valid := True
                  decodeIo.rExecSetPc.payload := ira
                }
                is (Flare32CpuInstrG4EncOp.reti) {         // Opcode 0x3: reti
                  decodeIo.rExecSetPc.valid := True
                  decodeIo.rExecSetPc.payload := ira
                  ie := U(default -> True)
                }
                is (Flare32CpuInstrG4EncOp.ei) {  // Opcode 0x4: ei
                  ie := U(default -> True)
                }
                is (Flare32CpuInstrG4EncOp.di) {  // Opcode 0x5: di
                  ie := U(default -> False)
                }
                is (Flare32CpuInstrG4EncOp.pushRaRb) { // Opcode 0x6: push rA, rB
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.pushSaRb) { // Opcode 0x7: push sA, rB
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.popRaRb) { // Opcode 0x8: pop rA, rB
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.popSaRb) { // Opcode 0x9: pop sA, rB
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.popPcRb) { // Opcode 0xa: pop pc, rB
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.mulRaRb) { // Opcode 0xb: mul rA, rB
                  ra := ra * rb
                }
                is (Flare32CpuInstrG4EncOp.udivRaRb) { // Opcode 0xc: udiv rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.sdivRaRb) { // Opcode 0xd: sdiv rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.umodRaRb) { // Opcode 0xe: umod rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.smodRaRb) { // Opcode 0xf: smod rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                //--------
                is (Flare32CpuInstrG4EncOp.lumulRaRb) { // Opcode 0x10: lumul rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.lsmulRaRb) { // Opcode 0x11: lsmul rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.udiv64RaRb) { // Opcode 0x12: udiv64 rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.sdiv64RaRb) { // Opcode 0x13: sdiv64 rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.umod64RaRb) { // Opcode 0x14: umod64 rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.smod64RaRb) { // Opcode 0x15: smod64 rA, rB
                  haltIt()
                  //cIdEx.haltIt()
                }
                is (Flare32CpuInstrG4EncOp.ldubRaRb) { // Opcode 0x16: ldub rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.ldsbRaRb) { // Opcode 0x17: ldsb rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.lduhRaRb) { // Opcode 0x18: lduh rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.ldshRaRb) { // Opcode 0x19: ldsh rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.stbRaRb) { // Opcode 0x1a: stb rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.sthRaRb) { // Opcode 0x1b: sth rA, [rB]
                  haltIt()
                }
                is (Flare32CpuInstrG4EncOp.cpyRaSb) { // Opcode 0x1c: cpy rA, sB
                  ra := sb
                }
                is (Flare32CpuInstrG4EncOp.cpySaRb) { // Opcode 0x1d: cpy sA, rB
                  sa := rb
                }
                is (Flare32CpuInstrG4EncOp.cpySaSb) { // Opcode 0x1e: cpy sA, sB
                  sa := sb
                }
                is (Flare32CpuInstrG4EncOp.indexRa) { // Opcode 0x1f: index rA
                  haltIt()
                }
                //--------
              }
            }
            is (Flare32CpuInstrFullgrpDec.g5) {
            }
            is (Flare32CpuInstrFullgrpDec.g6) {
            }
            is (Flare32CpuInstrFullgrpDec.g7Sg00) {
              switch (tempInstrDecEtc.instrEnc.g7Sg00.op) {
              }
            }
            is (Flare32CpuInstrFullgrpDec.g7Sg010) {
              switch (tempInstrDecEtc.instrEnc.g7Sg010.op) {
              }
            }
            is (Flare32CpuInstrFullgrpDec.g7Sg0110) {
            }
            default {
              // eek!
            }
          }
        }
      } otherwise {
        //cIdEx.throwWhen(psExSetPc.valid)
        cPrevCurr.throwIt() // cancel the current transaction
        //cIdEx.terminateIt() // clear `cIfId.down.valid`
        decodeIo.rExecSetPc.valid := False
      }
    }
  }
  //--------
}
