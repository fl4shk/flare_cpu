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
import libcheesevoyage.general.{
  PipeMemRmwConfig, PipeMemRmw, PipeMemRmwSimDut
}
import libcheesevoyage.math.LongDivMultiCycle
import libcheesevoyage.bus.lcvStall.{
  LcvStallIo, LcvStallHost, LcvStallHostSaved
}
import libsnowhouse._
import java.io.{FileInputStream, BufferedInputStream}


object FlareCpuFormalInstrCnt {
  def cntWidth = 8
}
object FlareCpuPipeStageInstrDecode {
  def apply(
    cfg: FlareCpuConfig,
    psId: SnowHousePipeStageInstrDecode
  ) = new Area {
    //--------
    // NOTE: the `KeepAttribute(...)`s seem to be required for signals
    // defined in this function.
    //--------
    import FlareCpuOp._
    def upPayload = psId.upPayload
    def io = psId.io
    def shCfg = psId.cfg
    def cId = psId.cId
    //val encInstr = FlareCpuInstrEnc(cfg=cfg)
    //encInstr.assignFromBits(
    //)
    val encInstr = (
      KeepAttribute(
        FlareCpuInstrEnc(cfg=cfg)
      )
      .setName("InstrDecode_encInstr")
    )
    encInstr.assignFromBits(
      ////io.ibus.devData.instr.asBits
      //Mux[Bits](
      //  !psId.shouldBubble,
        psId.tempInstr.asBits,
      //  psId.tempInstr.asBits.getZero
      //)
    )
    //object State
    //extends SpinalEnum(defaultEncoding=binarySequential) {
    //  val
    //    MAIN,
    //    HAVE_PRE,
    //    HAVE_LPRE,
    //    HAVE_INDEX_REG,
    //    HAVE_INDEX_SIMM
    //    = newElement()
    //}
    val rDecodeTemp = (
      KeepAttribute({
        val temp = Reg(FlareCpuDecodeTemp(cfg=cfg))
        temp.init(temp.getZero)
        temp
      })
      .setName(s"InstrDecode_rDecodeTemp")
    )
    val nextState = Bool()
    val rState = (
      KeepAttribute({
        RegNext(
          next=nextState,
          //init=nextState.getZero,
        )
      })
      .setName(s"InstrDecode_rMultiCycleState")
    )
    nextState := rState
    for (idx <- 0 until shCfg.maxNumGprsPerInstr) {
      upPayload.gprIsZeroVec(idx) := (
        upPayload.gprIdxVec(idx) === 0x0
      )
    }
    upPayload.gprIdxVec.foreach(gprIdx => {
      gprIdx := RegNext(
        next=gprIdx,
        init=gprIdx.getZero,
      )
    })
    //upPayload.gprIdxVec(0) := encInstr.raIdx
    //upPayload.gprIdxVec(1) := encInstr.rbIdx
    //upPayload.gprIdxVec(2) := encInstr.rcIdx
    //val tempImm = Cat(
    //  Mux[UInt](
    //    encInstr.imm16.msb,
    //    U"16'hffff",
    //    U"16'h0000",
    //  ),
    //  encInstr.imm16.asSInt
    //).asUInt
    //val tempImm = UInt(cfg.mainWidth bits)
    //tempImm := (
    //  Cat(
    //    Mux[UInt](
    //      encInstr.imm16.msb,
    //      U"16'hffff",
    //      U"16'h0000",
    //    ),
    //    encInstr.imm16.asSInt
    //  ).asUInt
    //)
    //val rPrevPreImm = (
    //  KeepAttribute(
    //    RegNextWhen(
    //      next=Cat(
    //        encInstr.raIdx,
    //        encInstr.rbIdx,
    //        encInstr.rcIdx,
    //        encInstr.imm16
    //      ).asUInt,
    //      cond=cId.up.isFiring,
    //      //init=encInstr.imm16.getZero,
    //    )
    //  )
    //  .setName(s"InstrDecode_rPrevPreImm")
    //)
    val rPrevPrefixImm = (
      KeepAttribute(
        Reg(UInt(cfg.mainWidth bits))
        init(0x0)
      )
      .setName(s"InstrDecode_rPrevPrefixImm")
    )
    //switch (rState) {
    //  is (False) {
    //    upPayload.imm := tempImm
    //  }
    //  is (True) {
    //    upPayload.imm := (
    //      Cat(
    //        rPrevPreImm,
    //        encInstr.imm16,
    //      ).asUInt.resized
    //    )
    //    when (cId.up.isFiring) {
    //      upPayload.blockIrq := False
    //      nextState := False
    //    }
    //  }
    //}
    def setOp(
      someOp: (Int, String),
      //someOutpOp: UInt=upPayload.op,
    ): Area = new Area {
      setName(s"setOp_${someOp._1}")
      var found = false
      for (
        ((tuple, opInfo), opInfoIdx) <- shCfg.opInfoMap.view.zipWithIndex
      ) {
        if (someOp == tuple) {
          val mySplitOp = upPayload.splitOp
          //for ((src, srcIdx) <- opInfo.srcArr.view.zipWithIndex) {
          //  mySplitOp.srcKindVec
          //}
          for (
            ((_, cpyOpInfo), cpyOpInfoIdx)
            <- shCfg.cpyCpyuiOpInfoMap.view.zipWithIndex
          ) {
            if (opInfo == cpyOpInfo) {
              println(
                //s"pureCpyOp (${cpyOpInfoIdx}): "
                //+ s"${opInfoIdx}: ${someOp._2}"
                s"cpyCpyuiOp: ${cpyOpInfoIdx} "
                + s"${someOp._2} // ${opInfoIdx}"
              )
              //upPayload.op := opInfoIdx
              //mySplitOp.pureCpyOp.valid := True
              mySplitOp.kind := SnowHouseSplitOpKind.CPY_CPYUI
              mySplitOp.cpyCpyuiOp := cpyOpInfoIdx
              //return
              found = true
            }
          }
          for (
            ((_, jmpOpInfo), jmpOpInfoIdx)
            <- shCfg.jmpBrOpInfoMap.view.zipWithIndex
          ) {
            if (opInfo == jmpOpInfo) {
              println(
                s"jmpBrOp: ${jmpOpInfoIdx} "
                + s"${someOp._2} // ${opInfoIdx}"
              )
              //upPayload.op := opInfoIdx
              //mySplitOp.pureJmpOp.valid := True
              mySplitOp.kind := SnowHouseSplitOpKind.JMP_BR
              mySplitOp.jmpBrOp := jmpOpInfoIdx
              //return
              found = true
            }
          }
          for (
            ((_, aluOpInfo), aluOpInfoIdx)
            <- shCfg.aluOpInfoMap.view.zipWithIndex
          ) {
            if (opInfo == aluOpInfo) {
              println(
                s"aluOp: ${aluOpInfoIdx} "
                + s"${someOp._2} // ${opInfoIdx}"
              )
              mySplitOp.kind := SnowHouseSplitOpKind.ALU
              mySplitOp.aluOp := aluOpInfoIdx
              found = true
            }
          }
          for (
            ((_, multiCycleOpInfo), multiCycleOpInfoIdx)
            <- shCfg.multiCycleOpInfoMap.view.zipWithIndex
          ) {
            if (opInfo == multiCycleOpInfo) {
              println(
                s"multiCycleOp: ${multiCycleOpInfoIdx} "
                + s"${someOp._2} // ${opInfoIdx}"
              )
              mySplitOp.kind := SnowHouseSplitOpKind.MULTI_CYCLE
              mySplitOp.multiCycleOp := multiCycleOpInfoIdx
              //return
              found = true
            }
          }
        }
      }
      if (!found) {
        assert(
          false,
          s"eek! ${someOp}"
        )
      }
    }
    def doDefault(
      doBlockIrq: Boolean,
      doSetImm: Boolean=true
    ): Unit = {
      setOp(addRaSimm)
      //if (doBlockIrq) {
      //  upPayload.blockIrq := True
      //} else {
      //  rDecodeTemp := rDecodeTemp.getZero
      //}
      //upPayload.gprIdxVec.foreach{gprIdx => {
      //  gprIdx := 0x0
      //}}
      if (doSetImm) {
        upPayload.imm.foreach(imm => {
          imm := 0x0
        })
      }
    }
    upPayload.blockIrq := (
      //RegNext(
      //  next=upPayload.blockIrq,
      //  init=upPayload.blockIrq.getZero,
      //)
      False
    )
    when (cId.up.isFiring) {
      when (rDecodeTemp.lpreOnlyHaveHi) {
        //when (!psId.shouldBubble) {
          rPrevPrefixImm := Cat(
            rPrevPrefixImm,
            psId.tempInstr,
          ).asUInt.resized
          rDecodeTemp.lpreOnlyHaveHi := False
          upPayload.blockIrq := True
        //}
      } otherwise {
        switch (encInstr.g0Pre.grp) {
          import FlareCpuInstrEncConst._
          is (g0Grp) {
            when (cId.up.isFiring) {
              when (encInstr.g0Pre.subgrp === g0PreSubgrp) {
                when (!rDecodeTemp.preLpreValid) {
                  rDecodeTemp.preLpreValid := True
                  rPrevPrefixImm := Cat(
                    Mux[SInt](
                      encInstr.g0Pre.simm.msb,
                      S(s"${cfg.mainWidth}'d-1"),
                      S(s"${cfg.mainWidth}'d0"),
                    ),
                    encInstr.g0Pre.simm,
                  ).asUInt.resized

                  upPayload.blockIrq := True
                } otherwise {
                  doDefault(
                    doBlockIrq=false,
                  )
                }
              } elsewhen (encInstr.g0LpreHi.subgrp === g0LpreSubgrp) {
                when (!rDecodeTemp.preLpreValid) {
                  rDecodeTemp.preLpreValid := True
                  rDecodeTemp.lpreOnlyHaveHi := True
                  
                  rPrevPrefixImm := Cat(
                    Mux[SInt](
                      encInstr.g0LpreHi.simmHi.msb,
                      S(s"${cfg.mainWidth}'d-1"),
                      S(s"${cfg.mainWidth}'d0"),
                    ),
                    encInstr.g0LpreHi.simmHi,
                  ).asUInt.resized
                  upPayload.blockIrq := True
                } otherwise {
                  doDefault(
                    doBlockIrq=false,
                  )
                }
              } otherwise {
                doDefault(
                  doBlockIrq=false,
                )
              }
            }
            //elsewhen (encInstr.g0Atomic.subgrp === g0AtomicSubgrp) {
            //  // TODO: support atomic instructions
            //  doDefault(
            //    doBlockIrq=false,
            //  )
            //}
          }
          is (g1Grp) {
            upPayload.gprIdxVec(0) := encInstr.g1.raIdx
            upPayload.gprIdxVec(1) := encInstr.g1.raIdx
            upPayload.gprIdxVec(2) := encInstr.g1.raIdx
            //upPayload.gprIdxVec(2) := encInstr.g1.rbIdx
            val tempUImm = UInt(cfg.mainWidth bits)
            val tempSImm = UInt(cfg.mainWidth bits)
            when (!rDecodeTemp.preLpreValid) {
              tempUImm := (
                encInstr.g1.imm.resized
              )
              tempSImm := (
                Cat(
                  Mux[SInt](
                    encInstr.g1.imm.msb,
                    S(s"${cfg.mainWidth}'d-1"),
                    S(s"${cfg.mainWidth}'d0"),
                  ),
                  encInstr.g1.imm
                ).asUInt.resized
              )
            } otherwise {
              //upPayload.rPrevPrefixImm
              tempUImm := (
                Cat(
                  rPrevPrefixImm,
                  encInstr.g1.imm,
                ).asUInt.resized
              )
              tempSImm := (
                Cat(
                  rPrevPrefixImm,
                  encInstr.g1.imm,
                ).asUInt.resized
              )
            }
            upPayload.imm.foreach(imm => {
              imm := tempSImm
            })
            rDecodeTemp := rDecodeTemp.getZero
            switch (encInstr.g1.op) {
              is (FlareCpuInstrG1EncOp.addRaS5) {
                // Opcode 0x0: add rA, #simm5
                setOp(FlareCpuOp.addRaSimm)
              }
              is (FlareCpuInstrG1EncOp.addRaPcS5) {
                // Opcode 0x1: add rA, pc, #simm5
                setOp(FlareCpuOp.addRaPcSimm)
              }
              is (FlareCpuInstrG1EncOp.addRaSpS5) {
                // Opcode 0x2: add rA, sp, #simm5
                upPayload.gprIdxVec(1) := gprSpIdx
                setOp(FlareCpuOp.addRaSimm)
              }
              is (FlareCpuInstrG1EncOp.addRaFpS5) {
                // Opcode 0x3: add rA, fp, #simm5
                upPayload.gprIdxVec(1) := gprFpIdx
                setOp(FlareCpuOp.addRaSimm)
              }
              is (FlareCpuInstrG1EncOp.cmpRaS5) {
                // Opcode 0x4: cmp rA, #simm5
                setOp(FlareCpuOp.cmpRaSimm)
              }
              is (FlareCpuInstrG1EncOp.cpyRaS5) {
                // Opcode 0x5: cpy rA, #simm5
                setOp(FlareCpuOp.cpyRaSimm)
              }
              is (FlareCpuInstrG1EncOp.lslRaI5) {
                // Opcode 0x6: lsl rA, #imm5
                //upPayload.imm := tempUImm
                upPayload.imm.foreach(imm => {
                  imm := tempUImm
                })
                setOp(FlareCpuOp.lslRaImm)
              }
              is (FlareCpuInstrG1EncOp.lsrRaI5) {
                // Opcode 0x7: lsr rA, #imm5
                //upPayload.imm := tempUImm
                upPayload.imm.foreach(imm => {
                  imm := tempUImm
                })
                setOp(FlareCpuOp.lsrRaImm)
              }
              is (FlareCpuInstrG1EncOp.asrRaI5) {
                // Opcode 0x8: asr rA, #imm5
                //upPayload.imm := tempUImm
                upPayload.imm.foreach(imm => {
                  imm := tempUImm
                })
                setOp(FlareCpuOp.asrRaImm)
              }
              is (FlareCpuInstrG1EncOp.andRaS5) {
                // Opcode 0x9: and rA, #simm5
                setOp(FlareCpuOp.andRaSimm)
              }
              is (FlareCpuInstrG1EncOp.orrRaS5) {
                // Opcode 0xa: orr rA, #simm5
                setOp(FlareCpuOp.orrRaSimm)
              }
              is (FlareCpuInstrG1EncOp.xorRaS5) {
                // Opcode 0xb: xor rA, #simm5
                setOp(FlareCpuOp.xorRaSimm)
              }
              default {
                doDefault(
                  doBlockIrq=false
                )
              }
              // TODO: the rest of these instructions
              //is (FlareCpuInstrG1EncOp.zeRaI5) {
              //  // Opcode 0xc: ze rA, #imm5
              //  upPayload.imm := tempUImm
              //}
              //is (FlareCpuInstrG1EncOp.seRaI5) {
              //  // Opcode 0xd: se rA, #imm5
              //  upPayload.imm := tempUImm
              //}
              //is (FlareCpuInstrG1EncOp.swiRaS5) {
              //  // Opcode 0xe: swi rA, #simm5
              //  //upPayload.imm := tempSImm
              //}
              //is (FlareCpuInstrG1EncOp.swiI5) {
              //  // Opcode 0xf: swi #imm5
              //  upPayload.imm := tempUImm
              //}
            }
          }
          is (g2Grp) {
            upPayload.gprIdxVec(0) := encInstr.g2.raIdx
            upPayload.gprIdxVec(1) := encInstr.g2.raIdx
            upPayload.gprIdxVec(2) := encInstr.g2.rbIdx
            rDecodeTemp := rDecodeTemp.getZero
            switch (encInstr.g2.op) {
              is (FlareCpuInstrG2EncOp.addRaRb) {
                // Opcode 0x0: add rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.addRaRb)
                } otherwise {
                  setOp(FlareCpuOp.addRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.subRaRb) {
                // Opcode 0x1: sub rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.subRaRb)
                } otherwise {
                  setOp(FlareCpuOp.subRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.addRaSpRb) {
                // Opcode 0x2: add rA, sp, rB
                upPayload.gprIdxVec(1) := gprSpIdx
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.addRaRb)
                } otherwise {
                  setOp(FlareCpuOp.addRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.addRaFpRb) {
                // Opcode 0x3: add rA, fp, rB
                upPayload.gprIdxVec(1) := gprFpIdx
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.addRaRb)
                } otherwise {
                  setOp(FlareCpuOp.addRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.cmpRaRb) {
                // Opcode 0x4: cmp rA, rB
                setOp(FlareCpuOp.cmpRaRb)
              }
              is (FlareCpuInstrG2EncOp.cpyRaRb) {
                // Opcode 0x5: cpy rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.cpyRaRb)
                } otherwise {
                  setOp(FlareCpuOp.cpyRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.lslRaRb) {
                // Opcode 0x6: lsl rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.lslRaRb)
                } otherwise {
                  setOp(FlareCpuOp.lslRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.lsrRaRb) {
                // Opcode 0x7: lsr rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.lsrRaRb)
                } otherwise {
                  setOp(FlareCpuOp.lsrRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.asrRaRb) {
                // Opcode 0x8: asr rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.asrRaRb)
                } otherwise {
                  setOp(FlareCpuOp.asrRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.andRaRb) {
                // Opcode 0x9: and rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.andRaRb)
                } otherwise {
                  setOp(FlareCpuOp.andRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.orrRaRb) {
                // Opcode 0xa: orr rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.orrRaRb)
                } otherwise {
                  setOp(FlareCpuOp.orrRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.xorRaRb) {
                // Opcode 0xb: xor rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.xorRaRb)
                } otherwise {
                  setOp(FlareCpuOp.xorRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.adcRaRb) {
                // Opcode 0xc: adc rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.adcRaRb)
                } otherwise {
                  setOp(FlareCpuOp.adcRaRbFlags)
                }
              }
              is (FlareCpuInstrG2EncOp.sbcRaRb) {
                // Opcode 0xd: sbc rA, rB
                when (!encInstr.g2.f) {
                  setOp(FlareCpuOp.sbcRaRb)
                } otherwise {
                  setOp(FlareCpuOp.sbcRaRbFlags)
                }
              }
              default {
                doDefault(
                  doBlockIrq=false,
                  doSetImm=true,
                )
              }
              //is (FlareCpuInstrG2EncOp.cmpbcRaRb) {
              //  // Opcode 0xe: cmpbc rA, rB
              //  // TODO
              //  doDefault(
              //    doBloc
              //  )
              //}
              //is (FlareCpuInstrG2EncOp.invalid0) {
              //  // Opcode 0xf: invalid operation 0
              //}
            }
          }
          is (g3Grp) {
            rDecodeTemp := rDecodeTemp.getZero
            //upPayload.gprIdxVec(0) := 0x0
            //upPayload.gprIdxVec(1) := 0x0
            //upPayload.gprIdxVec(2) := 0x0
            val tempUImm = UInt(cfg.mainWidth bits)
            val tempSImm = UInt(cfg.mainWidth bits)
            when (!rDecodeTemp.preLpreValid) {
              tempUImm := (
                encInstr.g3.simm.resized
              )
              tempSImm := (
                Cat(
                  Mux[SInt](
                    encInstr.g3.simm.msb,
                    S(s"${cfg.mainWidth}'d-1"),
                    S(s"${cfg.mainWidth}'d0"),
                  ),
                  encInstr.g3.simm
                ).asUInt.resized
              )
            } otherwise {
              //upPayload.rPrevPrefixImm
              tempUImm := (
                Cat(
                  rPrevPrefixImm,
                  encInstr.g3.simm,
                ).asUInt.resized
              )
              tempSImm := (
                Cat(
                  rPrevPrefixImm,
                  encInstr.g3.simm,
                ).asUInt.resized
              )
            }
            //upPayload.imm := tempSImm
            upPayload.imm.foreach(imm => {
              imm := tempSImm
            })
            switch (encInstr.g3.op) {
              is (FlareCpuInstrG3EncOp.blS9) {
                // Opcode 0x0: bl simm9
                setOp(FlareCpuOp.blSimm)
              }
              is (FlareCpuInstrG3EncOp.braS9) {
                // Opcode 0x1: bra simm9
                setOp(FlareCpuOp.braSimm)
              }
              is (FlareCpuInstrG3EncOp.beqS9) {
                // Opcode 0x2: beq simm9
                setOp(FlareCpuOp.beqSimm)
              }
              is (FlareCpuInstrG3EncOp.bneS9) {
                // Opcode 0x3: bne simm9
                setOp(FlareCpuOp.bneSimm)
              }
              is (FlareCpuInstrG3EncOp.bmiS9) {
                // Opcode 0x4: bmi simm9
                setOp(FlareCpuOp.bmiSimm)
              }
              is (FlareCpuInstrG3EncOp.bplS9) {
                // Opcode 0x5: bpl simm9
                setOp(FlareCpuOp.bplSimm)
              }
              is (FlareCpuInstrG3EncOp.bvsS9) {
                // Opcode 0x6: bvs simm9
                setOp(FlareCpuOp.bvsSimm)
              }
              is (FlareCpuInstrG3EncOp.bvcS9) {
                // Opcode 0x7: bvc simm9
                setOp(FlareCpuOp.bvcSimm)
              }
              is (FlareCpuInstrG3EncOp.bgeuS9) {
                // Opcode 0x8: bgeu simm9
                setOp(FlareCpuOp.bgeuSimm)
              }
              is (FlareCpuInstrG3EncOp.bltuS9) {
                // Opcode 0x9: bltu simm9
                setOp(FlareCpuOp.bltuSimm)
              }
              is (FlareCpuInstrG3EncOp.bgtuS9) {
                // Opcode 0xa: bgtu simm9
                setOp(FlareCpuOp.bgtuSimm)
              }
              is (FlareCpuInstrG3EncOp.bleuS9) {
                // Opcode 0xb: bleu simm9
                setOp(FlareCpuOp.bleuSimm)
              }
              is (FlareCpuInstrG3EncOp.bgesS9) {
                // Opcode 0xc: bges simm9
                setOp(FlareCpuOp.bgesSimm)
              }
              is (FlareCpuInstrG3EncOp.bltsS9) {
                // Opcode 0xd: blts simm9
                setOp(FlareCpuOp.bltsSimm)
              }
              is (FlareCpuInstrG3EncOp.bgtsS9) {
                // Opcode 0xe: bgts simm9
                setOp(FlareCpuOp.bgtsSimm)
              }
              is (FlareCpuInstrG3EncOp.blesS9) {
                // Opcode 0xf: bles simm9
                setOp(FlareCpuOp.blesSimm)
              }
            }
          }
          is (g4Grp) {
            rDecodeTemp := rDecodeTemp.getZero
            upPayload.gprIdxVec(0) := encInstr.g4.raIdx
            upPayload.gprIdxVec(1) := encInstr.g4.raIdx
            upPayload.gprIdxVec(2) := encInstr.g4.rbIdx
            switch (encInstr.g4.op) {
              is (FlareCpuInstrG4EncOp.jlRa) {
                // Opcode 0x0: jl rA
                upPayload.gprIdxVec(0) := gprLrIdx
                setOp(FlareCpuOp.jlRa)
              }
              is (FlareCpuInstrG4EncOp.jmpRa) {
                // Opcode 0x1: jmp rA
                setOp(FlareCpuOp.jmpRa)
              }
              //is (FlareCpuInstrG4EncOp.jmpIra) {
              //  // Opcode 0x2: jmp ira
              //}
              //is (FlareCpuInstrG4EncOp.reti) {
              //  // Opcode 0x3: reti
              //}
              //is (FlareCpuInstrG4EncOp.ei) {
              //  // Opcode 0x4: ei
              //}
              //is (FlareCpuInstrG4EncOp.di) {
              //  // Opcode 0x5: di
              //}
              //is (FlareCpuInstrG4EncOp.pushRaRb) {
              //  // Opcode 0x6: push rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.pushSaRb) {
              //  // Opcode 0x7: push sA, rB
              //}
              //is (FlareCpuInstrG4EncOp.popRaRb) {
              //  // Opcode 0x8: pop rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.popSaRb) {
              //  // Opcode 0x9: pop sA, rB
              //}
              //is (FlareCpuInstrG4EncOp.popPcRb) {
              //  // Opcode 0xa: pop pc, rB
              //}
              is (FlareCpuInstrG4EncOp.mulRaRb) {
                // Opcode 0xb: mul rA, rB
                setOp(FlareCpuOp.mulRaRb)
              }
              //is (FlareCpuInstrG4EncOp.udivmodRaRb) {
              //  // Opcode 0xc: udivmod rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.sdivmodRaRb) {
              //  // Opcode 0xd: sdivmod rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.lumulRaRb) {
              //  // Opcode 0xe: lumul rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.lsmulRaRb) {
              //  // Opcode 0xf: lsmul rA, rB
              //}
              //--------
              //is (FlareCpuInstrG4EncOp.udivmod64RaRb) {
              //  // Opcode 0x10: udivmod64 rA, rB
              //}
              //is (FlareCpuInstrG4EncOp.sdivmod64RaRb) {
              //  // Opcode 0x11: sdivmod64 rA, rB
              //}
              is (FlareCpuInstrG4EncOp.ldubRaRb) {
                // Opcode 0x12: ldub rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.ldubRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.ldubRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.ldsbRaRb) {
                // Opcode 0x13: ldsb rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.ldsbRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.ldsbRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.lduhRaRb) {
                // Opcode 0x14: lduh rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.lduhRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.lduhRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.ldshRaRb) {
                // Opcode 0x15: ldsh rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.ldshRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.ldshRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.ldrRaRb) {
                // Opcode 0x16: ldr rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.ldrRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.ldrRaIndexRegLdst)
                }
              }
              //is (FlareCpuInstrG4EncOp.reserved17) {
              //  // Opcode 0x17: reserved
              //}
              is (FlareCpuInstrG4EncOp.stbRaRb) {
                // Opcode 0x18: stb rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.stbRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.stbRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.sthRaRb) {
                // Opcode 0x19: sth rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.sthRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.sthRaIndexRegLdst)
                }
              }
              is (FlareCpuInstrG4EncOp.strRaRb) {
                // Opcode 0x1a: str rA, [rB]
                when (!rDecodeTemp.indexEitherValid) {
                  setOp(FlareCpuOp.strRaRbLdst)
                } otherwise {
                  setOp(FlareCpuOp.strRaIndexRegLdst)
                }
              }
              // TODO: the rest of these instructions
              //is (FlareCpuInstrG4EncOp.reserved1b) {
              //  // Opcode 0x1b: reserved
              //}
              //is (FlareCpuInstrG4EncOp.cpyRaSb) {
              //  // Opcode 0x1c: cpy rA, sB
              //}
              //is (FlareCpuInstrG4EncOp.cpySaRb) {
              //  // Opcode 0x1d: cpy sA, rB
              //}
              //is (FlareCpuInstrG4EncOp.cpySaSb) {
              //  // Opcode 0x1e: cpy sA, sB
              //}
              default {
                doDefault(
                  doBlockIrq=false,
                  doSetImm=true,
                )
              }
            }
          }
          is (g5Grp) {
            //rDecodeTemp := rDecodeTemp.getZero
            when (encInstr.g5Sg0.subgrp === g5Sg0Subgrp) {
              when (
                !rDecodeTemp.indexEitherValid
              ) {
                upPayload.gprIdxVec(1) := encInstr.g5Sg0.raIdx
                upPayload.gprIdxVec(2) := encInstr.g5Sg0.rbIdx
                upPayload.blockIrq := True
                setOp(FlareCpuOp.indexRaRb)
              } otherwise {
                doDefault(
                  doBlockIrq=false,
                  doSetImm=true,
                )
              }
            } otherwise {
              when (
                !rDecodeTemp.preLpreValid
                && !rDecodeTemp.indexEitherValid
              ) {
                rDecodeTemp.indexEitherValid := True
                rDecodeTemp.indexRaSimmValid := True
                //val tempUImm = UInt(cfg.mainWidth bits)
                val tempSImm = UInt(cfg.mainWidth bits)
                when (!rDecodeTemp.preLpreValid) {
                  tempSImm := (
                    Cat(
                      Mux[SInt](
                        encInstr.g5Sg1.simm.msb,
                        S(s"${cfg.mainWidth}'d-1"),
                        S(s"${cfg.mainWidth}'d0"),
                      ),
                      encInstr.g5Sg1.simm,
                    ).asUInt.resized
                  )
                } otherwise {
                  tempSImm := (
                    Cat(
                      rPrevPrefixImm,
                      encInstr.g5Sg1.simm,
                    ).asUInt.resized
                  )
                }
                //upPayload.imm := tempSImm
                upPayload.imm.foreach(imm => {
                  imm := tempSImm
                })
                upPayload.gprIdxVec(1) := encInstr.g5Sg1.raIdx
                upPayload.blockIrq := True
                setOp(FlareCpuOp.indexRaSimm)
              } otherwise {
                doDefault(
                  doBlockIrq=false,
                  doSetImm=true,
                )
              }
            }
          }
          //is (g6Grp) {
          //  rDecodeTemp := rDecodeTemp.getZero
          //}
          //is (g7Grp) {
          //  rDecodeTemp := rDecodeTemp.getZero
          //}
          default {
            doDefault(
              doBlockIrq=false,
              doSetImm=true,
            )
          }
        }
      }
    }
  }
}
object FlareCpuProgram {
  def doConvert(
    filename: String
  ) = {
    val tempArr = new ArrayBuffer[Short]()
    val bis = new BufferedInputStream(new FileInputStream(filename))
    var idx: Int = 0x0
    var prevB: Int = 0
    Iterator.continually(bis.read())
      .takeWhile(_ != -1)
      .foreach(b => {
        if ((idx + 1) % 2 == 1) {
          tempArr += (
            (prevB << 8) | b.toInt
            //(b.toInt << 8) | prevB
          ).toShort
        } 
        idx = (idx + 1) % 2
        prevB = b.toInt
      })
    tempArr
  }
}

//case class FlareCpuIcacheWordType(
//  params: FlareCpuParams
//) extends Bundle {
//  val baseAddr = UInt(params.icacheLineBaseAddrWidth bits)
//  val data = params.icacheLineMemWordType()
//}
//case class FlareCpuIcachePipePayload(
//  params: FlareCpuParams
//) extends Bundle {
//  val hit = Bool()
//  val valid = Bool()
//  val word = FlareCpuIcacheWordType(params=params)
//}
//case class FlareCpuDcacheWordType(
//  params: FlareCpuParams
//) extends Bundle {
//  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
//  val data = params.dcacheLineMemWordType()
//}
//case class FlareCpuDcachePipePayload(
//  params: FlareCpuParams
//) extends Bundle {
//  val hit = Bool()
//
//  val word = FlareCpuDcacheWordType(params=params)
//
//  val valid = Bool()
//  val dirty = Bool()
//}

//--------
//case class FlareCpuPipeStageId(
//  params: FlareCpuParams,
//  regFilePmRmwCfg: PipeMemRmwConfig[
//    UInt,
//    Bool,
//  ],
//  cId: CtrlLink,
//  pIf: Payload[FlareCpuPipeMemModExtType],
//  //pId: Payload[FlareCpuPipeMemModExtType],
//  ////pId: Payload[FlareCpuPipeMemModExtType],
//  io: FlareCpuIo,
//  regFile: Option[PipeMemRmw[
//    UInt,
//    Bool,
//    FlareCpuPipeMemModType[
//      UInt,
//      Bool,
//      FlareCpuPipeMemModExtType,
//    ],
//    PipeMemRmwDualRdTypeDisabled[UInt, Bool]
//  ]],
//  //mkRegFileModType: () => FlareCpuPipeMemModType[
//  //  UInt,
//  //  Bool,
//  //  FlareCpuPipeMemModExtType,
//  //],
//  psIdHaltIt: Bool,
//  psExSetPc: Flow[UInt],
//  //up: NodeApi,
//  //down: NodeApi,
//  optFormalTest: FlareCpuFormalTest=(
//    FlareCpuFormalTest.Dont
//  ),
//) extends Area {
//  //--------
//  val up = cId.up
//  val down = cId.down
//  //--------
//  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
//  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
//  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
//  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
//  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
//  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
//  def enumRegFileLim = FlareCpuParams.enumRegFileLim
//  //--------
//  def mkRegFileModType(
//    //pmRmwCfg: PipeMemRmwConfig[
//    //  UInt,
//    //  Bool,
//    //],
//  ) = (
//    FlareCpuParams.mkRegFileModType(
//      params=params,
//      pmRmwCfg=regFilePmRmwCfg,
//      optFormalTest=optFormalTest,
//    )
//    //FlareCpuPipeMemModType(
//    //  params=params,
//    //  wordType=params.regWordType(),
//    //  wordCountMax=params.sprFileEvenWordCount,
//    //  hazardCmpType=params.regFileHazardCmpType(),
//    //  modRdPortCnt=params.regFileModRdPortCnt,
//    //  modStageCnt=params.regFileModStageCnt,
//    //  optModHazardKind=params.regFileOptModHazardKind,
//    //  modExtType=FlareCpuPipeMemModExtType(params=params),
//    //)
//  )
//  //--------
//  //def enumFormalTestNone = FlareCpuParams.enumFormalTestNone
//  //def enumFormalTestMain = FlareCpuParams.enumFormalTestMain
//  //--------
//  def myFormal = (
//    optFormalTest != FlareCpuFormalTest.Dont
//  )
//  def myFormalMain = (
//    optFormalTest == FlareCpuFormalTest.Main
//  )
//  val upModExt = FlareCpuPipeMemModExtType(
//    params=params,
//    optFormalTest=optFormalTest,
//  )
//  //up(pId) := upModExt
//  upModExt := (
//    RegNext(upModExt)
//    init(upModExt.getZero)
//  )
//  upModExt.allowOverride
//  when (up.isValid) {
//    upModExt.regPc := cId.up(pIf).regPc
//    if (myFormal) {
//      upModExt.instrCnt := cId.up(pIf).instrCnt
//    }
//  }
//  //cId.bypass(pIf) := upModExt
//  //--------
//  def upInstrEnc = upModExt.instrEnc
//  def upInstrDecEtc = upModExt.instrDecEtc
//  upInstrDecEtc.allowOverride
//  val rSavedUpInstrDecEtc = (
//    RegNextWhen(
//      upInstrDecEtc,
//      up.isFiring,
//    ) init(upInstrDecEtc.getZero)
//  )
//  //val canIrq = Bool()
//  //canIrq := True
//
//  //val myFrontPayloadGprEven = mkRegFileModType()
//  val myFrontPayload = mkRegFileModType()
//  myFrontPayload := (
//    RegNext(myFrontPayload)
//    init(myFrontPayload.getZero)
//  )
//  //val myFrontPayloadGprEvenNonFp = mkRegFileModType()
//  //val myFrontPayloadGprFp = mkRegFileModType()
//  //val myFrontPayloadGprOddNonSp = mkRegFileModType()
//  //val myFrontPayloadGprSp = mkRegFileModType()
//  //val myFrontPayloadSprEven = mkRegFileModType()
//  //val myFrontPayloadSprOdd = mkRegFileModType()
//  def myGprEvenNonFpExt = myFrontPayload.myExt(
//    enumRegFileGprEvenNonFp
//  )
//  def myGprFpExt = myFrontPayload.myExt(
//    enumRegFileGprFp
//  )
//  def myGprOddNonSpExt = myFrontPayload.myExt(
//    enumRegFileGprOddNonSp
//  )
//  def myGprSpExt = myFrontPayload.myExt(
//    enumRegFileGprSp
//  )
//  def mySprEvenExt = myFrontPayload.myExt(
//    enumRegFileSprEven
//  )
//  def mySprOddExt = myFrontPayload.myExt(
//    enumRegFileSprOdd
//  )
//
//  //myFrontPayloadGprEvenNonFp := (
//  //  RegNext(myFrontPayloadGprEvenNonFp)
//  //  init(myFrontPayloadGprEvenNonFp.getZero)
//  //)
//  //myFrontPayloadGprFp := (
//  //  RegNext(myFrontPayloadGprFp)
//  //  init(myFrontPayloadGprFp.getZero)
//  //)
//  //myFrontPayloadGprOddNonSp := (
//  //  RegNext(myFrontPayloadGprOddNonSp)
//  //  init(myFrontPayloadGprOddNonSp.getZero)
//  //)
//  //myFrontPayloadGprSp := (
//  //  RegNext(myFrontPayloadGprSp)
//  //  init(myFrontPayloadGprSp.getZero)
//  //)
//  //myFrontPayloadSprEven := (
//  //  RegNext(myFrontPayloadSprEven)
//  //  init(myFrontPayloadSprEven.getZero)
//  //)
//  //myFrontPayloadSprOdd := (
//  //  RegNext(myFrontPayloadSprOdd)
//  //  init(myFrontPayloadSprOdd.getZero)
//  //)
//
//  regFile match {
//    case Some(myRegFile) => {
//      up(myRegFile.io.frontPayload) := (
//        myFrontPayload
//      )
//      //up(myRegFile.io.frontPayloadArr(enumRegFileGprEvenNonFp)) := (
//      //  myFrontPayloadGprEvenNonFp
//      //)
//      //up(myRegFile.io.frontPayloadArr(enumRegFileGprFp)) := (
//      //  myFrontPayloadGprFp
//      //)
//      //up(myRegFile.io.frontPayloadArr(enumRegFileGprOddNonSp)) := (
//      //  myFrontPayloadGprOddNonSp
//      //)
//      //up(myRegFile.io.frontPayloadArr(enumRegFileGprSp)) := (
//      //  myFrontPayloadGprSp
//      //)
//      //up(myRegFile.io.frontPayloadArr(enumRegFileSprEven)) := (
//      //  myFrontPayloadSprEven
//      //)
//      //up(myRegFile.io.frontPayloadArr(enumRegFileSprOdd)) := (
//      //  myFrontPayloadSprOdd
//      //)
//    }
//    case None => {
//    }
//  }
//  object MultiCycleState
//  extends SpinalEnum(defaultEncoding=binarySequential) {
//    val
//      PRIMARY,
//      LPRE_SIMM_LO,
//      G7_SUB_DECODE
//      = newElement()
//  }
//
//  val rMultiCycleState = (
//    Reg(MultiCycleState())
//    init(MultiCycleState.PRIMARY)
//  )
//  val rDidHandleG7SubDecode = Reg(Bool()) init(False)
//  def myDoHaltIt(): Unit = {
//    //psIdHaltIt := True
//    cId.haltIt()
//      // this `haltIt()` call prevents `up.isFiring` and prevents
//      // deassertion of `rDidHandleG7SubDecode`
//  }
//  when (up.isValid) {
//    // Take one extra cycle to decode group 7 instructions to help with
//    // fmax
//    when (
//      rMultiCycleState === MultiCycleState.PRIMARY
//      && upInstrEnc.g0Pre.grp === FlareCpuInstrEncConst.g7Grp
//      && !rDidHandleG7SubDecode
//    ) {
//      myDoHaltIt()
//      rMultiCycleState := MultiCycleState.G7_SUB_DECODE
//      rDidHandleG7SubDecode := True
//    }
//  }
//
//  //up(pId) := upModExt
//  when (
//    up.isFiring
//    //up.isValid
//  ) {
//    rDidHandleG7SubDecode := False
//    //upInstrDecEtc := upInstrDecEtc.getZero
//    //upInstrDecEtc.isInvalid := False
//    //upInstrDecEtc.haveFullInstr := True
//
//    //def clearRegsMain(): Unit = {
//    //}
//    myFrontPayload.modExt := upModExt
//    //--------
//    //myFrontPayloadGprEvenNonFp.myExt.modMemWordValid := (
//    //  upInstrDecEtc.gprEvenNonFpRaIdx.valid
//    //)
//    //myFrontPayloadGprEvenNonFp.modExt := upModExt
//    myGprEvenNonFpExt.memAddr(0) := (
//      upInstrDecEtc.gprEvenNonFpRaIdx.payload(
//        myGprEvenNonFpExt.memAddr(0).bitsRange
//      )
//    )
//    myGprEvenNonFpExt.memAddr(1) := (
//      upInstrDecEtc.gprEvenNonFpRbIdx.payload(
//        myGprEvenNonFpExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //myFrontPayloadGprFp.modExt := upModExt
//    ////myFrontPayloadGprFp.myExt.modMemWordValid := (
//    ////  upInstrDecEtc.gprFpRaIdx.valid
//    ////)
//    myGprFpExt.memAddr(0) := (
//      upInstrDecEtc.gprFpRaIdx.payload(
//        myGprFpExt.memAddr(0).bitsRange
//      )
//    )
//    myGprFpExt.memAddr(1) := (
//      upInstrDecEtc.gprFpRbIdx.payload(
//        myGprFpExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //myFrontPayloadGprOddNonSp.myExt.modMemWordValid := (
//    //  upInstrDecEtc.gprOddNonSpRaIdx.valid
//    //)
//    //myFrontPayloadGprOddNonSp.modExt := upModExt
//    myGprOddNonSpExt.memAddr(0) := (
//      upInstrDecEtc.gprOddNonSpRaIdx.payload(
//        myGprOddNonSpExt.memAddr(0).bitsRange
//      )
//    )
//    myGprOddNonSpExt.memAddr(1) := (
//      upInstrDecEtc.gprOddNonSpRbIdx.payload(
//        myGprOddNonSpExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //myFrontPayloadGprSp.myExt.modMemWordValid := (
//    //  upInstrDecEtc.gprSpRaIdx.valid
//    //)
//    //myFrontPayloadGprSp.modExt := upModExt
//    myGprSpExt.memAddr(0) := (
//      upInstrDecEtc.gprSpRaIdx.payload(
//        myGprSpExt.memAddr(0).bitsRange
//      )
//    )
//    myGprSpExt.memAddr(1) := (
//      upInstrDecEtc.gprSpRbIdx.payload(
//        myGprSpExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //myFrontPayloadSprEven.myExt.modMemWordValid := (
//    //  upInstrDecEtc.sprEvenSaIdx.valid
//    //)
//    //myFrontPayloadSprEven.modExt := upModExt
//    mySprEvenExt.memAddr(0) := (
//      upInstrDecEtc.sprEvenSaIdx.payload(
//        mySprEvenExt.memAddr(0).bitsRange
//      )
//    )
//    mySprEvenExt.memAddr(1) := (
//      upInstrDecEtc.sprEvenSbIdx.payload(
//        mySprEvenExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //myFrontPayloadSprOdd.myExt.modMemWordValid := (
//    //  upInstrDecEtc.sprOddSaIdx.valid
//    //)
//    //myFrontPayloadSprOdd.modExt := upModExt
//    mySprOddExt.memAddr(0) := (
//      upInstrDecEtc.sprOddSaIdx.payload(
//        mySprOddExt.memAddr(0).bitsRange
//      )
//    )
//    mySprOddExt.memAddr(1) := (
//      upInstrDecEtc.sprOddSbIdx.payload(
//        mySprOddExt.memAddr(1).bitsRange
//      )
//    )
//    //--------
//    //case class DecodeGpr(
//    //  gprRaIdx: UInt,
//    //  wrGprRa: Boolean,
//    //  dual64: Boolean=false
//    //) {
//    //}
//
//    def finishInstr(
//      //isBlJl: Boolean=false,
//      ////writeSprFlags: Option[Bool]=None,
//      //writeGpr: Bool=True,
//      //writeGpr: Option[(UInt, Boolean, Boolean)]=(
//      //  Some((U"1'd0", false, false))
//      //),
//      writeGpr: Option[(UInt, Boolean)]=Some((U"1'd0", false)),
//      gprDual64: Boolean=false,
//      readGprRaAsRb: Boolean=false,
//      //decodeGpr: Option[DecodeGpr]=(
//      //  Some(
//      //    DecodeGpr(
//      //      U"1'd0",
//      //      false,
//      //      //false,
//      //    )
//      //  )
//      //),
//      rdWrSpr0: Option[(UInt, Bool, Boolean)]=None,
//      rdWrSpr1: Option[(UInt, Bool, Boolean)]=None,
//    ): Unit = {
//      //upInstrDecEtc.decodeTemp.indexRaRbValid := False
//      //upInstrDecEtc.decodeTemp.indexRaSimmValid := False
//      //upInstrDecEtc.decodeTemp.preLpreValid := False
//      //upInstrDecEtc.decodeTemp.preValid := False
//      //upInstrDecEtc.decodeTemp.lpreValid := False
//      upInstrDecEtc.decodeTemp := upInstrDecEtc.decodeTemp.getZero
//
//      upInstrDecEtc.isInvalid := False
//      upInstrDecEtc.haveFullInstr := True
//      upInstrDecEtc.raIdx := upInstrEnc.g2.raIdx
//      upInstrDecEtc.rbIdx := upInstrEnc.g2.rbIdx
//
//      //upInstrDecEtc.haveFullInstr := True
//
//      //--------
//      // BEGIN: Old design for `finishInstr()`'s writing rA
//      //if (!isBlJl) {
//      //  upInstrDecEtc.raIdx := upInstrEnc.g2.raIdx
//      //} else { // if (isBlSimm)
//      //  upInstrDecEtc.raIdx := FlareCpuInstrEncConst.gprLrIdx
//      //}
//      // END: Old design for `finishInstr()`'s writing rA
//      //def setGprA(
//      //): Unit = {
//      //}
//      //--------
//      // BEGIN: New design for `finishInstr()`'s writing rA
//      def setGpr(
//        tempGprIdx: UInt,
//        isGprRa: Boolean,
//      ): Unit = {
//        //val tempRaIdx = (
//        //  if (!myWriteGpr._2) (
//        //    upInstrDecEtc.raIdx
//        //  ) else (
//        //    myWriteGpr._1
//        //  )
//        //) //--------
//        val myGprEvenNonFpRegIdx = (
//          if (isGprRa) (
//            upInstrDecEtc.gprEvenNonFpRaIdx
//          ) else (
//            upInstrDecEtc.gprEvenNonFpRbIdx
//          )
//        )
//        val myGprFpRegIdx = (
//          if (isGprRa) (
//            upInstrDecEtc.gprFpRaIdx
//          ) else (
//            upInstrDecEtc.gprFpRbIdx
//          )
//        )
//        val myGprOddNonSpRegIdx = (
//          if (isGprRa) (
//            upInstrDecEtc.gprOddNonSpRaIdx
//          ) else (
//            upInstrDecEtc.gprOddNonSpRbIdx
//          )
//        )
//        val myGprSpRegIdx = (
//          if (isGprRa) (
//            upInstrDecEtc.gprSpRaIdx
//          ) else (
//            upInstrDecEtc.gprSpRbIdx
//          )
//        )
//        if (!gprDual64) {
//          myGprEvenNonFpRegIdx.valid := (
//            !tempGprIdx(0)
//            && (
//              tempGprIdx =/= FlareCpuInstrEncConst.gprFpIdx
//            )
//          )
//          //--------
//          myGprFpRegIdx.valid := (
//            tempGprIdx === FlareCpuInstrEncConst.gprFpIdx
//          )
//          //--------
//          myGprOddNonSpRegIdx.valid := (
//            tempGprIdx(0)
//            && (
//              tempGprIdx =/= FlareCpuInstrEncConst.gprSpIdx
//            )
//          )
//          //--------
//          myGprSpRegIdx.valid := (
//            tempGprIdx === FlareCpuInstrEncConst.gprSpIdx
//          )
//          //--------
//        } else { // if (gprDual64)
//          myGprEvenNonFpRegIdx.valid := (
//            Cat(tempGprIdx(tempGprIdx.high downto 1), False).asUInt
//            =/= FlareCpuInstrEncConst.gprFpIdx
//          )
//          myGprFpRegIdx.valid := (
//            Cat(tempGprIdx(tempGprIdx.high downto 1), False).asUInt
//            === FlareCpuInstrEncConst.gprFpIdx
//          )
//          myGprOddNonSpRegIdx.valid := (
//            Cat(tempGprIdx(tempGprIdx.high downto 1), True).asUInt
//            =/= FlareCpuInstrEncConst.gprSpIdx
//          )
//          myGprSpRegIdx.valid := (
//            Cat(tempGprIdx(tempGprIdx.high downto 1), True).asUInt
//            === FlareCpuInstrEncConst.gprSpIdx
//          )
//        }
//        //--------
//        //upInstrDecEtc.wrGprSpRaIdx := (
//        //  tempGprIdx === FlareCpuInstrEncConst.gprSpIdx
//        //)
//        //--------
//        val myGprSel = (
//          if (isGprRa) (
//            upInstrDecEtc.gprRaSel
//          ) else (
//            upInstrDecEtc.gprRbSel
//          )
//        )
//        //if (!gprDual64) {
//          when (
//            //upInstrDecEtc.gprEvenNonFpRaIdx.valid
//            if (isGprRa) (
//              upInstrDecEtc.gprEvenNonFpRaIdx.fire
//            ) else (
//              upInstrDecEtc.gprEvenNonFpRbIdx.fire
//            )
//          ) {
//            myGprSel := FlareCpuGprSelect.gprEvenNonFp
//          } elsewhen (
//            if (isGprRa) (
//              upInstrDecEtc.gprFpRaIdx.fire
//            ) else (
//              upInstrDecEtc.gprFpRbIdx.fire
//            )
//          ) {
//            myGprSel := FlareCpuGprSelect.gprFp
//          } elsewhen (
//            if (isGprRa) (
//              upInstrDecEtc.gprOddNonSpRaIdx.fire
//            ) else (
//              upInstrDecEtc.gprOddNonSpRbIdx.fire
//            )
//          ) {
//            myGprSel := FlareCpuGprSelect.gprOddNonSp
//          } otherwise {
//            myGprSel := FlareCpuGprSelect.gprSp
//          }
//        //} else { // if (gprDual64)
//        //  //myGprSel := FlareCpuGprSelect.gpr64
//        //}
//        val myGpr64IsNonFpSp = (
//          if (isGprRa) (
//            upInstrDecEtc.gprRa64IsNonFpSp
//          ) else (
//            upInstrDecEtc.gprRb64IsNonFpSp
//          )
//        )
//        myGpr64IsNonFpSp := (
//          if (isGprRa) (
//            upInstrDecEtc.gprEvenNonFpRaIdx.fire
//          ) else (
//            upInstrDecEtc.gprEvenNonFpRbIdx.fire
//          )
//        )
//        //upInstrDecEtc.gprRa64IsNonFpSp := (
//        //  upInstrDecEtc.gprEvenNonFprR
//        //)
//      }
//
//      writeGpr match {
//        case Some(myWriteGpr) => {
//          val tempRaIdx = (
//            if (!myWriteGpr._2) (
//              upInstrEnc.g2.raIdx
//            ) else (
//              myWriteGpr._1
//            )
//          )
//          setGpr(
//            tempGprIdx=tempRaIdx,
//            isGprRa=true,
//          )
//          upInstrDecEtc.wrGprEvenNonFpRaIdx := (
//            upInstrDecEtc.gprEvenNonFpRaIdx.fire
//          )
//          upInstrDecEtc.wrGprFpRaIdx := (
//            upInstrDecEtc.gprFpRaIdx.fire
//          )
//          upInstrDecEtc.wrGprOddNonSpRaIdx := (
//            upInstrDecEtc.gprOddNonSpRaIdx.fire
//          )
//          upInstrDecEtc.wrGprSpRaIdx := (
//            upInstrDecEtc.gprSpRaIdx.fire
//          )
//        }
//        case None => {
//          ////upInstrDecEtc.wrGprEvenNonFpRaIdx := False
//          //upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
//          //upInstrDecEtc.gprFpRaIdx.valid := False
//          //upInstrDecEtc.gprOddNonSpRaIdx.valid := False
//          //upInstrDecEtc.gprSpRaIdx.valid := False
//          val tempRaIdx = (
//            //if (!myWriteGpr._2) (
//              upInstrDecEtc.raIdx
//            //) else (
//            //  myWriteGpr._1
//            //)
//          )
//          setGpr(
//            tempGprIdx=tempRaIdx,
//            isGprRa=true,
//          )
//          upInstrDecEtc.wrGprEvenNonFpRaIdx := (
//            False
//          )
//          upInstrDecEtc.wrGprFpRaIdx := (
//            False
//          )
//          upInstrDecEtc.wrGprOddNonSpRaIdx := (
//            False
//          )
//          upInstrDecEtc.wrGprSpRaIdx := (
//            False
//          )
//        }
//      }
//      setGpr(
//        tempGprIdx=(
//          if (readGprRaAsRb) (
//            upInstrDecEtc.raIdx
//          ) else (
//            upInstrDecEtc.rbIdx
//          )
//        ),
//        isGprRa=false,
//      )
//      //if (true) {
//      //  //--------
//      //  val tempRbIdx = (
//      //    //if (!myWriteGpr._2) (
//      //      upInstrDecEtc.rbIdx
//      //    //) else (
//      //    //  myWriteGpr._1
//      //    //)
//      //  )
//      //  upInstrDecEtc.gprEvenNonFpRbIdx.valid := (
//      //    !tempRbIdx(0)
//      //    && (
//      //      tempRbIdx =/= FlareCpuInstrEncConst.gprFpIdx
//      //    )
//      //  )
//      //  upInstrDecEtc.gprFpRbIdx.valid := (
//      //    tempRbIdx === FlareCpuInstrEncConst.gprFpIdx
//      //  )
//      //  upInstrDecEtc.gprOddNonSpRbIdx.valid := (
//      //    tempRbIdx(0)
//      //    && (
//      //      tempRbIdx =/= FlareCpuInstrEncConst.gprSpIdx
//      //    )
//      //  )
//      //  upInstrDecEtc.gprSpRbIdx.valid := (
//      //    tempRbIdx === FlareCpuInstrEncConst.gprSpIdx
//      //  )
//      //  when (upInstrDecEtc.gprEvenNonFpRbIdx.valid) {
//      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprEvenNonFp
//      //  } elsewhen (upInstrDecEtc.gprFpRbIdx.valid) {
//      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprFp
//      //  } elsewhen (upInstrDecEtc.gprOddNonSpRbIdx.valid) {
//      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprOddNonSp
//      //  } otherwise {
//      //    upInstrDecEtc.gprRbSel := FlareCpuGprSelect.gprSp
//      //  }
//      //}
//      // END: New design for `finishInstr()`'s writing rA
//      //--------
//
//      upInstrDecEtc.rbIdx := upInstrEnc.g2.rbIdx
//      //upInstrDecEtc.saIdx := upInstrEnc.g2.raIdx
//      //upInstrDecEtc.sbIdx := upInstrEnc.g2.rbIdx
//
//
//      //when (upInstrDecEtc.gprEvenRaIdx.valid) {
//      //} otherwise {
//      //}
//
//      //--------
//      upInstrDecEtc.gprEvenNonFpRaIdx.payload := (
//        Cat(
//          U"1'd0",
//          upInstrDecEtc.raIdx(upInstrDecEtc.raIdx.high downto 1),
//        ).asUInt
//      )
//      upInstrDecEtc.gprEvenNonFpRbIdx.payload := (
//        Cat(
//          U"1'd0",
//          upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
//        ).asUInt
//      )
//      //--------
//      upInstrDecEtc.gprFpRaIdx.payload := (
//        0x0
//      )
//      upInstrDecEtc.gprFpRbIdx.payload := 0x0
//      //--------
//      upInstrDecEtc.gprOddNonSpRaIdx.payload := (
//        Cat(
//          U"1'd0",
//          upInstrDecEtc.raIdx(upInstrDecEtc.raIdx.high downto 1),
//        ).asUInt
//      )
//      upInstrDecEtc.gprOddNonSpRbIdx.payload := (
//        Cat(
//          U"1'd0",
//          upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1),
//        ).asUInt
//      )
//      //--------
//      upInstrDecEtc.gprSpRaIdx.payload := (
//        0x0
//      )
//      upInstrDecEtc.gprSpRbIdx.payload := 0x0
//      //--------
//      //if (!writeSprFlags) {
//      //  upInstrDecEtc.sprSaIdx.payload := upInstrDecEtc.saIdx
//      //} else 
//      //if (writeSprFlags)
//      //writeSprFlags match {
//      //  case Some(myWriteSprFlags) => {
//      //    when (myWriteSprFlags) {
//      //      upInstrDecEtc.sprEvenSaIdx.valid := True
//      //      upInstrDecEtc.sprEvenSaIdx.payload := (
//      //        FlareCpuInstrEncConst.sprFlagsIdx
//      //      )
//      //    }
//      //  }
//      //  case None => {
//      //    //upInstrDecE
//      //  }
//      //}
//      rdWrSpr0 match {
//        case Some(myWriteSpr0) => {
//          when (myWriteSpr0._2) {
//            //if (myWriteSpr0._1 % 2 == 0) {
//            //} else {
//            //}
//            when (!myWriteSpr0._1.lsb) {
//              // even
//              upInstrDecEtc.sprSaSel := FlareCpuSprSelect.sprEven
//              if (myWriteSpr0._3) {
//                upInstrDecEtc.wrSprEvenSaIdx := True
//              }
//              upInstrDecEtc.sprEvenSaIdx.valid := True
//              upInstrDecEtc.sprEvenSaIdx.payload := (
//                Cat(
//                  False,
//                  myWriteSpr0._1(myWriteSpr0._1.high downto 1)
//                ).asUInt
//              )
//              rdWrSpr1 match {
//                case Some(myWriteSpr1) => {
//                }
//                case None => {
//                  //upInstrDecEtc.sprEvenSaIdx.valid := False
//                  disableSprOddWrite()
//                }
//              }
//            } otherwise { // when (myWriteSpr0._1.lsb)
//              upInstrDecEtc.sprSaSel := FlareCpuSprSelect.sprOdd
//              // odd
//              if (myWriteSpr0._3) {
//                upInstrDecEtc.wrSprOddSaIdx := True
//              }
//              upInstrDecEtc.sprOddSaIdx.valid := True
//              upInstrDecEtc.sprOddSaIdx.payload := (
//                Cat(
//                  False,
//                  myWriteSpr0._1(myWriteSpr0._1.high downto 1)
//                ).asUInt
//              )
//              rdWrSpr1 match {
//                case Some(myWriteSpr1) => {
//                }
//                case None => {
//                  //upInstrDecEtc.sprEvenSaIdx.valid := False
//                  disableSprEvenWrite()
//                }
//              }
//            }
//          }
//        }
//        case None => {
//          rdWrSpr1 match {
//            case Some(myWriteSpr1) => {
//            }
//            case None => {
//              disableSprWrites()
//            }
//          }
//        }
//      }
//      rdWrSpr1 match {
//        case Some(myWriteSpr1) => {
//          when (myWriteSpr1._2) {
//            //if (myWriteSpr1._1 % 2 == 0) {
//            //} else {
//            //}
//            when (!myWriteSpr1._1.lsb) {
//              if (myWriteSpr1._3) {
//                upInstrDecEtc.wrSprEvenSaIdx := True
//              }
//              upInstrDecEtc.sprEvenSaIdx.valid := True
//              upInstrDecEtc.sprEvenSaIdx.payload := (
//                Cat(
//                  False,
//                  myWriteSpr1._1(myWriteSpr1._1.high downto 1)
//                ).asUInt
//              )
//            } otherwise { // when (myWriteSpr1._1.lsb)
//              if (myWriteSpr1._3) {
//                upInstrDecEtc.wrSprOddSaIdx := True
//              }
//              upInstrDecEtc.sprOddSaIdx.valid := True
//              upInstrDecEtc.sprOddSaIdx.payload := (
//                Cat(
//                  False,
//                  myWriteSpr1._1(myWriteSpr1._1.high downto 1),
//                ).asUInt
//              )
//            }
//          }
//        }
//        case None => {
//        }
//      }
//      when (!upInstrDecEtc.rbIdx.lsb) {
//        upInstrDecEtc.sprSbSel := FlareCpuSprSelect.sprEven
//      } otherwise { // when (upInstrDecEtc.rbIdx.lsb)
//        upInstrDecEtc.sprSbSel := FlareCpuSprSelect.sprOdd
//      }
//      //when (!upInstrDecEtc.rbIdx.lsb) {
//        upInstrDecEtc.sprEvenSbIdx.valid := True
//        upInstrDecEtc.sprEvenSbIdx.payload := (
//          Cat(
//            False,
//            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1)
//          ).asUInt
//        )
//      //} otherwise {
//        // it doesn't matter (from a logic perspective) if we read from
//        // both
//        upInstrDecEtc.sprOddSbIdx.valid := True
//        upInstrDecEtc.sprOddSbIdx.payload := (
//          Cat(
//            False,
//            upInstrDecEtc.rbIdx(upInstrDecEtc.rbIdx.high downto 1)
//          ).asUInt
//        )
//      //}
//    }
//    //def writeSpr(
//    //  sprIdx: UInt,
//    //  cond: Bool,
//    //): Option[(UInt, Bool, Boolean)] = (
//    //  Some(
//    //    (sprIdx, cond, true)
//    //  )
//    //)
//    def writeSprFlags(cond: Bool): Option[(UInt, Bool, Boolean)] = (
//      Some(
//        (FlareCpuInstrEncConst.sprFlagsIdx, cond, true)
//      )
//    )
//    def markInstrInvalid(): Unit = {
//      upInstrDecEtc.haveFullInstr := True 
//      upInstrDecEtc.isInvalid := True
//      upInstrDecEtc.decOp := FlareCpuInstrDecOp.bubble
//
//      //upInstrDecEtc.decodeTemp.indexRaRbValid := False
//      //upInstrDecEtc.decodeTemp.indexRaSimmValid := False
//      //upInstrDecEtc.decodeTemp.preLpreValid := False
//      //upInstrDecEtc.decodeTemp.preValid := False
//      //upInstrDecEtc.decodeTemp.lpreValid := False
//      //upInstrDecEtc.decodeTemp.assignFromBits(B"5'd0")
//
//      upInstrDecEtc.decodeTemp := upInstrDecEtc.decodeTemp.getZero
//      disableRegWrites()
//    }
//    def disableGprWrites(): Unit = {
//      //--------
//      upInstrDecEtc.gprEvenNonFpRaIdx.valid := False
//      upInstrDecEtc.gprFpRaIdx.valid := False
//      upInstrDecEtc.gprOddNonSpRaIdx.valid := False
//      upInstrDecEtc.gprSpRaIdx.valid := False
//      //--------
//      upInstrDecEtc.wrGprEvenNonFpRaIdx := False
//      upInstrDecEtc.wrGprFpRaIdx := False
//      upInstrDecEtc.wrGprOddNonSpRaIdx := False
//      upInstrDecEtc.wrGprSpRaIdx := False
//      //--------
//    }
//    def disableSprEvenWrite(): Unit = {
//      //--------
//      upInstrDecEtc.sprEvenSaIdx.valid := False
//      //--------
//      upInstrDecEtc.wrSprEvenSaIdx := False
//      //--------
//    }
//    def disableSprOddWrite(): Unit = {
//      //--------
//      upInstrDecEtc.sprOddSaIdx.valid := False
//      //--------
//      upInstrDecEtc.wrSprOddSaIdx := False
//      //--------
//    }
//    def disableSprWrites(): Unit = {
//      disableSprEvenWrite()
//      disableSprOddWrite()
//    }
//    def disableRegWrites(): Unit = {
//      disableGprWrites()
//      disableSprWrites()
//    }
//    //def markInstrNotFull(): Unit = {
//    //}
//    //setRegsMain()
//    //if (optFormalTest == FlareCpuParams.enumFormalTestMain) {
//    //  //when (pastValidAfterReset) {
//    //    when (past(up.isFiring)) {
//    //      cover(
//    //        past(
//    //          rMultiCycleState
//    //          === MultiCycleState.PRIMARY
//    //        ) && past(
//    //          upInstrEnc.g0Pre.grp
//    //          === FlareCpuInstrEncConst.g0Grp
//    //        ) && past(
//    //          upInstrEnc.g0LpreHi.subgrp
//    //          === FlareCpuInstrEncConst.g0LpreSubgrp
//    //        ) && (
//    //          rMultiCycleState
//    //          === MultiCycleState.LPRE_SIMM_LO
//    //        )
//    //      )
//    //    }
//    //  //}
//    //}
//
//    switch (rMultiCycleState) {
//      is (MultiCycleState.PRIMARY) {
//    //when (rSavedUpInstrDecEtc.decOp =/= FlareCpuInstrDecOp.lpreSimmHi) {
//        disableRegWrites() // just do this to start with
//        switch (upInstrEnc.g0Pre.grp) {
//          is (FlareCpuInstrEncConst.g0Grp) {
//            when (
//              upInstrEnc.g0Pre.subgrp
//              === FlareCpuInstrEncConst.g0PreSubgrp
//            ) {
//              when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
//                upInstrDecEtc.decodeTemp.preLpreValid := True
//                upInstrDecEtc.decodeTemp.preValid := True
//                upInstrDecEtc.isInvalid := False
//                upInstrDecEtc.haveFullInstr := False
//                //upInstrDecEtc.fullgrp := (
//                //  FlareCpuInstrFullgrpDec.g0Pre
//                //)
//                upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0Pre
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.preSimm
//                //upInstrDecEtc.fullSimm := (
//                //  upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
//                //)
//                val tempSimm = SInt(params.mainWidth bits)
//                tempSimm := (
//                  upInstrEnc.g0Pre.simm.asSInt.resized
//                )
//                upInstrDecEtc.fullSimm := (
//                  tempSimm.asUInt
//                )
//                // I believe the Binutils port handles unsigned
//                // immediates as signed when there's `pre`
//                // TODO: check whether that's the case
//                //upInstrDecEtc.fullImm := (
//                //  upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
//                //)
//                upInstrDecEtc.fullImm := (
//                  //upInstrEnc.g0Pre.simm.asSInt.resized.asUInt
//                  tempSimm.asUInt
//                )
//                disableRegWrites()
//              } otherwise {
//                markInstrInvalid()
//              }
//            } elsewhen (
//              upInstrEnc.g0LpreHi.subgrp
//              === FlareCpuInstrEncConst.g0LpreSubgrp
//            ) {
//              rMultiCycleState := MultiCycleState.LPRE_SIMM_LO
//              upInstrDecEtc.isInvalid := False
//              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0LpreHi
//              upInstrDecEtc.haveFullInstr := False
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lpreSimmHi
//              disableRegWrites()
//              val tempSimm = SInt(params.mainWidth bits)
//              tempSimm := (
//                upInstrEnc.g0LpreHi.simmHi.asSInt.resized
//              )
//              upInstrDecEtc.fullSimm := (
//                tempSimm.asUInt
//              )
//              upInstrDecEtc.fullImm := (
//                //upInstrEnc.g0LpreHi.simmHi.asSInt.resized.asUInt
//                tempSimm.asUInt
//              )
//            } elsewhen (
//              upInstrEnc.g0Atomic.subgrp
//              === FlareCpuInstrEncConst.g0AtomicSubgrp
//            ) {
//              upInstrDecEtc.fwl := upInstrEnc.g0Atomic.l
//              finishInstr()
//              disableRegWrites()
//              switch (Cat(
//                upInstrDecEtc.fwl,
//                rSavedUpInstrDecEtc.decodeTemp.indexRaRbValid,
//              )) {
//                is (B"00") {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchg
//                }
//                is (B"01") {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchgLock
//                }
//                is (B"10") {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchg
//                }
//                is (B"11") {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchgLock
//                }
//              }
//              //when (
//              //  rSavedUpInstrDecEtc.indexRaRbValid
//              //  //|| rSavedUpInstrDecEtc.indexRaSimmValid
//              //) {
//              //  when (upInstrDecEtc.fwl) {
//              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchgLock
//              //  } otherwise {
//              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpxchg
//              //  }
//              //} otherwise {
//              //  when (upInstrDecEtc.fwl) {
//              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchgLock
//              //  } otherwise {
//              //    upInstrDecEtc.decOp := FlareCpuInstrDecOp.xchg
//              //  }
//              //}
//              //when (upInstrDecEtc.) {
//              //}
//            } otherwise {
//              markInstrInvalid()
//              //canIrq := False
//            }
//          }
//          is (FlareCpuInstrEncConst.g1Grp) {
//            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g1
//            when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
//              //upInstrDecEtc.fullSimm := (
//              //  upInstrEnc.g1.imm.asSInt.resized.asUInt
//              //)
//              val tempSimm = SInt(params.mainWidth bits)
//              tempSimm := (
//                upInstrEnc.g1.imm.asSInt.resized
//              )
//              upInstrDecEtc.fullSimm := (
//                tempSimm.asUInt
//              )
//              upInstrDecEtc.fullImm := (
//                upInstrEnc.g1.imm.resized
//              )
//            } otherwise {
//              upInstrDecEtc.fullSimm := Cat(
//                rSavedUpInstrDecEtc.fullSimm,
//                upInstrEnc.g1.imm,
//              ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
//              upInstrDecEtc.fullImm := Cat(
//                rSavedUpInstrDecEtc.fullImm,
//                upInstrEnc.g1.imm,
//              ).asUInt(upInstrDecEtc.fullImm.bitsRange)
//            }
//            upInstrDecEtc.fwl := False
//            switch (upInstrEnc.g1.op) {
//              is (FlareCpuInstrG1EncOp.addRaS5) {
//                // Opcode 0x0: add rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.addRaPcS5) {
//                // Opcode 0x1: add rA, pc, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaPcSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.addRaSpS5) {
//                // Opcode 0x2: add rA, sp, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.addRaFpS5) {
//                // Opcode 0x3: add rA, fp, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.cmpRaS5) {
//                // Opcode 0x4: cmp rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpRaSimm
//                //disableGprWrites()
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=writeSprFlags(True)
//                )
//              }
//              is (FlareCpuInstrG1EncOp.cpyRaS5) {
//                // Opcode 0x5: cpy rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.lslRaI5) {
//                // Opcode 0x6: lsl rA, #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaImm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.lsrRaI5) {
//                // Opcode 0x7: lsr rA, #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaImm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.asrRaI5) {
//                // Opcode 0x8: asr rA, #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaImm
//              }
//              is (FlareCpuInstrG1EncOp.andRaS5) {
//                // Opcode 0x9: and rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.orrRaS5) {
//                // Opcode 0xa: orr rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.xorRaS5) {
//                // Opcode 0xb: xor rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaSimm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.zeRaI5) {
//                // Opcode 0xc: ze rA, #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.zeRaImm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.seRaI5) {
//                // Opcode 0xd: se rA, #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.seRaImm
//                finishInstr()
//              }
//              is (FlareCpuInstrG1EncOp.swiRaS5) {
//                // Opcode 0xe: swi rA, #simm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.swiRaSimm
//                finishInstr(writeGpr=None)
//                //disableRegWrites()
//              }
//              is (FlareCpuInstrG1EncOp.swiI5) {
//                // Opcode 0xf: swi #imm5
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.swiImm
//                finishInstr(writeGpr=None)
//                //disableRegWrites()
//              }
//            }
//          }
//          is (FlareCpuInstrEncConst.g2Grp) {
//            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g2
//            upInstrDecEtc.fwl := upInstrEnc.g2.f
//            switch (upInstrEnc.g2.op) {
//              is (FlareCpuInstrG2EncOp.addRaRb) {
//                // Opcode 0x0: add rA, rB
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaRbFlags
//                //}
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//              }
//              is (FlareCpuInstrG2EncOp.subRaRb) {
//                // Opcode 0x1: sub rA, rB
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.subRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.subRaRbFlags
//                //}
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//              }
//              is (FlareCpuInstrG2EncOp.addRaSpRb) {
//                //--------
//                // Opcode 0x2: add rA, sp, rB
//                // we can always read `sp` since it's in its own
//                // `regFile` chunk
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaSpRbFlags
//                //}
//                //--------
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.addRaFpRb) {
//                //--------
//                // Opcode 0x3: add rA, fp, rB
//                // we can always read `fp` since it's in its own
//                // `regFile` chunk
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.addRaFpRbFlags
//                //}
//                //--------
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.cmpRaRb) {
//                //--------
//                // Opcode 0x4: cmp rA, rB
//                //--------
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpRaRb
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=writeSprFlags(True)
//                )
//                //disableGprWrites()
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.cpyRaRb) {
//                //--------
//                // Opcode 0x5: cpy rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaRbFlags
//                //}
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//              }
//              is (FlareCpuInstrG2EncOp.lslRaRb) {
//                //--------
//                // Opcode 0x6: lsl rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lslRaRbFlags
//                //}
//                finishInstr(
//                  rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl)
//                )
//              }
//              is (FlareCpuInstrG2EncOp.lsrRaRb) {
//                //--------
//                // Opcode 0x7: lsr rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.asrRaRb) {
//                //--------
//                // Opcode 0x8: asr rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.andRaRb) {
//                //--------
//                // Opcode 0x9: and rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.andRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.orrRaRb) {
//                //--------
//                // Opcode 0xa: orr rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.orrRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.xorRaRb) {
//                //--------
//                // Opcode 0xb: xor rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.xorRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//                //--------
//              }
//              is (FlareCpuInstrG2EncOp.adcRaRb) {
//                //--------
//                // Opcode 0xc: adc rA, rB
//                //--------
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.adcRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.adcRaRbFlags
//                //}
//                //--------
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//              }
//              is (FlareCpuInstrG2EncOp.sbcRaRb) {
//                // Opcode 0xd: sbc rA, rB
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.sbcRaRbFlags
//                //}
//                finishInstr(rdWrSpr0=writeSprFlags(upInstrDecEtc.fwl))
//              }
//              is (FlareCpuInstrG2EncOp.cmpbcRaRb) {
//                // Opcode 0xe: cmpbc rA, rB
//                //when (upInstrDecEtc.fwl) {
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbcRaRb
//                //} otherwise {
//                //  upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbcRaRbFlags
//                //}
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=writeSprFlags(True)
//                )
//                //disableGprWrites()
//              }
//              default {
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bubble
//                markInstrInvalid()
//              }
//            }
//          }
//          is (FlareCpuInstrEncConst.g3Grp) {
//            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g3
//            when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
//              val tempSimm = SInt(params.mainWidth bits)
//              tempSimm := (
//                upInstrEnc.g3.simm.resized
//              )
//              upInstrDecEtc.fullSimm := (
//                tempSimm.asUInt
//              )
//              //upInstrDecEtc.fullImm := (
//              //  upInstrEnc.g3.simm.resized
//              //)
//            } otherwise {
//              upInstrDecEtc.fullSimm := Cat(
//                rSavedUpInstrDecEtc.fullSimm,
//                upInstrEnc.g3.simm,
//              ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
//            }
//            upInstrDecEtc.fullPcrelSimm := (
//              upModExt.regPc
//              + upInstrDecEtc.fullSimm
//            )
//            switch (upInstrEnc.g3.op) {
//              is (FlareCpuInstrG3EncOp.blS9) {
//                // Opcode 0x0: bl simm
//                finishInstr(
//                  //isBlJl=true
//                  writeGpr=Some((FlareCpuInstrEncConst.gprLrIdx, true))
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.blSimm
//              }
//              is (FlareCpuInstrG3EncOp.braS9) {
//                // Opcode 0x1: bra simm
//                finishInstr(
//                  writeGpr=None,
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.braSimm
//              }
//              is (FlareCpuInstrG3EncOp.beqS9) {
//                // Opcode 0x2: beq simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.beqSimm
//              }
//              is (FlareCpuInstrG3EncOp.bneS9) {
//                // Opcode 0x3: bne simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bneSimm
//              }
//              is (FlareCpuInstrG3EncOp.bmiS9) {
//                // Opcode 0x4: bmi simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bmiSimm
//              }
//              is (FlareCpuInstrG3EncOp.bplS9) {
//                // Opcode 0x5: bpl simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bplSimm
//              }
//              is (FlareCpuInstrG3EncOp.bvsS9) {
//                // Opcode 0x6: bvs simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvsSimm
//              }
//              is (FlareCpuInstrG3EncOp.bvcS9) {
//                // Opcode 0x7: bvc simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bvcSimm
//              }
//              is (FlareCpuInstrG3EncOp.bgeuS9) {
//                // Opcode 0x8: bgeu simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgeuSimm
//              }
//              is (FlareCpuInstrG3EncOp.bltuS9) {
//                // Opcode 0x9: bltu simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltuSimm
//              }
//              is (FlareCpuInstrG3EncOp.bgtuS9) {
//                // Opcode 0xa: bgtu simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtuSimm
//              }
//              is (FlareCpuInstrG3EncOp.bleuS9) {
//                // Opcode 0xb: bleu simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bleuSimm
//              }
//              is (FlareCpuInstrG3EncOp.bgesS9) {
//                // Opcode 0xc: bges simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgesSimm
//              }
//              is (FlareCpuInstrG3EncOp.bltsS9) {
//                // Opcode 0xd: blts simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bltsSimm
//              }
//              is (FlareCpuInstrG3EncOp.bgtsS9) {
//                // Opcode 0xe: bgts simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.bgtsSimm
//              }
//              is (FlareCpuInstrG3EncOp.blesS9) {
//                // Opcode 0xf: bles simm
//                finishInstr(
//                  writeGpr=None,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprFlagsIdx, True, false
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.blesSimm
//              }
//            }
//          }
//          is (FlareCpuInstrEncConst.g4Grp) {
//            upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g4
//            switch (upInstrEnc.g4.op) {
//              is (FlareCpuInstrG4EncOp.jlRa) {
//                finishInstr(
//                  //isBlJl=true
//                  writeGpr=Some((FlareCpuInstrEncConst.gprLrIdx, true)),
//                  readGprRaAsRb=true,
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jlRa
//              }
//              is (FlareCpuInstrG4EncOp.jmpRa) {
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpRa
//              }
//              is (FlareCpuInstrG4EncOp.jmpIra) {
//                finishInstr(rdWrSpr0=Some(
//                  FlareCpuInstrEncConst.sprIraIdx, True, false
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.jmpIra
//              }
//              is (FlareCpuInstrG4EncOp.reti) {
//                finishInstr(
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprIraIdx, True, false,
//                  ),
//                  rdWrSpr1=Some(
//                    FlareCpuInstrEncConst.sprIeIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.reti
//              }
//              is (FlareCpuInstrG4EncOp.ei) {
//                finishInstr(rdWrSpr0=Some(
//                  FlareCpuInstrEncConst.sprIeIdx, True, true
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ei
//              }
//              is (FlareCpuInstrG4EncOp.di) {
//                finishInstr(rdWrSpr0=Some(
//                  FlareCpuInstrEncConst.sprIeIdx, True, true
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.di
//              }
//              is (FlareCpuInstrG4EncOp.pushRaRb) {
//                // Opcode 0x6: push rA, rB
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.pushRaRb
//              }
//              is (FlareCpuInstrG4EncOp.pushSaRb) {
//                // Opcode 0x7: push sA, rB
//                finishInstr(rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, false
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.pushSaRb
//              }
//              is (FlareCpuInstrG4EncOp.popRaRb) {
//                // Opcode 0x8: pop rA, rB
//                when (!(
//                  (
//                    upInstrEnc.g2.raIdx.lsb === upInstrEnc.g2.rbIdx.lsb
//                    && (
//                      upInstrEnc.g2.rbIdx
//                      =/= FlareCpuInstrEncConst.gprSpIdx
//                    )
//                  ) || (
//                    upInstrEnc.g2.raIdx === upInstrEnc.g2.rbIdx
//                  )
//                )) {
//                  finishInstr()
//                  upInstrDecEtc.decOp := FlareCpuInstrDecOp.popRaRb
//                } otherwise {
//                  markInstrInvalid()
//                }
//              }
//              is (FlareCpuInstrG4EncOp.popSaRb) {
//                // Opcode 0x9: pop sA, rB
//                finishInstr(rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, true
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.popSaRb
//              }
//              is (FlareCpuInstrG4EncOp.popPcRb) {
//                // Opcode 0xa: pop pc, rB
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.popPcRb
//              }
//              is (FlareCpuInstrG4EncOp.mulRaRb) {
//                // Opcode 0xb: mul rA, rB
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.mulRaRb
//              }
//              is (FlareCpuInstrG4EncOp.udivmodRaRb) {
//                // Opcode 0xc: udivmod rA, rB
//                finishInstr(rdWrSpr0=Some(
//                  FlareCpuInstrEncConst.sprLoIdx, True, true
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
//              }
//              is (FlareCpuInstrG4EncOp.sdivmodRaRb) {
//                // Opcode 0xd: sdivmod rA, rB
//                finishInstr(rdWrSpr0=Some(
//                  FlareCpuInstrEncConst.sprLoIdx, True, true
//                ))
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmodRaRb
//              }
//              is (FlareCpuInstrG4EncOp.lumulRaRb) {
//                // Opcode 0xe: lumul rA, rB
//                finishInstr(
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprHiIdx, True, true
//                  ),
//                  rdWrSpr1=Some(
//                    FlareCpuInstrEncConst.sprLoIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lumulRaRb
//              }
//              is (FlareCpuInstrG4EncOp.lsmulRaRb) {
//                // Opcode 0xf: lsmul rA, rB
//                finishInstr(
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprHiIdx, True, true
//                  ),
//                  rdWrSpr1=Some(
//                    FlareCpuInstrEncConst.sprLoIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsmulRaRb
//              }
//              is (FlareCpuInstrG4EncOp.udivmod64RaRb) {
//                // Opcode 0x10: udivmod64 rA, rB
//                finishInstr(
//                  gprDual64=true,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprHiIdx, True, true
//                  ),
//                  rdWrSpr1=Some(
//                    FlareCpuInstrEncConst.sprLoIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.udivmod64RaRb
//              }
//              is (FlareCpuInstrG4EncOp.sdivmod64RaRb) {
//                // Opcode 0x11: sdivmod64 rA, rB
//                finishInstr(
//                  gprDual64=true,
//                  rdWrSpr0=Some(
//                    FlareCpuInstrEncConst.sprHiIdx, True, true,
//                  ),
//                  rdWrSpr1=Some(
//                    FlareCpuInstrEncConst.sprLoIdx, True, true,
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.sdivmod64RaRb
//              }
//              is (FlareCpuInstrG4EncOp.ldubRaRb) {
//                // Opcode 0x12: ldub rA, [rB]
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldubRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.ldsbRaRb) {
//                // Opcode 0x13: ldsb rA, [rB]
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldsbRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.lduhRaRb) {
//                // Opcode 0x14: lduh rA, [rB]
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.lduhRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.ldshRaRb) {
//                // Opcode 0x15: ldsh rA, [rB]
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldshRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.ldrRaRb) {
//                // Opcode 0x16: ldr rA, [rB]
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
//              }
//              //is (FlareCpuInstrG4EncOp.reserved17) {
//              //  // Opcode 0x17: reserved
//              //  //finishInstr()
//              //  //upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
//              //  markInstrInvalid()
//              //}
//              is (FlareCpuInstrG4EncOp.stbRaRb) {
//                // Opcode 0x18: stb rA, [rB]
//                finishInstr(writeGpr=None)
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.stbRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.sthRaRb) {
//                // Opcode 0x19: sth rA, [rB]
//                finishInstr(writeGpr=None)
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.sthRaRbLdst
//              }
//              is (FlareCpuInstrG4EncOp.strRaRb) {
//                // Opcode 0x1a: str rA, [rB]
//                finishInstr(writeGpr=None)
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.strRaRbLdst
//              }
//              //is (FlareCpuInstrG4EncOp.reserved1b) {
//              //  // Opcode 0x1b: reserved
//              //  //finishInstr()
//              //  //upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrRaRbLdst
//              //  markInstrInvalid()
//              //}
//              is (FlareCpuInstrG4EncOp.cpyRaSb) {
//                // Opcode 0x1c: cpy rA, sB
//                finishInstr()
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpyRaSb
//              }
//              is (FlareCpuInstrG4EncOp.cpySaRb) {
//                // Opcode 0x1d: cpy sA, rB
//                finishInstr(
//                  rdWrSpr0=Some(
//                    upInstrEnc.g2.raIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpySaRb
//              }
//              is (FlareCpuInstrG4EncOp.cpySaSb) {
//                // Opcode 0x1e: cpy sA, sB
//                finishInstr(
//                  rdWrSpr0=Some(
//                    upInstrEnc.g2.raIdx, True, true
//                  ),
//                )
//                upInstrDecEtc.decOp := FlareCpuInstrDecOp.cpySaSb
//              }
//              default {
//                markInstrInvalid()
//              }
//            }
//          }
//          is (FlareCpuInstrEncConst.g5Grp) {
//            when (
//              upInstrEnc.g5Sg0.subgrp
//              === FlareCpuInstrEncConst.g5Sg0Subgrp
//            ) {
//              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g5Sg0
//              when (!rSavedUpInstrDecEtc.decodeTemp.indexRaRbValid) {
//                upInstrDecEtc.decodeTemp.indexRaRbValid := True
//                upInstrDecEtc.haveFullInstr := False
//              } otherwise {
//                markInstrInvalid()
//              }
//            } elsewhen (
//              upInstrEnc.g5Sg1.subgrp
//              === FlareCpuInstrEncConst.g5Sg1Subgrp
//            ) {
//              upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g5Sg1
//              upInstrDecEtc.haveFullInstr := False
//              when (!rSavedUpInstrDecEtc.decodeTemp.indexRaSimmValid) {
//                upInstrDecEtc.decodeTemp.indexRaSimmValid := True
//                upInstrDecEtc.haveFullInstr := False
//              } otherwise {
//                markInstrInvalid()
//              }
//            } otherwise {
//              //upInstrDecEtc.isInvalid := True
//              markInstrInvalid()
//            }
//          }
//          is (FlareCpuInstrEncConst.g6Grp) {
//            //upInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g6
//            markInstrInvalid()
//          }
//          is (FlareCpuInstrEncConst.g7Grp) {
//            ////psIdHaltIt := True
//            //cId.haltIt()
//            //myDoHaltIt()
//            //rMultiCycleState := MultiCycleState.G7_SUB_DECODE
//          }
//        }
//      }
//    //} otherwise { // when (the previous instruction was lpreSimmHi)
//      is (MultiCycleState.LPRE_SIMM_LO) {
//        rMultiCycleState := MultiCycleState.PRIMARY
//        upInstrDecEtc.isInvalid := False
//        upInstrDecEtc.haveFullInstr := False
//        upInstrDecEtc.fullgrp := (
//          FlareCpuInstrFullgrpDec.g0LpreLo
//        )
//        upInstrDecEtc.fwl := False
//        upInstrDecEtc.decOp := FlareCpuInstrDecOp.lpreSimmLo
//        upInstrDecEtc.fullSimm := Cat(
//          rSavedUpInstrDecEtc.fullSimm,
//          upInstrEnc.g0LpreLo,
//        ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
//        upInstrDecEtc.fullImm := Cat(
//          rSavedUpInstrDecEtc.fullImm,
//          upInstrEnc.g0LpreLo,
//        ).asUInt(upInstrDecEtc.fullImm.bitsRange)
//      }
//      is (MultiCycleState.G7_SUB_DECODE) {
//        rMultiCycleState := MultiCycleState.PRIMARY
//        when (!rSavedUpInstrDecEtc.decodeTemp.preLpreValid) {
//          //upInstrDecEtc.fullSimm := (
//          //  upInstrEnc.g7Sg0110.imm.asSInt.resized.asUInt
//          //)
//          val tempSimm = SInt(params.mainWidth bits)
//          tempSimm := (
//            upInstrEnc.g7Sg0110.imm.asSInt.resized
//          )
//          upInstrDecEtc.fullSimm := (
//            tempSimm.asUInt
//          )
//          upInstrDecEtc.fullImm := (
//            upInstrEnc.g7Sg0110.imm.resized
//          )
//        } otherwise {
//          upInstrDecEtc.fullSimm := Cat(
//            rSavedUpInstrDecEtc.fullSimm,
//            upInstrEnc.g7Sg0110.imm,
//          ).asUInt(upInstrDecEtc.fullSimm.bitsRange)
//          upInstrDecEtc.fullImm := Cat(
//            rSavedUpInstrDecEtc.fullImm,
//            upInstrEnc.g7Sg0110.imm,
//          ).asUInt(upInstrDecEtc.fullImm.bitsRange)
//        }
//        when (
//          upInstrEnc.g7Sg00.subgrp
//          === FlareCpuInstrEncConst.g7Sg00Subgrp
//        ) {
//          upInstrDecEtc.fwl := upInstrEnc.g7Sg00.w
//          switch (upInstrEnc.g7Sg00.op) {
//            is (FlareCpuInstrG7Sg00FullOpEnc.cmpbRaRb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=writeSprFlags(True),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmpbRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.lsrbRaRb) {
//              finishInstr()
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrbRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.asrbRaRb) {
//              finishInstr()
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrbRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.invalid0) {
//              markInstrInvalid()
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.cmphRaRb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=writeSprFlags(True),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.cmphRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.lsrhRaRb) {
//              finishInstr()
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.lsrhRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.asrhRaRb) {
//              finishInstr()
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.asrhRaRb
//            }
//            is (FlareCpuInstrG7Sg00FullOpEnc.invalid1) {
//              markInstrInvalid()
//            }
//          }
//        } elsewhen (
//          upInstrEnc.g7Sg010.subgrp
//          === FlareCpuInstrEncConst.g7Sg010Subgrp
//        ) {
//          switch (upInstrEnc.g7Sg010.op) {
//            is (FlareCpuInstrG7Sg010EncOp.ldrSaRb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, true
//                ),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrSaRbLdst
//            }
//            is (FlareCpuInstrG7Sg010EncOp.ldrSaSb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, true
//                ),
//                //rdWrSpr1=Some(
//                //  upInstrEnc.g2.rbIdx, True, false
//                //),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.ldrSaSbLdst
//            }
//            is (FlareCpuInstrG7Sg010EncOp.strSaRb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, false
//                ),
//                //rdWrSpr1=Some(
//                //  upInstrEnc.g2.rbIdx, True, false
//                //),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.strSaRbLdst
//            }
//            is (FlareCpuInstrG7Sg010EncOp.strSaSb) {
//              finishInstr(
//                writeGpr=None,
//                rdWrSpr0=Some(
//                  upInstrEnc.g2.raIdx, True, false
//                ),
//                //rdWrSpr1=Some(
//                //  upInstrEnc.g2.rbIdx, True, false
//                //),
//              )
//              upInstrDecEtc.decOp := FlareCpuInstrDecOp.strSaSbLdst
//            }
//          }
//        } elsewhen (
//          upInstrEnc.g7Sg0110.subgrp
//          === FlareCpuInstrEncConst.g7Sg0110Subgrp
//        ) {
//          upInstrDecEtc.decOp := FlareCpuInstrDecOp.icreloadRaSimm
//          finishInstr(
//            writeGpr=None
//          )
//          //disableRegWrites()
//        } elsewhen (
//          upInstrEnc.g7Sg01110.subgrp
//          === FlareCpuInstrEncConst.g7Sg01110Subgrp
//        ) {
//          upInstrDecEtc.decOp := FlareCpuInstrDecOp.icflush
//          finishInstr(writeGpr=None)
//          //disableRegWrites()
//        } otherwise {
//          markInstrInvalid()
//        }
//      }
//    //}
//    }
//  }
//  //}
//  when (io.ibus.ready) {
//    upInstrEnc.g0Pre.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g0LpreHi.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g0LpreLo.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g0Atomic.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g1.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g2.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g3.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g4.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g5Sg0.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g5Sg1.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g7Sg00.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g7Sg010.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g7Sg0110.assignFromBits(io.ibus.devData.asBits)
//    upInstrEnc.g7Sg01110.assignFromBits(io.ibus.devData.asBits)
//  } otherwise { // when (!io.ibus.ready)
//    //duplicateIt()
//    // `cId.haltIt()` could be wrong unless EX captures values when EX has
//    // `up.isValid`. So I think that means EX needs to capture the
//    // outputs of ID upon an `EX.up.isValid` if EX is going to stall.
//    ////psIdHaltIt := True
//    //cId.haltIt()
//    myDoHaltIt()
//    //cId.haltIt()
//    //psIdHaltIt := True
//    //cId.terminateIt()
//  }
//  //when (psExSetPc.fire) {
//  //}
//  //--------
//  //if (params.formal) {
//  //val myDidReset = Bool()
//  ////assumeInitial(!myDidReset)
//  ////assume(!myDidReset)
//  ////when (pastValid) {
//  ////  //when (ClockDomain.isResetActive) {
//  ////  //  //rDidReset := True
//  ////  //}
//  ////}
//  //val rDidReset = RegNext(myDidReset)
//  //myDidReset := rDidReset
//  //GenerationFlags.formal {
//    //println("testificate\n")
//  if (myFormalMain) {
//    //when (!pastValid) {
//    //  //assume(!myDidReset)
//    //  myDidReset := False
//    //} otherwise {
//    //  //when (ClockDomain.isResetActive) {
//    //  //}
//    //  when (
//    //    past(ClockDomain.current.isResetActive)
//    //    && !ClockDomain.current.isResetActive
//    //  ) {
//    //    myDidReset := True
//    //    //assume(myDidReset)
//    //  }
//    //  //myDidReset := True
//    //}
//    //assumeInitial(
//    //  rMultiCycleState === MultiCycleState.PRIMARY
//    //)
//    //assumeInitial(
//    //  !rDidHandleG7SubDecode
//    //)
//    //assumeInitial(
//    //  ClockDomain.isResetActive
//    //)
//    //when (ClockDomain.isResetActive) {
//    //  assume(!RegNext(ClockDomain.isResetActive))
//    //}
//    //cover(
//    //  pastValidAfterReset
//    //  && (
//    //    RegNextWhen(True, ClockDomain.isResetActive) init(False)
//    //  )
//    //  && !ClockDomain.isResetActive
//    //)
//    //cover(
//    //  
//    //  //pastValidAfterReset
//    //  ////&& (
//    //  ////  RegNextWhen(True, ClockDomain.isResetActive) init(False)
//    //  ////)
//    //  //pastValid
//    //  //&& 
//    //  myDidReset
//    //  //&& !ClockDomain.isResetActive
//    //)
//    cover(rMultiCycleState === MultiCycleState.LPRE_SIMM_LO)
//    when (
//      !pastValidAfterReset()
//    ) {
//      //assume(!rDidHandleG7SubDecode)
//    } elsewhen (
//      pastValidAfterReset()
//      ////&& (
//      ////  RegNextWhen(True, ClockDomain.isResetActive) init(False)
//      ////)
//      //pastValid
//      //&& 
//      //myDidReset
//      //&& !ClockDomain.isResetActive
//    ) {
//      ////cover(rMultiCycleState === MultiCycleState.LPRE_SIMM_LO)
//      //cover(
//      //  up.isFiring
//      //  && io.ibus.ready
//      //)
//      ////cover(
//      ////  up.isFiring
//      ////  && !psIdHaltIt
//      ////)
//      ////when (up.isFiring) {
//      ////  assume(
//      ////    io.ibus.ready
//      ////  )
//      ////  //assert(
//      ////  //  !psIdHaltIt
//      ////  //)
//      ////}
//      when (!io.ibus.ready) {
//        assert(
//          !up.isFiring
//        )
//      }
//      when (
//        //up.isValid
//        //&& 
//        !past(up.isFiring)
//        && io.ibus.ready
//      ) {
//        //assert(stable(io.ibus.ready))
//        assume(stable(io.ibus.ready))
//      }
//      assert(
//        rMultiCycleState.asBits.asUInt =/= 0x3
//      )
//      when (past(io.ibus.valid)) {
//        when (io.ibus.ready) {
//          //cover(
//          //  up.isValid
//          //)
//          cover(
//            up.isFiring
//          )
//          assert(
//            //up.isFiring
//            up.isValid
//          )
//          when (!io.ibus.valid) {
//            assume(!RegNext(io.ibus.ready))
//          }
//        }
//      }
//      when (
//        !past(up.isFiring)
//      ) {
//        //cover(io.ibus.ready)
//      }
//      //when (
//      //  up.isValid
//      //  && io.ibus.ready
//      //  //&& 
//      //) {
//      //  assert(
//      //  )
//      //}
//      //otherwise { // when (!up.isFiring)
//      //}
//      //cover(
//      //  past(up.isFiring)
//      //  && (
//      //    past(rMultiCycleState) === MultiCycleState.G7_SUB_DECODE
//      //  ) && (
//      //    rMultiCycleState === MultiCycleState.PRIMARY
//      //  )
//      //)
//      when (
//        //(
//        //  RegNextWhen(True, up.isFiring) init(False)
//        //) && (
//          //RegNext(up.isFiring) init(False)
//        //)
//        past(up.isFiring)
//      ) {
//        assert(
//          !rDidHandleG7SubDecode
//        )
//        when (
//          past(rMultiCycleState)
//          === MultiCycleState.G7_SUB_DECODE
//        ) {
//          assert(
//            rMultiCycleState === MultiCycleState.PRIMARY
//          )
//        }
//      }
//      when (up.isValid) {
//        switch (rMultiCycleState) {
//          is (MultiCycleState.PRIMARY) {
//            assert(
//              !rDidHandleG7SubDecode
//            )
//            when (
//              upInstrEnc.g0Pre.grp === FlareCpuInstrEncConst.g7Grp
//              && !rDidHandleG7SubDecode
//            ) {
//              //assert(
//              //  psIdHaltIt
//              //)
//              assert(
//                !cId.up.isReady
//              )
//              assert(
//                !cId.down.isValid
//              )
//            }
//          }
//          is (MultiCycleState.LPRE_SIMM_LO) {
//            when (RegNextWhen(True, up.isFiring) init(False)) {
//              assert(
//                !rDidHandleG7SubDecode
//              )
//            }
//          }
//          is (MultiCycleState.G7_SUB_DECODE) {
//            when (RegNextWhen(True, up.isFiring) init(False)) {
//              assert(
//                rDidHandleG7SubDecode
//              )
//            }
//          }
//        }
//      }
//      //cover(
//      //  past(up.isFiring)
//      //  && (
//      //    rMultiCycleState === MultiCycleState.LPRE_SIMM_LO
//      //  ) && (
//      //    past(rMultiCycleState) === MultiCycleState.PRIMARY
//      //  )
//      //)
//      when (
//        past(up.isFiring)
//        && (
//          rMultiCycleState === MultiCycleState.LPRE_SIMM_LO
//        )
//      ) {
//        assert(
//          past(rMultiCycleState) === MultiCycleState.PRIMARY
//        )
//      }
//    }
//    //when (
//    //  !(
//    //    RegNextWhen(psExSetPc.fire, up.isFiring) init(False)
//    //  )
//    //) {
//    //}
//    //cover (
//    //  upModExt.regPc
//    //  === (
//    //    RegNextWhen(upModExt.regPc, up.isFiring)
//    //    + (params.instrMainWidth / 8)
//    //  )
//    //)
//  }
//  //}
//  //}
//}
//case class FlareCpuExSetRegExtFuncArgs(
//  someIdx: Int,
//) {
//}

//case class FlareCpu(
//  params: FlareCpuParams,
//  optFormalTest: FlareCpuFormalTest=(
//    FlareCpuFormalTest.Dont
//  ),
//) extends Component {
//  //--------
//  val io = FlareCpuIo(params=params)
//  //val instrBmb = Bmb(p=params.busParams)
//  //val dataBmb = Bmb(p=params.busParams)
//  //val busArb = BmbArbiter(
//  //  inputsParameter=List(
//  //    params.busParams,
//  //    params.busParams,
//  //  ),
//  //  outputParameter=params.busParams,
//  //  lowerFirstPriority=false,
//  //)
//  //busArb.io.inputs(0) << instrBmb
//  //busArb.io.inputs(1) << dataBmb
//  //io.bus << busArb.io.output
//  //--------
//  def enumRegFileGprEvenNonFp = FlareCpuParams.enumRegFileGprEvenNonFp
//  def enumRegFileGprFp = FlareCpuParams.enumRegFileGprFp
//  def enumRegFileGprOddNonSp = FlareCpuParams.enumRegFileGprOddNonSp
//  def enumRegFileGprSp = FlareCpuParams.enumRegFileGprSp
//  def enumRegFileSprEven = FlareCpuParams.enumRegFileSprEven
//  def enumRegFileSprOdd = FlareCpuParams.enumRegFileSprOdd
//  def enumRegFileLim = FlareCpuParams.enumRegFileLim
//  //#define FLARE_FLAGS_Z_MASK \
//  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_Z_BITPOS) 
//  //#define FLARE_FLAGS_C_MASK \
//  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_C_BITPOS) 
//  //#define FLARE_FLAGS_V_MASK \
//  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_V_BITPOS) 
//  //#define FLARE_FLAGS_N_MASK \
//  //  (((flare_temp_t) 0x1ull) << FLARE_FLAGS_N_BITPOS) 
//
//  //#define FLARE_SIM_FLAGS_VN_MASK(bits) \
//  //  ((uint64_t) 0x1ull << (uint64_t) (bits - 1))
//  //#define FLARE_SIM_FLAGS_Z_MASK(bits) \
//  //  (FLARE_SIM_FLAGS_VN_MASK (bits) - (int64_t) 0x1ll)
//  //#define FLARE_SIM_FLAGS_C_MASK(bits) \
//  //  (FLARE_SIM_FLAGS_VN_MASK (bits) << (uint64_t) 0x1ull)
//  def myFlagsVnMask(bits: Int) = (
//    U(s"${params.mainWidth + 1}'d1") << (bits - 1)
//  )
//  def myFlagsZMask(bits: Int) = (
//    myFlagsVnMask(bits=bits) - 1
//  )
//  def myFlagsCMask(bits: Int) = (
//    myFlagsVnMask(bits=bits) << 1
//  )
//  //def performSetFlagsZn(
//  //  rawElemNumBytesPow: (Int, Int),
//  //  result: UInt,
//  //  //flagsOut: UInt,
//  //): Unit = {
//  //  //--------
//  //  //def myBits = params.elemNumBytesPow(
//  //  //  rawElemNumBytesPow=rawElemNumBytesPow
//  //  //)._2
//  //  //val outpFlags = UInt(params.mainWidth bits)
//  //  //outpFlags := (
//  //  //  //0x0
//  //  //  flags
//  //  //)
//  //  //outpFlags.allowOverride
//  //  //outpFlags(params.flagIdxZ) := (
//  //  //  result(myBits - 1 downto 0) === 0
//  //  //)
//  //  //outpFlags(params.flagIdxN) := result(myBits - 1)
//  //  //doWriteSpr(
//  //  //  regIdx=myInstrDecEtc.enumSprFlags,
//  //  //  payload=outpFlags,
//  //  //)
//  //  //--------
//  //}
//
//
//  //static INLINE void
//  //flare_sim_set_flags_zn (uint32_t bits,
//  //                          uint64_t result,
//  //                          int32_t *flags_out)
//  //{
//  //  uint64_t
//  //    temp_flags_z_mask = FLARE_SIM_FLAGS_Z_MASK (bits),
//  //    temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
//
//  //  /* set the `Z` flag */
//  //  if (result & temp_flags_z_mask)
//  //  {
//  //    *flags_out |= FLARE_FLAGS_Z_MASK;
//  //  }
//  //  else
//  //  {
//  //    *flags_out &= ~FLARE_FLAGS_Z_MASK;
//  //  }
//
//  //  /* set the `N` flag */
//  //  if (result & temp_flags_vn_mask)
//  //  {
//  //    *flags_out |= FLARE_FLAGS_N_MASK;
//  //  }
//  //  else
//  //  {
//  //    *flags_out &= ~FLARE_FLAGS_N_MASK;
//  //  }
//  //}
//  //def setFlagsZn(
//  //  bits: Int,
//  //  result: UInt,
//  //  flagsOut: UInt
//  //): Unit = {
//  //  assert(result.getWidth == params.mainWidth)
//  //  val tempFlagsZMask = myFlagsZMask(bits=bits)
//  //  val tempFlagsVnMask = myFlagsVnMask(bits=bits)
//  //  // set the `Z` flag
//  //  flagsOut(params.flagIdxZ) := (
//  //    (result & tempFlagsZMask) =/= 0
//  //  )
//  //  // set the `N` flag
//  //  flagsOut(params.flagIdxN) := (
//  //    (result & tempFlagsVnMask) =/= 0
//  //  )
//  //}
//  // Returns the sum/difference of the `add`/`sub`/`cmp`/`cmpb`/`cmph`
//  // Note: `NULL` `flags_out` indicates don't compute output flags
//  //static INLINE int32_t
//  //flare_sim_add_sub (uint32_t bits,
//  //                    int32_t operand_a,
//  //                    int32_t operand_b,
//  //                    int32_t flags_in,
//  //                    int32_t *flags_out, 
//  //                    bool with_carry_in,
//  //                    bool do_sub)
//  //{
//  //  uint64_t
//  //    ret = 0,
//  //    temp_operand_a = operand_a,
//  //    temp_operand_b = operand_b,
//  //    temp_flags_c_mask = 0,
//  //    temp_flags_vn_mask = 0;
//
//  //  if (!do_sub)
//  //  {
//  //    ret = temp_operand_a + temp_operand_b
//  //      + (with_carry_in
//  //        ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
//  //        : 0x0ull);
//  //  }
//  //  else // if (do_sub)
//  //  {
//  //    /* 6502-style subtraction */
//  //    ret = temp_operand_a + (~temp_operand_b)
//  //      + (with_carry_in 
//  //        ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
//  //        : 0x1ull);
//  //  }
//
//  //  if (flags_out != NULL)
//  //  {
//  //    temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
//  //    temp_flags_c_mask = FLARE_SIM_FLAGS_C_MASK (bits);
//
//  //    *flags_out = 0x0;
//  //    flare_sim_set_flags_zn (bits, ret, flags_out);
//
//  //    /* set the `C` flag */
//  //    if (ret & temp_flags_c_mask)
//  //    {
//  //      *flags_out |= FLARE_FLAGS_C_MASK;
//  //    }
//  //    /* set the `V` flag (6502-style) */
//  //    //if (!((temp_operand_a ^ temp_operand_b) & temp_flags_vn_mask)
//  //    //  && ((temp_operand_a ^ ret) & temp_flags_vn_mask))
//  //    /* The above ^ commented-out method is equivalent, but slower. */
//  //    if ((temp_operand_a ^ ret) & (temp_operand_b ^ ret)
//  //      & temp_flags_vn_mask)
//  //    {
//  //      *flags_out |= FLARE_FLAGS_V_MASK;
//  //    }
//  //  }
//
//  //  return (int32_t) ret;
//  //}
//  //def performAddSub(
//  //  bits: Int,
//  //  operandA: UInt,
//  //  operandB: UInt,
//  //  flagsIn: UInt,
//  //  withCarryIn: Boolean,
//  //  doSub: Boolean,
//  //  ret: UInt,
//  //  flagsOut: (UInt, Bool)=(U"32'd0", False),
//  //): Unit = {
//  //  //--------
//  //  //val ret = UInt(33 bits)
//  //  val tempRet = UInt(params.mainWidth + 1 bits)
//  //  val tempOperandA = UInt((params.mainWidth + 1) bits)
//  //  val tempOperandB = UInt((params.mainWidth + 1) bits)
//  //  val tempFlagsCMask = UInt(params.mainWidth bits)
//  //  val tempFlagsVnMask = UInt(params.mainWidth bits)
//  //  //--------
//  //  assert(ret.getWidth == params.mainWidth)
//  //  assert(operandA.getWidth == params.mainWidth)
//  //  assert(operandB.getWidth == params.mainWidth)
//  //  assert(flagsIn.getWidth == params.mainWidth)
//  //  ret := 0x0
//  //  tempOperandA := Cat(False, operandA).asUInt
//  //  tempOperandB := Cat(False, operandB).asUInt
//  //  tempFlagsCMask := 0x0
//  //  tempFlagsVnMask := 0x0
//  //  //--------
//  //  if (!doSub) {
//  //    //tempRet = temp_operand_a + temp_operand_b
//  //    //  + (with_carry_in
//  //    //    ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
//  //    //    : 0x0ull);
//  //    tempRet := (
//  //      tempOperandA + tempOperandB
//  //      + (
//  //        if (withCarryIn) (
//  //          Cat(
//  //            U(s"${params.mainWidth}'d0"),
//  //            flagsIn(params.flagIdxC)
//  //          ).asUInt
//  //        ) else ( // if (!withCarryIn)
//  //          U(s"${params.mainWidth}'d0")
//  //        )
//  //      )
//  //    )
//  //  } else { // if (doSub)
//  //    ///* 6502-style subtraction */
//  //    //tempRet = temp_operand_a + (~temp_operand_b)
//  //    //  + (with_carry_in 
//  //    //    ? ((flags_in & FLARE_FLAGS_C_MASK) >> FLARE_FLAGS_C_BITPOS)
//  //    //    : 0x1ull);
//  //    tempRet := (
//  //      tempOperandA + (~tempOperandB)
//  //      + (
//  //        if (withCarryIn) (
//  //          Cat(
//  //            U(s"${params.mainWidth}'d0"),
//  //            flagsIn(params.flagIdxC)
//  //          ).asUInt
//  //        ) else ( // if (!withCarryIn)
//  //          U(s"${params.mainWidth}'d1")
//  //        )
//  //      )
//  //    )
//  //  }
//  //  ret := tempRet(ret.bitsRange)
//  //  when (flagsOut._2) {
//  //    //temp_flags_vn_mask = FLARE_SIM_FLAGS_VN_MASK (bits);
//  //    //temp_flags_c_mask = FLARE_SIM_FLAGS_C_MASK (bits);
//  //    tempFlagsVnMask := myFlagsVnMask(bits=bits)
//  //    tempFlagsCMask := myFlagsCMask(bits=bits)
//
//  //    //*flags_out = 0x0;
//  //    //flare_sim_set_flags_zn (bits, tempRet, flags_out);
//  //    setFlagsZn(
//  //      bits=bits,
//  //      result=tempRet,
//  //      flagsOut=flagsOut._1,
//  //    )
//
//  //    ///* set the `C` flag */
//  //    //if (tempRet & temp_flags_c_mask)
//  //    //{
//  //    //  *flags_out |= FLARE_FLAGS_C_MASK;
//  //    //}
//  //    flagsOut._1(params.flagIdxC) := (
//  //      (tempRet & tempFlagsCMask) =/= 0
//  //    )
//  //    ///* set the `V` flag (6502-style) */
//  //    ////if (!((temp_operand_a ^ temp_operand_b) & temp_flags_vn_mask)
//  //    ////  && ((temp_operand_a ^ tempRet) & temp_flags_vn_mask))
//  //    ///* The above ^ commented-out method is equivalent, but slower. */
//  //    //if ((temp_operand_a ^ tempRet) & (temp_operand_b ^ tempRet)
//  //    //  & temp_flags_vn_mask)
//  //    //{
//  //    //  *flags_out |= FLARE_FLAGS_V_MASK;
//  //    //}
//  //    flagsOut._1(params.flagIdxV) := (
//  //      (
//  //        ((tempOperandA ^ tempRet) & (tempOperandB ^ tempRet)
//  //        & tempFlagsVnMask)
//  //      ) =/= 0
//  //    )
//  //  }
//  //}
//}
//object FlareCpuVerilog extends App {
//  Config.spinal.generateVerilog(FlareCpu(params=FlareCpuParams()))
//}
