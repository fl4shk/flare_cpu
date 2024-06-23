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
import libcheesevoyage.math.LongDivPipelined

object FlareCpuLpreState
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    //noLpre,
    haveHi,
    haveLo
    = newElement();
}
case class FlareCpuPrefixInfo(
  dataWidth: Int,
  isLpre: Boolean, 
) extends Bundle {
  val have = Bool()
  val lpreState = (isLpre) generate FlareCpuLpreState()
  val data = UInt(dataWidth bits)
}
case class FlareCpuAllPrefixInfo(
  params: FlareCpuParams,
) extends Bundle {
  val pre = FlareCpuPrefixInfo(
    dataWidth=params.preWidth,
    isLpre=false,
  )
  val lpre = FlareCpuPrefixInfo(
    dataWidth=params.lpreWidth,
    isLpre=true,
  )
  val index = FlareCpuPrefixInfo(
    dataWidth=params.numGprsSprsPow,
    isLpre=false,
  )
  val haveAnyPrefix = Bool()
  val havePreOrLpre = Bool()
  val haveLpreHiOnly = Bool()
  val haveFullLpre = Bool()
}
case class FlareCpuPipePayloadDecode(
  params: FlareCpuParams,
) extends Bundle {
  //--------
  val irq = Bool()
  val pc = UInt(params.mainWidth bits)
  val pcPlus2 = UInt(params.mainWidth bits)
  val fetchInstr = UInt(params.instrMainWidth bits)

  val instrDecEtc = /*Payload*/(FlareCpuInstrDecEtc(params=params))
  val allPrefixInfo = /*Payload*/(FlareCpuAllPrefixInfo(params=params))
  //--------
}
case class FlareCpuPsDecodeIo(
  params: FlareCpuParams,
  //cIfId: CtrlLink,
) extends Area {
  //val front = Node()
  //val frontPayload = Payload(FlareCpuPipePayload(params=params))
  //val back = Node()
  //val currPayload = Payload(FlareCpuPipePayload(params=params))

  val rExecSetPc = Reg(Flow(UInt(params.mainWidth bits)))
  rExecSetPc.init(rExecSetPc.getZero)
  val rGprVec = Vec.fill(params.numGprsSprs)(
    Reg(UInt(params.mainWidth bits)) init(0x0)
  )
  val rSprVec = Vec.fill(params.numGprsSprs)(
    Reg(UInt(params.mainWidth bits)) init(0x0)
  )
}
case class FlareCpuPsDecode(
  params: FlareCpuParams,
  prevPayload: Payload[FlareCpuPipePayload],
  currPayload: Payload[FlareCpuPipePayload],
  //lastMainExecPayload: Payload[FlareCpuPipePayloadExec],
  lastMainPayload: Payload[FlareCpuPipePayload],
  cPrevCurr: CtrlLink,
  cCurrNext: CtrlLink,
  cLastMain: CtrlLink,
) extends Area {
  //--------
  val io = FlareCpuPsDecodeIo(params=params)
  //--------
  //when (io.front.isValid) {
  //}
  val cPrevCurrArea = new cPrevCurr.Area {
    //val currPayload = FlareCpuPipePayload(params=params)
    //currPayload := up(prevPayload)
    //currPayload.allowOverride
    val upPayload = FlareCpuPipePayload(params=params)
    upPayload := RegNext(upPayload) init(upPayload.getZero)
    upPayload.allowOverride
    up(currPayload) := upPayload
    def myFetchInstr = upPayload.decode.fetchInstr
    def myInstrDecEtc = upPayload.decode.instrDecEtc
    //def myAllPrefixInfo = upPayload.decode.allPrefixInfo
    val myAllPrefixInfo = Reg(cloneOf(upPayload.decode.allPrefixInfo))
    myAllPrefixInfo.init(myAllPrefixInfo.getZero)
    upPayload.decode.allPrefixInfo := myAllPrefixInfo
    myFetchInstr := (
      RegNext(myFetchInstr) init(myFetchInstr.getZero)
    )
    myInstrDecEtc := (
      RegNext(myInstrDecEtc) init(myInstrDecEtc.getZero)
    )
    //myAllPrefixInfo := (
    //  RegNext(myAllPrefixInfo) init(myAllPrefixInfo.getZero)
    //)
    when (up.isValid) {
      upPayload := up(prevPayload)
    }
      //up.ready := down.isReady
      //def myInstrDecEtc = up(prevPayload).decode.instrDecEtc
      //def myFetchInstr = up(prevPayload).decode.fetchInstr.asBits
      //val myFetchInstr = UInt(params.instrMainWidth bits)
      //val myInstrDecEtc = FlareCpuInstrDecEtc(params=params)
      //val myAllPrefixInfo = FlareCpuAllPrefixInfo(params=params)


      def myPreInfo = myAllPrefixInfo.pre
      def myLpreInfo = myAllPrefixInfo.lpre
      def myIndexInfo = myAllPrefixInfo.index
      def myHaveAnyPrefix = myAllPrefixInfo.haveAnyPrefix
      def myHavePreOrLpre = myAllPrefixInfo.havePreOrLpre
      def myHaveLpreHiOnly = myAllPrefixInfo.haveLpreHiOnly
      def myHaveFullLpre = myAllPrefixInfo.haveFullLpre

      when (
        //!myLpreInfo.have
        //|| (
        //  myLpreInfo.lpreState === FlareCpuLpreState.haveLo
        //)
        //!myHaveFullLpre
        //myLpreInfo.lpreState =/= FlareCpuLpreState.haveHi
        //&& myLpreInfo.lpreState === FlareCpuLpreState.haveLo
        //&& myLpreInfo.lpreState === FlareCpuLpreState.have
        !myHaveLpreHiOnly
      ) {
        def myInstrEnc = myInstrDecEtc.instrEnc
        myInstrEnc.g0Pre.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g0LpreHi.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g1.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g2.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g3.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g4.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g5.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g6.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g7Sg00.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g7Sg010.assignFromBits(myFetchInstr.asBits)
        myInstrEnc.g7Sg0110.assignFromBits(myFetchInstr.asBits)

        myInstrDecEtc.raIdx := myInstrEnc.g2.raIdx
        myInstrDecEtc.rbIdx := myInstrEnc.g2.rbIdx
        //tempInstrDecEtc.saIdx := tempInstrEnc.g2.raIdx
        //tempInstrDecEtc.sbIdx := tempInstrEnc.g2.rbIdx
        //when (!myIndexInfo.have) {
        //  tempInstrDecEtc.rcIdx := 0x0
        //}
        //tempInstrDecEtc.rdIdx := 0x0
        def doClearPrefixes(): Unit = {
          //tempPreInfo := tempPreInfo.getZero
          //tempLpreInfo := tempLpreInfo.getZero
          //tempHavePreOrLpre := tempHavePreOrLpre.getZero
          //tempIndexInfo := tempIndexInfo.getZero
          //tempAllPrefixInfo := tempAllPrefixInfo.getZero
          myAllPrefixInfo := myAllPrefixInfo.getZero
        }
        def doInvalidInstr(): Unit = {
          doClearPrefixes()
          myInstrDecEtc.isInvalid := True
          //tempInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.invalid
        }
        def doFinishedInstr(): Unit = {
          doClearPrefixes()
          myInstrDecEtc.isInvalid := False
        }
        def getNonFwdRegFunc(
          decIdx: UInt,
          isGpr: Boolean,
          //whichReg: Int,
        ): UInt = {
          if (isGpr) {
            io.rGprVec(decIdx)
          } else {
            io.rSprVec(decIdx)
          }
        }
        //--------
        //for (idx <- 0 until params.numGprsSprs) {
        //  myInstrDecEtc.nonFwdGprVec(idx) := io.rGprVec(idx)
        //  myInstrDecEtc.nonFwdSprVec(idx) := io.rSprVec(idx)
        //}
        //--------
        //when (cLastMain.up.isValid) {
          // TODO: check that this work s
          //// if the write-back stage has `!up.isValid`, then
          //// we'd already have the correct data in the registers that
          //// aren't being forwarded to this pipeline stage. The Exec stage
          //// is the only one that can stall with `!isReady` following the
          //// Decode stage, since Write-back doesn't ever provide
          //// backpressure, and there are no stages between Exec and
          //// Write-back (at least at the time of this writing... not sure I
          //// want to add more stages there).
          myInstrDecEtc.doFwdAllRegs(
            //someCtrlLink=cExWb,
            execPayload=cLastMain.up(lastMainPayload).exec,
            fwdRc=false,
            extCond=(
              //cLastMain.up.isFiring
              cLastMain.up.isValid
            ),
          )(
            getNonFwdRegFunc=getNonFwdRegFunc
          )
        //}
        //--------
        switch (myInstrEnc.g0Pre.grp) {
          is (FlareCpuInstrEncConst.g0Grp) {
            when (!myHavePreOrLpre) {
              switch (myInstrEnc.g0LpreHi.subgrp) {
                is (FlareCpuInstrEncConst.g0PreMaskedSubgrp) {
                  myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0Pre
                  myPreInfo.have := True
                  myHavePreOrLpre := True
                }
                is (FlareCpuInstrEncConst.g0LpreSubgrp) {
                  myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g0Lpre
                  myLpreInfo.have := True
                  myLpreInfo.lpreState := FlareCpuLpreState.haveHi
                  myHaveLpreHiOnly := True
                  myHavePreOrLpre := True
                }
                default {
                  doInvalidInstr()
                }
              }
            } otherwise {
              doInvalidInstr()
            }
          }
          is (FlareCpuInstrEncConst.g1Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g1
            doFinishedInstr()
          }
          is (FlareCpuInstrEncConst.g2Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g2
            doFinishedInstr()
          }
          is (FlareCpuInstrEncConst.g3Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g3
            doFinishedInstr()
          }
          is (FlareCpuInstrEncConst.g4Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g4
            when (
              myInstrEnc.g4.op =/= FlareCpuInstrG4EncOp.indexRa
            ) {
              doFinishedInstr()
            } otherwise {
              myIndexInfo.have := True
              myInstrDecEtc.rcIdx := myInstrEnc.g4.raIdx
              //myInstrDecEtc.rc := doFwdWbReg(
              //  decIdx=myInstrDecEtc.rcIdx,
              //  isGpr=true,
              //)
              //--------
              //when (cLastMain.up.isValid) {
                // TODO: check that this works
                myInstrDecEtc.doFwdAllRegs(
                  //someCtrlLink=cExWb,
                  execPayload=cLastMain.up(lastMainPayload).exec,
                  fwdRc=true,
                  extCond=(
                    //cLastMain.up.isFiring
                    cLastMain.up.isValid
                  ),
                )(
                  getNonFwdRegFunc=getNonFwdRegFunc
                )
              //}
              //--------
            }
          }
          is (FlareCpuInstrEncConst.g5Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g5
            doFinishedInstr()
          }
          is (FlareCpuInstrEncConst.g6Grp) {
            myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g6
            doFinishedInstr()
          }
          is (FlareCpuInstrEncConst.g7Grp) {
            doFinishedInstr()
            when (
              myInstrEnc.g7Sg00.subgrp
              === FlareCpuInstrEncConst.g7Sg00Subgrp
            ) {
              myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g7Sg00
              doFinishedInstr()
            } elsewhen (
              myInstrEnc.g7Sg010.subgrp
              === FlareCpuInstrEncConst.g7Sg010Subgrp
            ) {
              myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g7Sg010
              doFinishedInstr()
            } elsewhen (
              myInstrEnc.g7Sg0110.subgrp
              === FlareCpuInstrEncConst.g7Sg0110Subgrp
            ) {
              myInstrDecEtc.fullgrp := FlareCpuInstrFullgrpDec.g7Sg0110
              doFinishedInstr()
            } otherwise {
              // invalid instruction, NOP
              //myInstrDecEtc.isNop := True
              //cIfId.haltIt()
              //myInstrDecEtc.isInvalid := True
              doInvalidInstr()
            }
          }
        }
        //myInstrDecEtc := myInstrDecEtc
      } otherwise {
        myHaveFullLpre := True
        myHaveLpreHiOnly := False
        myLpreInfo.have := True
        myLpreInfo.lpreState := FlareCpuLpreState.haveLo
        myLpreInfo.data(params.instrMainWidth - 1 downto 0).assignFromBits(
          myFetchInstr.asBits
        )
        //// TODO: add `throwIt()` back
        /*cPrevCurr.*/throwIt(usingReady=true)
        //cIfId.terminateIt() // clear `cIfId.my.valid`
      }
  }
  //--------
}
