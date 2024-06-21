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

object Flare32CpuLpreState
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    //noLpre,
    haveHi,
    haveLo
    = newElement();
}
case class Flare32CpuPrefixInfo(
  dataWidth: Int,
  isLpre: Boolean, 
) extends Bundle {
  val have = Bool()
  val lpreState = (isLpre) generate Flare32CpuLpreState()
  val data = UInt(dataWidth bits)
}
case class Flare32CpuAllPrefixInfo(
  params: Flare32CpuParams,
) extends Bundle {
  val pre = Flare32CpuPrefixInfo(
    dataWidth=params.preWidth,
    isLpre=false,
  )
  val lpre = Flare32CpuPrefixInfo(
    dataWidth=params.lpreWidth,
    isLpre=true,
  )
  val index = Flare32CpuPrefixInfo(
    dataWidth=params.numGprsSprsPow,
    isLpre=false,
  )
  val haveAnyPrefix = Bool()
  val havePreOrLpre = Bool()
  val haveLpreHiOnly = Bool()
  val haveFullLpre = Bool()
}
case class Flare32CpuPipePayloadDecode(
  params: Flare32CpuParams,
) extends Bundle {
  //--------
  val irq = Bool()
  val pc = UInt(params.mainWidth bits)
  val pcPlus2 = UInt(params.mainWidth bits)
  val fetchInstr = UInt(params.instrMainWidth bits)

  val instrDecEtc = /*Payload*/(Flare32CpuInstrDecEtc(params=params))
  val allPrefixInfo = /*Payload*/(Flare32CpuAllPrefixInfo(params=params))
  //--------
}
case class Flare32CpuDecodeIo(
  params: Flare32CpuParams,
  //cIfId: CtrlLink,
) extends Area {
  //val front = Node()
  //val frontPayload = Payload(Flare32CpuPipePayload(params=params))
  //val back = Node()
  val currPayload = Payload(Flare32CpuPipePayload(params=params))

  val rExecSetPc = Reg(Flow(UInt(params.mainWidth bits)))
  rExecSetPc.init(rExecSetPc.getZero)
  val rGprVec = Vec.fill(params.numGprsSprs)(
    Reg(UInt(params.mainWidth bits)) init(0x0)
  )
  val rSprVec = Vec.fill(params.numGprsSprs)(
    Reg(UInt(params.mainWidth bits)) init(0x0)
  )
}
case class Flare32CpuDecode(
  params: Flare32CpuParams,
  prevPayload: Payload[Flare32CpuPipePayload],
  cPrevCurr: CtrlLink,
  cLastMain: CtrlLink,
  lastMainExecPayload: Payload[Flare32CpuPipePayloadExec],
) extends Area {
  //--------
  val io = Flare32CpuDecodeIo(params=params)
  //--------
  //when (io.front.isValid) {
  //}
  val cPrevCurrArea = new cPrevCurr.Area {
    //val currPayload = Flare32CpuPipePayload(params=params)
    //currPayload := up(prevPayload)
    //currPayload.allowOverride
    val upPayload = Flare32CpuPipePayload(params=params)
    upPayload := RegNext(upPayload) init(upPayload.getZero)
    upPayload.allowOverride
    up(io.currPayload) := upPayload
    when (up.isFiring) {
      upPayload := up(prevPayload)
      //def myInstrDecEtc = up(prevPayload).decode.instrDecEtc
      //def myFetchInstr = up(prevPayload).decode.fetchInstr.asBits
      //val myFetchInstr = UInt(params.instrMainWidth bits)
      //val myInstrDecEtc = Flare32CpuInstrDecEtc(params=params)
      //val myAllPrefixInfo = Flare32CpuAllPrefixInfo(params=params)

      def myFetchInstr = upPayload.decode.fetchInstr
      def myInstrDecEtc = upPayload.decode.instrDecEtc
      def myAllPrefixInfo = upPayload.decode.allPrefixInfo
      myFetchInstr := (
        RegNext(myFetchInstr) init(myFetchInstr.getZero)
      )
      myInstrDecEtc := (
        RegNext(myInstrDecEtc) init(myInstrDecEtc.getZero)
      )
      myAllPrefixInfo := (
        RegNext(myAllPrefixInfo) init(myAllPrefixInfo.getZero)
      )

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
        //  myLpreInfo.lpreState === Flare32CpuLpreState.haveLo
        //)
        //!myHaveFullLpre
        //myLpreInfo.lpreState =/= Flare32CpuLpreState.haveHi
        //&& myLpreInfo.lpreState === Flare32CpuLpreState.haveLo
        //&& myLpreInfo.lpreState === Flare32CpuLpreState.have
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
          //tempInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.invalid
        }
        def doFinishedInstr(): Unit = {
          doClearPrefixes()
          myInstrDecEtc.isInvalid := False
        }
        def getNonFwdRegFunc(
          decIdx: UInt,
          isGpr: Boolean,
          whichReg: Int,
        ): UInt = {
          if (isGpr) {
            io.rGprVec(decIdx)
          } else {
            io.rSprVec(decIdx)
          }
        }
        //--------
        when (cLastMain.up.isValid) {
        // TODO: check that this works
          myInstrDecEtc.doFwdAllRegs(
            //someCtrlLink=cExWb,
            execPayload=cLastMain.up(lastMainExecPayload),
            fwdRc=false,
          )(
            getNonFwdRegFunc=getNonFwdRegFunc
          )
        }
        //--------
        switch (myInstrEnc.g0Pre.grp) {
          is (Flare32CpuInstrEncConst.g0Grp) {
            when (!myHavePreOrLpre) {
              switch (myInstrEnc.g0LpreHi.subgrp) {
                is (Flare32CpuInstrEncConst.g0PreMaskedSubgrp) {
                  myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g0Pre
                  myPreInfo.have := True
                  myHavePreOrLpre := True
                }
                is (Flare32CpuInstrEncConst.g0LpreSubgrp) {
                  myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g0Lpre
                  myLpreInfo.have := True
                  myLpreInfo.lpreState := Flare32CpuLpreState.haveHi
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
          is (Flare32CpuInstrEncConst.g1Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g1
            doFinishedInstr()
          }
          is (Flare32CpuInstrEncConst.g2Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g2
            doFinishedInstr()
          }
          is (Flare32CpuInstrEncConst.g3Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g3
            doFinishedInstr()
          }
          is (Flare32CpuInstrEncConst.g4Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g4
            when (
              myInstrEnc.g4.op =/= Flare32CpuInstrG4EncOp.indexRa
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
              when (cLastMain.up.isValid) {
              // TODO: check that this works
                myInstrDecEtc.doFwdAllRegs(
                  //someCtrlLink=cExWb,
                  execPayload=cLastMain.up(lastMainExecPayload),
                  fwdRc=true,
                )(
                  getNonFwdRegFunc=getNonFwdRegFunc
                )
              }
              //--------
            }
          }
          is (Flare32CpuInstrEncConst.g5Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g5
            doFinishedInstr()
          }
          is (Flare32CpuInstrEncConst.g6Grp) {
            myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g6
            doFinishedInstr()
          }
          is (Flare32CpuInstrEncConst.g7Grp) {
            doFinishedInstr()
            when (
              myInstrEnc.g7Sg00.subgrp
              === Flare32CpuInstrEncConst.g7Sg00Subgrp
            ) {
              myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g7Sg00
              doFinishedInstr()
            } elsewhen (
              myInstrEnc.g7Sg010.subgrp
              === Flare32CpuInstrEncConst.g7Sg010Subgrp
            ) {
              myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g7Sg010
              doFinishedInstr()
            } elsewhen (
              myInstrEnc.g7Sg0110.subgrp
              === Flare32CpuInstrEncConst.g7Sg0110Subgrp
            ) {
              myInstrDecEtc.fullgrp := Flare32CpuInstrFullgrpDec.g7Sg0110
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
        myLpreInfo.have := True
        myLpreInfo.lpreState := Flare32CpuLpreState.haveLo
        myLpreInfo.data(params.instrMainWidth - 1 downto 0).assignFromBits(
          myFetchInstr.asBits
        )
        cPrevCurr.throwIt()
        //cIfId.terminateIt() // clear `cIfId.my.valid`
      }
    }
  }
  //--------
}
