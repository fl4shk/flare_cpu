package flare_cpu
import spinal.core._
//import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb._
import spinal.lib.bus.simple._
import scala.collection.mutable.ArrayBuffer
import libcheesevoyage.general._

//object FlareCpuMulKind
//extends SpinalEnum(defaultEncoding=binarySequential) {
//  val
//    MUL32,
//    LUMUL,
//    LSMUL
//    = newElement();
//}
case class FlareCpuMulIo(
  inpAWidth: Int,
  inpBWidth: Int,
  outpWidth: Int,
  haveLongMul: Boolean=true,
  async: Boolean=false,
) extends Bundle {
  val inpValid = (!async) generate (
    in Bool()
  )
  val inpY = in UInt(inpBWidth bits)
  val inpX = in UInt(inpAWidth bits)
  val inpLongMul = (haveLongMul) generate (
    in Bool()
  )
  val inpSigned = (haveLongMul) generate (
    in Bool()
  )
  //val inpKind = (haveLongMul) generate (
  //  in(FlareCpuMulKind())
  //)
  val outpProd = out UInt(outpWidth bits)
  val outpValid = (!async) generate (
    out Bool()
  )
}
////case class FlareCpuAdc32p32To32Async(
////) extends Component {
////  //--------
////  val io = new Bundle {
////    val inpY = in UInt(32 bits)
////    val inpX = in UInt(32 bits)
////    val inpCarry = in Bool()
////    val outpSum = out UInt(32 bits)
////    val outpCarry = out Bool()
////  }
////  //--------
////  //io.outpSum := io.inpX + io.inpY
////  //val tempSum = UInt(33 bits)
////  val tempSum = (
////    Cat(U"1'b0", io.inpY).asUInt
////    + Cat(U"1'b0", io.inpX).asUInt
////    + Cat(U"32'd0", io.inpCarry).asUInt
////  )
////  io.outpCarry := tempSum.msb
////  io.outpSum := tempSum(io.outpSum.bitsRange)
////  //--------
////}
//case class FlareCpuMul16x16To32Async(
//) extends Component {
//  //--------
//  val io = FlareCpuMulIo(
//    inpAWidth=16,
//    inpBWidth=16,
//    outpWidth=32,
//    haveLongMul=false,
//    async=true,
//  )
//  //--------
//  io.outpProd := io.inpX * io.inpY
//  //--------
//}
//case class FlareCpuMul32(
//  //useTwoInnerMul: Boolean=true
//  optNumInnerMuls: Int=4
//) extends Component {
//  //--------
//  assert(
//    //optNumInnerMuls == 1
//    //|| optNumInnerMuls == 2
//    ////|| optNumInnerMuls == 3
//    //|| optNumInnerMuls == 4
//    optNumInnerMuls >= 1
//  )
//  assert(
//    //optNumInnerMuls <= 3
//    optNumInnerMuls <= 4
//  )
//  // actually there seems to be no
//  // optimization in performance from having three or four 16x16=32
//  // multipliers without also doing 32-bit add with carry
//  //--------
//  val io = FlareCpuMulIo(
//    inpAWidth=32,
//    inpBWidth=32,
//    outpWidth=64,
//    haveLongMul=true,
//  )
//  //--------
//  val myMulArr = Array.fill(optNumInnerMuls)(FlareCpuMul16x16To32Async())
//  for (idx <- 0 until myMulArr.size) {
//    myMulArr(idx).setName(s"myMulArr_${idx}")
//    myMulArr(idx).io.inpX := 0x0
//    myMulArr(idx).io.inpY := 0x0
//  }
//  //val mainAdc = FlareCpuAdc32p32To32Async()
//  //val myAdcArr = Array.fill(
//  //  if (optNumInnerMuls == 1) (
//  //    5
//  //  ) else ( // if (optNumInnerMuls == 2)
//  //    //4
//  //    //5
//  //    6
//  //  ) 
//  //  //else ( // if (optNumInnerMuls == 4)
//  //  //  6
//  //  //)
//  //)(FlareCpuAdc32p32To32Async())
//  //for (idx <- 0 until myAdcArr.size) {
//  //  myAdcArr(idx).setName(s"myAdcArr_${idx}")
//  //  myAdcArr(idx).io.inpX := 0x0
//  //  myAdcArr(idx).io.inpY := 0x0
//  //  myAdcArr(idx).io.inpCarry := False
//  //}
//  //--------
//  //val rTempCarryZ0 = Reg(Bool()) init(False)
//  //val rTempCarryZ1 = Reg(Bool()) init(False)
//  //val rTempCarryZ2Negate = Reg(Bool()) init(False)
//  val rOutpValid = Reg(Bool()) init(False)
//  val rOutpProd = Reg(UInt(64 bits)) init(0x0)
//  io.outpValid := rOutpValid
//  io.outpProd := rOutpProd
//
//  val rInpX = Reg(UInt(32 bits)) init(0x0)
//  val rInpY = Reg(UInt(32 bits)) init(0x0)
//  //val rInpAWasNegative = Reg(Bool()) init(False)
//  //val rInpBWasNegative = Reg(Bool()) init(False)
//  val rOutpWillBeNegative = Reg(Bool()) init(False)
//  //val rInpAAbs = Reg(UInt(32 bits)) init(0x0)
//  //val rInpBAbs = Reg(UInt(32 bits)) init(0x0)
//  //val rInpLumul = Reg(Bool()) init(False)
//  //val rInpKind = Reg(FlareCpuMulKind()) init(FlareCpuMulKind.MUL32)
//  //val rInpLongMul = Reg(Bool()) init(False)
//  val rInpSigned = Reg(Bool()) init(False)
//  //val rZ0 = Reg(UInt(32 bits)) init(0x0)
//  //val rZ1X0Y1 = Reg(UInt(32 bits)) init(0x0)
//  //val rZ1X1Y0 = Reg(UInt(32 bits)) init(0x0)
//  //val rZ1PairSum = Reg(UInt(64 bits)) init(0x0)
//  //val rZ2 = Reg(UInt(32 bits)) init(0x0)
//  val rY0X0 = Reg(UInt(32 bits)) init(0x0)
//  val rY0X1 = Reg(UInt(32 bits)) init(0x0)
//  val rY1X0 = Reg(UInt(32 bits)) init(0x0)
//  val rY1X1 = Reg(UInt(32 bits)) init(0x0)
//
//  //val rY0X1PlusY1X0Lsl16 = Reg(UInt(64 bits)) init(0x0)
//  val rZ1Lsl16 = Reg(UInt(64 bits)) init(0x0)
//  val rZ2Lsl32PlusZ0 = Reg(UInt(64 bits)) init(0x0)
//  /*
//        y1    y0
//        x1    x0
//    ------------
//            y0x0
//        y0x1<<16
//        y1x0<<16
//    y1x1<<32
//    ------------
//  */
//
//  def halfOf64Hi = 63 downto 32
//  def halfOf64Lo = 31 downto 0
//  def halfOf32Hi = 31 downto 16
//  def halfOf32Lo = 15 downto 0
//
//  //--------
//  def rInpXHi = rInpX(halfOf32Hi)
//  def rInpXLo = rInpX(halfOf32Lo)
//  //--------
//  def rInpYHi = rInpY(halfOf32Hi)
//  def rInpYLo = rInpY(halfOf32Lo)
//  //--------
//  def rY0X0Hi = rY0X0(halfOf32Hi)
//  def rY0X0Lo = rY0X0(halfOf32Lo)
//  //--------
//  def rY0X1Hi = rY0X1(halfOf32Hi)
//  def rY0X1Lo = rY0X1(halfOf32Lo)
//  //--------
//  def rY1X0Hi = rY1X0(halfOf32Hi)
//  def rY1X0Lo = rY1X0(halfOf32Lo)
//  //--------
//  def rY1X1Hi = rY1X1(halfOf32Hi)
//  def rY1X1Lo = rY1X1(halfOf32Lo)
//  //--------
//  //def rY0X1PlusY1X0Lsl16Hi = rY0X1PlusY1X0Lsl16(halfOf64Hi)
//  //def rY0X1PlusY1X0Lsl16Lo = rY0X1PlusY1X0Lsl16(halfOf64Lo)
//  //--------
//  def rOutpProdHi = rOutpProd(halfOf64Hi)
//  def rOutpProdLo = rOutpProd(halfOf64Lo)
//  //--------
//  def doStateIdle[
//    StateT <: SpinalEnum
//  ](
//    rState: SpinalEnumCraft[StateT],
//    tgtStateMul32: SpinalEnumCraft[StateT],
//    tgtStateMul64: SpinalEnumCraft[StateT],
//  ): Unit = {
//      when (io.inpValid) {
//        rOutpValid := False
//        rOutpProd := 0x0
//        //rState := State.MUL_PARTS
//        when (!io.inpLongMul) {
//          rState := tgtStateMul32
//          rY1X1 := 0x0
//        } otherwise {
//          rState := tgtStateMul64
//        }
//      }
//      when (!io.inpSigned) {
//        rInpX := io.inpX.resized
//        rInpY := io.inpY.resized
//      } otherwise {
//        // compute absolute values for `rInpX` and `rInpY`
//        when (io.inpX.msb) {
//          rInpX := (-io.inpX.asSInt.resized).asUInt
//        } otherwise {
//          rInpX := io.inpX.resized
//        }
//        when (io.inpY.msb) {
//          rInpY := (-io.inpY.asSInt.resized).asUInt
//        } otherwise {
//          rInpY := io.inpY.resized
//        }
//      }
//      rOutpWillBeNegative := (
//        //io.inpSigned
//        //&&
//        (
//          io.inpX.msb =/= io.inpY.msb
//        )
//      )
//      //rInpLongMul := io.inpLongMul
//      rInpSigned := io.inpSigned
//  }
//  def doStateAdd64TwoPairs[
//    StateT <: SpinalEnum
//  ](
//    rState: SpinalEnumCraft[StateT],
//    tgtState: SpinalEnumCraft[StateT],
//  ): Unit = {
//    //when (!(rInpLongMul && rInpSigned)) {
//    //  rState := 
//    //}
//    //rState := State.LUMUL_FINAL_PROD
//    rState := tgtState
//    val tempZ1Lsl16 = (rY0X1 + rY1X0) << 16
//    println(s"tempZ1Lsl16.getWidth: ${tempZ1Lsl16.getWidth}")
//    rZ1Lsl16 := tempZ1Lsl16.resized
//    rZ2Lsl32PlusZ0 := Cat(rY1X1, rY0X0).asUInt
//  }
//
//  def doStateLumulFinalProd[
//    StateT <: SpinalEnum
//  ](
//    rState: SpinalEnumCraft[StateT],
//    tgtState: SpinalEnumCraft[StateT],
//    idleState: SpinalEnumCraft[StateT],
//  ): Unit = {
//    when (!(rInpSigned && rOutpWillBeNegative)) {
//      rState := idleState //State.IDLE
//      rOutpValid := True
//    } otherwise {
//      rState := tgtState//State.LSMUL_FINAL_PROD
//    }
//    rOutpProd := rZ1Lsl16 + rZ2Lsl32PlusZ0
//  }
//  def doStateLsmulFinalProd[
//    StateT <: SpinalEnum
//  ](
//    rState: SpinalEnumCraft[StateT],
//    idleState: SpinalEnumCraft[StateT],
//  ): Unit = {
//    rState := idleState
//    rOutpValid := True
//    rOutpProd := (-rOutpProd.asSInt.resized).asUInt
//  }
//  val fourMulArea = new Area {
//    object State extends SpinalEnum(defaultEncoding=binarySequential) {
//      val
//        IDLE,
//        MUL_PARTS,
//        ADD64_TWO_PAIRS,
//        LUMUL_FINAL_PROD,
//        LSMUL_FINAL_PROD
//        = newElement();
//    }
//    val rState = (optNumInnerMuls == 4) generate (
//      Reg(State()) init(State.IDLE)
//    )
//    if (optNumInnerMuls == 4) {
//      switch (rState) {
//        is (State.IDLE) {
//          doStateIdle(
//            rState=rState,
//            tgtStateMul32=State.MUL_PARTS,
//            tgtStateMul64=State.MUL_PARTS,
//          )
//        }
//        is (State.MUL_PARTS) {
//          rState := State.ADD64_TWO_PAIRS
//          //rY0X0 := rInpYLo * rInpXLo
//          myMulArr(0).io.inpY := rInpYLo
//          myMulArr(0).io.inpX := rInpXLo
//          rY0X0 := myMulArr(0).io.outpProd
//
//          myMulArr(1).io.inpY := rInpYLo
//          myMulArr(1).io.inpX := rInpXHi
//          rY0X1 := myMulArr(1).io.outpProd
//
//          myMulArr(2).io.inpY := rInpYHi
//          myMulArr(2).io.inpX := rInpXLo
//          rY1X0 := myMulArr(2).io.outpProd
//
//          myMulArr(3).io.inpY := rInpYHi
//          myMulArr(3).io.inpX := rInpXHi
//          rY1X1 := myMulArr(3).io.outpProd
//        }
//        is (State.ADD64_TWO_PAIRS) {
//          doStateAdd64TwoPairs(
//            rState=rState,
//            tgtState=State.LUMUL_FINAL_PROD,
//          )
//        }
//        is (State.LUMUL_FINAL_PROD) {
//          doStateLumulFinalProd(
//            rState=rState,
//            tgtState=State.LSMUL_FINAL_PROD,
//            idleState=State.IDLE,
//          )
//        }
//        is (State.LSMUL_FINAL_PROD) {
//          doStateLsmulFinalProd(
//            rState=rState,
//            idleState=State.IDLE,
//          )
//        }
//      }
//    }
//  }
//  val threeMulArea = new Area {
//    object State extends SpinalEnum(defaultEncoding=binarySequential) {
//      val
//        IDLE,
//        MUL64_MUL_Z2,
//        MUL32_NEEDED_PARTS,
//        ADD64_TWO_PAIRS,
//        LUMUL_FINAL_PROD,
//        LSMUL_FINAL_PROD
//        = newElement();
//    }
//    val rState = (optNumInnerMuls == 4) generate (
//      Reg(State()) init(State.IDLE)
//    )
//    if (optNumInnerMuls == 4) {
//      switch (rState) {
//        is (State.IDLE) {
//          doStateIdle(
//            rState=rState,
//            tgtStateMul32=State.MUL32_NEEDED_PARTS,
//            tgtStateMul64=State.MUL64_MUL_Z2,
//          )
//        }
//        is (State.MUL64_MUL_Z2) {
//          rState := State.MUL32_NEEDED_PARTS
//
//          myMulArr(0).io.inpY := rInpYHi
//          myMulArr(0).io.inpX := rInpXHi
//          rY1X1 := myMulArr(0).io.outpProd
//        }
//        is (State.MUL32_NEEDED_PARTS) {
//          rState := State.ADD64_TWO_PAIRS
//          //rY0X0 := rInpYLo * rInpXLo
//          myMulArr(0).io.inpY := rInpYLo
//          myMulArr(0).io.inpX := rInpXLo
//          rY0X0 := myMulArr(0).io.outpProd
//
//          myMulArr(1).io.inpY := rInpYLo
//          myMulArr(1).io.inpX := rInpXHi
//          rY0X1 := myMulArr(1).io.outpProd
//
//          myMulArr(2).io.inpY := rInpYHi
//          myMulArr(2).io.inpX := rInpXLo
//          rY1X0 := myMulArr(2).io.outpProd
//        }
//        is (State.ADD64_TWO_PAIRS) {
//          doStateAdd64TwoPairs(
//            rState=rState,
//            tgtState=State.LUMUL_FINAL_PROD,
//          )
//        }
//        is (State.LUMUL_FINAL_PROD) {
//          doStateLumulFinalProd(
//            rState=rState,
//            tgtState=State.LSMUL_FINAL_PROD,
//            idleState=State.IDLE,
//          )
//        }
//        is (State.LSMUL_FINAL_PROD) {
//          doStateLsmulFinalProd(
//            rState=rState,
//            idleState=State.IDLE,
//          )
//        }
//      }
//    }
//  }
//  val twoMulArea = new Area {
//    object State extends SpinalEnum(defaultEncoding=binarySequential) {
//      val
//        IDLE,
//        MUL_Z0_Z2,
//        MUL_Z1_PAIR,
//        ADD64_TWO_PAIRS,
//        LUMUL_FINAL_PROD,
//        LSMUL_FINAL_PROD
//        = newElement();
//    }
//    val rState = (optNumInnerMuls == 2) generate (
//      Reg(State()) init(State.IDLE)
//    )
//    if (optNumInnerMuls == 2) {
//      switch (rState) {
//        is (State.IDLE) {
//          doStateIdle(
//            rState=rState,
//            tgtStateMul32=State.MUL_Z0_Z2,
//            tgtStateMul64=State.MUL_Z0_Z2,
//          )
//        }
//        is (State.MUL_Z0_Z2) {
//          rState := State.MUL_Z1_PAIR
//          //rY0X0 := rInpYLo * rInpXLo
//          myMulArr(0).io.inpY := rInpYLo
//          myMulArr(0).io.inpX := rInpXLo
//          rY0X0 := myMulArr(0).io.outpProd
//
//          myMulArr(1).io.inpY := rInpYHi
//          myMulArr(1).io.inpX := rInpXHi
//          rY1X1 := myMulArr(1).io.outpProd
//        }
//        is (State.MUL_Z1_PAIR) {
//          rState := State.ADD64_TWO_PAIRS
//
//          myMulArr(1).io.inpY := rInpYLo
//          myMulArr(1).io.inpX := rInpXHi
//          rY0X1 := myMulArr(1).io.outpProd
//
//          myMulArr(0).io.inpY := rInpYHi
//          myMulArr(0).io.inpX := rInpXLo
//          rY1X0 := myMulArr(0).io.outpProd
//        }
//        is (State.ADD64_TWO_PAIRS) {
//          doStateAdd64TwoPairs(
//            rState=rState,
//            tgtState=State.LUMUL_FINAL_PROD,
//          )
//        }
//        is (State.LUMUL_FINAL_PROD) {
//          doStateLumulFinalProd(
//            rState=rState,
//            tgtState=State.LSMUL_FINAL_PROD,
//            idleState=State.IDLE,
//          )
//        }
//        is (State.LSMUL_FINAL_PROD) {
//          doStateLsmulFinalProd(
//            rState=rState,
//            idleState=State.IDLE,
//          )
//        }
//      }
//    }
//  }
//  val oneMulArea = new Area {
//    object State extends SpinalEnum(defaultEncoding=binarySequential) {
//      val
//        IDLE,
//        MUL_Z2,
//        MUL_Z0,
//        MUL_Z1_Y0X1,
//        MUL_Z1_Y1X0,
//        ADD64_TWO_PAIRS,
//        LUMUL_FINAL_PROD,
//        LSMUL_FINAL_PROD
//        = newElement();
//    }
//    val rState = (optNumInnerMuls == 2) generate (
//      Reg(State()) init(State.IDLE)
//    )
//    if (optNumInnerMuls == 2) {
//      switch (rState) {
//        is (State.IDLE) {
//          doStateIdle(
//            rState=rState,
//            tgtStateMul32=State.MUL_Z0,
//            tgtStateMul64=State.MUL_Z2,
//          )
//        }
//        is (State.MUL_Z2) {
//          rState := State.MUL_Z0
//
//          myMulArr(0).io.inpY := rInpYHi
//          myMulArr(0).io.inpX := rInpXHi
//          rY1X1 := myMulArr(0).io.outpProd
//        }
//        is (State.MUL_Z0) {
//          rState := State.MUL_Z1_Y0X1
//          //rY0X0 := rInpYLo * rInpXLo
//          myMulArr(0).io.inpY := rInpYLo
//          myMulArr(0).io.inpX := rInpXLo
//          rY0X0 := myMulArr(0).io.outpProd
//        }
//        is (State.MUL_Z1_Y0X1) {
//          rState := State.MUL_Z1_Y1X0
//
//          myMulArr(0).io.inpY := rInpYLo
//          myMulArr(0).io.inpX := rInpXHi
//          rY0X1 := myMulArr(0).io.outpProd
//        }
//        is (State.MUL_Z1_Y1X0) {
//          rState := State.ADD64_TWO_PAIRS
//
//          myMulArr(0).io.inpY := rInpYHi
//          myMulArr(0).io.inpX := rInpXLo
//          rY1X0 := myMulArr(0).io.outpProd
//        }
//        is (State.ADD64_TWO_PAIRS) {
//          doStateAdd64TwoPairs(
//            rState=rState,
//            tgtState=State.LUMUL_FINAL_PROD,
//          )
//        }
//        is (State.LUMUL_FINAL_PROD) {
//          doStateLumulFinalProd(
//            rState=rState,
//            tgtState=State.LSMUL_FINAL_PROD,
//            idleState=State.IDLE,
//          )
//        }
//        is (State.LSMUL_FINAL_PROD) {
//          doStateLsmulFinalProd(
//            rState=rState,
//            idleState=State.IDLE,
//          )
//        }
//      }
//    }
//  }
//  //val threeMulArea = new Area {
//  //  object State extends SpinalEnum(defaultEncoding=binarySequential) {
//  //    val
//  //      IDLE,
//  //      MUL64_LSMUL_SET_ABS,
//  //      MUL_Y0X1_Y1X0_Y0X0,
//  //      MUL_
//  //  }
//  //  val rState = (optNumInnerMuls == 3) generate (
//  //    Reg(State()) init(State.IDLE)
//  //  )
//  //}
//  //val fourMulArea = new Area {
//  //  object State extends SpinalEnum(defaultEncoding=binarySequential) {
//  //    val
//  //      ILDE,
//  //      MUL64_LSMUL_SET_ABS,
//  //      MUL_VIA_INNER_MULS,
//  //      ADC32LO_Y0X1_Y1X0,
//  //      ADC32LO_Z1_Y0X0
//  //      = newElement();
//  //  }
//  //}
//  //val oneMulArea = new Area {
//  //  object State extends SpinalEnum(defaultEncoding=binarySequential) {
//  //    val
//  //      IDLE,
//  //      MUL64_LSMUL_SET_ABS,
//  //      MUL_Y0X1,
//  //      MUL_Y1X0,
//  //      MUL_Y0X0_ADC32LO_Y0X1_Y1X0,
//  //      MUL32_FINAL_PROD_ADC32LO_Y0X0,
//  //      MUL64_ADC32HI_Y0X1_Y1X0,
//  //      MUL64_LSMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0,
//  //      MUL64_LSMUL_NEGATE_ADC32LO,
//  //      MUL64_LSMUL_FINAL_PROD_NEGATE_ADC32HI
//  //      = newElement();
//  //  }
//  //  val rState = (optNumInnerMuls == 1) generate (
//  //    Reg(State()) init(State.IDLE)
//  //  )
//  //  if (optNumInnerMuls == 1) {
//  //    switch (rState) {
//  //      is (State.IDLE) {
//  //      }
//  //      is (State.MUL64_LSMUL_SET_ABS) {
//  //      }
//  //      is (State.MUL_Y0X1) {
//  //      }
//  //      is (State.MUL_Y1X0) {
//  //      }
//  //      is (State.MUL_Y0X0_ADC32LO_Y0X1_Y1X0) {
//  //      }
//  //      is (State.MUL32_FINAL_PROD_ADC32LO_Y0X0) {
//  //      }
//  //      is (State.MUL64_ADC32HI_Y0X1_Y1X0) {
//  //      }
//  //      is (State.MUL64_LSMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0) {
//  //      }
//  //      is (State.MUL64_LSMUL_NEGATE_ADC32LO) {
//  //      }
//  //      is (State.MUL64_LSMUL_FINAL_PROD_NEGATE_ADC32HI) {
//  //      }
//  //    }
//  //  }
//  //}
//  //val twoMulArea = new Area {
//  //  object State extends SpinalEnum(defaultEncoding=binarySequential) {
//  //    val
//  //      IDLE,
//  //      MUL64_LSMUL_SET_ABS,
//  //      MUL_Y0X1_Y1X0,
//  //      MUL_Y0X0_Y1X1_ADC32LO_Y0X1_Y1X0,
//  //      MUL32_FINAL_PROD_ADC32LO_Y0X0,
//  //      MUL64_ADC32HI_Y0X1_Y1X0,
//  //      MUL64_LUMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0,
//  //      MUL64_LSMUL_NEGATE_ADC32LO,
//  //      MUL64_LSMUL_FINAL_PROD_NEGATE_ADC32HI
//  //      = newElement();
//  //  }
//  //  val rState = (optNumInnerMuls == 2) generate (
//  //    Reg(State()) init(State.IDLE)
//  //  )
//  //  if (optNumInnerMuls == 2) {
//  //    switch (rState) {
//  //      is (State.IDLE) {
//  //        when (io.inpValid) {
//  //          rOutpValid := False
//  //          rOutpProd := 0x0
//  //          when (!(io.inpLongMul && io.inpSigned)) {
//  //            rState := State.MUL_Y0X1_Y1X0
//  //          } otherwise {
//  //            rState := State.MUL64_LSMUL_SET_ABS
//  //          }
//  //        }
//  //        rInpX := io.inpX.resized
//  //        rInpY := io.inpY.resized
//  //        rOutpWillBeNegative := (
//  //          //io.inpSigned
//  //          //&&
//  //          (
//  //            io.inpX.msb =/= io.inpY.msb
//  //          )
//  //        )
//  //        rTempCarryZ0 := False
//  //        rTempCarryZ1 := False
//  //        rInpLongMul := io.inpLongMul
//  //        rInpSigned := io.inpSigned
//  //      }
//  //      is (State.MUL64_LSMUL_SET_ABS) {
//  //        rState := State.MUL_Y0X1_Y1X0
//  //        when (rInpX.msb) {
//  //          rInpX := (-rInpX.asSInt).asUInt
//  //        }
//  //        when (rInpY.msb) {
//  //          rInpY := (-rInpY.asSInt).asUInt
//  //        }
//  //      }
//  //      is (State.MUL_Y0X1_Y1X0) {
//  //        rState := State.MUL_Y0X0_Y1X1_ADC32LO_Y0X1_Y1X0
//
//  //        myMulArr(0).io.inpX := rInpXLo
//  //        myMulArr(0).io.inpY := rInpYHi
//  //        rY0X1 := myMulArr(0).io.outpProd
//
//  //        myMulArr(1).io.inpX := rInpXHi
//  //        myMulArr(1).io.inpY := rInpYLo
//  //        rY1X0 := myMulArr(1).io.outpProd
//  //      }
//  //      is (State.MUL_Y0X0_Y1X1_ADC32LO_Y0X1_Y1X0) {
//  //        rState := (
//  //          //State.ADC32HI_Y0X1_Y1X0
//  //          State.MUL32_FINAL_PROD_ADC32LO_Y0X0
//  //        )
//
//  //        //myAdcArr(0).io.inpX := (rY0X1Lo << 16)(halfOf64Lo)
//  //        //myAdcArr(0).io.inpY := (rY1X0Lo << 16)(halfOf64Lo)
//  //        myAdcArr(0).io.inpX := Cat(rY0X1Lo, U"16'd0").asUInt
//  //        myAdcArr(0).io.inpY := Cat(rY1X0Lo, U"16'd0").asUInt
//  //        myAdcArr(0).io.inpCarry := False
//  //        rY0X1PlusY1X0Lsl16Lo := myAdcArr(0).io.outpSum
//  //        //rOutpProdLo := myAdcArr(0).io.outpSum
//  //        rTempCarryZ1 := myAdcArr(0).io.outpCarry
//
//  //        myMulArr(0).io.inpX := rInpXLo
//  //        myMulArr(0).io.inpY := rInpYLo
//  //        rY0X0 := myMulArr(0).io.outpProd
//
//  //        // This is done even when we're producing a 32-bit result despite
//  //        // the fact that it may be worse for power, for the sake of
//  //        // higher fmax. Perhaps that should be a generic for this module?
//  //        myMulArr(1).io.inpX := rInpXHi
//  //        myMulArr(1).io.inpY := rInpYHi
//  //        rY1X1 := myMulArr(1).io.outpProd
//  //      }
//  //      is (State.MUL32_FINAL_PROD_ADC32LO_Y0X0) {
//  //        when (!rInpLongMul) {
//  //          rState := State.IDLE
//  //          rOutpValid := True
//  //        } otherwise {
//  //          rState := State.MUL64_ADC32HI_Y0X1_Y1X0
//  //        }
//  //        rOutpProdHi := 0x0
//  //        myAdcArr(1).io.inpX := rY0X0
//  //        myAdcArr(1).io.inpY := rY0X1PlusY1X0Lsl16Lo
//  //        myAdcArr(1).io.inpCarry := False
//  //        rOutpProdLo := myAdcArr(1).io.outpSum
//  //        rTempCarryZ0 := myAdcArr(1).io.outpCarry
//  //      }
//  //      is (State.MUL64_ADC32HI_Y0X1_Y1X0) {
//  //        rState := State.MUL64_LUMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0
//
//  //        myAdcArr(2).io.inpX := Cat(U"16'd0", rY0X1Hi).asUInt
//  //        myAdcArr(2).io.inpY := Cat(U"16'd0", rY1X0Hi).asUInt
//  //        myAdcArr(2).io.inpCarry := rTempCarryZ1
//  //        rOutpProdHi := myAdcArr(2).io.outpSum
//  //      }
//  //      //is (
//  //      //  State.MUL64_ADC32HI_Y0X0
//  //      //) {
//  //      //  rState := State.MUL64_LUMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0
//
//  //      //}
//  //      is (State.MUL64_LUMUL_FINAL_PROD_ADC32HI_Y1X1_Y0X0) {
//  //        when (!(
//  //          rInpSigned 
//  //          && rOutpWillBeNegative
//  //        )) {
//  //          rState := State.IDLE
//  //          rOutpValid := True
//  //        } otherwise {
//  //          rState := State.MUL64_LSMUL_NEGATE_ADC32LO
//  //          rOutpProdLo := ~rOutpProdLo
//  //        }
//
//  //        myAdcArr(3).io.inpX := rOutpProdHi
//  //        myAdcArr(3).io.inpY := rY1X1 //rY0X1PlusY1X0Lsl16Hi
//  //        myAdcArr(3).io.inpCarry := rTempCarryZ0
//  //        rOutpProdHi := myAdcArr(3).io.outpSum
//  //      }
//  //      is (State.MUL64_LSMUL_NEGATE_ADC32LO) {
//  //        rState := State.MUL64_LSMUL_FINAL_PROD_NEGATE_ADC32HI
//  //        rOutpProdHi := ~rOutpProdHi
//  //        myAdcArr(4).io.inpX := rOutpProdLo
//  //        myAdcArr(4).io.inpY := 0x0
//  //        myAdcArr(4).io.inpCarry := True // need to add one
//  //        rOutpProdLo := myAdcArr(4).io.outpSum
//  //        rTempCarryZ2Negate := myAdcArr(4).io.outpCarry
//  //      }
//  //      is (State.MUL64_LSMUL_FINAL_PROD_NEGATE_ADC32HI) {
//  //        rState := State.IDLE
//  //        rOutpValid := True
//  //        myAdcArr(5).io.inpX := rOutpProdHi
//  //        myAdcArr(5).io.inpY := 0x0
//  //        myAdcArr(5).io.inpCarry := rTempCarryZ2Negate
//  //        rOutpProdHi := myAdcArr(5).io.outpSum
//  //      }
//  //    }
//  //  }
//  //}
//}
