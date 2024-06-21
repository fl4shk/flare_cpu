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

//case class Flare32CpuWrbackIo(
//  params: Flare32CpuParams,
//) extends Area {
//  val currPayload = Payload(Flare32CpuPipePayload(params=params))
//}

case class Flare32CpuWrback(
  params: Flare32CpuParams,
  prevPayload: Payload[Flare32CpuPipePayload],
  cPrevCurr: CtrlLink,
  decodeIo: Flare32CpuDecodeIo,
) extends Area {
  //--------
  //val io = Flare32CpuWrbackIo(params=params)
  //--------
  //when (cExWb.down.isFiring)
  val cPrevCurrArea = new cPrevCurr.Area {
    when (up.isValid) {
      def wrGpr = (
        //cExWb.down(psExOutp).get(isGpr=true)
        up(prevPayload).exec.get(isGpr=true)
      )
      def wrSpr = (
        //cExWb.down(psExOutp).get(isGpr=false)
        up(prevPayload).exec.get(isGpr=false)
      )
      when (wrGpr.wrReg.fire) {
        decodeIo.rGprVec(wrGpr.regIdx) := wrGpr.wrReg.payload
      }
      when (wrSpr.wrReg.fire) {
        decodeIo.rSprVec(wrSpr.regIdx) := wrSpr.wrReg.payload
      }
    }
  }
  //--------
}
