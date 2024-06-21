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

object Flare32CpuDcacheDataSize
extends SpinalEnum(defaultEncoding=binarySequential) {
  val
    SZ8,
    SZ16,
    SZ32
    = newElement();
}
case class Flare32CpuPipePayloadDcache(
  params: Flare32CpuParams,
) extends Bundle {
  val data = UInt(params.mainWidth bits)
  val size = Flare32CpuDcacheDataSize()
  val isWr = Bool()
}

case class Flare32CpuDcacheLineEntryPayload(
  params: Flare32CpuParams
) extends Bundle {
  //val valid = Bool()
  val dirty = Bool()
  val baseAddr = UInt(params.dcacheLineBaseAddrWidth bits)
  val data = params.dcacheLineMemWordType()
}
//case class Flare32CpuDcacheIoPushPayload(
//  params: Flare32CpuParams
//) extends Bundle {
//  //val 
//}
case class Flare32CpuDcacheIo(
  params: Flare32CpuParams,
) extends Area {
  //--------
  val dbus = /*master*/(/*Stream*/(Bmb(p=params.dbusParams)))
  //--------
}
case class Flare32CpuDcache(
  params: Flare32CpuParams,
) extends Area {
  //--------
  val io = Flare32CpuDcacheIo(params=params)
  //--------
  //val pipeMem = PipeMemRmw[
  //]
}
