#!/usr/bin/env python3

import math

from libcheesevoyage.general.pipeline_mods import (
	PipeSkidBufBus, PipeSkidBuf,
)
from libcheesevoyage.general.general_types import (
	PortDir, Modport, IntfShape, Splitintf, IntfarrShape, Splitintfarr,
)
from cpu_main_types import *

#def CACHE_LINE_NBYTES():
#	return 64
#def CACHE_LINE_WIDTH():
#	# 64 * 8 = 512
#	return CACHE_LINE_NBYTES() * 8
#def CACHE_MEM_DATA_WIDTH():
#	return 16
#def CACHE_TAG_WIDTH():
#	# 32 - 6 = 26
#	return MAIN_WIDTH() - math.ceil(math.log2(CACHE_LINE_NBYTES()))
#def CACHE_TAG_LSHIFT():
#	# 32 - 26 = 6
#	return MAIN_WIDTH() - CACHE_TAG_WIDTH()
##def CACHE_TAG
#
##def DEF_ICACHE_NBYTES():
##	return 4 * 1024
##def DEF_DCACHE_NBYTES():
##	return 4 * 1024
## cache size powers of 2
#def DEF_ICACHE_SZ_PWR():
#	# 4096 bytes
#	return 12
#def DEF_DCACHE_SZ_PWR():
#	# 4096 bytes
#	return 12
def CACHE_LINE_NBYTES():
	return 64
def CACHE_LINE_WIDTH():
	# 64 * 8 = 512
	return CACHE_LINE_NBYTES() * 8
def CACHE_LINE_BASE_ADDR_BITPOS():
	return math.ceil(math.log2(CACHE_LINE_NBYTES()))
#def CACHE_MEM_DATA_WIDTH():
#	return 16
def CACHE_TAG_BITPOS(depth_pwr: int):
	## 32 - 26 = 6
	#return MAIN_WIDTH() - CACHE_TAG_WIDTH()
	#return depth_pwr
	#return MAIN_WIDTH() - CACHE_TAG_WIDTH(depth_pwr)
	return depth_pwr + CACHE_LINE_BASE_ADDR_BITPOS()
def CACHE_TAG_WIDTH(depth_pwr: int):
	## 32 - 6 = 26
	#return MAIN_WIDTH() - CACHE_LINE_BASE_ADDR_BITPOS()
	return MAIN_WIDTH() - CACHE_TAG_BITPOS(depth_pwr)
def DEF_ICACHE_DEPTH_PWR():
	# 4096 bytes
	return 12 - CACHE_LINE_BASE_ADDR_BITPOS()
def DEF_DCACHE_DEPTH_PWR():
	# 4096 bytes
	# 2 ** 12 == 4096
	return 12 - CACHE_LINE_BASE_ADDR_BITPOS()
def DEF_DIV32_CHUNK_WIDTH():
	return 2

# IRQs
class FromIrqLayt(dict):
	def __init__(self):
		shape = {}
		shape["dummy"] = 1
		super().__init__(shape)

#class Flare32CpuOutpIrqLayt(dict):
#	def __init__(self):
#		shape = {}
#		shape["gnt"] = 1
#		#shape["data"] = 1
#		super().__init__(shape)
## Instruction reads
#class Flare32CpuInpInsnLayt(dict):
#	def __init__(self):
#		shape = {}
#		shape["data"] = (
#			# For a first attempt, just read 16 bits at a time.
#			# This might end up being faster overall anyway!
#			INSN_MAIN_WIDTH()
#
#			## Attempt to read the maximum possible size of a full
#			## instruction from icache
#			#INSN_MAX_WIDTH()
#		)
#		super().__init__(shape)
#class Flare32CpuOutpInsnLayt(dict):
#	def __init__(self):
#		shape = {}
#		##shape["acc_sz"] = BusAccSz.as_shape()
#		shape["addr"] = MAIN_WIDTH()
#		shape["icreload"] = 1
#		##shape["full_icreload"] = 1
#		super().__init__(shape)
## Data reads/writes
#class Flare32CpuInpDataLayt(dict):
#	def __init__(self):
#		shape = {}
#		shape["rd_data"] = MAIN_WIDTH()
#		super().__init__(shape)
#class Flare32CpuOutpDataLayt(dict):
#	def __init__(self):
#		shape = {}
#		#shape["en"] = 1
#		#shape["valid"] = 1
#
#		# Whether we're doing a read (0b0) or a write (0b1)
#		shape["wr_en"] = 1
#		# The size of the data we're wanting to read or write
#		shape["acc_sz"] = BusAccSz.as_shape()
#		# The address we want to communicate with
#		shape["addr"] = MAIN_WIDTH()
#		#shape["wr_data"] = MAIN_WIDTH_PAIR()
#		# In `Flare32Cpu`, we'll have to ensure that data being
#		# read from/written to data cache is 32-bit at the max
#		shape["wr_data"] = MAIN_WIDTH()
#		super().__init__(shape)

def MEM_ACC_SZ_WIDTH():
	return 2
# "Acc" is short for "Access"
class MemAccSz(enum.Enum, shape=MEM_ACC_SZ_WIDTH()):
#class MemAccSz(enum.Enum):
	SZ8 = 0b00
	SZ16 = 0b01
	SZ32 = 0b10
	#SZ64 = 0b11

#def MEM_ACC_KIND_WIDTH():
#	return 2
## "Acc" is short for "Access"
#class MemAccKind(enum.Enum, shape=MEM_ACC_KIND_WIDTH()):
#	Neither = 0x0
#	Read = enum.auto()
#	Write = enum.auto()

class CpuFromMemCtrlDataLayt(dict):
	def __init__(self):
		shape = {}
		#shape["ready"] = 1
		#shape["rd_data"] = CACHE_MEM_DATA_WIDTH()
		shape["rd_data"] = MAIN_WIDTH()
		super().__init__(shape)
class CpuToMemCtrlDataLayt(dict):
	def __init__(
		self,
		*,
		OPT_RD_ONLY: bool=False,
	):
		shape = {}
		# Whether we're doing a read (0b0) or a write (0b1)
		if not OPT_RD_ONLY:
			shape["wr_en"] = 1
		# The size of the data we're wanting to read or write
		shape["acc_sz"] = MemAccSz.as_shape()
		#shape["acc_kind"] = MemAccKind.as_shape()
		# The address we want to communicate with
		shape["addr"] = MAIN_WIDTH()
		#shape["wr_data"] = MAIN_WIDTH_PAIR()
		# In `Flare32Cpu`, we'll have to ensure that data being
		# read from/written to data cache is 32-bit at the max
		if not OPT_RD_ONLY:
			shape["wr_data"] = MAIN_WIDTH()
		super().__init__(shape)
def CPU_MEM_CTRL_ISHAPE_DEF_TAG_DCT():
	return {
		"from": "to_cpu_from_mctrl_tag",
		"to": "from_cpu_to_mctrl_tag",
		#"misc": "misc_mctrl_tag",
	}
class CpuMemCtrlIshape(IntfShape):
	def __init__(
		self,
		*,
		in_from: bool, # in cpu (True), or in mctrl (False)
		tag_dct=CPU_MEM_CTRL_ISHAPE_DEF_TAG_DCT(),
	):
		shape = CpuMemCtrlIshape.mk_fromto_shape(
			in_from=in_from,
			tag_dct=tag_dct,
		)["shape"]
		super().__init__(shape)
	@staticmethod
	def mk_fromto_shape(
		*,
		in_from: bool,
		tag_dct=CPU_MEM_CTRL_ISHAPE_DEF_TAG_DCT(),
	):
		return PipeSkidBufIshape.mk_fromto_shape(
			FromDataLayt=CpuFromMemCtrlDataLayt(),
			ToDataLayt=CpuToMemCtrlDataLayt(),
			in_from=in_from,
			name_dct={
				"from": "from_mctrl",
				"to": "to_mctrl",
			},
			tag_dct=tag_dct
		)
	@staticmethod
	def def_tag_dct():
		#return {
		#	"from": "to_cpu_from_mctrl_tag",
		#	"to": "from_cpu_to_mctrl_tag",
		#	#"misc": "misc_mctrl_tag",
		#},
		return CPU_MEM_CTRL_ISHAPE_DEF_TAG_DCT()
#def mk_bus_irq(
#	ObjKind,
#	rev: bool=False,
#) -> bool:
#	ret = Blank()
#	ret.inp = ObjKind(
#		data_info=SigInfo(
#			basenm="from_irq",
#			shape=FromIrqLayt(),
#		),
#		OPT_INCLUDE_VALID_BUSY=False,
#		OPT_INCLUDE_READY_BUSY=True,
#	)
#	if rev:
#		rev_ret = Blank()
#		rev_ret.outp = ret.inp
#		return rev_ret
#	else:
#		return ret
#def mk_bus_mem(
#	ObjKind,
#	rev: bool=False,
#) -> Blank:
#	ret = Blank()
#	ret.inp = ObjKind(
#		data_info=SigInfo(
#			basenm="from_mem",
#			shape=FromMemLayt()
#		),
#		OPT_INCLUDE_VALID_BUSY=False,
#		OPT_INCLUDE_READY_BUSY=True,
#	)
#	ret.outp = ObjKind(
#		data_info=SigInfo(
#			basenm="to_mem",
#			shape=ToMemLayt()
#		),
#		OPT_INCLUDE_VALID_BUSY=True,
#		OPT_INCLUDE_READY_BUSY=False,
#	)
#	if rev:
#		rev_ret = Blank()
#		rev_ret.inp = ret.outp
#		rev_ret.outp = ret.inp
#		return rev_ret
#	else:
#		return ret

#class Flare32CpuIrqInpLayt(dict):
#	def __init__(self):
#		shape = {
#			
#		}
#		super().__init__(shape)
#class Flare32CpuIrqIshape(IntfShape):
#	def __init__(
#		self,
#	):
#		from_cpu_shape = {
#			"ready": 1,
#		}
#		to_cpu_shape = {
#		}
#		shape = {
#			from_name="from_irq", to_name="to_irq",
#			from_shape=
#		}

#class Flare32CpuIrqIshape(IntfShape):
#	def __init__(
#		self,
#		*,
#		in_from: bool,
#		#from_cpu_tag=None,
#		#to_cpu_tag=None,
#		tag_dct={
#			"from": None,
#			"to": None,
#		},
#	):
#		from_cpu_name = "from_cpu"
#		to_cpu_name = "to_cpu"
#		from_cpu_shape = {
#			"ready": 1
#		}
#		to_cpu_shape = {
#			"valid": 1
#		}
#		shape = IntfShape.mk_fromto_shape(
#			name_dct={
#				"from": from_cpu_name,
#				"to": to_cpu_name,
#			},
#			shape_dct={
#				"from": from_cpu_shape,
#				"to": to_cpu_shape,
#			},
#			in_from=in_from,
#			#tag_dct={
#			#	"from": from_cpu_tag,
#			#	"to": to_cpu_tag,
#			#},
#			tag_dct=tag_dct,
#			mk_modport_dct={
#				"from": True,
#				"to": True,
#			},
#		)
#		super().__init__(shape)
#class Flare32CpuMemIshape(IntfShape):
#	def __init__(
#		self,
#		constants,
#		*,
#		in_from: bool,
#		from_cpu_tag=None,
#		to_cpu_tag=None,
#	):
#		from_cpu_name = "from_cpu"
#		to_cpu_name = "to_cpu"
#class MemIshape(IntfShape):
#	def __init__(
#		self,
#		constants,
#		*,
#		in_from: bool,
#		tag_dct={
#			"from": None, # "from_cpu_to_mem_tag",
#			"to": None, # "to_cpu_from_mem_tag",
#		},
#	):
#		shape 

class Flare32CpuIshape(IntfShape):
	def __init__(
		self,
		constants,
		*,
		in_from: bool,
		mem_tag_dct={
		},
	):
		shape = {
			"irq": IrqIshape(
				in_from=in_from
			),
			"mctrl": MemCtrlIshape(
				constants,
				in_from=in_from
			),
		}
		super().__init__(shape)
class Flare32CpuBus:
	def __init__(self, constants):
		self.__constants = constants
		#--------
		# IRQs
		#self.irq = mk_bus_irq(PipeSkidBufBus)
		#--------
		# Memory access
		#self.mem = mk_bus_mem(PipeSkidBufBus)
		#--------
		shape = Flare32CpuIshape(
			constants=constants,
			in_from=in_from
		)
	def constants(self):
		return self.__constants

class Flare32CpuConstants:
	def __init__(
		self,
		#PS_BUF_LEN
		#ICACHE_NBYTES=DEF_ICACHE_NBYTES(),
		#DCACHE_NBYTES=DEF_DCACHE_NBYTES(),
		ICACHE_DEPTH_PWR=DEF_ICACHE_DEPTH_PWR(),
		DCACHE_DEPTH_PWR=DEF_DCACHE_DEPTH_PWR(),
		DIV32_CHUNK_WIDTH=DEF_DIV32_CHUNK_WIDTH(),
		*,
		FORMAL: bool=False
	):
		#self.__PS_BUF_LEN = PS_BUF_LEN
		self.__ICACHE_DEPTH_PWR = ICACHE_DEPTH_PWR
		self.__DCACHE_DEPTH_PWR = DCACHE_DEPTH_PWR
		self.__DIV32_CHUNK_WIDTH = DIV32_CHUNK_WIDTH
		self.__FORMAL = FORMAL

	#def PS_BUF_LEN(self)
	#	return self.__PS_BUF_LEN
	def ICACHE_DEPTH_PWR(self):
		return self.__ICACHE_DEPTH_PWR
	def ICACHE_NBYTES(self):
		return (1 << self.ICACHE_DEPTH_PWR())
	def ICACHE_DEPTH(self):
		return self.ICACHE_NBYTES() // CACHE_LINE_NBYTES()
	def ICACHE_DEPTH_PWR_INSN(self):
		return self.ICACHE_DEPTH_PWR() // INSN_MAIN_NBYTES()
	def DCACHE_DEPTH_PWR(self):
		return self.__DCACHE_DEPTH_PWR
	def DCACHE_NBYTES(self):
		return (1 << self.DCACHE_DEPTH_PWR())
	def DCACHE_DEPTH(self):
		return self.DCACHE_NBYTES() // CACHE_LINE_NBYTES()
	def DIV32_CHUNK_WIDTH(self):
		return self.__DIV32_CHUNK_WIDTH
	def FORMAL(self):
		return self.__FORMAL

