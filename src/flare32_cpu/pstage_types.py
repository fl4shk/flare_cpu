#!/usr/bin/env python3

from amaranth import *
from amaranth.lib.data import *
from amaranth.lib import enum
from amaranth.asserts import Assert, Assume, Cover
from amaranth.asserts import Past, Rose, Fell, Stable

from amaranth.sim import Simulator, Delay, Tick

from libcheesevoyage.misc_util import *
from libcheesevoyage.general.container_types import *

from cpu_main_types import *
from cpu_bus_types import *

class TempRegLayt(dict):
	def __init__(self, index_reset=0x0):
		super().__init__({
			"index": FieldInfo(
				INSN_REG_WIDTH(), reset=index_reset
			),
			"val": MAIN_WIDTH(),
			#"have": 1,
			"do_wb": 1,
		})
class TempRegGrpLayt(dict):
	def __init__(self):
		super().__init__({
			#"r0": TempRegLayt(index_reset=Gpr.R0),
			#"r1": TempRegLayt(index_reset=Gpr.R1)
			"ra": TempRegLayt(),
			"rb": TempRegLayt(),
			"rc": TempRegLayt(),
			"rd": TempRegLayt(),
			"index_reg": TempRegLayt(),
		})

class ExOutpKind(enum.Enum):
	# WB acts on data from EX in the following case:
	DO_WB = 0x0

	# IF/IC act on data from EX in the following cases:
	#TAKE_IRQ = enum.auto()
	#TAKE_BRANCH = enum.auto()
	#TAKE_RETI = enum.auto()
	SET_PC = enum.auto()
	ICRELOAD = enum.auto()
class SbDataLayt(dict):
	def __init__(self):
		super().__init__({
			"regs": TempRegGrpLayt(),

			# For data going from EX to IF, this is the new PC value,
			# but for other `PipeSkidBuf`s, this is the value of the PC
			# that goes with the instruction at the current pipeline
			# stage
			"pc": MAIN_WIDTH(),

			# The full value of an immediate, including the extension
			# from a `pre`/`lpre` insm 
			"imm": MAIN_WIDTH(),

			# The calculated memory address
			# `IF` uses this for `icreload`
			"mem_addr": MAIN_WIDTH(),

			# The raw value of an instruction
			"raw_insn": INSN_MAIN_WIDTH(),

			"insn_grp": INSN_GRP_WIDTH(),
			"insn_subgrp": MAIN_WIDTH(),
			"insn_op": MAIN_WIDTH(),

			"ex_outp_kind": FieldInfo(
				ExOutpKind.as_shape(), reset=ExOutpKind.DO_WB
			)
		})
def sb_data_info(basenm: str="sb_data_info") -> SigInfo:
	return SigInfo(
		basenm=basenm,
		shape=SbDataLayt(),
	)
