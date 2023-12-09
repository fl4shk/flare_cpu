#!/usr/bin/env python3

import math

#from amaranth import *
#from amaranth.lib.data import *
#from amaranth.lib import enum
#from amaranth.asserts import Assert, Assume, Cover
#from amaranth.asserts import Past, Rose, Fell, Stable
#
#from amaranth.sim import Simulator, Delay, Tick

#from cache_mods import (
#	Icache, Dcache,
#)

from bus_arbiter_mod import (
	BusArbiterBus BusArbiter
)

# Pipeline Stage IF: Instruction Fetch
from pstage_if_mod import (
	PstageIfBus, PstageIf,
)
# Pipeline Stage ID: Instruction Decode
from pstage_id_mod import (
	PstageIdBus, PstageId,
)
# Pipeline Stage IC: Instruction Cache (waiting for it, that is)
#from pstage_ic_mod import (
#	PstageIcBus, PstageIc,
#)
# Pipeline Stage EX: EXecute (and decode the current instruction)
from pstage_ex_mod import (
	PstageExBus, PstageEx,
)
# Pipeline Stage WB: Write Back
from pstage_wb_mod import (
	PstageWbBus, PstageWb,
)

from libcheesevoyage.math_lcv.long_div_mods import *
#--------
class Flare32Cpu(Elaboratable):
	#--------
	def __init__(
		self,
		ICACHE_DEPTH_PWR=DEF_ICACHE_DEPTH_PWR(),
		DCACHE_DEPTH_PWR=DEF_DCACHE_DEPTH_PWR(),
		DIV32_CHUNK_WIDTH=DEF_DIV32_CHUNK_WIDTH(),
		*,
		FORMAL: bool=False
	):
		self.__constants = Constants(
			ICACHE_DEPTH_PWR=ICACHE_DEPTH_PWR,
			DCACHE_DEPTH_PWR=DCACHE_DEPTH_PWR,
			DIV32_CHUNK_WIDTH=DIV32_CHUNK_WIDTH,
			FORMAL=FORMAL
		)
		self.__bus = Flare32CpuBus(self.__constants)
	#--------
	def bus(self):
		return self.__bus
	def constants(self):
		return self.__constants
	#--------
	def elaborate(self, platform: str) -> Module:
		#--------
		m = Module()
		#--------
		bus = self.bus()
		constants = self.constants()

		loc = Blank()
		loc.m = Blank()
		#loc.m.gpr_file = m.submodules.gpr_file = RegFile(NUM_GPRS())
		#loc.m.spr_file = m.submodules.spr_file = RegFile(NUM_SPRS())

		#gprf_bus = loc.m.gpr_file.bus()
		## Whether or not to do write back
		#def should_do_wb(key):
		#	return (
		#		key == "ra"
		#		or key == "rb"
		#		or key == "rc"
		#		or key == "rd"
		#		#key != "index"
		#	)


		# Four pipeline stages, interlocked
		#loc.ps = {
		#	"if": Blank(),
		#	"id": Blank(),
		#	"ex": Blank(),
		#	#"mem": Blank(),
		#	"wb": Blank(),
		#}

		#sb_main_dct = loc.m.sb_main_dct = {}
		## `dict`s are now ordered
		#for key, value in loc.ps.items():
		#	#value.valid_busy = Signal(name=psconcat(
		#	#	"loc_ps_", key, "_valid_busy"
		#	#))
		#	#value.ready_busy = Signal(name=psconcat(
		#	#	"loc_ps_", key, "_ready_busy"
		#	#))
		#	#value.clear = Signal(name=psconcat(
		#	#	"loc_ps_", key, "_clear"
		#	#))
		#	sb_main_dct[key] = PipeSkidBuf(
		#		data_info=loc.sb_data_info,
		#		#OPT_INCLUDE_VALID_BUSY=True,
		#		#OPT_INCLUDE_READY_BUSY=True,
		#		OPT_INCLUDE_VALID_BUSY=(
		#			key == "if"
		#			or key == "id"
		#			or key == "ex"
		#		),
		#		#OPT_INCLUDE_READY_BUSY=(key == "ex"),
		#		OPT_INCLUDE_READY_BUSY=False,
		#	)
		#	value.sbmn = sb_main_dct[key]
		#	value.sbmn_bus = value.sbmn.bus()
		#m.submodules += sb_main_dct.values()

		# Connect the main `PipeSkidBuf` s
		#PipeSkidBuf.connect(
		#	parent=m,
		#	sb_bus_lst=[
		#		value.bus() for value in sb_main_dct.values()
		#	],
		#	tie_first_inp_fwd_valid=True,
		#	tie_last_inp_bak_ready=True,
		#)
		#ps_if = loc.ps["if"]
		#sbmn_if = ps_if.sbmn
		#sbmn_bus_if = ps_if.sbmn_bus

		#ps_id = loc.ps["id"]
		#sbmn_id = ps_id.sbmn
		#sbmn_bus_id = ps_id.sbmn_bus

		#ps_ex = loc.ps["ex"]
		#sbmn_ex = ps_ex.sbmn
		#sbmn_bus_ex = ps_ex.sbmn_bus

		#ps_wb = loc.ps["wb"]
		#sbmn_wb = ps_wb.sbmn
		#sbmn_bus_wb = ps_wb.sbmn_bus

		## Data going from IF to Insn Cache
		##ps_if.sb_to_ic = loc.m.sb_if_to_ic = PipeSkidBuf(
		##	data_info=loc.sb_data_info,
		##	OPT_INCLUDE_VALID_BUSY=False,
		##	OPT_INCLUDE_READY_BUSY=False,
		##)

		## Data going from EX to IF
		##ps_if.sb_from_ex = loc.m.sb_ex_to_if = PipeSkidBuf(
		##	data_info=loc.sb_data_info,
		##	OPT_INCLUDE_VALID_BUSY=True,
		##	OPT_INCLUDE_READY_BUSY=True,
		##	#OPT_INCLUDE_READY_BUSY=False,
		##)
		#ps_if.sb_from_ex_bus = ps_if.sb_from_ex.bus()
		#m.submodules += ps_if.sb_from_ex
		#PipeSkidBuf.connect(
		#	parent=m,
		#	sb_bus_lst=[ps_ex.sbmn_bus, ps_if.sb_from_ex_bus],
		#	tie_first_inp_fwd_valid=False,
		#	tie_last_inp_bak_ready=True,
		#)

		## Data going from EX to ID
		#ps_id.sb_from_ex = loc.m.sb_ex_to_id = PipeSkidBuf(
		#	data_info=loc.sb_data_info,
		#	OPT_INCLUDE_VALID_BUSY=True,
		#	OPT_INCLUDE_READY_BUSY=True,
		#	#OPT_INCLUDE_READY_BUSY=False,
		#)
		#ps_id.sb_from_ex_bus = ps_id.sb_from_ex.bus()
		#m.submodules += ps_id.sb_from_ex
		#PipeSkidBuf.connect(
		#	parent=m,
		#	sb_bus_lst=[ps_ex.sbmn_bus, ps_id.sb_from_ex_bus],
		#	tie_first_inp_fwd_valid=False,
		#	tie_last_inp_bak_ready=True,
		#)
		#--------
		# Pipeline Stage: IF
		#ps_if.temp_from_ex = Blank()

		#ps_if.temp_from_ex = cast_shape(
		#	{
		#		# comb
		#		"c": SigInfo.like(ps_if.sb_from_ex_bus.outp.fwd)

		#		# sync
		#		"s": SigInfo.like(ps_if.sb_from_ex_bus.outp.fwd)
		#	},
		#	name="loc_ps_if_temp_from_ex"
		#)
		#ps_if.ready_busy = cast_shape(
		#	{
		#		"c": 1, # comb
		#		"s": 1, # sync
		#	},
		#	name="loc_ps_if_ready_busy"
		#)
		#m.d.comb += [
		#	ps_if.sb_from_ex_bus.inp.ready_busy.eq(ps_if.ready_busy.c),
		#]
		#--------
		# Pipeline Stage: ID
		#m.d.comb += [
		#	sbmn_id.bus().inp.valid_busy.eq(0b0),
		#]
		#--------
		## Pipeline Stage: EX
		#class StateEx(enum.Enum):
		#	ONE_CYCLE = 0
		#	MUL32 = auto()
		#	LMUL = auto()
		#	DIV32 = auto()
		#	DIV64_BY_32 = auto()
		#ps_ex.state = Signal(
		#	StateEx.as_shape(),
		#	name="ps_ex_state",
		#	reset=StateEx.ONE_CYCLE,
		#)

		## Saved values for operand forwarding, and also the current value
		## (which is at index 0)
		#ps_ex.NUM_REG_GRPS = 3
		#ps_ex.reg_grp_arr = Splitarr(
		#	[
		#		TempRegGrpLayt()
		#		for i in range(ps_ex.NUM_REG_GRPS)
		#	],
		#	name="ps_ex_reg_grp_arr",
		#)
		#ps_ex.reg_grp_0_prev = Splitrec(
		#	TempRegGrpLayt(), name="ps_ex_reg_grp_0_prev",
		#)
		#def add_operand_fwd_muxes(ps_ex, key, i=0):
		#	reg_0 = getattr(ps_ex.reg_grp_arr[0], key)
		#	next_reg = getattr(ps_ex.reg_grp_arr[i + 1], key)
		#	inp_fwd_reg = getattr(ps_ex.sbmn_bus.inp.fwd.data.regs, key)
		#	return Mux(
		#		(reg_0.index == next_reg.index),
		#		next_reg,
		#		(
		#			add_operand_fwd_muxes(ps_ex, key, i + 1)
		#			if (i + 1 < ps_ex.NUM_REG_GRPS)
		#			else 
		#				# at this point, we know that there are no more
		#				# saved values left, so we grab the value from the
		#				# ID->EX `PipeSkidBufBus`
		#				inp_fwd_reg
		#		),
		#	)

		#for i in range(ps_ex.NUM_REG_GRPS - 1):
		#	# Save old values
		#	m.d.sync += ps_ex.reg_grp_arr[i + 1].eq(ps_ex.reg_grp_arr[i])
		#for key in ps_ex.reg_grp_arr[0].shape().keys():
		#	field = getattr(ps_ex.reg_grp_arr[0], key)
		#	field_prev = getattr(ps_ex.reg_grp_0_prev, key)
		#	#with m.If(ps_ex.sbmn_bus.inp.fwd.valid):
		#	with m.If(
		#		# NOTE: the `valid_busy` signal is handled by the
		#		# `PipeSkidBuf`, so we can just check the `valid` output of
		#		# EX's main `PipeSkidBuf`
		#		~ps_ex.sbmn_bus.outp.fwd.valid
		#	):
		#		m.d.comb += [
		#			field.eq(add_operand_fwd_muxes(ps_ex, key))
		#		]
		#		m.d.sync += [
		#			field_prev.eq(field),
		#		]
		#	with m.Else(): # If(ps_ex.sbmn_bus.outp.fwd.valid):
		#		m.d.comb += [
		#			field.eq(field_prev)
		#		]

		# Drive the `EX->WB` signals
		#m.d.comb += [
		#]

		#m.d.comb += [
		#	sbmn_ex.bus().inp.valid_busy.eq(
		#	),
		#]
		#--------
		## Pipeline Stage: MEM
		#--------
		# Pipeline Stage: WB
		#m.d.comb += [
		#	sbmn_wb.bus().inp.valid_busy.eq(
		#	),
		#]
		#--------

		#loc.sprf_bus = loc.m.spr_file.bus()

		## How signals in pipeline stages are connected to one another.
		#class PsConn(pyenum.Enum):
		#	Next2Prev = 0
		#	Prev2Next = pyenum.auto()

		#loc.ps_pair_dct = {
		#	"reg_pc": mk_ps_pair("reg_pc", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_flags": mk_ps_pair("spr_flags", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_ids": mk_ps_pair("spr_ids", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_ira": mk_ps_pair("spr_ira", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_ie": mk_ps_pair("spr_ie", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_ity": mk_ps_pair("spr_ity", MAIN_WIDTH(), PsConn.Fwd),
		#	"spr_sty": mk_ps_pair("spr_sty", MAIN_WIDTH(), PsConn.Fwd),
		#	# probably only need one `flush` signal
		#	#mk_ps_pair("flush", 1),
		#	"stb": mk_ps_pair("stb", 1, PsDir.Fwd),
		#	"busy": mk_ps_pair("busy", 1, PsDir.Bak),

		#	# Clock Enable
		#	#"ce": mk_ps_pair("ce", 1, PsDir.Fwd)
		#}
		return m
		#--------
	#--------
