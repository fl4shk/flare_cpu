#!/usr/bin/env python3

import math

#from enum import Enum, auto
import enum as pyenum

from amaranth import *
from amaranth.lib.data import *
from amaranth.lib import enum
from amaranth.asserts import Assert, Assume, Cover
from amaranth.asserts import Past, Rose, Fell, Stable

from amaranth.sim import Simulator, Delay, Tick


from libcheesevoyage.misc_util import *
from libcheesevoyage.general.container_types import *
#from libcheesevoyage.general.fifo_mods import *
#from libcheesevoyage.general.pipelining_types import PsDir
from libcheesevoyage.general.pipeline_mods import (
	PipeSkidBuf, PipeSkidBufBus,
)
from pstage_types import *
#(
#	TempRegLayt, TempRegGrpLayt, ExOutpKind, SbDataLayt, sb_data_info,
#)

#from reg_file_mod import (
#	RegFileBus
#)
def mk_pstage_if_sbmn(ObjKind):
	return ObjKind(
		data_info=sb_data_info("sbmn_bus"),
		OPT_INCLUDE_VALID_BUSY=True,
		OPT_INCLUDE_READY_BUSY=False,
	)
def mk_pstage_if_sb_fr_ex(ObjKind):
	return ObjKind(
		data_info=sb_data_info("sb_fr_ex_bus"),
		OPT_INCLUDE_VALID_BUSY=False,
		OPT_INCLUDE_READY_BUSY=False,
	)
#def mk_pstage_if_icache(ObjKind):
#	return ObjKind(
#		data_info=sb_data_info
#	)
class PstageIfBus:
	def __init__(self, constants):
		self.__constants = constants

		self.sbmn_bus = mk_pstage_if_sbmn(PipeSkidBufBus)
		self.sb_fr_ex_bus = mk_pstage_if_sb_fr_ex(PipeSkidBufBus)

	def constants(self):
		return self.__constants

# Pipeline Stage: IF
class PstageIf(Elaboratable):
	def __init__(self, constants):
		self.__constants = constants
		self.__bus = PstageIfBus(constants=self.constants())

	def bus(self):
		return self.__bus
	def constants(self):
		return self.__constants

	def elaborate(self, platform: str) -> Module:
		#--------
		m = Module
		#--------
		bus = self.bus()
		loc = Blank()

		loc.m = Blank()
		#--------
		# to icache
		loc.m.sbmn = mk_pstage_if_sbmn(PipeSkidBuf)
		# from EX
		loc.m.sb_fr_ex = mk_pstage_if_sb_fr_ex(PipeSkidBuf)
		m.submodules.sbmn = loc.m.sbmn
		m.submodules.sb_fr_ex = loc.m.sb_fr_ex

		loc.sbmn_tmp = {
			key: cast_shape(
				SbDataLayt(),
				name=psconcat("loc_sbmn_tmp_", key),
			)
			for key in ["from_child", "to_out"]
		}
		PipeSkidBuf.connect_child(
			parent=m,
			parent_sb_bus=bus.sbmn_bus,
			child_sb_bus=loc.m.sbmn.bus(),
			parent_data=loc.sbmn_tmp
		)
		#loc.sb_fr_ex_tmp = {
		#	key: cast_shape(
		#		SbDataLayt(),
		#		name=psconcat("loc_sbmn_tmp_", key),
		#	)
		#	for key in ["from_child", "to_out"]
		#}
		PipeSkidBuf.connect_child(
			parent=m,
			parent_sb_bus=bus.sb_fr_ex_bus,
			child_sb_bus=loc.m.sb_fr_ex.bus(),
			#parent_data=loc.sb_fr_ex_tmp
		)
		#--------
		class StateIf(enum.Enum):
			FETCH = 0x0
			SET_PC = enum.auto()
			ICRELOAD = enum.auto()
		# `loc.state` is set with combinational logic
		loc.state = cast_shape(
			StateIf.as_shape(),
			name="loc_state",
			reset=StateIf.FETCH,
		)

		# Data from EX
		fr_ex_o_fwd = bus.sb_fr_ex_bus.outp.fwd

		# Data going to EX
		o_fwd_data = Blank()
		o_fwd_data.curr = loc.sbmn_tmp["from_child"]
		o_fwd_data.next = loc.sbmn_tmp["to_out"]
		next_pc_mn = o_fwd_data.curr.pc + INSN_MAIN_NBYTES()
		next_pc_fr_ex = fr_ex_o_fwd.data.pc + INSN_MAIN_NBYTES()

		with m.If(fr_ex_o_fwd.valid):
			with m.Switch(fr_ex_o_fwd.data.ex_outp_kind):
				with m.Case(ExOutpKind.DO_WB):
					m.d.comb += loc.state.eq(StateIf.FETCH)
				with m.Case(ExOutpKind.SET_PC):
					m.d.comb += loc.state.eq(StateIf.SET_PC)
				with m.Case(ExOutpKind.ICRELOAD):
					m.d.comb += loc.state.eq(StateIf.ICRELOAD)
				with m.Default():
					m.d.comb += loc.state.eq(StateIf.FETCH)
		with m.Else(): # If(~fr_ex_o_fwd.valid):
			m.d.comb += loc.state.eq(StateIf.FETCH)

		with m.If(loc.state == StateIf.SET_PC):
			m.d.sync += [
				#o_fwd.data.pc.eq(fr_ex_o_fwd.data.pc + INSN_MAIN_NBYTES()),
				o_fwd_data.next.pc.eq(next_pc_fr_ex),
			]
		with m.Else():
			m.d.sync += [
				#o_fwd.data.pc.eq(o_fwd.data.pc + INSN_MAIN_NBYTES()),
				o_fwd_data.next.pc.eq(next_pc_mn),
			]

		#with m.Switch(loc.state):
		#	with m.Case(StateIf.FETCH):
		#		#m.d.comb += [
		#		#]
		#		m.d.sync += [
		#			#bus.insn.outp.addr.eq(o_fwd.data.pc),
		#			o_fwd.data.pc.eq(o_fwd.data.pc + INSN_MAIN_NBYTES()),
		#		]
		#	with m.Case(StateIf.SET_PC):
		#		#m.d.comb += [
		#		#]
		#		m.d.sync += [
		#			o_fwd.data.pc.eq(
		#				fr_ex_o_fwd.data.pc + INSN_MAIN_NBYTES()
		#			),
		#		]
		#	with m.Case(StateIf.ICRELOAD):
		#		#m.d.comb += [
		#		#]
		#		m.d.sync += [
		#			o_fwd.data.pc.eq(o_fwd.data.pc + INSN_MAIN_NBYTES()),
		#		]
		#		#m.d.sync += [
		#		#	o_fwd.data.pc.eq(o_fwd
		#		#]
		#	#with m.Default():
		#	#	m.d.sync += [
		#	#	]
		#m.d.comb += [
		#	sbmn_if.bus().inp.valid_busy.eq(0b0),
		#]
		return m
		#--------
