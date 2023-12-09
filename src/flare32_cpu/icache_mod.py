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
from libcheesevoyage.general.pipeline_mods import (
	PipeSkidBuf, PipeSkidBufBus,
)
from libcheesevoyage.mem.mem_wrapper_mod import *

from cpu_main_types import *
from cpu_bus_types import *
from pstage_types import *
from bus_arbiter_mod import (
	BusArbiterIcacheIshape
)
#--------
#def mk_icache_sbmn(ObjKind):
#	return ObjKind(
#		data_info=sb_data_info("sbmn_bus"),
#		OPT_INCLUDE_VALID_BUSY=True,
#		OPT_INCLUDE_READY_BUSY=False,
#	)
#def mk_icache_sb_fr_ex(ObjKind):
#	return ObjKind(
#		data_info=sb_data_info("sb_fr_ex_bus"),
#		OPT_INCLUDE_VALID_BUSY=False,
#		OPT_INCLUDE_READY_BUSY=False,
#	)
class IcacheMetadataLayt(dict):
	def __init__(self, constants):
		shape = {}
		#shape["tag"] = constants.ICACHE_DEPTH_PWR_INSN()
		#shape["tag"] = MAIN_WIDTH()
		shape["tag"] = CACHE_TAG_WIDTH(constants.ICACHE_DEPTH_PWR())
		shape["is_fetched"] = 1
		super().__init__(shape)
	#@staticmethod
	#def TAG_WIDTH():
	#	pass
# The output from Icache to to pipeline stages (IF/ID)
class IcachePsOutpLayt(dict):
	def __init__(self, constants):
		shape = {}
		shape["data"] = CACHE_LINE_WIDTH()
		shape["metadt"] = IcacheMetadataLayt(constants)
		super().__init__(shape)
# The data from IF to Icache
class IcacheFromIfLayt(dict):
	def __init__(self):
		shape = {}
		shape["pc"] = MAIN_WIDTH()
		super().__init__(shape)

class IcacheIfIshape(IntfShape):
	def __init__(
		self,
		constants,
		*,
		in_from: bool,
		#from_if_tag=None,
		#to_if_tag=None,
		#tag_dct={
		#	"from": "from_icache_to_if_tag",
		#	"to": "to_icache_from_if_tag",
		#},
	):
		shape = IntfShape.mk_fromto_shape(
			name_dct={
				"from": "from_if",
				"to": "to_if",
			},
			shape_dct={
				"from": IcacheFromIfLayt(),
				"to": IcachePsOutpLayt(constants),
			},
			in_from=in_from,
			#tag_dct={
			#	"from": from_if_tag,
			#	"to": to_if_tag,
			#},
			tag_dct={
				"from": "from_icache_to_if_tag",
				"to": "to_icache_from_if_tag",
			},
			mk_modport_dct={
				"from": True,
				"to": True,
			},
		)
		super().__init__(shape)

#class IcacheFromIdLayt(dict):
#	def __init__(self):
#		shape = {}
#		super().__init__(shape)

class IcacheIdIshape(IntfShape):
	def __init__(
		self,
		constants,
		*,
		in_from: bool,
		#from_id_tag=None,
		#to_id_tag="from_icache_to_id_tag",
	):
		shape = IntfShape.mk_fromto_shape(
			name_dct={
				"from": None,
				"to": "to_id",
			},
			shape_dct={
				"from": None,
				"to": IcachePsOutpLayt(constants),
			},
			in_from=in_from,
			tag_dct={
				"from": None,
				"to": "from_icache_to_id_tag",
			},
			mk_modport_dct={
				"from": True,
				"to": True,
			}
		)
		super().__init__(shape)
#--------
class IcacheIshape(IntfShape):
	def __init__(
		self,
		constants,
		#*,
		#in_from: bool=True,
		#from_if_tag=None,
		#to_if_tag=None,
		##from_id_tag=None,
		#to_id_tag=None,
		#from_busarb_tag=None,
		#to_busarb_tag=None,
		#if_tag_dct={
		#	"from": "from_icache_to_if_tag",
		#	"to": "to_icache_from_if_tag",
		#},
		#id_tag_dct={
		#	"from": "from_icache_to_id_tag",
		#	"to": "to_icache_from_id_tag",
		#},
	):
		shape = {
			"if_bus": IcacheIfIshape(
				constants=constants,
				#in_from=in_from,
				in_from=True,
				#from_if_tag=from_if_tag,
				#to_if_tag=to_if_tag,
			),
			"id_bus": IcacheIdIshape(
				constants=constants,
				#in_from=in_from,
				in_from=True,
				#from_id_tag=from_id_tag,
				#to_id_tag=to_id_tag,
			),
			#"mem_bus": MemIshape(
			#	in_mem=False,
			#	from_mem_tag=from_mem_tag,
			#	to_mem_tag=to_mem_tag,
			#),
			"ba_bus": BusArbiterIcacheIshape(
				#constants=constants,
				in_from=False,
			)
		}
		super().__init__(shape)
class IcacheBus:
	def __init__(
		self,
		constants,
	):
		#--------
		self.__constants = constants
		#--------
		ishape = IcacheIshape(
			constants=constants,
		)
		self.__bus = Splitintf(ishape)
		#--------
	@property
	def bus(self):
		return self.__bus
	def constants(self):
		return self.__constants
#class IcacheRdDataCheck(Elaboratable):
#	def __init__(
#		self,
#		constants,
#	):
class Icache(Elaboratable):
	def __init__(
		self,
		constants,
		#depth: int,
		*,
		mem_attrs=None,
	):
		self.__bus = IcacheBus(constants=constants)
		self.__depth = self.bus().constants().ICACHE_DEPTH()
	def bus(self):
		return self.__bus
	def depth(self):
		return self.__depth
	@staticmethod
	def check_if_cached(
		m: Module,
		addr,
		ps_outp: IcachePsOutpLayt,
		addr_is_cached: Signal,
	):
		m.d.comb += addr_is_cached.eq(
			(addr == ps_outp.metadt.tag) & ps_outp.metadt.is_fetched
		)
	def elaborate(self, platform: str) -> Module:
		#--------
		m = Module()
		#--------
		bus = self.bus()
		if_bus = bus.bus.if_bus
		id_bus = bus.bus.id_bus
		mem_bus = bus.bus.mem_bus
		constants = bus.constants()
		loc = Blank()

		loc.m = Blank()
		#--------
		m.submodules.mwrap = loc.m.mwrap = MemWrapper(
			#width=CACHE_LINE_WIDTH()
			shape=IcachePsOutpLayt(constants),
			depth=self.depth(),
			init=[0x0 for i in range(self.depth())],
			#attrs={"ram_style": "block"},
			attrs=mem_attrs,
		)
		loc.mw_bus = loc.m.mwrap.bus().bus
		to_mw = loc.mw_bus.to_mwrap
		from_mw = loc.mw_bus.from_mwrap
		m.d.comb += [
			# Here, we have incompatible `IntfShape`s, so we're doing a
			# conversion.
			to_mw.rd_addr.eq(if_bus.from_if.pc),
			Cat(if_bus.to_if.flattened()).eq(
				Cat(from_mw.rd_data.flattened())
			),
			Cat(id_bus.to_id.flattened()).eq(
				Cat(from_mw.rd_data.flattened())
			),
		]
		#if_bus.to_if.connect(
		#	other=loc.mw_bus.
		#)
		class State(enum.Enum):
			# The main state, where we try to read from `loc.m.mwrap` and
			# if we don't detect 
			MAIN = 0x0
			RE_FETCH
		#--------
		return m
		#--------
