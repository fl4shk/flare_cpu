#!/usr/bin/env python3

import math

import enum as pyenum

from amaranth import *
from amaranth.lib.data import *
from amaranth.lib import enum
from amaranth.asserts import Assert, Assume, Cover
from amaranth.asserts import Past, Rose, Fell, Stable

from libcheesevoyage.general.general_types import *
from libcheesevoyage.general.pipeline_mods import (
	PipeSkidBufIshape, PipeSkidBufBus, PipeSkidBuf,
)

from cpu_main_types import *
from cpu_bus_types import (
	CpuMemCtrlIshape
)

#from icache_mod import (
#	IcacheBus, Icache
#)
#from dcache_mod import (
#	DcacheBus, Dcache
#)
#--------
# From Icache
class BusArbiterFromIcDataLayt(CpuToMemCtrlDataLayt):
	def __init__(self):
		#shape = {}
		#shape["addr"] = MAIN_WIDTH()
		#super().__init__(shape)
		super().__init__(
			OPT_RD_ONLY=True,
		)
# From Dcache *or* Non-cached
class BusArbiterFromDcncDataLayt(CpuToMemCtrlDataLayt):
	def __init__(self):
		#super().__init__()
		super().__init__(
			OPT_RD_ONLY=False,
		)
# To Icache *or* Dcache *or* Non-cached
class BusArbiterToAnyDataLayt(CpuFromMemCtrlDataLayt):
	def __init__(self):
		super().__init__()

def BUS_ARB_IC_ISHAPE_DEF_TAG_DCT():
	return {
		"from": "from_busarb_to_ic_tag",
		"to": "to_busarb_from_ic_tag",
	}
class BusArbiterIcacheIshape(IntfShape):
	def __init__(
		self,
		*,
		in_from: bool,
		tag_dct=BUS_ARB_IC_ISHAPE_DEF_TAG_DCT(),
	):
		shape = PipeSkidBufIshape.mk_fromto_shape(
			FromDataLayt=BusArbiterFromIcDataLayt(),
			ToDataLayt=BusArbiterToAnyDataLayt(),
			in_from=in_from,
			name_dct={
				"from": "from_ic",
				"to": "to_ic",
			},
			tag_dct=tag_dct
		)["shape"]
		super().__init__(shape)
	@staticmethod
	def def_tag_dct():
		return BUS_ARB_IC_ISHAPE_DEF_TAG_DCT()

# Dcache *or* Non-cached
class _BusArbiterDcncBaseIshape(IntfShape):
	def __init__(
		self,
		*,
		in_from: bool,
		name_dct: dict,
		tag_dct: dict,
	):
		shape = PipeSkidBufIshape.mk_fromto_shape(
			FromDataLayt=BusArbiterFromDcncDataLayt(),
			ToDataLayt=BusArbiterToAnyDataLayt(),
			in_from=in_from,
			name_dct=name_dct,
			tag_dct=tag_dct
		)["shape"]
		super().__init__(shape)

def BUS_ARB_DC_ISHAPE_DEF_TAG_DCT():
	return {
		"from": "from_busarb_to_dc_tag",
		"to": "to_busarb_from_dc_tag",
	}
class BusArbiterDcacheIshape(_BusArbiterDcncBaseIshape):
	def __init__(
		self,
		*,
		in_from: bool,
		tag_dct=BUS_ARB_DC_ISHAPE_DEF_TAG_DCT(),
	):
		super().__init__(
			BusArbiterDcacheIshape.mk_fromto_shape(
				in_from=in_from,
				tag_dct=tag_dct,
			)["shape"]
		)
	@staticmethod
	def mk_fromto_shape(
		*
		in_from: bool,
		tag_dct=BUS_ARB_DC_ISHAPE_DEF_TAG_DCT(),
	):
		return _BusArbiterDcncBaseIshape.mk_fromto_shape(
			in_from=in_from,
			name_dct={
				"from": "from_dc",
				"to": "to_dc",
			},
			tag_dct=tag_dct,
		)
	@staticmethod
	def def_tag_dct():
		return BUS_ARB_DC_ISHAPE_DEF_TAG_DCT()

def BUS_ARB_NC_ISHAPE_DEF_TAG_DCT():
	return {
		"from": "from_busarb_to_nc_tag",
		"to": "to_busarb_from_nc_tag",
	}
class BusArbiterNoncachedIshape(_BusArbiterDcncBaseIshape):
	def __init__(
		self,
		*,
		in_from: bool,
		tag_dct=BUS_ARB_NC_ISHAPE_DEF_TAG_DCT(),
	):
		super().__init__(
			BusArbiterNoncachedIshape.mk_fromto_shape(
				in_from=in_from,
				tag_dct=tag_dct,
			)["shape"]
		)
	@staticmethod
	def mk_fromto_shape(
		*
		in_from: bool,
		tag_dct=BUS_ARB_NC_ISHAPE_DEF_TAG_DCT(),
	):
		return _BusArbiterDcncBaseIshape.mk_fromto_shape(
			in_from=in_from,
			name_dct={
				"from": "from_nc",
				"to": "to_nc",
			},
			tag_dct=tag_dct,
		)
	@staticmethod
	def def_tag_dct():
		return BUS_ARB_NC_ISHAPE_DEF_TAG_DCT()
class BusArbiterIshape(IntfShape):
	def __init__(
		self,
		*,
		#in_from: bool,
		#mctrl_tag_dct={
		#	"from": None,
		#	"to": None,
		#},
		ic_tag_dct=BusArbiterIcacheIshape.def_tag_dct(),
		dc_tag_dct=BusArbiterDcacheIshape.def_tag_dct(),
		nc_tag_dct=BusArbiterNoncachedIshape.def_tag_dct(),
		mctrl_tag_dct=CpuMemCtrlIshape.def_tag_dct(),
	):
		shape = {
			"ic_bus": BusArbiterIcacheIshape(
				in_from=False,
				tag_dct=ic_tag_dct,
			),
			"dc_bus": BusArbiterDcacheIshape(
				in_from=False,
				tag_dct=dc_tag_dct,
			),
			"nc_bus": BusArbiterNoncachedIshape(
				in_from=False,
				tag_dct=nc_tag_dct,
			),
			"mctrl_bus": CpuMemCtrlIshape(
				in_from=True,
				tag_dct=mctrl_tag_dct,
			)
		}
		super().__init__(shape)
class BusArbiterBus:
	def __init__(
		self,
		*,
		ic_tag_dct=BusArbiterIcacheIshape.def_tag_dct(),
		dc_tag_dct=BusArbiterDcacheIshape.def_tag_dct(),
		nc_tag_dct=BusArbiterNoncachedIshape.def_tag_dct(),
		mctrl_tag_dct=CpuMemCtrlIshape.def_tag_dct(),
	):
		#--------
		self.__constants = constants
		#--------
		ishape = BusArbiterIshape(
			ic_tag_dct=ic_tag_dct,
			dc_tag_dct=dc_tag_dct,
			nc_tag_dct=nc_tag_dct,
			mctrl_tag_dct=mctrl_tag_dct,
		)
		self.__bus = Splitintf(ishape, name="ba_bus")
		#--------
	@property
	def bus(self):
		return self.__bus
	def constants(self):
		return self.__constants
class BusArbiter(Elaboratable):
	def __init__(
		self,
		*,
		ic_tag_dct=BusArbiterIcacheIshape.def_tag_dct(),
		dc_tag_dct=BusArbiterDcacheIshape.def_tag_dct(),
		nc_tag_dct=BusArbiterNoncachedIshape.def_tag_dct(),
		mctrl_tag_dct=CpuMemCtrlIshape.def_tag_dct(),
	):
		self.__bus = BusArbiterBus(
			ic_tag_dct=ic_tag_dct,
			dc_tag_dct=dc_tag_dct,
			nc_tag_dct=nc_tag_dct,
			mctrl_tag_dct=mctrl_tag_dct,
		)
		self.__constants = constants
	def bus(self):
		return self.__bus
	def constants(self):
		return self.__constants
	def elaborate(self, platform: str) -> Module:
		#--------
		m = Module
		#--------
		constants = self.constants()
		bus = self.bus().bus
		ic_bus = bus.ic_bus
		dc_bus = bus.dc_bus
		nc_bus = bus.nc_bus
		mctrl_bus = bus.mctrl_bus

		loc = Blank()
		loc.m = Blank()
		m.submodules.sb_from_ic = loc.m.sb_from_ic = PipeSkidBuf(
		)
		#--------
		#--------
		return m
		#--------
#--------
