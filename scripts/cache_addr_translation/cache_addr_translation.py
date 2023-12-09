#!/usr/bin/env python3

import sys
import math
#--------
class Blank:
	pass

def psconcat(*args):
	return str().join([str(arg) for arg in args])

def lsconcat(lst):
	#return str().join([str(elem) for elem in lst])
	return psconcat(*lst)

def fprintout(file, *args, flush=False):
	print(psconcat(*args), sep="", end="", file=file, flush=flush)

def printout(*args):
	fprintout(sys.stdout, *args)

def printerr(*args):
	fprintout(sys.stderr, *args)

def convert_enum_to_str(to_conv):
	return str(to_conv)[str(to_conv).find(".") + 1:]

def convert_str_to_enum_opt(to_conv, EnumT, STR_ENUM_MAP):
	if not (isinstance(to_conv, EnumT) or isinstance(to_conv, str)):
		raise TypeError(psconcat("convert_str_to_enum_opt() error: ",
			to_conv, " ", type(to_conv)))

	if isinstance(to_conv, EnumT):
		return to_conv
	else: # if isinstance(to_conv, str):
		if to_conv not in STR_ENUM_MAP:
			raise KeyError(to_conv)
		return STR_DIRECTION_MAP[to_conv]

def obj_err_str(obj, i=None, lst=None):
	if i is None:
		return psconcat("{!r}, {}".format(obj, type(obj)))
	else: # if i is not None:
		assert isinstance(i, int), \
			obj_err_str(i)
		assert lst is None or isinstance(lst, list), \
			obj_err_str(lst)

		if lst is None:
			return psconcat("{}, {!r}, {}".format(i, obj, type(obj)))
		else: # if isinstance(lst, list):
			return psconcat("{!r}, {}".format(lst, obj_err_str
				(obj, i, None)))
#--------
def MAIN_WIDTH():
	return 32
def CACHE_LINE_NBYTES():
	return 64
def CACHE_LINE_WIDTH():
	# 64 * 8 = 512
	return CACHE_LINE_NBYTES() * 8
def CACHE_LINE_BASE_ADDR_BITPOS():
	return math.ceil(math.log2(CACHE_LINE_NBYTES()))
def CACHE_MEM_DATA_WIDTH():
	return 16
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
#--------
#--------
