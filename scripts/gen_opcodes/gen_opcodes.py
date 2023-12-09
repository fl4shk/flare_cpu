#! /usr/bin/env python3

import os, sys, enum

def printerr(*args, **kwargs):
	print(*args, file=sys.stderr, **kwargs)

def sconcat(*args):
	ret = ""
	for arg in args:
		ret += str(arg)
	return ret


