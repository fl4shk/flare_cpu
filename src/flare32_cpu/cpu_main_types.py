#!/usr/bin/env python3

from amaranth import *
from amaranth.lib.data import *
from amaranth.lib import enum
import enum as pyenum

def MAIN_WIDTH():
	return 32
def MAIN_WIDTH_PAIR():
	return 64
def INSN_GRP_WIDTH():
	return 3
def INSN_MAIN_WIDTH():
	return 16
def INSN_LPRE_WIDTH():
	return 32
def INSN_MAX_WIDTH():
	return 64
def INSN_MAIN_NBYTES():
	return (INSN_MAIN_WIDTH() // 8)
def INSN_LPRE_NBYTES():
	return (INSN_LPRE_WIDTH() // 8)
def INSN_MAX_NBYTES():
	return (INSN_MAX_WIDTH() // 8)
#def GPR_SPR_WIDTH():
#	return 32
def NUM_GPRS():
	return 16
#def SPR_WIDTH():
#	return 32
#def NUM_SPRS():
#	return 16
#def REAL_NUM_SPRS():
def NUM_SPRS():
	return 6
def INSN_REG_WIDTH():
	return 4
def SPR_ITY_WIDTH():
	return 2

class Gpr(enum.Enum, shape=INSN_REG_WIDTH()):
	R0 = 0
	R1 = 1
	R2 = 2
	R3 = 3
	R4 = 4
	R5 = 5
	R6 = 6
	R7 = 7
	R8 = 8
	R9 = 9
	R10 = 10
	R11 = 11
	R12 = 12
	LR = 13
	FP = 14
	SP = 15
class Spr(enum.Enum, shape=INSN_REG_WIDTH()):
	FLAGS = 0
	IDS = 1
	IRA = 2
	IE = 3
	ITY = 4
	STY = 5
	RESERVED_6 = 6
	RESERVED_7 = 7
	RESERVED_8 = 8
	RESERVED_9 = 9
	RESERVED_10 = 10
	RESERVED_11 = 11
	RESERVED_12 = 12
	RESERVED_13 = 13
	RESERVED_14 = 14
	RESERVED_15 = 15

class Flag(pyenum.Enum):
	Z = 0 # Zero
	C = 1 # Carry
	V = 2 # oVerflow
	N = 3 # Negative
class Ity(enum.Enum, shape=INSN_REG_WIDTH()):
	IRQ = 0x0
	SWI = 0x1

def INSN_G0_GRP_VAL():
	return 0b000
def INSN_G0_PRE_SUBGRP_WIDTH():
	return 1
def INSN_G0_PRE_SUBGRP_VAL():
	return 0b0
def INSN_G0_PRE_SIMM_WIDTH():
	return 12

#def INSN_G0_LPRE_GRP_VAL():
#	return 0b000
def INSN_G0_LPRE_SUBGRP_WIDTH():
	return 2
def INSN_G0_LPRE_SUBGRP_VAL():
	return 0b10
def INSN_G0_LPRE_SIMM_WIDTH():
	return 27
def INSN_G0_LPRE_HI_SIMM_WIDTH():
	return

def INSN_G1_GRP_VAL():
	return 0b001
def INSN_G1_SIMM_WIDTH():
	return 5
def INSN_G1_OP_WIDTH():
	return (INSN_G0_LPRE_SIMM_WIDTH() - INSN_MAIN_WIDTH())

class InsnG1Op(enum.Enum, shape=INSN_G1_OP_WIDTH()):
	ADD_RA_S5 = 0x0
	ADD_RA_PC_S5 = 0x1
	ADD_RA_SP_S5 = 0x2
	ADD_RA_FP_S5 = 0x3
	ADD_CMP_RA_S5 = 0x4
	CPY_RA_S5 = 0x5
	LSL_RA_U5 = 0x6
	LSR_RA_U5 = 0x7

	ASR_RA_U5 = 0x8
	AND_RA_S5 = 0x9
	ORR_RA_S5 = 0xa
	XOR_RA_S5 = 0xb
	ZE_RA_U5 = 0xc
	SE_RA_U5 = 0xd
	SWI_RA_S5 = 0xe
	SWI_U5 = 0xf

def INSN_G2_GRP_VAL():
	return 0b010
def INSN_G2_F_WIDTH():
	return 1
def INSN_G2_OP_WIDTH():
	return 4

class InsnG2Op(enum.Enum, shape=INSN_G2_OP_WIDTH()):
	ADD_RA_RB = 0x0
	SUB_RA_RB = 0x1
	ADD_RA_SP_RB = 0x2
	ADD_RA_FP_RB = 0x3
	CMP_RA_RB = 0x4
	CPY_RA_RB = 0x5
	LSL_RA_RB = 0x6
	LSR_RA_RB = 0x7

	ASR_RA_RB = 0x8
	AND_RA_RB = 0x9
	ORR_RA_RB = 0xa
	XOR_RA_RB = 0xb
	ADC_RA_RB = 0xc
	SBC_RA_RB = 0xd
	CMPBC_RA_RB = 0xe
	RESERVED_15 = 0xf

def INSN_G3_GRP_VAL():
	return 0b011
def INSN_G3_SIMM_WIDTH():
	return 9
def INSN_G3_OP_WIDTH():
	return 4

class InsnG3Op(enum.Enum, shape=INSN_G3_OP_WIDTH()):
	BL_S9 = 0x0
	BRA_S9 = 0x1
	BEQ_S9 = 0x2
	BNE_S9 = 0x3
	BMI_S9 = 0x4
	BPL_S9 = 0x5
	BVS_S9 = 0x6
	BVC_S9 = 0x7

	BGEU_S9 = 0x8
	BLTU_S9 = 0x9
	BGTU_S9 = 0xa
	BLEU_S9 = 0xb
	BGES_S9 = 0xc
	BLTS_S9 = 0xd
	BGTS_S9 = 0xe
	BLES_S9 = 0xf

def INSN_G4_GRP_VAL():
	return 0b100
def INSN_G4_OP_WIDTH():
	return 5

class InsnG4Op(enum.Enum, shape=INSN_G4_OP_WIDTH()):
	JL_RA = 0x0
	JMP_RA = 0x1
	JMP_IRA = 0x2
	RETI = 0x3
	EI = 0x4
	DI = 0x5
	PUSH_RA_RB = 0x6
	PUSH_SA_RB = 0x7

	POP_RA_RB = 0x8
	POP_SA_RB = 0x9
	POP_PC_RB = 0xa
	MUL_RA_RB = 0xb
	UDIV_RA_RB = 0xc
	SDIV_RA_RB = 0xd
	UMOD_RA_RB = 0xe
	SMOD_RA_RB = 0xf

	LUMUL_RA_RB = 0x10
	LSMUL_RA_RB = 0x11
	UDIV64_RA_RB = 0x12
	SDIV64_RA_RB = 0x13
	UMOD64_RA_RB = 0x14
	SMOD64_RA_RB = 0x15
	LDUB_RA_RB_LDST = 0x16
	LDSB_RA_RB_LDST = 0x17

	LDUH_RA_RB_LDST = 0x18
	LDSH_RA_RB_LDST = 0x19
	STB_RA_RB_LDST = 0x1a
	STH_RA_RB_LDST = 0x1b
	CPY_RA_SC = 0x1c
	CPY_SA_RB = 0x1d
	CPY_SA_SB = 0x1e
	INDEX_RA = 0x1f

def INSN_G5_GRP_VAL():
	return 0b101
def INSN_G5_SIMM_WIDTH():
	return 5

def INSN_G6_GRP_VAL():
	return 0b110
def INSN_G6_SIMM_WIDTH():
	return 5

def INSN_G7_GRP_VAL():
	return 0b111

def INSN_G7_ALUOPBH_SUBGRP_WIDTH():
	return 2
def INSN_G7_ALUOPBH_SUBGRP_VAL():
	return 0b00
def INSN_G7_ALUOPBH_W_WIDTH():
	return 1
def INSN_G7_ALUOPBH_OP_WIDTH():
	return 2
class InsnG7AluopbhW(enum.Enum, shape=INSN_G7_ALUOPBH_W_WIDTH()):
	BYTE = 0b0
	HWORD = 0b1
class InsnG7AluopbhOp(enum.Enum, shape=INSN_G7_ALUOPBH_OP_WIDTH()):
	CMPBH_RA_RB = 0x0
	LSRBH_RA_rB = 0x1
	ASRB_RA_RB = 0x2
	RESERVED_3 = 0x3

def INSN_G7_SPRLDST_SUBGRP_WIDTH():
	return 3
def INSN_G7_SPRLDST_SUBGRP_VAL():
	return 0b010
def INSN_G7_SPRLDST_OP_WIDTH():
	return 2
class InsnG7SprldstOp(enum.Enum, shape=INSN_G7_SPRLDST_OP_WIDTH()):
	LDR_SA_RB_LDST = 0x0
	LDR_SA_SB_LDST = 0x1
	STR_SA_RB_LDST = 0x2
	STR_SA_SB_LDST = 0x3

def INSN_G7_ICRELOAD_SUBGRP_WIDTH():
	return 4
def INSN_G7_ICRELOAD_SUBGRP_VAL():
	return 0b0110
def INSN_G7_ICRELOAD_SIMM_WIDTH():
	return 5

class InsnG0PreLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G0_PRE_SUBGRP_WIDTH(),
			"simm": INSN_G0_PRE_SIMM_WIDTH(),
		})
class InsnG0LpreLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G0_LPRE_SUBGRP_WIDTH(),
			"simm": INSN_G0_LPRE_SIMM_WIDTH(),
		})
class InsnG0LpreHiLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G0_LPRE_SUBGRP_WIDTH(),
			"simm": INSN_G0_LPRE_HI_SIMM_WIDTH(),
		})
class InsnG1Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"simm": INSN_G1_SIMM_WIDTH(),
			"op": INSN_G1_OP_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG2Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"f": INSN_G2_F_WIDTH(),
			"op": INSN_G2_OP_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG3Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"simm": INSN_G3_SIMM_WIDTH(),
			"op": INSN_G3_OP_WIDTH(),
		})
class InsnG4Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"op": INSN_G4_OP_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG5Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"simm": INSN_G5_SIMM_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG6Layt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"simm": INSN_G6_SIMM_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG7AluopbhLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G7_ALUOPBH_SUBGRP_WIDTH(),
			"w": INSN_G7_ALUOPBH_W_WIDTH(),
			"op": INSN_G7_ALUOPBH_OP_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG7SprldstLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G7_SPRLDST_SUBGRP_WIDTH(),
			"op": INSN_G7_SPRLDST_OP_WIDTH(),
			"rb": INSN_REG_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
class InsnG7IcreloadLayt(StructLayout):
	def __init__(self):
		super().__init__({
			"grp": INSN_GRP_WIDTH(),
			"subgrp": INSN_G7_ICRELOAD_SUBGRP_WIDTH(),
			"simm": INSN_G7_ICRELOAD_SIMM_WIDTH(),
			"ra": INSN_REG_WIDTH(),
		})
