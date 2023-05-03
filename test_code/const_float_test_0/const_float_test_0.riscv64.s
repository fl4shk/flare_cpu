	.file	"const_float_test_0.c"
	.option nopic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	get_3p5
	.type	get_3p5, @function
get_3p5:
	lui	a5,%hi(.LC0)
	flw	fa0,%lo(.LC0)(a5)
	ret
	.size	get_3p5, .-get_3p5
	.section	.srodata.cst4,"aM",@progbits,4
	.align	2
.LC0:
	.word	1080033280
	.ident	"GCC: (Arch User Repository) 12.2.0"
