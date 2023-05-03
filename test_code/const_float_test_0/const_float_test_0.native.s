	.file	"const_float_test_0.c"
	.text
	.p2align 4
	.globl	get_3p5
	.type	get_3p5, @function
get_3p5:
.LFB0:
	.cfi_startproc
	vmovss	.LC0(%rip), %xmm0
	ret
	.cfi_endproc
.LFE0:
	.size	get_3p5, .-get_3p5
	.section	.rodata.cst4,"aM",@progbits,4
	.align 4
.LC0:
	.long	1080033280
	.ident	"GCC: (GNU) 12.2.1 20230201"
	.section	.note.GNU-stack,"",@progbits
