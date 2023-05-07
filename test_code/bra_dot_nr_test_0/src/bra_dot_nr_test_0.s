.align 2
.text
.global _start
_start:
	//bra.nr _start
	cpy.nr r0, #3

test:
	bra.nr test

testificate:
	bra testificate
