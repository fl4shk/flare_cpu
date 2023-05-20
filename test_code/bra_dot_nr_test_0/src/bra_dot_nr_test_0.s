.align 2
.text
.global _start
_start:
	////cpy r0, #0xffff
	////cpy r1, #testificate
	////cpy r2, #asdf
	////lsl r3, #5
	////cpy r2, r3
	////lsl r3, #5
	////cpy r4, r5
	//cpy r3, r5
	//bra _start
	//bra target
	//cpy r2, r3
	//cpy r3, r5
	////cpy r2, r3

	//bra testificate
	bra target

//.org 0x200034
.org 0x32000
target:
	//cpy r0, #target
	//cpy r0, #0x3333
	//cpy r0, #_start
	cpy r0, #target
	lsr r0, #target + 0x3000
	bra.nr target
	bra _start
	bra testificate
//infin:
//	bra infin
//	bra testificate

//some_data:
//	.hword some_data
//	.hword target
//	.hword some_data - infin 

asdf = 0x3333
//	//bra.nr _start
//	cpy.nr r0, #3
//
//test:
//	bra.nr test
//
//testificate:
//	bra testificate
//	//jmp 0
//	//ret
