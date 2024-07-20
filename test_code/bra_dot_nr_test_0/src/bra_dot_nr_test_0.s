.align 2
.text
.global _start
_start:
	//////cpy r0, #0xffff
	//////cpy r1, #testificate
	//////cpy r2, #asdf
	//////lsl r3, #5
	//////cpy r2, r3
	//////lsl r3, #5
	//////cpy r4, r5
	////cpy r3, r5
	////bra _start
	////bra target
	////cpy r2, r3
	////cpy r3, r5
	//////cpy r2, r3

	////bra testificate
	////bra target
	////cpy.nr r0, #0
	////cpy.nr r0, #-1
	////add.nr r0, #-1//#1 << 30
	////add r2, #1 << 30
	////add r3, r5
	////add.nr r2, #_start + 3
	////add r2, #_start + 3
	////add r2, #_start + 3//0x1003
	////add r2, #asdf
	//add r2, #_start
	////cpy r8, r9

//.global asdf
asdf:
	//add r5, #asdf
	//bra asdf
	//add r5, #testificate
	//xchg.l [r9], sp
	//xchg r12, r3
	//cmpxchg.l [r3], r4, r5
	//cmpxchg [r6], r7, r8
	//bvc asdf
	ldr r3, [r4, #asdf]
	ldr r5, [r6]
	stb r6, [r9, #0x7f]
	sth r8, [sp, fp, #999]
	bgeu asdf
	//ldr r8, [r12, sp]
	//stb r1, [r2, #0x3]
	//lduh r1, [r2]
	//add r3, #asdf
	////ldr r5, [r6, r7]
	////lduh r8, [r6, r9]
	////add r3, #0x12345


////.org 0x200034
//.org 0x32000
////.global target
//target:
//	//cpy r0, #target
//	//cpy r0, #0x3333
//	//cpy r0, #_start
//	cpy r0, #target
//	//cpy r0, #target + 0x3000
//	lsr r0, #target + 0x3000
//	//bra.nr target
//	//bra _start
//	//bra testificate
//	//str r0, [r0, r2, #5]
//	//cpy r
//	pop pc
//infin:
//	bra infin
//	bra testificate
//
////some_data:
////	.hword some_data
////	.hword target
////	.hword some_data - infin 
//
//asdf = 0x3333
////	//bra.nr _start
////	cpy.nr r0, #3
////
////test:
////	bra.nr test
////
////testificate:
////	bra testificate
////	//jmp 0
////	//ret
