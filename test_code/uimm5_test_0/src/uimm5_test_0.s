.align 2
.text
.global _start
_start:
	cpy r0, #-1
	cpy r0, #-16
	cpy r0, #16
	lsr r0, #16
	lsr r0, #31
	lsl r0, #32
	cpy r1, #31
	cpy r1, #testificate
	ldr r3, [r1, r2, #3]
	str r8, [r2, #3]
	ze r0, #testificate 
	icflush
	icreload [r6, r7, #-0x1]
	cmpxchg.l [r3], r8, r4
