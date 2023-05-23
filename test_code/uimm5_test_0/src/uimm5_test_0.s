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
	ze r0, #testificate 
