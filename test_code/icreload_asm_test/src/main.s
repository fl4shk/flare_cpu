.text
.align 2 
.global _start


_start:
	// This is how you can get the address of a label into a register:
	//add r0, pc, #target - .
target:
	add r0, pc, #main - .
	jmp r0
	//bra main

main:
	//xor r0, r0
	//xor r1, r1
	//icreload [r0, #main]
	//add r0, pc, #trampoline - .
	//add r1, pc, #trampoline_end - .
	//cpy r2, r1
	//sub r2, r0
	//xor r0, r0

CACHE_LINE_NBYTES = 64
	add r0, pc, #trampoline_end - .
	//cpy r1, #trampoline_end - trampoline
	add r1, pc, #trampoline - .
	//add r2, pc, #trampoline_static_chain - .
	///add r3, pc, #trampoline_dst - .

	// store `CACHE_LINE_NBYTES` in a register for higher performance than
	// there would be with `sub` with a `pre`
	cpy r4, #CACHE_LINE_NBYTES
	//add r1, pc, #trampoline - .

icreload_loop:
	icreload [r0]
	sub r0, r4
	cmp r0, r1
	bgts icreload_loop

icreload_loop_end:

main_infin:
	bra main_infin

trampoline:
	add sp, #-4 // allocate space on the stack for the jump target
	push r0
	push r1
	//push r2
	add r0, pc, #trampoline_static_chain - .
	ldr r1, [r0]
	//pop r2
	pop r1
	pop r0
	pop pc
// I think this is *NOT* where the `static_chain` would go in a GCC
// trampoline.
trampoline_static_chain:
	.space 4 
trampoline_dst:
	.space 4
trampoline_end:
