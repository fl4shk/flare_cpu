.text
.align 2
.global memcpy_naive

// args:
// r0: void dest[restrict .n]
// r1: const void src[restrict .n]
// r2: size_t n
// NOTE: this is a somewhat naive implementation if `index rA` is not
// slower than manually adding 1 to a pointer
memcpy_naive:
	// `dest + i` in r3
	cpy r3, r0

	// `dest + n` in r2
	add r2, r0

	// Also, `src + i` in r1
Lmemcpy_naive_loop:
	ldub r4, [r1]	// r4 = *r1;
	stb r4, [r3]	// *r3 = r4;
	add r1, #1		// ++r1
	add r3, #1		// ++r3;
	cmp r3, r2				// if `r3` == `dest + n`, then
	beq Lmemcpy_naive_loop	// continue;
Lmemcpy_naive_return:
	jmp lr


.global memcpy
// args:
// r0: void dest[restrict .n]
// r1: const void src[restrict .n]
// r2: size_t n
// NOTE this is a (potentially) faster implementation if `index rA` runs
// more quickly in the hardware implementation
memcpy:
	xor r3, r3	// `i` in r3; `i = 0;`
Lmemcpy_loop:
	ldub r4, [r1, r3]	// r4 = *(src + i);
	stb r4, [r0, r3]	// *(dest + i) = r4;
	add r3, #1
	cmp r3, r2
	beq Lmemcpy_loop
Lmemcpy_return:
	jmp lr
