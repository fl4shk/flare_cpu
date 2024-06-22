.align 2
.text
.org 0x0000
.global _start
_start:
	cpy r0, #0x8899
	cpy r1, #0xaabb
	add r0, r1
	cpy r1, #10
	mul r0, r1
	bra infin

infin:
	bra infin

.bss
.org 0x8000
TEST_ARR_SIZE = 0x100
.global test_arr
test_arr:
	.space TEST_ARR_SIZE
