
mul_test_0.elf:     file format elf32-flare32


Disassembly of section .text:

00001000 <sum_u32>:
    1000:	20 41       	cmp	r1, #0
    1002:	61 22       	beq	18
    1004:	22 61       	lsl	r1, #2
    1006:	45 02       	cpy	r2, r0
    1008:	40 12       	add	r2, r1
    100a:	20 51       	cpy	r1, #0

0000100c <.L3>:
    100c:	a0 03       	ldr	r3, [r0, #0]
    100e:	40 31       	add	r1, r3
    1010:	24 00       	add	r0, #4
    1012:	54 20       	cmp	r0, r2
    1014:	7f 63       	bne	-10

00001016 <.L1>:
    1016:	45 10       	cpy	r0, r1
    1018:	81 0d       	jmp	lr

0000101a <.L0>:
	...

0000101c <_start>:
    101c:	04 44 39 50 	cpy	r0, #34969 // pre #0x444
    1020:	05 55 3b 51 	cpy	r1, #43707 // pre #0x555
    1024:	40 10       	add	r0, r1
    1026:	2a 51       	cpy	r1, #10
    1028:	8b 10       	mul	r0, r1
    102a:	60 01       	bra	0

0000102c <infin>:
    102c:	7f e1       	bra	-2
	...

0000901c <test_arr>:
	...
