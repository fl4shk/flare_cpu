.align 2
.text
.global _start

_start:
	bra address_zero - .

.data
//.org 0x1000
text_start_addr:
	.word _text_start

