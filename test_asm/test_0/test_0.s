.align 2
.text
//.org 0x1000
.global _start
_start:
	//cpy r0, #0x3
	//add sp, #0x3
	//cpy r0, #_start - test_label
	//bra #_start
	//bra #test_label
	//bra #test_label_2
	//bra #branch_target
	//cpy r2, fp
branch_target:
	//bra #0x80000000
	bgtu #branch_target
	bra #test_label_2
	//cpy r0, #test_label_2
	//cpy r3, #0x1ff
	//cpy r3, #_start
	//cpy sp, #_start
	//cpy r0, r1
	//bra #_start
test_label:
	//bra #test_label_2 - _start
	//cpy fp, #test_label_2 - _start
	//#cpy fp, #branch_target - test_label 
	//bra #test_label
	//bra #test_label
	//bra #_start

//.org 0x1000
test_label_2:
	//bra #_start
	//bra #test_label_2
	//cpy r2, sp

//.org 0x2000
test_label_3:
	cpy sp, #test_label_3 - test_label_2
	//cpy 
	//cpy fp, #test_label_2 - _start
	//bra #test_label_3 - test_label_2
	bra #test_label_2
	cpy r0, #3

.data
//.org 0x1008
test_data:
	//.byte 0x88, 0x89
	//.word test_label_3
	.word test_data
