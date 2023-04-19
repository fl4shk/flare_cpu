.align 2
.text
//.org 0x1000
.global _start
_start:
	//cpy r2, fp
branch_target:
	//bra 0x80000000
	//bgtu branch_target
	//bra test_label_2
	cpy r1, r3
test_label:
	//bra test_label_2 - _start
	//cpy fp, #test_label_2 - _start
	//cpy fp, #branch_target - test_label 

//.org 0x1000
test_label_2:
	//bra _start
	//bra test_label_2
	//cpy r2, sp

//.org 0x2000
test_label_3:
	//bra branch_target
	//cpy sp, #test_label_3 - test_label_2
	//cpy sp, #test_label_3 - _start
	//cpy sp, #_start
	//cpy fp, #3
	//cpy fp, #0x3333
	//add r0, #0x3
	//add r0, pc, #3
	//sub r3, fp
	//cmp sp, fp
	//cpy hi, fp
	//cpy fp, hi
	//reti
	//jmp ira
	//push r0, sp
	//pop hi, sp
	//push hi, sp
	//.word 0x10000000
	//.word test_label_3
	//cpy r4, #3
	//jmp r5
	//ldr r0, [r5, #0x0]
	//str r0, [r5, #0x3]
	//bra 0x2
	//cpy fp, #test_label_2 - _start
	//bra test_label_3 - test_label_2
	//bra test_label_2
	//cpy r0, #3
	//index r0
	ldub r0, [r3, r5]

.data
//.org 0x1008
test_data:
	.word _start
	//.byte 0x88, 0x89
	//.word test_label_3
	//.word test_data
