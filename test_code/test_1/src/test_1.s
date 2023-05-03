.align 1
.text
.global _start

newlib_sys_exit = 1
newlib_sys_open = 2
newlib_sys_close = 3
newlib_sys_read = 4
newlib_sys_write = 5
newlib_sys_unlink = 7

stdout = 0

O_RDONLY = 0x0000
O_WRONLY = 0x0001
O_RDWR = 0x0002
O_APPEND = 0x0008
O_CREAT = 0x0200
O_TRUNC = 0x0400
O_EXCL = 0x0800
O_SYNC = 0x2000

//O_ACCMODE = 0x00000003
//O_RDONLY = 0x00000000
//O_WRONLY = 0x00000001
//O_RDWR = 0x00000002
//O_CREAT = 0x00000100 /* not fcntl */
//O_EXCL = 0x00000200 /* not fcntl */
//O_NOCTTY = 0x00000400 /* not fcntl */
//O_TRUNC = 0x00001000 /* not fcntl */
//O_APPEND = 0x00002000
//O_NONBLOCK = 0x00004000
//O_DSYNC = 0x00010000 /* used to be O_SYNC, see below */
//FASYNC = 0x00020000 /* fcntl, for BSD compatibility */
//O_DIRECT = 0x00040000 /* direct disk access hint */
//O_LARGEFILE = 0x00100000
//O_DIRECTORY = 0x00200000 /* must be a directory */
//O_NOFOLLOW = 0x00400000 /* don't follow links */
//O_NOATIME = 0x01000000
//O_CLOEXEC = 0x02000000 /* set close_on_exec */
//__O_SYNC = 0x04000000
//O_SYNC = (__O_SYNC | O_DSYNC)
//O_PATH = 0x010000000


buflen = 1024
.extern test_fname
.extern test_fname_end
.extern test_str
.extern test_str_end

_start:
	//cpy sp, #0x10000000
	//bra _start + 2
	bra main

main:
	// Open a test file
	//cpy r0, #test_fname
	//cpy r1, #O_WRONLY | O_CREAT | O_APPEND //| O_SYNC
	//cpy r2, #0x0
	//swi #newlib_sys_open

	//cpy r3, r0
	cpy r0, #0		// stdout
	cpy r1, #test_str
	cpy r2, #test_str_end - test_str
	swi #newlib_sys_write

	//cpy r1, #buf
	//cpy r2, #buflen
	//swi #newlib_sys_write

	//cpy r0, r3
	//swi #newlib_sys_close

	// end the simulation
	swi #newlib_sys_exit

.bss
buf:
	.space buflen
