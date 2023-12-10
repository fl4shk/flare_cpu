# flare32\_cpu
A 32-bit soft CPU being developed in SpinalHDL. There is no support
for virtual memory at this time. Only integer operations are implemented in
hardware.

<!--
You will need [libcheesevoyage](https://github.com/fl4shk/libcheesevoyage)
to use the HDL code.
-->

The instruction set of Flare32 has been documented in
`./docs/flare32_cpu/flare32_cpu.pdf`

Most of a GNU Binutils port has been written:
[binutils-gdb-flare32](https://github.com/fl4shk/binutils-gdb-flare32)

<!--
A GCC port is in progress, but is not public yet, besides the partially
written one in the form of a `.patch` file in this git repository. A lot of
progress has been made on the current version of the GCC port.
-->
A GCC port is in progress:
[gcc-flare32](https://github.com/fl4shk/gcc-flare32)

A picolibc implementation has been started:
[picolibc-flare32](https://github.com/fl4shk/picolibc-flare32)
