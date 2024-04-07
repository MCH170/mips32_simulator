# mips32_simulator
MIPS32 cycle-accurate simulator.

Singlecycle, Multicycle, and Pipelined version (with Hazard Detection and Forwarding).

.

For commands see sample file.

It reads the commnd file and generates a new instr.txt file without tabs, spaces, comments and labels.

The end command is "sll $0, $0, $0" and is mandatory.

.

Singlecycle does not implement: jr, jal, lui

Pipelined does not implement: j, jr, jal, lui


