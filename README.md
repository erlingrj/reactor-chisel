A HW Reactor framework in Chisel
=======================


## Architecture

### Scheduler

### HWReactor
- Top level entity which communicates with SW through a register file and shared memory
- The memory ports are forwarded to Reactions communicating directly to its ports somehow and accesses to DRAM is done at the "right" address
- All the Reactions, Ports and Connections are instantiated by user (code generator) within the HWReactor


### Reactions

### Ports

### Connec