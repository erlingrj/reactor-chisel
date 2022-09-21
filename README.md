A Reactor implementation in Chisel3
=======================

This repo contains the work-in-progress of implementing Reactors in Chisel3.
This project is targeting HW/SW codesign and is not inteded for executing complete reactor networks on an FPGA.

Reactors are a model of computation (MoC) developed for designing concurrent cyber-physical system.
It has many similarities with the Discrete-Time MoC underlying HDLs such as VHDL, Verilog and SystemC.
Reactor-chisel implements a subset of the Reactor MoC which roughly corresponds to the Synchronous MoC.
Actions, Timers and delayed connections are not allowed and thus reactor-chisel is unable to advance logical time by itself.

reactor-chisel is intended as a backend for the Lingua Franca coordination language to enable HW/SW codesign "at the reactor-level".

For more information on Reactors and Lingua Franca visit: www.lf-lang.org
