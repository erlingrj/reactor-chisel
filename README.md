A Reactor implementation in Chisel3
=======================

This repo contains the work-in-progress of implementing Reactors in Chisel3.
This project is targeting both HW/SW codesign and pure HW development at the reactor-level.

Reactors are a model of computation (MoC) developed for designing concurrent cyber-physical system.
It has many similarities with the Discrete-Time MoC underlying HDLs such as VHDL, Verilog and SystemC.
Reactor-chisel implements a quasi-static dataflow variant of the Reactor MoC. All reactions behave like homgenous synchronous dataflow actors.
They are triggered when there are events (tokens) at each input port seen (either as a trigger or as a dependency) by the reaction.
A Reaction firing will also generate a single event on each output port (antidependency). Absent tokens are added to support the ability to
conditionally produce outputs from a reaction.  

Timers and a Modified Logical Action can be supported with out difficult and without introducing an event queue in hardware. 


For more information on Reactors and Lingua Franca visit: www.lf-lang.org
