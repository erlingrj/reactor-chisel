A reactor-runtime for doing HW/SW codesign in Lingua Franca
=======================

reactor-chisel is a hardware runtime enabling execution of reactors on FPGAs.
This project is connected to a project extending the Lingua Franca Compiler
(lfc) with a Chisel-target and a Codesign-target. This enables programming
hardware and software for SoC FPGAs at the "same level of abstraction".


## Prerequisits
- A Ubuntu 20.04 system.
- Java17 or above.
- verilator v4.222,  on Ubuntu it can be installed with `sudo apt install verilator`.
- g++ v9.4,0 or higher.
- CMake version 3.20 or higher.
- gtkwave
- SBT see installation guide here: https://www.scala-sbt.org/download.html

## Getting started
This repository is submoduled in our fork of the Lingua Franca compiler. To get
started clone our fork and run the Chisel and Codesign tests.
```
git clone www.github.com/erlingrj/lingua-franca.git && cd lingua-franca
./gradlew targetTest -Ptarget=Chisel
./gradlew targetTest -Ptarget=Cpp
```

This will build the compiler and run all the tests located in `test/Chisel/src`
and `test/Codesign/src`. Please refer to these tests for programs demonstrating
the features supported by the Chisel and Codesign target of Lingua Franca.

To build an `lfc` excutable issue: `./gradlew assemble`. The resuling executable
can be found in `build/install/lf-cli`. Either move `lfc` to a directory on the
system PATH or add the install directory to to PATH. 

From now we will assume that `lfc` is on your PATH.

## Hello World standalone
Lingua Franca projects are typically structured with source files organized
under a `src` directory and the compiler puts generated files under `src-gen`,
and compiled executables under `bin`. See the [LF
Handbook](https://www.lf-lang.org/docs/handbook/a-first-reactor/?target=c#structure-of-an-lf-project)
for more information.

To create a simple standalone HelloWorld program to excute in hardware:
```
mkdir hello-reactors && cd hello-reactors
mkdir src && touch src/HelloWorld.lf
```

Copy the following simple program:
```
target Chisel;

main reactor {
  reaction(startup) {=
    printf("Hello from this HW reactor @ %d\n", lf_time_logical())
  =}
}
```

Then run lfc

```
lfc src/HelloWorld.lf
```

This will eventually produce an executable which you now can run:
```
bin/lfc --trace
> Tracing enabled
> Hello from this HW reactor @              0
```


For more information on reactors and Lingua Franca visit: www.lf-lang.org
