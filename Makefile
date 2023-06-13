# Makefile for building FPGA emulations and circuits
SBT ?= sbt
TOP ?= ReactorCounter
PROGRAM_TYPE ?= standalone
BUILD_PATH ?= build/$(TOP)

GENERATED_VERILOG=$(BUILD_PATH)/ReactorChisel.v

all: build

run: build
	make -C $(BUILD_PATH) run

build: $(GENERATED_VERILOG)

$(GENERATED_VERILOG):
	$(SBT) "run $(PROGRAM_TYPE) $(TOP) $(BUILD_PATH)"

test:
	$(SBT) test

clean:
	rm -rf $(BUILD_PATH)

rebuild: clean build
rerun: clean run

.phony: run build test clean rebuild rerun