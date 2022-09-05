# Makefile for building FPGA emulations and circuits
SBT ?= sbt
BUILD_PATH ?= build

test:
		$(SBT) "run $(BUILD_PATH)"

clean:
	rm -r $(BUILD_PATH)
