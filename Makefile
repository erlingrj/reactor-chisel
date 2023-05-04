# Makefile for building FPGA emulations and circuits
SBT ?= sbt
BUILD_PATH ?= build
TOP ?= TopReactorEx

all: build

run: build
	make -C $(BUILD_PATH) run

build:
	$(SBT) "run $(TOP) $(BUILD_PATH)"

test:
	$(SBT) test

integration-test:
	./integration-tests.sh

clean:
	rm -rf $(BUILD_PATH)

.phony: run build test integration-tests clean