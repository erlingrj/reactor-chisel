# Makefile for building FPGA emulations and circuits
SBT ?= sbt

test:
	$(SBT) test
.phony: test