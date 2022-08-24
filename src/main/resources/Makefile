CPPFLAGS=--std=c++17 -g
LDFLAGS=-lstdc++fs
CC=g++
# requires a recent version of verilator, e.g. 3.916
TOP ?= $(shell dirname $(realpath $(filter %Makefile, $(MAKEFILE_LIST))))
TARGET_NAME:=RunReactor.x
VERILATOR_SRC_DIR:="/usr/share/verilator/include"

all:
	# call verilator to translate verilog to C++
	verilator -Iother-verilog --cc TesterWrapper.v -Wno-assignin -Wno-fatal -Wno-lint -Wno-style -Wno-COMBDLY -Wno-STMTDLY --Mdir verilated --trace
	# if verilator freezes while executing, consider adding +define+SYNTHESIS=1
	# to the cmdline here. this will disable the Chisel printfs though.

	# add verilated.cpp from source dirs
	cp -f $(VERILATOR_SRC_DIR)/verilated.cpp .
	cp -f $(VERILATOR_SRC_DIR)/verilated_vcd_c.cpp .
	# compile everything
	$(CC) $(CPPFLAGS) -I$(VERILATOR_SRC_DIR) -Iverilated *.cpp  verilated/*.cpp -o $(TARGET_NAME) $(LDFLAGS)

clean:
	rm verilated/*
	rm $(TARGET_NAME)