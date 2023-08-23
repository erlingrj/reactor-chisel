EMULATOR_BIN:=emu
TRACE ?= false
VERILATOR_SRC_DIR=/usr/share/verilator/include
VERIlATOR_SRC_GEN_DIR=verilated
VERILATOR_OPTIONS=-Iother-verilog -Wno-assignin -Wno-fatal -Wno-lint -Wno-style -Wno-COMBDLY -Wno-STMTDLY --trace
CC_OPTIONS := -CFLAGS "-DTRACE"

PLATFORM := TesterWrapper
VER_BUILD_DIR := obj_dir

SRCS=main.cpp platform-verilatedtester.cpp

TARGET_LIB=lfFPGA.a

$(EMULATOR_BIN): $(SRCS)
	verilator --cc $(PLATFORM).v $(VERILATOR_OPTIONS) $(CC_OPTIONS) --exe --build $^
	@cp obj_dir/V$(PLATFORM) $(EMULATOR_BIN)

build: $(EMULATOR_BIN)

lib: $(SRCS)
	verilator --cc $(PLATFORM).v $(VERILATOR_OPTIONS) $(CC_OPTIONS) --exe --build $^
	@rm $(VER_BUILD_DIR)/main.o
	ar -rcs $(TARGET_LIB) $(VER_BUILD_DIR)/*.o

clean:
	@rm -rf obj_dir $(TARGET_LIB)
	@rm -f $(EMULATOR_BIN)

rebuild: clean build

run: build
	@./$(EMULATOR_BIN) --timeout 100 --trace

.PHONY: build clean rebuild run
