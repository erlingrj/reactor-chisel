EMULATOR_BIN:=emu
TRACE ?= false
VERILATOR_SRC_DIR=/usr/share/verilator/include
VERIlATOR_SRC_GEN_DIR=verilated
VERILATOR_OPTIONS=-Iother-verilog -Wno-assignin -Wno-fatal -Wno-lint -Wno-style -Wno-COMBDLY -Wno-STMTDLY --trace
CC_OPTIONS := -CFLAGS "-DTRACE"

PLATFORM := ReactorChisel

$(EMULATOR_BIN): main.cpp
	@verilator --cc $(PLATFORM).v $(VERILATOR_OPTIONS) $(CC_OPTIONS) --exe --build main.cpp
	@cp obj_dir/V$(PLATFORM) $(EMULATOR_BIN)

build: $(EMULATOR_BIN)

clean:
	@rm -rf obj_dir
	@rm -f $(EMULATOR_BIN)

rebuild: clean build

run: build
	@./$(EMULATOR_BIN) --timeout 100 --trace

.PHONY: build clean rebuild run
