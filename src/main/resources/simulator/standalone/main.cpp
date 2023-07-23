#include <stdlib.h>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "VReactorChisel.h"

#define MAX_SIM_TIME 20
vluint64_t sim_time = 0;

double sc_time_stamp() { return sim_time; }

int main(int argc, char** argv, char** env) {
    bool trace_enabled = false;
    for (int i = 1; i< argc; i++) {
        if (!strcmp(argv[i], "--trace")) {
            std::cout << "Tracing enabled" << std::endl;
            trace_enabled = true;
        }
    }
    VReactorChisel *dut = new VReactorChisel;

    VerilatedVcdC *m_trace = NULL;
    if (trace_enabled) {
        Verilated::traceEverOn(true);
        m_trace = new VerilatedVcdC;
        dut->trace(m_trace, 5);
        m_trace->open("waveform.vcd");
    }
    
    // Simulate circuit until timeout, if specified
    int terminate = 0;

    while (terminate == 0) {
        if (sim_time <= 2) {
            dut->reset = 1;
        } else {
            dut->reset = 0;
        }
        dut->clock ^= 1;
        dut->eval();
        terminate = dut->io_terminate && sim_time > 2;
        if (trace_enabled) m_trace->dump(sim_time);
        sim_time++;
    }

    if (trace_enabled) m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}