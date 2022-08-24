#include <iostream>

#include "VaddReactor.hpp"
#include "platform.h"

using namespace std;

double sc_time_stamp() {
    return 0;
}

int main(int argc, char** argv) {
    cout << "Software driver for VaddReactor is running" << endl;
    WrapperRegDriver * platform;
    VaddReactor * fpga_reactor;

    try {
        platform = initPlatform();
        fpga_reactor = new VaddReactor(platform);
    } catch (char const * err) {
        cout << "Threw error: " << err <<endl;
        return -2;
    }

    const int n_inputs = 10;
    const int n_outputs = 5;

    uint64_t input[n_inputs];
    uint64_t output[n_outputs];

    for (int i = 0; i<n_inputs; i++) {
        input[i] = i;
    }

    unsigned int input_buf_size = n_inputs * sizeof(input[0]);
    unsigned int output_buf_size = n_outputs * sizeof(output[0]);

    // Allocate buffers that are accessible by the FPGA reactor, e.g. they are in shared mem
    void * fpga_reactor_input_buf = platform->allocAccelBuffer(input_buf_size);
    void * fpga_reactor_output_buf = platform->allocAccelBuffer(output_buf_size);

    // Move data into shared buffers
    platform->copyBufferHostToAccel(input, fpga_reactor_input_buf, input_buf_size);

    fpga_reactor->set_baseAddr((AccelDblReg) fpga_reactor_input_buf);
    fpga_reactor->set_baseAddrRes((AccelDblReg)fpga_reactor_output_buf);
    fpga_reactor->set_start(1);
    fpga_reactor->set_start(0);

    while(fpga_reactor->get_done() != 1);

    cout << "VaddReactor finished after " << fpga_reactor->get_cycles() << " cycles" << endl;
    cout << "Results are: " <<endl;
    platform->copyBufferAccelToHost(fpga_reactor_output_buf, output, output_buf_size);

    bool success = true;
    for (int i = 0; i<n_outputs; i++) {
        cout <<input[i*2] <<" + " <<input[i*2 + 1] << " = " <<output[i] <<endl;
        int expected = input[i*2]+input[i*2+1];
        if (expected != output[i]) {
            cout << "ERROR: results dont match" << endl;
            success = false;
        }
    }

    platform->deallocAccelBuffer(fpga_reactor_input_buf);
    platform->deallocAccelBuffer(fpga_reactor_output_buf);

    if (success) return 0;
    else return -1;
}