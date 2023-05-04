#include <iostream>

#include "TopReactorEx.hpp"
#include "platform.h"

using namespace std;

double sc_time_stamp() {
    return 0;
}

int main(int argc, char** argv) {
    cout << "Software driver for TopReactorEx is running" << endl;
    WrapperRegDriver * platform;
    TopReactorEx * fpga_reactor;

    try {
        platform = initPlatform();
        fpga_reactor = new TopReactorEx(platform);
    } catch (char const * err) {
        cout << "Threw error: " << err <<endl;
        return -2;
    }
    fpga_reactor->set_in_present(1);
    fpga_reactor->set_in_data(42);
    fpga_reactor->set_start(1);
    fpga_reactor->set_start(0);

    while(fpga_reactor->get_done() != 1);
    cout << "Results are: out1=" <<fpga_reactor->get_out1_data() <<" out2=" <<fpga_reactor->get_out2_data() <<endl;
    if (fpga_reactor->get_out1_data() != 48 || fpga_reactor->get_out2_data() != 46) {
    cerr << "Results wrong, expected 48 and 46" << endl;
    return 1;
    }
    return 0;
}