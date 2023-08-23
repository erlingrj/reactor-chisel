#include <iostream>
#include "platform.h"
#include "CodesignTopReactor.hpp"
#include <string>

using namespace std;

int main() {
  WrapperRegDriver * platform = initPlatform();
  CodesignTopReactor t(platform);

  cout << "Signature: " << hex << t.get_signature() << dec << endl;

  return 0;
}
