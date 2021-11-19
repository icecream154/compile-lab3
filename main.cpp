#include <iostream>
  
extern "C" {
            double foo(int, int);
}

int main() {
            std::cout << "test of 3, 4 is :" << foo(3,4) << std::endl;
            std::cout << "test of 5, 6 is :" << foo(5,6) << std::endl;
}
