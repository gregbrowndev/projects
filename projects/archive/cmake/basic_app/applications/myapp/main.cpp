#include <iostream>

#include "mylib/mylib.h"

int main() {
	std::cout << "Internal executable 'myapp' main entry point" << std::endl;
    my_message("Hello, World!");
    std::cout << std::endl;
    return 0;
}
