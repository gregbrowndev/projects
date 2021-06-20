#include <iostream>

#include "myotherlib.h"

void my_other_message() {
	std::cout << "Called internal library 'myotherlib' function 'my_other_message'" << std::endl;
    std::cout << "This is a message from myotherlib" << std::endl;
}
