#include <iostream>

#include "libB.h"


void print_message()
{
	std::cout << "Called external library 'libB' function 'print_message'" << std::endl;
	std::cout << "This is a transitive dependency!" << std::endl;
}
