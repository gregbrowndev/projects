#include <iostream>
#include "libA.h"
#include "libB/libB.h"

int add_two(int x)
{
	std::cout << "Called external library 'libA' function 'add_two'" << std::endl;
	print_message();

	return x + 2;
}
