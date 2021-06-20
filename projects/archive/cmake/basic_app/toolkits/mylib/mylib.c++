#include <iostream>

// import thirdparty library
#include "extlibrary/libA/libA.h"

#include "mylib.h"
#include "const.h"
#include "myotherlib/myotherlib.h"

void my_message(const std::string &message) {
	std::cout << "Called internal library 'mylib' function 'my_message'" << std::endl;
    std::cout << message << " Cya!" << x << std::endl;
    
    my_other_message();
    
    // call thirdparty library
    int x = add_two(5);
    
    std::cout << "5 plus 2 = " <<  x << std::endl;
}
