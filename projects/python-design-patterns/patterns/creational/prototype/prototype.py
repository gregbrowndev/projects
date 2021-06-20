# The Prototype Pattern
import copy
from dataclasses import dataclass


@dataclass
class Address:
    street: str
    city: str
    country: str

    def __str__(self):
        return f"{self.street}, {self.city}, {self.country}"


@dataclass
class Person(object):
    name: str
    address: Address

    def __str__(self):
        return f"{self.name} lives at {self.address}"


if __name__ == "__main__":
    john = Person("John", Address("123 London Road", "London", "UK"))
    print(john)

    # If we wanted to create a new person, Jane, who lives with John, you
    # we be tempted to do the following:
    # jane = john
    # jane.name = "Jane"
    # However, this doesn't work as jane refers to the same object to which john refers

    # we need to use deepcopy
    jane = copy.deepcopy(john)
    jane.name = "Jane"
    jane.address.street = "123B London Road"

    print("---")
    print(john)
    print(jane)

    # notice if we used 'shallow' copy mutating the address will change the original
    ben = copy.copy(john)
    ben.name = "Ben"
    ben.address.street = "154 London Road"

    print("---")
    print(john)
    print(ben)
