import copy
from dataclasses import dataclass


@dataclass
class Address(object):
    street: str
    suite: int
    city: str

    def __str__(self):
        return f"{self.street}, Suite #{self.suite}, {self.city}"


@dataclass
class Employee(object):
    name: str
    address: Address

    def __str__(self):
        return f"{self.name} works at {self.address}"


class EmployeeFactory(object):
    # Create static prototypes for the two office locations
    main_office_employee = Employee("", Address("123 East Drive", 0, "London"))
    aux_office_employee = Employee("", Address("123B East Drive", 0, "London"))

    @staticmethod
    def _new_employee(proto, name, suite) -> Employee:
        result: Employee = copy.deepcopy(proto)
        result.name = name
        result.address.suite = suite
        return result

    @staticmethod
    def new_main_office_employee(name, suite) -> Employee:
        return EmployeeFactory._new_employee(
            EmployeeFactory.main_office_employee, name, suite
        )

    @staticmethod
    def new_aux_office_employee(name, suite) -> Employee:
        return EmployeeFactory._new_employee(
            EmployeeFactory.aux_office_employee, name, suite
        )


if __name__ == "__main__":
    john = EmployeeFactory.new_main_office_employee("John", 101)
    jane = EmployeeFactory.new_aux_office_employee("Jane", 500)

    print(john)
    print(jane)
