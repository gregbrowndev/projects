import datetime
from dataclasses import dataclass
from typing import TypeVar


@dataclass
class Person(object):
    name: str = ""
    position: str = ""
    date_of_birth: datetime.date = datetime.date(2000, 1, 1)

    def __str__(self):
        return f"{self.name} born on {self.date_of_birth} works as {self.position}"

    @staticmethod
    def new():
        return PersonBuilder()


# See docs on generic self: https://mypy.readthedocs.io/en/latest/generics.html#generic-methods-and-generic-self
T = TypeVar("T", bound="PersonBuilder")


class PersonBuilder(object):
    person: Person

    def __init__(self):
        self.person = Person()

    def build(self) -> Person:
        return self.person


class PersonInfoBuilder(PersonBuilder):
    def called(self: T, name: str) -> T:
        self.person.name = name
        return self


class PersonJobBuilder(PersonInfoBuilder):
    def works_as_a(self: T, position: str) -> T:
        self.person.position = position
        return self


class PersonBirthDateBuilder(PersonJobBuilder):
    def born(self: T, date_of_birth: datetime.date) -> T:
        self.person.date_of_birth = date_of_birth
        return self


if __name__ == "__main__":
    pb = PersonBirthDateBuilder()
    me = (
        pb.called("Greg")
        .works_as_a("Engineer")
        .born(datetime.date(1992, 9, 29))
        .build()
    )
    print(me)
