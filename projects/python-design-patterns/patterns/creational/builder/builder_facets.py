# Builder Facets is the approach of using multiple builders to
# create an object. This is useful when the creation of an object
# is very complicated.
from dataclasses import dataclass, field


@dataclass
class Person(object):
    # address
    street: str = ""
    postcode: str = ""
    city: str = ""

    # employment
    company_name: str = ""
    position: str = ""
    annual_income: int = 0

    def __str__(self):
        return (
            f"Address: {self.street}, {self.postcode}, {self.city}. "
            f"Employed at {self.company_name} as a {self.position} earning "
            f"{self.annual_income}"
        )


@dataclass
class PersonBuilder(object):
    person: Person = field(default_factory=Person)

    @property
    def works(self) -> "PersonJobBuilder":
        return PersonJobBuilder(self.person)

    @property
    def lives(self) -> "PersonAddressBuilder":
        return PersonAddressBuilder(self.person)

    def build(self) -> Person:
        return self.person


class PersonJobBuilder(PersonBuilder):
    def __init__(self, person: Person):
        super().__init__(person)

    def at(self, company_name: str) -> "PersonJobBuilder":
        self.person.company_name = company_name
        return self

    def as_a(self, position: str) -> "PersonJobBuilder":
        self.person.position = position
        return self

    def earning(self, annual_income: int) -> "PersonJobBuilder":
        self.person.annual_income = annual_income
        return self


class PersonAddressBuilder(PersonBuilder):
    def __init__(self, person: Person):
        super().__init__(person)

    def at(self, street: str) -> "PersonAddressBuilder":
        self.person.street = street
        return self

    def with_postcode(self, postcode: str) -> "PersonAddressBuilder":
        self.person.postcode = postcode
        return self

    def in_city(self, city: str) -> "PersonAddressBuilder":
        self.person.city = city
        return self


if __name__ == "__main__":
    pb = PersonBuilder()
    person = (
        pb.lives.at("123 London Road")
        .in_city("London")
        .with_postcode("SW12BC")
        .works.at("Fabrikam")
        .as_a("Engineer")
        .earning(123000)
    ).build()

    print(person)
