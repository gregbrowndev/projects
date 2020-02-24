# Dependency Inversion Principle (DIP)
import enum
from dataclasses import dataclass, field
from typing import List, NamedTuple, Protocol, Iterable


@enum.unique
class Relationship(enum.Enum):
    PARENT = 0
    CHILD = 1
    SIBLING = 2


@dataclass
class Person(object):
    name: str


class Relation(NamedTuple):
    person1: Person
    relationship: Relationship
    persion2: Person


@dataclass()
class Relationships(object):
    relations: List[Relation] = field(default_factory=list)

    def add_parent_and_child(self, parent: Person, child: Person) -> None:
        self.relations.append(Relation(parent, Relationship.PARENT, child))
        self.relations.append(Relation(child, Relationship.CHILD, parent))


# high-level module violating DIP
# This code violates DIP as it depends on the concrete implementation of how
# relations are stored in the low-level module
class Research(object):
    @staticmethod
    def do_research(relationships: Relationships):
        for r in relationships.relations:
            if r[0].name == "John" and r[1] == Relationship.PARENT:
                print(f"John has a child called {r[2].name}.")


# To solve this issue we should code against an abstraction!
# low-level module provides interface
class RelationshipBrowser(Protocol):
    def find_all_children_of(self, name) -> Iterable[Person]:
        ...


# This is the Relationships class as before but now it is a concrete implementation of our interface
@dataclass()
class RelationshipsDIP(RelationshipBrowser):
    relations: List[Relation] = field(default_factory=list)

    def add_parent_and_child(self, parent: Person, child: Person) -> None:
        self.relations.append(Relation(parent, Relationship.PARENT, child))
        self.relations.append(Relation(child, Relationship.CHILD, parent))

    def find_all_children_of(self, name) -> Iterable[Person]:
        for r in self.relations:
            if r[0].name == "John" and r[1] == Relationship.PARENT:
                yield r[2]


# high-level module adhering to DIP
class ResearchDIP(object):
    @staticmethod
    def do_research(browser: RelationshipBrowser):
        for person in browser.find_all_children_of("John"):
            print(f"John has a child called {person.name}.")


if __name__ == "__main__":
    parent = Person("John")
    child1 = Person("Chris")
    child2 = Person("Mat")

    # code violating DIP
    # relationships = Relationships()
    # relationships.add_parent_and_child(parent, child1)
    # relationships.add_parent_and_child(parent, child2)
    #
    # Research.do_research(relationships)

    # code adhering to DIP
    relationships = RelationshipsDIP()
    relationships.add_parent_and_child(parent, child1)
    relationships.add_parent_and_child(parent, child2)

    ResearchDIP.do_research(relationships)
