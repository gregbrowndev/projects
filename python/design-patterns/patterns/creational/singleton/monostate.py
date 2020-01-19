# The Monostate pattern is a variation of the Singleton pattern
# where many instances of a class share the same state stored in
# a static variable
from dataclasses import dataclass
from typing import TypedDict, ClassVar, TypeVar, Protocol, Generic


class CEOSharedState(TypedDict):
    name: str
    age: int


@dataclass(init=False)
class CEO(object):
    __shared_state: ClassVar[CEOSharedState] = {"name": "Steve", "age": 55}
    name: str
    age: int

    def __init__(self):
        # Assign shared_state to __dict__ so each instance refers
        # to the same attributes
        self.__dict__ = self.__shared_state


# We can improve this by making a generic baseclass
T = TypeVar("T")


class Monostate(Generic[T]):
    __shared_state: ClassVar[T]

    def __new__(cls, *args, **kwargs):
        obj = super().__new__(cls, *args, **kwargs)
        obj.__dict__ = cls.__shared_state
        return obj


class CFOState(TypedDict):
    name: str
    money_managed: int


@dataclass(init=False)
class CFO(Monostate[CFOState]):
    name: str
    money_managed: int


if __name__ == "__main__":
    ceo1 = CEO()
    print(ceo1)

    ceo2 = CEO()
    ceo2.age = 47

    print(ceo1)
    print(ceo2)

    print(ceo1 == ceo2)

    cfo1 = CFO()
    cfo2 = CFO()
