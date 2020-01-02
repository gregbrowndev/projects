# Abstract Factory
import enum
from typing import Protocol, TypeVar, Optional, List, Tuple, Dict


class HotDrink(Protocol):
    def consume(self):
        ...


class Tea(HotDrink):
    def consume(self):
        print("This tea is delicious")


class Coffee(HotDrink):
    def consume(self):
        print("This coffee is delicious")


# Factories
T_co = TypeVar("T_co", covariant=True, bound=HotDrink)


class HotDrinkFactory(Protocol[T_co]):
    def prepare(self, amount: int) -> T_co:
        ...


class TeaFactory(HotDrinkFactory[Tea]):
    def prepare(self, amount: int) -> Tea:
        # do stuff to prepare tea
        print(f"Put in tea bag, boil water, pour {amount}ml, enjoy!")
        return Tea()


class CoffeeFactory(HotDrinkFactory[Coffee]):
    def prepare(self, amount: int) -> Coffee:
        # do stuff to prepare coffee
        print(f"Grind some beans, boil water, pour {amount}ml, enjoy!")
        return Coffee()


# Using the factory hierarchy without leveraging the ABC
def make_drink(drink_type: str) -> Optional[HotDrink]:
    if drink_type == "tea":
        return TeaFactory().prepare(200)
    elif drink_type == "coffee":
        return CoffeeFactory().prepare(50)
    return None


# We can organise things a little better and make use of the Abstract Factory pattern
class HotDrinkMachine(object):
    # Use Enum to specify available drinks in API. Note this violates OCP.
    class AvailableDrink(enum.Enum):
        COFFEE = enum.auto()
        TEA = enum.auto()

    factories: Dict[str, HotDrinkFactory]
    initialised = False

    def __init__(self):
        if not self.initialised:
            self.initialised = True
            self.factories = {}
            for d in self.AvailableDrink:
                # transform enum into capitalised drink name
                name = d.name[0] + d.name[1:].lower()

                # dynamically create factory class
                factory_name = name + "Factory"
                factory = eval(factory_name)()

                # add factory to factories
                self.factories[name] = factory

    def make_drink(self):
        print("Available drinks:")
        for drink_name in self.factories.keys():
            print(drink_name)

        # get user input for drink and amount
        s = input(f"Please pick a drink (0-{len(self.factories)-1}): ")
        drink_name = list(self.factories.keys())[int(s)]
        s = input(f"Specify amount: ")
        amount = int(s)

        # use factory to make drink
        factory = self.factories[drink_name]
        print(f"Making {drink_name}...")
        return factory.prepare(amount)


if __name__ == "__main__":
    # entry = input("What kind of drink would you like? ")
    # drink = make_drink(entry)
    # if drink:
    #     drink.consume()

    hdm = HotDrinkMachine()
    hdm.make_drink()
