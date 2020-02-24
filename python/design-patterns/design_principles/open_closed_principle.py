# Open-Closed Principle (OCP)

import enum
import operator
from dataclasses import dataclass
from typing import Iterable, TypeVar, Protocol, Any, Callable


class Colour(enum.Enum):
    RED = 1
    GREEN = 2
    BLUE = 3


class Size(enum.Enum):
    SMALL = 1
    MEDIUM = 2
    LARGE = 3


@dataclass
class Product(object):
    name: str
    size: Size
    colour: Colour


# Creating a product filter which doesn't adhere to the Open-Closed Principle
class ProductFilter(object):
    @staticmethod
    def filter_by_colour(
        products: Iterable[Product], colour: Colour
    ) -> Iterable[Product]:
        for p in products:
            if p.colour == colour:
                yield p

    @staticmethod
    def filter_by_size(products: Iterable[Product], size: Size) -> Iterable[Product]:
        for p in products:
            if p.size == size:
                yield p

    @staticmethod
    def filter_by_colour_and_size(
        products: Iterable[Product], colour: Colour, size: Size
    ) -> Iterable[Product]:
        for p in products:
            if p.colour == colour and p.size == size:
                yield p


# Enterprise Design Pattern: Specification


# Generic protocols
T = TypeVar("T")
T_co = TypeVar("T_co", covariant=True)
T_contra = TypeVar("T_contra", contravariant=True)


class Specification(Protocol[T_contra]):
    def is_satisfied(self, item: T_contra) -> bool:
        ...

    def __add__(
        self: "Specification[T]", other: "Specification[T]"
    ) -> "Specification[T]":
        # Some syntax sugar to create an AndSpecification
        return AndSpecification(self, other)

    def __or__(
        self: "Specification[T]", other: "Specification[T]"
    ) -> "Specification[T]":
        # Some syntax sugar to create an OrSpecification
        return OrSpecification(self, other)


# Concrete specs
class ColourSpecification(Specification[Product]):
    def __init__(self, colour: Colour):
        self.colour = colour

    def is_satisfied(self, item: Product) -> bool:
        return item.colour == self.colour


class SizeSpecification(Specification[Product]):
    def __init__(self, size: Size):
        self.size = size

    def is_satisfied(self, item: Product) -> bool:
        return item.size == self.size


# Combinators
class CombinatorSpecification(Specification[T_contra]):
    def __init__(
        self,
        op: Callable[[bool, bool], bool],
        spec1: Specification[T_contra],
        spec2: Specification[T_contra],
    ):
        self.op = op
        self.spec1 = spec1
        self.spec2 = spec2

    def is_satisfied(self, item: T_contra) -> bool:
        return self.op(self.spec1.is_satisfied(item), self.spec2.is_satisfied(item))


class AndSpecification(CombinatorSpecification[T_contra]):
    def __init__(self, spec1: Specification[T_contra], spec2: Specification[T_contra]):
        super().__init__(operator.and_, spec1, spec2)


class OrSpecification(CombinatorSpecification[T_contra]):
    def __init__(self, spec1: Specification[T_contra], spec2: Specification[T_contra]):
        super().__init__(operator.or_, spec1, spec2)


# Improved Filter class
class Filter(Protocol[T]):
    def filter(self, items: Iterable[T], spec: Specification[T]) -> Iterable[T]:
        ...


class BetterFilter(Filter[Product]):
    def filter(
        self, items: Iterable[Product], spec: Specification[Product]
    ) -> Iterable[Product]:
        for item in items:
            if spec.is_satisfied(item):
                yield item


if __name__ == "__main__":
    # Example usage
    apple = Product("Apple", colour=Colour.GREEN, size=Size.SMALL)
    tree = Product("Tree", colour=Colour.GREEN, size=Size.LARGE)
    house = Product("House", colour=Colour.BLUE, size=Size.LARGE)

    products = [apple, tree, house]

    # Filtering the list of products the old way
    pf = ProductFilter()
    print("Green products (old):")
    for p in pf.filter_by_colour(products, Colour.GREEN):
        # Not DRY and doesn't adhere to OCP
        print(f" - {p.name} is green")

    # Using Specification pattern which adheres to the Open-Closed Principle
    bf = BetterFilter()

    print("Green products (new):")
    green_spec = ColourSpecification(Colour.GREEN)
    for p in bf.filter(products, green_spec):
        print(f" - {p.name} is green")

    print("Large products (new):")
    large_spec = SizeSpecification(Size.LARGE)
    for p in bf.filter(products, large_spec):
        print(f" - {p.name} is large")

    # We create a 'Combinator' to combine specs
    print("Large green or blue products (new):")
    blue_spec = ColourSpecification(Colour.BLUE)
    advanced_spec = large_spec and (green_spec or blue_spec)
    for p in bf.filter(products, advanced_spec):
        print(f" - {p.name} is {p.size.name} and {p.colour.name}")
