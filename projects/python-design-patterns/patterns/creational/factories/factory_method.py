# Factory Method pattern
import enum
import math
from dataclasses import dataclass


class CoordinateSystem(enum.Enum):
    CARTESIAN = 1
    POLAR = 2


# BAD: constructor that supports cartesian and polar coordinate systems
# class Point(object):
# def __init__(self, a, b, system: CoordinateSystem = CoordinateSystem.CARTESIAN):
#     if system == CoordinateSystem.CARTESIAN:
#         self.x = a
#         self.y = b
#     elif system == CoordinateSystem.POLAR:
#         self.x = a * math.cos(b)
#         self.y = a * math.sin(b)


@dataclass
class Point(object):
    x: float
    y: float

    @staticmethod
    def new_cartesian_point(x: float, y: float) -> "Point":
        return Point(x, y)

    @staticmethod
    def new_polar_point(rho: float, theta: float) -> "Point":
        x = rho * math.cos(theta)
        y = rho * math.sin(theta)
        return Point(x, y)


if __name__ == "__main__":
    p = Point.new_cartesian_point(2, 3)
    p2 = Point.new_polar_point(1, 2)

    print(p)
    print(p2)
