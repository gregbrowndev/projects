# Factory Method pattern
import enum
import math
from dataclasses import dataclass


@dataclass
class Point(object):
    x: float
    y: float

    class PointFactory(object):
        @staticmethod
        def new_cartesian_point(x: float, y: float) -> "Point":
            return Point(x, y)

        @staticmethod
        def new_polar_point(rho: float, theta: float) -> "Point":
            x = rho * math.cos(theta)
            y = rho * math.sin(theta)
            return Point(x, y)

    factory = PointFactory()


if __name__ == "__main__":
    p = Point.factory.new_cartesian_point(2, 3)
    p2 = Point.factory.new_polar_point(1, 2)

    print(p)
    print(p2)
