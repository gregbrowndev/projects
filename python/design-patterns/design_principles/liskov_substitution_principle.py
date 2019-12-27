# Liskov Substitution Principle (LSP)


class Rectangle(object):
    def __init__(self, width: int, height: int):
        self._width = width
        self._height = height

    @property
    def width(self) -> int:
        return self._width

    @width.setter
    def width(self, value: int) -> None:
        self._width = value

    @property
    def height(self) -> int:
        return self._height

    @height.setter
    def height(self, value: int) -> None:
        self._height = value

    @property
    def area(self):
        return self.width * self.height

    def __repr__(self):
        return f"Rectangle(width={self.width!r}, height={self.height!r})"


class Square(Rectangle):
    def __init__(self, size):
        super().__init__(size, size)

    @Rectangle.width.setter
    def width(self, value) -> None:
        self._width = self._height = value

    @Rectangle.height.setter
    def height(self, value) -> None:
        self._height = self._width = value


def use_rect(rc: Rectangle):
    w = rc.width
    rc.height = 10
    expected = int(w * 10)
    print(f"Expected an area of {expected}, got {rc.area}")


if __name__ == "__main__":
    rc = Rectangle(2, 3)
    use_rect(rc)

    # breaking LSP
    sq = Square(5)
    use_rect(sq)
