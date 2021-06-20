# Builder Pattern for constructing a HTML document from text
from dataclasses import dataclass, field
from typing import ClassVar, List


@dataclass
class HtmlElement(object):
    indent_size: ClassVar[int] = 2
    name: str = field(default="")
    text: str = field(default="")
    elements: List["HtmlElement"] = field(default_factory=list)

    def __str__(self):
        return self.to_html(0)

    def to_html(self, indent: int = 0) -> str:
        lines = []
        i = " " * (indent * self.indent_size)
        lines.append(f"{i}<{self.name}>")

        if self.text:
            i1 = " " * ((indent + 1) * self.indent_size)
            lines.append(f"{i1}{self.text}")

        for e in self.elements:
            lines.append(e.to_html(indent + 1))

        lines.append(f"{i}</{self.name}>")
        return "\n".join(lines)

    @staticmethod
    def create(name: str) -> "HtmlBuilder":
        return HtmlBuilder(root_name=name)


@dataclass
class HtmlBuilder(object):
    root_name: str
    _root: HtmlElement = field(init=False)

    def __post_init__(self):
        self._root = HtmlElement(name=self.root_name)

    def __str__(self):
        return str(self._root)

    def add_child(self, child_name: str, child_text: str) -> "HtmlBuilder":
        self._root.elements.append(HtmlElement(name=child_name, text=child_text))
        return self  # Fluent API


if __name__ == "__main__":
    # building a HTML paragraph
    text = "hello"
    parts = ["<p>", text, "</p>"]
    print("".join(parts))

    # but this quickly becomes more complicated
    words = ["hello", "world"]
    parts = ["<ul>"]
    for w in words:
        parts.append(f"\t<li>{w}</li>")
    parts.append("</ul>")
    print("\n".join(parts))

    # Using HtmlElement without builder
    e = HtmlElement(
        name="ul",
        elements=[
            HtmlElement(text="Hello", name="li"),
            HtmlElement(text="World", name="li"),
        ],
    )
    print(f"Without using the Builder pattern:\n{e}")

    # Using builder
    builder = (
        HtmlElement.create("ul")  # note using factory method
        .add_child("li", "hello")
        .add_child("li", "there")
        .add_child("li", "world")
    )
    print(f"Using the Builder pattern:\n{builder}")
