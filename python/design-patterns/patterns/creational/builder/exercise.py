from unittest import TestCase


class Field(object):
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __str__(self):
        return "self.{} = {}".format(self.name, self.value)


class Klass(object):
    def __init__(self, name, fields=None):
        self.name = name
        if fields is None:
            fields = []
        self.fields = fields

    def __str__(self):
        lines = ["class {}:".format(self.name)]

        # create __init__
        if self.fields:
            lines.append("  def __init__(self):")
            for field in self.fields:
                lines.append("    {}".format(field))
        else:
            lines.append("  pass")

        return "\n".join(lines)

    def add_field(self, name, value):
        self.fields.append(Field(name, value))


class CodeBuilder:
    def __init__(self, root_name):
        self.klass = Klass(root_name)

    def add_field(self, name, value):
        self.klass.add_field(name, value)
        return self

    def __str__(self):
        return str(self.klass)


class Evaluate(TestCase):
    @staticmethod
    def preprocess(s=""):
        return s.strip().replace("\r\n", "\n")

    def test_empty(self):
        cb = CodeBuilder("Foo")
        self.assertEqual(self.preprocess(str(cb)), "class Foo:\n  pass")

    def test_person(self):
        cb = CodeBuilder("Person").add_field("name", '""').add_field("age", 0)
        self.assertEqual(
            self.preprocess(str(cb)),
            """class Person:
  def __init__(self):
    self.name = \"\"
    self.age = 0""",
        )


# if __name__ == "__main__":
#     cb = CodeBuilder("Person").add_field("name", '""').add_field("age", "0")
#     print(cb)
