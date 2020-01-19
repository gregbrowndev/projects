# Implementing the Singleton pattern using a metaclass
from typing import Dict, ClassVar


class Singleton(type):
    _instances: ClassVar[Dict] = {}

    def __call__(cls, *args, **kwargs):
        # check cls is in set of _instances
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(cls, *args, **kwargs)

            # Print out dictionary
            print(cls._instances)

        return cls._instances[cls]


class Database(metaclass=Singleton):
    def __init__(self, *args, **kwargs):
        print("Loading database")


if __name__ == "__main__":
    d1 = Database()
    d2 = Database()
    print(d1 == d2)
