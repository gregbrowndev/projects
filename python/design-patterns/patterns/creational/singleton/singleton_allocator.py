# Implementing the Singleton pattern by overriding the allocator
import random
from typing import Optional, cast


class Database(object):
    _instance: Optional["Database"] = None

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = cast(Database, super().__new__(cls, *args, **kwargs))
        return cls._instance

    def __init__(self):
        id = random.randint(1, 101)
        print(f"Loading dataset from file: {id}")


if __name__ == "__main__":
    d1 = Database()
    d2 = Database()

    print(d1 == d2)
