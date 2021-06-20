# In this section, we look at how to overcome some of the issues
# of the Singleton pattern, that led many to consider the pattern
# a "code smell". Using the metaclass implementation:
import unittest
from dataclasses import dataclass, field
from typing import ClassVar, Dict


class Singleton(type):
    _instances: ClassVar[Dict] = {}

    def __call__(cls, *args, **kwargs):
        # check cls is in set of _instances
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(cls, *args, **kwargs)
        return cls._instances[cls]


class Database(metaclass=Singleton):
    population: Dict

    def __init__(self, *args, **kwargs):
        self.population = {}
        with open("capitals.txt", "r") as f:
            lines = f.readlines()
            for i in range(0, len(lines), 2):
                self.population[lines[i].strip()] = int(lines[i + 1].strip())

    def get_population(self, name):
        return self.population[name]


# Some high-level module that uses the Database


class SingletonRecordFinder(object):
    def total_population(self, cities):
        result = 0
        for c in cities:
            result += Database().get_population(c)
        return result


# Create tests to test SingletonRecordFinder
class SingletonTests(unittest.TestCase):
    def test_is_singleton(self):
        # Setup
        db1 = Database()
        db2 = Database()

        # Assert
        self.assertEqual(db1, db2)

    def test_singleton_total_population(self):
        # Setup
        rf = SingletonRecordFinder()

        # Test
        result = rf.total_population(cities=["Seoul", "Mexico City"])

        # Assert
        # Note: SingletonRecordFinder is coupled to Database which uses live data.
        # We have to get the expected values - this is a huge problem as it is both
        # dangerous in a production environment and makes our test code brittle
        self.assertEqual(17500000 + 17400000, result)


# Better approach


@dataclass
class ConfigurableRecordFinder(object):
    db: Database = field(default_factory=Database)

    def total_population(self, cities):
        result = 0
        for c in cities:
            result += self.db.get_population(c)
        return result


# Improved testability


class DummyDatabase:
    population = {"alpha": 1, "beta": 2, "gamma": 3}

    def get_population(self, name):
        return self.population[name]


class ConfigurableRecordFinderTests(unittest.TestCase):
    ddb = DummyDatabase()

    def test_singleton_total_population(self):
        # Setup
        rf = ConfigurableRecordFinder(db=self.ddb)

        # Test
        result = rf.total_population(cities=["alpha", "beta"])

        # Assert
        # Note: SingletonRecordFinder is coupled to Database which uses live data.
        # We have to get the expected values - this is a huge problem as it is both
        # dangerous in a production environment and makes our test code brittle
        self.assertEqual(3, result)


if __name__ == "__main__":
    unittest.main()
