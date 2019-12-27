# Interface Segregation Principle (ISP)
from typing import Protocol


class Machine(Protocol):
    def print(self, document):
        ...

    def scan(self, document):
        ...

    def fax(self, document):
        ...


class MultiFunctionPrinter(Machine):
    def print(self, document):
        pass

    def scan(self, document):
        pass

    def fax(self, document):
        pass


# So far things look OK but the problems arise when we have a class
# which cannot support the interface entirely
class OldFashionedPrinter(Machine):
    def print(self, document):
        # OK we can print
        pass

    def scan(self, document):
        """Not supported"""
        pass  # noop

    def fax(self, document):
        """Not supported"""
        raise NotImplementedError("Printer cannot scan!")


# A better solution which adheres to ISP is to split the interface up
class Printer(Protocol):
    def print(self, document):
        ...


class Scanner(Protocol):
    def scan(self, document):
        ...


class Faxer(Protocol):
    def fax(self, document):
        ...


class MultiFunctionDevice(Printer, Scanner, Faxer, Protocol):
    pass


# concrete class
class PhotoCopier(Printer, Scanner):
    def print(self, document):
        pass

    def scan(self, document):
        pass
