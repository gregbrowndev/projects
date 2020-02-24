# Single Responsibility Principle (SRP) or Separation of Concerns (SoC)


# The adding and removing of entries is a Journal's primary responsibility
class Journal(object):
    def __init__(self):
        self.entries = []
        self.count = 0

    def add_entry(self, text):
        self.count += 1
        self.entries.append(f"{self.count}: {text}")

    def remove_entry(self, pos):
        del self.entries[pos]

    def __str__(self):
        return "\n".join(self.entries)

    # BAD: violates SRP as we've added a secondary responsibility: persistence
    # def save(self, filename):
    #     with open(filename, 'w') as f:
    #         f.write(str(self))
    #
    # def load(self, filename):
    #     pass
    #
    # def load_from_web(self, uri):
    #     pass


# Create a separate class to handle persistence
class PersistenceManager(object):
    @staticmethod
    def save_to_file(journal, filename):
        with open(filename, "w") as f:
            f.write(str(journal))


if __name__ == "__main__":
    j = Journal()
    j.add_entry("I cried today.")
    j.add_entry("I ate a bug.")
    print(f"Journal entries:\n{j}")

    # save journal
    PersistenceManager.save_to_file(j, "journal.txt")
