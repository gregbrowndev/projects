# Implementing the Single pattern using a decorator
def singleton(klass):
    instances = {}

    def get_instance(*args, **kwargs):
        if klass not in instances:
            instances[klass] = klass(*args, **kwargs)
        return instances[klass]

    return get_instance


@singleton
class Database(object):
    def __init__(self):
        print("Loading database")


if __name__ == "__main__":
    d1 = Database()
    d2 = Database()
    print(d1 == d2)
