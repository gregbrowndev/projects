from sqlalchemy import create_engine, Column, String
from sqlalchemy.orm import sessionmaker, Session


def create_session() -> Session:
    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    Session_ = sessionmaker(bind=engine)
    return Session_()


class StringNotNullColumn(Column):
    def __init__(self, *args, **kwargs):
        super().__init__(String, nullable=False, default='', *args, **kwargs)
