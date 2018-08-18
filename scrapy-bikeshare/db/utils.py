from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, Session


def create_session() -> Session:
    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    Session_ = sessionmaker(bind=engine)
    return Session_()
