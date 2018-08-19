from typing import Dict, Tuple

from sqlalchemy import create_engine, Column, String
from sqlalchemy.ext.declarative import DeclarativeMeta
from sqlalchemy.orm import Session
from sqlalchemy.orm import sessionmaker
from sqlalchemy.sql import ClauseElement


def create_session() -> Session:
    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    Session_ = sessionmaker(bind=engine)
    return Session_()


def get_or_create(session: Session, model: DeclarativeMeta, defaults: Dict = None, **kwargs: Dict) \
        -> Tuple[DeclarativeMeta, bool]:
    instance = session.query(model).filter_by(**kwargs).one_or_none()
    if instance:
        return instance, False
    else:
        params = {k: v for k, v in kwargs.items() if not isinstance(v, ClauseElement)}
        params.update(defaults or {})
        try:
            instance = model(**params)
            session.add(instance)
            session.commit()
        except Exception as e:
            session.rollback()
            raise e
        return instance, True


class StringNotNullColumn(Column):
    def __init__(self, *args, **kwargs):
        super().__init__(String, nullable=False, default='', *args, **kwargs)
