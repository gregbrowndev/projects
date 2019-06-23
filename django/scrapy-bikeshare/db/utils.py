from typing import Dict, Tuple

from sqlalchemy import create_engine, Column, String
from sqlalchemy.ext.declarative import DeclarativeMeta
from sqlalchemy.orm import Session
from sqlalchemy.orm import sessionmaker
from sqlalchemy.sql import ClauseElement


def create_session() -> Session:
    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    session = sessionmaker(bind=engine)()
    return session


def get_or_create(session: Session, model: DeclarativeMeta, defaults: Dict = None, **kwargs) \
        -> Tuple[DeclarativeMeta, bool]:
    instance = session.query(model).filter_by(**kwargs).one_or_none()
    if instance:
        return instance, False
    else:
        params = {k: v for k, v in kwargs.items() if not isinstance(v, ClauseElement)}
        params.update(defaults or {})
        instance = create(session, model, params)
        return instance, True


def update_or_create(session: Session, model: DeclarativeMeta, defaults: Dict = None, **kwargs) \
        -> Tuple[DeclarativeMeta, bool]:
    instance, created = get_or_create(session, model, defaults=defaults, **kwargs)
    if not created:
        # update
        params = {k: v for k, v in kwargs.items() if not isinstance(v, ClauseElement)}
        params.update(defaults or {})
        instance = update(session, instance, params)
    return instance, created


def create(session: Session, model: DeclarativeMeta, params: Dict) -> DeclarativeMeta:
    instance = model(**params)
    try:
        session.add(instance)
        session.commit()
    except Exception as e:
        session.rollback()
        raise e
    return instance


def update(session: Session, instance: DeclarativeMeta, params: Dict) -> DeclarativeMeta:
    for key, value in params.items():
        setattr(instance, key, value)
    try:
        session.commit()
    except Exception as e:
        session.rollback()
        raise e
    return instance


class StringNotNullColumn(Column):
    def __init__(self, *args, **kwargs):
        super().__init__(String, nullable=False, default='', *args, **kwargs)
