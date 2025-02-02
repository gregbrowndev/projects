from sqlalchemy import Column, Integer, Sequence
from sqlalchemy.ext.declarative import declarative_base, declared_attr
from sqlalchemy_repr import RepresentableBase


class PrimaryKeyedBase(RepresentableBase):
    """Custom Declarative Base passed to cls argument of declarative_base()

    Each table extending this base class will have:

    * a __repr__ method generated with class attributes (see RepresentableBase base class)
    * __tablename__ derived from the class name
    * a primary key column 'id'
    """

    @declared_attr
    def __tablename__(cls) -> str:
        return cls.__name__.lower()

    @declared_attr
    def id(cls) -> Column:
        # NOTE it seems that Sequence is being ignored by SqlAlchemy/Postgres
        sequence_name = f'{cls.__tablename__}_id_seq'
        return Column(Integer, Sequence(sequence_name), primary_key=True)


Base = declarative_base(cls=PrimaryKeyedBase)

