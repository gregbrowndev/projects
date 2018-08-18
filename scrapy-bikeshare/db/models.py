from sqlalchemy import Column, Integer, String, Sequence, DateTime, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy_repr import RepresentableBase

Base = declarative_base(cls=RepresentableBase)


class System(Base):
    __tablename__ = 'system'

    id = Column(Integer, Sequence('user_id_seq'), primary_key=True)
    source_id = Column(String)
    name = Column(String)
    phone_number = Column(String)
    email = Column(String)
    timezone = Column(String)
    url = Column(String)
    language = Column(String)
    created = Column(DateTime, default=func.now())


if __name__ == '__main__':
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker

    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    Session = sessionmaker(bind=engine)

    session = Session()
    print(session)

    system = System(
        name='TfL Bikeshare',
        phone_number='0343 222 6666',
        timezone='Europe/London',
        url='https://tfl.gov.uk/modes/cycling/santander-cycles',
        language='en')
    session.add(system)

    session.close()
