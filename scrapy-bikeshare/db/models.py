from sqlalchemy import Column, Integer, String, Sequence, DateTime, func
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


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

    def __repr__(self):
        return f'<System(' \
               f'source_id=${self.source_id},' \
               f'name=${self.name}' \
               f'phone_number=${self.phone_number}' \
               f'email=${self.email}' \
               f'timezone=${self.timezone}' \
               f'url=${self.url}' \
               f'language=${self.language}' \
               f'created=${self.created}' \
               f')>'


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
