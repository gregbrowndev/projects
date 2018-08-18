from sqlalchemy import Column, String

from db.mixins.scraper_item_mixin import ScraperItemMixin
from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base


class System(ScraperItemMixin, TimestampMixin, Base):
    name = Column(String, nullable=False)
    phone_number = Column(String, nullable=False, default='')
    email = Column(String, nullable=False, default='')
    timezone = Column(String, nullable=False, default='')
    url = Column(String, nullable=False, default='')
    language = Column(String, nullable=False, default='')


if __name__ == '__main__':
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker

    engine = create_engine('postgresql://postgres:mysecret@localhost:5432/scrapy-bikeshare', echo=True)
    Session = sessionmaker(bind=engine)

    session = Session()
    system = System(
        name='TfL Bikeshare',
        phone_number='0343 222 6666',
        timezone='Europe/London',
        url='https://tfl.gov.uk/modes/cycling/santander-cycles',
        language='en')

    try:
        session.add(system)
        session.commit()

        # query again
        obj = session.query(System).first()
        print(obj)
    except:
        session.rollback()
        raise
    finally:
        session.close()
