from sqlalchemy import Column, String
from sqlalchemy.orm import relationship

from db.mixins.scraper_item_mixin import ScraperItemMixin
from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base
from db.utils import create_session, StringNotNullColumn


class System(ScraperItemMixin, TimestampMixin, Base):
    name = Column(String, nullable=False)
    phone_number = StringNotNullColumn()
    email = StringNotNullColumn()
    timezone = StringNotNullColumn()
    url = StringNotNullColumn()
    language = StringNotNullColumn()

    stations = relationship("Station", back_populates="system")


if __name__ == '__main__':
    session = create_session()

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
