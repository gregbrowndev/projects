from sqlalchemy import Column, Integer, String, Float, Boolean, ForeignKey
from sqlalchemy.orm import relationship

from db.mixins.scraper_item_mixin import ScraperItemMixin
from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base
from db.utils import create_session, StringNotNullColumn


# NOTE - could use GeoAlchemy 2 to interface with PostGIS
class Station(ScraperItemMixin, TimestampMixin, Base):
    name = Column(String, nullable=False)
    latitude = Column(Float, nullable=False)
    longitude = Column(Float, nullable=False)
    address = StringNotNullColumn()
    capacity = Column(Integer)
    bikes_available = Column(Integer)
    docks_available = Column(Integer)
    bikes_disabled = Column(Integer)
    docks_disabled = Column(Integer)
    open = Column(Boolean)

    system_id = Column(Integer, ForeignKey("system.id", ondelete='CASCADE'), nullable=False)
    system = relationship("System", back_populates="stations")


if __name__ == '__main__':
    from db.models.system import System

    session = create_session()

    system = session.query(System).first()
    print(system)

    station = Station(
        name='Station 1',
        latitude=0,
        longitude=0,
        system=system
    )

    try:
        session.commit()

        # query again
        obj = session.query(Station).first()
        print(obj)
    except Exception as e:
        print('ROLLING BACK: ', e)
        session.rollback()
        raise
    finally:
        session.close()
