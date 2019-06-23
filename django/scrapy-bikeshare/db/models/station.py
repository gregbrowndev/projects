from sqlalchemy import Column, Integer, String, Float, Boolean, ForeignKey
from sqlalchemy.orm import relationship

from db.mixins.scraper_item_mixin import ScraperItemMixin
from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base
from db.utils import StringNotNullColumn


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
