from sqlalchemy import Column, String
from sqlalchemy.orm import relationship

from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base


class Scraper(TimestampMixin, Base):
    name = Column(String, nullable=False, unique=True)
    systems = relationship("System", back_populates="scraper")
    stations = relationship("Station", back_populates="scraper")
