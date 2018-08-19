from sqlalchemy import Column, String
from sqlalchemy.orm import relationship

from db.mixins.scraper_item_mixin import ScraperItemMixin
from db.mixins.timestamp_mixin import TimestampMixin
from db.models.base import Base
from db.utils import StringNotNullColumn


class System(ScraperItemMixin, TimestampMixin, Base):
    name = Column(String, nullable=False)
    phone_number = StringNotNullColumn()
    email = StringNotNullColumn()
    timezone = StringNotNullColumn()
    url = StringNotNullColumn()
    language = StringNotNullColumn()

    stations = relationship("Station", back_populates="system")
