from typing import Optional

from sqlalchemy import Column, String
from sqlalchemy.orm import relationship, Session
from sqlalchemy.orm.exc import NoResultFound

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

    @classmethod
    def get_system(cls, session: Session, scraper_id: int, source_id: Optional[str]) -> Optional['System']:
        """
        Finds a System using scraper_id and source_id. Returns None if not found.

        This works for sources which only publish a single system, e.g. gbfs,
        where the source_id is null, as well as sources with multiple systems,
        where each system is usually given a source_id in the source.
        """
        return session.query(System) \
            .filter_by(
            scraper_id=scraper_id,
            source_id=source_id) \
            .one_or_none()
