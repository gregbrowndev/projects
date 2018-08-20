from sqlalchemy import Column, String, Integer, ForeignKey, UniqueConstraint
from sqlalchemy.ext.declarative import declared_attr
from sqlalchemy.orm import relationship


class ScraperItemMixin(object):
    # Note source_id is nullable and defaults to null. This is to not violate the unique
    # constraint when no source_id is available.
    source_id = Column(String, default=None)

    @declared_attr
    def scraper_id(cls):
        return Column(Integer, ForeignKey("scraper.id", ondelete='CASCADE'), nullable=False)

    @declared_attr
    def scraper(cls):
        name_plural = cls.__tablename__ + 's'
        return relationship("Scraper", back_populates=name_plural)

    @declared_attr
    def __table_args__(cls):
        # Add unique constraint using the name of the mixed in class
        constraint_name = f'{cls.__tablename__}_scraper_id_source_id_key'
        return (UniqueConstraint('scraper_id', 'source_id', name=constraint_name),)

