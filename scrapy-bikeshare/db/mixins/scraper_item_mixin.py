from sqlalchemy import Column, String


class ScraperItemMixin(object):
    # Note source_id is nullable and defaults to null. This allows unique constraints to used properly.
    source_id = Column(String, default=None)
