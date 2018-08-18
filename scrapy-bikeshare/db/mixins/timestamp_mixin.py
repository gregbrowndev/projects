from sqlalchemy import Column, DateTime, func


class TimestampMixin(object):
    created = Column(DateTime, default=func.now())
