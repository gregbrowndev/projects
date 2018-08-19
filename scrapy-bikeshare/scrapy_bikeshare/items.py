# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html
from typing import Optional, Dict
from datetime import datetime
from dateutil import parser

import attr
from cattr import Converter
from pendulum.tz.zoneinfo import Timezone


def init_converter():
    converter = Converter()
    # converter.register_unstructure_hook(pendulum.DateTime, lambda dt: dt.to_iso8601_string())
    # converter.register_structure_hook(pendulum.DateTime, lambda ts, _: pendulum.parse(ts))
    converter.register_unstructure_hook(datetime, lambda dt: dt.isoformat() + 'Z')
    converter.register_structure_hook(datetime, lambda ts, _: parser.parse(ts))
    return converter


class BaseItem(object):
    """ItemBase with cattr methods for (de)serialization
    """
    # cattr converter
    converter = init_converter()

    @classmethod
    def structure(cls, data: Dict) -> 'ItemBase':
        return cls.converter.structure(data, cls)

    def unstructure(self) -> Dict:
        return self.converter.unstructure(self)


@attr.s(auto_attribs=True)
class SystemItem(BaseItem):
    name: str
    source_id: Optional[str] = None
    phone_number: Optional[str] = None
    email: Optional[str] = None
    timezone: Optional[str] = None
    url: Optional[str] = None
    language: Optional[str] = None


@attr.s(auto_attribs=True)
class StationItem(BaseItem):
    name: str
    latitude: float
    longitude: float
    address: Optional[str] = None
    source_id: Optional[str] = None
    capacity: Optional[int] = None
    bikes_available: Optional[int] = None
    docks_available: Optional[int] = None
    bikes_disabled: Optional[int] = None
    docks_disabled: Optional[int] = None
    open: Optional[bool] = None
