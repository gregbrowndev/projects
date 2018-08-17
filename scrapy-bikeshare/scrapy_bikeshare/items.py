# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html
from typing import Optional

import attr


@attr.s(auto_attribs=True)
class SystemItem(object):
    name: str

    source_id: Optional[str] = None
    phone_number: Optional[str] = None
    email: Optional[str] = None
    timezone: Optional[str] = None
    url: Optional[str] = None
    language: Optional[str] = None


@attr.s(auto_attribs=True)
class StationItem(object):
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
