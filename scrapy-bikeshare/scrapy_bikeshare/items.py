# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import attr


@attr.s
class SystemItem(object):
    name = attr.ib()

    source_id = attr.ib()
    phone_number = attr.ib()
    email = attr.ib()
    timezone = attr.ib()
    url = attr.ib()
    language = attr.ib()


@attr.s
class StationItem(object):
    name = attr.ib()
    address = attr.ib()
    source_id = attr.ib()
    latitude = attr.ib(type=float)
    longitude = attr.ib(type=float)
    bikes_available = attr.ib(type=int)
    docks_available = attr.ib(type=int)
    open = attr.ib(type=bool)