# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html
from enum import Enum

import scrapy


class Reason(Enum):
    UNKNOWN = 'UNKNOWN'
    OTHER = 'OTHER'
    ROAD_WORKS = 'ROAD_WORKS'
    ROAD_CLOSED = 'ROAD_CLOSED'
    WEATHER = 'WEATHER'
    PLANNED_EVENT = 'PLANNED_EVENT'
    UNPLANNED_EVENT = 'UNPLANNED_EVENT'
    MAINTENANCE = 'MAINTENANCE'
    TECHNICAL_PROBLEM = 'TECHNICAL_PROBLEM'
    DEMONSTRATION = 'DEMONSTRATION'
    ACCIDENT = 'ACCIDENT'
    HOLIDAY = 'HOLIDAY'
    CONSTRUCTION = 'CONSTRUCTION'
    EMERGENCY_ENVIRONMENT = 'EMERGENCY_ENVIRONMENT'
    POLICE_ACTIVITY = 'POLICE_ACTIVITY'
    MEDICAL_ACTIVITY = 'MEDICAL_ACTIVITY'


class Effect(Enum):
    UNKNOWN = 'UNKNOWN'
    OTHER = 'OTHER'
    NO_SERVICE = 'NO_SERVICE'
    REDUCED_SERVICE = 'REDUCED_SERVICE'
    ADDITIONAL_SERVICE = 'ADDITIONAL_SERVICE'
    MODIFIED_SERVICE = 'MODIFIED_SERVICE'
    DELAYS = 'DELAYS'
    DIVERSION = 'DIVERSION'
    ACCESSIBILITY_REDUCED = 'ACCESSIBILITY_REDUCED'
    STOP_MOVED = 'STOP_MOVED'


class SituationItem(scrapy.Item):
    # meta data
    created = scrapy.Field()
    source_type = scrapy.Field()
    source_raw_content = scrapy.Field()
    source_location = scrapy.Field()

    # situation data
    source_id = scrapy.Field()
    source_created = scrapy.Field()
    source_updated = scrapy.Field()
    title = scrapy.Field()
    description = scrapy.Field()
    url = scrapy.Field()
    planned = scrapy.Field()  # boolean
    reason = scrapy.Field()
    effect = scrapy.Field()

    # ?
    is_public = scrapy.Field()  # boolean
    detail = scrapy.Field()
    validity_period = scrapy.Field()
    publication_validity_period = scrapy.Field()

    # custom
    affected_services = scrapy.Field()
