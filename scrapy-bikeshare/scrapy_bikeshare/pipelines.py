# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://doc.scrapy.org/en/latest/topics/item-pipeline.html
from typing import Dict, Union

from scrapy import Spider
from scrapy.exceptions import DropItem
from sqlalchemy.orm import Session
from sqlalchemy.orm.exc import NoResultFound

from db.models.station import Station
from db.models.system import System
from db.utils import create_session, update_or_create
from scrapy_bikeshare.items import SystemItem, StationItem


class ScrapyBikesharePipeline(object):
    def __init__(self):
        print('INIT CALLED')
        self.session: Session = create_session()

    def close_spider(self, spider):
        self.session.close()

    def process_item(self, item: Dict, spider: Spider):
        item_type = item['item_type']
        if item_type == 'system':
            self.process_system(
                SystemItem.structure(item['data'])
            )
        elif item_type == 'station':
            self.process_station(
                StationItem.structure(item['data'])
            )

    def process_system(self, item: SystemItem):
        # Insert or Update
        instance, created = update_or_create(
            self.session,
            System,
            scraper_id=item.scraper_id,
            source_id=item.source_id,
            defaults={
                'name': item.name,
                'phone_number': item.phone_number,
                'email': item.email,
                'timezone': item.timezone,
                'url': item.url,
                'language': item.language
            }
        )
        return item

    def process_station(self, item: StationItem):
        # Need to link the system first
        source_system_id = item.source_system_id
        system = System.get_system(self.session, item.scraper_id, source_system_id)

        if not system:
            raise DropItem('No system was found for station: ', item)

        # Insert or Update
        instance, created = update_or_create(
            self.session,
            Station,
            system=system,
            scraper_id=item.scraper_id,
            source_id=item.source_id,
            defaults={
                'name': item.name,
                'latitude': item.latitude,
                'longitude': item.longitude,
                'address': item.address,
                'capacity': item.capacity,
                'bikes_available': item.bikes_available,
                'bikes_disabled': item.bikes_disabled,
                'docks_available': item.docks_available,
                'docks_disabled': item.docks_disabled,
                'open': item.open,
            }
        )
        return item

    def save(self, item: Union[System, Station]):
        try:
            self.session.add(item)
            self.session.commit()
        except Exception as e:
            self.session.rollback()
            raise DropItem
