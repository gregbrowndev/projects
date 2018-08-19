# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://doc.scrapy.org/en/latest/topics/item-pipeline.html
from typing import Dict, Union

from scrapy import Spider
from scrapy.exceptions import DropItem
from sqlalchemy.orm import Session

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
        # elif item_type == 'station':
        #     self.process_station(
        #         StationItem.structure(item['data'])
        #     )

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
        # instance: System = self.session.query(System).filter_by(source_id=item.source_id).one_or_none()
        # if instance:
        #     print('UPDATING')
        #     instance.name = item.name
        #     instance.phone_number = item.phone_number
        #     instance.email = item.email
        #     instance.timezone = item.timezone
        #     instance.url = item.url
        #     instance.language = item.language
        # else:
        #     print('INSERTING')
        #     system = System(
        #         scraper_id=item.scraper_id,
        #         source_id=item.source_id,
        #         name=item.name,
        #         phone_number=item.phone_number,
        #         email=item.email,
        #         timezone=item.timezone,
        #         url=item.url,
        #         language=item.language
        #     )
        #     self.session.add(system)
        # self.session.commit()
        return item

    def process_station(self, item: StationItem):
        return item

    def save(self, item: Union[System, Station]):
        try:
            self.session.add(item)
            self.session.commit()
        except Exception as e:
            self.session.rollback()
            raise DropItem
