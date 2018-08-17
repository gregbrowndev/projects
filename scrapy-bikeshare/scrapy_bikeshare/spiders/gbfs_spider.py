import json

import pendulum
import scrapy
import attr
from cattr import Converter
from pendulum import DateTime
from scrapy.http import HtmlResponse

from scrapy_bikeshare.items import StationItem
from scrapy_bikeshare.models.gbfs import GbfsModel, SystemInformationModel, StationInformationModel, StationStatusModel

converter = Converter()
converter.register_unstructure_hook(DateTime, lambda dt: dt.to_iso8601_string())
converter.register_structure_hook(DateTime, lambda ts, _: pendulum.parse(ts))


class GbfsSpider(scrapy.Spider):
    name = 'gbfs'
    start_urls = [
        'https://gbfs.bcycle.com/bcycle_madison/gbfs.json'
    ]

    def parse(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model: GbfsModel = converter.structure(data, GbfsModel)
        for feed in model.data.en.feeds:
            if feed.name == 'system_information':
                yield scrapy.Request(feed.url, callback=self.parse_system_information, meta={'feed_name': feed.name})
            elif feed.name == 'station_information':
                yield scrapy.Request(feed.url, callback=self.parse_station_information, meta={'feed_name': feed.name})

    def parse_system_information(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = converter.structure(data, SystemInformationModel)

    def parse_station_information(self, response: HtmlResponse):
        # TODO - need to employ request chaining or cache partial items, so station_status data is merged
        # see https://stackoverflow.com/questions/13910357/how-can-i-use-multiple-requests-and-pass-items-in-between-them-in-scrapy-python
        # ideas: is there RxJS forkJoin mechanism for Twisted? could then dispatch multiple requests and handle
        # both responses in the callback together

        data = json.loads(response.body_as_unicode())
        model = converter.structure(data, StationInformationModel)
        for station in model.data.stations:
            item = attr.asdict(StationItem(
                source_id=station.station_id,
                name=station.name,
                address=station.address,
                latitude=station.lat,
                longitude=station.lon,
                capacity=station.capacity
            ))
            yield item

    def parse_station_status(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = converter.structure(data, StationStatusModel)


if __name__ == '__main__':
    import requests

    r = requests.get('https://gbfs.bcycle.com/bcycle_madison/gbfs.json')
    model: GbfsModel = converter.structure(r.json(), GbfsModel)
    print(model)
