import json

import attr
import scrapy
from scrapy.http import HtmlResponse

from scrapy_bikeshare.items import StationItem
from scrapy_bikeshare.models.gbfs import GbfsModel, StationInformationModel, StationStatusModel


class GbfsSpider(scrapy.Spider):
    name = 'gbfs'
    start_urls = [
        'https://gbfs.bcycle.com/bcycle_madison/gbfs.json'
    ]

    def parse(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = GbfsModel.parse(data)
        for feed in model.feeds:
            if feed.name == 'system_information':
                yield scrapy.Request(feed.url, callback=self.parse_system_information, meta={'feed_name': feed.name})
            elif feed.name == 'station_information':
                yield scrapy.Request(feed.url, callback=self.parse_station_information, meta={'feed_name': feed.name})

    def parse_system_information(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = StationInformationModel.parse(data)

    def parse_station_information(self, response: HtmlResponse):
        # TODO - need to employ request chaining or cache partial items, so station_status data is merged
        # see https://stackoverflow.com/questions/13910357/how-can-i-use-multiple-requests-and-pass-items-in-between-them-in-scrapy-python
        # ideas: is there RxJS forkJoin mechanism for Twisted? could then dispatch multiple requests and handle
        # both responses in the callback together

        data = json.loads(response.body_as_unicode())
        model = StationInformationModel.converter.structure(data, StationInformationModel)
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
        model = StationStatusModel.converter.structure(data, StationStatusModel)


if __name__ == '__main__':
    import requests

    # r = requests.get('https://gbfs.bcycle.com/bcycle_madison/gbfs.json')
    # model = GbfsModel.parse(r.json())
    # print(model)

    # r = requests.get('https://gbfs.bcycle.com/bcycle_madison/system_information.json')
    # model = SystemInformationModel.parse(r.json())
    # print(model)

    # r = requests.get('https://gbfs.bcycle.com/bcycle_madison/station_information.json')
    # model = StationInformationModel.parse(r.json())
    # print(model)

    r = requests.get('https://gbfs.bcycle.com/bcycle_madison/station_status.json')
    model = StationStatusModel.parse(r.json())
    print(model)
