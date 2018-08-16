import json

import pendulum
import scrapy
from cattr import Converter
from pendulum import DateTime
from scrapy.http import HtmlResponse

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
            if feed.name in ['system_information', 'station_information', 'station_status']:
                request = scrapy.Request(feed.url, callback=self.parse_feed)
                request.meta['feed_name'] = feed.name
                yield request

    def parse_feed(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        feed_name = response.meta['feed_name']
        if feed_name == 'system_information':
            model = converter.structure(data, SystemInformationModel)
            self.parse_system_information(response, model)
        elif feed_name == 'station_information':
            model = converter.structure(data, StationInformationModel)
            self.parse_station_information(response, model)
        elif feed_name == 'station_status':
            model = converter.structure(data, StationStatusModel)
            self.parse_station_status(response, model)

    def parse_system_information(self, response: HtmlResponse, model: SystemInformationModel):
        print(model)
        print()

    def parse_station_information(self, response: HtmlResponse, model: StationInformationModel):
        print(model.data.stations[0])
        print()

    def parse_station_status(self, response: HtmlResponse, model: StationStatusModel):
        print(model.data.stations[0])
        print()


if __name__ == '__main__':
    import requests

    r = requests.get('https://gbfs.bcycle.com/bcycle_madison/gbfs.json')
    model: GbfsModel = converter.structure(r.json(), GbfsModel)
    print(model)
