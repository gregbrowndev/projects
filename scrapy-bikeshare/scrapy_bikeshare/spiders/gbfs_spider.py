import json
from typing import Optional

import attr
import scrapy
from scrapy.http import HtmlResponse

from scrapy_bikeshare.items import StationItem, SystemItem
from scrapy_bikeshare.models.gbfs import GbfsModel, StationInformationModel, StationStatusModel, SystemInformationModel

class GbfsSpider(scrapy.Spider):
    name = 'gbfs'
    start_urls = [
        'https://gbfs.bcycle.com/bcycle_madison/gbfs.json'
    ]

    def parse(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = GbfsModel.parse(data)
        self.feeds = {feed.name: feed.url for feed in model.feeds}
        for name, url in self.feeds.items():
            if name == 'system_information':
                yield scrapy.Request(url, callback=self.parse_system_information)
            elif name == 'station_information':
                yield scrapy.Request(url, callback=self.parse_station_information)

    def parse_system_information(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        system = SystemInformationModel.parse(data).data
        # yield attr.asdict(SystemItem(
        #     name=system.name,
        #     source_id=system.system_id,
        #     phone_number=system.phone_number,
        #     email=system.email,
        #     timezone=system.timezone,
        #     url=system.url,
        #     language=system.language
        # ))

    def parse_station_information(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        model = StationInformationModel.converter.structure(data, StationInformationModel)
        # chain station status request
        # see https://stackoverflow.com/questions/13910357/how-can-i-use-multiple-requests-and-pass-items-in-between-them-in-scrapy-python
        # TODO is there RxJS forkJoin mechanism for Twisted? could then dispatch multiple requests and await responses
        yield scrapy.Request(self.feeds['station_status'],
                             callback=self.parse_station_status,
                             meta={'station_information': model})

    def parse_station_status(self, response: HtmlResponse):
        data = json.loads(response.body_as_unicode())
        status_model = StationStatusModel.parse(data)
        statuses = {station.station_id: station for station in status_model.stations}

        # Get previous model
        information_model: StationInformationModel = response.meta['station_information']

        for station in information_model.stations:
            station_id = station.station_id
            status: Optional[StationStatusModel.DataModel.StationModel] = statuses.get(station_id, None)

            bikes_available = None
            docks_available = None
            bikes_disabled = None
            docks_disabled = None
            is_open = None

            if status:
                bikes_available = status.num_bikes_available
                docks_available = status.num_docks_available
                bikes_disabled = status.num_bikes_disabled
                docks_disabled = status.num_docks_disabled
                is_open = status.is_renting

            yield attr.asdict(StationItem(
                source_id=station_id,
                name=station.name,
                address=station.address,
                latitude=station.lat,
                longitude=station.lon,
                capacity=station.capacity,
                bikes_available=bikes_available,
                docks_available=docks_available,
                bikes_disabled=bikes_disabled,
                docks_disabled=docks_disabled,
                open=is_open
            ))


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
