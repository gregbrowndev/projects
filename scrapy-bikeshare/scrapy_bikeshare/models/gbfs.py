import enum
from typing import List, Optional, Dict

import attr
from cattr import Converter
import pendulum


def init_converter():
    converter = Converter()
    converter.register_unstructure_hook(pendulum.DateTime, lambda dt: dt.to_iso8601_string())
    converter.register_structure_hook(pendulum.DateTime, lambda ts, _: pendulum.parse(ts))
    return converter


@attr.s(auto_attribs=True)
class GbfsBaseModel(object):
    ttl: int
    last_updated: int

    # cattr converter
    converter = init_converter()

    @classmethod
    def parse(cls, data: Dict) -> 'GbfsBaseModel':
        return cls.converter.structure(data, cls)


@attr.s(auto_attribs=True)
class GbfsModel(GbfsBaseModel):
    @attr.s(auto_attribs=True)
    class DataModel(object):
        @attr.s(auto_attribs=True)
        class FeedsModel(object):
            @attr.s(auto_attribs=True)
            class FeedModel(object):
                url: str
                name: str
            feeds: List[FeedModel]
        en: FeedsModel
        # may have to add Optional[FeedsModel] for other languages
    data: DataModel

    def __attrs_post_init__(self):
        # alias
        self.feeds = self.data.en.feeds


@attr.s(auto_attribs=True)
class SystemInformationModel(GbfsBaseModel):
    @attr.s(auto_attribs=True)
    class DataModel(object):
        system_id: str
        name: str
        language: str
        timezone: str
        short_name: Optional[str] = None
        operator: Optional[str] = None
        url: Optional[str] = None
        purchase_url: Optional[str] = None
        start_date: Optional[str] = None
        phone_number: Optional[str] = None
        email: Optional[str] = None
        license_url: Optional[str] = None
    data: DataModel


@enum.unique
class RentalMethod(enum.Enum):
    KEY = 'KEY'
    CREDIT_CARD = 'CREDITCARD'
    PAY_PASS = 'PAYPASS'
    APPLE_PAY = 'APPLEPAY'
    ANDROID_PAY = 'ANDROIDPAY'
    TRANSIT_CARD = 'TRANSITCARD'
    PHONE = 'PHONE'


@attr.s(auto_attribs=True)
class StationInformationModel(GbfsBaseModel):
    @attr.s(auto_attribs=True)
    class DataModel(object):
        @attr.s(auto_attribs=True)
        class StationModel(object):
            station_id: str
            name: str
            lat: float
            lon: float
            capacity: Optional[int] = None
            address: Optional[str] = None
            cross_street: Optional[str] = None
            region_id: Optional[str] = None
            post_code: Optional[str] = None
            rental_methods: Optional[RentalMethod] = None
        stations: List[StationModel]
    data: DataModel

    def __attrs_post_init__(self):
        # alias
        self.stations = self.data.stations


@attr.s(auto_attribs=True)
class StationStatusModel(GbfsBaseModel):
    @attr.s(auto_attribs=True)
    class DataModel(object):
        @attr.s(auto_attribs=True)
        class StationModel(object):
            station_id: str
            num_bikes_available: int
            num_docks_available: int
            is_installed: int
            is_renting: int
            is_returning: int
            last_reported: int
            num_bikes_disabled: Optional[int] = None
            num_docks_disabled: Optional[int] = None
        stations: List[StationModel]
    data: DataModel

    def __attrs_post_init__(self):
        # alias
        self.stations = self.data.stations
