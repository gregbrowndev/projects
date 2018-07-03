import json
from datetime import datetime, timezone
from dateutil import parser, tz
import scrapy
from bs4 import BeautifulSoup

from disruptions.items import SituationItem
from disruptions.utils import get_text


class PhillySpider(scrapy.Spider):
    name = 'philly'
    start_urls = [
        'http://www3.septa.org/hackathon/Alerts/get_alert_data.php?req1=all'
    ]

    def __init__(self, name=None, **kwargs):
        super().__init__(name=None, **kwargs)
        self.now = datetime.now(timezone.utc)
        self.TZ = tz.gettz('America/New_York')

    def parse(self, response):
        data = json.loads(response.body_as_unicode())

        # store the title as the key
        detours = {}
        advisories = {}

        for item in data:
            # parse detour
            if item['detour_message']:
                title = get_text(item['detour_message'])
                route_id = item['route_id']

                # Get route name (and type)
                route = '{route} ({route_type})'\
                    .format(route=item['route_name'], route_type=self.get_route_type(route_id))

                detour = detours.get(title, None)
                if detour is not None:
                    # Add route to affected services
                    detour['affected_services'].append(route)
                    continue

                description = 'Start location: {start_location}\n' \
                              'Detour reason: {detour_reason}'.format(
                    start_location=item['detour_start_location'],
                    detour_reason=item['detour_reason'],
                )

                start_date = self.get_philly_timestamp(item['detour_start_date_time'])
                end_date = self.get_philly_timestamp(item['detour_end_date_time'])

                detours[title] = SituationItem({
                    'created': self.now.isoformat(),
                    'source_type': 'JSON',
                    'title': title,
                    'description': description,
                    'affected_services': [route,],
                    'validity_period': {
                        'start': start_date.isoformat(),
                        'finish': end_date.isoformat()
                    }
                })

        for detour in detours.values():
            yield detour

    def get_route_type(self, route_id):
        lookup = {
            'bus_route': 'Bus',
            'rr_route': 'Regional Rail Line',
            'trolley_route': 'Trolley',
            'cct': 'CCT'
        }
        for route_type in lookup.keys():
            if (route_type in route_id):
                return lookup[route_type]
        return None

    def get_philly_timestamp(self, timestamp):
        return parser.parse(timestamp)\
            .replace(tzinfo=self.TZ)