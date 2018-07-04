import json
import re
from datetime import datetime, timezone
from dateutil import parser, tz
import scrapy
from bs4 import BeautifulSoup

from disruptions.items import SituationItem, Reason, Effect
from disruptions.utils import get_text, clean


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

        # store lookups of title-description hash to detour/advisory
        detours = {}
        advisories = {}

        for item in data:
            # parse advisory
            if item['advisory_message']:
                self.parse_advisory_item(item, advisories)

            # parse detour
            if item['detour_message']:
                self.parse_detour_item(item, detours)

        for detour in detours.values():
            affected_services = sorted(set(detour['affected_services']))
            detour['affected_services'] = affected_services
            yield detour

        for advisory in advisories.values():
            affected_services = sorted(set(advisory['affected_services']))
            advisory['affected_services'] = affected_services
            yield advisory

    def parse_advisory_item(self, item, advisories):
        advisory_message = BeautifulSoup(item['advisory_message'], 'lxml')
        headers = advisory_message.find_all('h3', class_='separated')
        for header in headers:
            title = 'ADVISORY | ' + get_text(header)
            content = list(self.fetch_advisory_content(header))
            description = '\n'.join(get_text(c) for c in content)
            route = self.get_route(item)

            key = self.get_key(title + description)
            advisory = advisories.get(key, None)
            if advisory is not None:
                # Add route to affected services
                advisory['affected_services'].append(route)
                continue

            advisories[key] = SituationItem({
                'created': self.now.isoformat(),
                'source_type': 'JSON',
                'title': title,
                'description': description,
                'affected_services': [route, ],
            })

    def parse_detour_item(self, item, detours):
        title = 'DETOUR | ' + get_text(item['detour_message'])
        description = 'Start location: {start_location}\n' \
                      'Detour reason: {detour_reason}' \
            .format(
            start_location=clean(item['detour_start_location']),
            detour_reason=clean(item['detour_reason']),
        )
        route = self.get_route(item)

        key = self.get_key(title + description)
        detour = detours.get(key, None)
        if detour is not None:
            # Add route to affected services
            detour['affected_services'].append(route)
            return

        start_date = self.get_philly_timestamp(item['detour_start_date_time'])
        end_date = self.get_philly_timestamp(item['detour_end_date_time'])

        detours[key] = SituationItem({
            'created': self.now.isoformat(),
            'source_type': 'JSON',
            'title': title,
            'description': description,
            'affected_services': [route, ],
            'validity_period': {
                'start': start_date.isoformat(),
                'finish': end_date.isoformat()
            },
            'effect': Effect.DIVERSION.value
        })

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
        return parser.parse(timestamp) \
            .replace(tzinfo=self.TZ)

    def fetch_advisory_content(self, header):
        for elem in header.fetchNextSiblings():
            if elem.name == 'h3':
                break
            yield elem

    def get_route(self, item):
        route_id = item['route_id']

        # Get route name (and type)
        return '{route} ({route_type})' \
            .format(route=item['route_name'], route_type=self.get_route_type(route_id))

    def get_key(self, text):
        return re.sub(r'\s+', '', text)
