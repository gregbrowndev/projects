import json
from datetime import datetime, timezone
from dateutil import parser
import scrapy

from disruptions.items import SituationItem, Reason, Effect


class TorontoSpider(scrapy.Spider):
    name = 'lothian'
    start_urls = [
        'https://lothianupdates.com/api/public/getServiceUpdates?key=8094E98541294E7AC25491127FAC7A72'
    ]

    def parse(self, response):
        data = json.loads(response.body_as_unicode())
        now = datetime.now(timezone.utc)

        for event in data['events']:
            title = event['title']['en']
            description = event['description']['en']

            validity_period = list(self.get_validity_period(event['time_ranges']))

            # TODO - extract routes_affected

            yield SituationItem({
                'created': now,
                'source_created': parser.parse(event['created']).isoformat(),
                'source_updated': parser.parse(event['last_updated']).isoformat(),
                'source_type': 'JSON',
                'source_location': '',
                'source_id': event['id'],
                'title': title,
                'description': description,
                'url': event['url'],
                'planned': self.is_planned(event['severity']),
                'reason': self.get_reason(event['cause']).value,
                'effect': self.get_effect(event['effect']).value,
                'validity_period': validity_period
            })

    def is_planned(self, severity):
        if (severity == 'PLANNED'):
            return True
        else:
            return False

    def get_reason(self, cause):
        lookup = {
            'ROAD_CLOSED': Reason.ROAD_CLOSED,
            'ROADWORKS': Reason.ROAD_WORKS,
            'WEATHER': Reason.WEATHER,
            'PLANNED_EVENT': Reason.PLANNED_EVENT,
            'UNPLANNED_EVENT': Reason.UNPLANNED_EVENT,
            'EMERGENCY_SERVICES': Reason.EMERGENCY_ENVIRONMENT,
        }
        return lookup.get(cause, Reason.UNKNOWN)

    def get_effect(self, effect):
        lookup = {
            'DELAYS': Effect.DELAYS,
            'DIVERSION': Effect.DIVERSION,
            'MODIFIED_SERVICE': Effect.MODIFIED_SERVICE
        }
        return lookup.get(effect, Effect.UNKNOWN)

    def get_validity_period(self, time_ranges):
        for time_range in time_ranges:
            finish_time = time_range.get('finish', None)
            if finish_time:
                finish_time = parser.parse(finish_time).isoformat()
            yield {
                "start": parser.parse(time_range['start']).isoformat(),
                "finish": finish_time,
            }