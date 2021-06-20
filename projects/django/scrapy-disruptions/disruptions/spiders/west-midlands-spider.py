import scrapy
from scrapy.shell import inspect_response
from scrapy_splash import SplashRequest
from bs4 import BeautifulSoup

from disruptions.items import SituationItem
from disruptions.utils import get_text


class WestMidlandsSpider(scrapy.Spider):
    name = 'west-midlands'

    def start_requests(self):
        # Process AngularJS page with Network West Midlands disruptions data using Splash
        base_url = 'https://www.networkwestmidlands.com/plan-your-journey/disruptions/#/params?DisruptionType=&when=All&TransportModeA=5&TransportModeB=0&TransportModeC=4'
        yield SplashRequest(url=base_url, callback=self.parse, args={'wait': 1.0})

    def parse(self, response):
        content = response.css('div#disruptionContainer')
        items = content.css('div.disruption')

        if not items:
            self.logger.warning('Failed to parse response: {url}'.format(url=response.url))
            # inspect_response(item, self)
            return

        for item in items:
            title = get_text(item.css('h3')[0])

            rows = item.css('div.disruption__body > div.disruption-row')

            start_date = rows.xpath('span[text() = "Start date:"]') \
                .xpath('following-sibling::span/text()').extract_first()

            end_date = rows.xpath('span[text() = "End date:"]') \
                .xpath('following-sibling::span/text()').extract_first()

            description = get_text(rows.css('div.disrup-description')[0])

            affected_services = item.css('ul.disruptions > li')
            affected_services_list = [get_text(service) for service in affected_services]

            yield SituationItem({
                'source_id': '',
                'source_type': 'HTML',
                'source_location': response.url,
                'url': response.url,
                'title': title,
                'description': description,
                'affected_services': affected_services_list,
                'validity_period': [{
                    "start": start_date,
                    "finish": end_date,
                }]
            })
