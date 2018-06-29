import scrapy
from scrapy.shell import inspect_response
from scrapy_splash import SplashRequest
from bs4 import BeautifulSoup

from distruptions.items import SituationItem
from distruptions.utils import get_text


class WestMidlandsSpider(scrapy.Spider):
    name = 'west-midlands'

    def start_requests(self):
        # Process AngularJS page with Network West Midlands disruptions data using Splash
        base_url = 'https://www.networkwestmidlands.com/plan-your-journey/disruptions/#/params?DisruptionType=&when=All&TransportModeA=5&TransportModeB=0&TransportModeC=4'
        yield SplashRequest(url=base_url, callback=self.parse, args={'wait': 1.0})

    def parse(self, response):
        # Convert to BeautifulSoup as its easier to convert inline <i>, <b>, etc. to text
        soup = BeautifulSoup(response.text, 'lxml')
        content = soup.select_one('div#disruptionContainer')
        items = content.select('div.disruption')

        if not items:
            self.logger.warning('Failed to parse response: {url}'.format(url=response.url))
            # inspect_response(item, self)
            return

        for item in items:
            disruption_body = item.select_one('div.disruption__body')
            description = disruption_body.select_one('div.disrup-description')

            affected_services = item.select('ul.disruptions')
            detail = ', '.join(get_text(service) for service in affected_services)

            yield SituationItem({
                'source_id': '',
                'source_type': 'HTML',
                'source_location': response.url,
                'url': response.url,
                'title': item.select_one('h3').get_text().strip(),
                'description': description.get_text().strip(),
                'detail': detail
            })
