import scrapy
from bs4 import BeautifulSoup

from disruptions.items import SituationItem
from disruptions.utils import get_text


class TorontoSpider(scrapy.Spider):
    name = 'toronto'
    start_urls = [
        'http://www.ttc.ca/Service_Advisories/Route_diversions/index.jsp'
    ]

    def parse(self, response):
        content = response.css('div.main-content')
        links = content.css('h4 a::attr(href)').extract()
        for link in links:
            if (link == 'York_U_Strike.jsp'):
                continue
            next_page = response.urljoin(link)
            yield scrapy.Request(next_page, callback=self.parse_item)

    def parse_item(self, response):
        title = response.css('h1::text').extract_first()

        content = response.css('div#content-advisory')[0]
        description = get_text(content)

        # TODO - check span#expireDate this shows when diversion ends (if empty this means indefinite)
        # Still need to parse h3 tag for start date/time

        # TODO - extract lines affected

        yield SituationItem({
            'source_id': '',
            'source_type': 'HTML',
            'source_location': response.url,
            'url': response.url,
            'title': title,
            'description': description,
            'reason': '',
            'effect': ''
        })
