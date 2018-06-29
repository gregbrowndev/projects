import scrapy
from bs4 import BeautifulSoup

from distruptions.items import SituationItem


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
            yield scrapy.Request(next_page, callback=self.parse_diversion)

    def parse_diversion(self, response):
        title = response.css('h1::text').extract_first()

        content = response.css('div#content-advisory')
        content_soup = BeautifulSoup(content.extract_first(), 'lxml')
        description = content_soup.get_text().strip()

        # TODO - check span#expireDate this shows when diversion ends (if empty this means indefinite)
        # Still need to parse h3 tag for start date/time

        # TODO - extract lines affected

        yield SituationItem({
            'source_id': '',
            'source_type': 'HTML',
            'source_location': response.url,
            'url': response.url,
            # could pass the request url? But this won't ever be unique due to de-duping
            'title': title,
            'description': description,
            'reason': '',
            'effect': ''
        })
