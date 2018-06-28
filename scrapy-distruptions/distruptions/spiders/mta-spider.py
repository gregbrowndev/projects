import scrapy


class MTASpider(scrapy.Spider):
    name = "mta"
    start_urls = [
        'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=2&date=6/28/2018&time=&method=getstatus4',
    ]

    def parse(self, response):

        content = response.css('div#divAd')

        # objs = content.css('b::text')

        headers = content.css('a[onclick^="ShowHide"]')


        for quote in response.css('div.quote'):
            yield {
                'text': quote.css('span.text::text').extract_first(),
                'author': quote.css('small.author::text').extract_first(),
                'tags': quote.css('div.tags a.tag::text').extract(),
            }

