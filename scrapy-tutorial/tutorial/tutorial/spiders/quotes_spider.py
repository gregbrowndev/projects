import scrapy


class QuotesSpider(scrapy.Spider):
    name = "quotes"
    start_urls = [
        'http://quotes.toscrape.com/page/1/',
    ]

    def parse(self, response):
        for quote in response.css('div.quote'):
            yield {
                'text': quote.css('span.text::text').extract_first(),
                'author': quote.css('small.author::text').extract_first(),
                'tags': quote.css('div.tags a.tag::text').extract(),
            }

        # next_page = response.css('li.next a::attr(href)').extract_first()
        # if next_page is not None:
        #     # we can generate an absolute url, but response.follow can do that for us!
        #     # next_page = response.urljoin(next_page)
        #     yield response.follow(next_page, callback=self.parse)

        # even shorter
        for a in response.css('li.next a'):
            yield response.follow(a, callback=self.parse)
