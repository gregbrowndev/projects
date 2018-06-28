import re
import scrapy
from scrapy.selector import Selector


class MTASpider(scrapy.Spider):
    name = "mta"
    start_urls = [
        'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=2&date=6/28/2018&time=&method=getstatus4',
    ]

    image_regex = re.compile(r'<img src=\"images/(\d).png\"/?>')

    def parse(self, response):
        content = response.css('div#divAd')
        headers = content.css('a[onclick^="ShowHide"]')

        for header in headers:
            # Extract Disruption Header

            # select the HTML id value from the 'ShowHide' onclick handler
            id = header.xpath('@onclick').re_first(r'ShowHide\((\d)\)')
            header_text = header.css('b').extract_first()
            header_text = re.sub(self.image_regex, r'(\1)', header_text)
            header_sel = Selector(text=header_text)
            header_text = ''.join(header_sel.css('::text').extract())

            # TODO - extract 'type' from header, e.g. TRACK REPLACEMENT
            # TODO - extract 'date range' from headers

            # Extract Disruption Description

            # Select correspinding description using id
            description = content.css('div#{id}'.format(id=id))

            # Replace image tags with (line number)
            description_text = description.extract_first()

            description_text = re.sub(self.image_regex, r'(\1)', description_text)
            description_text = description_text.replace('______________________________', '')

            description = Selector(text=description_text)
            description_text = ''.join(description.css('::text').extract())

            yield {
                'header': header_text,
                'description': description_text,
            }

    def get_image_src(self, node):
        return node.css('img').xpath('@src')

    def get_line_number_from_image(self, node):
        return self.get_image_src(node).re_first(r'images/(.).png')


if (__name__ == '__main__'):
    from scrapy.selector import Selector

    text = '''
    <div id="1" style="display:inline;"><b><br></b>For service <i>to </i>these stations, take the <img src="images/1.png"> to 72 St or Times Sq-42 St and transfer
        <br>to an uptown <img src="images/1.png"> or <img src="images/2.png"> <i>local</i>.
        <br>
        <br>For service <i>from </i>these stations, take the <img src="images/1.png"> or <img src="images/2.png"> to 72 St or 96 St and transfer
        <br>to a South Ferry-bound <img src="images/1.png">.
        <br><b>______________________________<br></b>
    </div>
    '''
    sel = Selector(text=text)
    description = sel.css('div#1')


    def clean_html(description):
        for n in description.xpath('node()'):
            if (n.xpath('self::img')):
                yield n.xpath('@src').re_first(r'images/(.+).png')
            if (n.xpath('self::text()')):
                yield n.css('::text')


    text = ''.join(clean_html(description))
