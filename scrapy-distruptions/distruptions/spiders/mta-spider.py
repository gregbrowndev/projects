import re
from datetime import datetime, timedelta
import scrapy
from bs4 import BeautifulSoup
from scrapy.shell import inspect_response


class MTASpider(scrapy.Spider):
    name = "mta"
    start_urls = [
        # 'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=2&date=6/28/2018&time=&method=getstatus4',
        'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=ALL&date=6/29/2018&time=&method=getstatus4'
    ]

    image_regex = re.compile(r'<img src=\"images/(.+).png\"/?>')
    onclick_regex = re.compile(r'ShowHide\((\d+)\)')
    underline_regex = re.compile(r'_{4,}')

    def lookahead(self):
        '''
        Returns a list of the next 30 days
        :return: [datetime.datetime]
        '''
        today = datetime.today()
        return [today + timedelta(days=i) for i in range(30)]

    # def start_requests(self):
    #     base_url = 'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=ALL&date={date}&time=&method=getstatus4'
    #     dates = self.lookahead()
    #     return [scrapy.Request(url=base_url.format(date=date.strftime('%d/%m/%Y')), callback=self.parse)
    #             for date in dates]

    def parse(self, response):
        soup = BeautifulSoup(response.text, 'lxml')
        content = soup.select_one('div#divAd')

        # replace images with text
        for img in content.find_all('img'):
            line_name = re.sub(self.image_regex, r'(\1)', str(img))
            img.replace_with(line_name)

        # replace line breaks with a space
        for br in content.find_all('br'):
            br.replace_with(' ')

        # these are the disruption items
        items = content.select('a[onclick^="ShowHide"]')

        for item in items:
            try:
                # extract 'id' from onclick handler (this opens the description so can be used to select that later)
                id = re.match(self.onclick_regex, item['onclick']).group(1)

                if (int(id) > 1000):
                    # Ignore click handlers with large id (these are for nested tables within the description)
                    continue

                # get heading text
                heading = self.get_text(item)

                # find description
                description = soup.find('div', id=id)

                # remove underscore (matches at least 4 underscores to prevent legit mistake)
                description.find(string=self.underline_regex).replace_with('')

                yield {
                    'id': id,
                    'header': heading,
                    'description': self.get_text(description)
                }
            except:
                self.logger.warning('Failed to parse item: {text}', item.get_text())
                # inspect_response(item, self)
                continue

    def get_text(self, soup):
        '''
        Wrapper around BeautifulSoup get_text, which does take a 'strip=True' arg, but this appeared to join words
        together in some cases
        :param soup: BeautifulSoup soup
        :return: string stripped of excess whitespace. Note all whitespace characters are included
        '''
        text = soup.get_text()
        return re.sub('\s+', ' ', text).strip()


def get_text(soup):
    '''
    Wrapper around BeautifulSoup get_text, which does take a 'strip=True' arg, but this appeared to join words
    together in some cases
    :param soup: BeautifulSoup soup
    :return: string stripped of excess whitespace. Note all whitespace characters are included
    '''
    text = soup.get_text()
    return re.sub('\s+', ' ', text).strip()


if (__name__ == '__main__'):
    response = None
    # scrapy shell 'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=ALL&date=6/29/2018&time=&method=getstatus4'

    import re
    import scrapy
    from bs4 import BeautifulSoup

    image_regex = re.compile(r'<img src=\"images/(.+).png\"/?>')
    onclick_regex = re.compile(r'ShowHide\((\d+)\)')
    underline_regex = re.compile(r'_{4,}')

    soup = BeautifulSoup(response.text, 'lxml')
    content = soup.select_one('div#divAd')

    # replace images with text
    for img in content.find_all('img'):
        line_name = re.sub(image_regex, r'(\1)', str(img))
        img.replace_with(line_name)

    # replace line breaks with a space
    for br in content.find_all('br'):
        br.replace_with(' ')

    # these are the disruption items
    items = content.select('a[onclick^="ShowHide"]')

    item = content.select_one('a[onclick^="ShowHide(15);"]')
