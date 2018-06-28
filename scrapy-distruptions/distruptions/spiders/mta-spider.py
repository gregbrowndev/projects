from string import punctuation
import re
import scrapy
from bs4 import BeautifulSoup


class MTASpider(scrapy.Spider):
    name = "mta"
    start_urls = [
        'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=2&date=6/28/2018&time=&method=getstatus4',
    ]

    image_regex = re.compile(r'<img src=\"images/(.+).png\"/?>')
    onclick_regex = re.compile(r'ShowHide\((\d)\)')

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
            # extract 'id' from onclick handler (this opens the description so can be used to select that later)
            id = re.match(self.onclick_regex, item['onclick']).group(1)

            # get heading
            heading = item.find('b')

            # find description
            description = soup.find('div', id=id)

            # remove underscore (matches at least 4 underscores to prevent legit mistake)
            description.find(string=re.compile(r'_{4,}')).replace_with('')

            yield {
                'header': self.get_text(heading),
                'description': self.get_text(description)
            }

    def get_text(self, soup):
        '''
        Wrapper around BeautifulSoup get_text, which does take a 'strip=True' arg, but this appeared to join words
        together in some cases
        :param soup: BeautifulSoup soup
        :return: string stripped of excess whitespace. Note all whitespace characters are included
        '''
        text = soup.get_text()
        return re.sub('\s+', ' ', text).strip()
