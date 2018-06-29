import re
from datetime import datetime, timedelta
import scrapy
from bs4 import BeautifulSoup

from distruptions.items import SituationItem, Reason, Effect
from distruptions.utils import clean


# TODO - add validation using voluptuous or scrapy-jsonschema


class MTASpider(scrapy.Spider):
    name = "mta"
    # start_urls = [
    #     # 'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=2&date=6/28/2018&time=&method=getstatus4',
    #     'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=ALL&date=6/29/2018&time=&method=getstatus4'
    # ]

    image_regex = re.compile(r'<img src=\"images/(.+).png\"/?>')
    onclick_regex = re.compile(r'ShowHide\((\d+)\)')
    underline_regex = re.compile(r'_{4,}')

    REASON_LOOKUP = {
        'TRACK MAINTENANCE': Reason.MAINTAINCE,
        'STATION ENHANCEMENTS': Reason.CONSTRUCTION,
        'ELECTRICAL IMPROVEMENTS': Reason.MAINTAINCE,
        'TRACK REPLACEMENT': Reason.MAINTAINCE,
        'STATION RENOVATION': Reason.CONSTRUCTION,
        'SCHEDULED MAINTENANCE': Reason.MAINTAINCE,
        'SIGNAL MAINTENANCE': Reason.MAINTAINCE,
        'ELEVATOR INSTALLATION': Reason.CONSTRUCTION,
        'ALTERNATIVE SERVICE': Reason.PLANNED_EVENT  # NOTE - this could probably be its own Reason enum?
    }

    def __init__(self, name=None, **kwargs):
        super().__init__(name=None, **kwargs)
        # This keep track of 'titles' seen to only keep unique disruptions
        self.seen_titles = set()

    def start_requests(self):
        base_url = 'http://travel.mtanyct.info/serviceadvisory/routeStatusResult.aspx?tag=ALL&date={date}&time=&method=getstatus4'
        dates = self.lookahead()
        return [scrapy.Request(url=base_url.format(date=date.strftime('%d/%m/%Y')), callback=self.parse)
                for date in dates]

    def lookahead(self):
        '''
        Returns a list of the next 30 days
        :return: [datetime.datetime]
        '''
        today = datetime.today()
        return [today + timedelta(days=i) for i in range(30)]

    def parse(self, response):
        soup = BeautifulSoup(response.text, 'lxml')
        content = soup.select_one('div#divAd')

        # replace images with text
        for img in content.find_all('img'):
            match = re.match(self.image_regex, str(img))
            if (match):
                line_name = match.group(1)
                img.replace_with('({name})'.format(name=line_name))

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

                # TODO - find validity period
                # TODO - add timestamps

                # get title text
                title = self.get_text(item)

                # ignore duplicate disruption by checking heading text
                if (title in self.seen_titles):
                    continue
                else:
                    self.seen_titles.add(title)

                # get cause
                cause = title.split('|')[0].strip()
                reason = self.REASON_LOOKUP.get(cause, Reason.UNKNOWN).value

                # find description
                description = soup.find('div', id=id)

                # TODO - try to infer effect? - pretty difficult
                effect = Effect.UNKNOWN.value

                # remove underscore (matches at least 4 underscores to prevent legit mistake)
                description.find(string=self.underline_regex).replace_with('')

                yield SituationItem({
                    'source_id': id,  # TODO - remove as this is not really an id
                    'source_type': 'MTA',
                    'source_location': response.url,
                    # could pass the request url? But this won't ever be unique due to de-duping
                    'title': title,
                    'description': self.get_text(description),
                    'is_public': True,
                    'reason': reason,
                    'effect': effect
                })
            except:
                self.logger.warning('Failed to parse item: {text}'.format(text=item.get_text()))
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
        return clean(text)
