import re


def clean(to_clean):
    if isinstance(to_clean, str):
        return re.sub(r'\s+', ' ', to_clean).strip()
    return [re.sub(r'\s+', ' ', d).strip() for d in to_clean if d.strip()]


def get_text(soup):
    '''
    Wrapper around BeautifulSoup get_text, which does take a 'strip=True' arg, but this appeared to join words
    together in some cases
    :param soup: BeautifulSoup soup
    :return: string stripped of excess whitespace. Note all whitespace characters are included
    '''
    text = soup.get_text()
    return clean(text)
