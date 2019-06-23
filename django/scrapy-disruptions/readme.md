# Scrapy Disruptions Scraper

## Quick Start

Creating the project:

```bash
scrapy startproject scrapy-distruptions .
```


Run a spider:

```bash
scrapy crawl mta -o data/mta.json -L WARNING
```

The warning level is useful for quickly spotting issues.
Can also specify `--logfile` option to output logs to file.


## Overview

Scrapy is a framework for writing webscrapers.


SPA, client-side rendered apps can also be scraped with various
methods:

* Use Selenium + Browser
* Using Splash

The [scrapy-splash](https://github.com/scrapy-plugins/scrapy-splash) 
library allows you to scrape HTML rendered via JavaScript.

Run Splash server:

```bash
docker run -p 8050:8050 -p 5023:5023 scrapinghub/splash
```

