# Scrapy Distruptions Scraper

## Notes

Creating the project:

```bash
scrapy startproject scrapy-distruptions .
```


Run a spider:

```bash
scrapy crawl mta -o mta_disruptions.json -L WARNING
```

The warning level is useful for quickly spotting issues.
Can also specify `--logfile` option to output logs to file.