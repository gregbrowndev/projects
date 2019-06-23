# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://doc.scrapy.org/en/latest/topics/item-pipeline.html
from scrapy.exceptions import DropItem


class DisruptionsPipeline(object):
    def process_item(self, item, spider):
        # check required fields
        if (('title' not in item) or not item['title'].strip()):
            raise DropItem("Missing title in %s" % item)

        # if (('description' not in item) or not item['description'].strip()):
        #     raise DropItem("Missing description in %s" % item)

        return item
