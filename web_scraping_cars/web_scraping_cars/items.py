# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy

class WebScrapingCarsItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    year = scrapy.Field()
    model = scrapy.Field()
    config= scrapy.Field()
    price = scrapy.Field()
    ori_price = scrapy.Field()
    mileage = scrapy.Field()
    deal = scrapy.Field()
    hotornot = scrapy.Field()
    ext_color = scrapy.Field()
    int_color = scrapy.Field()
    transmission = scrapy.Field()
    drivetrain = scrapy.Field()
    carfax = scrapy.Field()
    owners = scrapy.Field()
    dealer = scrapy.Field()
    dealer_rating = scrapy.Field()
    dealer_review_number = scrapy.Field()
    vin = scrapy.Field()
    consumer_rating = scrapy.Field()
    consumer_review_number = scrapy.Field()
    good_deal_margin = scrapy.Field()
    sellin = scrapy.Field()
    dealer_zipcode = scrapy.Field()
