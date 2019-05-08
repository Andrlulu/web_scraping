from scrapy import Spider, Request
from web_scraping_cars.items import WebScrapingCarsItem
import re
import numpy as np

class Cars(Spider):
	name = 'cars_spider'
	allowed_urls = ['https://www.cars.com/']
	start_urls = ['https://www.cars.com/for-sale/searchresults.action/?mkId=28263&page=1&perPage=100&rd=99999&searchSource=GN_REFINEMENT&shippable-dealers-checkbox=true&sort=relevance&stkTypId=28881&zc=10001&userSetIxt=true&localVehicles=false']

	def parse(self, response):
		number_cars = response.xpath('//span[@class="count"]/text()').extract_first()
		cars_per_page = 100
		n_cars = int(number_cars.replace(',',''))

		if n_cars%cars_per_page ==0:
			total_pages = n_cars//cars_per_page
		else:
			total_pages = n_cars//cars_per_page + 1

		urls = ['https://www.cars.com/for-sale/searchresults.action/?mkId=28263&page={}&perPage=100&rd=99999&searchSource=PAGINATION&shippable-dealers-checkbox=true&sort=relevance&stkTypId=28881&zc=10001&userSetIxt=true&localVehicles=false'.format(i) for i in range(1, total_pages + 1)]

		for url in urls:
			yield Request(url=url, callback = self.parse_result_page)

	def parse_result_page(self, response):
		# print('='*50)
		# print(len(detail_urls))
		blocks = response.xpath('//*[@id="srp-listing-rows-container"]/div[@class="shop-srp-listings__listing-container"]')
		for i,block in enumerate(blocks):

			year_model = block.xpath('.//h2[@class="listing-row__title"]/text()').extract_first().strip()

			year = year_model[0:4]

			model = re.findall('(?<=Tesla ).{7}',year_model)

			config = re.findall('(?<=Tesla .{7} ).*',year_model)


			try:
				price = block.xpath('.//span[@class="listing-row__price "]/text()').extract_first().strip().replace('$','').replace(',','')
			except AttributeError:
				price = block.xpath('.//span[@class="listing-row__price new"]/text()').extract_first().strip().replace('$','').replace(',','')

			try:
				ori_price = block.xpath('.//span[@class="listing-row__old-price"]/text()').extract_first().strip().replace('$','').replace(',','')
			except AttributeError:
				ori_price = ""
			
			mileage = int(block.xpath('.//span[@class="listing-row__mileage"]/text()').extract_first().strip().replace(',','').replace(' mi.',''))
			
			try:
				deal = block.xpath('.//div[@class="listing-row__price-comparison-tool"]//span/strong/text()').extract_first().strip()
			except AttributeError:
				deal = ''
			try:
				hotornot = block.xpath('.//div[@class="listing-row__hot-car-badge--label"]/strong/text()').extract_first().strip()
			except AttributeError:
				hotornot = "NOT HOT"
			ext_color = block.xpath('.//ul[@class="listing-row__meta"]//text()').extract()[3].strip()
			int_color = block.xpath('.//ul[@class="listing-row__meta"]//text()').extract()[7].strip()
			transmission = block.xpath('.//ul[@class="listing-row__meta"]//text()').extract()[11].strip()
			drivetrain = block.xpath('.//ul[@class="listing-row__meta"]//text()').extract()[15].strip()
			carfax = block.xpath('.//ul[@class="listing-row__offers"]//span/text()').extract()[1].strip()
			try:
				owners = int(re.findall('\d+',carfax)[0])
			except IndexError:
				owners = "more than 1"
			dealer = block.xpath('.//div[@class="dealer-name"]/span/text()').extract_first().strip()
			try:
				dealer_rating = float(block.xpath('.//div[@class="dealer-rating-stars "]//text()').extract()[3].strip())
			except IndexError:
				dealer_rating = np.nan
			try:
				dealer_review_number = int(re.findall('\d+',block.xpath('.//div[@class="dealer-rating-stars "]//text()').extract()[4].strip())[0])
			except IndexError:
				dealer_review_number = np.nan

			detail_url = block.xpath('//div[@class="shop-srp-listings__listing-container"][{}]/a/@href'.format(i+1)).extract_first()

			yield Request(url='https://cars.com'+ detail_url, callback=self.parse_detail_page,\
				meta={
				'year': year,
				'model': model,
				'config': config,
				'price': price,
				'ori_price': ori_price,
				'mileage': mileage,
				'deal': deal,
				'hotornot': hotornot,
				'ext_color': ext_color,
				'int_color': int_color,
				'transmission': transmission,
				'drivetrain': drivetrain,
				'carfax': carfax,
				'owners': owners,
				'dealer': dealer,
				'dealer_rating': dealer_rating,
				'dealer_review_number': dealer_review_number
				})

	def parse_detail_page(self, response):
		year = response.meta['year']
		model = response.meta['model']
		config = response.meta['config']
		price = response.meta['price']
		ori_price = response.meta['ori_price']
		mileage = response.meta['mileage']
		deal = response.meta['deal']
		hotornot = response.meta['hotornot']
		ext_color = response.meta['ext_color']
		int_color = response.meta['int_color']
		transmission = response.meta['transmission']
		drivetrain = response.meta['drivetrain']
		carfax = response.meta['carfax']
		owners = response.meta['owners']
		dealer = response.meta['dealer']
		dealer_rating = response.meta['dealer_rating']
		dealer_review_number = response.meta['dealer_review_number']
		
		try:
			vin = "".join(response.xpath('.//li[@class="vdp-details-basics__item"]//text()').extract())
			vin = re.findall('(?<=VIN: ).*',vin)[0].strip()
		except IndexError:
			vin = ''

		try:
			consumer_rating = float(response.xpath('.//div[@class="overall-review-stars"]//p/text()').extract_first())
		except IndexError:
			consumer_rating = np.nan

		try:
			consumer_review_number = response.xpath('.//div[@class="review-stars-average"]//a/text()').extract_first()
			consumer_review_number = int(re.findall('\d+',consumer_review_number)[0])
		except TypeError:
			consumer_review_number = np.nan
		try:
			good_deal_margin = response.xpath('.//div[@class="good-deal-price"]/text()').extract_first()
		except IndexError:
			good_deal_margin = np.nan

		try:
			sellin = response.xpath('.//div[@class="hot-badge-container"]/p/text()').extract_first().strip()
			sellin = int(re.findall('\d+',sellin)[1])
		except AttributeError:
			sellin = np.nan
		try:
			dealer_zipcode = response.xpath('.//div[@class="get-directions-link seller-details-location__text"]/a/text()').extract_first()
			dealer_zipcode = re.findall('\d+',dealer_zipcode)
		except TypeError:
			dealer_zipcode = np.nan

		item = WebScrapingCarsItem()
		item['year'] = year
		item['model'] = model
		item['config'] = config
		item['price'] = price
		item['ori_price'] = ori_price
		item['mileage'] = mileage
		item['deal'] = deal
		item['hotornot'] = hotornot
		item['ext_color'] = ext_color
		item['int_color'] = int_color
		item['transmission'] = transmission
		item['drivetrain'] = drivetrain
		item['carfax'] = carfax
		item['owners'] = owners
		item['dealer'] = dealer
		item['dealer_rating']=dealer_rating
		item['dealer_review_number'] = dealer_review_number
		item['vin'] = vin
		item['consumer_rating'] = consumer_rating
		item['consumer_review_number'] = consumer_review_number
		item['good_deal_margin'] = good_deal_margin
		item['sellin'] = sellin
		item['dealer_zipcode'] = dealer_zipcode

		yield item