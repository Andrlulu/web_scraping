
import requests
import json
import csv
import pandas as pd

writer = pd.ExcelWriter('RawData.xlsx',options={'strings_to_urls': False})

url = 'https://www.cargurus.com/Cars/inventorylisting/ajaxFetchSubsetInventoryListing.action?entitySelectingHelper.selectedEntity=d2039&distance=50000&zip=90009&sourceContext=forSaleTab_false_0'

response = requests.post(url)

data = response.json()

data1 = data['listings']

columns = data1[0].keys()
rows = {column:[] for column in columns}

for car in data1:
	for column in columns:
		try:
			rows[column].append(car[column])
		except:
			rows[column].append('')
rows = pd.DataFrame(rows)
rows.to_excel(writer,sheet_name = 'Tesla_Model_S', columns=columns)

writer.save()
