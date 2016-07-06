#!/usr/bin/env python
import json

def sortRestaurants():
	#f = open('C:/Users/tofor_000/Desktop/Cours_IIT/Data Mining/yelp_dataset_challenge_academic_dataset/xaa.json','r')


	data = []
	with open('yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json') as f:
		for line in f:
			data.append(json.loads(line))
	#print type(data[6])
	#json_obj = json.load(data)
	#print data
        output = open('Restaurants.json','w')
	#with open('C:/Users/tofor_000/Desktop/Cours_IIT/Data Mining/yelp_dataset_challenge_academic_dataset/xaa.json') as data_file:
		

	for item in data:
		if 'Restaurants' in item['categories']:
			#print 'YES Works'
			output.write(json.dumps(item))
			output.write("\n")
		else:
			#print 'NOT WORKING :('
			continue

   	output.close()
   	f.close()

sortRestaurants()
