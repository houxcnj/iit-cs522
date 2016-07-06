#!/usr/bin/env python
import json

def sortRestaurants():
	#f = open('C:/Users/tofor_000/Desktop/Cours_IIT/Data Mining/yelp_dataset_challenge_academic_dataset/xaa.json','r')


	data = []
	with open('Restaurants.json') as f:
		for line in f:
			data.append(json.loads(line))
	#print type(data[6])
	#json_obj = json.load(data)
	#print data
        output = open('RestaurantsReview.json','w')
        data1 = []
	with open('yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json') as data_file:
            for line1 in data_file:
                data1.append(json.loads(line1))


	for item in data:
            for item1 in data1:
		if item1['business_id'] in item['business_id']:
			#print 'YES Works'
			output.write(json.dumps(item1))
			output.write("\n")
		else:
			#print 'NOT WORKING :('
			continue

   	output.close()
   	f.close()
   	data_file.close()

sortRestaurants()
