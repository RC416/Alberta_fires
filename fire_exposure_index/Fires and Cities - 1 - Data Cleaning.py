# -*- coding: utf-8 -*-
"""

Clean datasets to prepare for plotting and calculating fire exposure index

Key steps
- filter cities for Alberta
- convert lattitude/longitude coordinates to measures of distance
- estimate radius of fires and cities

"""

import pandas as pd
import math
import geopy.distance as gp # for geographic distances

# load raw datasets
fires = pd.read_csv('Alberta Fires 2006-2018.csv', encoding='latin_1') # had to try different encodings
cities = pd.read_csv('canada cities data.csv', encoding='latin_1')
csd = pd.read_csv('geographic attribution data for CSD.csv', encoding='latin_1') # dataset with all 400+ census subdivisions in Alberta


'''
1 - Cities - original set of 117 Alberta cities
'''

# filter cities and create useful variables
cities = cities.loc[cities['province_name']=='Alberta',:].reset_index() # remove cities outside of Alberta
cities['area_sq_km'] = cities['population_per_sq_km'] / cities['density'] # back out municipality area
cities['radius_km'] = (cities['area_sq_km'] / math.pi) ** 0.5 # estimate city radius
# note Wood Buffalo is by far the largest. This seems to be correct although the size is smaller than listed on internet.

# locations are given in lat/long coordinates
# express location in distance (km) from Edmonton (approx. center of province)

# set origin of plot to be Edmonton
Edmonton_lat = 53.5344
Edmonton_long = -113.490

# functions to convert lat/long coordinates to distance:   
# returns vertical distance (north/south) in km between a given city and Edmonton
def vertical_dist_from_Edmonton (city_latitude):      
    dist = gp.distance((city_latitude, Edmonton_long), (Edmonton_lat, Edmonton_long)).km
    return dist

# returns horizontal distance (east/west) in km between a given city and Edmonton
def horizontal_dist_from_Edmonton (city_longitude):
    dist = gp.distance((Edmonton_lat, city_longitude), (Edmonton_lat, Edmonton_long)).km
    return dist

# convert lat/long coordinates into distance using the above functions:
# recall latitude lines run east/west so difference in latitude is north/south
cities['horizontal_dist_km'] = cities['lng'].apply(horizontal_dist_from_Edmonton)
cities['vertical_dist_km'] = cities['lat'].apply(vertical_dist_from_Edmonton) 

# assign direction to the distance
cities['east_of_ed'] = cities['lng'].apply(lambda x: 1 if (x - Edmonton_long > 0) else -1)  # negative means west of Edmonton             
cities['north_of_ed'] = cities['lat'].apply(lambda x: 1 if (x - Edmonton_lat > 0) else -1) # negative means south of Edmonton
                                           
# apply direction to lat/long distance
cities['horizontal_dist_km'] = cities['horizontal_dist_km'] * cities['east_of_ed']
cities['vertical_dist_km'] = cities['vertical_dist_km'] * cities['north_of_ed']

cities.to_csv('cities_clean.csv', index=False)


''' 
2 - Fires 
'''

 # estimate radius on fire (from hectares to radius as km)
fires['radius_km'] = ((fires['current_size'] / math.pi) ** 0.5) * 1/10

# convert lat/long coordinates into distance using the above functions:
# recall latitude lines run east/west so difference in latitude is north/south
fires['horizontal_dist_km'] = fires['fire_location_longitude'].apply(horizontal_dist_from_Edmonton) # same functions as line 40-47
fires['vertical_dist_km'] = fires['fire_location_latitude'].apply(vertical_dist_from_Edmonton) 

# assign direction to the distance
fires['east_of_ed'] = fires['fire_location_longitude'].apply(lambda x: 1 if (x - Edmonton_long >0) else -1)  # negative means west of Edmonton             
fires['north_of_ed'] = fires['fire_location_latitude'].apply(lambda x: 1 if (x - Edmonton_lat > 0) else -1) # negative means south of Edmonton   

# apply direction to lat/long distance
fires['horizontal_dist_km'] = fires['horizontal_dist_km'] * fires['east_of_ed']
fires['vertical_dist_km'] = fires['vertical_dist_km'] * fires['north_of_ed']

fires.to_csv('fires_clean.csv', index=False)



'''
3 - CSDs - set of all census subdivisions for Alberta. Used for final analysis.
'''

# filter for relevant rows and columns
# note that multiple DBs make up a DA which make up a CSD
# https://geosuite.statcan.gc.ca/geosuite/en/index

csd = csd.loc[csd['PRname/PRnom'] == 'Alberta', ['PRname/PRnom', 'CSDuid/SDRidu',
                                                 'CSDname/SDRnom','DBuid/IDidu',
                                                 'DBpop2016/IDpop2016', 'DBarea2016/IDsup2016',
                                                 'DArplat/ADlat','DArplong/ADlong']]

# For plotting purposes:
# locations are given in lat/long coordinates
# express location in distance (km) from Edmonton (approx. center of province)


# convert lat/long coordinates into distance using the above functions:
# recall latitude lines run east/west so difference in latitude is north/south
csd['horizontal_dist_km'] = csd['DArplong/ADlong'].apply(horizontal_dist_from_Edmonton) 
csd['vertical_dist_km'] = csd['DArplat/ADlat'].apply(vertical_dist_from_Edmonton) 

# assign direction to the distance
csd['east_of_ed'] = csd['DArplong/ADlong'].apply(lambda x: 1 if (x - Edmonton_long > 0) else -1)  # negative means west of Edmonton             
csd['north_of_ed'] = csd['DArplat/ADlat'].apply(lambda x: 1 if (x - Edmonton_lat > 0) else -1) # negative means south of Edmonton
                                           
# apply direction to lat/long distance
csd['horizontal_dist_km'] = csd['horizontal_dist_km'] * csd['east_of_ed']
csd['vertical_dist_km'] = csd['vertical_dist_km'] * csd['north_of_ed']

csd.to_csv('csd_clean.csv', index=False)