# -*- coding: utf-8 -*-
"""
Calculating fire exposure index for Alberta cities.
Using the full list of Census Subdivisions for the 400+ CSDs in Alberta. 
Calculates exposure of each DU then calculates population-weighted average for CSD
Key step is writing a function to calculate fire exposure.
Calculates both regular fire exposure and lightning fire exposure.

"""
import pandas as pd
import geopy.distance as gp # convert lat/long coordinates to distance
import math
# import swifter # https://github.com/jmcarpenter2/swifter. For parallel processing, not faster when used here

# load key files
fires = pd.read_csv('fires_clean.csv')
cities = pd.read_csv('csd_clean.csv') # load file with all CSDs, not the cities dataset

''' 
1.1 - Setup fires
'''
# convert fire start/end date (strings) to datetime objects to faclitate subtraction
# documentation for handling "format" parameter: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
fires['fire_start_date'] = pd.to_datetime(fires['fire_start_date'], format='%Y-%m-%d')
fires['ex_fs_date'] = pd.to_datetime(fires['ex_fs_date'], format='%Y-%m-%d')

# optional: filter out very large fires 
fires = fires.loc[fires['current_size']<200,:]

# filter out very small fires
fires = fires.loc[fires['current_size']>0.01]

# drop unneeded columns
fires = fires.loc[:, ('fire_location_latitude', 'fire_location_longitude',
                     'fire_start_date', 'ex_fs_date',
                     'current_size', 'general_cause_desc', 'fire_year')] 

# get subset of fires caused by lightning
lightning_fires = fires.loc[fires['general_cause_desc'] == 'Lightning',]

'''
1.2 - Setup cities
        too many DU (distribution units) to run exposure independently
        scores for all DUs in a given CSU were essentially equal
        DU coordinates to get coordinates for a CSU
'''

# group by CSD
cities = cities.groupby(['CSDuid/SDRidu', 'CSDname/SDRnom']).agg({'DArplat/ADlat':['mean'], 'DArplong/ADlong':['mean']})
cities.columns = ['DArplat/ADlat','DArplong/ADlong']
cities.reset_index(inplace=True)



'''
2 - Functions

define two functions:
    1. fire_exposure(): calculates the fire exposure of one fire to a given city
    2. total_fire_exposure(): applies fire_exposure() to each fire and sums 
        to get total exposure to a city in a given year.
'''

# function gets applied row-wise one at a time to the fires df
# creates an output column in the fires df
# the output column is later summed by total_fire_exposure() to get total exposure
# sacrifices some speed but allows for easier verification and modification of function

def single_fire_exposure_1(fires_row, city_lat, city_long):
    
    # get fire data       
    fire_lat = fires_row['fire_location_latitude'] 
    fire_long = fires_row['fire_location_longitude']
    
    # get distance between city and fire in km
    fire_distance = gp.distance((fire_lat, fire_long), (city_lat, city_long)).km
    
    # get size of fire
    fire_size = fires_row['current_size'] # area burned, see other measures
    
    # get duration of fire    
    fire_start_date = fires_row['fire_start_date']
    fire_end_date = fires_row['ex_fs_date']
    
    fire_duration = (fire_end_date - fire_start_date).seconds / 3600 # timedelta object. Answer in hours. 
    
    # calculate fire exposure
    exposure = fire_duration * fire_size * math.exp(-fire_distance/100) # could add params to make magnitude more comprehensible
    return exposure

# see Example 2 for df.apply() syntax https://www.geeksforgeeks.org/apply-a-function-to-each-row-or-column-in-dataframe-using-pandas-apply/
# .apply(function, axis (1=row), arg=[list of additional arguments])
# function is automatically passed the dataframe row ("fires_row" here). Must pass the additonal args as a list.


# gets fire exposure from each fire in a given year (from fire_exposure)
# returns sum of all individual fires
def total_fire_exposure(cities_row, fires, year):
    
    # get set of fires in given year, drop unncecessary columns
    fires_subset = fires.loc[fires['fire_year']==year,:] # note local scope of fires. But gets passed the global fires df when called

    # get city location
    city_lat = cities_row['DArplat/ADlat']
    city_long = cities_row['DArplong/ADlong']
    
    # calculate exposure from each fire
    fire_exposures = fires_subset.apply(single_fire_exposure_1, axis=1, args=[city_lat, city_long])
    # alternative to inspect results more easily: fires_subset['fire_exposures']= fires_subset.apply(single_fire_exposure_1, axis=1, args=[city_lat, city_long])

    return fire_exposures.sum()

      
'''
3 - Calculate Total Exposure

Applies total_fire_exposure() to each city for each given year
'''


# select years
years = [year for year in range(2006, 2019)]

# get raw total exposure score for each year
for year in years:
    cities['FE_' + str(year)] = cities.apply(total_fire_exposure, axis=1, args=[fires, year]) # create new column for fire exposure (FE) in each year
    print(year) # to show progress
 
    

'''
4 - Calculate Total Exposure for Fires Caused by Lightning

Applies total_fire_exposure() to each city for each given year 
using the subset of fires caused by lightning
'''

# select years
years = [year for year in range(2006, 2019)]

# get raw exposure score for fires caused by lightning in each year
for year in years:
    cities['FE_lightning_' + str(year)] = cities.apply(total_fire_exposure,
                                             axis=1, args=[lightning_fires, year]) # create new column for fire exposure (FE) in each year
    print(year) # to show progress
   

    
# save to new file
cities.to_csv('csd_clean_with_FE.csv', index=False)

    

'''
4.1 - Testing - get exposure, distance, duration and size for each fire
'''

'''
year = 2014
CSD = 4801003 # first Dissemination Block  

# subset of fires in given year, drop unnecessary columns
fires_subset = fires.loc[fires['fire_year']==year,:]
        
# get city location
city_lat = cities.loc[cities['CSDuid/SDRidu'] == CSD, 'DArplat/ADlat'].values[0]
city_long = cities.loc[cities['CSDuid/SDRidu'] == CSD, 'DArplong/ADlong'].values[0]

# function to calculate distance between city and each fire
def fire_dist(fires_row, city_lat, city_long):
    fire_lat = fires_row['fire_location_latitude']
    fire_long = fires_row['fire_location_longitude']
    return gp.distance((fire_lat, fire_long), (city_lat, city_long)).km  

# results for single city
fires_subset['distance'] = fires_subset.apply(fire_dist, axis=1, args=[city_lat, city_long])
fires_subset['duration'] = fires_subset['ex_fs_date'] - fires_subset['fire_start_date']
fires_subset['FE'] = fires_subset.apply(single_fire_exposure_1, axis=1, args=[city_lat, city_long])    

fires_subset['FE'].sum()
'''



'''
5 - Calculate Standardized Score for total Fire Exposure and Lightning FE

get a standardized score between 0 and 1
must standardize all years by the same max/min (rather than within a year)
also must standardize FE and Lightning FE by the same numbers 
'''

'''
# get highest and lowest score from each year
max_scores = []
min_scores = []

for year in years:
    max_scores.append(cities['FE_' + str(year)].max()) # highest score in year
    min_scores.append(cities['FE_' + str(year)].min()) # lowest score in year
    
max_score = max(max_scores) # get highest score across all years
min_score = min(min_scores) # get lowest score across all years (should be 0)

# standardize with max/min across the given years for both FE and FE lightning
for year in years:    
    cities['FE_std_' + str(year)] = (cities['FE_' + str(year)] - min_score) / (max_score - min_score)
    cities['FE_std_lightning' + str(year)] = (cities['FE_lightning_' + str(year)] - min_score) / (max_score - min_score)


    
# save to new file
cities.to_csv('csd_clean_with_FE.csv', index=False)
'''
