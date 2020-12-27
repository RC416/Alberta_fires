# -*- coding: utf-8 -*-
"""
Plot Census Subdivisions (minicipalities) and fires on an overlapping chart

Plot city & fire location using distance (km) from reference (Edmonton)
Have point radius correspond to approximate size of city & fire
Use plt.Circle to draw accurately sized circles

"""
import pandas as pd
import matplotlib.pyplot as plt
import math

# load clean datasets
fires = pd.read_csv('fires_clean.csv') # had to try different encodings
csd = pd.read_csv('csd_clean.csv')  # load Census Subdivision dataset



'''
1 - Create plot 
'''

# set up figure parameters
fig, ax = plt.subplots(dpi=1000)
plt.xlim(-500, 400)
plt.ylim(-570, 800)
ax.set_aspect(1) # prevent stretching of graph
ax.axis('off') # hide axes and box around plot

# plot cities
for DB in csd['DBuid/IDidu']:
    
    # get city location (km) and radius (km)
    location = (csd.loc[csd['DBuid/IDidu']==DB, 'horizontal_dist_km'].values[0],
                csd.loc[csd['DBuid/IDidu']==DB, 'vertical_dist_km'].values[0])
    
    radius = ( csd.loc[csd['DBuid/IDidu']==DB,'DBarea2016/IDsup2016'].values[0] / math.pi ) ** 0.5 # convert area to radius
    radius = min(radius, 2) # for better visualization
    
    circle = plt.Circle(location, radius, color='blue', alpha=0.3) # create circle object
    ax.add_artist(circle) # add to plot

# plot fires
years = [2011,2012,2013,2014,2015,2016,2017,2018] # plot fires in given years
list_of_fires = fires.index[fires['fire_year'].isin(years)] # index of fires in given years. must use index since fire_number is not unique

for fire in list_of_fires:

    # get fire location (km) and radius (km)
    location = (fires.loc[fire, 'horizontal_dist_km'],
                fires.loc[fire, 'vertical_dist_km'])
    
    radius = fires.loc[fire, 'radius_km'] 
    # radius = max(radius, 1) # alternative for better visualization by setting minimum fire size

    circle = plt.Circle(location, radius, color='red') # create circle object
    ax.add_artist(circle) # add to plot

plt.show()