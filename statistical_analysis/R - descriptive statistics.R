library(dplyr)

# raw fires dataset
dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file <- "Alberta Fires 2006-2018.csv"
path <- paste(dir,file,sep='')

raw_fires <- read.csv(path)

total_fires <- raw_fires %>%
  select(fire_year, size_class, general_cause_desc) %>%
  filter(fire_year %in% 2013:2016,
         size_class %in% c('B', 'C', 'D'),
         general_cause_desc=='Lightning') %>%
  select(fire_year) %>%
  table()

total_fires


# fire exposure by CSD
load('alberta_all_csd_with_fe.Rda')

for (yr in 2013:2016) {

fe <- alberta %>%
  filter(year==yr) %>%
  select(fe_lightning)

print(yr)
print(mean(fe[[1]]))
print(sd(fe[[1]]))
print(IQR(fe[[1]]))
}


# census variables
load('csd_with_FE_census.Rda')

values <- na.omit(select(census_FE, hrs_worked))
mean(values[[1]])
sd(values[[1]])
quantile(values[[1]], c(0.25, 0.50, 0.75))


fires <- census_FE %>%
  select(FE_2013,FE_2014,FE_2015,FE_2016) %>%
  pivot_longer(cols=starts_with('FE'),
               names_to='FE')

mean(fires[['value']])
sd(fires[['value']])
quantile(fires[['value']], c(0.25, 0.50, 0.75))

hist(fires[['value']])  


# panel data
values <- na.omit(select(alberta, air_high_risk))
values <- filter(values, crimes_per_cap<5)
mean(values[[1]])
sd(values[[1]])
quantile(values[[1]], c(0.25, 0.50, 0.75))


# 

load('alberta_panel.Dta')

values <- na.omit(select(alberta, pm25))
mean(values[[1]])
sd(values[[1]])
quantile(values[[1]], c(0.25, 0.50, 0.75))

