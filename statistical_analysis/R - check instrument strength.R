# testing strength of lightning fires as an instrument
# using small city dataset (117 cities) which is not the main city dataset

library(dplyr)
library(tidyr)
library(ggplot2)

path <- 'C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/'

cities_clean_with_FE <- read.csv(paste(path, 'cities_clean_with_FE.csv', sep=''))

# single-year models
summary(lm(FE_2013 ~ FE_lightning_2013, data=cities_clean_with_FE))
summary(lm(FE_2014 ~ FE_lightning_2014, data=cities_clean_with_FE))
summary(lm(FE_2015 ~ FE_lightning_2015, data=cities_clean_with_FE))
summary(lm(FE_2016 ~ FE_lightning_2016, data=cities_clean_with_FE))
summary(lm(FE_2017 ~ FE_lightning_2017, data=cities_clean_with_FE))
summary(lm(FE_2018 ~ FE_lightning_2018, data=cities_clean_with_FE))

# combine years
FE_all_years <- cities_clean_with_FE %>%
  select(index, starts_with('FE_2')) %>%
  pivot_longer(cols=starts_with('FE_20'),
               names_to = 'year',
               names_prefix = 'FE_20',
               values_to = 'FE')

FE_all_years_lightning <- cities_clean_with_FE %>%
  select(index, starts_with('FE_light')) %>%
  pivot_longer(cols=starts_with('FE_light'),
               names_to = 'year',
               names_prefix = 'FE_lightning_20',
               values_to = 'FE_lightning')

FE_all_years['FE_lightning'] <- FE_all_years_lightning['FE_lightning']

# multi-year model
summary(lm(FE ~ FE_lightning, data=FE_all_years))



# plots
for (yr in 13:18) {

dat = filter(FE_all_years, year==yr)
  
plt <- ggplot(data=dat, aes(x=FE_lightning, y=FE)) +
  geom_point() +
  geom_smooth(formula='y~x', method=lm, se=F, color='black', alpha=0.5, size=0.6) +
  xlab(paste('Annual Lightning Fire Exposure 20', yr, sep='')) +
  ylab(paste('Annual Total Fire Exposure 20', yr, sep='')) +
  ggtitle(paste('Regression first stage for fires year 20', yr, sep='')) +
  theme_classic()

print(plt)
}






# quick plots
plot(x=FE_all_years$FE_lightning, y=FE_all_years$FE)
    ,xlim=c(0,10000000),
     ylim=c(0,10000000))
abline(a=0, b=1) # all points should be on or above abline of y=x since lightning fires is subset of all fires

plot(x=cities_clean_with_FE$FE_lightning_2013, y=cities_clean_with_FE$FE_2013)
plot(x=cities_clean_with_FE$FE_lightning_2014, y=cities_clean_with_FE$FE_2014)
plot(x=cities_clean_with_FE$FE_lightning_2015, y=cities_clean_with_FE$FE_2015)
plot(x=cities_clean_with_FE$FE_lightning_2016, y=cities_clean_with_FE$FE_2016)
plot(x=cities_clean_with_FE$FE_lightning_2017, y=cities_clean_with_FE$FE_2017)
plot(x=cities_clean_with_FE$FE_lightning_2018, y=cities_clean_with_FE$FE_2018)
