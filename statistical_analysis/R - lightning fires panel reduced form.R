# Get Fire Exposure and Fire Exposure from Lightning for all CSDs
# match to panel data
# conclusion: does not appear to be any relation

library(dplyr)
library(tidyr)
library(plm)

# clean dataset (see below for how it was created)
load('alberta_all_csd_with_fe.Rda')
# remove duplicate rows
alberta <- alberta %>% distinct(CSDuid.SDRidu, year, .keep_all=T)

#-------------------------------------------------------------------------------
# 1. Panel models

# crimes per capita ------------------------------------------------------------

# OLS
OLSc <- plm(crimes_per_cap ~ fe_lightning,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLSc)

# Fixed effects
FEc <- plm(crimes_per_cap ~ fe_lightning,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FEc)



# violent crimes per capita ----------------------------------------------------

# OLS
OLSvc <- plm(violent_crimes_per_cap ~ fe_lightning,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLSvc)

# Fixed effects
FEvc <- plm(violent_crimes_per_cap ~ fe_lightning,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FEvc)




# deaths per capita ------------------------------------------------------------

# OLS
OLSd <- plm(deaths_per_cap ~ fe_lightning,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLSd)

# Fixed effects
FEd <- plm(deaths_per_cap ~ fe_lightning,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FEd)


# income -----------------------------------------------------------------------

# OLS
OLSi <- plm(All.Families.Income..Median...in... ~ fe_lightning,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLSi)

# Fixed effects
FEi <- plm(All.Families.Income..Median...in... ~ fe_lightning,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FEi)


# bankruptch -----------------------------------------------------------------------

# OLS
OLSbb <- plm(buis_bankrupt_per_cap ~ fe_lightning,
            data=alberta, index=c('CSDuid.SDRidu', 'year'),
            effect='twoways', model='pooling')
summary(OLSbb)

# Fixed effects
FEbb <- plm(buis_bankrupt_per_cap ~ fe_lightning,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='within')
summary(FEbb)




# tabulate results -------------------------------------------------------------
library(stargazer)

# with income
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLSc, FEc, OLSvc, FEvc, OLSd, FEd, OLSi, FEi, apply.coef=multiply_by_1000000, apply.se=multiply_by_1000000)


# with bankruptch
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLSc, FEc, OLSvc, FEvc, OLSd, FEd, OLSbb, FEbb, apply.coef=multiply_by_1000000, apply.se=multiply_by_1000000)









################################################################################
# Creating the clean dataset
################################################################################

# ------------------------------------------------------------------------------
# 1. Load dataset with fire exposure

load('csd_tall_01-200_FEL.Rda')


# file location
dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file <- "csd_clean_with_FE.csv"
path <- paste(dir, file, sep='')

csd <- read.csv(path)

# convert from wide to tall for FE and FE lightning
csd_FE_tall <- csd %>%
  pivot_longer(cols=(starts_with('FE_20')),
               names_to=('year'),
               names_prefix=('FE_'),
               values_to=('fire_exposure'))
  
csd_FEL_tall <- csd %>%
  pivot_longer(cols=(starts_with('FE_lightning')),
                     names_to=('year'),
                     names_prefix=('FE_lightning_'),
                     values_to=('fe_lightning'))

# combine datasets
csd_FEL_tall['fire_exposure'] = csd_FE_tall['fire_exposure']
csd_tall <- select(csd_FEL_tall, -starts_with('FE_20'))
csd_tall['year'] = as.numeric(csd_tall[['year']])

save(csd_tall, file='csd_tall_01-200_FEL.Rda')



# ------------------------------------------------------------------------------
# 2. Load Alberta dataset of outcomes

dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file <- "AlbertaData.csv"
path <- paste(dir, file, sep='')

alberta <- read.csv(path)

# cells with "." have no value; replace with NA so that R reads numeric properly.
# alberta[alberta == '.'] <- NA
# write.csv(alberta, path, row.names=F)

# match Alberta data to CSD fire exposure
alberta <- left_join(csd_tall, alberta, by=c('CSDuid.SDRidu'='CSDUID', 'year'='Period'))
# might need: csd_tall['year'] = as.numeric(csd_tall[['year']])

# ------------------------------------------------------------------------------
# 3. Load census data to get CSD population and other controls

load('census_data.Rda')

# select variables to add to alberta
census_data <- select(census_data, GeoUID, Population)

alberta <- merge(alberta, census_data, by.x='CSDuid.SDRidu', by.y='GeoUID')


# -----------------------------------------------------------------------------
# 4. Create useful derivative variables

# drop CSD with no population
alberta <- filter(alberta, Population!=0)

alberta['births_per_cap'] = alberta['Births'] / alberta['Population']
alberta['deaths_per_cap'] = alberta['Deaths'] / alberta['Population']
alberta['crimes_per_cap'] = alberta['Crimes..reported.'] / alberta['Population']
alberta['violent_crimes_per_cap'] = alberta['Violent.Crimes..reported.'] / alberta['Population']
alberta['cons_bankrupt_per_cap'] = alberta['Consumer.Bankruptcies'] / alberta['Population']
alberta['buis_bankrupt_per_cap'] = alberta['Business.Bankruptcies'] / alberta['Population']

alberta['air_low_risk'] = alberta['Air.Quality.1..percent.high.quality.hours.'] +
                          alberta['Air.Quality.2..percent.high.quality.hours.'] +
                          alberta['Air.Quality.3..percent.high.quality.hours.']

alberta['air_mod_risk'] = alberta['Air.Quality.4..percent.high.quality.hours.'] +
                          alberta['Air.Quality.5..percent.high.quality.hours.'] +
                          alberta['Air.Quality.6..percent.high.quality.hours.']

alberta['air_high_risk'] = alberta['Air.Quality.7..percent.high.quality.hours.'] +
                           alberta['Air.Quality.8..percent.high.quality.hours.'] +
                           alberta['Air.Quality.9..percent.high.quality.hours.'] +
                           alberta['Air.Quality.10..percent.high.quality.hours.'] +
                           alberta['Air.Quality.10...percent.high.quality.hours.']

save(alberta, file='alberta_all_csd_with_fe.Rda')
load('alberta_all_csd_with_fe.Rda')











