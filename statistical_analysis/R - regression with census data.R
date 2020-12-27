# outcomes ~ FE | FE_lightning

# Load census data and get useful variables

# Match to CSD fire exposure


# ------------------------------------------------------------------------------
# 1. Get Census Data

load('census_clean.Rda')

# to get more variables use the API: https://censusmapper.ca/api#api_overview
# https://www.r-bloggers.com/2020/02/working-with-statistics-canada-data-in-r-part-4-canadian-census-data-cancensus-package-setup/

#install.packages("cancensus")
#library(cancensus)

#options(cancensus.api_key= 'CensusMapper_dc157d8bb8ed0342caa74a387198204d')
#options(cancensus.cache_path ="C:/Users/Ray/OneDrive/Economics/Economics Datasets")

#census_data <- get_census(dataset='CA16', regions=list(PR="48"),
#                          vectors=c("v_CA16_6719","v_CA16_6692","v_CA16_6707","v_CA16_6734",
#                                     "v_CA16_454","v_CA16_388","v_CA16_391","v_CA16_5330",
#                                     "v_CA16_5105","v_CA16_5102","v_CA16_5636","v_CA16_5615",
#                                     "v_CA16_5618","v_CA16_5612","v_CA16_4969", "v_CA16_557",
#                                     "v_CA16_545", "v_CA16_472"),
#                          labels="detailed", geo_format=NA, level='CSD')

#save(census_data, file='census_data.Rda')

# create clean dataset 
census_clean <- census_data[,1:3]                                         # CSDid and name
census_clean['pop'] = census_data['Population']                           # 2016 population
census_clean['house'] = census_data['Households']                         # number of households in CSD
census_clean['pct_movers_5'] = 1 - ( census_data[,11] / census_data['Population']) # [11] = count non-movers in past 5 years
census_clean['pct_movers_1'] = 1 - ( census_data[,14] / census_data['Population']) # [14] = count non-movers in past 1 year
census_clean['int_mig_1_pc'] = census_data[,12] / census_data['Population'] # per_cap internal migrants in last 1 year
census_clean['int_mig_5_pc'] = census_data[,13] / census_data['Population'] # per_cap internal migrants in last 5 years
census_clean['pct_married'] = census_data[15] / census_data['Population'] # percent married  
census_clean['pct_15_64'] = census_data[17] / 100                         # percent aged 15-64  
census_clean['pct_65'] = census_data[18] / 100                            # percent aged 65+  
census_clean['pct_hs'] = census_data[21] / census_data['Population']      # percent with high school diploma equivalent
census_clean['pct_ps'] = census_data[20] / census_data['Population']      # percent with postsecondary certificate, diploma or degree
census_clean['hrs_worked'] = census_data[22]                              # average weeks worked in reference year
census_clean['emp'] = census_data[23]                                     # employment rate
census_clean['unemp'] = census_data[24]                                   # unemployment rate
census_clean['part'] = census_data[25]                                    # participation rate
census_clean['income'] = census_data[26]                                  # average market income in 2015 among recipients
census_clean['pop_dens'] = census_data['Area (sq km)'] / census_data['Population'] # average market income in 2015 among recipients
census_clean['pct_eng'] = census_data[27] / census_data['Population']     # % English first language
census_clean['pct_divorced'] = census_data[16]/census_data['Population']  # % divorced

census_clean <- filter(census_clean, pop>0)

save(census_clean, file='census_clean.Rda')


# ------------------------------------------------------------------------------
# 2. Load dataset with fire exposure by CSD and match with Census Data

library(dplyr)

csd_with_FE <- read.csv('csd_clean_with_FE_all_fires.csv')

# join datasets by CSD id
census_FE <- merge(csd_with_FE, census_clean, by.x='CSDuid.SDRidu', by.y='GeoUID')

save(census_FE, file='csd_with_FE_census.Rda')

# ------------------------------------------------------------------------------
# 3. Run regressions

library(ivreg)

load('csd_with_FE_census.Rda')

# ------------------------------------------------------------------------------
# Income Regression

# for writing equations
var <- 'income'
controls <- 'pct_15_64 + pct_ps + pct_married'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
income_lm <- paste(var, controls, sep=' ~ ')
income_fe <- paste(var, fires, sep=' ~ ')
income_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
income_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
income_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

# run regressions
OLS_base <- lm(income_lm, data=census_FE)
OLS_no_con <- lm(income_fe, data=census_FE)
OLS_fe_con <- lm(income_fe_con, data=census_FE)

IV_no_con <- ivreg(income_iv, data=census_FE)
IV_con <- ivreg(income_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con)


# ------------------------------------------------------------------------------
# Migration Regression

var <- 'pct_movers_5'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)


# ------------------------------------------------------------------------------
#  Divorce regression

var <- 'pct_divorced'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)


# ------------------------------------------------------------------------------
#  unemployment regression

var <- 'unemp'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)



# ------------------------------------------------------------------------------
#  hours worked regression

var <- 'hrs_worked'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)


# ------------------------------------------------------------------------------
#  Participation regression

var <- 'part'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2016 + FE_2015 + FE_2014'
instruments <- 'FE_lightning_2016 + FE_lightning_2015 + FE_lightning_2014'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)



# ------------------------------------------------------------------------------
#  internal movers regression

var <- 'int_mig_5_pc'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2016 + FE_2015 + FE_2014'
instruments <- 'FE_lightning_2016 + FE_lightning_2015 + FE_lightning_2014'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)


# ------------------------------------------------------------------------------
#  employment rate regression

var <- 'emp'
controls <- 'pct_15_64 + pct_ps + pct_eng'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
movers_lm <- paste(var, controls, sep=' ~ ')
movers_fe <- paste(var, fires, sep=' ~ ')
movers_fe_con <- paste(var, paste(fires, controls, sep=' + '), sep=' ~ ')
movers_iv <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
movers_iv_con <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

OLS_base <- lm(movers_lm, data=census_FE)
OLS_no_con <- lm(movers_fe, data=census_FE)
OLS_fe_con <- lm(movers_fe_con, data=census_FE)

IV_no_con <- ivreg(movers_iv, data=census_FE)
IV_con <- ivreg(movers_iv_con, data=census_FE)

summary(OLS_base)
summary(OLS_no_con)
summary(OLS_fe_con)
summary(IV_no_con)
summary(IV_con)

# visualize results
library(stargazer)
multiply_by_1000000 <- function(x) (x*1000000)
stargazer(OLS_no_con, OLS_fe_con, IV_no_con, IV_con,
          apply.coef = multiply_by_1000000, apply.se=multiply_by_1000000)



# ------------------------------------------------------------------------------
# Income Regression "one by one" to find correlation with controls

# for writing equations
var <- 'income'
controls <- 'pct_15_64 + pct_ps + pct_married'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
income_1 <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
income_2 <- paste(var, paste(paste(fires, 'pct_ps', sep='+'), 
                             paste(instruments, 'pct_ps', sep='+'), sep=' | '), sep=' ~ ')
income_3 <- paste(var, paste(paste(fires, 'pct_15_64 + pct_ps', sep='+'),
                             paste(instruments, 'pct_15_64 + pct_ps', sep='+'), sep=' | '), sep=' ~ ')
income_4 <- paste(var, paste(paste(fires, 'pct_15_64 + pct_ps + pct_married', sep='+'),
                             paste(instruments, 'pct_15_64 + pct_ps + pct_married', sep='+'), sep=' | '), sep=' ~ ')
income_5 <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

# run regressions
IV_1 <- ivreg(income_1, data=census_FE)
IV_2 <- ivreg(income_2, data=census_FE)
IV_3 <- ivreg(income_3, data=census_FE)
IV_4 <- ivreg(income_4, data=census_FE)
IV_5 <- ivreg(income_5, data=census_FE)

summary(IV_5)

# visualize results
library(stargazer)
stargazer(IV_1, IV_2, IV_3, IV_4, IV_5)




# ------------------------------------------------------------------------------
# Income Regression "one by one" to find correlation with controls

# for writing equations
var <- 'income'
controls <- 'pct_15_64 + pct_ps + pct_married'
fires <- 'FE_2015 + FE_2014 + FE_2013'
instruments <- 'FE_lightning_2015 + FE_lightning_2014 + FE_lightning_2013'

# write regression formulas
income_1 <- paste(var, paste(fires,instruments,sep=' | '), sep=' ~ ')
income_2 <- paste(var, paste(paste(fires, 'pct_married', sep='+'), 
                             paste(instruments, '', sep='+'), sep=' | '), sep=' ~ ')
income_3 <- paste(var, paste(paste(fires, 'pct_married + pct_ps', sep='+'),
                             paste(instruments, 'pct_married + pct_ps', sep='+'), sep=' | '), sep=' ~ ')
income_4 <- paste(var, paste(paste(fires, 'pct_15_64 + pct_ps + pct_married', sep='+'),
                             paste(instruments, 'pct_15_64 + pct_ps + pct_married', sep='+'), sep=' | '), sep=' ~ ')
income_5 <- paste(var, paste(paste(fires, controls, sep='+'), paste(instruments, controls, sep='+'), sep=' | '), sep=' ~ ')

# run regressions
IV_1 <- ivreg(income_1, data=census_FE)
IV_2 <- ivreg(income_2, data=census_FE)
IV_3 <- ivreg(income_3, data=census_FE)
IV_4 <- ivreg(income_4, data=census_FE)
IV_5 <- ivreg(income_5, data=census_FE)

summary(IV_5)

# visualize results
library(stargazer)
stargazer(IV_1, IV_2, IV_3, IV_4, IV_5)




# ------------------------------------------------------------------------------
# Check correlations

library(corrplot)


ab_for_cor <- select(census_FE, 25:30, 35:51)

names(ab_for_cor) <- abbreviate(names(ab_for_cor),minlength=5)

M <- cor(ab_for_cor, , use="pairwise.complete.obs")
corrplot(M, method='number', type='lower')

