# Get Fire Exposure and Fire Exposure from Lightning for all CSDs
# match to panel data
# conclusion: does not appear to be any relation

library(dplyr)
library(tidyr)
library(plm)


# ------------------------------------------------------------------------------
# 1. Load dataset with fire exposure

load('csd_tall_01-200_FEL.Rda')


# ignore just load the file

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

# drop duplicates
alberta <- distinct(alberta, CSDuid.SDRidu, year, .keep_all=T)

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

alberta['air_mod_high_risk'] = alberta['air_mod_risk'] + alberta['air_high_risk']




# ------------------------------------------------------------------------------
# check correlations of air quality variables and outcome variables
library(corrplot)

#ab_for_cor <- alberta %>%
#  select(5,13:23,24,46:51)

ab_for_cor <- alberta %>%
  select(6,7,51:54,10,11,23,45:50)

names(ab_for_cor) <- abbreviate(names(ab_for_cor),minlength=5)

M <- cor(ab_for_cor, use="pairwise.complete.obs")
corrplot(M, method='number', type='lower')


#-------------------------------------------------------------------------------
# 5. First stage

# best first stage is to instrument high risk days (F-stat 11.61)
fs <- lm(air_high_risk ~ fe_lightning, data=alberta)
summary(fs)

library(ggplot2)
ggplot(data=alberta, aes(x=fe_lightning, y=air_high_risk))+
  geom_point()+
  geom_smooth(method='lm', se=F, color='black')+
  xlab('Annual Fire Exposure from Lightning')+
  ylab('Annual % Days High Risk Air Quality')+
  xlim(0,6500)+
  theme_classic()

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fs)
par(mfrow=c(1,1)) # change layout back




#-------------------------------------------------------------------------------
# 6. Panel models

# crimes per capita ------------------------------------------------------------

# OLS
OLS <- plm(crimes_per_cap ~ air_mod_high_risk,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(crimes_per_cap ~ air_mod_high_risk,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(crimes_per_cap ~ air_mod_high_risk | fe_lightning,
             data=alberta, index=c('CSDuid.SDRidu', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# tabulate results
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000,
          column.labels = c("OLS", "FE", "FE+IV"))



# violent crimes per capita ------------------------------------------------------------

# OLS
OLS <- plm(violent_crimes_per_cap ~ air_mod_high_risk,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(violent_crimes_per_cap ~ air_mod_high_risk,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(violent_crimes_per_cap ~ air_mod_high_risk | fe_lightning,
             data=alberta, index=c('CSDuid.SDRidu', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# tabulate results
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000,
          column.labels = c("OLS", "FE", "FE+IV"))


# death per capita ------------------------------------------------------------

# OLS
OLS <- plm(deaths_per_cap ~ air_mod_high_risk,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(deaths_per_cap ~ air_mod_high_risk,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(deaths_per_cap ~ air_mod_high_risk | fe_lightning,
             data=alberta, index=c('CSDuid.SDRidu', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# tabulate results
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000,
          column.labels = c("OLS", "FE", "FE+IV"))



# consumer bankruptcy per capita ------------------------------------------------------------

# OLS
OLS <- plm(cons_bankrupt_per_cap ~ air_mod_high_risk,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(cons_bankrupt_per_cap ~ air_mod_high_risk,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(cons_bankrupt_per_cap ~ air_mod_high_risk | fe_lightning,
             data=alberta, index=c('CSDuid.SDRidu', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# tabulate results
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000,
          column.labels = c("OLS", "FE", "FE+IV"))


# business bankruptcy per capita ------------------------------------------------------------

# OLS
OLS <- plm(buis_bankrupt_per_cap ~ air_mod_high_risk,
           data=alberta, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(buis_bankrupt_per_cap ~ air_mod_high_risk,
          data=alberta, index=c('CSDuid.SDRidu', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(buis_bankrupt_per_cap ~ air_mod_high_risk | fe_lightning,
             data=alberta, index=c('CSDuid.SDRidu', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# tabulate results
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000,
          column.labels = c("OLS", "FE", "FE+IV"))







# ------------------------------------------------------------------------------
# 5. Naive linear models

names(alberta)

var <- names(alberta)[47]
eq <- 'pm25'
eq <- names(alberta)[51]
formula <- as.formula(paste(var, eq, sep=' ~ '))


mod <- lm(formula, data=alberta)
summary(mod)
plot(alberta[[eq]], alberta[[var]], xlab=eq, ylab=var)
abline(mod)

mean(alberta[[eq]])
sd(alberta[[eq]])


#-------------------------------------------------------------------------------
# 6. Panel model testing

# remove duplicate rows
alberta_nd <- alberta %>% distinct(CSDuid.SDRidu, year, .keep_all=T)

var <- names(alberta)[48]
eq <- 'fire_exposure | fe_lightning'
eq <- names(alberta)[53]
formula <- as.formula(paste(var, eq, sep=' ~ '))


plm <- plm(formula, data=alberta_nd, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='within')

plm <- plm(births_per_cap ~ fire_exposure | fe_lightning, data=alberta_nd, index=c('CSDuid.SDRidu', 'year'),
           effect='twoways', model='within')

summary(plm)



nrow(unique(select(alberta, CSDuid.SDRidu, year)))
