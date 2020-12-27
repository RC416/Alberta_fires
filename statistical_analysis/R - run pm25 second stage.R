# Get NAPS data with pm2.5 measure and Fire Exposure from Lightning
# condense to format suitable for analysis

library(dplyr)
library(plm)


# ------------------------------------------------------------------------------
# 1. Load dataset with pm2.5 air quality and fire exposure

load('naps_annual_pm25_FE.Rda')

# -------------------Ignore this code-------------------------------------------
# file location
# dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
# file <- "naps_with_pm_and_FE.csv"
# path <- paste(dir, file, sep='')

# naps <- read.csv(path)
 
#naps_tall <- naps %>%
#  group_by(CSDUID, Year) %>%
#  summarize(NAPS.ID....Identifiant.SNPA=first(NAPS.ID....Identifiant.SNPA),
#            City....Ville = first(City....Ville),
#            pm25 = mean(pm25),
#            FEL_month = mean(FE_lightning_monthly),
#            FEL_quarter = mean(FE_lightning_quarterly),
#            FEL_annual = mean(FE_lightning_annual))

#save(naps_tall, file='naps_FE_pm25_tall_v1.Rda')
#write.csv(naps_tall, 'naps_FE_pm25_tall_v1.csv')


# ------------------------------------------------------------------------------
# 2. Load Alberta dataset of outcomes

dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file <- "AlbertaData.csv"
path <- paste(dir, file, sep='')

alberta <- read.csv(path)

# cells with "." have no value; replace with NA so that R reads numeric properly.
# alberta[alberta == '.'] <- NA
# write.csv(alberta, path, row.names=F)

# match Alberta data to NAPS air quality and fire exposure
alberta <- left_join(naps_annual, alberta, by=c('CSDUID'='CSDUID', 'year'='Period'))

# ------------------------------------------------------------------------------
# 3. Load census data to get CSD population and other controls

load('census_data.Rda')

# select variables to add to alberta
census_data <- select(census_data, GeoUID, Population)

alberta <- merge(alberta, census_data, by.x='CSDUID', by.y='GeoUID')


# -----------------------------------------------------------------------------
# 4. Create useful derivative variables

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

save(alberta, file='alberta_panel.Dta')

# ------------------------------------------------------------------------------
# 5. Naive linear models

names(alberta)

var <- names(alberta)[49]
eq <- 'pm25'
eq <- names(alberta)[15]
formula <- as.formula(paste(var, eq, sep=' ~ '))


mod <- lm(formula, data=alberta)
summary(mod)
plot(alberta[[eq]], alberta[[var]], xlab=eq, ylab=var)
abline(mod)

mean(alberta[[eq]])
sd(alberta[[eq]])


# ------------------------------------------------------------------------------
# check correlations of air quality variables and outcome variables
library(corrplot)

#ab_for_cor <- alberta %>%
#  select(5,13:23,24,46:51)

ab_for_cor <- alberta %>%
  select(4,5,6,22,44:49)

names(ab_for_cor) <- abbreviate(names(ab_for_cor),minlength=5)

M <- cor(ab_for_cor, use="pairwise.complete.obs")
corrplot(M, method='number', type='lower')


#-------------------------------------------------------------------------------
# 6. Panel models

# crimes per capita ------------------------------------------------------------

# OLS
OLS <- plm(crimes_per_cap ~ pm25,
           data=alberta, index=c('CSDUID', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(crimes_per_cap ~ pm25,
          data=alberta, index=c('CSDUID', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(crimes_per_cap ~ pm25_hat,
             data=alberta, index=c('CSDUID', 'year'),
             effect='twoways', model='within')
summary(FE_iv)


# violent crimes per capita ----------------------------------------------------

# OLS
OLS <- plm(violent_crimes_per_cap ~ pm25,
           data=alberta, index=c('CSDUID', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(violent_crimes_per_cap ~ pm25,
          data=alberta, index=c('CSDUID', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(violent_crimes_per_cap ~ pm25_hat,
             data=alberta, index=c('CSDUID', 'year'),
             effect='twoways', model='within')
summary(FE_iv)



# deaths per capita ------------------------------------------------------------

# OLS
OLS <- plm(deaths_per_cap ~ pm25,
           data=alberta, index=c('CSDUID', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(deaths_per_cap ~ pm25,
          data=alberta, index=c('CSDUID', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(deaths_per_cap ~ pm25_hat,
             data=alberta, index=c('CSDUID', 'year'),
             effect='twoways', model='within')
summary(FE_iv)

# income ------------------------------------------------------------

# OLS
OLS <- plm(All.Families.Income..Median...in... ~ pm25,
           data=alberta, index=c('CSDUID', 'year'),
           effect='twoways', model='pooling')
summary(OLS)

# Fixed effects
FE <- plm(All.Families.Income..Median...in... ~ pm25,
          data=alberta, index=c('CSDUID', 'year'),
          effect='twoways', model='within')
summary(FE)

# Instrument + Fixed effects
FE_iv <- plm(All.Families.Income..Median...in... ~ pm25_hat,
             data=alberta, index=c('CSDUID', 'year'),
             effect='twoways', model='within')
summary(FE_iv)


# tabulate results -------------------------------------------------------------
library(stargazer)
stargazer(OLS, FE, FE_iv)

multiply_by_1000 <- function(x) (x*1000)
stargazer(OLS, FE, FE_iv, apply.coef=multiply_by_1000, apply.se=multiply_by_1000)
