# naps with daily pm25 reading
naps <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/naps_with_pm_and_FE_daily.csv")

library(dplyr)

# create lag terms
for (n in 1:10) {
naps[paste('lag',n,sep='')] = c(rep(0,n), naps[['FE_light']])[1:75525]
}

# run OLS with and without lags
OLS <- lm(pm25 ~ FE_light, data=naps)
OLS_3lag <- lm(pm25 ~ FE_light + lag1 + lag2 + lag3, data=naps)
OLS_6lag <- lm(pm25 ~ . -lag7 -lag8 - lag9 - lag10 ,
          data=select(naps, pm25, FE_light, starts_with('lag')))


# check with fixed effects
library(plm)

FE <- plm(pm25 ~ FE_light, data=naps,
                  index=c('CSDUID'), effect='individual',
                  method='within')

FE_3lag <- plm(pm25 ~ FE_light + lag1 + lag2 + lag3, data=naps,
                  index=c('CSDUID'), effect='individual',
                  method='within')


# Get instrumented pm2.5 level with 3-lag OLS model
naps['pm2.5_hat_day'] <- fitted(OLS_3lag)
save(naps, file='naps_instrumend_daily.Rda')


# tabulate model results
library(stargazer)
stargazer(OLS, OLS_3lag, FE, FE_3lag)


# plot data
library(ggplot2)

ggplot(data=naps, aes(x=FE_light,y=pm25))+
  geom_point()+
  geom_smooth(method='lm', se=F, color='black')+
  xlab('Daily Fire Exposure from Lightning')+
  ylab('Daily Average PM2.5')+
  theme_classic()

ggplot(data=naps)+
  geom_point(aes(x=FE_light,y=pm25))+
  geom_point(aes(x=lag1,y=pm25))+
  geom_point(aes(x=lag2,y=pm25))+
  geom_point(aes(x=lag3,y=pm25))+
  geom_point(aes(x=lag4,y=pm25))+
  geom_point(aes(x=lag5,y=pm25))+
  geom_point(aes(x=lag6,y=pm25))+
  xlab('Fire Exposure from Lightning')+
  ylab('Average Daily PM2.5')+
  theme_classic()






