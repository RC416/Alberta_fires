
naps <- read.csv("C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/naps_with_pm_and_FE.csv")

# create lag terms
for (n in 1:4) {
  naps[paste('lag',n,sep='')] = c(rep(0,n), naps[['FE_lightning_monthly']])[1:2770]
}

# run OLS with and without lags
OLS <- lm(pm25 ~ FE_lightning_monthly, data=naps)
OLS_lag1 <- lm(pm25 ~ FE_lightning_monthly + lag1,
               data=naps)
OLS_lag2 <- lm(pm25 ~ FE_lightning_monthly + lag1 + lag2,
               data=naps)
OLS_lag3 <- lm(pm25 ~ FE_lightning_monthly + lag1 + lag2 + lag3,
               data=naps)
OLS_lag4 <- lm(pm25 ~ FE_lightning_monthly + lag1 + lag2 + lag3 + lag4,
               data=naps)

# check with fixed effects
library(plm)

FE <- plm(pm25 ~ FE_lightning_monthly, data=naps,
          index=c('CSDUID'), effect='individual',
          method='within')

FE_1lag <- plm(pm25 ~ FE_lightning_monthly + lag1, data=naps,
               index=c('CSDUID'), effect='individual',
               method='within')
FE_2lag <- plm(pm25 ~ FE_lightning_monthly + lag1 + lag2, data=naps,
               index=c('CSDUID'), effect='individual',
               method='within')
FE_3lag <- plm(pm25 ~ FE_lightning_monthly + lag1 + lag2 + lag3, data=naps,
               index=c('CSDUID'), effect='individual',
               method='within')
FE_4lag <- plm(pm25 ~ FE_lightning_monthly + lag1 + lag2 + lag3 + lag4, data=naps,
               index=c('CSDUID'), effect='individual',
               method='within')

# Get instrumented pm2.5 level with OLS model
naps['pm25_hat_month_OLS'] <- fitted(OLS)
naps['pm25_hat_month_plm'] <- fitted(FE_1lag)
save(naps, file='naps_instrumend_monthly.Rda')

# tabulate results
library(stargazer)
stargazer(OLS, OLS_lag1, OLS_lag2, OLS_lag3, OLS_lag4, FE, FE_1lag, FE_2lag, FE_3lag, FE_4lag)

# plot data
library(ggplot2)

ggplot(data=naps, aes(x=FE_lightning_monthly, y=pm25))+
  geom_point()+
  geom_smooth(method='lm', se=F, color='black')+
  xlab('Monthly Average Fire Exposure from Lightning')+
  ylab('Monthly Average PM2.5')+
  theme_classic()


par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(OLS)
par(mfrow=c(1,1)) # change layout back
