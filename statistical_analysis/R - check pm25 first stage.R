# Get NAPS data with pm2.5 measure and Fire Exposure from Lightning
# condense to format suitable for analysis

library(dplyr)
library(plm)

# file location
dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file <- "naps_with_pm_and_FE.csv"
path <- paste(dir, file, sep='')

# ------------------------------------------------------------------------------
# 1. Load dataset with pm2.5 air quality and fire exposure

load('naps_FE_pm25_tall_v1.Rda')

# naps <- read.csv(path)
# 
# naps_tall <- naps %>%
#   group_by(City....Ville, Year) %>%
#   summarize(NAPS.ID....Identifiant.SNPA=first(NAPS.ID....Identifiant.SNPA),
#             CSDUID = first(CSDUID),
#             pm25 = mean(pm25),
#             FEL_month = mean(FE_lightning_monthly),
#             FEL_quarter = mean(FE_lightning_quarterly),
#             FEL_annual = mean(FE_lightning_annual))
# 
# save(naps_tall, file='naps_FE_pm25_tall_v1.Rda')
# write.csv(naps_tall, 'naps_FE_pm25_tall_v1.csv')


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
alberta <- left_join(naps_tall, alberta, by=c('CSDUID'='CSDUID', 'Year'='Period'))


# ------------------------------------------------------------------------------
# 3. Run models






