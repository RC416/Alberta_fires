# Plots and descriptive statistics for fire dataset

library(ggplot2)
library(dplyr)

# get working director and clean fires dataset
dir <- "C:/Users/Ray/OneDrive/Economics/Course Material/Empirical Microeconomics Shared Folder/Datasets/"
file_name <- 'fires_clean.csv'
path <- paste(dir, file_name, sep='')

fires <- read.csv(path)


# plot of all fires
ggplot(data=fires,aes(x=current_size)) +
  geom_histogram() +
  # geom_density() +
  theme_classic()

# fires class A,B,C,D (excluding class E)
ad_fires <- filter(fires, current_size<200, current_size>1)

ggplot(data=ad_fires,aes(x=current_size)) +
  geom_histogram(bins=100) +
  # geom_density() +
  theme_classic()

# fires class A,B,C
ac_fires <- filter(fires, current_size<40, current_size>0.1)

ggplot(data=ac_fires,aes(x=current_size)) +
  geom_histogram(bins=100) +
  # geom_density() +
  theme_classic()


# Looking at fire size and size class
summary(fires['current_size'])

# total fires of each type
table(fires['size_class'])
# per year averages
round(table(fires['size_class']) / (2018 - 2006))

