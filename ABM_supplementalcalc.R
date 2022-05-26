# Code Purpose: Pre-Processing Work for the Water Consumption ABM
# Code By: Renee Obringer
# Code Run: 13 May 2022

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load the hydroclimatic input data
# DATA PRE-PROCESSING: spatiotemporal aggregation, unit conversion, etc.
# WATER CONSUMPTION CALCULATION: using Wang et al. 2021, calculate water consumption for all survey respondents
# FIT DISTRIBUTIONS: fit distirbutions for all hydrocliamtic variables and water consumption
# FIGURES AND TABLES: code for plotting figures and creating tables included in manuscript 

rm(list=ls())
options(scipen = 999)

# libraries
library(hydroGOF)      # for NRMSE calculations
library(lubridate)     # for working with dates
library(reshape2)      # for data pre-processing
library(fitdistrplus)  # for fitting distributions
library(markovchain)   # for fitting distributions
library(dplyr)         # for data processing
library(ggplot2)       # for plotting

# set file path
# NOTE: set this path to the folder on your personal machine which contains the downloaded data 
# for example: path <- '/Users/obringer/Downloads/WaterConsumptionABM'

path <- '/Users/rqo5125/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/GitHub/private/WaterConsumptionABM' # main file path

# set directories
maindir <- path                                    # main directory
datadir <- paste(path, '/hydrodata/', sep = '')    # directory for storing the hydroclimatic data
rdatadir <- paste(path, '/rdatafiles/', sep = '')  # directory for storing rdata files

# OPTIONAL: create an output directory
outputdir <- paste(path,'/output/', sep = '')    # The output directory will be where you send any non-rdata output files (e.g., csv, pdf, etc.)
dir.create(outputdir)

########## LOAD DATA ################

# CAP data (Lake Pleasant)
setwd(paste(datadir, '/CAP_Phoenix', sep=''))
lakepleasant <- read.csv('LakePleasant_WSELEV_VOL_01012010_01312021.csv')
LPelev <- lakepleasant[,1:2]; LPvol <- lakepleasant[,c(1,3)]

# SRP data (Lakes Apache, Bartlett, Canyon, Horseshoe, Roosevelt, Saguaro)
setwd(paste(datadir, '/SaltRiverProject_Phoenix', sep=''))
Aelev <- read.csv('Apache_elevation.csv', skip = 19, header = T); Avol <- read.csv('Apache_volume.csv', skip = 19, header = T)
Belev <- read.csv('Bartlett_elevation.csv', skip = 19, header = T); Bvol <- read.csv('Bartlett_volume.csv', skip = 19, header = T)
Celev <- read.csv('Canyon_elevation.csv', skip = 19, header = T); Cvol <- read.csv('Canyon_volume.csv', skip = 19, header = T)
Helev <- read.csv('Horseshoe_elevation.csv', skip = 19, header = T); Hvol <- read.csv('Horseshoe_volume.csv', skip = 19, header = T)
Relev <- read.csv('Roosevelt_elevation.csv', skip = 19, header = T); Rvol <- read.csv('Roosevelt_volume.csv', skip = 19, header = T)
Selev <- read.csv('Saguaro_elevation.csv', skip = 19, header = T); Svol <- read.csv('Saguaro_volume.csv', skip = 19, header = T)

# reservoir inflow (SRP)
Rflow1 <- read.csv('flow_salt_roosevelt.csv', skip = 29, header = T)
Rflow2 <- read.csv('flow_tonto_roosevelt.csv', skip = 29, header = T)
Hflow <- read.csv('flow_verde_horseshoe.csv', skip = 29, header = T)

# precipitation
setwd(datadir)
precipitation <- read.csv('precip.csv')

# water use
setwd(paste(datadir, '/wateruse/', sep=''))
wateruse <- read.csv('water.csv')
wu2018to2020 <- read.csv('WaterPerfReport.csv')

# evaporation 
setwd(datadir)
evaporation <- read.csv('evaporation_asu.csv', skip = 1, header = T)

# save rdata
setwd(rdatadir)
save.image('rawdata.rdata')

########## DATA PRE-PROCESSING ###############

# OPTIONAL: load rdata with raw data files
setwd(rdatadir)
load('rawdata.rdata')

# DESCRIPTION:
# This section of code is for pre-processing the hydroclimatic data. Each variable 
# has different treatments. For example, the raw storage data is a mix of stage 
# (water level) and volume. We first merge these two values into a single dataframe, 
# then we use linear regression to determine the relationship between the two
# measures. This relationship is used to interpolate between periods in which 
# the stage data was recorded, but the volume data was not. Then the volume data 
# is aggregated to to daily values and summed over all the reservoirs.
#
# The remaining variables are spatially aggregated and the units are converted from 
# US customary to metric. 


# STORAGE PRE-PROCESSING

# merge datasets
Apache <- merge(Avol, Aelev[1:length(Avol$ReadValue),], by = 'ReadDateTime')
Bartlett <- merge(Bvol, Belev[1:length(Bvol$ReadValue),], by = 'ReadDateTime')
Canyon <- merge(Cvol, Celev[1:length(Cvol$ReadValue),], by = 'ReadDateTime')
Horseshoe <- merge(Hvol, Helev[1:length(Hvol$ReadValue),], by = 'ReadDateTime')
Roosevelt <- merge(Rvol, Relev[1:length(Rvol$ReadValue),], by = 'ReadDateTime')
Saguaro <- merge(Svol, Selev[1:length(Svol$ReadValue),], by = 'ReadDateTime')

# get equations using linear regression
Alm <- lm(Apache$ReadValue.x ~Apache$ReadValue.y); Blm <- lm(Bartlett$ReadValue.x ~ Bartlett$ReadValue.y)
Clm <- lm(Canyon$ReadValue.x ~ Canyon$ReadValue.y); Hlm <- lm(Horseshoe$ReadValue.x ~ Horseshoe$ReadValue.y)
Rlm <- lm(Roosevelt$ReadValue.x ~ Roosevelt$ReadValue.y); Slm <- lm(Saguaro$ReadValue.x ~ Saguaro$ReadValue.y)

# check results to see if linear regression was a good fit
summary(Alm); summary(Blm); summary(Clm); summary(Hlm); summary(Rlm); summary(Slm)
# Adj R^2: A - 1; B - 0.9983; C - 1; H - 0.9616; R - 0.9997; S - 0.9999
# CONCLUSION: Very high goodness-of-fit

# calculate volume using equations
Avol_full <- Alm$coefficients[2]*Aelev[,2] + Alm$coefficients[1]
Bvol_full <- Blm$coefficients[2]*Belev[,2] + Blm$coefficients[1]
Cvol_full <- Clm$coefficients[2]*Celev[,2] + Clm$coefficients[1]
Hvol_full <- Hlm$coefficients[2]*Helev[,2] + Hlm$coefficients[1]
Rvol_full <- Rlm$coefficients[2]*Relev[,2] + Rlm$coefficients[1]
Svol_full <- Slm$coefficients[2]*Selev[,2] + Slm$coefficients[1]

# check accuracy using NRMSE
nrmse(Avol_full[1:length(Avol$ReadValue)], Avol[,2], norm = "maxmin") # 0.2%
nrmse(Bvol_full[1:length(Bvol$ReadValue)], Bvol[,2], norm = "maxmin") # 1.7%
nrmse(Cvol_full[1:length(Cvol$ReadValue)], Cvol[,2], norm = "maxmin") # 0.1%
nrmse(Hvol_full[1:length(Hvol$ReadValue)], Hvol[,2], norm = "maxmin") # 6.8%
nrmse(Rvol_full[1:length(Rvol$ReadValue)], Rvol[,2], norm = "maxmin") # 0.5%
nrmse(Svol_full[1:length(Svol$ReadValue)], Svol[,2], norm = "maxmin") # 0.2%
# CONCLUSION: Acceptable levels of error

# temporally aggregate data to daily
dates <- as.Date(sapply(strsplit(Aelev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Avol_final <- aggregate(data.frame(dates, Avol_full), by = list(dates), FUN = mean)
dates <- as.Date(sapply(strsplit(Belev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Bvol_final <- aggregate(data.frame(dates, Bvol_full), by = list(dates), FUN = mean)
dates <- as.Date(sapply(strsplit(Celev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Cvol_final <- aggregate(data.frame(dates, Cvol_full), by = list(dates), FUN = mean)
dates <- as.Date(sapply(strsplit(Helev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Hvol_final <- aggregate(data.frame(dates, Hvol_full), by = list(dates), FUN = mean)
dates <- as.Date(sapply(strsplit(Relev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Rvol_final <- aggregate(data.frame(dates, Rvol_full), by = list(dates), FUN = mean)
dates <- as.Date(sapply(strsplit(Selev$ReadDateTime," "), `[`, 1),"%m-%d-%Y"); Svol_final <- aggregate(data.frame(dates, Svol_full), by = list(dates), FUN = mean)

# spatially aggregate data to form one mega-reservoir for modeling purposes
allVolume <- data.frame(Avol_final$dates, Avol_final$Avol_full, Bvol_final$Bvol_full, Cvol_final$Cvol_full, Hvol_final$Hvol_full, 
                        Rvol_final$Rvol_full, Svol_final$Svol_full)
names(allVolume) <- c('Date','Apache','Bartlett','Canyon','Horseshoe','Roosevelt','Saguaro')
totalVol <- data.frame(allVolume$Date, rowSums(allVolume[,2:7]))

# convert units and rename columns
totalVol[,3] <- totalVol[,2]*1233.48  # ac-ft --> m^3
names(totalVol) <- c("date","ac-ft","m3")

# STREAMFLOW PRE-PROCESSING

# spatially aggregate flows
Rflow <- data.frame(Rflow1$X20d, Rflow1$X14n + Rflow2$X14n); names(Rflow) <- c("date","cfs")
Hflow <- data.frame(Hflow$X20d, Hflow$X14n); names(Hflow) <- c("date","cfs")
totalFlow <- data.frame(Rflow$date, Rflow$cfs + Hflow$cfs)

# convert units and rename columns
totalFlow[,3] <- totalFlow[,2]*0.0283168  # cfs --> m^3/s
totalFlow[,4] <- totalFlow[,3]*86400      # m^3/s --> m^3/day
names(totalFlow) <- c("date","cfs","m3s-1","m3")

# PRECIPITATION PRE-PROCESSING

# extract columns of interest
precip <- precipitation[,c(1,4)]; precip[,1] <- as.Date(precip[,1],"%B %d,%Y")

# convert units 
precip[,3] <- precip[,2]*0.0254  # in --> m

# spatially aggregate and rename
surfarea <- 29992062 + 10391032 + 8153282.6 + 3843542.4 + 3196045.3 + 3194232.3 + 5115226.5 # surface area of Lake Pleasant, Apache, Bartlett, Canyon, Horseshoe, Roosevelt, Saguaro, respectively
precip[,4] <- precip[,3]*surfarea   # m --> m^3
names(precip) <- c("date","in","m","m3")

# WATER USE PRE-PROCESSING

# reformat dates and extract period of interest for pre-2018 data
wateruse$month <- as.Date(wateruse$month, "%m/%d/%y")
consumption <- data.frame(wateruse$month, wateruse$MG); names(consumption) <- c('date','MGal')
consumption <- consumption[with(consumption, format(as.Date(date), "%Y") >= "2010"), ]
dates <- data.frame(date=seq(as.Date("2010-01-01"), as.Date("2018-06-01"), by="days"))

# merge data to get daily values
dailycons <- merge(consumption,dates,by.x='date',by.y='date',all.x=T,all.y=T)
dailycons <- na.locf(dailycons)

# convert MGal/month to MGal/day
dailycons[,2] <- dailycons$MGal/days_in_month(dailycons$date)

# reformat dates and extract period of interest for post-2018 data
c2018to2020 <- wu2018to2020[,c(1,8)]; c2018to2020$Date <- as.Date(c2018to2020$Date, '%m/%d/%y')
c2018to2020 <- c2018to2020[which(c2018to2020$Date > '2018-06-01'),]; names(c2018to2020) <- c('date','MGal')

# merge both periods together
dc_allyears <- rbind(dailycons, c2018to2020)

# convert units and rename columns
dc_allyears[,3] <- dc_allyears$MGal*1000000    # MGal --> gal
dc_allyears[,4] <- dc_allyears[,3]*0.00378541  # gal --> m^3
names(dc_allyears) <- c('date','MGal','Gal','m3')

# EVAPORATION PRE-PROCESSING

# reformat dates
evap <- evaporation[-c(367:368), c(1,30:40)]; evap <- melt(evap, id = c("Day"))
evap[,2] <- gsub("X", "", as.character(evap$variable)); evap[,2] <- paste(evap$Day, evap$variable, sep="-")
evap[,2] <- as.Date(evap$variable, format = "%d-%b-%Y"); evap <- evap[-grep("^ --", evap$value),]
evap <- evap[-grep("^--", evap$value),]
leapyears <- which(is.na(evap$variable)); evap[leapyears[1],2] <- as.Date("2012-02-29", format = "%Y-%m-%d")
evap[leapyears[2],2] <- as.Date("2016-02-29", format = "%Y-%m-%d"); evap[leapyears[3],2] <- as.Date("2020-02-29", format = "%Y-%m-%d")
evap <- evap[-1]

# convert units, spatially aggregate over reservoirs, and rename columns
evap[,3] <- as.numeric(evap[,2])*0.0254  # in --> m
evap[,4] <- evap[,3]*surfarea            # m --> m^3
names(evap) <- c("date","in","m","m3")

# replace NA values with previous observation
evap <- na.locf(evap)

# save rdata file
setwd(rdatadir)
save.image('processedhydrodata.RDATA')

########## WATER CONSUMPTION CALCULATION ####################

# OPTIONAL: load rdata file
setwd(rdatadir)
load('processedhydrodata.rdata')

# DESCRIPTION:
# Calculating the water consumption for all the suvery respondents using the 
# equation developed by Wang et al. 2021 (DOI: 10.1016/j.resconrec.2021.105520)

# read in maximum temperature data (from NCEI)
setwd(datadir)
maxtemp <- read.csv('maxtemp.csv')

# reformat date
maxtemp <- maxtemp[which(as.Date(maxtemp$DATE) >= "2010-01-01"),c(3:4)]

# convert units and rename columns
maxtemp[,3] <- (maxtemp[,2] - 32)*5/9 # degF --> degC
names(maxtemp) <- c('date','degF','degC')

# convert m --> mm for both precipiation and evaporation + rename and remove NA values
precip[,5] <- precip[,3]*1000; names(precip)[5] <- 'mm'; precip <- na.omit(precip)
evap[,5] <- evap[,3]*1000; names(evap)[5] <- 'mm'

# calculate antecedent precipitation index
api <- c()
api[1] <- 0.95*precip[1,5]
for (i in 2:length(precip[,1])) {
  # decay value = 0.95, as recommended by Hill et al. 2014
  api[i] <- 0.95*api[i-1] + precip[i,5]
}

# calculate the number of continuous days without rain
nd <- c()
nd[1] <- 0
for (i in 2:length(precip[,1])) {
  if (precip[i,5] == 0) {
    nd[i] <- nd[i-1] + 1
  } else {
    nd[i] <- 0
  }
}

# find income 
annualincome <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2019-01-01"), by = "years"), 
                           income = c(48823,48596,47866,47139,46881,47326,49328,52080,54765,57549))

dailydates <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by = "days"))

income_int <- merge(annualincome, dailydates, by = 'date',by.x='date',by.y='date',all.x=T,all.y=T)
income_int <- na.locf(income_int)
income_int[,3] <- income_int[,2]/12; names(income_int)[c(2,3)] <- c('annualIncome','monthlyIncome')

# find water price (estimated from CAP website)
waterprice <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2019-01-01"), by = "years"),
                         price = c(318,403,437,508,575,644,660,704,675,727)/1233.48) # $/ac-ft to $/m3

dailydates <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by = "days"))

price_int <- merge(waterprice, dailydates, by = 'date',by.x='date',by.y='date',all.x=T,all.y=T)
price_int <- na.locf(price_int); names(price_int)[2] <- 'price'

# set up multiple linear regression (following Wang et al. 2021 methods)

mydata <- data.frame(date = maxtemp$date[1:3652], wateruse = dc_allyears$m3[1:3652], maxtemp = maxtemp$degC[1:3652], precipitation = precip$mm[1:3652],
                     evaporation = evap$mm[1:3652], api = api[1:3652], drydays = nd[1:3652], income = income_int$monthlyIncome,
                     price = price_int$price)

mlr_fit <- lm(wateruse ~ maxtemp + precipitation + evaporation + api + drydays + income + price, data = mydata)
summary(mlr_fit)
plot(mlr_fit) # results of this plot indicate there are a number of outliers

# remove outliers
outliers <- which(mlr_fit$residuals > 4000000)
updateddata <- mydata[-outliers,]

mlr_fit <- lm(wateruse ~ maxtemp + precipitation + evaporation + api + drydays + income + price, data = updateddata)
summary(mlr_fit)
plot(mlr_fit) # much better fit

# OPTIONAL: save data for input to ABM
clim_se_data <- mydata[,-2]
setwd(paste(maindir, '/inputData/', sep = ''))
write.csv(clim_se_data,'PhoenixClimateSocioEconData.csv')

# use Wang et al. 2021 to get water consumption estimates for each respondent

# load data from archetypes analysis (under review)
setwd(rdatadir)
load('clusteringanalysis.rdata')

# separate income data
surveyincome <- as.numeric(surveydata$q18)

# get specific income from categories (assuming uniform distribution)
for (i in 1:length(surveyincome)) {
  if (is.na(surveyincome[i])) {
    next
  } else if (surveyincome[i] == 1) {
    surveyincome[i] <- runif(1,1000,20000)
  } else if (surveyincome[i] == 2) {
    surveyincome[i] <- runif(1,20001,40000)
  } else if (surveyincome[i] == 3) {
    surveyincome[i] <- runif(1,40001,60000)
  } else if (surveyincome[i] == 4) {
    surveyincome[i] <- runif(1,60001,80000)
  } else if (surveyincome[i] == 5) {
    surveyincome[i] <- runif(1,80001,100000)
  } else if (surveyincome[i] == 6) {
    surveyincome[i] <- runif(1,100001,300000)
  }
}

# extract monthly data in 2017
data2017 <- mydata[which(mydata$date >= '2017-01-01' & mydata$date <= '2017-12-31'),]
data2017[,10] <- month(data2017$date)
mondata2017 <- aggregate(data2017, by = list(data2017$V10), FUN = mean)
mondata2017 <- mondata2017[,-c(1:2)]; names(mondata2017)[9] <- 'month'

# merge survey income data with weather data for each month in 2017
sdata <- list()
for (i in 1:12) {
  sdata[[i]] <- data.frame(mondata2017 %>% slice(i, each = length(surveyincome)),surveyincome/12)
  sdata[[i]] <- sdata[[i]][,-c(1,7,9)]
  names(sdata[[i]])[c(7)] <- c('income')
}

# predict water use for each respondent in each month in 2017
waterusePred <- list()
for (i in 1:12) {
  waterusePred[[i]] <- predict(mlr_fit, newdata = sdata[[i]])
}

# get means by archetype by month
avgwaterusePC <- list()
for (i in 1:12) {
  cdata <- data.frame(wateruse = waterusePred[[i]], node = model$unit.classif)
  avgwateruse <- aggregate(cdata$wateruse, by = list(cdata$node), FUN = mean, na.rm=TRUE, na.action=NULL)
  avgwateruse <- data.frame(avgwateruse, cluster = clusters)
  avgwaterusePC[[i]] <- aggregate(avgwateruse$x, by = list(avgwateruse$cluster), FUN = mean)
}

setwd(rdatadir)
save.image('waterconsumptioncalc.rdata')

########## FIT DISTRIBUTIONS ####################

# OPTIONAL: load rdata file
setwd(rdatadir)
load('processedhydrodata.rdata')
load('waterconsumptioncalc.rdata')

# rename variables and remove/replace NA values
S <- totalVol[,3]; P <- precip[,4]; Q <- totalFlow[,4]; W <- dc_allyears[,4]; E <- evap[,4]
P <- na.omit(P); W <- na.locf(W)

# unaccounted for losses 
L <- c()
for (i in 2:4018) {
  L[i] <- S[i-1] - S[i] + P[i] + Q[i] - W[i] - E[i]
}

# check water balance
Smod<- c(); Smod[1] <- S[1]
for (i in 2:4018) {
  Smod[i] <- Smod[i-1] + P[i] + Q[i] - W[i] - E[i] - L[i]
}

nrmse(Smod, S[1:4018]) # check accuracy of data-driven water balance

alldata <- data.frame(S[1:4018], P[1:4018], Q[1:4018], W[1:4018], E, L[1:4018]) # 1/1/2010 - 12/31/2020
names(alldata) <- c('S','P','Q','W','E','L')

# save data for input to ABM
setwd(paste(maindir,'/inputData/',sep=''))
write.csv(alldata,'PhoenixWaterBalData.csv')

# Get distributions of data

# PRECIPITATION

# wet/dry days = first order Markov chain [see Acharya et al. 2017 for justification]
Pnum <- P; Pnum[which(Pnum != 0)] <-  1
mcPnum <- markovchainFit(Pnum)
mcPrecip <- new("markovchain", states = c("dry","wet"),
                transitionMatrix = matrix(c(mcPnum$estimate[1,1],mcPnum$estimate[2,1],
                                            mcPnum$estimate[1,2],mcPnum$estimate[2,2]), nrow = 2), 
                name = "Precipitation")
predPnum <- rmarkovchain(n = 4061, object = mcPrecip, t0 = "dry")
predPnum[which(predPnum == "dry")] <- 0; predPnum[which(predPnum == "wet")] <- 1
predPnum <- as.numeric(predPnum)

length(which(predPnum == 1)); length(which(Pnum == 1))

# precipitation magnitude: gamma distribution
Pmag <- data.frame(precip[which(precip[,3] > 0),3]); names(Pmag) <- c("P") # precip in meters
Pfit <- fitdist(Pmag$P, "gamma")
Pest <- rgamma(4061, shape = Pfit$estimate[1], scale = Pfit$estimate[2])

# STREAMFLOW

# gamma distribution 
Qfit <- fitdist(Q, "gamma", method = 'mme')
Qest <- rgamma(4064, shape = Qfit$estimate[1], rate = Qfit$estimate[2])

# EVAPORATION

E <- evap[,4]

# gamma distribution 
Efit <- fitdist(E, "gamma", method = 'mme')
Eest <- rgamma(4061, shape = Efit$estimate[1], rate = Efit$estimate[2])

# LOSSES

L <- na.omit(L)

# normal distribution
Lfit <- fitdist(as.numeric(L), "norm")
Lest <- rnorm(4061, mean = Lfit$estimate[1], sd = Lfit$estimate[2])

# WATER USE

# get distributions by archetype by month
distwaterusePC <- list()
for (i in 1:12) {
  cdata <- data.frame(wateruse = waterusePred[[i]], node = model$unit.classif, cluster = NA)
  for (j in 1:30) {
    cdata[which(cdata$node == j), 3] <- clusters[j]
  }
  waterusedist <- list()
  for (k in 1:7) {
    waterusedist[[k]] <- cdata[which(cdata$cluster == k),]
  }
  distwaterusePC[[i]] <- waterusedist
}

# fit gamma distributions
gammafit <- list()
seasons <- list(c(12, 1, 2, 3), c(4, 5, 10, 11), c(6, 7, 8, 9))
for (i in 1:3) {
  gammafit1 <- list()
  for (j in 1:7) {
    wdata <- c(na.omit(distwaterusePC[[seasons[[i]][1]]][[j]]$wateruse), 
               na.omit(distwaterusePC[[seasons[[i]][2]]][[j]]$wateruse), 
               na.omit(distwaterusePC[[seasons[[i]][3]]][[j]]$wateruse),
               na.omit(distwaterusePC[[seasons[[i]][4]]][[j]]$wateruse))
    gammafit1[[j]] <- fitdist(as.numeric(wdata), "gamma", method = 'mme')
  }
  gammafit[[i]] <- gammafit1
}

# save rdata file
setwd(rdatadir)
save.image('distributions.rdata')

########## FIGURES AND TABLES ##################

# read in results from ABM model
setwd(maindir)
abmresults <- read.csv('experimentresults.csv', skip = 6, stringsAsFactors=FALSE)

# read in actual data
actualdata <- read.csv(paste(maindir, '/inputData/PhoenixWaterBalData.csv', sep = ''))

# data pre-processing

baseline <- abmresults[-c(1:10),which(abmresults[1,] == 'base')]              # baseline scenario: observed proportion of archetypes
partialpart <- abmresults[-c(1:10),which(abmresults[1,] == 'partial-part')]   # partially participatory scenario: 50% of archetype 3 --> 5
particpatory <- abmresults[-c(1:10),which(abmresults[1,] == 'part')]          # participatory scenario: 100% of archetype 3 --> 5
individualistic <- abmresults[-c(1:10),which(abmresults[1,] == 'indiv')]      # individualistic scenario: 100% of archetype 6 --> 2
concerned <- abmresults[-c(1:10),which(abmresults[1,] == 'concern')]          # concerned scenario: 100% of archetype 7 --> 4
combined <- abmresults[-c(1:10),which(abmresults[1,] == 'all-changes')]       # combined scenario: all of the above changes (100% of 3 --> 5, 6 --> 2, and 7 --> 4)

# FIGURE 1

# calculate average
baseline_matrix <- matrix(as.numeric(unlist(baseline)), nrow = 3500, byrow = F)
average <- rowMeans(baseline_matrix)

# convert matrix to data frame
baseline_df <- as.data.frame(baseline_matrix)

# rename columns in baseline_df
names(baseline_df) = gsub(pattern = "V*", replacement = "", x = names(baseline_df))

# prep plotting data
timestep <- c(1:3500)
alldata_baseline <- cbind(baseline_df, average, timestep, actualdata[1:3500,1])

# rearrange data to long form for plotting
plotdata <- data.frame(melt(alldata_baseline, id.vars = 'timestep', variable.name = 'ModelRun', value.name = 'Storage'))

# add in variable for distinguishing average from the rest of the runs
typerun <- c(rep('model', 350000), rep('Average', 3500), rep('Actual', 3500))
plotdata <- cbind(plotdata, typerun)

# plot data + save as a pdf
setwd(outputdir)
pdf('figure1.pdf', height = 5, width = 12.5)
ggplot(plotdata) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = typerun, size = typerun)) +
  scale_color_manual(values = c('blue','black','#D3D3D3')) + theme_light(base_size = 16) + guides(color = 'none', size = 'none') +
  xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  scale_size_manual(values = c(1,1,3))
dev.off()

# FIGURE 2

# convert to matrix
partialpart_matrix <- matrix(as.numeric(unlist(partialpart)), nrow = 3500, byrow = F)
particpatory_matrix <- matrix(as.numeric(unlist(particpatory)), nrow = 3500, byrow = F)
individualistic_matrix <- matrix(as.numeric(unlist(individualistic)), nrow = 3500, byrow = F)
concerned_matrix <- matrix(as.numeric(unlist(concerned)), nrow = 3500, byrow = F)
combined_matrix <- matrix(as.numeric(unlist(combined)), nrow = 3500, byrow = F)

# calculate average
partialpart_average <- rowMeans(partialpart_matrix)
particpatory_average <- rowMeans(particpatory_matrix)
individualistic_average <- rowMeans(individualistic_matrix)
concerned_average <- rowMeans(concerned_matrix)
combined_average <- rowMeans(combined_matrix)

# convert matrix to data frame
partialpart_df <- as.data.frame(partialpart_matrix)
particpatory_df <- as.data.frame(particpatory_matrix)
individualistic_df <- as.data.frame(individualistic_matrix)
concerned_df <- as.data.frame(concerned_matrix)
combined_df <- as.data.frame(combined_matrix)

# rename columns
names(partialpart_df) = gsub(pattern = "V*", replacement = "", x = names(partialpart_df))
names(particpatory_df) = gsub(pattern = "V*", replacement = "", x = names(particpatory_df))
names(individualistic_df) = gsub(pattern = "V*", replacement = "", x = names(individualistic_df))
names(concerned_df) = gsub(pattern = "V*", replacement = "", x = names(concerned_df))
names(combined_df) = gsub(pattern = "V*", replacement = "", x = names(combined_df))

# prep plotting data
timestep <- c(1:3500)
alldata_modelruns <- rbind(cbind(baseline_df, timestep),
                           cbind(partialpart_df, timestep),
                           cbind(particpatory_df, timestep),
                           cbind(individualistic_df, timestep),
                           cbind(concerned_df, timestep),
                           cbind(combined_df, timestep))
scenarios <- c(rep('1baseline', 3500), rep('2partialpart', 3500), rep('3part', 3500), rep('4indiv', 3500), rep('5concerned', 3500), rep('6allchanges', 3500))
alldata_modelruns <- cbind(alldata_modelruns, scenarios)

alldata_averages <- data.frame(average, partialpart_average, particpatory_average, individualistic_average, concerned_average, combined_average, timestep)

# rearrange data to long form for plotting
plotdata_modelruns <- data.frame(melt(alldata_modelruns, id.vars = c('timestep','scenarios'), variable.name = 'ModelRun', value.name = 'Storage'))
plotdata_averages <- data.frame(melt(alldata_averages, id.vars = c('timestep'), variable.name = 'ModelRun', value.name = 'Storage'))

plotdata_averages <- cbind(plotdata_averages, scenarios)

# other data for labeling plots
scenario_names <- c('1baseline'="Baseline", '2partialpart'="Partial Participation",
  '3part'="Full Participation", '4indiv'="Individualistic", '5concerned' = "Concerned", '6allchanges'="All Changes")

# plot model runs data + save as a pdf
setwd(outputdir)
pdf('figure2a.pdf', height = 6, width = 12.5)
ggplot(plotdata_modelruns) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = scenarios)) +
  scale_color_manual(values = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f')) + theme_light(base_size = 16) + 
  xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  facet_wrap(~ scenarios, labeller=as_labeller(scenario_names)) + guides(color = 'none')
dev.off()

# plot averagedata + save as a pdf
setwd(outputdir)
pdf('figure2b.pdf', height = 4.5, width = 12.5)
ggplot(plotdata_averages) + geom_line(aes(x = timestep, y = Storage/1000000000, group = ModelRun, color = scenarios), size = 1) +
  scale_color_manual('Scenario', values = c('black','#66c2a5','#8da0cb','#e78ac3','#a6d854','#ffd92f'), 
                     labels = c('Baseline','Partial Participation', 'Full Participation', 'Individualistic','Concerned','All Changes')) + 
  theme_light(base_size = 16) + xlab('Time Step (Days)') + ylab(expression(paste('Storage (billions of ', m^3, ')', sep = ''))) +
  theme(legend.position = "bottom")
dev.off()

# TABLE 2

# calculate measures of model performance 

nrmse_calc <- c()
r2_calc <- c()

for (i in 1:100) {
  # NRSME
  nrmse_calc[i] <- nrmse(as.numeric(baseline[,i]), actualdata[1:3500,1], norm = 'maxmin') 
  
  # R^2
  r2_calc[i] <-  gof(as.numeric(baseline[,i]), actualdata[1:3500,1])[17]
}

# store values
modperf <- data.frame(measure = c('nrmse','r-squared'), minimum = c(min(nrmse_calc), min(r2_calc)), average = c(mean(nrmse_calc), mean(r2_calc)), maximum = c(max(nrmse_calc), max(r2_calc)))

# save csv
setwd(outputdir)
write_csv(modperf, 'modelperformance.csv')

# TABLE 3

# store measures of interest (min/mean/max)
scenario_results <- data.frame(scenario = c('baseline', 'partial_part', 'part', 'indiv', 'concerned', 'combined'), 
                               minimum = c(min(baseline_df), min(partialpart_df), min(particpatory_df), min(individualistic_df), min(concerned_df), min(combined_df)),
                               average = c(mean(as.matrix(baseline_df)), mean(as.matrix(partialpart_df)), mean(as.matrix(particpatory_df)), mean(as.matrix(individualistic_df)),
                                           mean(as.matrix(concerned_df)), mean(as.matrix(combined_df))),
                               maximum = c(max(baseline_df), max(partialpart_df), max(particpatory_df), max(individualistic_df), max(concerned_df), max(combined_df)))

# save csv
setwd(outputdir)
write_csv(scenario_results, 'scenarioresults.csv')

# statistical tests

# get average for each scenario
all_averages <- data.frame(cbind(average, rowMeans(partialpart_df), rowMeans(particpatory_df), rowMeans(individualistic_df), rowMeans(concerned_df), rowMeans(combined_df)))
names(all_averages) <- c('baseline','partialpart','part','indiv','concerned','allchanges')

# t test: Are the scenarios statistically significantly different from the baseline
for (i in 2:6) {
  print(names(all_averages)[i])
  print(t.test(all_averages[,1],all_averages[,i]))
}

#RESULTS: 
# partialpart --> p-value = 0.000003021 < 0.05 therefore reject the null, difference in means is NOT 0
# part --> p-value < 0.00000000000000022 < 0.05 therefore reject the null, difference in means is NOT 0
# indiv --> p-value = 0.9038 therefore fail to reject the null, difference in means is 0
# concerned --> p-value = 0.8409 therefore fail to reject the null, difference in means is 0
# allchanges --> p-value < 0.00000000000000022 < 0.05 therefore reject the null, difference in means is NOT 0








