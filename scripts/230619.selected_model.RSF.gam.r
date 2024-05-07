# 2023-11-22 this script is run on my laptop

# code to run gam for resource selection for caribou late winter range.
# uses data generated from 230619.DATA.prep.RSF.gam.r
# selected model
#-------------------------------------------------------------------------------
rm(list = ls())
gc()

library(arrow)
library(mgcv)
library(dplyr)
library(ggplot2)
library(marginaleffects)
getwd()

dat = read_parquet("./data/all_data_LATE_WINTER_231024.parquet") #dat = read.csv("./data/rsf/gam/all_data_230804.csv")
names(dat)
# which columns to keep
dat1 = dat[,c("period", "pt_type", "individual.local.identifier", "timestamp", "herd", 
              "weight", "elev_220809", "slope_220809", "proj_age_220809", "glacier", 
              "crown_clos", "burned","logged","x", "y")]


# for the gam I need to create a raster of herd and individual to set to 0 during
# the prediction

# set herd to values 1-3 corresponding to each herd: 1:cn, 2:cs, 3:hr
dat1$herd = ifelse(dat1$herd == 'cn', 1, 
                   ifelse(dat1$herd == 'cs', 2, 3))
#dat1$herd.num = NULL

rm(dat)
gc()
# weighted as per https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2656.13441
# but not using such large weights as we also downweight our known values
dat1$weight = ifelse(dat1$pt_type == 'available', 1, dat1$weight) 

# some carpentry
dat1$detection = as.integer(ifelse(dat1$pt_type == 'available', 0,1))
dat1$individual.local.identifier = as.factor(dat1$individual.local.identifier)
levels(dat1$individual.local.identifier)

dat1$period = as.factor(dat1$period)
dat1$herd = as.factor(dat1$herd)
dat1$glacier = as.factor(dat1$glacier)
dat1$burned = as.factor(dat1$burned)
dat1$logged = as.factor(dat1$logged)
dat1$timestamp = as.POSIXct(dat1$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
#-------------------------------------------------------------------------------

# GAM
# choose during as the period of most likely habitat selection
# drop collars that we excluded earlier for the during period: 32611, 44095, 44098
# plus two don't have HR estimates: 42144 and 42296
dat2 = droplevels(dat1[!(dat1$individual.local.identifier %in% c('32611', '44095', '44098', '42144', '42296')),]) 

#NAs in crown_clos, set them to 0
dat2$crown_clos[is.na(dat2$crown_clos)] <- 0

# get times for available
dat.avail = dat2[dat2$pt_type == 'available',]
dat.avail$timestamp = "2021-01-01 01:00:00"
dat.avail$timestamp = as.POSIXct(dat.avail$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
dat.used = dat2[dat2$pt_type == 'used',]
dat.used$timestamp = as.POSIXct(dat.used$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
dat2 = rbind(dat.used, dat.avail)

head(dat2)
head(dat2[dat2$pt_type == 'used',])
dat2 = na.omit(dat2)

# check for correlation
library(corrplot)
dat.cor = dat2[,c("elev_220809","slope_220809","proj_age_220809", 'crown_clos', 
                  'burned', 'logged', 'glacier')]
dat.cor$Burned = as.numeric(dat.cor$Burned)
dat.cor$Logged = as.numeric(dat.cor$Logged)
dat.cor$Glacier = as.numeric(dat.cor$Glacier)

names(dat.cor) = c("Elevation","Slope","Forest Age", "Crown Closure", "Burned",
                   "Logged","Glacier")
corrplot(cor(dat.cor), method = 'number')

# some info for reporting
dat.summary = dat2 %>% 
  group_by(herd, individual.local.identifier) %>%
  summarise(number_fixes = n())

datb = dat.summary %>%
  group_by(herd) %>%
  summarise(n_animals = n())
#-------------------------------------------------------------------------------
# subset for each herd using dat2, the full data set, subset to during
# holdout collars:
#cn.hold = c(29082, 29095, 41448)
#cs.hold = c(22562, 22564, 37668)
#hr.hold = c(44094, 44098, 45318, 81332, 81354)

#hold.collars = c(29082,29095, 22562,  41448,44094,44098,45318,81332,81354) #cs 22564, 37668, - only included one cs since so few animals collared
hold.year = 'prior1'
#hrds = c("cn", "cs", "hr")

# select analysis set:
dat3 = dat2[!(dat2$period %in% hold.year),]
table(dat3$pt_type)
table(dat3$period)
#-------------------------------------------------------------------------------
# all predictors

# now run one model for all data using herd as the random effect:
K <- 1e-6 # down weighting for used:

mAll <-
  bam(detection / K ~ #this down weights the known values
        s(herd, bs = 're') +
        s(individual.local.identifier, bs = 're') + 
        s(elev_220809, herd, k = 5, bs = 'fs') + 
        s(slope_220809, herd, k = 3, bs = 'fs') + 
        s(crown_clos, k = 5, bs = 'fs') + # no herd random effect for CC
        s(proj_age_220809, herd, k = 3, bs = 'fs') + 
        ti(proj_age_220809, crown_clos, k = 5) + # no herd random effect for interactions
        ti(elev_220809, slope_220809, k = 5) + # no herd random effect for interactions
        ti(proj_age_220809, by = burned, k = 5) + 
        ti(proj_age_220809, by = logged, k = 5) +
        glacier +
        burned +
        logged,
      family = poisson(link = 'log'), 
      data = dat3,  
      weights = weight, # downscale 1s based on sample size and K
      method = 'fREML',
      discrete = TRUE,
      control = gam.control(nthreads = 10, trace = TRUE))

summary(mAll)
save(mAll, file = "./models/mAll_LATE_WINTER_231113.rds")  # model with no K is saved as "./models/mAll_LATE_WINTER_NO_K_240110.rds"

png(file = "./figures/mAll_gam_plot_231113.png",
     width = 890, height = 865, units = "px")
plot.gam(mAll, rug = FALSE, pages = 1, scale = 0, scheme = 3,
         residuals = FALSE, ylab = "smoothed term", res = 600)
dev.off()




