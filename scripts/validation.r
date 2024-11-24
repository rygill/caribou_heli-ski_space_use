
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
dat2 = droplevels(dat1[dat1$pt_type == 'used' & !(dat1$individual.local.identifier %in% c('32611', '44095', '44098', '42144', '42296')),]) 

#NAs in crown_clos, set them to 0
dat2$crown_clos[is.na(dat2$crown_clos)] <- 0

# set herd to values 1-3 corresponding to each herd: 1:cn, 2:cs, 3:hr
#dat2$herd = ifelse(dat2$herd == 1, 'cn', 
#                   ifelse(dat2$herd == 2, 'cs', 'hr'))

head(dat2)
#head(dat2[dat2$pt_type == 'used',])
dat2 = na.omit(dat2)
rm(dat1)
gc()

dat2 = dat2[,c(1:3, 5, 14,15)]

#-------------------------------------------------------------------------------
hold.year = 'prior1'

# select analysis set:
datanal = dat2[!(dat2$period %in% hold.year),]
dathold = dat2[dat2$period == hold.year,]

# read in SMC rsf
smc = rast('./predictions/standardized/RATA_LATE_WINTER_standardized_231113.tif')

# extract original values at those points first to compare
dat3 = st_as_sf(dat2, crs = 'EPSG:3005', coords = c('x','y'))
dat3$x = st_coordinates(dat3)[,1]
dat3$y = st_coordinates(dat3)[,2]
dat3 = extract(smc, dat3, bind = TRUE)
dat3 = terra::as.data.frame(dat3)
dat3 = rename(dat3, 'predicted_value' = 'pred.gam')

# convert back to sf to extract the classes
dat3 = st_as_sf(dat3, coords = c('x','y'), crs = 'EPSG:3005')

# classify into 10 bins
m <- c(0, 0.1, 1,
       0.10000000001, 0.1999999999, 2,
       0.2, 0.2999999999, 3,
       0.3, 0.3999999999, 4,
       0.4, 0.4999999999, 5,
       0.5, 0.5999999999, 6,
       0.6, 0.6999999999, 7,
       0.7, 0.7999999999, 8,
       0.8, 0.8999999999, 9,
       0.9, 1, 10)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
smc10 <- classify(smc, rclmat, include.lowest=TRUE)
#plot(smc)

# convert dat2 to sf
dat.10 = extract(smc10, dat3, bind = TRUE)
dat.10 = as.data.frame(dat.10)
dat.10 = rename(dat.10, 'rsf_bin' = 'pred.gam')
dat.10 = na.omit(dat.10)

# create a column that specifies whether it was a holdout or analysis year
dat.10$set = ifelse(dat.10$period == 'prior1','holdout','analysis')

# get total points per period
dat.total = dat.10 %>%
  group_by(set) %>%
  summarise(total_pts_set = n())

dat.10.summary = dat.10 %>%
  group_by(set, rsf_bin) %>%
  summarise(n_locations = n())

# get the total points per period from dat.total to get proportions
dat.10.summary = inner_join(dat.10.summary, dat.total, by = 'set')
#proportion
dat.10.summary$prop.bin = dat.10.summary$n_locations / dat.10.summary$total_pts_set
dat.10.summary$rsf_bin = as.factor(dat.10.summary$rsf_bin)
dat.val = dat.10.summary %>%
  group_by(set, rsf_bin, total_pts_set) %>%
  summarise(total_locs = sum(n_locations))

ggplot(dat.10.summary) +
  geom_bar(aes(x = rsf_bin, y = prop.bin), stat = 'identity') +
  theme_bw() +
  #scale_x_discrete(labels = c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x = 'RSF bin', y = 'proportion of points in each bin') +
  facet_wrap(~set, scales = 'free')
ggsave(file = paste0('./figures/validation.jpg'),
       width = 1400,
       height=1300,
       units = "px",
       dpi = 250)

# for a chi-squared test
# need dat.10 by period (same as dat.10.summary, but for period, not set)
chi.10.summary = dat.10 %>%
  group_by(period, rsf_bin) %>%
  summarise(n_locations = n())
# need totals per period
period.total = dat.10 %>%
  group_by(period) %>%
  summarise(total_pts_period = n())

# join to calculate proportions
chi.10.summary = inner_join(chi.10.summary, period.total, by = 'period')
chi.10.summary$prop_bin = chi.10.summary$n_locations / chi.10.summary$total_pts_period
# assign which set each period belongs to
chi.10.summary$set = ifelse(chi.10.summary$period == 'prior1','holdout','analysis')
chi.dat = chi.10.summary %>%
  group_by(set, rsf_bin) %>%
  summarise(mean_prop = mean(prop_bin))

# need data in wide format
chi.wide = tidyr::pivot_wider(chi.dat, names_from = 'set', values_from = 'mean_prop')

chisq.test(chi.wide$analysis, chi.wide$holdout)

chi.wide$chisq = (chi.wide$holdout - chi.wide$analysis)^2 / chi.wide$analysis
sum(chi.wide$chisq)
# alpha = 0.05 ,df = 9. Critical value is therefore 16.919
# 0.01173447 is less than 16.919 so we accept that there is no difference between the two

# Mike's manual approach
#Run the chi-square test
chi_sq <- sum(((chi.wide$holdout - chi.wide$analysis)^2)/chi.wide$analysis)
pchisq(chi_sq, df = nrow(chi.wide) - 1, lower.tail = FALSE)


#-------------------------------------------------------------------------------
# heli-ski
#decided to use all data without removing those locations
#all data:
rm(list = ls())
gc()
# read in the dataset created in heli.rsf.r
validata = read.csv('./heli_rsf_validata_24529.csv')

#assign pred.gam.bn to 10 bins
# same as for smc:
validata$rsf_bin = ifelse(validata$pred.gam.bn < 0.1, 1,
                          ifelse(validata$pred.gam.bn > 0.10000000001 & validata$pred.gam.bn < 0.1999999999, 2,
                                 ifelse(validata$pred.gam.bn > 0.2 & validata$pred.gam.bn < 0.2999999999, 3,
                                        ifelse(validata$pred.gam.bn > 0.3 & validata$pred.gam.bn < 0.3999999999, 4,
                                               ifelse(validata$pred.gam.bn > 0.4 & validata$pred.gam.bn < 0.4999999999, 5,
                                                      ifelse(validata$pred.gam.bn > 0.5 & validata$pred.gam.bn < 0.5999999999, 6,
                                                             ifelse(validata$pred.gam.bn > 0.6 & validata$pred.gam.bn < 0.6999999999, 7,
                                                                    ifelse(validata$pred.gam.bn > 0.7 & validata$pred.gam.bn < 0.7999999999, 8,
                                                                           ifelse(validata$pred.gam.bn > 0.8 & validata$pred.gam.bn < 0.8999999999, 9,10)))))))))

hist(validata$pred.gam.bn)

# get total points per period
valid.total = validata %>%
  group_by(set) %>%
  summarise(total_pts_set = n())

validata.summary = validata %>%
  group_by(set, rsf_bin) %>%
  summarise(n_locations = n())

# get the total points per period from dat.total to get proportions
validata.summary = inner_join(validata.summary, valid.total, by = 'set')
#proportion
validata.summary$prop.bin = validata.summary$n_locations / validata.summary$total_pts_set
validata.summary$rsf_bin = as.factor(validata.summary$rsf_bin)

dat.val = validata.summary %>%
  group_by(set, rsf_bin, total_pts_set) %>%
  summarise(total_locs = sum(n_locations))

ggplot(validata.summary) +
  geom_bar(aes(x = rsf_bin, y = prop.bin), stat = 'identity') +
  theme_bw() +
  #scale_x_discrete(labels = c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x = 'RSF bin', y = 'proportion of points in each bin') +
  facet_wrap(~set, scales = 'free')
ggsave(file = paste0('./figures/validation_HELI.jpg'),
       width = 1400,
       height=1300,
       units = "px",
       dpi = 250)

chi.dat = validata.summary[,c('set', 'rsf_bin', 'prop.bin')]

# need data in wide format
chi.wide = tidyr::pivot_wider(chi.dat, names_from = 'set', values_from = 'prop.bin')

chisq.test(chi.wide$analysis, chi.wide$holdout)

chi.wide$chisq = (chi.wide$holdout - chi.wide$analysis)^2 / chi.wide$analysis
sum(chi.wide$chisq)
# alpha = 0.05 ,df = 9. Critical value is therefore 16.919
# 0.01173447 is less than 16.919 so we accept that there is no difference between the two

# Mike's manual approach
#Run the chi-square test
chi_sq <- sum(((chi.wide$holdout - chi.wide$analysis)^2)/chi.wide$analysis)
pchisq(chi_sq, df = nrow(chi.wide) - 1, lower.tail = FALSE)
