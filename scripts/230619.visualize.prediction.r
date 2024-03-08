library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(gridExtra)

rm(list = ls())
gc()
getwd()

# load the data
dat = read_parquet("./data/all_data_LATE_WINTER_231024.parquet")
names(dat)
# get timestamp formatted
dat$timestamp = as.POSIXct(dat$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
head(dat[dat$pt_type == 'used',])
#dat.avail = dat[dat$pt_type == 'available',]
#dat.avail$timestamp = "2021-02-01 01:00:00"
#dat.avail$timestamp = as.POSIXct(dat.avail$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
#dat.used = dat[dat$pt_type == 'used',]
#dat.used$timestamp = as.POSIXct(dat.used$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
#dat = rbind(dat.used, dat.avail)


# which columns to keep
dat1 = dat[,c("period", "pt_type", "individual.local.identifier", "herd", 
               "weight", "elev_220809", "slope_220809", "proj_age_220809", 
              "glacier", "crown_clos", "burned", "logged","x", "y")]



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
dat1$timestamp = NULL
#-------------------------------------------------------------------------------

# GAM
# choose during as the period of most likely habitat selection
# drop collars that we excluded earlier for the during period: 32611, 44095, 44098
# plus two don't have HR estimates: 42144 and 42296
dat2 = droplevels(dat1[!(dat1$individual.local.identifier %in% c('32611', '44095', '44098', '42144', '42296')),]) 

#some NAs in ndvi
#dat2 = dat2[!is.na(dat2$ndvi),]

#NAs in crown_clos, set them to 0
dat2$crown_clos[is.na(dat2$crown_clos)] <- 0
dat2 = na.omit(dat2)

#-------------------------------------------------------------------------------
# density plots to look at how caribou select what's available and how that varies among herds

#-----------elevation
jpeg(file="./figures/rsf_gam/density_elevation.jpeg",
     width=800, height=539)
plot(density(dat2[which(dat2$detection == 0 & dat2$herd == 2),"elev_220809"]), 
     lwd = 2, lty = 2, col = "#FFACAC", ylim = c(0,0.0026),
     main="Elevation",
     xlab="Elevation (m)",
     ylab="Density")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 2),"elev_220809"]), lwd = 3, col = "#FF0000")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 1),"elev_220809"]), lwd = 2, lty = 2, col = "#9DB8FF")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 1),"elev_220809"]), lwd = 3, col = "#1052FF")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 3),"elev_220809"]), lwd = 2, lty = 2, col = "#72F36D")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 3),"elev_220809"]), lwd = 3, col = "#23951F")
legend(x = "topright",          # Position
       legend = c("hr used", "hr avail", "cn used", "cn avail", "cs used", "cs avail"),  # Legend texts
       lty = c(1, 2, 1, 2, 1, 2),           # Line types
       col = c("#23951F", "#72F36D", "#1052FF", "#9DB8FF", "#FF0000", "#FFACAC"),           # Line colors
       lwd = 2)
dev.off()

#-----------slope
jpeg(file="./figures/rsf_gam/density_slope.jpeg",
     width=800, height=539)
plot(density(dat2[which(dat2$detection == 0 & dat2$herd == 2),"slope_220809"]), 
     lwd = 1, col = "#FFACAC", ylim = c(0,0.05),
     main="Slope",
     xlab="Slope (degrees)",
     ylab="Density")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 2),"slope_220809"]), lwd = 3, col = "#FF0000")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 1),"slope_220809"]), lwd = 1, col = "#9DB8FF")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 1),"slope_220809"]), lwd = 3, col = "#1052FF")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 3),"slope_220809"]), lwd = 1, col = "#72F36D")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 3),"slope_220809"]), lwd = 3, col = "#23951F")
legend(x = "topright",          # Position
       legend = c("hr used", "hr avail", "cn used", "cn avail", "cs used", "cs avail"),  # Legend texts
       lty = c(1, 2, 1, 2, 1, 2),           # Line types
       col = c("#23951F", "#72F36D", "#1052FF", "#9DB8FF", "#FF0000", "#FFACAC"),           # Line colors
       lwd = 2)

dev.off()

#-----------crown closure
jpeg(file="./figures/rsf_gam/density_crown_closure.jpeg",
     width=800, height=539)
plot(density(dat2[which(dat2$detection == 0 & dat2$herd == 2),"crown_clos"]), 
     lwd = 1, col = "#FFACAC", ylim = c(0,0.1),
     main="Crown Closure",
     xlab="Crown closure (percent)",
     ylab="Density")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 2),"crown_clos"]), lwd = 3, col = "#FF0000")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 1),"crown_clos"]), lwd = 1, col = "#9DB8FF")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 1),"crown_clos"]), lwd = 3, col = "#1052FF")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 3),"crown_clos"]), lwd = 1, col = "#72F36D")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 3),"crown_clos"]), lwd = 3, col = "#23951F")
legend(x = "topright",          # Position
       legend = c("hr used", "hr avail", "cn used", "cn avail", "cs used", "cs avail"),  # Legend texts
       lty = c(1, 2, 1, 2, 1, 2),           # Line types
       col = c("#23951F", "#72F36D", "#1052FF", "#9DB8FF", "#FF0000", "#FFACAC"),           # Line colors
       lwd = 2)

dev.off()

#-----------forest age
jpeg(file="./figures/rsf_gam/density_forest_age.jpeg",
     width=800, height=539)
plot(density(dat2[which(dat2$detection == 0 & dat2$herd == 2),"proj_age_220809"]), 
     lwd = 1, col = "#FFACAC", ylim = c(0,0.02),
     main="Forest Age",
     xlab="Forest age (years)",
     ylab="Density")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 2),"proj_age_220809"]), lwd = 3, col = "#FF0000")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 1),"proj_age_220809"]), lwd = 1, col = "#9DB8FF")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 1),"proj_age_220809"]), lwd = 3, col = "#1052FF")

lines(density(dat2[which(dat2$detection == 0 & dat2$herd == 3),"proj_age_220809"]), lwd = 1, col = "#72F36D")
lines(density(dat2[which(dat2$detection == 1 & dat2$herd == 3),"proj_age_220809"]), lwd = 3, col = "#23951F")
legend(x = "topright",          # Position
       legend = c("hr used", "hr avail", "cn used", "cn avail", "cs used", "cs avail"),  # Legend texts
       lty = c(1, 2, 1, 2, 1, 2),           # Line types
       col = c("#23951F", "#72F36D", "#1052FF", "#9DB8FF", "#FF0000", "#FFACAC"),           # Line colors
       lwd = 2)

dev.off()

hold.year = 'prior1'

# dat3 is the analysis set excluding the holdout collars, we'll call this dataset
# dat4 to avoid mixing things up:
dat4 = dat2[dat2$period == hold.year,]
dat4$individual.local.identifier = as.factor(29088)
dat4$herd = as.factor(1)


# the model was run on the analysis set that did not include the holdout collars
load("H:/caribou_anthropuase/mAll_LATE_WINTER_231113.rds") #load("./data/rsf/gam/models/analysis_set/mAll_LATE_WINTER_231024.rds") 
summary(mAll)

#--------------- plot the model
jpeg(file = "./figures/rsf_gam/mAll_gam_plot_231019.jpeg",
          width = 990, height = 965, units = "px")

plot.gam(mAll, rug = FALSE, pages = 1, scale = 0, scheme = 3,
         residuals = FALSE, ylab = "smoothed term")

dev.off()

# predict on the holdout data
dat4$pred.gam= predict(object = mAll,
                       newdata = dat4,
                       type = 'response', 
                       exclude = c("(Intercept)","s(individual.local.identifier)", "s(herd)"), 
                       proximity = FALSE)
#hist(dat4$pred.gam)
max.val = quantile(dat4$pred.gam, probs = 0.999)
max.val = max.val[[1]]
dat4$pred.gam = ifelse(dat4$pred.gam > max.val, max.val, dat4$pred.gam)

dat4$pred.gam = dat4$pred.gam / max.val

#--------------- plot the results of the prediction on the holdout
ggplot() + 
  geom_boxplot(data = dat4, aes(x = pt_type, y = as.numeric(pred.gam), fill = pt_type), outlier.shape = NA) +
  #coord_cartesian(ylim = c(0, 751831186)) +
  ggtitle("rsf validation (2019/2020 holdout)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=10, family = "sans"),
        plot.title = element_text(size=10, hjust = 0, family = "sans"),
        axis.text.y  = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank()) +
  ylab("gam RSF Score") +
  scale_fill_manual(values = c("#1E88E5","#D81B60")) +
  scale_x_discrete(labels=c("used" = "Used", "available" = "Available"))
ggsave(file="./figures/rsf_gam/mAll_holdout_FINAL_231121.png",
       width = 1500,
       height=1011,
       units = "px",
       dpi = 300)


# examine how caribou use disturbances differently
# based on these plots I included these disturbances in the model
#####
fire = rast("C:/Users/Ryan/OneDrive/Biodiveristy Pathways/primary_prey_habitat/data/rasters/fire_raster.tif")
names(fire) = 'fire_age'
cutblock = rast("C:/Users/Ryan/OneDrive/Biodiveristy Pathways/primary_prey_habitat/data/rasters/cutblock_raster.tif")
names(cutblock) = 'cutblock_age'
fire = resample(fire, cutblock)

dat2sf = st_as_sf(dat2, coords = c("x","y"), crs = "EPSG:3005")
disturb = c(fire, cutblock)
dat2sf = dat2sf[,c('detection', 'herd')]
dat2sf = extract(disturb, dat2sf, bind = TRUE, na.rm = TRUE)
dat2df = terra::as.data.frame(dat2sf)

ggplot(dat2df[dat2df$detection == 1 & dat2df$fire_age < 200,]) +
  geom_histogram(aes(fire_age, binwidth = 1)) +
  #ggtitle("Use of burned areas") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=10, family = "sans"),
        plot.title = element_text(size=10, hjust = 0, family = "sans"),
        axis.text.y  = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank()) #+
  #ylab("Count of used locations")
ggsave(file="./figures/rsf_gam/fire_use_inset_231018.png",
       width = 1500,
       height=1011,
       units = "px",
       dpi = 300)
# tend to use older fires, but overwhelmingly use unburned areas

ggplot(dat2df[dat2df$detection == 1 & dat2df$cutblock_age < 200,]) +
  geom_histogram(aes(cutblock_age, binwidth = 1)) +
  #ggtitle("Use of logged areas") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=10, family = "sans"),
        plot.title = element_text(size=10, hjust = 0, family = "sans"),
        axis.text.y  = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank())# +
#ylab("Count of used locations")
ggsave(file="./figures/rsf_gam/cutblock_use_inset_231018.png",
       width = 1500,
       height=1011,
       units = "px",
       dpi = 300)
# seem to use young cutblocks (0 - 10 years, then decreasing use), still overwhelmingly use unlogged.

# ------------------------------------------------------------------------------
# predict on the original dataset to get boxplots for each individual

# load the model
load("H:/caribou_anthropuase/mAll_LATE_WINTER_log_transformed_age_231026.rds") 

# load the data
dat = read_parquet("./data/all_data_LATE_WINTER_231024.parquet")
names(dat)

# which columns to keep
dat1 = dat[,c("period", "pt_type", "individual.local.identifier", "herd", 
              "weight", "elev_220809", "slope_220809", "proj_age_220809", 
              "glacier", "crown_clos", "burned", "logged","x", "y")]



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
dat1$timestamp = NULL
#-------------------------------------------------------------------------------

# drop collars that we excluded earlier for the during period: 32611, 44095, 44098
# plus two don't have HR estimates: 42144 and 42296
dat5 = droplevels(dat1[!(dat1$individual.local.identifier %in% c('32611', '44095', '44098', '42144', '42296')),]) 
dat5= as.data.frame(dat5)
#NAs in crown_clos, set them to 0
dat5$crown_clos[is.na(dat5$crown_clos)] <- 0
dat5 = na.omit(dat5)

# set herd and id to that which we used earlier
dat5$collar_id = dat5$individual.local.identifier
dat5$individual.local.identifier = 29088

dat5$herd = 1

# now predict on this dataset so we can make some histograms
dat5$pred.gam = predict(object = mAll,
                        newdata = dat5,
                        type = 'response', 
                        exclude = c("(Intercept)","s(individual.local.identifier)", "s(herd)"), proximity = FALSE)

quantile(dat5$pred.gam, c(0.05, 0.999))
dat5$pred.gam = ifelse(dat5$pred.gam >= 1087685617, 1087685617, dat5$pred.gam)
dat5$herd = as.factor(dat5$herd)
dat5$pt_type = as.factor(dat5$pt_type)
for(i in unique(dat5$collar_id)){
  print(i)
  ggplot() + 
  geom_boxplot(data = dat5[dat5$collar_id == i,], aes(x = pt_type, y = as.numeric(pred.gam), fill = pt_type), outlier.shape = NA) +
  #coord_cartesian(ylim = c(0, 5819034364)) +
  ggtitle("rsf validation (2019/2020 holdout)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=10, family = "sans"),
        plot.title = element_text(size=10, hjust = 0, family = "sans"),
        axis.text.y  = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank()) +
  ylab("gam RSF Score") +
  scale_fill_manual(values = c("#005AB5","#DC3220")) +
  facet_grid(~collar_id, scales = 'free_y') +
  scale_x_discrete(labels=c("used" = "Used", "available" = "Available"))
ggsave(file=paste0("./figures/rsf_gam/individual_boxplots/log_transformed/",i,".png"),
       width = 1500,
       height=1011,
       units = "px",
       dpi = 300)
  
}
