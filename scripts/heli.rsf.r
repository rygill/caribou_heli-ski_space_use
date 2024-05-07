# 2023-11-22 this script is run on my laptop

#
#code to run glm on human from Strava
#

#-------------------------------------------------------------------------------
#2023-02-05
#-------------------------------------------------------------------------------
#VERSION 2 - using strava data:
library(ggplot2)
library(dplyr)

library(sf)
library(terra)

library(mgcv)
library(caret)

rm(list = ls())
gc()

#this dataset was subset to remove points within 100m of a stream to remove those
#locations where skiers congregate for pickup. In addition, a 200 m buffer was 
#drawn around the points, and the location of the maximum elevation was extracted.
#This maximum location was then buffered by 250m to remove locations on mountain tops,
#or on the opposite side of ridges from where skiers descended
#These were done to select only points that fell within the run itself, to avoid those
#locations that were in the pickup and dropoff zones.
#different subset data
#20m
#strava.verts = st_read('./data/strava_run_points_20m_creek_250m_peak_230307.shp')
#100 m
#strava.verts = st_read('./data/strava_run_points_100m_creek_250m_peak_230307.shp')

#decided to use all data without removing those locations
#all data:
strava.verts = st_read('./data/strava_centroids_win_tenures_Albers.shp')
#check for empty geometries
any(is.na(st_dimension(strava.verts)))
st_is_valid(strava.verts)
#clean up empty geometries (only full dataset has t his problem)
strava.verts = strava.verts %>% filter(!st_is_empty(.))


head(strava.verts)
strava.verts$type = as.factor(strava.verts$type)
strava.verts$tenure = as.factor(strava.verts$tenure)
levels(strava.verts$tenure)

#take only those points with home ranges assigned
strava.verts = strava.verts[!(is.na(strava.verts$tenure)),]
levels(strava.verts$type)
#limit to those points that we know are not ski touring lodges that do not fall
#within a heli-ski tenure
strava.verts = strava.verts[is.na(strava.verts$type),]

min(strava.verts$DN)

#read in the study area to predict to (I'll clip the rasters to this for the prediction):
sa = st_read('./data/study_area_230919.shp')

# first clip everything to the heli-ski tenures to act as the availability domain
tenures = st_read('./data/heliski_tenures_names.shp')
#-------------------------------------------------------------------------------
#buffer tenures by 40m before clipping - this allows us to clip to the tenures
#for the prediction which removes spurious values along the edge of the raster.
clp = st_buffer(tenures, 40)

#avcan = st_read('C:/Users/Ryan/OneDrive/geodatabase/boundaries/avcan_regions.shp')
#rasterize heli home range
#avcan.raster = rasterize(avcan, elev, field = 'ras_value', background = 0, filename = './data/rasters/large_extent/regions.tif')

#-------------------------------------------------------------------------------
#load rasters:
#large extent unstandardised rasters:
elev = rast('../rasters_for_prediction/elev_230927.tif')
slope = rast('../rasters_for_prediction/slope_230927.tif')
#roughness = rast('./data/rasters/large_extent/roughness_2023.tif')
aspect = rast('../rasters_for_prediction/aspect_230927.tif')
tpi = rast('../rasters_for_prediction/tpi_230927.tif')
dist.ldge = rast('../rasters_for_prediction/heli_lodge_proximity_230927.tif')

region = rast('../rasters_for_prediction/regions.tif')
names(region)
region = resample(region, elev, method = 'near')
hist(region)
slope_length = rast('../rasters_for_prediction/slope_length_230927.tif')
#wind = rast('../rasters_for_prediction/wind_exposure.tif')

# resample dist.lodge, slope_length and wind to match rows columns and extent of elev
dist.ldge = resample(dist.ldge, elev)
slope_length = resample(slope_length, elev)
#wind = resample(wind, elev)

# add rasterized tenures to use as a random effect
ten.ras = rast('../rasters_for_prediction/tenure_id.tif')

herd.bound = rast('../rasters_for_prediction/herd_id.tif')

# set the names
names(elev) = 'elev'
names(aspect) = 'aspect'
names(tpi) = 'tpi'
names(dist.ldge) = 'dist_lodge'
names(slope_length) = 'slope_length'
names(ten.ras) = 'tenure_id'
#names(wind) = 'wind_exposure'
names(herd.bound) = 'herd_id'
plot(region)
# ------------------------------------------------------------------------------
# RASTERS FOR PREDICTION
# read in the rasters to predict over the study area
pr.elev = crop(elev, sa)
pr.slope = crop(slope, sa)
pr.aspect = crop(aspect, sa)
pr.tpi = crop(tpi, sa)
pr.dist.ldge = crop(dist.ldge, sa)
pr.slope_length = crop(slope_length, sa)
#pr.wind = crop(wind, sa)
pr.region = crop(region, sa)
pr.tenure = crop(ten.ras, sa)
pr.herd = crop(herd.bound, sa)
pr.stack = c(pr.elev, pr.slope, pr.aspect, pr.tpi, pr.dist.ldge, 
             pr.slope_length, pr.region, pr.tenure, pr.herd)

# reduce the size
pr.stack = mask(pr.stack, sa)
rm(pr.elev, pr.slope, pr.aspect, pr.tpi, pr.dist.ldge, pr.slope_length, pr.region)
gc()

pr.stack.df = terra::as.data.frame(pr.stack, xy = TRUE)
pr.stack.df = as.data.frame(pr.stack.df)
names(pr.stack.df)
# ------------------------------------------------------------------------------
# RASTERS FOR MODEL
# crop them individually
elev = crop(elev, clp)
slope = crop(slope, clp)
aspect = crop(aspect, clp)
tpi = crop(tpi, clp)
dist.ldge = crop(dist.ldge, clp)
slope_length = crop(slope_length, clp)
#wind = crop(wind, clp)
region = crop(region, clp)
ten.ras = crop(ten.ras, clp)
herd.bound = crop(herd.bound,clp)

#hist(region$ras_value)

#tpi = terrain(elev, 'TPI')
#alternative to terrain()
#tpi <- focal(elev, w=matrix(7/7, nc=7, nr=7), fun = /(elev) elev[5] - mean(elev[-5]))
#names(tpi) = 'tpi'
#writeRaster(tpi, './data/rasters/rasters_for_prediction/tpi_230927.tif')

#stack and extract
rstack = c(region, elev, slope, aspect, tpi, dist.ldge, slope_length, ten.ras, herd.bound)
predstack = crop(rstack, sa) #clp is tenures buffered by 40 m
names(rstack)

#extract
strava.attrs = extract(rstack, strava.verts, bind = TRUE, na.rm = TRUE)
strava.attrs = st_as_sf(strava.attrs, coords = c('x','y'), crs = "EPSG:3005")
strava.attrs$x = st_coordinates(strava.attrs)[1]
strava.attrs$y = st_coordinates(strava.attrs)[2]

strava.attrs = st_drop_geometry(strava.attrs)

strava.attrs = as.data.frame(strava.attrs)
names(strava.attrs)

#available
stk.df = terra::as.data.frame(rstack, xy = TRUE)
names(stk.df)
stk.df = as.data.frame(stk.df)

#clean up datasets to join them:
strava.attrs = strava.attrs[,c("DN",'ras_value', "tenure_id", 'herd_id', "elev","slope", "aspect", 'tpi', 
                               "dist_lodge", "slope_length", 
                               "x", "y")]

#assign DN value of 0 for non-detections:
stk.df$DN = 0
stk.df = stk.df[,c("DN",'ras_value', "tenure_id", 'herd_id', "elev","slope", "aspect", 'tpi', 
                   "dist_lodge", "slope_length", 
                   "x", "y")]


#check names again
names(strava.attrs)
names(stk.df)

min_vals = stk.df %>%
  group_by(ras_value) %>%
  summarise(count_of_values = n())

#sample randomly within each area
stk.df1 <- stk.df[stk.df$ras_value > 0,] %>% 
  group_by(ras_value) %>% 
  slice_sample(n=3000000)

#some data clean up and formatting
stk.df = as.data.frame(stk.df1)
rm(stk.df1)
strava.attrs = as.data.frame(strava.attrs)


#merge known and available:
data = rbind(strava.attrs, stk.df)
data = na.omit(data)
data$ras_value = as.factor(data$ras_value)
data$tenure_id = as.factor(data$tenure_id)
# set up the binomal approach
data$detection = ifelse(data$DN == 0,0,1)
data$weight = data$DN/ mean(data$DN)
data$weight = ifelse(data$detection == 0, 10000, data$weight)


#examine data
head(data)
tail(data)
max(data$heli_lodge_prox_2023)

gc()

#subsample to get train and test:
dt = sort(sample(nrow(data), nrow(data)*.8))
train = data[dt,]
test = data[-dt,]

#-------------------------------------------------------------------------------
#plots of use as a function of each predictor:
#*plots of use as a function of each predictor:
plot.g = function(a,b,c)
{
  ggplot() +
    geom_point(data = a, aes(x = b, y = c)) +
    geom_smooth(data = a, aes(x = b, y = c), 
                method = 'loess', se = FALSE) +
    xlab(b) +
    ylab("Intensity of Use")
}

plot.g(strava.attrs, strava.attrs$elev_2023, strava.attrs$DN)
plot.g(strava.attrs, strava.attrs$slope_2023, strava.attrs$DN)
plot.g(strava.attrs, strava.attrs$aspect_2023, strava.attrs$DN)
plot.g(strava.attrs, strava.attrs$slope_length, strava.attrs$DN)

#collinearity
library(corrplot)
dat.cor = data[,c("elev_2023","slope_2023","roughness_2023", "aspect",
                  "tpi", "heli_lodge_prox_2023", "slope_length")]
names(dat.cor) = c("Elevation","Slope","Roughness", "aspect",
                   "tpi", "Lodge Distance", "slope length")
corrplot(cor(dat.cor), method = 'number')
# roughness and slope are highly correlated, pick slope
##------------------------------------------------------------------------------
#GAM
##------------------------------------------------------------------------------
library(mgcv)

# BINOMIAL response works best
#subsample to get train and test:
dt = sort(sample(nrow(data), nrow(data)*.8))
train2 = data[dt,]
test2 = data[-dt,]

m_2 <-
  bam(detection ~
        s(tenure_id, bs = 're') +
        s(elev, k = 4, bs = "tp") + #fs for random effects
        s(slope, k = 5, bs = "tp") + 
        s(aspect, k = 6, bs = "cc") +
        s(tpi, k = 5, bs = "tp") + 
        s(dist_lodge, k = 6, bs = "tp"),
      family = poisson(link = "log"),
      knots = list(aspect = c(0, 360)),
      data = train2,
      method = 'fREML', 
      discrete = TRUE, 
      weights = weight,
      control = gam.control(nthreads = 7, trace = TRUE), 
      na.action = 'na.fail')

summary(m_2)
save(m_2, file = "./models/m_2_heliski_re_231121.rds")
load("./models/m_2_heliski_re_231121.rds")

#plot model parameters

#plot model parameters
jpeg(file = "./figures/m_2_heliski_re_231121_FINAL.jpeg",
     width = 686, height = 665, units = "px")
plot.gam(m_2, rug = FALSE, pages = 1, scale = 0,
         residuals = FALSE, ylab = "smoothed term")
dev.off()

#import 20% holdout - cross validation points
#####################################
test2$pred.gam.bn <- predict(m_2, newdata=test2 ,type="response", exclude = c("(Intercept)", "s(tenure_id)", "s(dist_lodge)"))
test2$pt_type = ifelse(test2$detect == 0,"Available","Used")
test2$pred.gam.bn = test2$pred.gam.bn / max(test2$pred.gam.bn)
ggplot() + 
  geom_boxplot(data = test2, aes(x = pt_type, y = as.numeric(pred.gam.bn), fill = pt_type), outlier.shape = NA) +
#  coord_cartesian(ylim = c(0, 20000)) +
  ggtitle("Binomial heli-ski rsf validation random holdout") +
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
ggsave(file="./figures/heli_ski_m_2_holdout_231113_FINAL.png",
              width = 1500,
              height=1011,
              units = "px",
              dpi = 300)


# predict
pr.stack.df$pred.gam.bn = predict(m_2, newdata = pr.stack.df, type = 'response',
                                  exclude = c("(Intercept)", "s(tenure_id)", "s(dist_lodge)"))
library(arrow)
write_parquet(pr.stack.df[,c('x','y','tenure_id', 'herd_id', 'pred.gam.bn')], sink = './predictions/HELI_herd_pred_gam_all_data_231122.parquet')

# calculate that 99.9%tile
max(pr.stack.df$pred.gam.bn, na.rm = TRUE)
max.val = quantile(pr.stack.df$pred.gam.bn, c(0.999), na.rm = TRUE)
max.val = max.val[[1]]
# reset those very high values to the highest of the 99.9%
pr.stack.df$pred.gam.bn = ifelse(pr.stack.df$pred.gam.bn >= max.val, max.val, 
                                 pr.stack.df$pred.gam.bn)

pr.stack.df = pr.stack.df[,c('x','y','pred.gam.bn')]
pred.gam.ras = rast(pr.stack.df, crs = "EPSG:3005")

# standardize
pred.gam.ras.std = pred.gam.ras / max.val
writeRaster(pred.gam.ras.std, './predictions/standardized/HELI_LATE_WINTER_standardized_231121.tif',
            overwrite = TRUE)

#-------------------------------------------------------------------------------
# plot marginal effects

# use the same method as the RATA model 2024-04-24
library(gratia)
rm(list = ls())
load("./models/m_2_heliski_re_231121.rds")
summary(m_2)
# elevation
elevation = smooth_estimates(m_2, smooth = "s(elev)" )
elevation = add_confint(elevation)
elevation$smooth = 'elevation'
elevation = rename(elevation,
                   'elevation_est' = 'elev')


e = ggplot(elevation, aes(x = elevation_est)) + 
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
                fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = elevation, aes(x = elevation_est, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x = "Elevation (m)", y = "Partial effect", tag = 'a)')


# slope
slope = smooth_estimates(m_2, smooth = "s(slope)" )
slope = add_confint(slope)
slope$smooth = 'slope'
slope = rename(slope,
                   'slope_est' = 'slope')

s = ggplot(slope, aes(x = slope_est)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = slope, aes(x = slope_est, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x = "Slope (degrees)", y = "Partial effect", tag = 'b)')


# aspect
aspect = smooth_estimates(m_2, smooth = "s(aspect)" )
aspect = add_confint(aspect)
aspect$smooth = 'aspect'
aspect = rename(aspect,
               'aspect_est' = 'aspect')

a = ggplot(aspect, aes(x = aspect_est)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = aspect, aes(x = aspect_est, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x = "Aspect (degrees)", y = "Partial effect", tag = 'c)')


# tpi
tpi = smooth_estimates(m_2, smooth = "s(tpi)" )
tpi = add_confint(tpi)
tpi$smooth = 'tpi'
tpi = rename(tpi,
               'tpi_est' = 'tpi')

t = ggplot(tpi, aes(x = tpi_est)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = tpi, aes(x = tpi_est, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x = "Topographic position index", y = "Partial effect", tag = 'd)')
  

# distance to lodge
dist.lodge = smooth_estimates(m_2, smooth = "s(dist_lodge)" )
dist.lodge$dist_lodge = dist.lodge$dist_lodge/1000
dist.lodge = add_confint(dist.lodge)
dist.lodge$smooth = 'distance to lodge'
dist.lodge = rename(dist.lodge,
               'dist_lodge_est' = 'dist_lodge')

d = ggplot(dist.lodge, aes(x = dist_lodge_est)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = dist.lodge, aes(x = dist_lodge_est, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x = "Distance to lodge (km)", y = "Partial effect", tag = 'e)')

# arrange plots
#library(cowplot)
# cowplot solution
#gridPlot = plot_grid(e, s, a, t, d, align = "h", ncol = 3)
library(gridExtra)
plot.arr = grid.arrange(e, s, a, t, d, ncol = 3)


ggsave(plot.arr, file="./figures/HELI_plot_gam_240424.png",
       width = 1500,
       height=1000,
       units = "px",
       dpi = 130)


# EOF
# read in the standardized product and calculate the global 0.05 confint
ras.pred = rast('C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/git_caribou/caribou_heli-ski_space_use/predictions/product/product_RATA_HELI_std_231121.tif')
global(ras.pred, quantile, probs = c(0.05, 0.999), na.rm = TRUE)

ras.pred = ifel(ras.pred < 1.935233e-08, 1.935233e-08, ras.pred)
ras.pred = ifel(ras.pred > 0.6151363, 0.6151363, ras.pred)
writeRaster(ras.pred, './predictions/product/product_RATA_HELI_std_231122.tif')


