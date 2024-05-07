# 2023-11-22 this script is run on my laptop

# up-to-date data prep 2023-10-29
# script to load data, rasters and format all for the model and prediction

rm(list = ls())
gc()

library(sf)
library(terra)
library(stringr)
library(dplyr)
#find out where tempdir is and make sure 'raster' is in there:
path = paste0(tempdir(),'/raster')
dir.create(path)

#-------------------------------------------------------------------------------
#READ IN DATA:
#-------------------------------------------------------------------------------
data = read.csv("./data/input_data/220526_moving_window_formatted_for_tele_dat.csv") 
data$resident = rowSums(data[,c("prior1", "prior2", "during", "after")])
data = data[data$resident == 1,]
data = data[complete.cases(data), ]

data$timestamp = as.POSIXct(data$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
data$herd = as.factor(data$herd)
data$mort.status = as.factor(data$mort.status)
data$outlier = as.factor(data$outlier)

#make a period column to simplify things later:
data$period = ifelse(data$prior1 == 1, 'prior1',
                     ifelse(data$prior2 == 1, 'prior2',
                            ifelse(data$during == 1, 'during', 'after')))

data$period = as.factor(data$period)

#make sure they're all live locations:
levels(data$mort.status)

# subset to the late winter period
dat.hr = data[data$herd == 'hr',]
#HR prior1: 2019-12-17 to 2020-02-04
dat.hr$prior1 = ifelse(dat.hr$herd == 'hr' & dat.hr$timestamp >= '2019-12-17' & dat.hr$timestamp <= '2020-02-04', 1, 0)

#HR prior2: 2018-12-10 to 2019-01-21
dat.hr$prior2 = ifelse(dat.hr$herd == 'hr' & dat.hr$timestamp >= '2018-12-10' & dat.hr$timestamp <= '2019-01-21', 1, 0)

#HR during: 2020-12-09 to 2021-02-18
dat.hr$during = ifelse(dat.hr$herd == 'hr' & dat.hr$timestamp >= '2020-12-09' & dat.hr$timestamp <= '2021-02-18', 1, 0)

#HR after: 2022-01-22 to 2022-02-25
dat.hr$after = ifelse(dat.hr$herd == 'hr' & dat.hr$timestamp >= '2022-01-22' & dat.hr$timestamp <= '2022-02-25', 1, 0)

#COLUMBIA NORTH
dat.cn = data[data$herd == 'cn',]
#CN prior1: 2020-01-17 to 2020-03-26
dat.cn$prior1 = ifelse(dat.cn$timestamp >= '2020-01-17' & dat.cn$timestamp <= '2020-03-26', 1, 0) 

#CN prior2: 2019-02-19 to 2019-04-01
dat.cn$prior2 = ifelse(dat.cn$timestamp >= '2019-02-19' & dat.cn$timestamp <= '2019-04-01', 1, 0)

#CN during: 2021-01-29 to 2021-03-25
dat.cn$during = ifelse(dat.cn$timestamp >= '2021-01-29' & dat.cn$timestamp <= '2021-03-25', 1, 0)

#CN after: 2022-01-08 to 2022-04-08
dat.cn$after = ifelse(dat.cn$timestamp >= '2022-01-08' & dat.cn$timestamp <= '2022-04-08', 1, 0)

#CENTRAL SELKIRKS  
dat.cs = data[data$herd == 'cs',]
#CS prior1: 2019-12-31 to 2020-03-10
dat.cs$prior1 = ifelse(dat.cs$timestamp >= '2019-12-31' & dat.cs$timestamp <= '2020-03-10', 1, 0)

#CS prior2: 2019-02-26 to 2019-04-08
dat.cs$prior2 = ifelse(dat.cs$timestamp >= '2019-02-26' & dat.cs$timestamp <= '2019-04-08', 1, 0)

#CS during: 2020-12-30 to 2021-04-08
dat.cs$during = ifelse(dat.cs$timestamp >= '2020-12-30' & dat.cs$timestamp <= '2021-04-08', 1, 0)

#CS after: 2022-02-19 to 2022-04-08
dat.cs$after = ifelse(dat.cs$timestamp >= '2022-02-19' & dat.cs$timestamp <= '2022-04-08', 1, 0)

#subset for just the period within the winter range windows
#each of the dates is subset in 06.Batch_Run.r so these last 6 lines are not required, but added to calculate sample size.:
dat.cn$resident = rowSums(dat.cn[,c("prior1", "prior2", "during", "after")])
dat.cn = dat.cn[dat.cn$resident == 1,]
dat.cs$resident = rowSums(dat.cs[,c("prior1", "prior2", "during", "after")])
dat.cs = dat.cs[dat.cs$resident == 1,]
dat.hr$resident = rowSums(dat.hr[,c("prior1", "prior2", "during", "after")])
dat.hr = dat.hr[dat.hr$resident == 1,]

data = rbind(dat.cn, dat.cs, dat.hr)

library(dplyr)
check = data %>%
  group_by(herd, period) %>%
  summarise(first_date = min(timestamp), last_date = (max(timestamp)))

#-------------------------------------------------------------------------------
#READ IN LANDSCAPE RASTERS:
#-------------------------------------------------------------------------------
elev = rast('./data/rasters/rasters_for_prediction/elev_230927.tif')
names(elev) = 'elev_220809'
slope = rast('./data/rasters/rasters_for_prediction/slope_230927.tif')
names(slope) = 'slope_220809'
# forest age had age replaced where buffered roads (30m) and cutblocks overlaid 
proj.age = rast('./data/rasters/rasters_for_prediction/proj_age_230927.tif')
#NAs throwing error, reclass to 0
# first fill no data
proj.age = focal(proj.age, w = 5, fun = 'modal', na.policy = "only")
# if they persist reclassify to 0
m <- c(NA,NA,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
proj.age <- classify(proj.age, rclmat, include.lowest=TRUE)

proj.age = resample(proj.age, elev)
names(proj.age) = "proj_age_220809"

crown_clos = rast('./data/rasters/rasters_for_prediction/crown_clos_230927.tif')
crown_clos = focal(crown_clos, w = 5, fun = 'modal', na.policy = "only")
crown_clos = classify(crown_clos, rclmat, include.lowest = TRUE)
names(crown_clos) = "crown_clos"

#add glaciers and distance to road
glacier = rast('./data/rasters/rasters_for_prediction/glacier_230927.tif')
names(glacier) = 'glacier'

# add some disturbances in:
fire = rast("C:/Users/Ryan/OneDrive/Biodiveristy Pathways/primary_prey_habitat/data/rasters/fire_raster.tif")
names(fire) = 'burned'
cutblock = rast("C:/Users/Ryan/OneDrive/Biodiveristy Pathways/primary_prey_habitat/data/rasters/cutblock_raster.tif")
names(cutblock) = 'logged'
fire = resample(fire, elev)
cutblock = resample(cutblock, elev)
#road_dist = rast('./data/rasters/rasters_for_prediction/distance_roads_230915.tif')
#road_dist = resample(road_dist, elev)
#names(road_dist) = 'dist_roads'


# replace age with age of fire to update burned areas
proj.age = ifel(fire < 200, fire, proj.age)
proj.age = ifel(cutblock < 200, cutblock, proj.age)
# this results in a forest age raster with roads, cutblocks and fire age replacing
# forest age values at the locations those disturbances occur.

# convert fire and cutblock to 0/1 like glacier
burned = ifel(fire == 200,0,1)
names(burned) = 'burned'

logged = ifel(cutblock == 200,0,1)
names(logged) = 'logged'

#heli.ten = rast('./data/rasters/august/heli_ten_220809.tif')
#dist_road = rast('./data/rasters/august/distance_to_roads.tif')
#roughness = rast('./data/rasters/rasters_for_prediction/roughness_220809.tif')
#wind.sh = rast('./data/rasters/august/wind_shelter_index_225.tif')

# add in some other vegetation layers to see how they perform
#needle trees extent in QGIS to elevation by exporting and setting crs to Albers:
#needles = rast('./data/rasters/august/Tree_Cover/needle_trees_clipped_elev.tif')
#res(needles)
#crs(needles)

#needles.res = resample(needles, elev)
#res(needles.res)
#res(elev)
#xstack = c(elev, needles.res)

#writeRaster(needles.res, './data/rasters/august/VegetationData/needle_trees_220809.tif')

#needles = rast('./data/rasters/august/VegetationData/needle_trees_220809.tif')
#names(needles) = "needles"


#-------------------------------------------------------------------------------
# some rasterizing
#ext(wind.sh) = ext(elev)
#get glaciers
#vri = st_read('C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/spatial/vri/VRI_2020_220601.shp')
#gl = vri[vri$BCLCS_LE_4 == 'GL',]
#gl$glacier = 1
#gl.raster = rasterize(gl, elev, field = 'glacier', background = 0, filename = './data/rasters/august/glaciers.tif')

#add in stand openness as per: https://www.sciencedirect.com/science/article/pii/S0378112722004108
#vri = st_read('C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/spatial/vri/VRI_2020_220601.shp')
#rasterize based on CROWN_CLOS
#cc = rasterize(vri, elev, field = 'CROWN_CLOS', background = 0, filename = './data/rasters/august/crown_closure_class.tif')
#res(cc)
#nrow(cc)
#test
#xstack = c(elev, cc)
#writeRaster(cc, './data/rasters/august/crown_clos_220809.tif', overwrite = TRUE)

#get distance to road
#roads = st_read('C:/Users/Ryan/OneDrive/geodatabase/roads/roads_FSR_provincial/roads_FSR_provincial.shp')
#road.raster = rasterize(roads, elev, field = 'road', background = 0, filename = './data/rasters/august/roads.tif')
#distance(road.raster, target = 1, exclude=NULL, unit="m", haversine=TRUE, 
#         overwrite = TRUE, filename='./data/rasters/large_extent/distance_to_roads.tif')
#-------------------------------------------------------------------------------

#named list of rasters for rsf:
#rstack = c(elev, slope, heli.ten, proj.age, glacier, dist_road, wind.sh, ndvi, needles, crown_clos)
# selected variables
rstack = c(elev, slope, proj.age, glacier, crown_clos, burned, logged)

#-------------------------------------------------------------------------------
#-----------------------CHANGE PERIOD------------------------------------------#
period = 'during'
dat = droplevels(data[data$period == period,]) 
#-----------------------CHANGE PERIOD------------------------------------------#

#In addition to those collars above, remove those collars we deemed as having 
#problematic outliers not detected during cleaning: 32611 and 44095. Even though
#problems occured for only one period we'll remove them completely:
dat = dat[!(dat$individual.local.identifier %in% c(32611, 44095, 44098)),]

#CONVERT data to sf
#first have to go to WGS84:
dat.sp = st_as_sf(dat, coords = c('location.long','location.lat'), 
                  crs = st_crs('+init=EPSG:4326'))
#then to NAD83 Albers for metric:
dat.sp = st_transform(dat.sp, crs = st_crs('+init=EPSG:3005'))

#extract variables for each KNOWN location
#KNOWN:
dat.ext = terra::as.data.frame(extract(rstack, dat.sp, bind = TRUE, xy = TRUE, na.rm = TRUE))
dat.ext$pt_type = 'used'
names(dat.ext)

dat.ext = dat.ext[,c("id", "individual.local.identifier","timestamp", "mort.status", "herd", 
                     "prior2", "prior1", "during", "after", "elev_220809",
                     "slope_220809", "proj_age_220809", "glacier","crown_clos",
                     "burned","logged",'x', 'y','pt_type')]
#clean up:
gc()
#-------------------------------------------------------------------------------
#READ IN merged shapes:
#-------------------------------------------------------------------------------
period.shp = st_read(paste0('./data/home_range/merged_95_HR/',period,'_merged_95_HR_Albers.shp'))
period.shp$name = str_sub(period.shp$name, 0, 5)
#CHECK that the records in DATA match the records in the uds object. If not
#add a folder to the UD_path called censor and move UDs not in DATA into that 
#folder to avoid crashes.

#-------------------------------------------------------------------------------
#extract availability domain from each UD
#extract raster covariates for all points (known and available)
#gam each animal
#df = data.frame()
available = data.frame()
for(i in period.shp$name){
  print(i)
  x = period.shp[period.shp$name == i,]
  rstack0 = crop(rstack, x)
  rstack1 = mask(rstack0, x)
  rm(rstack0)
  gc()
  stack.df = terra::as.data.frame(rstack1, xy = TRUE)
  stack.df$period = period
  stack.df$individual.local.identifier = i
  stack.df$timestamp = as.POSIXct('00:00:00 2021-01-01', format = '%Y-%m-%d %H:%M', tz='UTC')
  stack.df$mort.status = "normal"
  stack.df$herd = 'XX'
  stack.df$prior2 = ifelse(period == 'prior2', 1,0)
  stack.df$prior1 = ifelse(period == 'prior1', 1,0)
  stack.df$during = ifelse(period == 'during', 1,0)
  stack.df$after = ifelse(period == 'after', 1,0)
  stack.df$period = period
  stack.df$id = 0
  stack.df$pt_type = 'available'
  
  available = rbind(stack.df, available)
}
names(available)
#reorganize so all fields match:
available = available[,c("id", "individual.local.identifier","timestamp", "mort.status", "herd", 
                         "prior2", "prior1", "during", "after", "elev_220809",
                         "slope_220809", "proj_age_220809", "glacier","crown_clos",
                         "burned","logged",'x', 'y','pt_type')]

#dat.period has available locations for each HR estimate for each animal
#can then use this dataset in a gam framework to create an rsf for each 
#individual
dat.period = rbind(dat.ext, available)

#check if any collars don't have both available and used:
dat.check = dat.period %>% 
  group_by(individual.local.identifier, pt_type) %>%
  summarise(num_each = n()) %>%
  filter(n() < 2)

write.csv(dat.period, paste0("./data/rsf/gam/",period,"_used_available_LATE_WINTER_231024.csv"))

#dat.test = dat.period[dat.period$individual.local.identifier == 81363,]

rm(dat.period, available, dat.check, elev, slope, proj.age, glacier, crown_clos, fire, cutblock,
   dat, dat.ext, dat.sp, rclmat, rstack1, stack.df)
gc()

#change period line 127
#-------------------------------------------------------------------------------
# WEIGHTS
#only need to run this once, further down we call the result of this section
#-------------------------------------------------------------------------------
#get weights for each HR estimate:
# create empty dataframe to hold results:
weights = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("period","ID", "weight"))
#-------------------------------------------------------------------------------
# CHANGE PERIOD:
#period = 'after'
#-------------------------------------------------------------------------------
# set the path to the folder holding rdas for each period:
UD_path = paste0("./data/home_range/", period, "/UDs_with_error")
#-------------------------------------------------------------------------------

#folder with UD
setwd(UD_path)

#load UDs from that folder:
ud.dir = list.files(pattern="*.rda", all.files=TRUE, 
                    full.names=FALSE)

for(i in 1:length(ud.dir)){
  load(ud.dir[[i]])
  print(i)
  # extract id and weights  
  res <- as.data.frame(period)
  res$ID <- cilla.akde@info$identity
  res$weight <- mean(cilla.akde$weights)
  weights = rbind(res, weights)
}  

setwd('../../../../')
getwd()
library(arrow)
write_parquet(weights, sink = "./data/home_range/akde_hr_weights_230707.parquet", row.names = FALSE)

#-------------------------------------------------------------------------------
write_parquet(p1, sink = "./data/rsf/gam/prior1_used_available_LATE_WINTER_231024.parquet")
write_parquet(p2, sink = "./data/rsf/gam/prior2_used_available_LATE_WINTER_231024.parquet")
write_parquet(du, sink = "./data/rsf/gam/during_used_available_LATE_WINTER_231024.parquet")
write_parquet(af, sink = "./data/rsf/gam/after_used_available_LATE_WINTER_231024.parquet")

#-------------------------------------------------------------------------------
rm(list = ls())

# read in each period:
p1 = read_parquet("./data/rsf/gam/prior1_used_available_LATE_WINTER_231024.parquet")
p2 = read_parquet("./data/rsf/gam/prior2_used_available_LATE_WINTER_231024.parquet")
du = read_parquet("./data/rsf/gam/during_used_available_LATE_WINTER_231024.parquet")
af = read_parquet("./data/rsf/gam/after_used_available_LATE_WINTER_231024.parquet")

#merge them together
data = rbind(p1, p2, du, af)
rm(p2,p1,du,af)

#-----------------##################--------------------------------------------
#data$period = 'during'
names(data)
head(data)
tail(data)
#check some out:
test = data[data$individual.local.identifier == '22560',]

#make data key to join:
data$key = paste(data$period, data$individual.local.identifier, sep = '-')

#read in the weights to join
weights = read.csv("./data/home_range/akde_hr_weights_230707.csv")
weights$key = paste(weights$period, weights$ID, sep = "-")
#join the weights to the data
dat = merge(data, weights, by = "key")

dat$herd = as.factor(dat$herd)

rm(data, weights)
gc()

# available points don't have herd assigned, but they do have ID assigned for
# points that fall within each animals HR estimate so I can assign a herd to 
# each available location
herds = dat[,c("individual.local.identifier", 'herd')] %>%
  distinct(individual.local.identifier, herd) %>%
  filter(!(herd == 'XX'))

dat = merge(dat, herds, by = "individual.local.identifier")
dat = rename(dat, 'herd' = 'herd.y',
             "period" = "period.x")
dat$herd.x = dat$period.y = dat$X = dat$ID = NULL
gc()
#write this:
write_parquet(dat, sink =  "./data/rsf/gam/all_data_LATE_WINTER_231024.parquet") #original result from all_data_230804.csv

# EOF
