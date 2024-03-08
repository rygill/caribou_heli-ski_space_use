
#-------------------------------------------------------------------------------
# 2023-09-29
# load rasters
# stack
# convert to df
# predict
# rasterize
#-------------------------------------------------------------------------------
setwd("H:/caribou_anthropuase")


library(mgcv)
library(terra)

library(dplyr)
library(ggplot2)
library(sf)

library(arrow)
library(sfarrow)

library(tictoc)


rm(list = ls())
gc()
#-------------------------------------------------------------------------------
# load the rasters
elev = rast('./rasters_for_prediction/elev_230927.tif')
names(elev) = 'elev_220809'
slope = rast('./rasters_for_prediction/slope_230927.tif')
names(slope) = 'slope_220809'
# forest age had age replaced where buffered roads (30m) and cutblocks overlaid 
proj.age = rast('./rasters_for_prediction/proj_age_230927.tif')
#NAs throwing error, reclass to 0
# first fill no data
proj.age = focal(proj.age, w = 5, fun = 'modal', na.policy = "only")
# if they persist reclassify to 0
m <- c(NA,NA,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
proj.age <- classify(proj.age, rclmat, include.lowest=TRUE)

proj.age = resample(proj.age, elev)
names(proj.age) = "proj_age_220809"

crown_clos = rast('./rasters_for_prediction/crown_clos_230927.tif')
crown_clos = focal(crown_clos, w = 5, fun = 'modal', na.policy = "only")
crown_clos = classify(crown_clos, rclmat, include.lowest = TRUE)
names(crown_clos) = "crown_clos"

#add glaciers and distance to road
glacier = rast('./rasters_for_prediction/glacier_230927.tif')
names(glacier) = 'glacier'

# add some disturbances in:
fire = rast("H:/BP_EVI/rasters/fire_raster.tif")
names(fire) = 'burned'
cutblock = rast("H:/BP_EVI/rasters/cutblock_raster.tif")
names(cutblock) = 'logged'

# get them in the same extent and resolution
fire = resample(fire, elev)
cutblock = resample(cutblock, elev)

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

#sa.ras = rasterize(sa, field = 'id', elev, background = 0)
#writeRaster(sa.ras, "H:/caribou_anthropuase/sa_ras.tif")
sa.ras = rast("H:/caribou_anthropuase/sa_ras.tif")
names(sa.ras) = 'region_id'

# all
rstack = c(elev, slope, proj.age, glacier, crown_clos, burned, logged, sa.ras)
names(rstack)

# ALL
# convert to a dataframe
rstack.df = terra::as.data.frame(rstack, xy = TRUE)
rstack.df = rstack.df[!is.na(rstack.df$elev_220809) & !is.na(rstack.df$slope_220809),]
rstack.df$individual.local.identifier = 29088
rstack.df$herd = 1

# save these individually to save space and allow re-loading
rstack.df.1 = rstack.df[rstack.df$region_id == 1,]
write_parquet(rstack.df.1, sink = "./partitioned_all/stack_All_1_231025.parquet")

rstack.df.2 = rstack.df[rstack.df$region_id == 2,]
write_parquet(rstack.df.2, sink = "./partitioned_all/stack_All_2_231025.parquet")

rstack.df.rest = rstack.df[rstack.df$region_id %in% c(3,4,6,7),]
write_parquet(rstack.df.rest, sink = "./partitioned_all/stack_All_rest_231025.parquet")

rm(list = ls())
gc()

# read study area to crop to
sa = st_read("./spatial_data/study_area_230919.shp")

#load individually and clip to study area to reduce size
all1 = read_parquet("./partitioned_all/stack_All_1_231025.parquet")
all1 = st_as_sf(all1, coords = c("x","y"), crs = "EPSG:3005")
all1 = st_crop(all1, sa)
st_write_parquet(all1, dsn = "./partitioned_all/stack_All_1_cropped_231025.parquet")
rm(all1)

all2 = read_parquet("./partitioned_all/stack_All_2_231025.parquet")
all2 = st_as_sf(all2, coords = c("x","y"), crs = "EPSG:3005")
all2 = st_crop(all2, sa)
st_write_parquet(all2, dsn = "./partitioned_all/stack_All_2_cropped_231025.parquet")
rm(all2)

all.rest = read_parquet("./partitioned_all/stack_All_rest_231025.parquet")
all.rest = st_as_sf(all.rest, coords = c("x","y"), crs = "EPSG:3005")
all.rest = st_crop(all.rest, sa)
st_write_parquet(all.rest, dsn = "./partitioned_all/stack_All_rest_cropped_231025.parquet")
rm(all.rest)

# reload these together and merge them
all1 = st_read_parquet("./partitioned_all/stack_All_1_cropped_231025.parquet")
all2 = st_read_parquet("./partitioned_all/stack_All_2_cropped_231025.parquet")
allrest = st_read_parquet("./partitioned_all/stack_All_rest_cropped_231025.parquet")

# now we have a complete ALL stack cropped to a smaller study area to remove those
# potential outlying values
stack.All.df = rbind(all1, all2, allrest)
st_write_parquet(stack.All.df, dsn = "./partitioned_all/stack_All_study_area_231025.parquet")

rm(list = ls())
gc()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ALL
#-------------------------------------------------------------------------------
# load the model
load("H:/caribou_anthropuase/mAll_LATE_WINTER_231024.rds") 
# load the log transformed age model

load("H:/caribou_anthropuase/mAll_LATE_WINTER_log_transformed_age_231026.rds") 

summary(mAll)
# load the stack as df with xy
rstack.All.df = st_read_parquet("./partitioned_all/stack_All_study_area_231025.parquet")
names(rstack.All.df)
#rm(stack.All.df)
gc()
# next step completed earlier
#rstack.All.df = rstack.All.df[!is.na(rstack.All.df$elev_220809) & !is.na(rstack.All.df$slope_220809),]
#write_parquet(rstack.All.df, "stack_All_231003.parquet")

# add those fields to exclude
rstack.All.df$herd = as.factor(rstack.All.df$herd)
rstack.All.df$individual.local.identifier = as.factor(rstack.All.df$individual.local.identifier)

#rstack.All.df = st_as_sf(rstack.All.df, coords = c("x", "y"), crs = "EPSG:3005")
#gc()

summary(mAll)

# temp
#rstack.All.df[is.na(rstack.All.df)] = 0
#table(is.na(rstack.All.df$crown_clos))
#table(is.na(rstack.All.df$proj_age_220809))

# predict
bloc.len = 500000

# create an id for each block
rstack.All.df$chunk.id <- as.factor(rep(seq(1, 1 + nrow(rstack.All.df) %/% bloc.len), 
                                        each = bloc.len, length.out = nrow(rstack.All.df)))
levels(rstack.All.df$chunk.id)

rstack.All.df$x = st_coordinates(rstack.All.df)[,1]
rstack.All.df$y = st_coordinates(rstack.All.df)[,2]

rstack.All.df = as.data.frame(rstack.All.df)

gc()
out.df = data.frame()
for(i in unique(rstack.All.df$chunk.id)){
  print(i)
  pred.df = rstack.All.df[rstack.All.df$chunk.id == i,]
  pred.df$pred.gam = predict(object = mAll,
                             newdata = pred.df,
                             type = 'response', 
                             exclude = c("(Intercept)","s(individual.local.identifier)", "s(herd)"), proximity = FALSE)
  
  out.df = rbind(pred.df, out.df)
  
}

str(out.df)

# subset
out.df = out.df[,c("x","y","pred.gam")]

#hist(rstack.All.df$elev_220809)
#hist(rstack.All.df$slope_220809)
#hist(rstack.All.df$crown_clos)

# write the results of the loop
write_parquet(out.df, sink = "H:/caribou_anthropuase/prediction/prediction_ALL_loop_LATE_WINTER_log_trans_231025.parquet")
out.df = read_parquet("H:/caribou_anthropuase/prediction/prediction_ALL_loop_LATE_WINTER_log_trans_231025.parquet")

# as per the test, convert this to a dataframe (I'm not sure if the parquet format is the same, but change regardless)
pred.df = as.data.frame(out.df[,c("x","y","pred.gam")])
rm(out.df)
gc()

# extract prediction to raster stack to check the predictor values where these large values fall
#check.stack = st_as_sf(rstack.All.df, coords = c("x","y"), crs = "EPSG:3005")
#check.stack = extract(pred.all.spatras, check.stack, bind = TRUE)

# convert prediction to raster:
tic()
pred.all.spatras = rast(pred.df, crs = "EPSG:3005")
toc()
#get the 99.9% cutoff
global(pred.all.spatras, quantile, probs=c(0.05, 0.999), na.rm=TRUE)

pred.all.spatras.reclass = ifel(pred.all.spatras >= 1271261797, 1271261797, pred.all.spatras)

# checking max predicted values for known locations - I read in the data, selected
# for those known locations, extracted the predicted value at each known location and
# took the maximum, which was 885. This could be used as 

#writeRaster(pred.all.spatras.reclass, "./prediction/All_prediction_LATE_WINTER_231026.tif", overwrite = TRUE)

# standardize
pred.ras.standard = pred.all.spatras.reclass/1271261797#7572447167
writeRaster(pred.ras.standard, "./prediction/All_prediction_LATE_WINTER_standardized_log_transformed_231026.tif", overwrite = TRUE)

# EOF

#####-------------------------------------------------------------------------------
# All has some very high values
# use focal to smooth out those high areas
#pred.all.mean = terra::focal(pred.all.spatras, w = 3, fun =  function(x){sqrt(mean((x[5] - x[-5]))^2)}, na.policy = "omit")
#plot(pred.all.mean)
#range(pred.all.mean)
#pre.all.mean.std = pred.all.mean / 15633.52
#writeRaster(pred.all.mean, "./prediction/focal_all_prediction_231011.tif", overwrite = TRUE)
#pred.mean.df = as.data.frame(pred.all.mean, xy = TRUE)


# k means clustering on the focal to find breakpoints in the data

# first determine the number of clusters
#library(factoextra)
# I keep getting an error that cannot allocate vector with anything greater than 1000 rows
# run the loop to see if the figure changes
#for(i in 1:100){
#  b = pred.mean.df[sample(nrow(pred.mean.df), 1000), ]
#  print(fviz_nbclust(b, kmeans, method = "wss") +
#    geom_vline(xintercept = 3, linetype = 2))
#  
#  }
#rm(knee.dat, pred.all.spatras, rstack.All.df)
#gc()
# consistently 3 clusters, but I think we want to exclude those really high clusters
# so I'll use more.

#pp = kmeans(pred.mean.df, 5, nstart = 25)
#print(pp)

#assign the cluster to the data
#pred.mean.cluster <- cbind(pred.mean.df, cluster = pp$cluster)

# what are the values for each cluster
#cluster.max = pred.mean.cluster %>%
#  group_by(pred.mean.cluster$cluster) %>%
#  summarise(max_value = max(pred.gam)) %>%
#  rename('cluster' = 'pred.mean.cluster$cluster')

#cluster.max = cluster.max[cluster.max$max_value < 100,]

#ggplot() +
#  geom_point(data = cluster.max, aes(x = cluster, y = max_value))

#write_parquet(pred.mean.cluster, sink = "H:/caribou_anthropuase/prediction/prediction_ALL_updated_focal_cluster_231011.parquet")

#plot(pred.mean.cluster$cluster, pred.mean.cluster$pred.gam)

#------------#################--------------------------------------------------
#pred.mean.cluster = read_parquet("./prediction/prediction_focal_cluster_231006.parquet")
# set cluster 2 pred.gam value to NA
#pred.mean.cluster$pred.gam = ifelse(pred.mean.cluster$cluster %in% c(3,4,5), 
#                                    NA, pred.mean.cluster$pred.gam )

#hist(pred.mean.cluster$pred.gam)
# rasterize
#cluster.raster = rast(pred.mean.cluster, crs = "EPSG:3005")
#names(cluster.raster[[1]])
#writeRaster(cluster.raster[[1]], "H:/caribou_anthropuase/prediction/cluster_raster_231006.tif", overwrite = TRUE)

#------------#################--------------------------------------------------
#cluster.raster = rast("./prediction/cluster_raster_231006.tif")

# standardize
#pred.all.spatras.std = pred.all.spatras / max(pred.all.spatras) #3.332568e+02
#range(pred.all.spatras)
#hist(pred.all.spatras)
#writeRaster(pred.all.spatras, "H:/caribou_anthropuase/prediction/prediction_All_231004.tif", overwrite = TRUE)
#writeRaster(pred.all.spatras.std, 
#            "H:/caribou_anthropuase/prediction/prediction_stndrdzd_All_231004.tif", 
#            datatype = "FLT4U", overwrite = TRUE)
#####-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


