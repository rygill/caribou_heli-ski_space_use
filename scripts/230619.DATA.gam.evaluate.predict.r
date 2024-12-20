##
# 2023-11-22 this script is run on a UBC lab computer for processing power, all data reside on
# the H drive at UBC.

# 2024-11-04 this script is run on RG's tower. Input files reside in OneDrive folder here:
# "C:\Users\Ryan\OneDrive\ABMI\caribou_anthropause\git_caribou\caribou_heli-ski_space_use"

#-------------------------------------------------------------------------------
# 2023-09-29
# load rasters
# stack
# convert to df
# predict
# rasterize
#-------------------------------------------------------------------------------

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
# rstack.All.df is the output from the stacking and saving done above. This script
# can be run from here down
#-------------------------------------------------------------------------------
# load the model
load("./models/mAll_LATE_WINTER_231113.rds") # this should be 231026, if not rerun
# load the log transformed age model
summary(mAll)

# load the stack as df with xy
rstack.All.df = st_read_parquet("./partitioned_all/stack_All_study_area_231025.parquet")
rstack.All.df$x = st_coordinates(rstack.All.df)[,1]
rstack.All.df$y = st_coordinates(rstack.All.df)[,2]



names(rstack.All.df)
#rm(stack.All.df)
gc()
# next step completed earlier
#rstack.All.df = rstack.All.df[!is.na(rstack.All.df$elev_220809) & !is.na(rstack.All.df$slope_220809),]
#write_parquet(rstack.All.df, "stack_All_231003.parquet")

# add those fields to exclude
rstack.All.df$herd = as.factor(rstack.All.df$herd)
rstack.All.df$individual.local.identifier = as.factor(rstack.All.df$individual.local.identifier)

# modifcation 2024-11-04:
rstack.All.df$glacier = as.factor(rstack.All.df$glacier)
rstack.All.df$burned = as.factor(rstack.All.df$burned)
rstack.All.df$logged = as.factor(rstack.All.df$logged)

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

# write the results of the loop
# next lines not run (2024-11-04)
#write_parquet(out.df, sink = "H:/caribou_anthropuase/prediction/prediction_ALL_loop_LATE_WINTER_log_trans_231025.parquet")
#out.df = read_parquet("H:/caribou_anthropuase/prediction/prediction_ALL_loop_LATE_WINTER_log_trans_231025.parquet")

# as per the test, convert this to a dataframe (I'm not sure if the parquet format is the same, but change regardless)
pred.df = as.data.frame(out.df[,c("x","y","pred.gam")])
rm(out.df)
gc()

# convert prediction to raster:
tic()
pred.all.spatras = rast(pred.df, crs = "EPSG:3005")
toc()
#get the 99.9% cutoff
global(pred.all.spatras, quantile, probs=c(0.05, 0.999), na.rm=TRUE)

# 2024-11-04 modification:
new.max = global(pred.all.spatras, quantile, probs=c(0.05, 0.999), na.rm=TRUE)[[2]]

pred.all.spatras.reclass = ifel(pred.all.spatras >= new.max, new.max, pred.all.spatras)
range(pred.all.spatras.reclass)

#writeRaster(pred.all.spatras.reclass, "./prediction/All_prediction_LATE_WINTER_231026.tif", overwrite = TRUE)

# standardize
pred.ras.standard = pred.all.spatras.reclass/new.max#1271261797#7572447167

#modification 2024-11-04
writeRaster(pred.ras.standard, "./predictions/standardized/All_prediction_LATE_WINTER_standardized_241104.tif", overwrite = TRUE)

# original:
#writeRaster(pred.ras.standard, "./prediction/All_prediction_LATE_WINTER_standardized_log_transformed_231026.tif", overwrite = TRUE)

#EOF


  
  

  #############################
# re-run with the model without the random effects
load("./models/mmod_LATE_WINTER_231113.rds") # this should be 231026, if not rerun
#rstack.All.df$pred.gam.mmod = predict(object = mmod,
#                                      newdata = rstack.All.df,
#                                      type = 'response', 
#                                      exclude = c("(Intercept)","s(individual.local.identifier)"), proximity = FALSE)


out.mmod.df = data.frame()
for(i in unique(rstack.All.df$chunk.id)){
  print(i)
  mmod.df = rstack.All.df[rstack.All.df$chunk.id == i,]
  mmod.df$pred.gam = predict(object = mmod,
                             newdata = mmod.df,
                             type = 'response', 
                             exclude = c("(Intercept)","s(individual.local.identifier)", "s(herd)"), proximity = FALSE)
  
  out.mmod.df = rbind(mmod.df, out.mmod.df)
  
}




out.mmod.df = select(out.mmod.df, c('x','y','pred.gam'))
out.mmod.df = rename(out.mmod.df, 'pred.gam.mmod' = 'pred.gam')
x.mmod = quantile(out.mmod.df$pred.gam.mmod,c(0.05,0.99), na.rm = TRUE)[[2]]
out.mmod.df$pred.gam.mmod = ifelse(out.mmod.df$pred.gam.mmod > x.mmod, x.mmod, out.mmod.df$pred.gam.mmod)
# standardize
out.mmod.df$pred.gam.mmod = out.mmod.df$pred.gam.mmod / x.mmod
x.mAll = quantile(out.df$pred.gam,c(0.05,0.99), na.rm = TRUE)[[2]]
out.df$pred.gam = ifelse(out.df$pred.gam > x.mAll, x.mAll, out.df$pred.gam)
# standardize
out.df$pred.gam = out.df$pred.gam / x.mAll

# rasterize and write
tic()
pred.mmod.all.spatras = rast(out.mmod.df, crs = "EPSG:3005")
toc()
writeRaster(pred.mmod.all.spatras, "./predictions/standardized/mmod_prediction_LATE_WINTER_standardized_241104.tif", overwrite = TRUE)
# this raster is used to test the difference in the quadrants

# join the two
out.df$jfield = paste(out.df$x, out.df$y, sep = '-')
out.mmod.df$jfield = paste(out.mmod.df$x, out.mmod.df$y, sep = '-')
out.df.all = merge(out.df, out.mmod.df, by = 'jfield')
write.csv(out.df.all, "./predictions/standardized/mAll_mmod_standardized_values.csv", row.names = FALSE)

#pred.ras.standard.mmod = pred.all.spatras.mmod/x
#writeRaster(pred.ras.standard.mmod, "./predictions/standardized/RATA_LATE_WINTER_standardized_mAll_241101_XXX.mmod.tif", overwrite = TRUE)

# load this joined df:
out.df.all = read.csv("./predictions/standardized/mAll_mmod_standardized_values.csv")
# reclassify each result as > or < 0.5
out.df.all = dplyr::rename(out.df.all, 'x' = 'x.x', 'y' = 'y.x')
out.df.all = dplyr::select(out.df.all, c(x,y,pred.gam, pred.gam.mmod))
out.df.all$mmod_class = ifelse(out.df.all$pred.gam.mmod < 0.5, 1, 2)
out.df.all$mAll_class = ifelse(out.df.all$pred.gam < 0.5, 1, 2)
out.chi.dat = pivot_longer(out.df.all, names_to = 'model', values_to = 'prediction')
 
#### chi square 
ras.mAll = select(out.df, c(x,y,pred.gam))
ras.mAll = as.data.frame(ras.mAll)
ras.mAll = select(ras.mAll, -geometry)
ras.mmod = select(out.df, c(x,y,pred.gam.mmod))
ras.mmod = as.data.frame(ras.mmod)
ras.mmod = select(ras.mmod, -geometry)


ras.mAll = rast(ras.mAll, crs = "EPSG:3005")
ras.mmod = rast(ras.mmod, crs = "EPSG:3005")
plot(ras.mAll)

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
ras.mAll10 <- classify(ras.mAll, rclmat, include.lowest=TRUE)
ras.mmod10 <- classify(ras.mmod, rclmat, include.lowest=TRUE)
#plot(smc)

ras.mAll10 = as.data.frame(ras.mAll10)
ras.mAll10$model = 'mAll'
ras.mmod10 = as.data.frame(ras.mmod10)
ras.mmod10$model = 'mmod'
ras.mmod10 = rename(ras.mmod10, 'pred.gam' = 'pred.gam.mmod')
hist(ras.mmod10$pred.gam)

chi.dat = rbind(ras.mAll10, ras.mmod10)

# get total points per period
dat.total = chi.dat %>%
  group_by(model, pred.gam) %>%
  summarise(total_pts_set = n())

barplot(dat.total$pred.gam, dat.total$total_pts_set)
ggplot() +
  geom_col(data = dat.total, aes(x = pred.gam, y = total_pts_set)) +
  facet_grid(~model)

# need the format: 
# use dat.total but convert to wide so there are columns mAll, mmod, pred.ga
dat.chi = tidyr::pivot_wider(dat.total, names_from = 'model', values_from = 'total_pts_set')
chisq.test(dat.chi$mAll, dat.chi$mmod)





# EOF


