# 2023-11-22 this script is run on my laptop

library(terra)
library(sf)

# elevation
elev = rast('../rasters_for_prediction/elev_230927.tif')
names(elev) = 'elev_220809'

# slope
slope = rast('./data/rasters/rasters_for_prediction/slope_230927.tif')
names(slope) = 'slope_220809'

#aspect = terrain(elev, v = 'aspect', unit = "degrees")
#writeRaster(aspect, "./data/rasters/rasters_for_prediction/aspect_230927.tif")

# tpi
#tpi <- focal(elev, w=matrix(7/7, nc=7, nr=7), fun = \(elev) elev[5] - mean(elev[-5]))
#names(tpi) = 'tpi'
#writeRaster(tpi, './rasters_for_prediction/tpi_230927.tif')
tpi = rast('./rasters_for_prediction/tpi_230927.tif')

# slope length
slope_length = rast('./rasters_for_prediction/slope_length_230927.tif')
  
# wind exposure
wind = rast('./rasters_for_prediction/wind_exposure_230927_230927.tif')

dist.ldge = rast('./rasters_for_prediction/heli_lodge_proximity_230927.tif')

# read study area to crop to
sa = st_read("./spatial_data/study_area_230919.shp")

# rasterize tenure to use as a random effect
tenures = st_read('./data/heliski_tenures_names.shp')
ten.ras = rasterize(tenures, elev, field = 'id', background = 0)
names(ten.ras) = 'tenure_id'
writeRaster(ten.ras,'../rasters_for_prediction/tenure_id.tif')

herd.bound = st_read("./data/smc_herd_boundaries_sg_ID.shp")
herd.bound = herd.bound[,c('id','HERD_NAME')]
# add id column since HERD_NO is giving incorrect results
#herd.bound$id = seq.int(nrow(herd.bound))
#herd.bound = st_as_sf(herd.bound, coords = 'geometry', crs = "EPSG:3005")
#st_write(herd.bound, dsn = "./data/smc_herd_boundaries_sg_ID.shp", driver = 'ESRI Shapefile')
herd.bound = rasterize(herd.bound, elev, field = 'id', background = 0)
names(herd.bound) = 'herd_id'
writeRaster(herd.bound,'../rasters_for_prediction/herd_id.tif', overwrite = TRUE)



