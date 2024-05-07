#
library(arrow)
library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(ggridges)

## -----------------------------
# commented out lines have been run to generate the data used later in the script.
## -----------------------------

# load the caribou rsf and calculate the median for each tenure
#bou.rsf = read_parquet("./data/prediction_ALL_loop_LATE_WINTER_231113.parquet")
#bou.rsf = st_as_sf(bou.rsf, coords = c('x','y'), crs = "EPSG:3005")
# get herd name associated with the rsf value

# load the tenure shapefile
#ten.shp = st_read('./data/heliski_tenures_names.shp')
# crop the smc rsf output to the tenures, this has probably been done already
#bou.rsf = st_crop(bou.rsf, ten.shp)
#gc()
#str(bou.rsf)
# convert to a dataframe
#bou.rsf = as.data.frame(bou.rsf)

#write_parquet(bou.rsf, sink = "./data/prediction_ALL_loop_LATE_WINTER_231113_CLIPPED_TENURES.parquet")
#ten.names = as.data.frame(ten.shp[,c('id','licencee')])
#ten.names = dplyr::select(ten.names, -geometry)
#ten.names = dplyr::rename(ten.names, 'tenure_id' = 'id')
#ten.names$tenure_id = as.factor(ten.names$tenure_id)
# load the heli-ski tenures
#tenures = rast('../rasters_for_prediction/tenure_id.tif')

# assign the tenure id to each rsf cell value
#tenure.bou = extract(tenures, bou.rsf, bind = TRUE, xy = TRUE, na.rm = TRUE, fun = 'median')


# writing this as one file produces errors, so break it up into chunks
#for(i in unique(ten.shp$id)){
#  print(i)
#  bou.ten = tenure.bou.df[tenure.bou.df$tenure_id == i,]
#  write_parquet(bou.ten, sink = paste0('./data/segmented_predictions/caribou_rsf_values_by_tenure_',i,'.parquet'))
#}

dat.dir = './data/segmented_predictions/'
setwd(dat.dir)
bou.dir = list.files(pattern = 'caribou*', all.files=TRUE, 
                     full.names=FALSE)

bou.dir

# create a list to append the files to
bou.list = list()

# loop through the directory and read those files in, adding a field for MU from the file name
for(b in bou.dir){
  #b = "caribou_rsf_values_by_tenure_31.parquet"
  bfile = read_parquet(b)
  
  bou.list[[b]] <-  bfile # append it to the list
}

setwd('../../') # back out of that directory to the main

# append those items into a dataframe
tenure.bou.df = do.call(rbind, bou.list)
rm(bfile, bou.list, b, bou.dir)

# get rid of an old column
tenure.bou.df$pred.log = NULL

# standardize then log
tenure.bou.df$smc.log = log(tenure.bou.df$pred.gam.smc)
tenure.bou.df$smc.std.log = tenure.bou.df$smc.log / max(tenure.bou.df$smc.log, na.rm = TRUE)
hist(tenure.bou.df$smc.std.log)


ggplot(tenure.bou.df, aes(x = smc.std.log, y = licencee, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0) +
  scale_fill_viridis_c(option = "D") +
  theme_bw() +
  theme(legend.position = 'none')
xlabs('Temperatures in Lincoln NE in 2016')

# END BOU
#-------------------------------------------------------------------------------
# HELI

# load the heli-ski rsf
#heli.rsf = read_parquet('./predictions/HELI_pred_gam_all_data_231122.parquet')
#heli.rsf = heli.rsf[!(heli.rsf$tenure_id == 0),c('pred.gam.bn', 'tenure_id', 'x', 'y')]
#heli.rsf = st_as_sf(heli.rsf, coords = c('x','y'), crs = "EPSG:3005")
#herds = rast('../rasters_for_prediction/herd_id.tif')

#herd.bound = dplyr::rename(herd.bound, 
#                           'herd_id' = 'id')
#herd.bound = herd.bound[,c('herd_id','HERD_NAME')]
#herd.bound$herd_id = as.factor(herd.bound$herd_id)

#heli.herd = extract(herds, heli.rsf, bind = TRUE, xy = TRUE, na.rm = TRUE, fun = 'median')
#heli.herd.df = as.data.frame(heli.herd)
#heli.herd.df = dplyr::rename(heli.herd.df, 
#                             'pred.gam.heli' = 'pred.gam.bn')

#heli.herd.df = merge(heli.herd.df, herd.bound, by = 'herd_id')

#for(i in unique(heli.herd.df$herd_id)){
#  print(i)
#  heli.df = heli.herd.df[heli.herd.df$herd_id == i,]
#  write_parquet(heli.df, sink = paste0('./data/segmented_predictions/heli_rsf_values_by_herd_',i,'.parquet'))
#}


dat.dir = './data/segmented_predictions/'
setwd(dat.dir)

hel.dir = list.files(pattern = 'heli*', all.files=TRUE, 
                     full.names=FALSE)

hel.dir

# create a list to append the files to
hel.list = list()

# loop through the directory and read those files in, adding a field for MU from the file name
for(h in hel.dir){
  #h = "heli_rsf_values_by_herd_10.parquet"
  hfile = read_parquet(h)
  #bfile$MU = str_sub(g, start = 21, end = 23)
  hel.list[[h]] <-  hfile # append it to the list
}

setwd('../../') # back out of that directory to the main

# append those items into a dataframe
# these are heli-ski suitability predictions by herd
heli.herd.df = do.call(rbind, hel.list)

rm(hel.list, hfile, h, hel.dir)

# standardize then log
heli.herd.df$hel.log =  log(heli.herd.df$pred.gam.heli)
heli.herd.df$hel.std.log = heli.herd.df$hel.log / max(heli.herd.df$hel.log, na.rm = TRUE)

hist(heli.herd.df$hel.std.log)

# recovered from slack:
ggplot(heli.herd.df, aes(x = std.log, y = HERD_NAME)) + #, tried adding alpha here with no change
  geom_density_ridges() + # alpha here produces an error
  xlab("Heli-ski suitability score") +
  ylab("Caribou herd") +
  #scale_fill_viridis_c(name = 'SMC rsf /n score', alpha = 0.2)  +# adding alpha = heli.df$extirpated here produces some weird pattern
  scale_alpha_manual(values = c(0.2,1)) 


# merge the dataframes by their location
heli.herd.df$jfield = paste(heli.herd.df$x, heli.herd.df$y, sep = '-')
tenure.bou.df$jfield = paste(tenure.bou.df$x, tenure.bou.df$y,sep = '-')

mega.dat = merge(heli.herd.df, tenure.bou.df, by = 'jfield')

mega.dat = select(mega.dat,
                  c('licencee', 'HERD_NAME','x.x', 'y.x', 'smc.std.log', 'hel.std.log'))

mega.dat = rename(mega.dat, 'x' = 'x.x', 'y' = 'y.x')

h.rds = unique(mega.dat$HERD_NAME)

for(h in unique(h.rds)){
  #h = 'Barkerville'
  print(h)
  ggplot(mega.dat[mega.dat$HERD_NAME == h,]) +
     theme_bw() +
     ggtitle(h) +
     theme(legend.position = 'none',
           plot.title = element_text(hjust = 1)) +
     geom_point(aes(x = hel.std.log, y = smc.std.log, alpha = 0.05), size = 0.3, stroke = NA) +
     geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'red') +
     geom_vline(xintercept = 0.5, linetype = 'dashed', color = 'red') +
     labs(x = "Log standardized heli-ski suitability", y = "Log standardized SMC suitability") 
  ggsave(file = paste0('./figures/mega_figs/',h,'_mega_dat.jpg'),
         width = 1500,
         height=1011,
         units = "px",
         dpi = 300)

}

# next lines not used
# overlap
#over = rast('./predictions/standardized/product_RATA_HELI_std_231122.tif')
#over.df = terra::as.data.frame(over)

#herds = rast('../rasters_for_prediction/herd_id.tif')
#tenures = rast('../rasters_for_prediction/tenure_id.tif')
#h.t = c(herds, tenures)

#over.df = extract(h.t, over.df, bind = TRUE, na.rm = TRUE)

# reclassify based on quadrants in above figures greater/less than 0.5
heli = rast('./predictions/standardized/HELI_LATE_WINTER_standardized_231121.tif')
heli = ifel(heli < 0.5, 1, 3)

smc = rast('./predictions/standardized/RATA_LATE_WINTER_standardized_231113.tif')
smc = ifel(smc < 0.5,1,5)
cstack = c(smc, heli)
comb = sum(cstack)

writeRaster(comb, './predictions/standardized/reclassed_heli_smc_240506.tif')
# 2 = lower left
# 4 = lower right
# 6 = upper left
# 8	= upper right
