#
library(arrow)
library(dplyr)
library(sf)
library(terra)
library(ggplot2)
#library(ggridges)

rm(list = ls())
gc()

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

# just standardize
tenure.bou.df$std.pred.smc = tenure.bou.df$pred.gam.smc / max(tenure.bou.df$pred.gam.smc, na.rm = TRUE)
range(tenure.bou.df$std.pred)

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
#herd.bound = st_read("./data/smc_herd_boundaries_sg_ID.shp")

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

# just standardize
heli.herd.df$std.pred.heli = heli.herd.df$pred.gam.heli / max(heli.herd.df$pred.gam.heli, na.rm = TRUE)

# standardize then log
heli.herd.df$hel.log =  log(heli.herd.df$pred.gam.heli)
heli.herd.df$hel.std.log = heli.herd.df$hel.log / max(heli.herd.df$hel.log, na.rm = TRUE)

hist(heli.herd.df$hel.std.log)

# recovered from slack:
#ggplot(heli.herd.df, aes(x = std.log, y = HERD_NAME)) + #, tried adding alpha here with no change
#  geom_density_ridges() + # alpha here produces an error
#  xlab("Heli-ski suitability score") +
#  ylab("Caribou herd") +
#  #scale_fill_viridis_c(name = 'SMC rsf /n score', alpha = 0.2)  +# adding alpha = heli.df$extirpated here produces some weird pattern
#  scale_alpha_manual(values = c(0.2,1)) 


# merge the dataframes by their location
heli.herd.df$jfield = paste(heli.herd.df$x, heli.herd.df$y, sep = '-')
tenure.bou.df$jfield = paste(tenure.bou.df$x, tenure.bou.df$y,sep = '-')

mega.dat = merge(heli.herd.df, tenure.bou.df, by = 'jfield')

mega.dat = select(mega.dat,
                  c('licencee', 'HERD_NAME','x.x', 'y.x', 'std.pred.smc', 'std.pred.heli')) #original 'smc.std.log', 'hel.std.log'

mega.dat = rename(mega.dat, 'x' = 'x.x', 'y' = 'y.x')

h.rds = unique(mega.dat$HERD_NAME)

# dataframe to annotate each figure
ann.df = data.frame(
  label = c('smc: > 0.5 \n heli < 0.5', 'smc: > 0.5 \n heli > 0.5', 'smc: < 0.5 \n heli < 0.5', 'smc: < 0.5 \n heli: > 0.5'),
  x = c(0.06, 0.9, 0.06, 0.9), #
  y = c(0.95, 0.95, 0.4, 0.4))

library(shadowtext)
for(h in unique(h.rds)){
  #h = 'Barkerville'
  print(h)
  ggplot(mega.dat) + #[mega.dat$HERD_NAME == h,]) +
     theme_bw() +
     ggtitle(h) +
     theme(legend.position = 'none',
           plot.title = element_text(hjust = 1)) +
     geom_point(aes(x = std.pred.heli, y = std.pred.smc, alpha = 0.05), size = 0.3, stroke = NA) + # original: x = hel.std.log, y = smc.std.log
     geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'red') +
     geom_vline(xintercept = 0.5, linetype = 'dashed', color = 'red') +
     geom_shadowtext(data=ann.df, aes(x=x, y=y, label=label), 
              size=3, color = 'white') +
     labs(x = "Standardized heli-ski suitability", y = "Standardized SMC suitability") 
  ggsave(file = paste0('./figures/mega_figs/not_logged/',h,'_mega_dat_std.jpg'),
         width = 1500,
         height=1011,
         units = "px",
         dpi = 300)

}

# crop to heli-ski tenures, so we're only looking at these values within existing tenures, for interests sake
mega.sf = st_as_sf(mega.dat, crs = 'EPSG:3005', coords = c('x','y'))
mega.sf = st_crop(mega.sf, ten.shp)
head(mega.sf)
mega.sf = as.data.frame(mega.sf)
str(mega.sf)

for(l in unique(mega.sf$licencee)){
  #l = 'CMH Silver'
  print(l)
  ggplot(mega.sf[mega.sf$licencee == l,]) +
    theme_bw() +
    ggtitle(h) +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 1)) +
    geom_point(aes(x = std.pred.heli, y = std.pred.smc, alpha = 0.05), size = 0.3, stroke = NA) + # original: x = hel.std.log, y = smc.std.log
    geom_hline(yintercept = 0.5, linetype = 'dashed', color = 'red') +
    geom_vline(xintercept = 0.5, linetype = 'dashed', color = 'red') +
    geom_shadowtext(data=ann.df, aes(x=x, y=y, label=label), 
                    size=3, color = 'white') +
    labs(x = "Standardized heli-ski suitability", y = "Standardized SMC suitability") 
  ggsave(file = paste0('./figures/mega_figs/not_logged/cropped_to_tenures/',l,'_mega_sf_std.jpg'),
         width = 1500,
         height=1011,
         units = "px",
         dpi = 300)
  
}

# reclassify based on quadrants in above figures greater/less than 0.5
library(terra)
library(sf)
library(dplyr)
library(ggplot2)

heli = rast('./predictions/standardized/HELI_LATE_WINTER_standardized_231121.tif')
# keeping distance in:
heli = rast('./predictions/standardized/HELI_LATE_WINTER_standardized_241108_WITH_DISTANCE_LODGE.tif')
range(heli)
heli = ifel(heli < 0.5, 1, 3)

smc = rast('./predictions/standardized/RATA_LATE_WINTER_standardized_231113.tif')
range(smc)
smc = ifel(smc < 0.5,1,5)
cstack = c(smc, heli)
comb = sum(cstack)

writeRaster(comb, './predictions/standardized/reclassed_heli_smc_240506_WITH_DISTANCE_LODGE.tif')
# 2 = lower left
# 4 = lower right
# 6 = upper left
# 8	= upper right

# get proportion of each class for each sub-population
rm(list = ls())
comb = rast('./predictions/standardized/reclassed_heli_smc_240506.tif')
#freq(comb)
# read smc boundaries
herd.bound = st_read("./data/smc_herd_boundaries_sg_ID.shp")

herd.bound = herd.bound[,c('id','HERD_NAME')]
herd.bound$id = as.factor(herd.bound$id)

ten.bound = st_read('./data/heliski_tenures_names.shp')
plot(ten.bound)

comb.ten = crop(comb, ten.bound)
comb.ten = mask(comb.ten, ten.bound)
vals_in_tenures = as.data.frame(freq(comb.ten))
vals_in_tenures$area_km = vals_in_tenures$count *1225 / 1000000
vals_in_tenures$value = ifelse(vals_in_tenures$value == 2, 'poor SMC, poor heli-ski',
                               ifelse(vals_in_tenures$value == 4, 'poor SMC, good heli-ski', 
                                      ifelse(vals_in_tenures$value == 6, 'good SMC, poor heli-ski', 'good SMC, good heli-ski')))

# loop over herds and get the freq values for each herd
#freq.df = data.frame('layer' = character(), 'value' = factor(), 'count' = numeric(),'sub_pop' = factor())
freq.df = data.frame()
for(h in unique(herd.bound$HERD_NAME)){
  print(h)
  #h = 'George_Mountain'
  h.spat = herd.bound[herd.bound$HERD_NAME == h,]
  crop.freq = crop(comb, h.spat)
  h.freq = freq(crop.freq)
  h.freq$herd = h
  freq.df = rbind(h.freq, freq.df)
  }
freq.df = freq.df[,c('herd', 'value','count')]

# summarise the data
total.count = freq.df %>% # get the total count for each herd
  group_by(herd) %>%
  summarise(total = sum(count)) 
# join the total back to the original dataframe
freq.dfa = inner_join(freq.df, total.count, by = 'herd')
# all values including low for both
freq.dfa = freq.dfa %>%
  group_by(herd, value) %>%
  summarise(percent_area = (count/total)*100)

freq.dfa$percent_area = round(freq.dfa$percent_area, 2)
freq.dfa$description = ifelse(freq.dfa$value == 2, 'low suitability skiing, low suitability SMC',
                              ifelse(freq.dfa$value == 4, 'high suitability skiing, low suitability SMC',
                                     ifelse(freq.dfa$value == 6, 'low suitability skiing, high suitability smc', 'high suitability for both')))

# just values that are non-low for both
# need new totals
nonlow.count = freq.df[freq.df$value > 2,] %>% # get the total count for each herd
  group_by(herd) %>%
  summarise(total = sum(count)) 
# join the total back to the original dataframe
freq.dfb = inner_join(freq.df[freq.df$value > 2,], nonlow.count, by = 'herd')
# all values including low for both
freq.dfb = freq.dfb %>%
  group_by(herd, value) %>%
  summarise(prop_count = (count/total)*100)

freq.dfb$value = as.factor(freq.dfb$value)
freq.dfb = na.omit(freq.dfb)
freq.dfb$desc = ifelse(freq.dfb$value == 4, 'good skiing / poor smc',
                       ifelse(freq.dfb$value == 6, 'poor skiing / good smc', 'good for both'))
freq.dfb$desc = factor(freq.dfb$desc, levels = c( 'good skiing / poor smc', 'poor skiing / good smc', 'good for both'))

# get some reporting metrics
prop.metrics = freq.dfb %>%
  group_by(desc) %>%
  summarise(mean.prop = mean(prop_count), 
            sd.prop = sd(prop_count),
            max.prop = max(prop_count),
            min.prop = min(prop_count)) %>%
  mutate(se.prop = sd.prop / sqrt(65),
         lower.ci.prop = mean.prop - qt(1 - (0.05 / 2), 65 - 1) * se.prop,
         upper.ci.prop = mean.prop + qt(1 - (0.05 / 2), 65 - 1) * se.prop)

prop.metrics = select(prop.metrics, c('desc', 'mean.prop', 'lower.ci.prop', 'upper.ci.prop'))

# area in total study area
df.comb = as.data.frame(comb)
df.comb = df.comb %>%
  group_by(sum) %>%
  summarise(n_values = n())
df.comb$area = (df.comb$n_values *1225) / 1000000
sum(df.comb$n_values)

desc.cols = c('good skiing / poor smc' = '#31688e', 'poor skiing / good smc' = '#35b779', 'good for both' = '#fde725')
ggplot(freq.dfb) +
  geom_bar(aes(x = value, y = prop_count, fill = desc), stat = 'identity') +
  scale_x_discrete(breaks = c(4,6,8), labels = c()) + # 4 = good hski, poor smc, 6 = poor hski, good smc, 8 = good both
  scale_fill_manual(values = desc.cols) + 
  theme_bw() +
  theme(legend.position = 'top', legend.title = element_blank()) +
  labs(x = 'habitat quadrant', y = 'percent of each quadrant by herd') +
  facet_wrap(~herd)
ggsave(file = paste0('./figures/proportion_by_quadrant_herd250.jpg'),
       width = 1500,
       height=1400,
       units = "px",
       dpi = 250)


# validation - binning rsf values of known locations
