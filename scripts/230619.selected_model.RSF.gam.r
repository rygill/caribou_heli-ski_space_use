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


library(mgcViz)
library(viridis)
library(gridExtra)



load("./models/mAll_LATE_WINTER_231113.rds")
mb <- getViz(mAll)

#plot(mb, allTerms = FALSE, xlab = 'HORSE', ylab = 'MULE')
#plot(mb, allTerms = FALSE, select= c(3)) + l_fitLine() + l_ciLine(level = 0.95)

a = plot(mb, allTerms = FALSE, select= c(3))
b = plot(mb, allTerms = FALSE, select= c(4))
c = plot(mb, allTerms = FALSE, select= c(5))
d = plot(mb, allTerms = FALSE, select= c(6))
e = plot(mb, allTerms = FALSE, select= c(9))
f = plot(mb, allTerms = FALSE, select= c(10))
g = plot(mb, allTerms = FALSE, select= c(11))
h = plot(mb, allTerms = FALSE, select= c(12))
i = plot(mb, allTerms = FALSE, select= c(7))
j = plot(mb, allTerms = FALSE, select= c(8))


f.size = 10
a.size = 7
# crown closure

a.plot <- a$plots[[1]]$ggObj 
# Changing ggplot attributes
agrob = a.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="elevation (m)", y="s(elevation)") +  #labs(x="Crown closure", y="s(Crown closure)", color = "My ID labels:")+
  labs(title = "a)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))
agrob

b.plot <- b$plots [[1]]$ggObj
#a$data$id <- as.factor(p$data$id)
# Changing ggplot attributes
bgrob = b.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="slope (deg)", y="s(slope)") +  #labs(x="Crown closure", y="s(Crown closure)", color = "My ID labels:")+
  labs(title = "b)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))
bgrob

c.plot <- c$plots [[1]]$ggObj
#a$data$id <- as.factor(p$data$id)
# Changing ggplot attributes
cgrob = c.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="crown closure", y="s(crown closure)") +  #labs(x="Crown closure", y="s(Crown closure)", color = "My ID labels:")+
  labs(title = "c)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))
cgrob

# age
d.plot <- d$plots [[1]]$ggObj
dgrob = d.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="forest age", y="s(forest age)")+
  labs(title = "d)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

dgrob

# age:unburned
e.plot <- e$plots [[1]]$ggObj
egrob = e.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="age unburned forest", y="s(age unburned)")+
  labs(title = "e)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

egrob

# age:burned
f.plot <- f$plots [[1]]$ggObj
fgrob = f.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="forest age at burn", y="s(age burned)")+
  labs(title = "f)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

fgrob

# age:unlogged
g.plot <- g$plots [[1]]$ggObj
ggrob = g.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="age unlogged forest", y="s(age unlogged)")+
  labs(title = "g)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

ggrob

# age:logged
h.plot <- h$plots [[1]]$ggObj
hgrob = h.plot +
  geom_line(linetype = 1, linewidth = 1)+ 
  scale_color_brewer(palette = "Set1")+
  labs(x="age at harvest", y="s(age:logged)")+
  labs(title = "h)") +
  theme_classic(14) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

hgrob

#----
# age crown closure
i.plot <- i$plots [[1]]$ggObj

igrob = i.plot +
  geom_tile(aes(fill = (z))) + 
  geom_contour(aes(z = z, fill = NULL), colour = "black") +
  scale_fill_viridis() +
  labs(x="forest age", y="crown closure")+
  labs(title = "i)") +
  theme_classic(14) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

igrob

#elevation slope
j.plot <- j$plots [[1]]$ggObj

jgrob = j.plot +
  geom_tile(aes(fill = (z))) + 
  geom_contour(aes(z = z, fill = NULL), colour = "black") +
  scale_fill_viridis() +
  labs(x="elevation", y = "slope") +
  labs(title = "j)") +
  theme_classic(14) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

jgrob
#----


# arrange plots
gridPrint(grobs = list(agrob,bgrob,cgrob,dgrob,egrob,fgrob,ggrob,hgrob), ggplot(), ncol = 4)
gridPrint(grobs = list(igrob,jgrob), ggplot(), ncol = 2)

gridPlot = grid.arrange(gridPrint(grobs = list(agrob, bgrob, cgrob,dgrob,egrob,fgrob,ggrob,hgrob), ggplot(), ncol = 4), 
             gridPrint(grobs = list(igrob,jgrob), ggplot(), nrow = 1), heights = c(5,4))

ggsave(gridPlot, file="./figures/RATA_plot_gam.png",
       width = 2000,
       height=2000,
       units = "px",
       dpi = 300)

#igridPrint(plot(sm(b, 2)) + l_fitRaster() + l_fitContour() + labs(title = NULL) + guides(fill=FALSE),
#          plot(pterm(b, 2)) + l_ciPoly() + l_fitLine(), ncol = 2)


library(gratia)
library(dplyr)
rm(list = ls())

load("./models/mAll_LATE_WINTER_231113.rds")

smooths(mAll)

# elevation
elevation = smooth_estimates(mAll, smooth = "s(elev_220809,herd)" )
elevation = add_confint(elevation)
elevation$smooth = 'elevation'
elevation = rename(elevation,
                   'elevation' = 'elev_220809')
elevation$herd = ifelse(elevation$herd == 1, 'CN', 
                        ifelse(elevation$herd == 2, 'CS','HR'))


ggplot(elevation, aes(x = elevation)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = elevation, aes(x = elevation, y = est, colour = herd), linewidth = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        legend.position = "none") + 
  ylab("Partial effect") +
  xlab("Elevation (m)") 

# slope
slope = smooth_estimates(mAll, smooth = "s(slope_220809,herd)" )
slope = add_confint(slope)
slope$smooth = 'slope'
slope = rename(slope,
                   'slope_deg' = 'slope_220809')
slope$herd = ifelse(slope$herd == 1, 'CN', 
                        ifelse(slope$herd == 2, 'CS','HR'))

ggplot(slope, aes(x = slope_deg)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(data = slope, aes(x = slope_deg, y = est, colour = herd), linewidth = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        legend.position = "none") + 
  ylab("Partial effect") +
  xlab("Slope (degrees)") 


# crown closure
crown_closure = smooth_estimates(mAll, smooth = "s(crown_clos)" )
crown_closure = add_confint(crown_closure)
crown_closure$smooth = 'crown_closure'
crown_closure = rename(crown_closure,
               'cc' = 'crown_clos')

ccplot = ggplot(crown_closure, aes(x = cc)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = cc, y = est), linewidth = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        legend.position = "none") + 
  ylab("Partial effect") +
  xlab("Crown closure (percent)") 


# age
age = smooth_estimates(mAll, smooth = "s(proj_age_220809,herd)" )
age = add_confint(age)
age$smooth = 'forest age'
age = rename(age,
             'forest_age' = 'proj_age_220809')

ggplot(age, aes(x = forest_age)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  #geom_line(aes(x = forest_age, y = est), linewidth = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        legend.position = "none") + 
  ylab("Partial effect") +
  xlab("Crown closure (percent)") 


age_cc = smooth_estimates(mAll, smooth = "ti(proj_age_220809,crown_clos)") 
age_cc = add_confint(age_cc)
age_cc$smooth = 'forest age : crown closure'
age_cc = rename(age_cc,
             'forest_age' = 'proj_age_220809',
             'crown_closure' = 'crown_clos')



elev_slope =  smooth_estimates(mAll, smooth = "ti(elev_220809,slope_220809)")
elev_slope = add_confint(elev_slope)
elev_slope$smooth = 'elevation : slope'
elev_slope = rename(elev_slope,
                'elevation' = 'elev_220809',
                'slope' = 'slope_220809')


draw(mAll, n_contour = 5, n = 50,
     continuous_fill = scale_fill_distiller(palette = "Spectral",
                                            type = "div"))



age_burned0 =  smooth_estimates(mAll, smooth = "ti(proj_age_220809):burned0")


age_burned1 =  smooth_estimates(mAll, smooth = "ti(proj_age_220809):burned1")   


age_logged0 =  smooth_estimates(mAll, smooth = "ti(proj_age_220809):logged0")   


age_logged1 =  smooth_estimates(mAll, smooth = "ti(proj_age_220809):logged1")





elevation |>
  add_confint() |>
  

slope = smooth_estimates(mb, smooth = "s(slope_220809,herd)" )





cc = smooth_estimates(mb, smooth = "s(crown_clos)" )
cc = add_confint(cc)
cc$smooth = 'crown closure'


cc |>
  add_confint() |>
  ggplot() + 
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
  #            alpha = 0.2, fill = "forestgreen") +
  geom_line(data = cc, aes(x = , y = est, colour = herd), linewidth = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        legend.position = "none") + 
  ylab("Partial effect") +
  xlab("Elevation (m)") 





age = smooth_estimates(mb, smooth = "s(proj_age_220809,herd)" )
age_cc = smooth_estimates(mb, smooth = "ti(proj_age_220809,crown_clos)") 
elev_slope = smooth_estimates(mb, smooth = "ti(elev_220809,slope_220809)")
age_burned0 = smooth_estimates(mb, smooth = "ti(proj_age_220809):burned0")
age_burned1 = smooth_estimates(mb, smooth = "ti(proj_age_220809):burned1")
age_logged0 = smooth_estimates(mb, smooth = "ti(proj_age_220809):logged0")
age_logged1 = smooth_estimates(mb, smooth = "ti(proj_age_220809):logged1")




elevation |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = elevation_220809)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "forestgreen") +
  geom_line(colour = "forestgreen", linewidth = 1.5) +
  labs(y = "Partial effect",
       title = expression("Partial effect of" ~ f(x[2])),
       x = expression(x[2]))

