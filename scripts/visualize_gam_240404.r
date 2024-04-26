#-------------------------------------------------------------------------------
# PLOT - first section is plotting the interactions, second is plotting the 

library(mgcViz)
library(gratia)
library(ggplot2)
library(dplyr)
library(viridis)
library(gridExtra)

rm(list = ls())
gc()

load("./models/mAll_LATE_WINTER_231113.rds")
summary(mAll)
smooths(mAll)


#-------------------------------------------------------------------------------
# uses gratia for the other marginal effects

# set herd colors
herd.colors <- c(CN = "#d13ab8", CS = "#fce96c", HR ="#43ddb6")

## create the plotting datasets
# elevation
elevation = smooth_estimates(mAll, smooth = "s(elev_220809,herd)" )
elevation = add_confint(elevation)
elevation$smooth = 'elevation'
elevation = rename(elevation,
                   'elevation_m' = 'elev_220809')
elevation$herd = ifelse(elevation$herd == 1, 'CN', 
                        ifelse(elevation$herd == 2, 'CS','HR'))
#elevation$span = elevation$upper_ci - elevation$lower_ci # extemely small CIs

# slope
slope = smooth_estimates(mAll, smooth = "s(slope_220809,herd)" )
slope = add_confint(slope)
slope$smooth = 'slope'
slope = rename(slope,
               'slope_deg' = 'slope_220809')
slope$herd = ifelse(slope$herd == 1, 'CN', 
                    ifelse(slope$herd == 2, 'CS','HR'))

# crown closure
crown_closure = smooth_estimates(mAll, smooth = "s(crown_clos)" )
crown_closure = add_confint(crown_closure)
crown_closure$smooth = 'crown_closure'
crown_closure = rename(crown_closure,
                       'cc' = 'crown_clos')

# age
age = smooth_estimates(mAll, smooth = "s(proj_age_220809,herd)" )
age = add_confint(age)
age$smooth = 'forest age'
age = rename(age,
             'forest_age' = 'proj_age_220809')
age$herd = ifelse(age$herd == 1, 'CN', 
                    ifelse(age$herd == 2, 'CS','HR'))

# plot elevation
a = ggplot(elevation, aes(x = elevation_m)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, group = herd),
              fill = "deepskyblue4") + #
  geom_line(data = elevation, aes(x = elevation_m, y = est, color = herd), linewidth = 1.5) +
  scale_color_manual(values = herd.colors) + # c("#333BFF", "#CC6600", "#9633FF")) +
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

# plot slope
b = ggplot(slope, aes(x = slope_deg)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, group = herd),
              fill = "deepskyblue4") + #
  geom_line(data = slope, aes(x = slope_deg, y = est, color = herd), linewidth = 1.5) +
  scale_color_manual(values = herd.colors) + # c("#333BFF", "#CC6600", "#9633FF")) +
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


# plot crown closure
c = ggplot(crown_closure, aes(x = cc)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = cc, y = est), linewidth = 1) +
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
  labs(x = "Crown closure (percent)", y = "Partial effect", tag = 'c)')



# plot age
d = ggplot(age, aes(x = forest_age)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2, group = age$herd) +
  scale_color_manual(values = herd.colors) +
  geom_line(aes(x = forest_age, y = est, colour = herd), linewidth = 1) +
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
  labs(x = "Forest age (years)", y = "Partial effect", tag = 'd)') 



#-------------------------------------------------------------------------------
# use mgcviz for the interactions
mb <- mgcViz::getViz(mAll)
#plot(mb, allTerms = TRUE)
#a = plot(mb, allTerms = FALSE, select= c(3)) # elevation
#b = plot(mb, allTerms = FALSE, select= c(4)) # slope
#c = plot(mb, allTerms = FALSE, select= c(5)) # crown closure
#d = plot(mb, allTerms = FALSE, select= c(6)) # age

e = plot(mb, allTerms = FALSE, select= c(9)) # age:unburned
f = plot(mb, allTerms = FALSE, select= c(10)) # age:burned
g = plot(mb, allTerms = FALSE, select= c(11)) # age:unlogged
h = plot(mb, allTerms = FALSE, select= c(12)) # age:logged
i = plot(mb, allTerms = FALSE, select= c(7)) # crown closure : age
j = plot(mb, allTerms = FALSE, select= c(8)) # slope : elevation


f.size = 10
a.size = 7

# interactions
# age:unburned
e.plot <- e$plots [[1]]$ggObj

e.data = e.plot$data
e.data = rename(e.data, 'est' = 'ty')
e.data = add_confint(e.data)
egrob = ggplot(e.data, aes(x = x)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = x, y = est), linewidth = 1) +
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
  labs(x="Age unburned forest", y="Partial effect", tag = 'e)')


# age:burned
f.plot <- f$plots [[1]]$ggObj
f.data = f.plot$data
f.data = rename(f.data, 'est' = 'ty')
f.data = add_confint(f.data)

fgrob = ggplot(f.data, aes(x = x)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = x, y = est), linewidth = 1) +
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
  labs(x="Forest age at burn", y="Partial effect", tag = 'f)')



# age:unlogged
g.plot <- g$plots [[1]]$ggObj
g.data = g.plot$data
g.data = rename(g.data, 'est' = 'ty')
g.data = add_confint(g.data)

ggrob = ggplot(g.data, aes(x = x)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = x, y = est), linewidth = 1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, face = 'bold', family = "sans"),
        axis.title.x = element_text(size=10, face = 'bold', family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.text  = element_text(size=8, family = "sans"),
        legend.title  = element_text(size=8, family = "sans"),
        legend.key.size = unit(0.35, 'cm'),
        legend.key.height = unit(0.35, 'cm'),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans"),
        legend.position = "none") + 
  labs(x="Age unlogged forest", y="Partial effect", tag = 'g)')


# age:logged
h.plot <- h$plots [[1]]$ggObj
h.data = h.plot$data
h.data = rename(h.data, 'est' = 'ty')
h.data = add_confint(h.data)

hgrob = ggplot(h.data, aes(x = x)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "deepskyblue4", alpha = 0.2) +
  geom_line(aes(x = x, y = est), linewidth = 1) +
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
  labs(x="Age at harvest", y="Partial effect", tag = 'h)')


#----
# age crown closure
i.plot <- i$plots [[1]]$ggObj

igrob = i.plot +
  geom_tile(aes(fill = (z))) + 
  geom_contour(aes(z = z, fill = NULL), colour = "black") +
  scale_fill_viridis() +
  labs(x="Forest age (years)", y="Crown closure (%)", tag = 'i)') +
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
        plot.title = element_blank(), #element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans")) + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))



#elevation slope
j.plot <- j$plots [[1]]$ggObj

jgrob = j.plot +
  geom_tile(aes(fill = (z))) + 
  geom_contour(aes(z = z, fill = NULL), colour = "black") +
  scale_fill_viridis() +
  labs(x="Elevation (m)", y = "Slope (degrees)", tag = "j)") +
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
        plot.title = element_blank(), #element_text(hjust = -0.05, size = 10, family = "sans"),
        plot.tag = element_text(size=18, family = "sans")) + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=a.size),
        axis.title=element_text(size=f.size))

#-------------------------------------------------------------------------------
# arrange plots
#gridPlot = grid.arrange(gridPrint(grobs = list(agrob, bgrob, cgrob,dgrob,egrob,fgrob,ggrob,hgrob), ggplot(), ncol = 4), 
#             gridPrint(grobs = list(igrob,jgrob), ggplot(), nrow = 1), heights = c(5,4))

gridtop = grid.arrange(a,b,c,d, ncol = 4)
gridmid = grid.arrange(egrob,fgrob,ggrob,hgrob, ncol = 4)
gridbot = grid.arrange(igrob,jgrob, ncol = 3)
gridplot = grid.arrange(gridtop, gridmid, gridbot, nrow = 3)
ggsave(gridplot, file="./figures/RATA_plot_gam_2240424.png",
       width = 1500,
       height=1000,
       units = "px",
       dpi = 130)






