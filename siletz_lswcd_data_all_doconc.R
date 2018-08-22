# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(reshape2)
require(dplyr)
library(tidyverse)
library(scales)
library(jpeg)
library(grDevices)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)

#----------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)----------
#-------------------------------------------------------------------------------
# Import data from master data file
dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405"
dir.sub1 <- "\\001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"
dir.sub2 <- "\\005_reporting\\figures"

data.all <- read.csv(paste0(dir, dir.sub1, "siletz_volmon_cont_data.csv"))
data.all$DATE.TIME <- as.POSIXct(data.all$DATE.TIME, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
data.all <- data.all[complete.cases(data.all[, 5:8]), ] # Remove NA
# Replace STAID as integers to Full Names and create factors for station list and reorder
data.all$Station <- gsub("_zz_", "\n", data.all$Station)
# Reformat data into a date/time by column table
doc.by.sta <- dcast(data.all, data.all$DATE.TIME ~ data.all$STAID, fun = mean, value.var = "DO_mgL")
#Reorder sites from alphabetical to D/S -> U/S
col.order <- c(1, 13, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
doc.by.sta <- doc.by.sta[col.order] 
colnames(doc.by.sta)[colnames(doc.by.sta) == "data.all$DATE.TIME"] <- "DATE"
doc.by.sta$DATE <- as.POSIXct(doc.by.sta$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
doc.by.sta <- doc.by.sta[order(doc.by.sta$DATE), ]

# Set criterion date bounds for rearing and spawning (truncated to monitoring periods)
dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Rearing end date

lims.t <- c(dat.R.beg, dat.S.end)

sites <- read.csv(paste0(dir, dir.sub1, "sites_order_z.csv"))
STAID <- sites$STATION
grph.lbl <- sites$FULL_NAME
grph.lbl <- gsub("_zz_", "\n", grph.lbl)

#--------------------------------PLOT DATA--------------------------------------
#-------------------------------------------------------------------------------
# This sections outputs 12 panel graphs to be included in a figure of 2 x 6 graphs
# of dissolved oxygen concentrations for each of the stations of the Lincoln County
# Soil and Water Conservation District Volunteer Monitoring Program on the Siletz
# collected during the summer and fall of 2017. There are two other panel figures
# to be included, DO % Sat and Temperature.

grid.plots <- vector("list", 12) 
save.dir <- paste0(dir, dir.sub2)
      mf <- 86400
      # This plot is the bottom left graph and includes the x and y axes
      i = 6
      col.sel <- c("data.all$DATE.TIME", STAID[i])
      tmp <- doc.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (mg/L)") +
                  scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
                  scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_text(size = 6), 
                                     axis.title.y = element_text(size = 6),
                                     axis.text.y = element_text(size = 6)) +
                  geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8),color = "blue", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 9.65, color = "black", label = "Spawning\n11.0 mg/L", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
                  annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Rearing 30d mean min, 8.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Rearing 7d min mean, 6.5 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Rearing abs min, 6.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
      grid.plots[[i]] <- doconc.plot
      # This plot is the bottom right graph and includes only the x axis
      i = 12
      col.sel <- c("data.all$DATE.TIME", STAID[i])
      tmp <- doc.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (mg/L)") +
                  scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
                  scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_text(size = 6),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank()) +
                  geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 9.65, color = "black", label = "Spawning\n11.0 mg/L", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
                  annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Rearing 30d mean min, 8.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Rearing 7d min mean, 6.5 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Rearing abs min, 6.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
      grid.plots[[i]] <- doconc.plot
      # These plots are for the left column graphs and include the y axes only
      for (i in 1 : 5) {
              col.sel <- c("data.all$DATE.TIME", STAID[i])
              tmp <- doc.by.sta[col.sel]
              names(tmp) <- c("a", "b")
              doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (mg/L)") +
                  scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
                  scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.title.y = element_text(size = 6),
                                     axis.text.y = element_text(size = 6)) +
                  geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 9.65, color = "black", label = "Spawning\n11.0 mg/L", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
                  annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Rearing 30d mean min, 8.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Rearing 7d min mean, 6.5 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Rearing abs min, 6.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
              grid.plots[[i]] <- doconc.plot
              }
      # These plots are for the right-hand column graphs and do not include either axis
      for (i in 7 : 11) {
              col.sel <- c("data.all$DATE.TIME", STAID[i])
              tmp <- doc.by.sta[col.sel]
              names(tmp) <- c("a", "b")
              doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (mg/L)") +
                  scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
                  scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_blank()) +
                  geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 9.65, color = "black", label = "Spawning\n11.0 mg/L", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
                  annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Rearing 30d mean min, 8.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Rearing 7d min mean, 6.5 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Rearing abs min, 6.0 mg/L", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
              grid.plots[[i]] <- doconc.plot
      }

            
x <- grid.arrange(grid.plots[[1]], grid.plots[[7]],
                  grid.plots[[2]], grid.plots[[8]],
                  grid.plots[[3]], grid.plots[[9]],
                  grid.plots[[4]], grid.plots[[10]],
                  grid.plots[[5]], grid.plots[[11]],
                  grid.plots[[6]], grid.plots[[12]],
                  ncol = 2, 
                  widths = c(4.17, 3.83),
                  heights = c(rep(1.33, 5), 1.46))
ggsave(filename = "fig05_lswcd_do_conc_all.jpg", plot = x, path = save.dir, width = 8, height = 8.123, units = "in", dpi = 300)

# PLOT FLOW DATA - INCLUDE THIS SECTION IF YOU WANT TO ADD TWO GRAPHS AT THE BOTTOM TO SHOW FLOW DURING THE MONITORING PERIOD
# --------------------------------------------------------------------------------------------------------------------------- 
# data.q <- read.csv("C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/flow/flow_test.csv")
# data.q$datetime <- as.POSIXct(data.q$datetime, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
# q.plot1 <-  ggplot(data.q) + geom_point(aes(x = datetime, y = instantaneous_flow_cfs), size = .25, shape = 1) +
#     ylab("Flow (cfs)") + scale_y_log10(limits = c(10, 30000), breaks = c(10, 100, 1000, 10000), labels = scientific) +
#     scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
#     theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                        axis.title.x = element_blank(), axis.text.x = element_text(size = 6),
#                        axis.title.y = element_text(size = 1), axis.text.y = element_text(size = 6))
# q.plot2 <-  ggplot(data.q) + geom_point(aes(x = datetime, y = instantaneous_flow_cfs), size = .25, shape = 1) +
#     ylab("Flow (cfs)") + scale_y_log10(limits = c(10, 30000), breaks = c(10, 100, 1000, 10000), labels = scientific) +
#     scale_x_datetime(limits = lims.t, breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
#     theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                        axis.title.x = element_blank(), axis.text.x = element_text(size = 6),
#                        axis.title.y = element_blank(), axis.text.y = element_blank(),
#                        axis.ticks.y = element_blank())