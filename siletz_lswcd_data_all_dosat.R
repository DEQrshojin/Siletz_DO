# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(reshape2)
require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)
library(jpeg)
library(BiocInstaller)
library(grDevices)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)


#-------------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)-------------
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
dos.by.sta <- dcast(data.all, data.all$DATE.TIME ~ data.all$STAID, fun = mean, value.var = "DO_Sat")
#Reorder sites from alphabetical to D/S -> U/S
col.order <- c(1, 13, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
dos.by.sta <- dos.by.sta[col.order]

# Set criterion date bounds for rearing and spawning (truncated to monitoring periods)
dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Rearing end date

sites <- read.csv(paste0(dir, dir.sub1, "sites_order_z.csv"))
STAID <- sites$STATION
grph.lbl <- sites$FULL_NAME
grph.lbl <- gsub("_zz_", "\n", grph.lbl)

#--------------------------------PLOT DATA--------------------------------------
#-------------------------------------------------------------------------------
grid.plots <- vector("list", 12) 
save.dir <- paste0(dir, dir.sub2)
mf <- 86400
      i = 6
      col.sel <- c("data.all$DATE.TIME", STAID[i])
      tmp <- dos.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (% Sat)") +
                  scale_y_continuous(limits = c(60, 150), breaks = c(60, 90, 120, 150)) +
                  scale_x_datetime(limits = c(dat.R.beg, dat.S.end), breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_text(size = 6), 
                                     axis.title.y = element_text(size = 6),
                                     axis.text.y = element_text(size = 6)) +
                  geom_segment(aes(x = dat.R.beg, y = 90, xend = dat.R.end, yend = 90), color = "blue", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.S.beg, y = 95, xend = dat.S.end, yend = 95), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 87, color = "black", label = "Spawning,\n95 percent", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 83, ymax = 86, fill = "white", alpha = 1) +
                  annotate("text", dat.R.beg + mf * 1, 85, color = "black", label = "Rearing, 90 percent", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 135, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
      grid.plots[[i]] <- doconc.plot
      i = 12
      col.sel <- c("data.all$DATE.TIME", STAID[i])
      tmp <- dos.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (% Sat)") +
                  scale_y_continuous(limits = c(60, 150), breaks = c(60, 90, 120, 150)) +
                  scale_x_datetime(limits = c(dat.R.beg, dat.S.end), breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_text(size = 6),
                                     axis.title.y = element_blank(), 
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank()) +
                  geom_segment(aes(x = dat.R.beg, y = 90, xend = dat.R.end, yend = 90), color = "blue", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.S.beg, y = 95, xend = dat.S.end, yend = 95), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 87, color = "black", label = "Spawning,\n95 percent", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 83, ymax = 86, fill = "white", alpha = 1) +
                  annotate("text", dat.R.beg + mf * 1, 85, color = "black", label = "Rearing, 90 percent", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 135, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
      grid.plots[[i]] <- doconc.plot
      for (i in 1 : 5) {
              col.sel <- c("data.all$DATE.TIME", STAID[i])
              tmp <- dos.by.sta[col.sel]
              names(tmp) <- c("a", "b")
              doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (% Sat)") +
                  scale_y_continuous(limits = c(60, 150), breaks = c(60, 90, 120, 150)) +
                  scale_x_datetime(limits = c(dat.R.beg, dat.S.end), breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.title.y = element_text(size = 6),
                                     axis.text.y = element_text(size = 6)) +
                  geom_segment(aes(x = dat.R.beg, y = 90, xend = dat.R.end, yend = 90), color = "blue", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.S.beg, y = 95, xend = dat.S.end, yend = 95), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 87, color = "black", label = "Spawning,\n95 percent", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 83, ymax = 86, fill = "white", alpha = 1) +
                  annotate("text", dat.R.beg + mf * 1, 85, color = "black", label = "Rearing, 90 percent", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 135, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
              grid.plots[[i]] <- doconc.plot
      }
      for (i in 7 : 11) {
              col.sel <- c("data.all$DATE.TIME", STAID[i])
              tmp <- dos.by.sta[col.sel]
              names(tmp) <- c("a", "b")
              doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("Dissolved\nOxygen (% Sat)") +
                  scale_y_continuous(limits = c(60, 150), breaks = c(60, 90, 120, 150)) +
                  scale_x_datetime(limits = c(dat.R.beg, dat.S.end), breaks=date_breaks("14 days"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_blank()) +
                  geom_segment(aes(x = dat.R.beg, y = 90, xend = dat.R.end, yend = 90), color = "blue", size = 0.4, linetype = 2) + 
                  geom_segment(aes(x = dat.S.beg, y = 95, xend = dat.S.end, yend = 95), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.end, 87, color = "black", label = "Spawning,\n95 percent", hjust = 1, size = 1.75) +
                  annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 83, ymax = 86, fill = "white", alpha = 1) +
                  annotate("text", dat.R.beg + mf * 1, 85, color = "black", label = "Rearing, 90 percent", hjust = 0, size = 1.75) +
                  annotate("text", dat.R.beg, 135, color = "black", label = grph.lbl[i], hjust = 0, size = 2)
              grid.plots[[i]] <- doconc.plot
      }
      x <- grid.arrange(grid.plots[[1]], grid.plots[[7]],
                        grid.plots[[2]], grid.plots[[8]],
                        grid.plots[[3]], grid.plots[[9]],
                        grid.plots[[4]], grid.plots[[10]],
                        grid.plots[[5]], grid.plots[[11]],
                        grid.plots[[6]], grid.plots[[12]], 
                        ncol = 2, widths = c(4.17, 3.83),
                        heights = c(rep(1.33, 5), 1.46))
      ggsave(filename = "fig06_lswcd_do_sat_all.jpg", plot = x, path = save.dir, width = 8, height = 8, units = "in", dpi = 300)