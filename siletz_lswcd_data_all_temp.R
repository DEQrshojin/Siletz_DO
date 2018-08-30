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
data.all <- data.all[complete.cases(data.all[, 5]), ] # Remove NA
data.all$STAID[data.all$STAID == 10391] <- 29287 # Replace 10391 with 29287
# Replace STAID as integers to Full Names and create factors for station list and reorder
data.all$Station <- gsub("_zz_", "\n", data.all$Station)
# Reformat data into a date/time by column table
tem.by.sta <- dcast(data.all, data.all$DATE.TIME ~ data.all$STAID, fun = mean, value.var = "TEMP_C")
#Reorder sites from alphabetical to D/S -> U/S
col.order <- c(1, 13, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
tem.by.sta <- tem.by.sta[col.order] 
colnames(tem.by.sta)[colnames(tem.by.sta) == "data.all$DATE.TIME"] <- "DATE"
tem.by.sta$DATE <- as.POSIXct(tem.by.sta$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
tem.by.sta <- tem.by.sta[order(tem.by.sta$DATE), ]

# Set criterion date bounds for Cold-water and spawning (truncated to monitoring periods)
dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Cold-water end date
dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Cold-water end date
dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Cold-water end date

lims.t <- c(dat.R.beg, dat.S.end)

sites <- read.csv(paste0(dir, dir.sub1, "sites_order_z.csv"))
STAID <- sites$STATION
grph.lbl <- sites$FULL_NAME
grph.lbl <- gsub("_zz_", "\n", grph.lbl)

#--------------------------------PLOT DATA--------------------------------------
# This sections outputs 13 panel graphs to be included in a figure of 5 x 3 graphs
# (two empty) of dissolved oxygen concentrations for each of the stations of the
# Lincoln County Soil and Water Conservation District Volunteer Monitoring Program
# on the Siletz collected during the summer and fall of 2017. There are two other
# panel figures to be included, DO % Sat and Temperature.

save.dir <- paste0(dir, dir.sub2)
grid.plots <- vector("list", 15)
mf <- 86400
# This plot is the bottom left hand graphs and includes the x and y axes
ind = c(1, 4, 7, 10, 12)
for (i in 1 : length(ind))
{
    col.sel <- c("DATE", STAID[ind[i]])
    tmp <- tem.by.sta[col.sel]
    names(tmp) <- c("a", "b")
    grid.plots[[ind[i]]] <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) + ylab("Temperature (°C)") +
          scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
          scale_x_datetime(limits = lims.t, breaks=date_breaks("1 months"), labels=date_format("%m/%d")) +
          theme_bw() + theme(panel.grid.minor=element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_text(size = 6), 
                             axis.title.y = element_text(size = 6),
                             axis.text.y = element_text(size = 6)) +
          geom_segment(aes(x = dat.R.beg, y = 16, xend = dat.R.end, yend = 16), color = "blue", size = 0.4, linetype = 2) + 
          geom_segment(aes(x = dat.S.beg, y = 13, xend = dat.S.end, yend = 13), color = "red", size = 0.4, linetype = 2) +
          annotate("text", dat.S.end, 11.5, color = "black", label = "Spawning, 13°C", hjust = 1, size = 1.75) +
          annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 86, ymax = 89, fill = "white", alpha = 1) +
          annotate("text", dat.R.beg + mf * 1, 14.5, color = "black", label = "Cold-water, 16°C", hjust = 0, size = 1.75) +
          annotate("text", dat.R.beg, 3, color = "black", label = grph.lbl[ind[i]], hjust = 0, size = 2)
}

# These plots are for the remainder of the graphs and only include the x labels
ind = c(2, 3, 5, 6, 8, 9, 11, 13)
for (i in 1 : length(ind))
{
    col.sel <- c("DATE", STAID[ind[i]])
    tmp <- tem.by.sta[col.sel]
    names(tmp) <- c("a", "b")
    grid.plots[[ind[i]]] <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
          scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
          scale_x_datetime(limits = lims.t, breaks=date_breaks("1 months"), labels=date_format("%m/%d")) +
          theme_bw() + theme(panel.grid.minor=element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_text(size = 6),
                             axis.title.y = element_blank(), 
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank()) +
          geom_segment(aes(x = dat.R.beg, y = 16, xend = dat.R.end, yend = 16), color = "blue", size = 0.4, linetype = 2) + 
          geom_segment(aes(x = dat.S.beg, y = 13, xend = dat.S.end, yend = 13), color = "red", size = 0.4, linetype = 2) +
          annotate("text", dat.S.end, 11.5, color = "black", label = "Spawning, 13°C", hjust = 1, size = 1.75) +
          annotate("rect", xmin = dat.R.beg + mf * 0.5, xmax = dat.R.beg + mf * 26, ymin = 86, ymax = 89, fill = "white", alpha = 1) +
          annotate("text", dat.R.beg + mf * 1, 14.5, color = "black", label = "Cold-water, 16°C", hjust = 0, size = 1.75) +
          annotate("text", dat.R.beg, 3, color = "black", label = grph.lbl[ind[i]], hjust = 0, size = 2)
}

for (i in 14 : 15)
{
    grid.plots[[i]] <- rectGrob(gp=gpar(fill="white", lty = 0))
}

x <- grid.arrange(grid.plots[[1]], grid.plots[[2]], grid.plots[[3]],
                  grid.plots[[4]], grid.plots[[5]], grid.plots[[6]],
                  grid.plots[[7]], grid.plots[[8]], grid.plots[[9]],
                  grid.plots[[10]], grid.plots[[11]], grid.plots[[14]],
                  grid.plots[[12]], grid.plots[[13]], grid.plots[[15]],
                  ncol = 3, nrow = 5, widths = c(2.7, 2.4, 2.4))

ggsave(filename = "fig08_lswcd_temp_all.png", plot = x, path = save.dir, width = 7.5, height = 9, units = "in", dpi = 300)
      