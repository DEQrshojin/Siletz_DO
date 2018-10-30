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

# REVISIONS ----
# changed annotation and text font size from 1.75 to 2.25

# Import data from master data file----------
dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405"
dir.sub1 <- "\\001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"
dir.sub2 <- "\\005_reporting\\figures\\analysis_memo"
data.all <- read.csv(paste0(dir, dir.sub1, "siletz_volmon_cont_data.csv"))
data.all$DATE.TIME <- as.POSIXct(data.all$DATE.TIME, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
data.all <- data.all[complete.cases(data.all[, 5:8]), ] # Remove NA
data.all$STAID[data.all$STAID == 10391] <- 29287 # Replace 10391 with 29287
sites <- read.csv(paste0(dir, dir.sub1, "sites_order_z.csv")) # import site list
STAID <- sites$STATION
grph.lbl <- sites$FULL_NAME
grph.lbl <- gsub("_zz_", "\n", grph.lbl)

# Replace STAID as integers to Full Names and create factors for station list and reorder----------
data.all$Station <- gsub("_zz_", "\n", data.all$Station)
# Reformat data into a date/time by column table----------
doc.by.sta <- dcast(data.all, data.all$DATE.TIME ~ data.all$STAID, fun = mean, value.var = "DO_mgL")
#Reorder sites from alphabetical to D/S -> U/S
col.order <- c(1, 13, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
doc.by.sta <- doc.by.sta[col.order] 
colnames(doc.by.sta)[colnames(doc.by.sta) == "data.all$DATE.TIME"] <- "DATE"
doc.by.sta$DATE <- as.POSIXct(doc.by.sta$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
doc.by.sta <- doc.by.sta[order(doc.by.sta$DATE), ]

# Set criterion date bounds for rearing and spawning (truncated to monitoring periods)----------
dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Rearing end date
dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Rearing end date
lims.t <- c(dat.R.beg, dat.S.end)

#_________________________________________________________________________________________
# This sections outputs 13 panel graphs to be included in a figure of 5 x 3 graphs
# (two empty) of dissolved oxygen concentrations for each of the stations of the
# Lincoln County Soil and Water Conservation District Volunteer Monitoring Program
# on the Siletz collected during the summer and fall of 2017. There are two other
# panel figures to be included, DO % Sat and Temperature.
#_________________________________________________________________________________________

save.dir <- paste0(dir, dir.sub2)
grid.plots <- vector("list", 15)
mf <- 86400

# This plot is the bottom left hand graphs and includes the x and y axes ----
ind = c(1, 4, 7, 10, 12)
for (i in 1 : length(ind))
{
      col.sel <- c("DATE", STAID[ind[i]])
      tmp <- doc.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) +
                  xlab("Date") + ylab("DO (mg/L)") +
                  scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
                  scale_x_datetime(limits = lims.t, breaks=date_breaks("1 months"), labels=date_format("%m/%d")) +
                  theme_bw() + theme(panel.grid.minor=element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text.x = element_text(size = 8),
                                     axis.title.y = element_text(size = 8),
                                     axis.text.y = element_text(size = 8)) +
                  geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) +
                  geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
                  annotate("text", dat.S.beg, 12, color = "black", label = "Spawning\n11.0 mg/L", hjust = 0, size = 2.25) +
                  annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
                  annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Cold-water 30d, 8.0 mg/L", hjust = 0, size = 2.25) +
                  annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Cold-water 7d, 6.5 mg/L", hjust = 0, size = 2.25) +
                  annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Cold-water min, 6.0 mg/L", hjust = 0, size = 2.25) +
                  annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[ind[i]], hjust = 0, size = 2.25)
      grid.plots[[ind[i]]] <- doconc.plot
}
      
# These plots are for the remainder of the graphs and only include the x labels ----
ind = c(2, 3, 5, 6, 8, 9, 11, 13)
for (i in 1 : length(ind))
{
      col.sel <- c("DATE", STAID[ind[i]])
      tmp <- doc.by.sta[col.sel]
      names(tmp) <- c("a", "b")
      doconc.plot <- ggplot(tmp) + geom_point(aes(x = a, y = b), size = .25, shape = 1) + xlab("Date") +
          scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
          scale_x_datetime(limits = lims.t, breaks=date_breaks("1 months"), labels=date_format("%m/%d")) +
          theme_bw() + theme(panel.grid.minor=element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_text(size = 8),
                             axis.title.y = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank()) +
          geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.4, linetype = 2) +
          geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "green", size = 0.4, linetype = 2) +
          geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6), color = "orange", size = 0.4, linetype = 2) +
          geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "red", size = 0.4, linetype = 2) +
          annotate("text", dat.S.beg, 12, color = "black", label = "Spawning\n11.0 mg/L", hjust = 0, size = 2.25) +
          annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
          annotate("text", dat.R.end + mf * 1, 8.1, color = "black", label = "Cold-water 30d, 8.0 mg/L", hjust = 0, size = 2.25) +
          annotate("text", dat.R.end + mf * 1, 6.65, color = "black", label = "Cold-water 7d, 6.5 mg/L", hjust = 0, size = 2.25) +
          annotate("text", dat.R.end + mf * 1, 5.9, color = "black", label = "Cold-water min, 6.0 mg/L", hjust = 0, size = 2.25) +
          annotate("text", dat.R.beg, 13.5, color = "black", label = grph.lbl[ind[i]], hjust = 0, size = 2.25)
      grid.plots[[ind[i]]] <- doconc.plot
}

# Creates the grid and prints to .png ----
for (i in 14 : 15) # Create white panel grobs for empty spaces
{
    grid.plots[[i]] <- rectGrob(gp=gpar(fill="white", lty = 0))
}

x <- grid.arrange(grid.plots[[1]], grid.plots[[2]], grid.plots[[3]],
                  grid.plots[[4]], grid.plots[[5]], grid.plots[[6]],
                  grid.plots[[7]], grid.plots[[8]], grid.plots[[9]],
                  grid.plots[[10]], grid.plots[[11]], grid.plots[[14]],
                  grid.plots[[12]], grid.plots[[13]], grid.plots[[15]],
                  ncol = 3, nrow = 5, widths = c(2.7, 2.4, 2.4))

ggsave(filename = "fig06_lswcd_do_conc_all_v2.png", plot = x, path = save.dir, width = 7.5, height = 9, units = "in", dpi = 300)


