# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(reshape2)
require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA) ----

# Import data from master data file

data.dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"

dir.sub1 <- "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"

data.tmp <- read.csv(paste0(data.dir, dir.sub1, "siletz_volmon_cont_data.csv"))

data.tmp <- data.tmp[which(data.tmp$STAID != 38941), ] # Remove 38941 Strome Park

data.tmp$STAID[data.tmp$STAID == 10391] <- 29287 # Replace 10391 with 29287

data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME, format = "%m/%d/%Y %H:%M",
                                 tz = "America/Los_Angeles")

STAID_vctr <- unique(data.tmp$STAID) # create a list of stations

data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA

data.tmp$DATE <- as.Date(data.tmp$DATE.TIME, tz = "America/Los_Angeles") # Create dates columns

# CALCULATE DAILY, 7-DAY and 30-DAY STATISTICS ----

# Create column to assume a DO Sat of 100% if measured DO > 100%

data.tmp$DOS_pct <- ifelse(data.tmp$DO_Sat > 100, 100, data.tmp$DO_Sat)

# Create table of mean and min daily DO per site, and re-order from Downstream -> Upstream

dos.day.mn <- dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun = mean, value.var = "DOS_pct")

dos.day.mn[, 2:13] <- round(dos.day.mn[, 2:13], 1)

col.order <- c(1, 13, 4, 7, 3, 6, 9, 10, 2, 5, 8, 12, 11) # Initial ordering in alphabetical

dos.day.mn <- dos.day.mn[col.order] #Reorder sites from alphabetical to D/S -> U/S

# Change out Inf to NA in the Min mean DO table

for (z in 1:ncol(dos.day.mn)){dos.day.mn[,z][is.infinite(dos.day.mn[,z])] = NA}

# Change the DATES column name in both tables 

colnames(dos.day.mn)[colnames(dos.day.mn)=="data.tmp$DATE"] <- "DATE"

# Duplicate the dos.day.mn for 7-day mean min, 30-day mean min and 7 day min mean

dos.30d.mn <- dos.day.mn 

dos.7d.mn <- dos.day.mn

# Rear/spawn period dates

dat.R.beg <- min(dos.day.mn$DATE) # Cold-water start date

dat.R.end <- as.Date("2017-08-30", tz = "America/Los_Angeles") # Cold-water end date

dat.S.beg <- as.Date("2017-09-07", tz = "America/Los_Angeles") # Spawn start date

dat.S.end <- max(dos.day.mn$DATE) # Spawn end date

# Rear/spawn period index values

ind.R.end <- which(dos.day.mn$DATE == dat.R.end)

ind.S.beg <- ind.R.end + 1

ind.S.end <- which(dos.day.mn$DATE == dat.S.end)

# Create table of 7D mean-min and 7D min-mean of DO at 100 per cent sat 

for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Cold-water Period
  for (i in 1:ind.R.end) {
    if (dos.day.mn[i, 1] < dat.R.beg + 7) {
      dos.7d.mn[i, j + 1] <- NaN
    }
    else {
      dos.7d.mn[i, j + 1] <- NaN
    }
  }
  # Calculate rolling average of daily mean/min for Spawning Period
  for (i in ind.S.beg:ind.S.end) {
    if (dos.day.mn[i, 1] < dat.S.beg + 7) {
      dos.7d.mn[i, j + 1] <- mean(dos.day.mn[ind.S.beg:i, j + 1], na.rm = TRUE)
    }
    else {
      dos.7d.mn[i, j + 1] <- mean(dos.day.mn[(i - 6):i, j + 1], na.rm = TRUE)
    }
  }
}

# Create table of 30D mean-min of DO at 100 per cent sat 
for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Cold-water Period
  for (i in 1:ind.R.end) {
    if (dos.day.mn[i, 1] < dat.R.beg + 30) {
      dos.30d.mn[i, j + 1] <- mean(dos.day.mn[1:i, j + 1], na.rm = TRUE)
    }
    else {
      dos.30d.mn[i, j + 1] <- mean(dos.day.mn[(i - 29):i, j + 1], na.rm = TRUE)
    }
  }
  # Calculate rolling average of daily mean/min for Spawning Period
  for (i in ind.S.beg:ind.S.end) {
    if (dos.day.mn[i, 1] < dat.S.beg + 30) {
      dos.30d.mn[i, j + 1] <- NaN
    }
    else {
      dos.30d.mn[i, j + 1] <- NaN
    }
  }
}

#--------------------REFORMAT DATA AND CHANGE STATION NAMES---------------------
# Reshape the calculated daily DO tables into long format 

dos.7d.mn.gr <- melt(dos.7d.mn, id.vars = "DATE", variable.name = "Station")

dos.30d.mn.gr <- melt(dos.30d.mn, id.vars = "DATE", variable.name = "Station")

# Rename the values in the Station column

sites.old <- unique(dos.7d.mn.gr$Station)

data.site1 <- read.csv(paste0(data.dir, dir.sub1, "sites_order_z.csv"))

data.site1$FULL_NAME <- gsub("_zz_", " ", data.site1$FULL_NAME)

data.site1 = data.site1[-1, ]

sites.new1 <- data.site1$FULL_NAME

for (i in 1:length(sites.old)) {
  dos.7d.mn.gr$Station <- gsub(sites.old[i], sites.new1[i], dos.7d.mn.gr$Station)
  dos.30d.mn.gr$Station <- gsub(sites.old[i], sites.new1[i], dos.30d.mn.gr$Station)
}

sta.order <- c(12, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New column order

dos.7d.mn.gr$Station <- factor(dos.7d.mn.gr$Station,
                               levels(factor(dos.7d.mn.gr$Station))[sta.order])
dos.30d.mn.gr$Station <- factor(dos.30d.mn.gr$Station,
                                levels(factor(dos.30d.mn.gr$Station))[sta.order])

#--------------------------------PLOT DATA--------------------------------------

save.dir <- paste0(data.dir, "005_reporting\\presentations\\003_TWG_20190403\\figures_maps")

# Cold-water criterion 1, 30-day mean minimum concentrations

doPlotCW <- ggplot(data = dos.30d.mn.gr, aes(x = DATE, y = value)) + geom_line() +
    xlab("Date") + ylab("Dissolved Oxygen (% Saturation)") +
    scale_x_date(date_breaks = "14 days", date_labels = "%m/%d",
                 limits = c(dat.R.beg, dat.R.end)) +
    scale_y_continuous(limits = c(80, 110), breaks = c(80, 90, 100, 110)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
    geom_hline(yintercept = 90, color = "blue", size = .5, linetype = 2) +
    annotate("text", dat.R.end - 28, 88, color = "black", size = 3,
             label = "Cold-water Minimum, 90%", hjust = 0.25) +
    theme(plot.title = element_text(size = 10, hjust = 0.5)) + 
    facet_wrap(~Station, ncol = 4)

ggsave(filename = "fig18_lswcd_dos_30dmn_rear_v2.png", plot = doPlotCW, path = save.dir,
       width = 12, height = 9, units = "in", dpi = 300)

# Spawning criterion, 7-day mean minimum concentrations
doPlotSP <- ggplot(data = dos.7d.mn.gr, aes(x = DATE, y = value)) + geom_line() +
    xlab("Date") + ylab("Dissolved Oxygen (% Saturation)") +
    scale_x_date(date_breaks = "14 days", date_labels = "%m/%d",
                 limits = c(dat.S.beg, dat.S.end)) +
    scale_y_continuous(limits = c(80, 110), breaks = c(80, 90, 100, 110)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
    geom_hline(yintercept = 95, color = "blue", size = .5, linetype = 2) +
    annotate("text", dat.S.end -28, 93, color = "black",
             label = "Spawning Minimum, 95%", hjust = 0.2, size = 3) +
    theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    facet_wrap(~Station, ncol = 4)

ggsave(filename = "fig20_lswcd_dos_7dmn_spwn_v2.png", plot = doPlotSP, path = save.dir,
       width = 12, height = 9, units = "in", dpi = 300)

