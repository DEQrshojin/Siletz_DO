# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

# LOAD PACKAGES----
require(reshape2)
require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)

#----------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)----------
# Import data from master data file
data.dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"
dir.sub1 <- "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"
data.tmp <- read.csv(paste0(data.dir, dir.sub1, "siletz_volmon_cont_data.csv"))
data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
STAID_vctr <- unique(data.tmp$STAID) # create a list of stations
data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA
data.tmp$DATE <- as.Date(data.tmp$DATE.TIME, tz = "America/Los_Angeles") # Create dates columns
# Create DO at saturation concentrations, i.e., at 100% if greater than 100% as per OAR 340-041-0002
data.tmp$DOS_mgL <- ifelse(data.tmp$DO_Sat > 100, data.tmp$DO_mgL/data.tmp$DO_Sat * 100, data.tmp$DO_mgL)

#----------CALCULATE DAILY, 7-DAY and 30-DAY STATISTICS----------
#-------------------------------------------------------------------------------
# Create table of mean and min daily DO per site, and re-order from Downstream -> Upstream
do.day.mn <- dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun = mean, value.var = "DOS_mgL")
do.day.mi <- dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun = min, value.var = "DOS_mgL")
col.order <- c(1, 13, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
do.day.mn <- do.day.mn[col.order] #Reorder sites from alphabetical to D/S -> U/S
do.day.mi <- do.day.mi[col.order] 
# Change out Inf to NA in the Min mean DO table
for (z in 1:ncol(do.day.mi)){do.day.mi[,z][is.infinite(do.day.mi[,z])] = NA}
# Change the DATES column name in both tables 
colnames(do.day.mn)[colnames(do.day.mn)=="data.tmp$DATE"] <- "DATE"
colnames(do.day.mi)[colnames(do.day.mi)=="data.tmp$DATE"] <- "DATE"
# Duplicate the do.day.mn for 7-day mean min, 30-day mean min and 7 day min mean
do.7d.mn <- do.day.mn
do.7d.mi <- do.day.mn
do.30d.mn <- do.day.mn
# Rear/spawn period dates
dat.R.beg <- min(do.day.mn$DATE) # Cold-water start date
dat.R.end <- as.Date("2017-08-30", tz = "America/Los_Angeles") # Cold-water end date
dat.S.beg <- as.Date("2017-09-07", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- max(do.day.mn$DATE) # Spawn end date
# Rear/spawn period index values
ind.R.end <- which(do.day.mn$DATE == dat.R.end)
ind.S.beg <- ind.R.end + 1
ind.S.end <- which(do.day.mn$DATE == dat.S.end)

# Create table of 7D mean-min and 7D min-mean of DO at 100 per cent sat 
for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Cold-water Period; i refers to dates, j refers to stations
  for (i in 1:ind.R.end) {
    if (do.day.mn[i, 1] < dat.R.beg + 7) {
      do.7d.mn[i, j + 1] <- NaN
      do.7d.mi[i, j + 1] <- mean(do.day.mi[1:i, j + 1], na.rm = TRUE)
    }
    else {
      do.7d.mn[i, j + 1] <- NaN
      do.7d.mi[i, j + 1] <- mean(do.day.mi[(i - 6):i, j + 1], na.rm = TRUE)
    }
  }
  # Calculate rolling average of daily mean/min for Spawning Period
  for (i in ind.S.beg:ind.S.end) {
    if (do.day.mn[i, 1] < dat.S.beg + 7) {
      do.7d.mn[i, j + 1] <- mean(do.day.mn[ind.S.beg:i, j + 1], na.rm = TRUE)
      do.7d.mi[i, j + 1] <- NaN
      do.day.mi[i, j + 1] <- NaN
    }
    else {
      do.7d.mn[i, j + 1] <- mean(do.day.mn[(i - 6):i, j + 1], na.rm = TRUE)
      do.7d.mi[i, j + 1] <- NaN
      do.day.mi[i, j + 1] <- NaN
    }
  }
}

# Create table of 30D mean-min of DO at 100 per cent sat 
for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Cold-water Period; i refers to dates, j refers to stations
  for (i in 1:ind.R.end) {
    if (do.day.mn[i, 1] < dat.R.beg + 30) {
      do.30d.mn[i, j + 1] <- mean(do.day.mn[1 : i, j + 1], na.rm = TRUE)
    }
    else {
      do.30d.mn[i, j + 1] <- mean(do.day.mn[(i - 29):i, j + 1], na.rm = TRUE)
    }
  }
  # Calculate rolling average of daily mean/min for Spawning Period - Not needed for spawning
  for (i in ind.S.beg:ind.S.end) {
    if (do.day.mn[i, 1] < dat.S.beg + 30) {
      do.30d.mn[i, j + 1] <- NaN
    }
    else {
      do.30d.mn[i, j + 1] <- NaN
    }
  }
}

#--------------------REFORMAT DATA AND CHANGE STATION NAMES---------------------
#-------------------------------------------------------------------------------
# Reshape the calculated daily DO tables into long format 
do.7d.mn.gr <- melt(do.7d.mn, id.vars = "DATE", variable.name = "Station")
do.7d.mi.gr <- melt(do.7d.mi, id.vars = "DATE", variable.name = "Station")
do.30d.mn.gr <- melt(do.30d.mn, id.vars = "DATE", variable.name = "Station")
do.day.mi.gr <- melt(do.day.mi, id.vars = "DATE", variable.name = "Station")
# Rename the values in the Station column
sites.old <- unique(do.7d.mi.gr$Station)
data.site1 <- read.csv(paste0(data.dir, dir.sub1, "sites_order_z.csv"))
sites.new1 <- data.site1$FULL_NAME
for (i in 1:length(sites.old)) {
  do.7d.mn.gr$Station <- gsub(sites.old[i], sites.new1[i], do.7d.mn.gr$Station)
  do.7d.mi.gr$Station <- gsub(sites.old[i], sites.new1[i], do.7d.mi.gr$Station)
  do.30d.mn.gr$Station <- gsub(sites.old[i], sites.new1[i], do.30d.mn.gr$Station)
  do.day.mi.gr$Station <- gsub(sites.old[i], sites.new1[i], do.day.mi.gr$Station)
}
# These are added to include a hard return for the legend entries
do.7d.mn.gr$Station <- gsub("_zz_", "\n", do.7d.mn.gr$Station)
do.7d.mi.gr$Station <- gsub("_zz_", "\n", do.7d.mi.gr$Station)
do.30d.mn.gr$Station <- gsub("_zz_", "\n", do.30d.mn.gr$Station)
do.day.mi.gr$Station <- gsub("_zz_", "\n", do.day.mi.gr$Station)

sta.order <- c(12, 13, 3, 6, 1, 5, 8, 9, 2, 4, 7, 11, 10)
do.7d.mn.gr$Station <- factor(do.7d.mn.gr$Station, levels(factor(do.7d.mn.gr$Station))[sta.order])
do.7d.mi.gr$Station <- factor(do.7d.mi.gr$Station, levels(factor(do.7d.mi.gr$Station))[sta.order])
do.30d.mn.gr$Station <- factor(do.30d.mn.gr$Station, levels(factor(do.30d.mn.gr$Station))[sta.order])
do.day.mi.gr$Station <- factor(do.day.mi.gr$Station, levels(factor(do.day.mi.gr$Station))[sta.order])

#--------------------------------PLOT DATA--------------------------------------
#-------------------------------------------------------------------------------
save.dir <- paste0(data.dir, "005_reporting\\figures")

# Cold-water criterion 1, 30-day mean minimum concentrations -- NEEDS caveated legend for
do.30d.mn.plot <- ggplot() + geom_line(data = do.30d.mn.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                 scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                 xlab("Date") + ylab("Dissolved Oxygen (mg/L)") +
                 scale_x_date(date_breaks = "14 days", date_labels = "%m/%d", limits = c(dat.R.beg, dat.R.end)) +
                 scale_y_continuous(limits = c(5, 10), breaks = c(5, 6, 7, 8, 9, 10)) +
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                 geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8), color = "blue", size = 0.5, linetype = 2) +
                 annotate("text", dat.R.end - 21, 7.85, color = "black", label = "Cold-water Minimum, 8.0 mg/L", hjust = 0.25) +
                 theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                       legend.text=element_text(size=8))

ggsave(filename = "fig15_lswcd_doc_30dmn_rear.png", plot = do.30d.mn.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)

# Cold-water criterion 2, 7-day minimum mean concentrations
do.7d.mi.plot <- ggplot() + geom_line(data = do.7d.mi.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                 scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                 xlab("Date") + ylab("Dissolved Oxygen (mg/L)") +
                 scale_x_date(date_breaks = "14 days", date_labels = "%m/%d", limits = c(dat.R.beg, dat.R.end)) +
                 scale_y_continuous(limits = c(5, 10), breaks = c(5, 6, 7, 8, 9, 10)) +
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                 geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5), color = "blue", size = 0.5, linetype = 2) +
                 annotate("text", dat.R.beg + 7, 6.35, color = "black", label = "Cold-water Minimum, 6.5 mg/L", hjust = 0.25) +
                 theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                       legend.text=element_text(size=8))
 
ggsave(filename = "fig16_lswcd_doc_7dmi_rear.png", plot = do.7d.mi.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)
 
# Cold-water criterion 3, diurnal minimum concentrations
do.day.mi.plot <- ggplot() + geom_line(data = do.day.mi.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                 scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                 xlab("Date") + ylab("Dissolved Oxygen (mg/L)") +
                 scale_x_date(date_breaks = "14 days", date_labels = "%m/%d", limits = c(dat.R.beg, dat.R.end)) +
                 scale_y_continuous(limits = c(5, 10), breaks = c(5, 6, 7, 8, 9, 10)) +
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                 geom_segment(aes(x = dat.R.beg, y = 6.0, xend = dat.R.end, yend = 6.0), color = "blue", size = 0.5, linetype = 2) +
                 annotate("text", dat.R.beg + 7, 5.85, color = "black", label = "Cold-water Minimum, 6.0 mg/L", hjust = 0.25) +
                 theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                       legend.text=element_text(size=8))
 
ggsave(filename = "Fig17_vol_doc_dimi_rear.png", plot = do.day.mi.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)
 
# Spawning criterion, 7-day mean minimum concentrations
do.7d.mn.plot <- ggplot() + geom_line(data = do.7d.mn.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                 scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                 xlab("Date") + ylab("Dissolved Oxygen (mg/L)") +
                 scale_x_date(date_breaks = "14 days", date_labels = "%m/%d",limits = c(dat.S.beg, dat.S.end)) +
                 scale_y_continuous(limits = c(5, 15), breaks = c(5, 6, 7, 8, 9, 10, 11, 12)) +
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                 geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11), color = "blue", size = 0.5, linetype = 2) +
                 annotate("text", dat.S.beg + 3, 11.25, color = "black", label = "Spawning Minimum, 11.0 mg/L", hjust = 0) +
                 theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                 legend.text=element_text(size=8))

ggsave(filename = "fig19_lswcd_doc_7dmn_spwn.png", plot = do.7d.mn.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)
