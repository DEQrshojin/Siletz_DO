# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(reshape2)
require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)

#----------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)----------
#-------------------------------------------------------------------------------
# Import data from master data file
data.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/wq/siletz_VolMon_data/"
data.tmp <- read.csv(paste0(data.dir, "siletz_volmon_cont_data.csv"))
data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
STAID_vctr <- unique(data.tmp$STAID) # create a list of stations
data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA
data.tmp$DATE <- as.Date(data.tmp$DATE.TIME, tz = "America/Los_Angeles") # Create dates columns

#----------CALCULATE DAILY, 7-DAY and 30-DAY STATISTICS----------
#-------------------------------------------------------------------------------
# Create column to assume a DO Sat of 100% if measured DO > 100%
data.tmp$DOS_pct <- ifelse(data.tmp$DO_Sat > 100, 100, data.tmp$DO_Sat)
# Create table of mean and min daily DO per site, and re-order from Downstream -> Upstream
dos.day.mn <- dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun = mean, value.var = "DOS_pct")
dos.day.mn[, 2:13] <- round(dos.day.mn[, 2:13], 1)
col.order <- c(1, 13, 4, 2, 7, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
dos.day.mn <- dos.day.mn[col.order] #Reorder sites from alphabetical to D/S -> U/S
# Change out Inf to NA in the Min mean DO table
for (z in 1:ncol(dos.day.mn)){dos.day.mn[,z][is.infinite(dos.day.mn[,z])] = NA}
# Change the DATES column name in both tables 
colnames(dos.day.mn)[colnames(dos.day.mn)=="data.tmp$DATE"] <- "DATE"
# Duplicate the dos.day.mn for 7-day mean min, 30-day mean min and 7 day min mean
dos.30d.mn <- dos.day.mn 
dos.7d.mn <- dos.day.mn
# Rear/spawn period dates
dat.R.beg <- min(dos.day.mn$DATE) # Rearing start date
dat.R.end <- as.Date("2017-08-30", tz = "America/Los_Angeles") # Rearing end date
dat.S.beg <- as.Date("2017-09-07", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- max(dos.day.mn$DATE) # Spawn end date
# Rear/spawn period index values
ind.R.end <- which(dos.day.mn$DATE == dat.R.end)
ind.S.beg <- ind.R.end + 1
ind.S.end <- which(dos.day.mn$DATE == dat.S.end)

# Create table of 7D mean-min and 7D min-mean of DO at 100 per cent sat 
for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Rearing Period; i refers to dates, j refers to stations
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
  # Calculate rolling average of daily mean/min for Rearing Period; i refers to dates, j refers to stations
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
#-------------------------------------------------------------------------------
# Reshape the calculated daily DO tables into long format 
dos.7d.mn.gr <- melt(dos.7d.mn, id.vars = "DATE", variable.name = "Station")
dos.30d.mn.gr <- melt(dos.30d.mn, id.vars = "DATE", variable.name = "Station")
# Rename the values in the Station column
sites.old <- unique(dos.7d.mn.gr$Station)
data.site1 <- read.csv(paste0(data.dir, "sites_order_z_TEMP.csv"))
data.site2 <- read.csv(paste0(data.dir, "sites_order_z.csv"))
sites.new1 <- data.site1$FULL_NAME
sites.new2 <- data.site2$FULL_NAME
for (i in 1:length(sites.old)) {
  dos.7d.mn.gr$Station <- gsub(sites.old[i], sites.new1[i], dos.7d.mn.gr$Station)
  dos.30d.mn.gr$Station <- gsub(sites.old[i], sites.new2[i], dos.30d.mn.gr$Station)
}
# These are added to include a hard return for the legend entries
dos.7d.mn.gr$Station <- gsub("_zz_", "\n", dos.7d.mn.gr$Station)
dos.30d.mn.gr$Station <- gsub("_zz_", "\n", dos.30d.mn.gr$Station)

sta.order <- c(12, 3, 1, 6, 5, 8, 9, 2, 4, 7, 11, 10)
dos.7d.mn.gr$Station <- factor(dos.7d.mn.gr$Station, levels(factor(dos.7d.mn.gr$Station))[sta.order])
dos.30d.mn.gr$Station <- factor(dos.30d.mn.gr$Station, levels(factor(dos.30d.mn.gr$Station))[sta.order])

#--------------------------------PLOT DATA--------------------------------------
#-------------------------------------------------------------------------------
save.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/002_figures"

# Rearing criterion 1, 30-day mean minimum concentrations
dos.30d.mn.plot <- ggplot() + geom_line(data = dos.30d.mn.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                 scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                 xlab("Date") + ylab("Dissolved Oxygen (% Saturation)") +
                 scale_x_date(date_breaks = "14 days", date_labels = "%m/%d", limits = c(dat.R.beg, dat.R.end)) +
                 scale_y_continuous(limits = c(80, 110), breaks = c(80, 90, 100, 110, 110)) +
                 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                 geom_segment(aes(x = dat.R.beg, y = 90, xend = dat.R.end, yend = 90), color = "blue", size = .5, linetype = 2) +
                 annotate("text", dat.R.end - 21, 89, color = "black", label = "Rearing Minimum, 90%", hjust = 0.25) +
                 theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                       legend.text=element_text(size=8))

ggsave(filename = "fig17_lswcd_dos_30dmn_rear.jpg", plot = dos.30d.mn.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)

# Spawning criterion, 7-day mean minimum concentrations
dos.7d.mn.plot <- ggplot() + geom_line(data = dos.7d.mn.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                  scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                  xlab("Date") + ylab("Dissolved Oxygen (% Saturation)") +
                  scale_x_date(date_breaks = "14 days", date_labels = "%m/%d",limits = c(dat.S.beg, dat.S.end)) +
                  scale_y_continuous(limits = c(80, 110), breaks = c(80, 90, 100, 110)) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                  geom_segment(aes(x = dat.S.beg, y = 95, xend = dat.S.end, yend = 95), color = "blue", size = .5, linetype = 2) +
                  annotate("text", dat.S.beg +3, 94, color = "black", label = "Spawning Minimum, 95%", hjust = 0.2) +
                  theme(plot.title = element_text(size = 12, hjust = 0.5), legend.key.size = unit(2, 'lines'),
                        legend.text=element_text(size=8))

ggsave(filename = "fig19_lswcd_dos_7dmn_spwn.jpg", plot = dos.7d.mn.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)
