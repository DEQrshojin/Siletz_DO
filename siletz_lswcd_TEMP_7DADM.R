# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(reshape2)
require(dplyr)
require(ggplot2)

#----------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)----------
#-------------------------------------------------------------------------------
# Import data from master data file
data.dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"
dir.sub1 <- "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"
data.tmp <- read.csv(paste0(data.dir, dir.sub1, "siletz_volmon_cont_data.csv"))
data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
STAID_vctr <- unique(data.tmp$STAID) # create a list of stations
data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA
data.tmp$DATE <- as.Date(data.tmp$DATE.TIME, tz = "America/Los_Angeles") # Create dates columns

#----------CALCULATE DAILY, 7-DAY and 30-DAY STATISTICS----------
temp.day.mx <- dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun = max, value.var = "TEMP_C")
col.order <- c(1, 13, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
temp.day.mx <- temp.day.mx[col.order] #Reorder sites from alphabetical to D/S -> U/S
# Change out Inf to NA in the Min mean DO table
for (z in 1:ncol(temp.day.mx)){temp.day.mx[,z][is.infinite(temp.day.mx[,z])] = NA}
colnames(temp.day.mx)[colnames(temp.day.mx)=="data.tmp$DATE"] <- "DATE"
# Duplicate the do.day.mn for 7-day mean min, 30-day mean min and 7 day min mean
temp.7d.mx <- temp.day.mx
# Rear/spawn period dates and create index values for start/end dates
dat.R.beg <- min(temp.day.mx$DATE) # Rearing start date
dat.R.end <- as.Date("2017-08-30", tz = "America/Los_Angeles") # Rearing end date
dat.S.beg <- as.Date("2017-09-07", tz = "America/Los_Angeles") # Spawn start date
dat.S.end <- max(temp.day.mx$DATE) # Spawn end date
ind.R.end <- which(temp.day.mx$DATE == dat.R.end)
ind.S.beg <- ind.R.end + 1
ind.S.end <- which(temp.day.mx$DATE == dat.S.end)

# Create table of 7D mean-min and 7D min-mean of DO at 100 per cent sat 
for (j in 1:length(STAID_vctr)) {
  # Calculate rolling average of daily mean/min for Rearing Period; i refers to dates, j refers to stations
  for (i in 1:ind.R.end) {
    if (temp.day.mx[i, 1] < dat.R.beg + 7) {
      # START STATEMENT
      temp.7d.mx[i, j + 1] <- mean(temp.day.mx[1:i, j + 1], na.rm = TRUE)
    }
    else {
      # START STATEMENT
      temp.7d.mx[i, j + 1] <- mean(temp.day.mx[(i - 6):i, j + 1], na.rm = TRUE)
    }
  }
  # Calculate rolling average of daily mean/min for Spawning Period
  for (i in ind.S.beg:ind.S.end) {
    if (temp.day.mx[i, 1] < dat.S.beg + 7) {
      # START STATEMENT
      temp.7d.mx[i, j + 1] <- mean(temp.day.mx[ind.S.beg:i, j + 1], na.rm = TRUE)
    }
    else {
      # START STATEMENT
      temp.7d.mx[i, j + 1] <- mean(temp.day.mx[(i - 6):i, j + 1], na.rm = TRUE)
    }
  }
}
# Create a dummy row of NA values to separate rearing and spawning season
dum.na <- temp.7d.mx[56:62,]
dum.na[1, 1] <- as.Date("2017-08-31", tz = "America/Los_Angeles")
for (i in 2 : 7) {
  dum.na[i, 1] <- dum.na[i - 1, 1] + 1
}
dum.na[1:7, 2:13] <- NaN
temp.7d.mx <- rbind(temp.7d.mx, dum.na)
temp.7d.mx <- temp.7d.mx[order(temp.7d.mx$DATE),]

#--------------------REFORMAT DATA AND CHANGE STATION NAMES---------------------
#-------------------------------------------------------------------------------
# Reshape the calculated daily DO tables into long format 
temp.7d.mx.gr <- melt(temp.7d.mx, id.vars = "DATE", variable.name = "Station")
# Rename the values in the Station column
sites.old <- unique(temp.7d.mx.gr$Station)
data.site <- read.csv(paste0(data.dir, dir.sub1, "sites_order_z.csv"))
sites.new <- data.site$FULL_NAME
for (i in 1:length(sites.old)) {
  temp.7d.mx.gr$Station <- gsub(sites.old[i], sites.new[i], temp.7d.mx.gr$Station)
}
# These are added to include a hard return for the legend entries
temp.7d.mx.gr$Station <- gsub("_zz_", "\n", temp.7d.mx.gr$Station)
sta.order <- c(12, 13, 3, 6, 1, 5, 8, 9, 2, 4, 7, 11, 10)
temp.7d.mx.gr$Station <- factor(temp.7d.mx.gr$Station, levels(factor(temp.7d.mx.gr$Station))[sta.order])

#----------PLOT DATA----------
#-------------------------------------------------------------------------------
# Spawning criterion, 7-day mean minimum concentrations
save.dir <- paste0(data.dir, "005_reporting\\figures")
temp.7d.mx.plot <- ggplot() + geom_line(data = temp.7d.mx.gr, aes(x = DATE, y = value, group = Station, color = Station), size = 1) +
                  scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                  xlab("Date") + ylab("Temperature (°C)") +
                  scale_x_date(date_breaks = "1 month", date_labels = "%m/%d", limits = c(dat.R.beg, dat.S.end)) +
                  scale_y_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
                  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
                  geom_segment(aes(x = dat.R.beg, y = 16, xend = dat.R.end + 1, yend = 16), color = "black", size = 0.5) +
                  geom_segment(aes(x = dat.R.end + 1, y = 13, xend = dat.S.end, yend = 13), color = "black", size = 0.5) +
                  annotate("text", dat.R.beg, 15.3, color = "black", label = "Rearing Maximum, 16°C", hjust = 0, size = 3) +
                  annotate("text", dat.S.end - 45, 12, color = "black", label = "Spawning\nMaximum, 13.0°C", hjust = 0, size = 3) +
                  theme(plot.title = element_text(size = 12, hjust = 0.5), legend.text=element_text(size=8), legend.key.size = unit(2, 'lines'))

ggsave(filename = "fig21_lswcd_temp_7dadm_all.png", plot = temp.7d.mx.plot, path = save.dir, width = 8, height = 6, units = "in", dpi = 300)
