# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(ggplot2)
library(scales)

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA) ----

# Import data from master data file

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405"

dir.sub1 <- "\\001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"

dir.sub2 <- "\\005_reporting\\figures\\analysis_memo"

data.all <- read.csv(paste0(dir, dir.sub1, "siletz_volmon_cont_data.csv"))

data.all$DATE.TIME <- as.POSIXct(data.all$DATE.TIME, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

data.all <- data.all[complete.cases(data.all[, 5]), ] # Remove NA

data.all$STAID[data.all$STAID == 10391] <- 29287 # Replace 10391 with 29287

# Remove 38941 - Strom Park 

data.all = data.all[which(data.all$STAID != 38941), ]

# Factorize stations and reorder

staOrder <- c(12, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New column order

data.all$Station <- factor(data.all$Station, levels(factor(data.all$Station))[staOrder])

# Set criterion date bounds for Cold-water and spawning (truncated to monitoring periods)

dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Rearing start date

dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Rearing end date

dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date

dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Spawn end date

lims.t <- c(dat.R.beg, dat.S.end)

# PLOT DATA ----

save.dir <- paste0(dir, dir.sub2)

mf <- 86400

x <- ggplot(data.all) + geom_point(aes(x = DATE.TIME, y = TEMP_C), size = 0.1, shape = 1) +
    xlab("Date") + ylab("Temperature (°C)") +
    scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    scale_x_datetime(limits = lims.t,
                     breaks = date_breaks("1 months"),
                     labels = date_format("%m/%d")) +
    theme_bw() + theme(panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 10),
                       axis.title.y = element_text(size = 10),
                       axis.text.y = element_text(size = 10),
                       plot.title = element_text(size = 10, hjust = 0.5)) +
    geom_segment(aes(x = dat.R.beg, y = 16, xend = dat.R.end, yend = 16),
                 color = "blue", size = 0.4, linetype = 2) + 
    geom_segment(aes(x = dat.S.beg, y = 13, xend = dat.S.end, yend = 13),
                 color = "red", size = 0.4, linetype = 2) +
    annotate("text", dat.S.end, 11.5, color = "black", label = "Spawning, 13°C",
             hjust = 1, size = 2.5) +
    annotate("text", dat.R.beg + mf * 1, 14.5, color = "black", label = "Cold-water, 16°C",
             hjust = 0, size = 2.5) +
    facet_wrap(~Station, ncol = 3)


ggsave(filename = "fig08_lswcd_temp_all.png", plot = x, path = save.dir, width = 7.5,
       height = 9, units = "in", dpi = 300)









      