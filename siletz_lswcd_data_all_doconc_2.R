# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

library(scales)
library(ggplot2)

# REVISIONS ----
# changed annotation and text font size from 1.75 to 2.25
# Removed 38941 - Strome Park results

# Import data from master data file ----

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405"

dir.sub1 <- "\\001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"

dir.sub2 <- "\\005_reporting\\figures\\analysis_memo"

data.all <- read.csv(paste0(dir, dir.sub1, "siletz_volmon_cont_data.csv"))

data.all$DATE.TIME <- as.POSIXct(data.all$DATE.TIME, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

data.all <- data.all[complete.cases(data.all[, 5:8]), ] # Remove NA

data.all$STAID[data.all$STAID == 10391] <- 29287 # Replace 10391 with 29287

# Remove 38941 - Strom Park 

data.all = data.all[which(data.all$STAID != 38941), ]

# Factorize stations and reorder

staOrder <- c(12, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New column order

data.all$Station <- factor(data.all$Station, levels(factor(data.all$Station))[staOrder])

# Set criterion date bounds for rearing and spawning (truncated to monitoring periods) ----

dat.R.beg <- as.POSIXct("2017-07-01 00:00", tz = "America/Los_Angeles") # Rearing start date

dat.R.end <- as.POSIXct("2017-08-30 00:00", tz = "America/Los_Angeles") # Rearing end date

dat.S.beg <- as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles") # Spawn start date

dat.S.end <- as.POSIXct("2017-11-01 00:00", tz = "America/Los_Angeles") # Spawn end date

lims.t <- c(dat.R.beg, dat.S.end)

# PLOT DATA ----

save.dir <- paste0(dir, dir.sub2)

mf <- 86400

x <- ggplot(data.all) + geom_point(aes(x = DATE.TIME, y = DO_mgL), size = 0.1, shape = 1) +
    xlab("Date") + ylab("DO (mg/L)") +
    scale_y_continuous(limits = c(5, 15), breaks = c(6, 8, 10, 12, 14)) +
    scale_x_datetime(limits = lims.t,
                     breaks = date_breaks("1 months"),
                     labels = date_format("%m/%d")) +
    theme_bw() + theme(panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 10),
                       axis.title.y = element_text(size = 10),
                       axis.text.y = element_text(size = 10),
                       plot.title = element_text(size = 10, hjust = 0.5)) +
    geom_segment(aes(x = dat.R.beg, y = 8, xend = dat.R.end, yend = 8),
                 color = "blue", size = 0.4, linetype = 2) +
    geom_segment(aes(x = dat.R.beg, y = 6.5, xend = dat.R.end, yend = 6.5),
                 color = "green", size = 0.4, linetype = 2) +
    geom_segment(aes(x = dat.R.beg, y = 6, xend = dat.R.end, yend = 6),
                 color = "orange", size = 0.4, linetype = 2) +
    geom_segment(aes(x = dat.S.beg, y = 11, xend = dat.S.end, yend = 11),
                 color = "red", size = 0.4, linetype = 2) +
    annotate("text", dat.S.beg, 12, color = "black",
             label = "Spawning\n11.0 mg/L", hjust = 0, size = 2.25) +
    annotate("rect", xmin = dat.R.end + mf * .5, xmax = dat.R.end + mf * 24, 
             ymin = 7.65, ymax = 8.35, fill = "white", alpha = 1) +
    annotate("text", dat.R.end + mf * 1, 8.1, color = "black",
             label = "Cold-water 30d, 8.0 mg/L", hjust = 0, size = 2.25) +
    annotate("text", dat.R.end + mf * 1, 6.65, color = "black",
             label = "Cold-water 7d, 6.5 mg/L", hjust = 0, size = 2.25) +
    annotate("text", dat.R.end + mf * 1, 5.9, color = "black",
             label = "Cold-water min, 6.0 mg/L", hjust = 0, size = 2.25) +
    facet_wrap(~Station, ncol = 4)

# Path for presentation figures
p2 <- '//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_Siletz_River_1710020405/005_reporting/presentations/003_TWG_20190403/figures_maps'

ggsave(filename = "fig06_lswcd_do_conc_all_v2.png", plot = x, path = p2,
       width = 12, height = 9, units = "in", dpi = 300)

