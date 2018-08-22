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
# Replace STAID as integers to Full Names and create factors for station list and reorder
data.all$Station <- gsub("_zz_", "\n", data.all$Station)
data.all <- data.all[ -c(2, 6, 9)]
# Reformat data into a date/time by column table
data.par.sta <- list()
data.par.sta.min <- list()
data.par.sta.max <- list()
parms <- names(data.all[, 4 : 6])
STAID <- unique(data.all$STAID)
for (i in 1 : 3)
    {
        data.par.sta[[i]] <- dcast(data.all, data.all$DATE.TIME ~ data.all$STAID, na.rm = TRUE, value.var = parms[i])
        colnames(data.par.sta[[i]])[1] <- "DATE.TIME"
        col.order <- c(1, 13, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
        data.par.sta[[i]] <- data.par.sta[[i]][col.order]
        data.par.sta[[i]]$DATE <- as.Date(data.par.sta[[i]]$DATE.TIME - hours(7))
        data.par.sta.min[[i]] <- unique(data.par.sta[[i]]$DATE)
        data.par.sta.min[[i]][, 2 : length(STAID)] <- NA
    }


