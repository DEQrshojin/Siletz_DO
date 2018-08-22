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
dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405"
dir.sub1 <- "\\001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"
data.tmp <- read.csv(paste0(dir, dir.sub1, "siletz_volmon_cont_data.csv"))
data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
STAID <- unique(data.tmp$STAID) # create a list of stations
data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA
data.tmp$DATE <- as.factor(as.Date(data.tmp$DATE.TIME, tz = "America/Los_Angeles")) # Create factored column of dates
DATES <- sort(unique(data.tmp$DATE)) # create a list of dates
data.tmp$STAID <- as.factor(data.tmp$STAID)
data.tmp <- data.tmp[ -c(2, 4, 6, 9)]
row.names(data.tmp) <- 1 : nrow(data.tmp)

max.min <- list(list(), list())
max.min.t <- list(list(), list())
stat <- c("max", "min")
names(max.min) <- stat
parms <- names(data.tmp[, 3 : 5])
for (j in 1 : 3) # One list for each parameter
{
    max.min[[1]][[j]] = dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun.aggregate = max, value.var = parms[j])
    max.min[[2]][[j]] = dcast(data.tmp, data.tmp$DATE ~ data.tmp$STAID, fun.aggregate = min, value.var = parms[j])
    for (i in 1 : 2)
    {
        # Reorder columns
        col.order <- c(1, 13, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical
        max.min[[i]][[j]] <- max.min[[i]][[j]][col.order]
        for (k in 1 : length(STAID))
        {
            # Replace -Inf with NA
            max.min[[i]][[j]][ , k + 1][is.infinite(max.min[[i]][[j]][ , k + 1])] = NA
            # find the time during each day for each station where the value of the
            tmp = max.min[[i]][[j]]
        }
    }
}
names(max.min[[1]]) <- parms
names(max.min[[2]]) <- parms

for (j in 1 : 3)
{
    for (i in 1 : 2)
    {
        max.min.t[[i]][[j]] <- max.min[[i]][[j]]
        max.min.t[[i]][[j]][ , 1 : length(STAID) + 1] <- NA
    }
}
num.sta <- length(STAID)
date.len <- length(max.min.t[[i]][[j]][ , 1])
tmp.vctr <- as.POSIXct(DATES)

for (j in 1 : 3)
{
    for (i in 1 : 2)
    {
        for (k in 1 : num.sta)
        {
            for (m in 1 : date.len)
            {
                # g is the index where the max/min value occurs for that parameter, max/min, station and data in that order 
                g = which(data.tmp$DATE == DATES[m] & data.tmp$STAID == STAID[k] & data.tmp[, j + 2] == max.min[[i]][[j]][m, k + 1])
                # this 
                tmp.vctr[m] <- median(data.tmp[g, 1], na.rm = TRUE) # populate the temporary Date/TIME vector with the matching max/min date 
                max.min.t[[i]][[j]] <- max.min.t[[i]][[j]][ -(k  + 1)] # delete the station in the data frame
                max.min.t[[i]][[j]] <- cbind(max.min.t[[i]][[j]], tmp.vctr) # append the temp vector to the data frame
                colnames(max.min.t[[i]][[j]]) <- c("DATE", STAID) # rename the the vector to the station
            }
        }
    }
}

names(max.min.t) <- stat
names(max.min.t[[1]]) <- parms
names(max.min.t[[2]]) <- parms

write.csv(max.min, file = (paste0(dir, dir.sub1, "maxmin_val.csv")))
write.csv(max.min.t, file = (paste0(dir, dir.sub1, "maxmin_time.csv")))
          