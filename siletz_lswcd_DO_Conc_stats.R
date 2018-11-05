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

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA) ----

# Import data from master data file

dataDir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"

dirSub1 <- "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"

dataTmp <- read.csv(paste0(dataDir, dirSub1, "siletz_volmon_cont_data.csv"))

dataTmp$DATE.TIME <- as.POSIXct(dataTmp$DATE.TIME,
                                format = "%m/%d/%Y %H:%M",
                                tz = "America/Los_Angeles")

staid <- unique(dataTmp$STAID) # create a list of stations

staid <- staid[-1] # remove Strome Park 38941

dataTmp <- dataTmp[complete.cases(dataTmp[, 4:7]), ] # Remove NA

dataTmp$DATE <- as.Date(dataTmp$DATE.TIME, tz = "America/Los_Angeles") # Create dates columns

# Create DO at saturation concentrations, i.e., at 100% if greater than 100% as per OAR 340-041-0002

dataTmp$DOS_mgL <- ifelse(dataTmp$DO_Sat > 100,
                          dataTmp$DO_mgL / dataTmp$DO_Sat * 100,
                          dataTmp$DO_mgL)

# CALCULATE DAILY, 7-DAY and 30-DAY STATISTICS ----

# Create table of daily stats and re-order D/S -> U/S

doDayMn <- dcast(dataTmp, dataTmp$DATE ~ dataTmp$STAID, fun = mean, value.var = "DOS_mgL")

doDayMi <- dcast(dataTmp, dataTmp$DATE ~ dataTmp$STAID, fun = min, value.var = "DOS_mgL")

# Change out Inf to NA in the Min mean DO table

for (z in 1:ncol(doDayMi)){doDayMi[ , z][is.infinite(doDayMi[ , z])] = NA}

col.order <- c(1, 14, 4, 7, 2, 6, 9, 10, 3, 5, 8, 12, 11) # Initial ordering in alphabetical

doDayMn <- doDayMn[col.order] #Reorder sites from alphabetical to D/S -> U/S

doDayMi <- doDayMi[col.order] 

# Change the DATES column name in both tables 

colnames(doDayMn)[colnames(doDayMn) == "dataTmp$DATE"] <- "DATE"

colnames(doDayMi)[colnames(doDayMi) == "dataTmp$DATE"] <- "DATE"

# Duplicate the do.day.mn for 7-day mean min, 30-day mean min and 7 day min mean

doStats = list(doDayMi, doDayMi, doDayMi, doDayMi)

statNames = c("CW30D", "CW7D", "CWAbs", "SP7D")

names(doStats) = statNames

# Rear/spawn period dates

dates <- c(min(doDayMi$DATE), as.Date("2017-08-30"), as.Date("2017-09-07"), as.Date("2017-10-30"))

# Rear/spawn period index values

endRIndex <- which(doDayMn$DATE == dates[2])

strSIndex <- endRIndex + 1

endSIndex <- which(doDayMn$DATE == dates[4])

# Create table of 7D mean-min and 7D min-mean of DO at 100 per cent sat 

for (j in 1 : length(staid)) # loop through stations
{

    # Calculate rolling average of daily mean/min for Cold-water Period
    
    for (i in 1 : endRIndex) # loop through days
    {
        
        if (doDayMn[i, 1] < dates[1] + 7) # If less than the the start of 
            
        {
        
            doStats[["SP7D"]][i, j + 1] <- NaN
            
            doStats[["CW7D"]][i, j + 1] <- mean(doDayMi[1 : i, j + 1], na.rm = TRUE)
            
        }
    
        else
    
        {
    
            doStats[["SP7D"]][i, j + 1] <- NaN
        
            doStats[["CW7D"]][i, j + 1] <- mean(doDayMi[(i - 6) : i, j + 1], na.rm = TRUE)
        
        }
        
    }
    
    # Calculate rolling average of daily mean/min for Spawning Period

    for (i in strSIndex : endSIndex) # loop through days
    {
        
        if (doDayMn[i, 1] < dates[3] + 7)
        {
      
            doStats[["SP7D"]][i, j + 1] <- mean(doDayMn[strSIndex : i, j + 1], na.rm = TRUE)
            
            doStats[["CW7D"]][i, j + 1] <- NaN
            
            doStats[["CWAbs"]][i, j + 1] <- NaN
            
    }
        else
        {
      
            doStats[["SP7D"]][i, j + 1] <- mean(doDayMn[(i - 6) : i, j + 1], na.rm = TRUE)
            
            doStats[["CW7D"]][i, j + 1] <- NaN

            doStats[["CWAbs"]][i, j + 1] <- NaN

        }
    }
}

# Create table of 30D mean-min of DO at 100 per cent sat 

for (j in 1:length(staid))
{

    # Calculate rolling average of daily mean/min for Cold-water Period

    for (i in 1 : endRIndex)
    {
    
        if (doDayMn[i, 1] < dates[1] + 30)
        {
            
            doStats[["CW30D"]][i, j + 1] <- mean(doDayMn[1 : i, j + 1], na.rm = TRUE)
      
        }
        else
        {
            
            doStats[["CW30D"]][i, j + 1] <- mean(doDayMn[(i - 29) : i, j + 1], na.rm = TRUE)
            
        }
    }

    # Calculate rolling average of daily mean/min for Spawning Period - Not needed for spawning
    
    for (i in strSIndex : endSIndex)
    {
        
        if (doDayMn[i, 1] < dates[3] + 30)
        {
            
            doStats[["CW30D"]][i, j + 1] <- NaN
            
        }
        else
        {
            
            doStats[["CW30D"]][i, j + 1] <- NaN
            
        }
  }
}

# REFORMAT DATA AND CHANGE STATION NAMES ----

# Reshape the calculated daily DO tables into long format 

doStatGG = list()

for (stat in statNames)
{
    
    doStatGG[[stat]] = melt(doStats[[stat]], id.vars = "DATE", variable.name = "Station")
    
}

# Reorganize the data to rename station names and column order

sitesOld <- unique(doStatGG[["CW30D"]]$Station) # Rename the values in the Station column

dataSite1 <- read.csv(paste0(dataDir, dirSub1, "sites_order_z.csv"))

dataSite1 = dataSite1[-1, ]

sitesNew1 <- dataSite1$FULL_NAME

staOrder <- c(12, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New column order

for (i in 1 : length(sitesOld))
{

    for (stat in statNames) # Iterate through each list of stats and rename and reorder
    {
        
        # Change out the station names for each site
        
        doStatGG[[stat]]$Station = gsub(sitesOld[i], sitesNew1[i], doStatGG[[stat]]$Station)
        
        # Substitute the _zz_ with a space (legacy code--used to a hard return)
        
        doStatGG[[stat]]$Station <- gsub("_zz_", " ", doStatGG[[stat]]$Station)
        
        # Factorize the stations, then reorder
        
        doStatGG[[stat]]$Station <- factor(doStatGG[[stat]]$Station,
                                           levels(factor(doStatGG[[stat]]$Station))[staOrder])
        
    }
}

# PLOT DATA ----

saveDir <- paste0(dataDir, "005_reporting\\figures\\analysis_memo")

val <- read.csv(paste0(saveDir, "\\pltVals.csv"), stringsAsFactors = FALSE)

val$annlab = gsub("_zz_", "\n", val$annlab)

val$xlim1 = as.Date(val$xlim1, "%m/%d/%Y")

val$xlim2 = as.Date(val$xlim2, "%m/%d/%Y")

doPlots <- list()

for (i in 1 : length(statNames))
{
    
    doPlots[[i]] <- ggplot(data = doStatGG[[i]], aes(x = DATE, y = value)) + geom_line() +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        xlab("") + ylab("Dissolved Oxygen (mg/L)") +
        scale_x_date(date_breaks = "14 days", date_labels = "%m/%d",
                     limits = c(val[i, 2], val[i, 3])) +
        scale_y_continuous(limits = c(c(val[i, 4], val[i, 5])),
                           breaks = c(seq(val[i, 4], val[i, 5], 1))) +
        geom_hline(yintercept = val[i, 6], color = "blue", size = 0.5, linetype = 2) +
        annotate("text", val[i, 3] - val[i, 11], val[i, 7], color = "black",
                 label = val[i, 8], hjust = 0, vjust = val[i, 10], size = 3) +
        theme(plot.title = element_text(size = 10, hjust = 0.5),
              axis.title.x = element_blank()) + 
        facet_wrap(~Station, ncol = 3)

    ggsave(filename = val[i, 9], plot = doPlots[[i]], path = saveDir,
           width = 7.5, height = 9, units = "in", dpi = 300)
    
}
