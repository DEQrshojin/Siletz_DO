# LIBRARIES AND OPTIONS ----
library(ggplot2)
library(reshape2)

options(scipen = -1, warn = -1)

# Broad measures:
# Total annual loads
# Annual export rates for HRUs compared to SPARROW data
# Seasonal loads
# Partioned annual loads (SURO/INFW/AGWO)
# Partitioned mean concentrations (SURO/INFW/AGWO)
# Perform for TSS, NOx, PO4, TP, OrgC

# Identify periods of AGWO to determine bounds of DWCs
# Identify periods of IFWO  to determine bounds of InfwCs
# The remainder of the observations will be use to clasify the bounds of EMCs

# Constituent counts - these are the parameters to be included in calibration
calPar <- c('TSS', 'NOx', 'PO4', 'TP', 'OrgC')

# LOAD DATA ----
pthOut <- 'c:/siletz_old/scratch/wq'

wqDataAll <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved O',
                            'xygen/Middle_Siletz_River_1710020405/001_data/wq_',
                            'data/awqms_WQ_data.Rdata'))

# PROCESS DATA ----
wqData <- wqDataAll[[1]]

# Remove dql = E
wqData <- wqData[which(wqData$dql != 'DQL=E'), ] 

# Select calibration constituents
wqData <- wqData[wqData$new %in% calPar, ]

# Set ND to half the reported value
wqData$vnd <- ifelse(wqData$opr == '<', wqData$val / 2, wqData$val)

# Plotting dates
pDates <- as.POSIXct(c('10-01-2004', '01-01-2020'), '%m-%d-%Y', 
                    tz = 'America/Los_Angeles')

wqData <- wqData[which(wqData$dt >= pDates[1]), ]

# Create a column of dates
wqData$date <- as.Date(wqData$dt, '%Y-%m-%d %H:%M:%S', 
                       tz = 'America/Los_Angeles')

# Identify runoff component periods


wqData <- wqData[which(wqData$dt >= pDates[1]), ]


