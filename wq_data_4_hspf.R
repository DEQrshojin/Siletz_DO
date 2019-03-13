library(ggplot2)
library(reshape2)

options(scipen = -1)

pthOut <- 'D:/siletz_old/scratch/wq'

wqDataAll <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved O',
                            'xygen/Middle_Siletz_River_1710020405/001_data/wq_',
                            'data/awqms_WQ_data.Rdata'))

wqData <- wqDataAll[[1]]

# Remove dql = E
wqData <- wqData[which(wqData$dql != 'DQL=E'), ] 

wqPars <- unique(wqData$new)

# Isolate parameters: NH3, NOx, TKN, TP, PO4, OrgC, TSS, Turb
wqPars <- wqPars[c(1, 6, 12, 10, 8, 7, 13, 14)]

wqData <- wqData[wqData$new %in% wqPars, ]

# Set ND to half the reported value
wqData$vnd <- ifelse(wqData$opr == '<', wqData$val / 2, wqData$val)

# Isolate stations
wqStns <- wqDataAll[[3]][wqDataAll[[3]]$MLocID %in% unique(wqData$stn), ]

# Plotting dates
pDates <- as.POSIXct(c('1-1-1990', '1-1-2020'), '%m-%d-%Y', 
                    tz = 'America/Los_Angeles')

wqData <- wqData[which(wqData$dt >= pDates[1]), ]

# Create a column of dates
wqData$date <- as.Date(wqData$dt, '%Y-%m-%d %H:%M:%S', 
                       tz = 'America/Los_Angeles')

# SEDIMENT ----
wqSed <- wqData[which(wqData$new == 'TSS'), ]

wqSed <- wqSed[which(wqSed$stn == '10391-ORDEQ'), ]

wqSed <- load_flows(wqSed)

write.csv(wqSed, file = 'D:/siletz/calib/wq/sediment_STA10391.csv',
          row.names = FALSE)

load_flows <- function(wqDF) {

  # LOAD FLOW DATA
  qDly <- read.csv('D:/siletz_tmp/scratch/wq/slz_qDaily.csv',
                   stringsAsFactors = FALSE)
  
  qDly$Date <- as.Date(qDly$Date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  # ESTIMATE LOADS, estimate flows at 10391 (area ration method)
  # Station 10391 is at Ojalla area = 583.1 sqmi, USGS area = 523.6 sqmi
  # Ratio = 1.114
  qDly$qSlz <- qDly$qSlz * 1.114

  # Calculate tons/day for each constituent
  wqDF <- merge(wqDF, qDly, by.x = 'date', by.y = 'Date',
                all.x = TRUE, all.y = FALSE)
  
  wqDF <- wqDF[, c(1, 17, 10, 18)]
  
  # Convert to a load conversions: cfs->m3s; sec->day; mg->tons(m);m3->L
  wqDF$load <- wqDF$vnd * wqDF$qSlz * 0.028316847 * 86400 * 10^-9 * 1000
  
  return(wqDF)

}

