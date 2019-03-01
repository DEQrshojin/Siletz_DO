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

# Isolate parameters: NH3, NOx, TKN, TP, PO4, TSS, Turb
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

wqDF <- wqSed

wqSed <- load_flows(wqSed)

# sedTSPlot <- ggplot(data = wqSed, aes(x = date, y = load), size = 2) +
#   geom_line() + geom_point() + scale_y_log10(limits = c(0.1, 10000))
# 
# write.csv(wqSed, file = 'D:/siletz_tmp/scratch/wq/sediment/sediment_STA10391.csv',
#           row.names = FALSE)

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
  
  # Convert to a load conversions: cfs -> m3s; sec/day; mg -> tons (US, short)
  wqDF$load <- wqDF$vnd * wqDF$qSlz * 28.317 * 86400 * 1.10e-9
  
  return(wqDF)

}

# # NITROGEN ----
# nSpc <- c('NH3', 'NOx', 'TKN')
# 
# wqNDat <- wqData[wqData$new %in% nSpc, c(17, 5, 16, 9)]
# 
# wqNDat2 <- dcast(wqNDat, date ~ new, fun.aggregate = sum, value.var = 'vnd')
# 
# wqNDat2$TKN <- ifelse(wqNDat2$TKN == 0, NA, wqNDat2$TKN)
# 
# wqNDat2$OrN <- wqNDat2$TKN - wqNDat2$NH3 
# 
# wqNDat2 <- data.frame(date = wqNDat2$date,
#                       new = 'OrN',
#                       vnd = wqNDat2$OrN,
#                       opr = 'cal')
# 
# wqNDat <- rbind(wqNDat, wqNDat2)
# 
# nPlot <- ggplot(data = wqNDat, aes(x = date, y = vnd, color = factor(new),
#                                    shape = factor(opr)), size = 2) +
#   geom_point() + scale_y_continuous(limits = c(0, 1)) + 
#   scale_shape_manual(values = c(0, 15, 8))
# 
# ggsave('nconc.png', plot = nPlot, path = 'D:/siletz_tmp/scratch/wq',
#        width = 15, height = 10, units = 'in', dpi = 300)
# 
# nMeans <- aggregate(wqNDat$vnd, by = list(wqNDat$new), FUN = 'mean',
#                     na.rm = TRUE)
# 
# # LOAD FLOW DATA
# qDly <- read.csv('D:/siletz_tmp/scratch/wq/slz_qDaily.csv',
#                  stringsAsFactors = FALSE)
# 
# qDly$Date <- as.Date(qDly$Date, '%Y-%m-%d', tz = 'America/Los_Angeles')
# 
# # ESTIMATE LOADS, estimate flows at 10391 (area ration method)
# # Station 10391 is at Ojalla area = 583.1 sqmi, USGS area = 523.6 sqmi
# # Ratio = 1.114
# qDly$qSlz <- qDly$qSlz * 1.114
# 
# qMax <- max(qDly$qSlz, na.rm = TRUE)
# 
# # Calculate tons/day for each constituent
# wqNDat <- merge(wqNDat, qDly, by.x = 'date', by.y = 'Date',
#                 all.x = TRUE, all.y = FALSE)
# 
# volCnv <- 28.316846592 # Convert cubic feet to liters
# 
# tmeCnv <- 86400 # seconds per day
# 
# mssCnv <- 1.1023113109e-9 # Convert milligrams to tons (US, short)
# 
# wqNDat$load <- wqNDat$vnd * wqNDat$qSlz * volCnv * tmeCnv * mssCnv
# 
# nCons <- unique(wqNDat$new)
# 
# nCons <- nCons[-1]
# 
# for (i in 1 : length(nCons)) {
#   
#   nPlot <- ggplot(data = wqNDat[wqNDat$new == nCons[i], ],
#                   aes(x = qSlz, y = load), size = 2) + geom_point()
#   
#   ggsave(paste0('nload_', nCons[i], '.png'), plot = nPlot,
#          path = 'D:/siletz_tmp/scratch/wq', width = 15, height = 10,
#          units = 'in', dpi = 300)
# 
# }
# 
# 
# 
# 
