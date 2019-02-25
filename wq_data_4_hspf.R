library(ggplot2)
library(reshape2)

options(scipen = -1)

pthOut <- 'c:/siletz_old/scratch/wq'

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

# Need to make a list of stations * wq pars * length of record
plots <- list()

# Create a column of dates
wqData$date <- as.Date(wqData$dt, '%Y-%m-%d %H:%M:%S', 
                       tz = 'America/Los_Angeles')

# Examine nitrogen species
nSpc <- c('NH3', 'NOx', 'TKN')

wqNDat <- wqData[wqData$new %in% nSpc, c(17, 5, 16, 9)]

wqNDat2 <- dcast(wqNDat, date ~ new, fun.aggregate = sum, value.var = 'vnd')

wqNDat2$TKN <- ifelse(wqNDat2$TKN == 0, NA, wqNDat2$TKN)

wqNDat2$OrN <- wqNDat2$TKN - wqNDat2$NH3 

wqNDat2 <- data.frame(date = wqNDat2$date,
                      new = 'OrN',
                      vnd = wqNDat2$OrN,
                      opr = 'cal')

wqNDat <- rbind(wqNDat, wqNDat2)

nPlot <- ggplot(data = wqNDat, aes(x = date, y = vnd, color = factor(new),
                                   shape = factor(opr)), size = 2) +
  geom_point() + scale_y_continuous(limits = c(0, 1)) + 
  scale_shape_manual(values = c(0, 15, 8))

nMeans <- aggregate(wqNDat$vnd, by = list(wqNDat$new), FUN = 'mean',
                    na.rm = TRUE)


# ESTIMATE SPECIATION
# LOAD FLOW DATA
# ESTIMATE LOADS






str(wqNDat)
