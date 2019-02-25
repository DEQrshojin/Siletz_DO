
# Load the data
wqDat <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/',
                        'Dissolved Oxygen/Middle_Siletz_River_1710020405/',
                        '001_data/wq_data/awqms_WQ_data.Rdata'))

# Isolate DO
wqDat <- wqDat[['data']][(wqDat[['data']]$new == 'DO' |
                          wqDat[['data']]$new == 'Tmp'), ]

# 2012 303(d) Cat 5 listing: 3 exceedences of 10 days of sampling between
# 9/13/07 and 11/12/08 at station 34457. Narrow to those dates and location
dts <- as.POSIXct(c('09/13/2007', '11/12/2008'), format = '%m/%d/%Y',
                  tz = 'America/Los_Angeles')

wqDat <- wqDat[(wqDat$dt >= dts[1] & wqDat$dt <= dts[2]), ]

doDatStn <- wqDat[wqDat$stn == '34457-ORDEQ', ]

doDatExc <- doDatStn[doDatStn$val < 11 & doDatStn$new == 'DO', ]

excDt <- unlist(doDatExc$dt)

wqPrd <- doDatStn[doDatStn$dt %in% excDt, ]

wqPrd <- wqPrd[, -c(1, 3 : 4, 6 : 7, 9 : 15)]

wqPrd <- dcast(wqPrd, dt ~ new, fun.aggregate = sum, value.var = 'val')

wqPrd$T_K <- wqPrd$Tmp + 273.15

z <- 110 * 0.3048 # convert elevation of 110 ft to meters

# Using Eq. 1 from Pelletier and Chapra 2008. Qual2kw theory and documentation,
# Coefficients:
a <- -139.34411
b <- 1.575701*10^5
c <- -6.642308*10^7
d <- 1.2438*10^10
e <- -8.621949*10^11
f <- 0.0001148

bb <- b / wqPrd$T_K
cc <- c / wqPrd$T_K^2
dd <- d / wqPrd$T_K^3
ee <- e / wqPrd$T_K^4
ff <- (1 - f * z)

sumParts <- a + bb + cc + dd + ee

expParts <- exp(sumParts)

wqPrd$DOS <- (wqPrd$DO / expParts * ff) * 100


library(ggplot2)
library(scales)

# Read in USGS data from csv (URL not working on 12/26/18) ----
dir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
              'Middle_Siletz_River_1710020405/001_data/flow_data/')

svDir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
                'Middle_Siletz_River_1710020405/005_reporting/figures/',
                'modeling_memo')

fil <- 'siletz_q_data_2.csv'

qDat <- read.csv(paste0(dir, fil), stringsAsFactors = FALSE)

qDly <- mean_daily_flow(qDat)

qDly <- qDly[complete.cases(qDly[, 2 : 3]), ]

qDly$doyF <- as.factor(qDly$DOY)

# Create a daily timeseries from 7/1/17 - 11/1/17
qRnk2012 <- pct_rank_doy(qDat, doDatStn$dt)

qRnkExcd <- pct_rank_doy(qDat, doDatExc$dt)

tsQ2017 <- as.POSIXct(c('2017-07-01', '2017-11-01'), tz = 'America/Los_Angeles')

tsQ2017 <- seq(tsQ2017[1], tsQ2017[2], 86400)

qRnk2017 <- pct_rank_doy(qDat, tsQ2017)

# Calculate mean daily flow quantiles for the plot
qntls <- c(0.05, 0.25, 0.5, 0.75, 0.95)

qQntls <- flow_quantiles(qDat, qntls)

# Want to plot qDly as box plot and qRnk2017 + qRnk2015 as points
qAnPlt <- ggplot() + 
  geom_boxplot(data = qDly, aes(x = doyF, y = qMean, group = doyF),
               outlier.shape = 4, outlier.size = 0.8) +
  geom_point(data = qRnkExcd, aes(x = doy, y = qMean), # White shadow
             shape = 17, size = 4, color = 'white') +
  geom_point(data = qRnkExcd, aes(x = doy, y = qMean), # 2010 Exceedences
             shape = 24, size = 4, color = 'red', stroke = 1.2) +
  geom_point(data = qRnk2012, aes(x = doy, y = qMean), # 2010 303(d) DO Flows
             shape = 17, size = 2.5, color = 'red') +
  geom_label(data = qRnkExcd, aes(x = doy, y = qMean, label = date),
             hjust = -0.2, vjust = 0.9) +
  geom_line(data = qRnk2017, aes(x = doy, y = qMean), # mean daily flows 2017
            size = 1.8, color = 'white') +
  geom_line(data = qRnk2017, aes(x = doy, y = qMean), # mean daily flows 2017
            size = 0.7, color = 'blue') +
  scale_y_log10(limits = c(10, 40000),
                breaks = c(10, 100, 1000, 10000, 40000)) +
  scale_x_discrete(breaks = seq(1, 365, 14),
                   labels = as.character(format(as.Date(seq(1, 365, 14),
                                                        origin = "2017-01-01"),
                                                '%m/%d'))) +
  xlab('Day of the Year') + ylab('Stream Flow (cfs)') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "do_303d_listing_data_vs_flow.png", plot = qAnPlt,
       path = svDir, width = 10, height = 7.5, units = "in", dpi = 300)






