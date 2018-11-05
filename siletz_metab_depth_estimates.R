# Flow and depth estiamtes ----
# This script estimates flows and depth for each basin to inform the stream metabolism
# estimates more accurately. This script assumes flows basin on basin size scaled from
# USGS gage flows during the dates of the monitoring -- 07/01/2017 - 10/31/2017
# October 30, 2018
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# This script uses flow/velocity/depth relationships developed by John Yagecic, P.E
# https://github.com/JohnYagecic/ManningsConverterBatch

# LIBRARIES ----
library(reshape)
library(ggplot2)
library(scales)

# Declarations ----

dir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"
subDir1 = "001_data\\"
subDir2 = "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\"

# 1) Import and organize reach statistics and flow 2 depth coefficients ----

# Read in tables 

staid = read.csv(paste0(dir, subDir2, "sites_order_z.csv"), stringsAsFactors = FALSE)

rchStats = read.csv(paste0(dir, subDir1, "HSPF_Basin_Reach_Statistics.csv"),
                    stringsAsFactors = FALSE)

q2d = read.csv(paste0(dir, subDir1, "siletz_q2d_coefficients.csv"), stringsAsFactors = FALSE)

doData = readRDS(paste0(dir, subDir1, "\\metabolism\\data.DO.metab.NAreplce.RData"))

# Add a column to Stations table designate corresponding HSPF F Table

staid = staid[-1, ]

staid$BAS = c(16, 15, 14, 13, 12, 11, 8, 7, 6, 1, 10, 10)

# Add a column to Reach table station to corresponding HSPF Basin

rchStats$Bas = c(16, 15, 14, 13, 12, 11, 8, 7, 6, 3, 1, 2, 10, 9, 5, 4)

# Add a column to Reach table to indicate the percent area of each basin as a function 
# of the area at the USGS station STAID 38918

rchStats$areaPct = rchStats$area.sqkm / rchStats[6, 15]

# Remove unnecessary columns

rchStats = rchStats[, -c(1, 2, 5, 6, 8 : 12, 17)]

# Create a lookup table to relate instantaneous flows to the ration of basin area to USGS basin area

staBas = merge(staid, rchStats, by.x = "BAS", by.y = "Bas")

# Remove extra columns

staBas = staBas[, -c(3, 4, 6 : 15, 17 : 22)]

# Re-order to set the station order from D/S to U/S

staBas = staBas[c(12, 11, 10, 9, 8, 7, 4, 3, 2, 1, 5, 6), ]

rownames(staBas) = 1 : nrow(staBas)

# Manually add area from South Fork to North Fork to combine areas

staBas[10, 5] = (rchStats[11, 6] + rchStats[12, 6]) / rchStats[6, 6]

# 2) Import Flow data ----

qData = readLines(paste0(dir, "\\001_data\\flow_data\\USGS_Sta5500_Sil_InstQ_0717_1117.txt"))

# Parse data

qData = data.frame(do.call("rbind", strsplit(qData, "\\s+")), stringsAsFactors = FALSE)

# Remove unnecessary columns

qData = qData[, -c(1, 2, 5, 7)]

names(qData) = c("date", "time", "Q_cfs")

# reformat columns

qData$datetime = as.POSIXct(paste0(qData$date, " ", qData$time),
                            "%Y-%m-%d %H:%M",
                            tz = "America/Los_Angeles",
                            origin = "1970-01-01") 

qData$Q_cfs = as.numeric(qData$Q_cfs)

qData = qData[, c(4, 3)]

# 3) Parse flow data per catchment ----

# Iterate thru stations for time series data of flow per basin based on percent area of total basin
# Scaled to the flow at the USGS station, which is Basin 11, STAID 38918

for (i in 1 : nrow(staBas))
{
    
    qData = cbind(qData, round(qData$Q_cfs * staBas[i, 5], 1))
    
}

# New column names

colnames(qData) = c("datetime", "Q_cfs", paste0("Q", staid$STATION))

# Remove original flow column

qData = qData[ , -2]

# 4) Estimate depth at each site ----

qDepth = qData

# Calculate the depths of the reach based on the equation coefficients by John Yagecic
# The equations for Ojalla and First Bridge give negative depths--Ask David if this is what
# happened (stations 38300 & 29287). Switch out stations using coefficients for North Fork station

# Re-order q2d records

q2d = q2d[c(1, 2, 11, 11, 5, 6, 7, 8, 9, 11, 13, 13), ]

rownames(q2d) = 1 : nrow(q2d)

# Calculate depths

for (i in 1 : nrow(staid))
{
    
    qDepth[, i + 1] = round(q2d[i, 8] * qData[, i + 1]^q2d[i, 9], 2)
    
}

# Plot for check

# qDepthMelt = melt(qDepth, id.vars = "datetime")
# 
# names(qDepthMelt) = c("datetime", "STAID", "depth_f")
# 
# depPlot = ggplot(qDepthMelt, aes(x = datetime, y = depth_f, group = STAID)) + geom_line() +
#     scale_y_log10()

# Subset and iterate through the stations

swDt = as.POSIXct("2017-09-01 00:00", tz = "America/Los_Angeles")

stations = staid$STATION

doData2 = NULL

for (i in 1 : length(stations))
{

    # CONDITION 1 - COLD-WATER PERIOD
    
    doSub = doData[which(doData$STAID == stations[i] & doData$DATE.TIME < swDt), ]

    depSub = qDepth[, c(1, i + 1)]
    
    depSub[, 2] = depSub[, 2] * 0.3048 # Comvert to depth in feet to depth in meters
        
    tmp = merge(doSub, depSub, by.x = "DATE.TIME", by.y = "datetime")
    
    # Estimate depth at time i as the intial depth plus the change in water surface elevation (WSE)
    # change in WSE is defined as the WSE at time i minus the initial WSE 

    strWSE = tmp[1, 10] # Initial water surface elevation
    
    tmp$D2 = tmp$DEP_CW

    tmp[, 11] = tmp[, 8] + tmp[, 10] - strWSE # D_i = D_o + (WSE_i - WSE_o)
    
    tmp = tmp[, -10]

    doData2 = rbind(doData2, tmp)
                
    # CONDITION 2 - SPAWNING PERIOD
    
    doSub = doData[which(doData$STAID == stations[i] & doData$DATE.TIME > swDt), ]
    
    depSub = qDepth[, c(1, i + 1)]
    
    depSub[, 2] = depSub[, 2] * 0.3048
    
    tmp = merge(doSub, depSub, by.x = "DATE.TIME", by.y = "datetime")        
    
    strWSE = tmp[1, 10]
    
    tmp$D2 = tmp$DEP_CW
    
    tmp[, 11] = tmp[, 9] + tmp[, 10] - strWSE
    
    tmp = tmp[, -10]
    
    doData2 = rbind(doData2, tmp)
    
}

# delete the extra depth columns

doData2 = doData2[, -c(8, 9)]

saveRDS(doData2, paste0(dir, subDir1, "metabolism\\data.DO.metab.NAreplce_2.RData"))

