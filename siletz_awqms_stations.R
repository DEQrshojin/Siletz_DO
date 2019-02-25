# LIBRARIES ----
library(AWQMSdata)
library(raster)
library(sp)

# Bring in basin shapefile ----
dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_Siletz_River_1710020405/"
shpDir <- "/004_gis/001_data/001_shape/"
datDir <- "001_data/wq_data/"
shpFil <- paste0(dir, shpDir, "HUC12_siletz.shp")
shpFil <- shapefile(shpFil)

# Select withi the HUC ----
slzStat <- AWQMS_Stations(HUC8 = 17100204)
slzStat <- slzStat[(slzStat$MonLocType == 'River/Stream'), ]
slzStat <- slzStat[which(slzStat$HUC12 %in% shpFil@data[["HUC_12"]]), ]
slzStat <- slzStat[complete.cases(slzStat), ]

# Reproject to WGS84 (to match station coordinate system) ----
projStat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
shpFil <- spTransform(shpFil, crs(projStat))
coordinates(slzStat) <- ~Long_DD + Lat_DD
proj4string(slzStat) <- proj4string(shpFil)

plot(shpFil)
points(slzStat$Long_DD, slzStat$Lat_DD, col = "red", cex = 1)

slzChar <- AWQMS_Chars(station = slzStat@data[["MLocID"]])

# Search for DO, temperature, nitrogen, phosphorus, carbon, sediment
slzParm <- c(unique(slzChar[grep('[Tt]emp', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Cc]arbon', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Oo]xygen', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Nn]itrate | [Nn]itrite | [Nn]itrogen', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Aa]mmonia', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Pp]hosphorus', slzChar$Char_Name), ]),
             unique(slzChar[grep('[Pp]hosphate', slzChar$Char_Name), ]),
             'pH', 'Total Suspended Solids', 'Turbidity',
             'Chlorophyll a', 'Total Nitrogen', 'Nitrogen')

slzParm <- slzParm[-c(3, 4, 7)]

slzWQData <- AWQMS_Data(startdate = '1950-01-01', enddate = '2018-04-01',
                        station = slzStat@data[["MLocID"]], char = slzParm)

saveRDS(object = slzWQData, file = paste0(dir, datDir, 'awqms_WQ_data_raw_20181220.Rdata'),
        ascii = FALSE)
