# August 21, 2018
# Query LASAR stations in the lower Siletz River to estimate estuary/river boundary
# Ryan Shojinaga, Water Quality Analyst, Watershed Management Section, DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Load packages
library(RODBC)
library(stringr)
library(data.table)
library(ggplot2)

# Make a connection to LASAR WEB
channel <- odbcConnect("DEQLEAD-LIMS") # Change as needed

# Grab all the stations in the database
StationsAll <- sqlFetch(channel, "STATION")

# Grab all the Area Geography and associated codes
AreaClass <- sqlFetch(channel, "XLU_AREA_CLASS")
AreaKeys <- sqlFetch(channel, "XLU_AREA")
StationAreas <- sqlFetch(channel,"STATION_AREA")

# Station query list

# Search string for Siletz-Yaquina, Siuslaw, and Siltcoos basins
HUC8 <- c("1710020404.{2,}$", "1710020405.{2,}$", "1710020406.{2,}$", "1710020407.{2,}$")

HUC8.list <- replicate(length(HUC8), list()) #empty list for global search
HUC8.2 <- character(0) #empty character string

for (i in 1:length(HUC8)){
  HUC8.list[[i]] <- as.vector(AreaKeys$AREA_ABBREVIATION[grep(HUC8[i], 
                                                              as.character(AreaKeys$AREA_ABBREVIATION))])
  HUC8.2 <- c(HUC8.2, HUC8.list[[i]])
}

HUC8.2 <- as.numeric(HUC8.2) # needs to be numeric because value exceeds max integer level

myAreas <- AreaKeys[AreaKeys$AREA_ABBREVIATION %in% HUC8.2, ]
myAreaKeys <- myAreas$AREA_KEY

# Get a list of all the stations in my area
AreaStations <- StationAreas[StationAreas$XLU_AREA %in% myAreaKeys, ]
myStations <- as.integer(AreaStations$STATION)
siletzStations <- StationsAll[StationsAll$STATION_KEY %in% myStations, ]

myStations.2 <- paste(myStations, collapse =", ")

# My parameters

# LASAR parameters
# myParams.DO <- "379, 1847, 2823, 2906, 4759"
myParams.T <- "408, 975, 2215, 2347, 11760"
# myParams.pH <- "391, 535, 286, 841, 1088, 1425, 4580, 11777, 11778"
myParams.SpC <- "492, 4571, 2253, 1848"

myParams <- paste(myParams.SpC, myParams.T, sep = ", ") # myParams.pH, myParams.DO, 

# Set the Date/Time Range
myStartdate <- "1950-01-01 00:00:00"
myEnddate <- "2018-08-20 00:00:00"

# Set data type
# myDatatype <- 2 # 0: both, 1: grab, 2: continuous 

# Query for grab data
myGrabQuery <- paste0("SELECT * FROM Result WHERE (Station in (",
                      myStations.2,")) AND (XLU_LASAR_PARAMETER in (",
                      myParams,")) AND (DATA_TYPE =", 
                      1,") AND (SAMPLE_DATE_TIME >='", 
                      myStartdate,"') AND (SAMPLE_DATE_TIME <='", 
                      myEnddate,"')")
siletzGrabData <- sqlQuery(channel, myGrabQuery, stringsAsFactors=FALSE, na.strings="NA")

# Query for continuous data
myContQuery <- paste0("SELECT * FROM Result WHERE (Station in (",
                      myStations.2,")) AND (XLU_LASAR_PARAMETER in (",
                      myParams,")) AND (DATA_TYPE =",
                      2,") AND (SAMPLE_DATE_TIME >='",
                      myStartdate,"') AND (SAMPLE_DATE_TIME <='",
                      myEnddate,"')")
siletzContData <- sqlQuery(channel, myContQuery, stringsAsFactors=FALSE, na.strings="NA")

# Combine the data
# Distills the parameter data.frame
myParamsQuery <- paste0("SELECT * FROM XLU_LASAR_PARAMETERS WHERE (XLU_LASAR_PARAMETERS_KEY in (",myParams,"))")
myParamsdata <- sqlQuery(channel, myParamsQuery, stringsAsFactors=FALSE, na.strings="NA")
close(channel)
myParamsdata <- myParamsdata[ -c(2 : 6, 8 : 9, 11 : 14)]

# Sets the station IDs for the lower siletz
estStations <- as.integer(c(11272, 11248, 32207, 11245, 13321, 13320, 13319, 31590))
# Reformats and merges the data to get a data frame with concurrent temper/cond measurements for the lower Siletz stations
siletzLASARData <- rbind(siletzGrabData, siletzContData)
Data <- siletzLASARData[ -c(1 : 5, 7, 8, 12 : 22, 24, 25, 28 : 31, 33, 34)]
Data <- Data[siletzLASARData$STATION %in% estStations, ]
Data <- Data[!duplicated(Data), ]
Data <- merge(Data, myParamsdata, by.x = "XLU_LASAR_PARAMETER", by.y = "XLU_LASAR_PARAMETERS_KEY")
Sal.1 <- Data[ -c(1, 4 : 7, 10)]
Sal.1$RESULT = as.numeric(Sal.1$RESULT)
Sal.2 <- dcast(Sal.1, SAMPLE_DATE_TIME ~ STATION + PARAMETER_NM, fun.aggregate = mean, value.var = "RESULT")
Sal.2 <- Sal.2[ -c(2 : 4, 11)]
x <- rowSums(is.na(Sal.2[ , 2 : 7]) == 0)
Sal.3 <- cbind(Sal.2, x)
Sal.3 <- Sal.3[(Sal.3$x != 0 & Sal.3$x %% 2 == 0), ]
Sal.3 <- Sal.3[ -8]
Sal.3 <- Sal.3[ -c(3, 5, 7)]
colnames(Sal.3) <- c("DATE", "Cond_13319", "Cond_13320", "Cond_13321")

# Convert conductivity to salinity - All measurements were in uS/cm @ 25oC
# Approximate function based on power relationship ax^b from 
a <- 0.4126296168
b <- 1.1180419533
Sal.3[ , 5 : 7] <- apply(Sal.3[, 2 : 4], 2, function(x) a * (x / 1000) ^ b)
Sal.3[ , 5 : 7] <- round(Sal.3[ , 5 : 7], digits = 2)

setnames(Sal.3, old = c("V5", "V6", "V7"), new = c("13319", "13320", "13321"))
Sal.4 <- Sal.3[ -c(2 : 4)]
Sal.4 <- melt(Sal.4, id.vars = "DATE", measure.vars = c("13319", "13320", "13321"), na.rm = TRUE)
colnames(Sal.4) <- c("DATE", "STAID", "Sal_ppt")
Sal.4$DATE <- as.POSIXct(Sal.4$DATE, "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")

lims.y <- c(0, 30)

box.Sal <- ggplot() + geom_boxplot(data = Sal.4, aes(x = STAID, y = Sal_ppt), outlier.shape = 1, outlier.size = 1) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = lims.y, breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    xlab("Station") + ylab("Salinity (ppt)") +
    geom_hline(yintercept = 10, color = "blue", size = 0.4, linetype = 2)

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\005_reporting\\figures\\"
ggsave(filename = "FigXX_Salinity_Estuary.jpg", plot = box.Sal, path = dir, width = 8, height = 8, units = "in", dpi = 300)




