library(RODBC)

HUC = "17100204"
str = "2000-01-01 00:00:00.000"
end = "2018-08-19 00:00:00.000"
parms = "Conductiviyy"
dir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\wq_data\\Monitoring 2017\\DEQ\\Grab_Samples\\estuary\\"
stations_huc <- read.csv(paste0(dir, "sta.list.est.csv"))
planArea = NULL

options(stringsAsFactors = FALSE)

#For testing
# library(sp)
# library(rgdal)
# library(rgeos)
# agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
# agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
# HUC <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'huc250k_a_or', verbose = FALSE)
# HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
# HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
# names(HUClist) <- agwqma$PlanName
# 
# planArea <- 'South Santiam'
# startDate <- "2000-03-01 00:00:00.000"
# endDate <- "2017-03-01 00:00:00.000"
# inParms <- c('Total Phosphorus')
# input <- data.frame(select = rep(planArea, 3), parms = inParms, dates = c(startDate, endDate, startDate))
# parms <- read.csv('AgWQMA_DataRetrieval/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#### Define Geographic Area using myArea from 01_DataQueryUI.R ####

if (is.null(planArea)) {
    myHUCs <- HUClist
} else if (grepl("[0-9].", planArea)) {
    myHUCs <- strsplit(planArea, split = " - ")[[1]][1]
} else {
    myHUCs <- HUClist[HUClist$PlanName == planArea,'HUC8']
}

elm <- odbcConnect('ELEMENT')

st <- stations_wbd[stations_wbd$HUC8 %in% myHUCs,]
st <- st[!duplicated(st$STATION_KEY),]

myStations <- paste(st$STATION_KEY,collapse="','")

#### Specify element names for inParms ####
qryParms <- c()
if (any(inParms == 'Temperature')) {
    qryParms <- c(qryParms, 'Temperature')
}
if (any(inParms == 'pH')) {
    qryParms <- c(qryParms, 'pH')
}
if (any(inParms == 'Bacteria')) {
    qryParms <- c(qryParms, c('E. Coli','Fecal Coliform','Enterococcus'))
}
if (any(inParms == 'Dissolved Oxygen')) {
    qryParms <- c(qryParms, c('Dissolved Oxygen','Dissolved oxygen saturation'))
}
if(any(inParms == 'Total Suspended Solids')) {
    qryParms<- c(qryParms, c('Total Suspended Solids'))
}
if(any(inParms == 'Total Phosphorus')) {
    qryParms <- c(qryParms, c('Phosphate, Total as P'))
}
if(any(inParms == 'Total Nitrogen')) {
    qryParms <- c(qryParms, c('Nitrogen', 'Total Nitrogen'))
}
if(any(inParms == 'Conductivity')) {
    qryParms <- c(qryParms, c('Conductivity', 'Specific Conductivity', 'Specific Conductance'))
}
qryParms <- paste(qryParms,collapse="','")
#### Restrict Matrix to surface water ####
siteType <- c("'River/Stream','Estuary','Ocean','Lake'")

#### Build query ####
qry <- paste0("SELECT * FROM Repo_Result WHERE Station_ID in ('",
              myStations,"') AND Analyte in ('", 
              qryParms, "') AND Matrix in (", 
              siteType, ") AND Sampled >= '", 
              startDate, "' AND Sampled <= '",
              endDate, "' AND DQL in ('A', 'B', 'C', 'E');")

#### Pass the query ####
myData <- sqlQuery(elm, qry)

myData <- merge(myData, st[,c('STATION_KEY','HUC8')], 
                by.x = 'Station_ID', by.y = 'STATION_KEY', all.x = TRUE)

odbcCloseAll()

