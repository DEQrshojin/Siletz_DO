# LIBRARIES ----
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(reshape2)
library(xlsx)

# READ FILES
xls.dir <- '\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\soils'
shp.soils <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/shape/siletz_soils.shp"
dat.soils <- paste0(xls.dir, "\\", "final_soils_reimp.csv")

shp.soils <- shapefile(shp.soils)
dat.soils <- read.csv(dat.soils)

shp.soils.merge <- merge(shp.soils, dat.soils, by = "CODE_1")

shp.soils.2 <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/shape/siletz_soils2.shp"
shapefile(shp.soils.merge, shp.soils.2, overwrite = TRUE)

