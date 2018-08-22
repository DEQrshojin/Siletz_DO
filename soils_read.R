
# SET FILE NAMES FOR READING DATA
dir1 = '\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\'
dir2 = 'C:/Users/rshojin/Desktop/002_GIS/002_geophysical/soils'
dir.sub1 = '/lincoln/OR638/tabular/'
dir.sub2 = '/polk/OR053/tabular/'

mu.siletz1 = paste0(dir1, 'soils_linc.txt')
mu.siletz2 = paste0(dir1, 'soils_polk.txt')
muagg.lnc <- paste0(dir2, dir.sub1, 'muaggatt.txt')
muagg.plk <- paste0(dir2, dir.sub2, 'muaggatt.txt')

# READ DATA
muagg <- rbind(read.delim(muagg.lnc, header = FALSE, sep = "|"), read.delim(muagg.plk, header = FALSE, sep = "|"))
col.rem <- c(3 : 15, 19 : 40)
muagg <- muagg[ -col.rem]
colnames(muagg) <- c("MUSYM", "DESC", "c1_DR", "C2_DR", "SOIL_CLASS") 

soils1 <- read.delim(mu.siletz1, header = TRUE, sep = ",") 
soils2 <- read.delim(mu.siletz2, header = TRUE, sep = ",")
soils.siletz <- rbind(soils1, soils2)
soils.siletz <- soils.siletz[ -c(1, 3, 5)]
col.rem <- c(3 : 15, 19 : 40)

soils.siletz.area <- dcast(soils.siletz, MUSYM ~ AREASYMBOL, fun.aggregate = sum)

x <- merge(soils.siletz.area, muagg)
