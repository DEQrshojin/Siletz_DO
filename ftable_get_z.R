library(raster)

xpts <- shapefile('C:/siletz_old/scratch/SLZ_shp/xpts.csv')

demPth <- 'C:/siletz_old/scratch/SLZ_shp/dem_tif'

ext <- seq(1, 1515, 101)

demNme <- xpts@data[ext, 8]

demFil <- paste0(demPth, '/', demNme, '.tif')

sites <- unique(xpts@data$ste)

# Add elevation column to data 
xpts@data$z <- 0

for (i in 1 : length(sites)) {
  
  # load dem
  dem <- raster(demFil[i])
  
  # interrogate at pnts
  intInd <- which(xpts@data$ste == sites[i])
  
  tmp <- xpts@data[intInd, 4 : 5]
  
  xpts@data[intInd, 9] <- extract(dem, tmp, method = 'simple')

}

# PLot the cross sections!
library(ggplot2)

# add another column for distance
xpts@data$d <- rep(seq(-150, 150, 3), 15)

for (i in 1 : length(sites)) {

  plt <- ggplot(xpts@data[xpts@data$ste == sites[i], ], aes(x = d, y = z)) +
    geom_point() + geom_line() + ggtitle(sites[i])
  
  ggsave(paste0('floodplain_xsect_', sites[i], '.png'),
         plot = plt, 
         path = 'C:/siletz_old/scratch/xsects',
         width = 15, height = 10, units = 'in', dpi = 300)

}

write.csv(xpts@data, file = 'C:/siletz_old/scratch/SLZ_shp/xpts_z.csv',
          row.names = FALSE)


