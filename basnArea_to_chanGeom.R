# RELATE CHANNEL GEOMETRY TO BASIN SIZE
basnArea_to_chanGeom <- function(sites) {
  
  # Log tranform all parameters:
  sitesLog <- log10(sites)
  
  vars <- names(sitesLog)
  
  sitesRegs <- sitesSumm <- list()
  
  rStt <- data.frame(matrix(data = 0,
                            nrow = 4,
                            ncol = (length(sitesLog) - 1)),
                     row.names = c('int', 'cef', 'slp', 'r2'))
  
  names(rStt) <- vars[2 : 6]
  
  for (i in 2 : length(sitesLog)) {
    
    sitesRegs[[i - 1]] <- lm(sitesLog[, i] ~ sitesLog[, 1], sitesLog)
    
    sitesSumm[[i - 1]] <- summary(sitesRegs[[i - 1]])
    
    # y = slp * area_basin^cef, where cef = 10^int
    rStt[, i - 1] <- c(sitesRegs[[i - 1]][['coefficients']][['(Intercept)']],
                       10^sitesRegs[[i - 1]][['coefficients']][['(Intercept)']],
                       sitesRegs[[i - 1]][['coefficients']][['sitesLog[, 1]']],
                       sitesSumm[[i - 1]][['r.squared']])
    
  }
  
  return(rStt)
  
}
