# CREATE FTABLE VALUES FOR EACH REACH (HSPF FTABLES ARE NO GOOD)
# Additional cross sectional geometry work can be founr in the following dirs:
# C:\siletz\sup\xsect.xlsx & C:\siletz\sup\floodplain_xpts_z.xlsx

# LOAD LIBRARIES ----
library(reshape2)
library(ggplot2)
library(stats)

# CALIBRATION PARAMETERS PARAMETERS ----
# THIS IS THE FTABLE PARAMETER SET!!!!__________________________________________
slpChn <- c(0.153, 0.153, 0.350) # Main channel side slope # For 

slpChn <- c(rep(slpChn[1], 8), rep(slpChn[2], 2), rep(slpChn[3], 6))

# Modification of fldpln s slope (Lower, Mid, Upper) ^ = steeper \| = shallower
slopeMod <- c(0.4, 1.0, 1.0) # (0.5, 0.42, 0.3) (0.25, 0.6, 0.15)

# Not n but multiplier of n = (0.0602, 0.0795, 0.0873) (Lower, Mid, Upper)
nMod <- c(0.5, 0.4, 1.2)

# Manning's n for floodplain areas
floodplainN <- c(0.07, 0.07, 0.12)
# THIS IS THE FTABLE PARAMETER SET!!!!__________________________________________

for (jj in 1) {

  # LOAD FUNCTIONS ----
  dir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
                'Middle_Siletz_River_1710020405/002_scripts/r/')
  
  scripts <- c('ftable_calibration.R',
               'proc_rch_stats.R',
               'basnArea_to_chanGeom.R')
  
  sapply(paste0(dir, scripts), source)
  
  # RELATE BASIN AREA TO CHANNEL GEOMETRY ----
  # Channel geometry data from 2017 surveys:
  names <- c('JPM', 'HHI', 'MNS', 'CDR', 'EUR', 'MLL')
  
  sites <- data.frame(bs_area = c(641.3, 532.3, 297.4, 35.7, 35.2, 32.0),
                      xs_area = c(268.7, 118.2, 74.4,  5.5, 18.3,  6.4),
                      depth   = c(2.260, 1.440, 2.28, 0.64,  0.8, 0.56),
                      top_wid = c(139.5, 110.30,  44.5, 10.0, 33.4, 17.2),
                      bot_wid = c(80.20, 45.490,  16.0, 5.44, 12.88, 3.6),
                      flow    = c(183.34, 60.82, 49.22, 5.91, 5.12, 8),
                      row.names = names, stringsAsFactors = FALSE)
  
  # Convert depth, top_wid & bot width from ft to m
  sites[, 3 : 5] <- round(sites[, 3 : 5] * 0.3048, 3)
  
  # Relate channel geometry to basin area
  rStt <- basnArea_to_chanGeom(sites)
  
  # PROCESS REACH CHARACTERISTICS ----
  rch <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/',
                         'Dissolved Oxygen/Middle_Siletz_River_1710020405/',
                         '001_data/HSPF_Basin_Reach_Statistics.csv'),
                  stringsAsFactors = FALSE)
  
  # Process reach data
  rch <- proc_rch_stats(rch, slopeMod, nMod, floodplainN)
  
  dptMlt <- c(0, 0, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0) # Depth multiplier (for bnkful)
  
  # CREATE FTABLE VALUES ----
  tmpDF <- rch[, c(1 : 2)]
  
  # zero depth, 2017 survey, mean, bankfull, floodplain 1, 2, 3 & 4
  tabRows <- c('zero', '2017', 'mean', 'bkfl', 'flp1', 'flp2', 'flp3', 'flp4')
  
  ftab <- list()
  
  for (i in 1 : 8) {
    
    ftab[[i]] <- tmpDF
    
    # Water depths
    if (i == 2) { # In the case of the 2017 data, use the regression
      
      ftab[[i]]$y <- rStt[2, 2] * rch$abs^rStt[3, 2]
      
    } else {
      
      ftab[[i]]$y <- dptMlt[i] * rch$ybf1
      
    }
    
    if (i <= 4) { # FTABLE VALUES FOR DEPTHS LESS THAN OR EQUAL TO ybf
      
      # Channel top width (m)
      ftab[[i]]$w <- rch$wbot + 2 * ftab[[i]]$y / slpChn
      
      # Cross-sectional area (m2)
      ftab[[i]]$x <- ftab[[i]]$y * (rch$wbot + ftab[[i]]$y / slpChn) 
  
      # Total reach surface area (ha)
      ftab[[i]]$SA <- rch$dx * ftab[[i]]$w / 10^4 
      
      # Total Reach volume (Mm3)
      ftab[[i]]$V <- rch$dx * ftab[[i]]$x / 10^6 
      
      # Wetted perimeter (m)
      ftab[[i]]$P <- rch$wbot + 2 * ftab[[i]]$y * sqrt(1 + (1 / slpChn)^2) 
      
      # Reach outflow (m3/s)
      ftab[[i]]$Q <- ftab[[i]]$x * (1 / rch$n) *
                     (ftab[[i]]$x / ftab[[i]]$P)^(2/3) * rch$S^0.5
      
      # Reach outflow (converted to cfs)
      ftab[[i]]$Qcfs <- ftab[[i]]$Q * 35.3147
  
    } else if (i == 5) { # FTABLE VALUES FOR FIRST FLOOD DEPTHS
  
      # Adjust channel depth
      ftab[[i]]$y <- ftab[[i]]$y + rch$D1
  
      # Channel top width (m)
      ftab[[i]]$w <- rch$wbot +                      # Channel bottom
                     2 * ftab[['bkfl']]$y / slpChn + # Bankfull side slope areas
                     2 * rch$D1 / rch$ss1            # Fldpln side slope areas
  
      # Cross-sectional area (m2)
      ftab[[i]]$x <- ftab[['bkfl']]$x + rch$D1 * (ftab[[i]]$w + rch$D1 / rch$ss1)
  
      # Total reach surface area (ha)
      ftab[[i]]$SA <- rch$dx * ftab[[i]]$w / 10^4
  
      # Total Reach volume (Mm3)
      ftab[[i]]$V <- rch$dx * ftab[[i]]$x / 10^6
  
      # Wetted perimeter (m)
      ftab[[i]]$P <- ftab[['bkfl']]$P + 2 * rch$D1 * sqrt(1 + (1 / rch$ss1)^2) 
  
      # Aggregated Mannings n
      nInt = (rch$n * ftab[['bkfl']]$P + rch$fpn * 2 * rch$D1 / rch$ss1) /
             (ftab[['bkfl']]$P + 2 * rch$D1 / rch$ss1)
  
      # Reach outflow (m3/s)
      ftab[[i]]$Q <- ftab[[i]]$x * (1 / nInt) *
                    (ftab[[i]]$x / ftab[[i]]$P)^(2/3) * rch$S^0.5
  
      # Reach outflow (converted to cfs)
      ftab[[i]]$Qcfs <- ftab[[i]]$Q * 35.3147
  
    } else if (i == 6) { # FTABLE VALUES FOR SECOND FLOOD DEPTH
      
      # Adjust channel depth
      ftab[[i]]$y <- ftab[[i]]$y + rch$D2
      
      # Channel top width (m)
      ftab[[i]]$w <- rch$wbot +                      # Channel bottom
                     2 * ftab[['bkfl']]$y / slpChn + # Bankfull side slope areas
                     2 * rch$D2 / rch$ss1            # Fldpln side slope areas
      
      # Cross-sectional area (m2)
      ftab[[i]]$x <- ftab[['bkfl']]$x + rch$D2 * (ftab[[i]]$w + rch$D2 / rch$ss1)
  
      # Total reach surface area (ha)
      ftab[[i]]$SA <- rch$dx * ftab[[i]]$w / 10^4
      
      # Total Reach volume (Mm3)
      ftab[[i]]$V <- rch$dx * ftab[[i]]$x / 10^6
      
      # Wetted perimeter (m)
      ftab[[i]]$P <- ftab[['bkfl']]$P + 2 * rch$D2 * sqrt(1 + (1 / rch$ss1)^2) 
      
      # Aggregated Mannings n
      nInt = (rch$n * ftab[['bkfl']]$P + rch$fpn * 2 * rch$D2 / rch$ss1) /
             (ftab[['bkfl']]$P + 2 * rch$D2 / rch$ss1)
      
      # Reach outflow (m3/s)
      ftab[[i]]$Q <- ftab[[i]]$x * (1 / nInt) *
                    (ftab[[i]]$x / ftab[[i]]$P)^(2/3) * rch$S^0.5
      
      # Reach outflow (converted to cfs)
      ftab[[i]]$Qcfs <- ftab[[i]]$Q * 35.3147
      
    } else if (i == 7) { # FTABLE VALUES FOR THIRD FLOOD DEPTH
      
      # Adjust channel depth
      ftab[[i]]$y <- ftab[[i]]$y + rch$D3
      
      bDep <- rch$D3 - rch$D2
      
      # Channel top width (m)
      ftab[[i]]$w <- rch$wbot +                      # Channel bottom
                     2 * ftab[['bkfl']]$y / slpChn + # Bankfull side slope areas
                     2 * rch$D2 / rch$ss1 +          # Fldpln side slope areas 1
                     2 * bDep / rch$ss2              # Fldpln side slope areas 2
  
      # Cross-sectional area (m2)
      ftab[[i]]$x <- ftab[['flp2']]$x + bDep * (ftab[['flp2']]$w + bDep / rch$ss2)
      
      # Total reach surface area (ha)
      ftab[[i]]$SA <- rch$dx * ftab[[i]]$w / 10^4
      
      # Total Reach volume (Mm3)
      ftab[[i]]$V <- rch$dx * ftab[[i]]$x / 10^6
      
      # Wetted perimeter (m)
      ftab[[i]]$P <- ftab[['flp2']]$P + 2 * bDep * sqrt(1 + (1 / rch$ss2)^2) 
      
      # Aggregated Mannings n
      nInt = (rch$n * ftab[['bkfl']]$P +
              rch$fpn * 2 * rch$D2 / rch$ss1 + 
              rch$fpn * 2 * bDep / rch$ss2) /
             (ftab[['bkfl']]$P + 2 * rch$D2 / rch$ss1 + 2 * bDep / rch$ss2)
      
      # Reach outflow (m3/s)
      ftab[[i]]$Q <- ftab[[i]]$x * (1 / nInt) *
                    (ftab[[i]]$x / ftab[[i]]$P)^(2/3) * rch$S^0.5
      
      # Reach outflow (converted to cfs)
      ftab[[i]]$Qcfs <- ftab[[i]]$Q * 35.3147
      
    } else if (i == 8) { # FTABLE VALUES FOR THIRD FLOOD DEPTH
      
      # Adjust channel depth
      ftab[[i]]$y <- ftab[[i]]$y + rch$D4
      
      bDep <- rch$D4 - rch$D2
      
      # Channel top width (m)
      ftab[[i]]$w <- rch$wbot +         # Channel bottom
        2 * ftab[['bkfl']]$y / slpChn + # Bankfull side slope areas
        2 * rch$D2 / rch$ss1 +          # Fldpln side slope areas 1
        2 * bDep / rch$ss2              # Fldpln side slope areas 2
      
      # Cross-sectional area (m2)
      ftab[[i]]$x <- ftab[['flp2']]$x + bDep * (ftab[['flp2']]$w + bDep / rch$ss2)
      
      # Total reach surface area (ha)
      ftab[[i]]$SA <- rch$dx * ftab[[i]]$w / 10^4
      
      # Total Reach volume (Mm3)
      ftab[[i]]$V <- rch$dx * ftab[[i]]$x / 10^6
      
      # Wetted perimeter (m)
      ftab[[i]]$P <- ftab[['flp2']]$P + 2 * bDep * sqrt(1 + (1 / rch$ss2)^2) 
      
      # Aggregated Mannings n
      nInt = (rch$n * ftab[['bkfl']]$P +
              rch$fpn * 2 * rch$D2 / rch$ss1 + 
              rch$fpn * 2 * bDep / rch$ss2) /
             (ftab[['bkfl']]$P + 2 * rch$D2 / rch$ss1 + 2 * bDep / rch$ss2)
      
      # Reach outflow (m3/s)
      ftab[[i]]$Q <- ftab[[i]]$x * (1 / nInt) *
        (ftab[[i]]$x / ftab[[i]]$P)^(2/3) * rch$S^0.5
      
      # Reach outflow (converted to cfs)
      ftab[[i]]$Qcfs <- ftab[[i]]$Q * 35.3147
      
    }
    
    ftab[[i]][, 3 : 10] <- round(ftab[[i]][, 3 : 10], 3)
    
    names(ftab)[i] <- tabRows[i]
  
  }
  
  # AGGREGATE INTO FTABLES ----
  ftables <- data.frame(matrix(ncol = 5, nrow = (nrow(rch) + 1) * length(ftab)))
  
  names(ftables) <- c('fName', 'stage', 'rchAr', 'rchVl', 'rchQO')
  
  # Set the processing order
  pOrd <- c(11, 12, 10, 16, 15, 9, 8, 7, 14, 13, 6, 5, 4, 3, 2, 1)
  
  count <- 1
  
  for (i in 1 : (nrow(rch) + 1)) {
    
    for (j in 1 : length(ftab)) {
      
      if (i != nrow(rch) + 1) {
        
        ftables[count, ] <- c(fName <- paste0('FTABLE ', i),
                              stage <- ftab[[j]][pOrd[i], 3],
                              rchAr <- ftab[[j]][pOrd[i], 6],
                              rchVl <- ftab[[j]][pOrd[i], 7],
                              rchQO <- ftab[[j]][pOrd[i], 9])
        
      } else { # No values for Basin 17 (Jaybird Ck)--duplicate from Basin 4
        
        ftables[count, ] = ftables[24 + j, ]
  
      }
  
      count <- count + 1
  
    } 
  }
  
  # Coerce to numeric
  ftables$stage <- as.numeric(ftables$stage)
  
  ftables$rchAr <- as.numeric(ftables$rchAr)
  
  ftables$rchVl <- as.numeric(ftables$rchVl)
  
  ftables$rchQO <- as.numeric(ftables$rchQO)
  
  ftables[129 : 136, 1] <- 'FTABLE 17'
  
  # Write to csv
  file <- 'C:/siletz/inputs/ftab.csv'

  write.csv(ftables, file = file, row.names = FALSE)

  comb <- ftable_calibration(ftables)
  
  View(comb)

}

