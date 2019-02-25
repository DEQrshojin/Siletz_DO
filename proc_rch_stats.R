# PROCESS REACH CHARACTERISTICS
proc_rch_stats <- function(rch, slopeMod, nMod, floodplainN) {
  
  rch <- rch[, c(3, 4, 15, 13, 16, 22, 19, 20, 21, 18)]
  
  names(rch) <- c('rch', 'dsc', 'abs', 'dx', 'S',
                  'n', 'ybf1', 'wbf1', 'xbk1', 'qbf1')
  
  # Dimensional measurements are in ft -- convert to m
  rch[, 7 : 8] <- round(rch[, 7 : 8] * 0.3048, 3)
  
  rch[, 9] <- round(rch[, 9] * 0.3048^2, 3)
  
  # bottom width
  rch$wbot <- rStt[2, 4] * rch$abs^rStt[3, 4] # Based on the regressions above
  
  # Set reach-based floodplain side slopes and break-depths
  rch$ss1 <- c(rep(slopeMod[1] * 0.50, 8),
               rep(slopeMod[2] * 0.42, 2),
               rep(slopeMod[3] * 0.30, 6))
  
  rch$ss2 <- c(rep(slopeMod[1] * 0.25, 8),
               rep(slopeMod[2] * 0.60, 2),
               rep(slopeMod[3] * 0.15, 6))
  
  rch$D1 <- c(rep(1.75, 8), rep(1.50, 2), rep(0.90, 6)) # Half the Fldpln chan 1
  
  rch$D2 <- c(rep(3.50, 8), rep(3.00, 2), rep(1.80, 6)) # Full Fldpln chan 1
  
  rch$D3 <- rch$ybf1 * 9 # Bankfull times 5 (don't include actual bankfull)
  
  rch$D4 <- rch$ybf1 * 49 # Bankfull times 10 (don't include actual bankfull)
  
  # modify manning's n: (reduction or increase factor)
  nmod <- c(rep(nMod[1], 2), rep(nMod[2], 6), rep(nMod[3], 8))
  
  for (i in 1 : nrow(rch)) {rch[i, 6] = rch[i, 6] * nmod[i]}
  
  rch$fpn <- c(rep(floodplainN[1], 2),
               rep(floodplainN[2], 6),
               rep(floodplainN[3], 8))
  
  return(rch)
  
}
