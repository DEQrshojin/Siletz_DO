ftable_calibration <- function(ftab) {
  
  names(ftab) <- c('TABLE', 'DP_m', 'SA_ha', 'VO_Mm3', 'QO_m3s')
  
  ftab$DP_ft <- ftab$DP_m / 0.3048
  
  ftab$QO_cfs <- ftab$QO_m3s * 35.3147
  
  ftab <- ftab[which(ftab$TABLE == 'FTABLE 4' | ftab$TABLE == 'FTABLE 11'),
               c(1, 6 : 7)]
  
  ftab <- ftab[c(3 : 5, 11 : 13), ]
  
  ftab[, 2] <- round(ftab[, 2], 2)
  
  ftab[, 3] <- round(ftab[, 3], 0)
  
  qGge <- data.frame('RCH' = c('SUN', 'SUN', 'SUN', 'SLZ', 'SLZ', 'SLZ'),
                     'MSR' = c('Mean', 'Bnkf', 'Fld1', 'Mean', 'Bnkf', 'Fld1'),
                     'STG' = c(1.51, 1.89, 4.84, 5.68, 7.10, 12.84),
                     'FLW' = c(25.0, 65.0, 1260, 2200, 3730, 12500))
  
  comb <- data.frame(cbind(ftab, qGge))
  
  comb$PBIAS <- round(100 * (comb$QO_cfs - comb$FLW) / comb$FLW, 1)
  
  return(comb)

}

