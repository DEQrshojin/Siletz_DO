# PROCESS RAW AWQMS DATA
proc_awqms <- function(wqDataRaw) {

  # Create unique, 6-digit ID to track entries (data to metadata)
  wqDatRaw$UNID <- unique_ID(nrow(wqDatRaw))
  
  # PARSE DATA ----
  # Metadata - Not used only for future reference if necessary
  wqMetaDat <- wqDatRaw[, c(1 : 4, 7 : 21, 25, 26, 28, 32 : 36, 40 : 53,
                            56 : 62, 64 : 68, 70, 71, 74 : 78)]
  
  # Columns of data for analysis
  wqDt1 <- wqDatRaw[, c(5, 6, 22 : 24, 27, 29 : 31, 37 : 39,
                        54, 55, 63, 69, 72, 73, 78)]
  
  # New names and name reference list
  wqDatNme <- cbind('org' = names(wqDt1),
                    'new' = c('stn', 'nme', 'dte', 'tme', 'tz', 'mth', 'par',
                              'spc', 'frc', 'val', 'opr', 'unt', 'stt', 'nvl', 
                              'dql', 'mdl', 'mrl', 'mun', 'uid'))
  
  names(wqDt1) <- wqDatNme[, 2]
  
  # PROCESS DATA ----
  # Dates
  wqDt1$tme <- ifelse(is.na(wqDt1$tme), '00:00:00',
                      gsub('.0000000', "", wqDt1$tme))
                      
  wqDt1$dt <- as.POSIXct(paste(wqDt1$dte, wqDt1$tme), '%Y-%m-%d %H:%M:%S',
                         tz = 'America/Los_Angeles')
  
  # QC: remove if val = NA, DQL = C/D, 
  cond <- which(is.na(wqDt1$val) | wqDt1$dql == 'DQL=C' | wqDt1$dql == 'DQL=D')
  
  wqDtDQ <- wqDt1[cond, ]
  
  wqDt1 <- wqDt1[-cond, ]
  
  # Rename parameters
  pars <- data.frame('old' = unique(wqDt1$par))
  
  pars$new <- c('DO', 'pH', 'Tmp', 'Turb', 'PO4', 'TP', 'NH3', 'BOD',
                'Chl-a', 'NOx', 'TKN', 'OrC', 'TSS', 'DOS', 'BOD')
  
  # Carbon and BOD will have to be isolated within their respective dfs
  
  wqDt1 <- merge(wqDt1, pars, by.x = 'par', by.y = 'old', all.x = TRUE)
  
  wqDt1BU <- wqDt1
  
  # Clean up and reorganize; This is the database to be used for WQ Analysis
  wqDt1 <- wqDt1[, c(19, 20, 2, 1, 21, 8, 9, 10, 11, 12, 16, 17, 13, 15, 3)]
  
  # Stations
  # Round station lat/lon to 4 sigdigs
  wqDatRaw[, 15 : 16] <- round(wqDatRaw[, 15 : 16], 4)
  
  wqStn <- wqDatRaw[, c(5, 6, 13, 15, 16)] 
  
  wqStn <- wqStn[!duplicated(wqStn), ]
  
  wqData <- list('data' = wqDt1,
                 'mtdt' = wqMetaDat,
                 'stns' = wqStn,
                 'wqdq' = wqDtDQ)
  
  return(wqData)
  
  # saveRDS(wqData, 'awqms_WQ_data.Rdata', ascii = FALSE)

}

# RETURNS A VECTOR OF UNIQUE, 6-DIGIT, RADNOMLY GENERATED, ALPHA-NUMERIC IDS
# OF LENGTH EQUAL TO THE NUMBER OF OBSERVATIONS IN THE DATAFRAME
unique_ID <- function(vectorLength) {

  vector = rep(NA, vectorLength)
  
  for (i in 1 : vectorLength) {
    
    repeat {
      
      tmp <- paste0(intToUtf8(round(runif(n = 1, min = 65, max = 90), 0)),
                    round(runif(n = 1, min = 0, max = 9), 0),
                    intToUtf8(round(runif(n = 1, min = 65, max = 90), 0)),
                    round(runif(n = 1, min = 0, max = 9), 0),
                    intToUtf8(round(runif(n = 1, min = 65, max = 90), 0)),
                    round(runif(n = 1, min = 0, max = 9), 0))
      
      if (!(tmp %in% vector)) {
        
        vector[i] <- tmp
        
        break
        
      }
    }  
  }
  
  return(vector)
  
}
