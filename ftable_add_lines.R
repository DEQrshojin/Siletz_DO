library(ggplot2)
library(raster)

filPath <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
                  'Middle_Siletz_River_1710020405/001_data/')

# Select the river system ----
whichRiver <- 1 # 1 = Siletz, 2 = Big Elk Creek

if (whichRiver == 1) {

  # SILETZ
  fil <- paste0(filPath, 'ftables.csv')
  
  pNme <- 'Siletz'
  
} else if (whichRiver == 2) {

  # BIG ELK CREEK
  fil <- paste0(filPath, 'ftab_bec.csv')

  pNme <- 'Big_Elk_Creek'
  
} else {
  
  cat('Give a river, dude')
  
}

# Read in data ----
fTab <- read.csv(fil, stringsAsFactors = FALSE)

fNms <- c('basn', 'dpth', 'area', 'volm', 'flow')

names(fTab) <- fNms

# Loop through and create a list of the individual basin ftables ----
bsns <- unique(fTab$basn)

fTbs <- list()

for (i in 1 : length(bsns)) {
  
  # separate into individual data frames
  fTbs[[i]] <- fTab[fTab$basn == bsns[i], ]
  
  # check if depth[1] = 0, if not, create new row
  if (fTbs[[i]][1, 2] != 0) {

    newRow <- data.frame(basn = bsns[i],
                         dpth = 0,
                         area = fTbs[[i]][1, 3],
                         volm = 0,
                         flow = 0,
                         stringsAsFactors = FALSE)

    fTbs[[i]] <- rbind(fTbs[[i]], newRow)
    
    fTbs[[i]] <- fTbs[[i]][order(fTbs[[i]]$dpth),]
  }
  
  if(fTbs[[i]][1, 3] != 0) {fTbs[[i]][1, 3] = 0}
  
}

# Reform ftables with missing or zeroed values ----
fTab = NULL

for (i in 1 : length(fTbs)) {fTab = rbind(fTab, fTbs[[i]])}

plot_ftables(fTab, filPath, pNme)

write.csv(fTab, file = fil, append = FALSE,
          row.names = FALSE, col.names = FALSE)

# Function for plotting the ftable parameters vs. depth ----
plot_ftables <- function(fTab = NULL,     # data to graph
                         svDIR = NULL,    # directort for saving figures
                         pNme = NULL) {   # name of the graphs
  
  fTab$basn <- as.numeric(gsub('FTABLE ', '', fTab$basn))
  
  fNms <- names(fTab)
  
  for(i in 3 : 5) {
    
    dvParPlt <- ggplot(data = fTab,
                       aes(x = fTab[, 2], y = fTab[, i], group = fTab[, 1])) +
      geom_line() + geom_point(size = 1.5) + theme_bw() + facet_wrap(~basn) + 
      scale_x_log10() + scale_y_log10()
    
    filNme <- paste0(pNme, '_', fNms[i], '.png')
    
    ggsave(filNme, plot = dvParPlt, path = filPath, width = 15, height = 10,
           units = 'in', dpi = 300)
    
  }  
}
