# CREATE BOX PLOTS OF STREAM METABOLISM FROM STREAM METABOLIZER ----
# Ryan Shojinaga, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# Created 20 September 2018

# LOAD LIBRARIES ----
library(ggplot2)
library(xlsx)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)

# IMPORT DATA ----

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\metabolism"

fil <- "Metab_Output_MLE_all_20180920.RData"

x <- readRDS(paste0(dir, "\\", fil)) # This loads the result of the stream metaboliser analysis

Stations <- names(x[["Cold-water Period"]])

STAID <- as.numeric(Stations)

date.break <- as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

k600 = data.frame(matrix(data = NA, nrow = length(STAID), ncol = 3))

names(k600) = c("STAID", "CW", "SP")

k600$STAID = STAID

for (i in 1 : length(STAID))
{

    for (j in 1 : 2)
    {

        # Calculate means of daily rearration for each station for each period
        
        if (is.character(x[[j]][[i]]))
        {
            
            k600[i, j + 1] = NA
            
        } else {
            
            k600[i, j + 1] = mean(x[[j]][[i]]@fit[["K600.daily"]], na.rm = TRUE)        

        }
    }
}




