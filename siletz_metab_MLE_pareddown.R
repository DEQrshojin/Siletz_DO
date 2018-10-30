# LOAD LIBRARIES ----
library(streamMetabolizer)
library(unitted)
library(plyr)
library(lubridate)
library(Rcpp)
library(RcppEigen)
library(ggplot2)

# INTRO SCRIPT MODIFY AND USE, ONLY NECESSARY FOR BAYES MODEL ----

x <- readRDS("\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\metabolism\\data.DO.metab.NAreplce.RData")
STAID <- unique(x$STAID)
STA <- as.character(STAID)
t_brk = as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
perc.comp = 0
ts = nrow(x)
metab.output.all <- list("Cold-water Period" = list(), "Spawning Period" = list())
print(paste0("Model started at ", Sys.time()))

for (i in 1 : length(STAID))
{
    # Cold-water
    df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ])
    perc.comp <- perc.comp + df.dim[1] / ts
    if (df.dim[1] == 0)
    {
        metab.output.all[[1]][[i]] = "No Data"
    } else {
        xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ]
        xsub <- xsub[ , -c(1, 3, 9)]
        xsub <- rename(xsub, c("DEP_CW" = "depth"))
        y <- metab(specs = specs(mm_name(type = 'mle')), data = xsub)
        metab.output.all[[1]][[i]] <- y
    }
    print(paste0("Station ", STA[i], ", Cold-water at ", Sys.time()," -- ", round(100 * perc.comp, 0), "% COMPLETE"))
    # Spawning
    df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ])
    perc.comp <- perc.comp + df.dim[1] / ts
    if (df.dim[1] == 0)
    {
        metab.output.all[[2]][[i]] = "No Data"
    } else {
        xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ]
        xsub <- xsub[ , -c(1, 3, 8)]
        xsub <- rename(xsub, c("DEP_SP" = "depth"))
        y <- metab(specs = specs(mm_name(type = 'mle')), data = xsub)
        metab.output.all[[2]][[i]] <- y
    }
    print(paste0("Station ", STA[i], ", Spawning at ", Sys.time()," -- ", round(100 * perc.comp, 0), "% COMPLETE"))
}

names(metab.output.all[[1]]) <- STA
names(metab.output.all[[2]]) <- STA
