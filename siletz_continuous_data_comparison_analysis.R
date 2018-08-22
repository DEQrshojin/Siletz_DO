require(reshape2)
require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)
library(jpeg)
library(BiocInstaller)
library(grDevices)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)
library(lubridate)

# Set "global" variables
seas <- c("R", "S")
mon <- c("July", "September")
stn.N <- c("JackMorgan_36367", "SiletzUSGS_38918", "SiletzMoon_37396")
sites.old <- c("36367-JackMorgan", "38918-SiletzUSGS", "37396-SiletzMoon")
sites.new <- c("36367 - Siletz River\nat Jack Morgan Park", "38918 - Siletz River\nat USGS Station",
               "37396 - Siletz River at\nMoonshine Park")
parameter <- c("Dissolved Oxygen (mg/L)", "Dissolved Oxygen (% Sat)", "Temperature (?C)")
param <- c("DOCONC", "DOSAT", "TEMPC")

#---------------------------------READ DEQ DATA---------------------------------
#-------------------------------------------------------------------------------
# Set variables for reading and manipulating the data
options(stringsAsFactors = F)
data.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/wq/"
data.deq <- list(data.frame(), data.frame())
col.nms <- c("DT", "TM", "TMFR", "STN", "FC", "BTV", "CBV", "T_C", "CON", "DO_C", "DO_S", "pH", "TRB", "TSS", "D_M")

# Read in data files, combine and format
for (i in 1 : length(seas))
      {
      for (j in 1 : length(stn.N))
            {
            data.tmp <- read.csv(paste0(data.dir, "DEQ/", mon[i], "/", stn.N[j], "_", mon[i], "2017.csv"))
            names(data.tmp) <- col.nms
            data.deq[[i]] <- rbind(data.deq[[i]], data.tmp)
            # Rename Station
            data.deq[[i]]$STN <- gsub(sites.old[j], sites.new[j], data.deq[[i]]$STN)
            }
      # Combine date and time into one column
      data.deq[[i]]$DTTM <- as.POSIXct(paste0(data.deq[[i]]$DT, data.deq[[i]]$TM), format="%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")
      # Extract station ID
      data.deq[[i]]$STAID <- as.integer(substr(data.deq[[i]]$STN, 1, 5))
      # Delete extraneuos rows
      data.deq[[i]] <- data.deq[[i]][ c(16, 17, 4, 10, 11, 8)]
      data.deq[[i]]$Source <- "DEQ" # Add the source of the data to the table for factoring later
      }
data.deq[[2]]$DTTM <- data.deq[[2]]$DTTM + hours(7) # adjust time ahead 7 hours 
# Set temporal limits
time.frames <- read.csv(paste0(data.dir, "DEQ/deq_time_frames.csv"))
tf <- time.frames$DT <- as.POSIXct(time.frames$DT, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

#--------------------------------READ LSWCD DATA--------------------------------
#-------------------------------------------------------------------------------
# Import data from master data file
data.tmp1 <- read.csv(paste0(data.dir, "siletz_VolMon_data/siletz_volmon_cont_data.csv"))
data.tmp1$DATE.TIME <- as.POSIXct(data.tmp1$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
data.tmp1 <- data.tmp1[ -c(2, 6, 9)]
data.tmp1 <- data.tmp1[ c(1, 2, 3, 5, 6, 4)]
col.nms <- c("DTTM", "STAID", "STN", "DO_C", "DO_S", "T_C")
names(data.tmp1) <- col.nms
data.tmp1$Source <- "LSWCD"
data.tmp1$STN <- gsub("_zz_", "\n", data.tmp1$STN)
data.lswcd <- list(data.frame(), data.frame())
# Remove sites that aren't common to both monitoring periods
comm.sites <- unique(data.deq[[1]]$STAID)
data.tmp2 <- data.frame()
for (i in 1 : length(comm.sites))
      {
      block <- data.tmp1[which(data.tmp1$STAID == comm.sites[i]), ]
      data.tmp2 <- rbind(data.tmp2, block)
      }
# Remove observations that aren't common to both monitoring periods
for (i in 1 : 2)
      {
      data.lswcd[[i]] <- data.tmp2[which(data.tmp2$DTTM >= tf[2 * i - 1] & data.tmp2$DTTM <= tf[2 * i]), ]
      }

#-------------------------Recombine Data and Dcast------------------------------
#-------------------------------------------------------------------------------












# data.comb <- rbind(data.deq[[1]], data.deq[[2]],
#                    data.lswcd[[1]], data.lswcd[[2]])
# data.comb <- melt(data.comb, id.vars = c("DTTM", "Source", "STAID", "STN"), measure.vars = c("DO_C", "DO_S", "T_C"))
# data.comb <- dcast(data.comb, fun.aggregate = value, sep = "_", margins = NULL, subset = NULL, fill = NULL,
#                    drop = TRUE, value.var = guess(data), verbose = getOption("datatable.verbose"))
# 
# block <- list()
# d <- 1
# for (i in 1 : length(comm.sites))
#     {
#     for (j in 1 : length(seas))
#         {
#         block[[d]] <- rbind(data.deq[[j]][which(data.deq[[j]]$STAID == comm.sites[i]), ],
#                        data.lswcd[[j]][which(data.lswcd[[j]]$STAID == comm.sites[i]), ])
#         d <- d + 1
#         }
#     }