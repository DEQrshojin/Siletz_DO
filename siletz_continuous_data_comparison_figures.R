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
# # Set temporal limits
# time.frames <- read.csv(paste0(data.dir, "DEQ/deq_time_frames.csv"))
# tf <- time.frames$DT <- as.POSIXct(time.frames$DT, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

#--------------------------------READ LSWCD DATA--------------------------------
#-------------------------------------------------------------------------------
# Import data from master data file
# data.tmp1 <- read.csv(paste0(data.dir, "siletz_VolMon_data/siletz_volmon_cont_data.csv"))
# data.tmp1$DATE.TIME <- as.POSIXct(data.tmp1$DATE.TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
# data.tmp1 <- data.tmp1[ -c(2, 6, 9)]
# data.tmp1 <- data.tmp1[ c(1, 2, 3, 5, 6, 4)]
# col.nms <- c("DTTM", "STAID", "STN", "DO_C", "DO_S", "T_C")
# names(data.tmp1) <- col.nms
# data.tmp1$Source <- "LSWCD"
# data.tmp1$STN <- gsub("_zz_", "\n", data.tmp1$STN)
# data.lswcd <- list(data.frame(), data.frame())
# # Remove sites that aren't common to both monitoring periods
# comm.sites <- unique(data.deq[[1]]$STAID)
# data.tmp2 <- data.frame()
# for (i in 1 : length(comm.sites))
#       {
#       block <- data.tmp1[which(data.tmp1$STAID == comm.sites[i]), ]
#       data.tmp2 <- rbind(data.tmp2, block)
#       }
# # Remove observations that aren't common to both monitoring periods
# for (i in 1 : 2)
#       {
#       data.lswcd[[i]] <- data.tmp2[which(data.tmp2$DTTM >= tf[2 * i - 1] & data.tmp2$DTTM <= tf[2 * i]), ]
#     }

# #---------------------------------PLOT ALL DATA---------------------------------
# #-------------------------------------------------------------------------------
# save.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/002_figures/comparisons"
# stp <- read.csv(paste0(save.dir, "/stp.csv"))
# lmt <- read.csv(paste0(save.dir, "/lmt.csv"))
# grp <- read.csv(paste0(save.dir, "/grp.csv"))
# grp$AnnX <- as.POSIXct(grp$AnnX, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
# u <- "_"
# d <- 1
# plt.grp <- list()
# for (i in 1 : length(comm.sites))
#     {
#     for (j in 1 : length(seas))
#         {
#         for (k in 1 : length(param))
#             {
#             block <- rbind(data.deq[[j]][which(data.deq[[j]]$STAID == comm.sites[i]), ],
#                            data.lswcd[[j]][which(data.lswcd[[j]]$STAID == comm.sites[i]), ])
            # #---------------------------------------START GRAPHING---------------------------------------
            # plt.grp[[d]] <- ggplot() + geom_point(data = block, aes(x = block[, 1], y = block[, k + 3], group = Source, color = Source),
            #                                  shape = 1, size = .5) +
            #                           ylab(parameter[k]) +
            #                           scale_x_datetime(limits = c(tf[2 * j - 1], tf[2 * j]),
            #                                            breaks=date_breaks("12 hours"), labels=date_format("%m/%d %H:%M")) +
            #                           scale_y_continuous(limits = lmt[, k],
            #                                            breaks = stp[, k]) +
            #                           theme_bw() + theme(panel.grid.major = element_blank(),
            #                                            panel.grid.minor = element_blank(),
            #                                            axis.title.x = element_blank(),
            #                                            axis.title.y = element_text(size = 8),
            #                                            legend.position = "none") +
            #                           geom_hline(yintercept=grp$Std[d], color = "blue", size = 0.4) +
            #                           annotate("text", grp$AnnX[d], grp$AnnY[d], color = "black", label = grp$Desc[d], hjust = 0, size = 3)
            #                           ifelse((k == 1 | k == 3),
            #                                 plt.grp[[d]] <- plt.grp[[d]] + theme(axis.text.y = element_text(margin = margin(r = 7))),
            #                                 NA)
            #                           if (k == 3)
            #                                 {
            #                                 # add x axes to graphs if the parameter is temperature
            #                                 plt.grp[[d]] <- plt.grp[[d]] + theme(axis.text.x = element_text(angle = 45, hjust = 1))
            #                                 } else
            #                                 {
            #                                 # remove the x axes from graphs if parameters are DOC or DOS
            #                                 plt.grp[[d]] <- plt.grp[[d]] + theme(axis.text.x = element_blank())
            #                                 }
            #                           if (j == 2)
            #                                 {
            #                                 # remove y axis from graphs is the season is spawning
            #                                 plt.grp[[d]] <- plt.grp[[d]] + theme(axis.title.y = element_blank(),
            #                                                            axis.text.y = element_blank(),
            #                                                            axis.ticks.y = element_blank())
            #                                 }
            #                           if (d == 6 | d == 12 | d == 18)
            #                                 {
            #                                 # add legend if based on where the legend should go (to come last)
            #                                 plt.grp[[d]] <- plt.grp[[d]] + theme(legend.position = c(0.5, 0.85), legend.box = "horizontal",
            #                                                            legend.text = element_text(size = 8),
            #                                                            legend.title = element_text(size = 8)) +
            #                                                      guides(col = guide_legend(nrow = 1, title = "Data Source",
            #                                                                                title.position = "left"))
            #                                 }
            # #----------------------------------------END GRAPHING----------------------------------------
            # # ggsave(filename = paste0(comm.sites[i], "_", seas[j], "_", param[k], ".jpg"), plot = plt.grp,
            # #        path = save.dir, width = grp$W[d], height = grp$H[d], units = "in", dpi = 300)
#             d <- d + 1
#             }
#         }
#     }
# 
# x <- grid.arrange(plt.grp[[1]], plt.grp[[4]],
#                   plt.grp[[2]], plt.grp[[5]],
#                   plt.grp[[3]], plt.grp[[6]],
#                   ncol = 2)
# ggsave(filename = "Fig19_sta36367_comp.png", plot = x, path = save.dir, width = 8, height = 8, units = "in", dpi = 300)
# 
# y <- grid.arrange(plt.grp[[7]], plt.grp[[10]],
#                   plt.grp[[8]], plt.grp[[11]],
#                   plt.grp[[9]], plt.grp[[12]],
#                   ncol = 2)
# ggsave(filename = "Fig20_sta38918_comp.png", plot = y, path = save.dir, width = 8, height = 8, units = "in", dpi = 300)
# 
# z <- grid.arrange(plt.grp[[13]], plt.grp[[16]],
#                   plt.grp[[14]], plt.grp[[17]],
#                   plt.grp[[15]], plt.grp[[18]],
#                   ncol = 2)
# ggsave(filename = "Fig21_sta37396_comp.png", plot = z, path = save.dir, width = 8, height = 8, units = "in", dpi = 300)
# 
