# July 24, 2018
# R script to process Siletz River water quality monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

require(dplyr)
require(ggplot2)
library(tidyverse)
library(scales)
require(reshape2)
library(jpeg)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)

#-------------DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)-------------
#-------------------------------------------------------------------------------
dir <- ("\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\")
dir.sub1 <- ("001_data\\wq_data\\Monitoring 2017\\DEQ\\Grab_Samples\\")
dir.sub2 <- ("005_reporting\\figures")
data.wq <- read.csv(paste0(dir, dir.sub1, "deq_grabs_2017.csv"))
ylims <- read.csv(paste0(dir, dir.sub1, "ylims.csv"))
ylims$C1 <- gsub("_zz_", "\n", ylims$C1)
ylims$C2 <- gsub("_zz_", "\n", ylims$C2)
data.wq$DATE <- as.POSIXct(data.wq$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
par.ord <- c(1, 2, 3, 14, 15, 6, 7, 8, 9, 4, 5, 16, 17, 12, 13, 10, 11, 18, 19)
data.wq <- data.wq[par.ord]
piv.date <- as.POSIXct("09/01/2017 00:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
rear <- as.POSIXct(c("7/17/2017 00:00", "7/21/2017 00:00"), format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
spwn <- as.POSIXct(c("9/11/2017 00:00", "9/15/2017 00:00"), format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
#-------------DATA IMPORTED AND CLEANED-----------------------------------------
#-------------------------------------------------------------------------------
#-------------ADD ROW OF ND DATA FOR ALL STATIONS TO BOUTH PERIODS--------------
tmp1 <- data.wq[1, ]
for (i in 4 : 19) # puts the ND = -1 into the values and ND columns
{
    tmp1[1, i] <- -1
}
STAID <- unique(data.wq$STAID)
tmp2 <- tmp1
for (i in 2 : length(STAID)) # Creates a table that appends new rows of duplicate info for each station
{
    tmp1$STAID <- STAID[i]
    tmp2 <- rbind(tmp2, tmp1)
}
tmp3 <- tmp2
tmp3$DATE <- spwn[1] + 86400
data.wq <- rbind(data.wq, tmp2, tmp3)

#-------------GRAPH DATA--------------------------------------------------------
#-------------------------------------------------------------------------------
plts <- vector("list", 16) 
count <- 1
for (i in 1 : 2)
{   # 2 figures based on constituent; Fig 1 - TKN, NH3, NOx, CBOD; Fig 2 - PO4, TP, TOC, TSS
    for (j in  1 : 4)
    {   # 4 vertical panels per constituent per figure
        for (k in 1 : 2)
        {   # 2 horizontal panels per season
            # Create a temporary data.frame of the constituent on hand - Date, Station, Param, Param NDs
            data.var <- cbind(data.wq[, 1 : 3], data.wq[, count + 3], data.wq[, count + 4])
            names(data.var) <- c("DATE", "STAID", "GRP", "VAL", "ND")
            data.var <- na.omit(data.var) # remove NAs
            data.var$GRP <- as.factor(data.var$GRP)
            data.var$ND <- as.factor(data.var$ND)
            data.rear <- data.var[which(data.var$DATE < piv.date), ] # Create subset of data for rearing
            data.spwn <- data.var[which(data.var$DATE > piv.date), ] # Create subset of data for spawning

            # Plot for rearing and migration
            plts[[count]] <- ggplot() + geom_point(data = data.rear, aes(x = DATE, y = VAL, group = ND, shape = GRP),
                                                   size = 2) + 
                scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                ylab(ylims[j, 2 * i - 1]) +
                scale_x_datetime(breaks = date_breaks("1 days"), labels = date_format("%m/%d"), limits = rear) +
                scale_y_continuous(limits = c(0, ylims[j, i * 2])) +
                scale_shape_discrete(name  = "Detected/Not Detected",
                                     breaks = c("-1", "1"),
                                     labels = c("Not detected", "Detected")) +
                theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   axis.title.x = element_blank(),
                                   legend.position='none')
            # Plot for spawning
            plts[[count + 1]] <- ggplot() + geom_point(data = data.spwn, aes(x = DATE, y = VAL, group = STAID, color = STAID, shape = ND),
                                                   size = 2) + 
                scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                ylab(ylims[j, 2 * i - 1]) +
                scale_x_datetime(breaks = date_breaks("1 days"), labels = date_format("%m/%d"), limits = spwn) +
                scale_y_continuous(limits = c(0, ylims[j, i * 2])) +
                scale_shape_discrete(name  = "Detected/Not Detected",
                                     breaks = c("-1", "1"),
                                     labels = c("Not detected", "Detected")) +
                theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                                   legend.position='none')
            # Adds legend for Set 1 - In the second graph down (NH3)
            if ((i == 1) & (j == 2))
            {
                plts[[count]] <- plts[[count]] + theme(legend.position = c(0.5, 0.80),
                                                       legend.text = element_text(size = 6),
                                                       legend.title = element_text(size = 6),
                                                       legend.background = element_rect(fill="transparent")) +
                    guides(col = guide_legend(ncol = 5, title = "Station",
                                              title.position = "top"),
                           shape = FALSE)
            }
            # Adds legend for Set 2 - In the upper left graph (SRP)
            else if ((i == 2) & (j == 1))
            {
                plts[[count]] <- plts[[count]] + theme(legend.position = c(0.5, 0.80),
                                                       legend.text = element_text(size = 6),
                                                       legend.title = element_text(size = 6),
                                                       legend.background = element_rect(fill="transparent")) +
                    guides(col = guide_legend(ncol = 5, title = "Station",
                                              title.position = "top"),
                           shape = FALSE)
            }
        }
        # Start a new constituent (two columns over)
        count <- count + 2
    }
    # Sets the layout of the graphs, then saves
    x <- grid.arrange(plts[[8 * i - 7]], plts[[8 * i - 6]],
                      plts[[8 * i - 5]], plts[[8 * i - 4]],
                      plts[[8 * i - 3]], plts[[8 * i - 2]],
                      plts[[8 * i - 1]], plts[[8 * i]],
                      ncol = 2)
    ggsave(filename = paste0("test_", i, ".png"), plot = x, path = paste0(dir, dir.sub2), width = 8, height = 8, units = "in", dpi = 300)
}



# else if ((i == 1) & (j == 1))
# {
#     plts[[count]] <- plts[[count]] + theme(legend.position = c(0.5, 0.80),
#                                            legend.text = element_text(size = 6),
#                                            legend.title = element_text(size = 6),
#                                            legend.background = element_rect(fill="transparent")) +
#         guides(col = guide_legend(ncol = 5, title = "Detect/Non-detect",
#                                   title.position = "top"),
#                group = FALSE)
# }            else if ((i == 2) & (j == 2))
# {
#     plts[[count]] <- plts[[count]] + theme(legend.position = c(0.5, 0.80),
#                                            legend.text = element_text(size = 6),
#                                            legend.title = element_text(size = 6),
#                                            legend.background = element_rect(fill="transparent")) +
#         guides(col = guide_legend(ncol = 5, title = "Detect/Non-detect",
#                                   title.position = "top"),
#                group = FALSE)
# }