# July 24, 2018
# R script to process Siletz River water quality monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

# LOAD LIBRARIES------------------------------------------------------------------------------
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
        

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA)---------------------------------------
dir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"
dir.sub1 = "001_data\\wq_data\\Monitoring 2017\\DEQ\\Grab_Samples\\"
dir.sub2 = "005_reporting\\figures\\"
data.wq <- read.csv(paste0(dir, dir.sub1, "deq_grabs_2017.csv"))
ylims <- read.csv(paste0(dir, dir.sub1, "ylims.csv"))
ylims$C1 <- gsub("_zz_", "\n", ylims$C1)
ylims$C2 <- gsub("_zz_", "\n", ylims$C2)
data.wq$DATE <- as.POSIXct(data.wq$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
par.ord <- c(1, 2, 3, 14, 15, 6, 7, 8, 9, 4, 5, 12, 13, 16, 17, 10, 11, 18, 19)
data.wq <- data.wq[par.ord]
piv.date <- as.POSIXct("09/01/2017 00:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
rear <- as.POSIXct(c("7/17/2017 00:00", "7/21/2017 00:00"), format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
spwn <- as.POSIXct(c("9/11/2017 00:00", "9/15/2017 00:00"), format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
# ADD ROW OF ND DATA FOR ALL STATIONS FOR BOUTH PERIODS---------------------------------------
tmp1 <- data.wq[1, ]
tmp1[1, 4: 19] <- -1
STAID <- unique(data.wq$STAID)
tmp2 <- tmp1
for (i in 2 : length(STAID))
{
    tmp1$STAID <- STAID[i]
    tmp2 <- rbind(tmp2, tmp1)
}
tmp3 <- tmp2
tmp3$DATE <- spwn[1] + 86400
data.wq <- rbind(data.wq, tmp2, tmp3)
# GRAPH DATA----------------------------------------------------------------------------------
plts <- vector("list", 16) 
count <- 1
for (i in 1 : 2)
{   # 2 figures based on constituent; Fig 1 - TKN, NH3, NOx, CBOD; Fig 2 - PO4, TP, TOC, TSS
    for (j in  1 : 4)
    {   # 4 vertical panels per figure for each constituent in the set
        for (k in 1 : 2)
        {   
            # Create a constituent specific subset for plotting each variable---- 
            data.var <- cbind(data.wq[, 1 : 3], data.wq[, count + 3], data.wq[, count + 4])
            names(data.var) <- c("DATE", "STAID", "GRP", "VAL", "ND")
            data.var <- na.omit(data.var)
            data.var$STAID <- as.factor(data.var$STAID)
            data.var$ND <- as.factor(data.var$ND)
            data.var$STAID <- factor(data.var$STAID, levels(factor(data.var$STAID))[c(3, 5, 2, 7, 6, 9, 1, 4, 10, 8)])
            data.rear <- data.var[which(data.var$DATE < piv.date), ]
            data.spwn <- data.var[which(data.var$DATE > piv.date), ]
            # 2 horizontal panels per season----
            # This plot is for the Cold-water period----
            plts[[count]] <- ggplot() + geom_point(data = data.rear,
                                                   aes(x = DATE, y = VAL, group = STAID, color = STAID, shape = ND),
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
            # This plot is for the Spawning period----
            plts[[count + 1]] <- ggplot() + geom_point(data = data.spwn,
                                                       aes(x = DATE, y = VAL, group = STAID, color = STAID, shape = ND),
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
        }
        # Start a new constituent (two columns over)
        count <- count + 2
    }
    # Plots are arranged and output to a graphics file----
    # CREATE LEGEND TO FIT INTO THE BOTTOM OF THE GRAPHS AS A STAND ALONE PLOT (GROB)
    lims.blank <- as.POSIXct(c("1/1/2000 00:00", "1/1/2001 00:00"), format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
    u <- ggplot() + geom_point(data = data.spwn, aes(x = DATE, y = VAL, group = STAID, color = STAID, shape = ND),
                               size = 2) + scale_color_hue(h = c(0, 180), l = 85, c = 75) +
                    scale_x_datetime(limits = lims.blank) + 
                    scale_shape_discrete(name  = "Detected/Not Detected", breaks = c("-1", "1"), labels = c("Not detected", "Detected")) +
                    theme_bw() + 
                    theme(panel.grid = element_blank(), axis.title = element_blank(),
                          axis.text = element_blank(), axis.ticks = element_blank(),
                          legend.box = "horizontal", legend.position = "top", legend.title=element_blank())
    
    # Create layout matrix to include legend at the bottom
    layout <- rbind(c(1, 2),
                    c(3, 4),
                    c(5, 6),
                    c(7, 8),
                    c(9, 9))
    
    # Set plots in layout
    x <- grid.arrange(plts[[8 * i - 7]], plts[[8 * i - 6]],
                      plts[[8 * i - 5]], plts[[8 * i - 4]],
                      plts[[8 * i - 3]], plts[[8 * i - 2]],
                      plts[[8 * i - 1]], plts[[8 * i]],
                      u, layout_matrix = layout,
                      ncol = 2, nrow = 5,
                      heights = c(2, 2, 2, 2, 1))
    # Print to file
    ggsave(filename = paste0("fig0", i + 3, "_wq_deq_grabs_set", i, ".png"),
           plot = x, path = paste0(dir, dir.sub2), width = 8, height = 9,
           units = "in", dpi = 300)
}