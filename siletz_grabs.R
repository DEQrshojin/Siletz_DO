# July 24, 2018
# R script to process Siletz River water quality monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

# LIBRARIES ----
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
library(RColorBrewer)
library(cowplot)
library(ggpubr)

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA) ----

dir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"

dir.sub1 = "001_data\\wq_data\\Monitoring 2017\\DEQ\\Grab_Samples\\"

dir.sub2 = "005_reporting\\figures\\analysis_memo"

data.wq <- read.csv(paste0(dir, dir.sub1, "deq_grabs_2017.csv"))

ylims <- read.csv(paste0(dir, dir.sub1, "ylims.csv"))

ylims$C1 <- gsub("_zz_", "\n", ylims$C1)

ylims$C2 <- gsub("_zz_", "\n", ylims$C2)

data.wq$DATE <- as.POSIXct(data.wq$DATE, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

x = names(data.wq)

par.ord <- c(1, 2, 3, 4, 15, 16, 7, 8, 9, 10, 5, 6, 13, 14, 17, 18, 11, 12, 19, 20)

# par.ord <- c(1, 2, 3, 14, 15, 6, 7, 8, 9, 4, 5, 12, 13, 16, 17, 10, 11, 18, 19)

data.wq <- data.wq[par.ord]

piv.date <- as.POSIXct("09/01/2017 00:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

rear <- as.POSIXct(c("7/17/2017 00:00", "7/21/2017 00:00"),
                   format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

spwn <- as.POSIXct(c("9/11/2017 00:00", "9/15/2017 00:00"),
                   format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

# ADD ROWS OF DATA FOR LEGEND DISPLAY OF ALL STATIONS FOR BOTH PERIODS ----

tmp1 <- data.wq[1, ]
tmp1 <- rbind(tmp1, tmp1)
tmp1[1, 4: 20] <- 1 # Detect Values
tmp1[2, 4: 20] <- -1 # ND Values
chngVals = c(seq(5, 20, 2))
tmp1[, chngVals] = -999
tmp1 <- rbind(tmp1, tmp1) # Duplicate to create same data for spawning
tmp1[3 : 4, 1] = as.POSIXct("9/13/2017 00:00", "%m/%d/%Y %H:%M",
                            tz = "America/Los_Angeles")

STAID <- unique(data.wq$STAID)

tmp2 <- tmp1

for (i in 2 : length(STAID))
{

    tmp1$STAID <- STAID[i]

    tmp2 <- rbind(tmp2, tmp1)

}

data.wq <- rbind(data.wq, tmp2)

detNotes = paste0("Solid points indicate values detected above the detection limit\n",
                  "Hollow points indicate values not detected")

# GRAPH DATA ----

plts <- vector("list", 16)

count <- 1

for (i in 1 : 2)
{   # 2 figures based on constituent; Fig 1 - TKN, NH3, NOx, CBOD; Fig 2 - PO4, TP, TOC, TSS
    
    for (j in  1 : 4)
    {   # 4 vertical panels per figure for each constituent in the set
        
        for (k in 1 : 2)
        {   

            data.var <- cbind(data.wq[, 1 : 3], data.wq[, count + 4], data.wq[, count + 5])

            names(data.var) <- c("DATE", "STAID", "GRP", "VAL", "ND")

            data.var <- na.omit(data.var) # remove NAs

            data.var$STAID <- as.factor(data.var$STAID)

            data.var$STAID <- factor(data.var$STAID,
                                     levels(factor(data.var$STAID))[c(3, 5, 2, 7, 6,
                                                                      9, 1, 4, 10, 8)])

            data.rear <- data.var[which(data.var$DATE < piv.date), ]

            data.spwn <- data.var[which(data.var$DATE > piv.date), ]
            
            shapeScale = rep(21 : 25, 2) # Fill for detects, hollow for ND
            
            det = subset(data.rear, ND == 1)
            
            ND = subset(data.rear, ND == -1)
            
            # Plot Cold-water
            
            plts[[count]] <- ggplot() + theme_bw() +
                geom_point(data = det, size = 3, 
                           aes(x = DATE, y = VAL, color = STAID, shape = STAID, group = STAID,
                               fill = STAID)) + # Fill for detected values
                geom_point(data = ND, size = 3, stroke = 1.1,
                           aes(x = DATE, y = VAL, color = STAID, shape = STAID, group = STAID)) +
                scale_fill_brewer(palette = "Spectral", aesthetics = "fill") +
                scale_colour_brewer(palette = "Spectral") + 
                scale_shape_manual(values = shapeScale) +
                scale_x_datetime(breaks = date_breaks("1 days"),
                                 labels = date_format("%m/%d"), limits = rear) +
                scale_y_continuous(limits = c(0, ylims[j, i * 2])) +
                ylab(ylims[j, 2 * i - 1]) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.title.x = element_blank(), legend.position = "none",
                      legend.text = element_text(size = 10)) +
                guides(col = guide_legend(ncol = 5))
            
            # Plot Spawning
            
            det = subset(data.spwn, ND == 1)
            
            ND = subset(data.spwn, ND == -1)
            
            plts[[count + 1]] <- ggplot() + theme_bw() + # REMEMBER TO REMOVE Y AXIS TITLE AND UNITS
                geom_point(data = det, size = 3, 
                           aes(x = DATE, y = VAL, color = STAID, shape = STAID, group = STAID,
                               fill = STAID)) + # Fill for detected values
                geom_point(data = ND, size = 3, stroke = 1.1,
                           aes(x = DATE, y = VAL, color = STAID, shape = STAID, group = STAID)) +
                scale_fill_brewer(palette = "Spectral", aesthetics = "fill") +
                scale_colour_brewer(palette = "Spectral") + 
                scale_shape_manual(values = shapeScale) +
                scale_x_datetime(breaks = date_breaks("1 days"),
                                 labels = date_format("%m/%d"), limits = spwn) +
                scale_y_continuous(limits = c(0, ylims[j, i * 2])) +
                ylab(ylims[j, 2 * i - 1]) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.title.x = element_blank(), axis.title.y = element_blank(),
                      axis.text.y = element_blank(), legend.position = "none",
                      legend.text = element_text(size = 10)) +
                guides(col = guide_legend(ncol = 5))
            
        }
        # Start a new constituent (two columns over)
        
        count <- count + 2
    }
    
    # Plots are arranged and output to a graphics file----
    # CREATE LEGEND TO FIT INTO THE BOTTOM OF THE GRAPHS AS A STAND ALONE PLOT (GROB)

    lims.blank <- as.POSIXct(c("1/1/2000 00:00", "1/1/2001 00:00"),
                             format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

    u <- ggplot() + theme_bw() + scale_x_datetime(limits = lims.blank) + 
        scale_colour_brewer(palette = "Spectral", guide = guide_legend(label.hjust = 1.5)) +
        scale_shape_manual(values = shapeScale) +
        scale_fill_brewer(palette = "Spectral", aesthetics = "fill") +
        geom_point(data = data.spwn,
                   aes(x = DATE,
                       y = VAL,
                       group = STAID,
                       color = STAID,
                       shape = STAID,
                       fill = STAID),
                   size = 2) +
        theme(panel.grid = element_blank(), panel.border = element_blank(),
              axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), legend.box = "horizontal",
              legend.title = element_blank(), legend.position = c(0, 1),
              legend.justification = c(0, 1), legend.direction = "horizontal",
              legend.key = element_rect(size = 5), legend.spacing.x = unit(0.15,"cm"),
              legend.key.size = unit(1.7, 'lines')) + 
        annotate("text", color = "black", label = detNotes, hjust = 0, size = 2.25)
    
    v = text_grob(detNotes, just = c(0.5, 0), color = "black", face = "plain", size = 8)

    # Create layout matrix to include legend at the bottom

    layout <- rbind(c(1, 2),
                    c(3, 4),
                    c(5, 6),
                    c(7, 8),
                    c(9, 10))

    # Set plots in layout

    x <- grid.arrange(plts[[8 * i - 7]], plts[[8 * i - 6]],
                      plts[[8 * i - 5]], plts[[8 * i - 4]],
                      plts[[8 * i - 3]], plts[[8 * i - 2]],
                      plts[[8 * i - 1]], plts[[8 * i]],
                      u, v,
                      layout_matrix = layout,
                      ncol = 2, nrow = 5,
                      heights = c(2, 2, 2, 2, 1),
                      widths = c(4.2, 3.8))
    # Print to file

    ggsave(filename = paste0("fig0", i + 3, "_wq_deq_grabs_set", i, "_v2.png"),
           plot = x, path = paste0(dir, dir.sub2), width = 8, height = 9,
           units = "in", dpi = 300)
}
