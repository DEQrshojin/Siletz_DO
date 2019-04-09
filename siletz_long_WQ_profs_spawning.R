# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

# LOAD PACKAGES----
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)

# DATA IMPORT AND AUDITING (REMOVE NA & D-QUALITY DATA) ----

# Import data from master data file

dir <- paste0("\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\",
              "Middle_Siletz_River_1710020405\\")

dir.sub1 <- paste0("001_data\\wq_data\\Monitoring 2017\\LSWCD\\",
                   "Lincoln_SWCD_SILETZ RIVER_06292017-01052018")

dir.sub2 <- "004_gis\\001_data\\001_shape\\sampling_locations"

dir.sub3 <- "005_reporting\\figures\\analysis_memo"

data.tmp <- read.csv(paste0(dir, dir.sub1, "\\siletz_volmon_cont_data.csv"))

data.tmp$DATE.TIME <- as.POSIXct(data.tmp$DATE.TIME,
                                 format = "%m/%d/%Y %H:%M",
                                 tz = "America/Los_Angeles")

STAID_vctr <- unique(data.tmp$STAID) # create a list of stations

data.tmp <- data.tmp[complete.cases(data.tmp[, 4:7]), ] # Remove NA

data.tmp <- data.tmp[which(data.tmp$STAID != 38941), ] # Remove Strome Pk 38941

data.tmp$STAID[data.tmp$STAID == 10391] <- 29287 # Replace 10391 with 29287

data.tmp <- data.tmp[ -c(6, 9)]

data.reord <- c(1 : 4, 6, 7, 5)

data.tmp <- data.tmp[data.reord]

# GET STATION LIST AND DISTANCES ----

stations <- read.csv(paste0(dir, dir.sub2, "\\cont_wq.csv"))

stations <- stations[-1, ] # Remove strome park

# MERGE THE TWO SETS TO ASSIGN RKM TO STATIONS ----

data.bxplt <- merge(data.tmp, stations, by.x = "STAID", by.y = "STAID")

data.bxplt <- data.bxplt[ -c(4, 9 : 12, 15)]

data.bxplt$STAID <- factor(data.bxplt$STAID,
                           levels(factor(data.bxplt$STAID))[c(12, 3, 6, 1,
                                                              5,  8, 9, 2,
                                                              4, 7, 11, 10)])

# PLOT DATA ----

seas <- "S"

parms <- c("DO_mgL", "DO_Sat", "TEMP_C")

box.Rock <- list()

box.Siletz <- list()

j <- 10

# MAKE GRAPHS ----

# PLOT DO CONCENTRATION (mg/L)

for (i in 1 : length(seas)) {

  box.Siletz[[i]] <- ggplot() +
                     geom_boxplot(data = subset(data.bxplt,
                                                (SEASON == seas[i] & River == "Siletz River")),
                                  aes(x = RKm, y = DO_mgL, group = RKm),
                                  outlier.shape = 1, outlier.size = 1) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", plot.title = element_text(hjust = 0.5),
                       axis.title.y = element_text(margin = margin(r = 9))) +
    scale_x_continuous(limits = c(20, 70), breaks = c(20, 30, 40, 50, 60, 70)) +
    scale_y_continuous(limits = c(6, 16), breaks = c(6, 8, 10, 12, 14)) +
    xlab("River Mile") + ylab("Dissolved Oxygen (mg/L)") + ggtitle("Siletz River") +
    annotate("text", x = stations$RKm, y = 13.5,
             label = paste0(stations$STAID, " - ", stations$Site_ID), angle = 90, size = 3,
             hjust = 0) + 
    annotate("text", x = 46.9, y = 6.5, label = "Rock Creek\nConfluence", size = 3) +
    geom_segment(aes(x = 46.9, y = 6.8, xend = 46.9, yend = 8.1),
                 color = "gray48", size = 1, arrow = arrow(length = unit(0.05, "inches"))) + 
    geom_hline(yintercept = 11, color = "blue", size = 0.4, linetype = 2) +
    annotate("text", 53, 10.85, label = "Spawning, 7d mean min,\n 11.0 mg/L",
             hjust = 0, vjust = 1, size = 3.0)

  box.Rock[[i]] <- ggplot() +
    geom_boxplot(data = subset(data.bxplt, (SEASON == seas[i] & River == "Rock Creek")),
                 aes(x = RKm, y = DO_mgL, group = RKm),
                 outlier.shape = 1, outlier.size = 1) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", plot.title = element_text(hjust = 0.5),
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank()) +
    scale_x_continuous(limits = c(-1, 3), breaks = c(0, 1, 2, 3)) +
    scale_y_continuous(limits = c(6, 16), breaks = c(6, 8, 10, 12, 14)) +
    annotate("text", x = stations$RKm, y = 13.5,
             label = paste0(stations$STAID, " - ", stations$Site_ID), angle = 90, size = 3,
             hjust = 0) + 
    xlab("River Mile") + ggtitle("Rock Creek") +
    geom_hline(yintercept = 11, color = "blue", size = 0.4, linetype = 2)

  x <- grid.arrange(box.Siletz[[i]], box.Rock[[i]], widths = c(8, 2))
  
  ggsave(paste0("fig", j, "_do_conc_bxplt_", seas[i], ".png"), plot = x,
         path = paste0(dir, dir.sub3), scale = 1, width = 10, height = 6,
         units = "in", dpi = 300)
  
  j = j + 2
}

# DO CONCENTRATION (% SAT)---------------------------
    
    for (i in 1 : length(seas))
    {
        box.Siletz[[i]] <- ggplot() +
            geom_boxplot(data = subset(data.bxplt, (SEASON == seas[i] & River == "Siletz River")),
                         aes(x = RKm, y = DO_Sat, group = RKm),
                         outlier.shape = 1, outlier.size = 1) +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                               legend.position = "none", plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(limits = c(20, 70), breaks = c(20, 30, 40, 50, 60, 70)) +
            scale_y_continuous(limits = c(50, 175), breaks = c(50, 75, 100, 125, 150)) +
            xlab("River Mile") + ylab("Dissolved Oxygen (% Saturation)") + ggtitle("Siletz River") +
            annotate("text", x = stations$RKm, y = 143.75,
                     label = paste0(stations$STAID, " - ",stations$Site_ID),
                     angle = 90, size = 3, hjust = 0) + 
            annotate("text", x = 46.9, y = 56.25, label = "Rock Creek\nConfluence", size = 3) +
            geom_segment(aes(x = 46.9, y = 60, xend = 46.9, yend = 90), color = "gray48", size = 1,
                         arrow = arrow(length = unit(0.05, "inches"))) +
            annotate("text", 53, 94, color = "black",
                     label = "Spawning Minimum,\n95 percent", hjust = 0, vjust = 1, size = 3) +
            geom_hline(yintercept = 95.0, color = "blue", size = 0.4, linetype = 2)
        
        box.Rock[[i]] <- ggplot() +
            geom_boxplot(data = subset(data.bxplt, (SEASON == seas[i] & River == "Rock Creek")),
                         aes(x = RKm, y = DO_Sat, group = RKm), 
                         outlier.shape = 1, outlier.size = 1) +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                               legend.position = "none", plot.title = element_text(hjust = 0.5),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank()) +
            scale_x_continuous(limits = c(-1, 3), breaks = c(0, 1, 2, 3)) +
            scale_y_continuous(limits = c(50, 175), breaks = c(50, 75, 100, 125, 150)) +
            xlab("River Mile") + ggtitle("Rock Creek") +
            annotate("text", x = stations$RKm, y = 143.75,
                     label = paste0(stations$STAID, " - ", stations$Site_ID),
                     angle = 90, size = 3, hjust = 0) +
            geom_hline(yintercept = 95.0, color = "blue", size = 0.4, linetype = 2)
        
        x <- grid.arrange(box.Siletz[[i]], box.Rock[[i]], widths = c(8, 2))
        
        ggsave(paste0("fig", j, "_do_sat_bxplt_", seas[i], ".png"), plot = x,
               path = paste0(dir, dir.sub3), scale = 1, width = 10, height = 6,
               units = "in", dpi = 300)
        
        j = j + 2
    }

# TEMPERATURE (degC)-----------------------------------
    for (i in 1 : length(seas))
    {
        box.Siletz[[i]] <- ggplot() +
            geom_boxplot(data = subset(data.bxplt, (SEASON == seas[i] & River == "Siletz River")),
                         aes(x = RKm, y = TEMP_C, group = RKm),
                         outlier.shape = 1, outlier.size = 1) +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                               legend.position = "none", plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(limits = c(20, 70), breaks = c(20, 30, 40, 50, 60, 70)) +
            scale_y_continuous(limits = c(0, 37.5), breaks = c(0, 5, 10, 15, 20, 25)) +
            xlab("River Mile") + ylab("Temperature (°C)") + ggtitle("Siletz River") +
            annotate("text", x = stations$RKm, y = 28.125,
                     label = paste0(stations$STAID, " - ", stations$Site_ID),
                     angle = 90, size = 3, hjust = 0) + 
            annotate("text", x = 46.9, y = 1.875, label = "Rock Creek\nConfluence", size = 3) +
            geom_segment(aes(x = 46.9, y = 3, xend = 46.9, yend = 10), color = "gray48", size = 1,
                         arrow = arrow(length = unit(0.05, "inches"))) +
            annotate("text", 53, 14.4, color = "black",
                     label = "Spawning 7DAD Maximum,\n13°C", hjust = 0, size = 3.0) +
            geom_hline(yintercept = 13, color = "blue", size = 0.4, linetype = 2)
        
        box.Rock[[i]] <- ggplot() +
            geom_boxplot(data = subset(data.bxplt, (SEASON == seas[i] & River == "Rock Creek")),
                         aes(x = RKm, y = TEMP_C, group = RKm),
                         outlier.shape = 1, outlier.size = 1) +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                               legend.position = "none", plot.title = element_text(hjust = 0.5),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank()) +
            scale_x_continuous(limits = c(-1, 3), breaks = c(0, 1, 2, 3)) +
            scale_y_continuous(limits = c(0, 37.5), breaks = c(0, 5, 10, 15, 20, 25)) +
            xlab("River Mile") + ggtitle("Rock Creek") +
            annotate("text", x = stations$RKm, y = 28.125,
                     label = paste0(stations$STAID, " - ", stations$Site_ID), angle = 90, size = 3,
                     hjust = 0) +
            geom_hline(yintercept = 13, color = "blue", size = 0.4, linetype = 2)
        
        x <- grid.arrange(box.Siletz[[i]], box.Rock[[i]], widths = c(8, 2))
        
        ggsave(paste0("fig", j, "_temp_bxplt_", seas[i], ".png"), plot = x,
               path = paste0(dir, dir.sub3), scale = 1, width = 10, height = 6,
               units = "in", dpi = 300)
        
        j = j + 2
        
    }
