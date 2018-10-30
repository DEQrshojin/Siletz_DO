# PLOT DEQ CONTINUOUS WQ DATA FOR SILETZ RIVER 2017 DATA COLLECTION
# Ryan Shojinaga 06/22/2018
# Oregon Department of Environmental Quality
# 21 June 2018

# Base code for WQ plots of data sonde parameters for Siletz River

# Load libraries
library(tidyverse)
library(lubridate)
library(scales)
require(reshape2)
require(dplyr)
library(jpeg)
library(grDevices)
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)

# COLD-WATER PERIOD ----

# Set working directory to import data

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"

sub.dir1 <- "001_data\\wq_data\\Monitoring 2017\\DEQ\\July\\"

sub.dir2 <- "001_data\\wq_data\\Monitoring 2017\\DEQ\\September\\"

sub.dir3 <- "005_reporting\\figures\\analysis_memo\\"

# Read in data files, combine and format

options(stringsAsFactors = F)

st.36367.R <- read.csv(paste0(dir, sub.dir1, "JackMorgan_36367_July2017.csv"))

st.38918.R <- read.csv(paste0(dir, sub.dir1, "SiletzUSGS_38918_July2017.csv"))

st.37396.R <- read.csv(paste0(dir, sub.dir1, "SiletzMoon_37396_July2017.csv"))

data.R <- bind_rows(st.36367.R, st.38918.R, st.37396.R) # rbind files together

data.R$DATE_TIME <- as.POSIXct(paste0(data.R$Date..MM.DD.YYYY., data.R$Time..HH.MM.SS.),
                               format="%m/%d/%Y %H:%M:%S") - hours(7)

# Rename Columns and reorder

data.R <- data.R[ -c(1 : 3, 5 : 7)]

names(data.R) <- c("Station", "TempC", "SpCon", "DOmgL","DOSat",
                   "pH", "Turb", "TSSmgL", "Depm","DATE_TIME")

col.ord <- c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)

data.R <- data.R[col.ord]

# Replace turbidity values < 0 to = 0

data.R$Turb <- ifelse(data.R$Turb < 0, 0, data.R$Turb)

# Rename the values in the Station column

sites.old <- unique(data.R$Station)

sites.new <- c("36367 - Siletz River\nat Jack Morgan Park",
               "38918 - Siletz River\nat USGS Station",
               "37396 - Siletz River at\nMoonshine Park")

for (i in 1:length(sites.old))
{

    data.R$Station <- gsub(sites.old[i], sites.new[i], data.R$Station)

}

# Need to order factors for plotting

data.R$Station <- factor(data.R$Station, levels(factor(data.R$Station))[c(1, 3, 2)])

# Set time boundaries

lims.1 <- strptime("07/18/2017", "%m/%d/%Y", tz = "America/Los_Angeles") # Setting limit for plotting label

t.min <- as.POSIXct("07/17/2017 04:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

t.max <- as.POSIXct("07/20/2017 16:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

a.marg <- 8

b.marg <- 12

c.marg <- 16

# DO CONCENTRATION PLOT ----

doconc.R.plots <-  ggplot(data = data.R, aes(x = DATE_TIME, y = DOmgL,
                                             group = Station, color = Station)) + 
             geom_point(size = 1, shape = 1) + ylab("DO (mg/L)") + theme_bw() + 
             scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
             scale_y_continuous(limits = c(5, 13), breaks = c(5.0, 7.0, 9.0, 11.0, 13.0)) +
             theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                   legend.position = "none",
                   axis.title.y = element_text(size = 10, margin = margin(t = 0, r = b.marg, b = 0, l = 0)),
                   axis.title.x = element_blank(), axis.text.x = element_blank()) +
             geom_hline(yintercept = 8, color = "blue", size = 0.4, linetype = 2) +
             geom_hline(yintercept = 6.5, color = "green4", size = 0.4, linetype = 2) +
             geom_hline(yintercept = 6, color = "red4", size = 0.4, linetype = 2) +
             annotate("text", lims.1 - 0.7 * 86400 , 7.6,
                      label = "Cold-water 30d mean min, 8.0 mg/L", hjust = 0, size = 2.9) +
             annotate("text", lims.1 - 0.7 * 86400, 6.5,
                      label = "Cold-water 7d min mean, 6.5 mg/L", hjust = 0, size = 2.9) +
             annotate("text", lims.1 + 86400 * 1.2, 6,
                      label = "Cold-water absolute min, 6.0 mg/L", hjust = 0, size = 2.9) + 
             scale_color_manual(values=c("red", "blue", "lawngreen"))

# DO SATURATION PLOT ----

dosat.R.plots <- ggplot(data = data.R, aes(x = DATE_TIME, y = DOSat, group = Station, color = Station)) +
             geom_point(size = 1, shape = 1) + ylab("DO (% Sat)") + theme_bw() + 
             scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
             scale_y_continuous(limits = c(80, 130), breaks = c(80, 90, 100, 110, 120, 130)) +
             theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(), legend.position = "none", axis.title.y = element_text(size = 10, margin = margin(r = a.marg)),
                   axis.title.x = element_blank(), axis.text.x = element_blank()) +
             annotate("text", lims.1 + 86400 * 1, 86, color = "black", label = "Cold-water Minimum, 90 percent", hjust = 0, size = 2.9) +
             geom_hline(yintercept = 90.0, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))

# TEMPERATURE PLOT ----

temp.R.plots <-  ggplot(data = data.R, aes(x = DATE_TIME, y = TempC, group = Station, color = Station)) +
             geom_point(size = 1, shape = 1) + ylab("Temp (°C)") +
             scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
             scale_y_continuous(limits = c(10, 25), breaks = c(10, 15, 20, 25)) +
             theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(), 
                                legend.position = "none", axis.title.y = element_text(size = 10, margin = margin(t = 0, r = b.marg, b = 0, l = 0)),
                                axis.title.x = element_blank(), axis.text.x = element_blank()) +
             annotate("text", lims.1 + 86400 * 1, 15, color = "black",
                      label = "Cold-water 7DAD Maximum, 16°C", hjust = 0, size = 2.9) +
             geom_hline(yintercept = 16, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))

# PH PLOT ----

pH.R.plots <-  ggplot(data = data.R, aes(x = DATE_TIME, y = pH, group = Station, color = Station)) +
             geom_point(size = 1, shape = 1) + ylab("pH") +
             scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
             scale_y_continuous(limits = c(6, 9), breaks = c(6, 7, 8, 9)) +
             theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                legend.position = "none", axis.title.y = element_text(size = 10, margin = margin(t = 0, r = c.marg, b = 0, l = 0)),
                                axis.title.x = element_blank(), axis.text.x = element_blank()) +
             annotate("text", t.min + 86400 * 1, 8.75, color = "black",
             label = "pH criteria, 6.5 - 8.5", hjust = 0, size = 2.9) +
             geom_hline(yintercept = 6.5, color = "blue", size = 0.4, linetype = 2) + 
             geom_hline(yintercept = 8.5, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))

# CONDUCTIVITY PLOT ----

cond.R.plots <-  ggplot(data = data.R, aes(x = DATE_TIME, y = SpCon, group = Station, color = Station)) +
             geom_point(size = 1, shape = 1) + ylab("Cond (µS/cm)") +
             scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
             scale_y_continuous(limits = c(40, 80), breaks = c(40, 50, 60, 70, 80)) +
             theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                legend.position = "none", axis.title.y = element_text(size = 10, margin = margin(t = 0, r = b.marg, b = 0, l = 0)),
                                axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))

# TURBIDITY PLOT ----

turb.R.plots <-  ggplot(data = data.R, aes(x = DATE_TIME, y = Turb, group = Station, color = Station)) +
             geom_point(size = 1, shape = 1) + ylab("Turbidity (FNU)") +
             scale_x_datetime(limits = c(t.min, t.max),
                              breaks=date_breaks("12 hours"), labels=date_format("%m/%d %H:%M")) +
             scale_y_continuous(limits = c(0, 1000), breaks = c(0, 200, 400, 600, 800, 1000)) +
             theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                                legend.position = c(0.20, 0.55), axis.title.y = element_text(size = 10),
                                legend.title=element_blank(), axis.title.x = element_blank(),
                                axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
                                legend.text = element_text(size  = 8))  + 
    scale_color_manual(values=c("red", "blue", "lawngreen")) 

#__________________________________________________________________
# THIS SECTION DEALS WITH THE DATA COLLECTED IN THE SPAWNING PERIOD
#__________________________________________________________________

# Read in data files, combine and format
options(stringsAsFactors = F)
st.36367.S <- read.csv(paste0(dir, sub.dir2, "JackMorgan_36367_September2017.csv"))
st.38918.S <- read.csv(paste0(dir, sub.dir2, "SiletzUSGS_38918_September2017.csv"))
st.37396.S <- read.csv(paste0(dir, sub.dir2, "SiletzMoon_37396_September2017.csv"))
data.S <- bind_rows(st.36367.S, st.38918.S, st.37396.S) # rbind files together
data.S$Date_time <- as.POSIXct(data.S$Date_time, format="%m/%d/%Y %H:%M", tz = "America/Los_Angeles") - hours(7)

# Rename Columns and reorder
data.S <- data.S[ -c(1 : 3, 5 : 7, 17, 18)]
names(data.S) <- c("Station", "TempC", "SpCon", "DOmgL", "DOSat", "pH", "Turb", "TSSmgL", "Depm","DATE_TIME")
col.ord <- c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)
data.S <- data.S[col.ord]

# Replace turbidity values < 0 to = 0
data.S$Turb <- ifelse(data.S$Turb < 0, 0, data.S$Turb)

# Rename the values in the Station column
sites.old <- unique(data.S$Station)
sites.new <- c("36367 - Siletz River\nat Jack Morgan Park", "38918 - Siletz River\nat USGS Station",
               "37396 - Siletz River at\nMoonshine Park")
for (i in 1:length(sites.old)) {
    data.S$Station <- gsub(sites.old[i], sites.new[i], data.S$Station)
}
# Need to order factors for plotting
data.S$Station <- factor(data.S$Station, levels(factor(data.S$Station))[c(1, 3, 2)])

# Set time boundaries
lims.1 <- strptime("09/12/2017", "%m/%d/%Y", tz = "America/Los_Angeles") # Setting limit for plotting label
t.min <- as.POSIXct("09/11/2017 04:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
t.max <- as.POSIXct("09/14/2017 16:00", format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
a.mar <- 8
b.mar <- 12
c.mar <- 16

# DO CONCENTRATION PLOT--------------------------------------------------------------------------------------
doconc.S.plots <-  ggplot(data = data.S, aes(x = DATE_TIME, y = DOmgL, group = Station, color = Station)) + 
    geom_point(size = 1, shape = 1) + ylab("DO (mg/L)") +
    scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
    scale_y_continuous(limits = c(5, 13), breaks = c(5.0, 7.0, 9.0, 11.0, 13.0)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
    geom_hline(yintercept = 11, color = "blue", size = 0.4, linetype = 2) +
    annotate("text", lims.1 + 86400 * 1, 11.6, label = "Spawning 7d mean min, 11.0 mg/L", hjust = 0, size = 2.9) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))
# DO SATURATION PLOT   --------------------------------------------------------------------------------------
dosat.S.plots <- ggplot(data = data.S, aes(x = DATE_TIME, y = DOSat, group = Station, color = Station)) +
    geom_point(size = 1, shape = 1) + ylab("DO (% Sat)") +
    scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
    scale_y_continuous(limits = c(80, 130), breaks = c(80, 90, 100, 110, 120, 130)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
    annotate("text", lims.1 + 86400 * 1, 91, color = "black",
             label = "Spawning Minimum, 95 percent", hjust = 0, size = 2.9) +
    geom_hline(yintercept = 95.0, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))
# TEMPERATURE PLOT     --------------------------------------------------------------------------------------
temp.S.plots <-  ggplot(data = data.S, aes(x = DATE_TIME, y = TempC, group = Station, color = Station)) +
    geom_point(size = 1, shape = 1) + ylab("Temp (°C)") +
    scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
    scale_y_continuous(limits = c(10, 25), breaks = c(10, 15, 20, 25)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
    annotate("text", lims.1 + 86400 * 1, 12, color = "black",
             label = "Spawning 7DAD Maximum, 13°C", hjust = 0, size = 2.9) +
    geom_hline(yintercept = 13, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))
# PH PLOT              --------------------------------------------------------------------------------------
pH.S.plots <-  ggplot(data = data.S, aes(x = DATE_TIME, y = pH, group = Station, color = Station)) +
    geom_point(size = 1, shape = 1) + ylab("pH") +
    scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
    scale_y_continuous(limits = c(6, 9), breaks = c(6, 7, 8, 9)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    annotate("text", t.min + 86400 * 1, 8.75, color = "black",
             label = "pH criteria, 6.5 - 8.5", hjust = 0, size = 2.9) +
    geom_hline(yintercept = 6.5, color = "blue", size = 0.4, linetype = 2) + 
    geom_hline(yintercept = 8.5, color = "blue", size = 0.4, linetype = 2) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))
# CONDUCTIVITY PLOT    --------------------------------------------------------------------------------------
cond.S.plots <-  ggplot(data = data.S, aes(x = DATE_TIME, y = SpCon, group = Station, color = Station)) +
    geom_point(size = 1, shape = 1) + ylab("Cond (µS/cm)") +
    scale_x_datetime(limits = c(t.min, t.max), breaks=date_breaks("12 hours")) +
    scale_y_continuous(limits = c(40, 80), breaks = c(40, 50, 60, 70, 80)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))
# TURBIDITY PLOT       --------------------------------------------------------------------------------------
turb.S.plots <-  ggplot(data = data.S, aes(x = DATE_TIME, y = Turb, group = Station, color = Station)) +
    geom_point(size = 1, shape = 1) + ylab("Turbidity (FNU)") +
    scale_x_datetime(limits = c(t.min, t.max),
                     breaks=date_breaks("12 hours"), labels=date_format("%m/%d %H:%M")) +
    scale_y_continuous(limits = c(0, 1000), breaks = c(0, 200, 400, 600, 800, 1000)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
                       legend.position = "none", axis.title.y = element_blank(),
                       axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                       legend.title = element_blank(), axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + 
    scale_color_manual(values=c("red", "blue", "lawngreen"))

# THIS SECTION ARRANGES THE PLOTS AND OUTPUTS TO FILE -------------

x <- grid.arrange(doconc.R.plots, doconc.S.plots,  
                  dosat.R.plots, dosat.S.plots,
                  temp.R.plots, temp.S.plots,
                  pH.R.plots, pH.S.plots,
                  cond.R.plots, cond.S.plots,
                  turb.R.plots, turb.S.plots,
                  heights = c(rep(1.5, each = 5), 2.1),
                  widths = c(4.17, 3.83),
                  ncol = 2)

ggsave(filename = "Fig03_deq_cont_data_all_v2.png", plot = x, path = paste0(dir, sub.dir3),width = 9, height = 8, units = "in", dpi = 300)
