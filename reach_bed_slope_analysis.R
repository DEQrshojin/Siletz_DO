# Ryan Shojinaga, Oregon Department of Env Quality, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 23 August 2018
# Script to analyze DEM data for reach and slope data 

# LOAD LIBRARIES----
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(plyr)
library(ggplot2)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)
library(scales)
library(reshape2)

# READ IN FILE AND CLEAN UP ----
# _______________________________________________________________________________________________________________________
# file <- file.choose(new = F)
file <- "C:\\Users\\rshojin\\Desktop\\001_projects\\mid_coast_DO_tmdls\\GIS\\001_data\\shape\\siletz_rm_200m_TT_z12.shp"
nodes <- shapefile(file)
df <- nodes@data
df$FID_1 <- as.numeric(df$FID_1) # set the FID to numeric
df$facc <- as.numeric(df$facc)
df <- df[order(df$FID_1), ] # re-order based on the original file FID
row.names(df) <- 1 : nrow(df)
df$ENDPT <- ifelse(is.na(df$ENDPT), "INT", df$ENDPT) # Need to replace NAs in the ENDPT column with dummy ("INT") value

# ADJUST DF TO INCLUDE NEW COLUMNS FOR DISTANCE AND ELEVATION ----
# _______________________________________________________________________________________________________________________
# Adds columns for distance, elevation and cumulative components of the two
df$dx <- NA
df$dz <- NA
df$dxcum <- NA
df$dzcum <- NA
# Add column to indicate reach for each plot (e.g., Siletz, NF, Sunshine). The numbers indicate the plotting order
df$pltrch <- NA
df[which(df$Reach == "Siletz" & df$Desc_ != "North Fork" & df$Desc_ != "South Fork"), 17] <- 1   # Sets main stem as 1
df[which(df$Reach == "Siletz" & df$Desc_ == "South Fork"), 17] <- 3                              # Sets South Fork as 3
df[which(df$Reach == "Siletz" & df$Desc_ == "North Fork"), 17] <- 2                              # Sets North Fork as 2
df[which(df$Reach == "Sunshine Creek"), 17] <- 4                                                 # Sets Sunshine Ck as 4
df[which(df$Desc_ == "Big Rock Creek"), 17] <- 5                                                 # Sets B. Rock Ck as 5
df[which(df$Desc_ == "Little Rock Creek"), 17] <- 6                                              # Sets L. Rock Ck as 6
df$rivrch <- NA
for (i in 1 : nrow(df))
{
    if(df[i, 6] == "START")
    {
        # If the node is a reach "START" node, the change in distance and elevation are 0
        df[i, 13 : 16] = c(0, 0, 0, 0)
    }
    else
    {
        # All other nodes are used for the calculation of inst. and cumulative distances and elevations
        df[i, 13] <- round(sqrt((df[i, 7] - df[i - 1, 7])^2 +
                                    (df[i, 8] - df[i - 1, 8])^2), 1)        # Calculates the horizontal distance
        df[i, 14] <- round(df[i, 9] - df[i - 1, 9], 2)                      # Calculates the change in elevation
        df[i, 15] <- df[i - 1, 15] + df[i, 13]                              # Cumulative distance from the reach start 
        df[i, 16] <- df[i - 1, 16] + df[i, 14]                              # Cumulative elevation change from the reach start
    }
    
    # Calculate the river miles for each mainstem (e.g., Siletz, Sunshine, North Fork, etc.)
    if(i == 1) {df[i, 18] = 0}
    else
    {
        df[i, 18] = ifelse(df[i, 17] == df[i - 1, 17], round(df[i - 1, 18] + df[i, 13] / 1000 * 0.62137119, 3), 0)
    }
}
# Add 21.6 miles to the mainstem RMs to get at the actual river miles -- not applicable to the other reaches.
df[which(df[, 17] == 1), 18] <- df[which(df[, 17] == 1), 18] + 21.6

# FILL IN THE NON-SAMPLED WATERSHED AREA GAPS ----
# _______________________________________________________________________________________________________________________
file <- "C:\\Users\\rshojin\\Desktop\\001_projects\\mid_coast_DO_tmdls\\GIS\\001_data\\shape\\nodes_TT_missing_dirs7.shp"
gaps <- shapefile(file)
df.gaps <- gaps@data
df.gaps <- df.gaps[, -c(3 : 9)]
df.gaps$FID_1 <- as.factor(df.gaps$FID_1)
df.gaps$set <- as.factor(df.gaps$set)
df.gaps <- dcast(df.gaps, FID_1 ~ set)

df.gaps$east <- as.numeric(df.gaps$east) 
df.gaps$north <- as.numeric(df.gaps$north) 
df.gaps$orig <- as.numeric(df.gaps$orig) 
df.gaps$west <- as.numeric(df.gaps$west) 
df.gaps$south <- as.numeric(df.gaps$south) 

df.gaps$val <- 0
for (i in 1 : nrow(df.gaps))
{
    df.gaps[i, 7] <- max(df.gaps[i, 2 : 6])    
}
df.gaps <- df.gaps[, -c(2 : 6)]

# MERGE ORIGINAL TABLE AND GAPFILL TABLE AND FILL THE REMAINING MISSING VALUES ----
# _______________________________________________________________________________________________________________________

df$FID_F <- as.factor(df$FID_1)
df.fill <- merge(df, df.gaps, by.x = "FID_F", by.y = "FID_1", all.x = T)
df.fill$val <- ifelse(is.na(df.fill$val), df.fill$facc_sqkm, round(as.double(df.fill$val * 81 / 10763910.41671), 2))

# Delete extraneous columns
df.fill <- df.fill[, -c(1, 3, 4, 12, 13)]
# Reorder by column index
df.fill <- df.fill[ c(1, 13, 2, 3, 8, 4, 14, 5, 6, 7, 9, 10, 11, 12, 15)]
# Rename columns
colnames(df.fill) <- c("node", "plt.rch", "reach", "con.to", "site", "type", "rm",
                       "east", "north", "z", "dx", "dz", "dx.cum", "dz.cum", "ar.sqkm")
# Manually fill values that were 0 (NULL) after the gap-fill process (9 records)
man.fill = cbind(c(38, 124, 131, 241, 270, 272, 417, 602, 911),
                 c(4603596, 13948482, 13824551, 0, 16570062, 16523334, 0, 1904749, 492543))

df.fill$dummy <- df.fill$ar.sqkm

for (k in 1 : nrow(man.fill))
{
    df.fill[man.fill[k, 1] + 1, 16] <- man.fill[k, 2] * 81 / 10763910.41671
}

# THIS SECTIONS ADDRESSES UPSTREAM WATERSHED AREA ----
# _______________________________________________________________________________________________________________________

catch.size.px <- c(18447306, 18618972, 22182700, 23810585, 14742710) # Maximum flow accumulation values for each subwatershed
catch.size <- as.double(catch.size.px * 81 / 10763910.41671) # Converts each catchment size into square km
# catch.name <- c("lsiletz1", "lsiletz2", "msiletz", "usiletz", "rockck")
us.lw1 <- round(sum(catch.size[3 : 5]), 2) # catchment area of middle, upper siletz and rock ck.
us.lw2 <- round(sum(catch.size[c(1, 3 : 5)]), 2) # catchment area of lower 1, middle, upper siletz and rock ck.
us.mid <- round(catch.size[4], 2) # catchment area of upper siletz

# Add upstream sub-watershed areas
df.fill$dum2 <- 0
df.fill[1 : 39, 17] <- ifelse(df.fill[1 : 39, 15] == 0, # adds the area upstream of the lower2 boundary (at Euchre)
                              df.fill[1 : 39, 16] + us.lw2,
                              df.fill[1 : 39, 15] + us.lw2)
df.fill[40 : 242, 17] <- ifelse(df.fill[40 : 242, 15] == 0, # adds the area upstream of the lower1 boundary (at Rock Ck)
                                df.fill[40 : 242, 16] + us.lw1,
                                df.fill[40 : 242, 15] + us.lw1)
df.fill[243 : 418, 17] <- ifelse(df.fill[243 : 418, 15] == 0, # adds the area upstream of the lower1 boundary (at Rock Ck)
                                 df.fill[243 : 418, 16] + us.mid,
                                 df.fill[243 : 418, 15] + us.mid)
df.fill[419 : 930, 17] <- df.fill[419 : 930, 16] # adds the area upstream of the lower1 boundary (at Rock Ck)
df.fill[637, 17] <- us.lw1
df.fill <- df.fill[, -c(15 : 16)]
colnames(df.fill)[15] <- 'area.sqkm'
df.fill$area.sqkm <- round(df.fill$area.sqkm, 1)

# CALCULATE SUMMARY TABLE OF REACH LENGTHS, SLOPES, CHANNEL GEOMETRY AND CHARACTERISTICS ----
# _______________________________________________________________________________________________________________________

reach.stats <- df.fill[which(df.fill$type == "END"), ]
row.names(reach.stats) <- 1 : nrow(reach.stats)
reach.stats$grad <- round(reach.stats$dz.cum / (reach.stats$dx.cum / 0.3048), 5)
reach.stats[15 : 16, 4] <- c("Lower Sunshine Creek", "Upper Sunshine Creek")
upstream.areas <- reach.stats$con.to
reach.stats$area.sqkm <- as.numeric(c(694.9, 638.4, 583.1, 561.1, 529.9, 523.6, 457.3, 346.0,
                                      296.0, 267.0, 109.8, 69.2, 110.9, 52.9, 30.7, 17.3))

# CALCULATIONS OF MEAN BANKFUL WIDTH, DEPTH AND X-SECT AREA BASED ON AREA
# Kuck, T.D. 2000. Regional Hydraulic Geometry Curves of the South Umpqua Area in Southwestern Oregon. Stream Notes,
# Stream Systems Technology Center, Rocky Mountain Research Station, January 2000
reach.stats$area.sqmi = round(0.386102159 * reach.stats$area.sqkm, 1) # Convert area to square miles for channel geometry calcs
reach.stats$Q.bfk = round(44.8 * reach.stats$area.sqmi^0.918, 1) # Bankfull discharge (cfs), R2 = 0.85
reach.stats$D.bfk = round(0.9 * reach.stats$area.sqmi^0.389, 3) # Mean depth (feet), R2 = 0.97
reach.stats$W.bfk = round(11.5 * reach.stats$area.sqmi^0.419, 1) # Bankfull width (feet), R2 = 0.94
reach.stats$A.bfk = round(10.8 * reach.stats$area.sqmi^0.809, 1) # Bankfull area (square feet), R2 = 0.98

# CALCULATIONS OF MANNINGS N BASED ON MEASURED FLOWS AT JACK MORGAN, HH ILAHEE & MOONSHINE ----
# _______________________________________________________________________________________________________________________

fl1 <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data"
fl2 <- "\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018\\2017_Siletz_Flow_Q"
fls <- c("jmpark_T1.csv", "jmpark_T2.csv", "2017-09-14_SILETZ_RIVER_Hee_Hee_Ilahee_Park.csv", "2017-09-14_SILETZ_RIVER_Moonshine_Park.csv")

data.raw <- list()
names <- toupper(letters[1 : 14])
# Used node specific gradients for more realistic Manning's n values
summary <- as.data.frame(cbind(c("JMP_1", "JMP_2", "HHIP", "Moon"),
                 as.numeric(c(0.00041, 0.00041, 0.00089, 0.00100))))
names(summary) <- c("RCH", "S") #, "RM")
summary$Q <- 0
summary$A <- 0
summary$Rh <- 0

for (i in 1 : 4)
{
    data.raw[[i]] <- read.csv(paste0(fl1, fl2, "\\", fls[i]))
    summary[i, 3] <- dmm::unfactor(data.raw[[i]][grep("TOTAL DISCHARGE", data.raw[[i]][, 1]), 2])
    summary[i, 4] <- dmm::unfactor(data.raw[[i]][grep("TOTAL AREA", data.raw[[i]][, 1]), 2])
    summary[i, 5] <- dmm::unfactor(data.raw[[i]][grep("HYDRAULIC RADIUS", data.raw[[i]][, 1]), 2])
}

# Adjust the observed flow values from the DEQ measurements based on flow measurement--the USGS gage measured 68.5 cf
# at the date/time the DEQ measurements were made the USGS gage is 1.5 miles upstream of where the DEQ measurements
# were collected (Hee Hee Ilahee Park). Therefore all flow measurements have been adjusted by a factor of 1.13
summary$Qadj <- summary$Q * 1.13525
summary$S <- dmm::unfactor(summary$S)
summary$n <- (1.49 / summary$Qadj) * summary$A * summary$Rh ^ (2/3) * sqrt(summary$S)

# Populate the reach.stats df with Manning's n values from the summary df
reach.stats$n <- 0
reach.stats[1 : 2, 22] <- round(mean(summary[1 : 2, 7]), 5)
reach.stats[3 : 8, 22] <- round(summary[3, 7], 5)
reach.stats[9 : 16, 22] <- round(summary[4, 7], 5)

write.csv(reach.stats, paste0(fl1, "\\HSPF_Basin_Reach_Statistics.csv"), row.names = FALSE)

# # PLOT STREAM GRADIENTS ----
# _______________________________________________________________________________________________________________________
# prof <- list()
# 
# # SEt up labels, limits and breaks
# rch.name <- c("Siletz River\nMain Stem", "North Fork\nSiletz River", "South Fork\nSiletz River",
#               "Sunshine\nCreek", "Big Rock Creek", "Little Rock Creek")
# int.lbls <- df[complete.cases(df), c(1 : 2, 5 : 6, 9 : 10, 17 : 18)]
# val.fil <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\"
# slp.val <- read.csv(paste0(val.fil, "slp.lbls.csv"))
# 
# # Create the plots
# for (j in 1 : max(unique(df$pltrch)))
# {
#     # Subset the stream and station intersection labels
#     int.sub <- int.lbls[which(int.lbls$pltrch == j), ]
#     # Set limits and breaks
#     x.lims <- c(slp.val[j, 3], slp.val[j, 4])
#     brk <- seq(x.lims[1], x.lims[2], slp.val[j, 5])
#     ttl.ann.x <- ifelse(j == 1, 30, 2.75)
#     ttl.ann.y <- ifelse(j == 1, 2700, 2700)
#     # Make plots
#     prof[[j]] <- ggplot() + geom_line(data = subset(df, pltrch == j), aes(x = rivrch, y = z)) +
#                  geom_point(data = subset(int.lbls, pltrch == j), aes(x = rivrch, y = z), size = 1, color = "red") +
#                  # Axes designations
#                  ylab("Elevation (ft)") + xlab("River Mile") +
#                  scale_y_continuous(limits = c(0, 3000), breaks = c(0, 1000, 2000, 3000), expand = c(0, 0)) +
#                  scale_x_continuous(limits = x.lims, breaks = brk, expand = c(0.01, 0)) +
#                  theme_bw() + theme(legend.position = "none",
#                                     axis.title.x = element_text(size = 8),
#                                     axis.title.y = element_text(size = 8),
#                                     axis.text.x = element_text(size = 6),
#                                     axis.text.y = element_text(size = 6)) +
#                  # Set annotations
#                  annotate("text", x = ttl.ann.x, ttl.ann.y, size = 3, hjust = 0, vjust = 1,
#                           label = rch.name[j]) +
#                  annotate("text", x = int.sub$rivrch, y = int.sub$z + 300, size = 2, angle = 45, hjust = 0,
#                          label = int.sub$SITE2)
# }
# 
# # ARRANGE OVERALL PLOT AND SAVE FILE
# fil.path = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\005_reporting\\figures\\modeling_memo"
# 
# # Set empty grobs
# 
# # Use layout matrix???
# 
# # Arrange Layout
# x <- grid.arrange(prof[[1]], prof[[2]], prof[[3]], prof[[4]], prof[[5]], prof[[6]], ncol = 1)
# 
# ggsave("stream_gradient.png", plot = x, path = fil.path, scale = 1, width = 7.5, height = 30, units = "in", dpi = 300)

                 
# CALCULATE THE SLOPE BASED ON NAGEL 2010 -- ************** NOT USED ************** ----
# Nagel, D., Buffington, J.M., and Isaak, D. 2010. Estimating Stream Gradient Using
#   NHD Stream Lines and DEM Data. U.S. Forest Service, Rocky Mountain Research
#   Station Boise Aquatic Sciences Lab, Boise, ID. July 14, 2010
#   Class 1 - Cascade (> 0.075) use 200m (should be 160m)
#   Class 2 - Step-pool (0.03-0.075) use 200m (should be 230m)
#   Class 3 - Plane bed (0.015 - 0.03) use 600m (should be 540m)
#   Class 4 - Pool-riffle (< 0.015) use 800m (should be 810m)

# df.fill$grad <- 0
# # Calculate gradient for each node gradient calculated for the ith node based on the ith node
# # and the downstream (i - 1)th node.
# for (i in 1 : nrow(df.fill))
# {
#     df.fill[i, 16] <- ifelse(df.fill[i, 6] != "START", round(df.fill[i, 12] / (df.fill[i, 11] / 0.3048), 5), 0)
# }
# df.fill$grad <- format(df.fill$grad, scientific=FALSE)
# df.fill$grad <- as.numeric(df.fill$grad)
# 
# # df.fill$s.class <- NA
# 
# df.fill$s.class <- ifelse(df.fill$grad <= 0.015, "CLASS 4",
#                           ifelse(df.fill$grad <= 0.03, "CLASS 3",
#                                  ifelse(df.fill$grad <= 0.075, "CLASS 2", "CLASS 1")))
# df.c4 <- df.fill[which(df.fill[, 17] == "CLASS 4"), ]
