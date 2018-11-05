# CREATE BIVARIATE DENSITY DISTRIBUTIONS OF STREAM METABOLISM FROM STREAM METABOLIZER ----
# Ryan Shojinaga, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# Created 31 October 2018

# LOAD LIBRARIES ----
library(ggplot2)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)

# IMPORT DATA ----

dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"

dir1 = "001_data\\metabolism"

dir2 = "001_data\\wq_data\\Monitoring 2017\\LSWCD\\Lincoln_SWCD_SILETZ RIVER_06292017-01052018"

svDir = "005_reporting\\figures\\analysis_memo"

fil <- "Metab_Output_MLE_all_wDepth.RData"

# Load the result of the stream metaboliser analysis

x <- readRDS(paste0(dir, dir1, "\\", fil))

# The Cold-water period data for the Toldeo Intake STAID 37848 were included in the metabolism 
# analysis but are removed here because they did not meet the quality control requirements.

x[["Cold-water Period"]][["37848"]] = "No Data"

Stations <- names(x[["Cold-water Period"]])

STAID <- as.numeric(Stations)

dateBreak <- as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

# CREATE A DUMMY ROW WITH DATA THAT WON'T APPEAR ON THE GRAPH ----

dummy <- x[["Cold-water Period"]][["11246"]]@fit[1, c(1 : 2, 5, 8, 14 : 16)]

dummy$STAID <- STAID[1]

dummy$ratio = NA

densCWdf <- dummy

densCWdf <- densCWdf[-1, ]

densSPdf <- densCWdf

dummy[1, 1] <- dateBreak

dummy[1, 2 : 4] <- c(-99, 99, 99)

dummy[1, 5 : 8] <- "No Data"

# PULL OUT DATA FROM LIST AND ORGANIZE INTO DF FOR PLOTTING ----

for (i in 1 : length(STAID))
{
    # Cold-water
    
    if(class(x[["Cold-water Period"]][[i]]) == 'metab_mle')
    {
        
        xsub <- x[["Cold-water Period"]][[i]]@fit[ , c(1 : 2, 5, 8, 14 : 16)]
        
        xsub$STAID <- STAID[i] # Add station
        
        xsub$ratio = xsub$GPP.daily / abs(xsub$ER.daily) # Calculate GPP/ER ratio
        
        densCWdf <- rbind(densCWdf, xsub)
        
    } else {
        
        densCWdf <- rbind(densCWdf, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        densCWdf[nrow(densCWdf), ncol(densCWdf) - 1] <- STAID[i]
        
    }
    
    # Spawning
    
    if(class(x[["Spawning Period"]][[i]]) == 'metab_mle')
    {
        
        xsub <- x[["Spawning Period"]][[i]]@fit[ , c(1 : 2, 5, 8, 14 : 16)]
        
        xsub$STAID <- STAID[i] # Add station
        
        xsub$ratio = xsub$GPP.daily / abs(xsub$ER.daily) # Calculate GPP/ER ratio
        
        densSPdf <- rbind(densSPdf, xsub)
        
    } else {
        
        densSPdf <- rbind(densSPdf, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        densSPdf[nrow(densSPdf), ncol(densSPdf) - 1] <- STAID[i]
    }
}

densCWdf <- densCWdf[complete.cases(densCWdf[, 2 : 4]), ] # Remove NA

densSPdf <- densSPdf[complete.cases(densSPdf[, 2 : 4]), ] # Remove NA

col.ord <- c(1, 8, 2 : 4, 9, 5 : 7) # New column order

densCWdf <- densCWdf[col.ord] # Reorder

densSPdf <- densSPdf[col.ord] # Reorder

# Factorize & Order STAID

colOrder = c(12, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10)

densCWdf$STAID <- factor(densCWdf$STAID, levels(factor(densCWdf$STAID))[colOrder]) 

densSPdf$STAID <- factor(densSPdf$STAID, levels(factor(densSPdf$STAID))[colOrder])

densCWdf$SEAS = "CW"

densSPdf$SEAS = "SP"

ratioTab = rbind(densCWdf, densSPdf)

# Calculate tables of statistics for each parameter (GPP, ER, K600 and GPP/ER ratio)

ratioMed = dcast(ratioTab, STAID ~ SEAS, value.var = "ratio", median)

gppMed = dcast(ratioTab, STAID ~ SEAS, value.var = "GPP.daily", median)

erMed = dcast(ratioTab, STAID ~ SEAS, value.var = "ER.daily", median)

k600Med = dcast(ratioTab, STAID ~ SEAS, value.var = "K600.daily", median)

ratioMax = dcast(ratioTab, STAID ~ SEAS, value.var = "ratio", max)

gppMax = dcast(ratioTab, STAID ~ SEAS, value.var = "GPP.daily", max)

erMax = dcast(ratioTab, STAID ~ SEAS, value.var = "ER.daily", max)

k600Max = dcast(ratioTab, STAID ~ SEAS, value.var = "K600.daily", max)

ratioMean = dcast(ratioTab, STAID ~ SEAS, value.var = "ratio", mean)

gppMean = dcast(ratioTab, STAID ~ SEAS, value.var = "GPP.daily", mean)

erMean = dcast(ratioTab, STAID ~ SEAS, value.var = "ER.daily", mean)

k600Mean = dcast(ratioTab, STAID ~ SEAS, value.var = "K600.daily", mean)

# Try to determine seasonal changes (from Cold-water to Spawning)
# A positive value indicates the value decreased from CW to SP
# A negative value indicates the value increased from CW to SP

ratioMed$DIFF = round(ratioMed$CW - ratioMed$SP, 2)

gppMed$DIFF = round(gppMed$CW - gppMed$SP, 2)

erMed$DIFF = round(erMed$CW - erMed$SP, 2)

k600Med$DIFF = round(k600Med$CW - k600Med$SP, 2)

DiffMean = data.frame(cbind("STAID" = as.character(ratioMed$STAID),
                            "GPP" = gppMed$DIFF,
                            "ER" = erMed$DIFF,
                            "k600" = k600Med$DIFF,
                            "Ratio" = ratioMed$DIFF),
                      stringsAsFactors = FALSE)

DiffMean = DiffMean[-c(1, 2, 4, 5, 10 : 12), ]



# Plot GPP/ER ratio

# ratioPlotCW <- ggplot() + theme_bw() +
#     geom_boxplot(data = densCWdf, aes(x = STAID, y = ratio, group = STAID),
#                  outlier.shape = 1, outlier.size = 1) + 
#     theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
#           plot.title = element_text(hjust = 0.5)) +
#     annotate("text", 9, 1.75, label = "COLD-WATER\nPERIOD", hjust = 0, size = 3) +
#     scale_y_continuous(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
#     ylab("GPP/ER ratio")
# 
# ratioPlotSP <- ggplot() + theme_bw() +
#     geom_boxplot(data = densSPdf, aes(x = STAID, y = ratio, group = STAID),
#                  outlier.shape = 1, outlier.size = 1) + 
#     theme(plot.title = element_text(hjust = 0.5)) +
#     annotate("text", 9, 1.75, label = "SPAWNING\nPERIOD", hjust = 0, size = 3) +
#     scale_y_continuous(limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
#     ylab("GPP/ER ratio") + xlab("Station")
# 
# ratioGrid <- grid.arrange(ratioPlotCW, ratioPlotSP, nrow = 2,
#                           widths = 6.5, heights = c(3, 3.25))
# 
# ggsave(filename = "fig26_eco_metab_PR_ratio.png", plot = ratioGrid,
#        path = paste0(dir, svDir), width = 6.5, height = 6.5, units = "in", dpi = 300)
# 
# 
# 
# 
