# CREATE BIVARIATE DENSITY DISTRIBUTIONS OF STREAM METABOLISM FROM STREAM METABOLIZER ----
# Ryan Shojinaga, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# Created 31 October 2018

# LOAD LIBRARIES ----
library(ggplot2)
library(xlsx)
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
        
        xsub$ratio = xsub$GPP.daily / abs(xsub$ER.daily) # Calculate GPP/ER ratio
        
        xsub$STAID <- STAID[i] # Add station
        
        densCWdf <- rbind(densCWdf, xsub)
        
    } else {
        
        densCWdf <- rbind(densCWdf, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        densCWdf[nrow(densCWdf), ncol(densCWdf)] <- STAID[i]
        
    }
    
    # Spawning
    
    if(class(x[["Spawning Period"]][[i]]) == 'metab_mle')
    {
        
        xsub <- x[["Spawning Period"]][[i]]@fit[ , c(1 : 2, 5, 8, 14 : 16)]
        
        xsub$ratio = xsub$GPP.daily / abs(xsub$ER.daily) # Calculate GPP/ER ratio
        
        xsub$STAID <- STAID[i] # Add station
        
        densSPdf <- rbind(densSPdf, xsub)
        
    } else {
        
        densSPdf <- rbind(densSPdf, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        densSPdf[nrow(densSPdf), ncol(densSPdf)] <- STAID[i]
    }
}

densCWdf <- densCWdf[complete.cases(densCWdf[, 2 : 4]), ] # Remove NA

densSPdf <- densSPdf[complete.cases(densSPdf[, 2 : 4]), ] # Remove NA

col.ord <- c(1 : 4, 9, 8, 5 : 7) # New column order

densCWdf <- densCWdf[col.ord] # Reorder

densSPdf <- densSPdf[col.ord] # Reorder

# Add column to give station name

sites = read.csv(paste0(dir, dir2, "\\sites_order_z.csv"))

sites = sites[-1, -c(2 : 6, 8, 9)]

sites$FULL_NAME = gsub("_zz_", " ", sites$FULL_NAME) 

densCWdf = densCWdf[, -9]

densCWdf = merge(densCWdf, sites, by.x = "STAID", by.y = "STATION")

densSPdf = merge(densSPdf, sites, by.x = "STAID", by.y = "STATION")

# Factorize & Order STAID

STAID.ord <- c(12, 13, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New station order (D/S -> U/S)

densCWdf$FULL_NAME <- factor(densCWdf$FULL_NAME,
                             levels(factor(densCWdf$FULL_NAME))[STAID.ord]) 

densSPdf$FULL_NAME <- factor(densSPdf$FULL_NAME,
                             levels(factor(densSPdf$FULL_NAME))[STAID.ord])

# CREATE DENSITY PLOTS ----

idLine = data.frame(cbind("x" = c(0, 6), "y" = c(0, -6)), stringsAsFactors = FALSE)

densCWPlot <- ggplot(data = densCWdf, aes(x = GPP.daily, y = ER.daily)) +
    theme_bw() + facet_wrap(~FULL_NAME, ncol = 3) + scale_fill_viridis_c() + 
    stat_density_2d(geom = "polygon", aes(fill = stat(level))) +
    scale_x_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) +
    scale_y_continuous(limits = c(-6, 0), breaks = c(-6, -4, -2, 0)) + 
    xlab(bquote("Mean Daily GPP (g " *~O[2] ~m^-2 ~day^-1*")")) +
    ylab(bquote("Mean Daily ER (g " *~O[2] ~m^-2 ~day^-1*")")) +
    theme(legend.position = "none", strip.text.x = element_text(size = 8)) + 
    geom_line(data = idLine, aes(x = x, y = y), color = "gray", size = 0.5)

ggsave(filename = "fig25_densplt_DOmetab_ColdWater.png", plot = densCWPlot,
       path = paste0(dir, svDir), width = 7.5, height = 9, units = "in", dpi = 300)
    
densSPPlot <- ggplot(data = densSPdf, aes(x = GPP.daily, y = ER.daily)) +
    theme_bw() + facet_wrap(~FULL_NAME, ncol = 3) + scale_fill_viridis_c() + 
    stat_density_2d(geom = "polygon", aes(fill = stat(level))) +
    scale_x_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) +
    scale_y_continuous(limits = c(-6, 0), breaks = c(-6, -4, -2, 0)) + 
    xlab(bquote("Mean Daily GPP (g " *~O[2] ~m^-2 ~day^-1*")")) +
    ylab(bquote("Mean Daily ER (g " *~O[2] ~m^-2 ~day^-1*")")) +
    theme(legend.position = "none", strip.text.x = element_text(size = 8)) + 
    geom_line(data = idLine, aes(x = x, y = y), color = "gray", size = 0.5)

ggsave(filename = "fig26_densplt_DOmetab_Spawning.png", plot = densSPPlot,
       path = paste0(dir, svDir), width = 7.5, height = 9, units = "in", dpi = 300)