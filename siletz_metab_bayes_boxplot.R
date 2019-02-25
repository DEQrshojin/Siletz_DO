# CREATE BOX PLOTS OF STREAM METABOLISM FROM STREAM METABOLIZER ----
# Ryan Shojinaga, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# Created 20 September 2018

# LOAD LIBRARIES ----
library(ggplot2)
library(xlsx)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)

# IMPORT DATA ----

# dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\"
# 
# dir1 = "001_data\\metabolism"
# 
# svDir = "005_reporting\\figures\\analysis_memo"
# 
# fil <- "Metab_Output_Bayes.RData"
# 
# x <- readRDS(paste0(dir, dir1, "\\", fil)) # This loads the result of the stream metaboliser analysis

x = metab.output.all

Stations <- names(x[["Cold-water Period"]])

STAID <- as.numeric(Stations)

date.break <- as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

# CREATE A DUMMY ROW WITH DATA THAT WON'T APPEAR ON THE GRAPH ----

dummy <- x[["Cold-water Period"]][["11246"]]@fit[1, c(1 : 2, 5, 8, 14 : 16)]

dummy$STAID <- STAID[1]

bxplt.CW.df <- dummy

bxplt.CW.df <- bxplt.CW.df[-1, ]

bxplt.SP.df <- bxplt.CW.df

dummy[1, 1] <- date.break

dummy[1, 2 : 4] <- c(-99, 99, 99)

dummy[1, 5 : 8] <- "No Data"

# PULL OUT DATA FROM LIST AND ORGANIZE INTO DF FOR PLOTTING ----
for (i in 1 : length(STAID))
{
    # Cold-water

    if(class(x[["Cold-water Period"]][[i]]) == 'metab_mle')
    {

        xsub <- x[["Cold-water Period"]][[i]]@fit[ , c(1 : 2, 5, 8, 14 : 16)]
        
        xsub$STAID <- STAID[i]
        
        bxplt.CW.df <- rbind(bxplt.CW.df, xsub)
        
    } else {
        
        bxplt.CW.df <- rbind(bxplt.CW.df, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        bxplt.CW.df[nrow(bxplt.CW.df), ncol(bxplt.CW.df)] <- STAID[i]
        
    }
    
    # Spawning
    
    if(class(x[["Spawning Period"]][[i]]) == 'metab_mle')
    {
        
        xsub <- x[["Spawning Period"]][[i]]@fit[ , c(1 : 2, 5, 8, 14 : 16)]
        
        xsub$STAID <- STAID[i]
        
        bxplt.SP.df <- rbind(bxplt.SP.df, xsub)
        
    } else {
        
        bxplt.SP.df <- rbind(bxplt.SP.df, dummy) # INSERT DUMMY ROW HERE WITH BOGUS DATA
        
        bxplt.SP.df[nrow(bxplt.SP.df), ncol(bxplt.SP.df)] <- STAID[i]
    }
}

# ORGANIZE DATA: REMOVE ROWS W/ NA, REORDER COLUMNS, FACTORIZE STAID FOR PLOTTING & ----
# FIX NAs IN STATION 38912 

bxplt.CW.df <- bxplt.CW.df[complete.cases(bxplt.CW.df[, 2 : 4]), ] # Remove NA

bxplt.SP.df <- bxplt.SP.df[complete.cases(bxplt.SP.df[, 2 : 4]), ] # Remove NA

col.ord <- c(1 : 4, 8, 5 : 7) # New column order

bxplt.CW.df <- bxplt.CW.df[col.ord] # Reorder

bxplt.SP.df <- bxplt.SP.df[col.ord] # Reorder

STAID.ord <- c(12, 13, 3, 6, 2, 5, 8, 9, 1, 4, 7, 11, 10) # New station order (D/S -> U/S)

bxplt.CW.df$STAID <- factor(bxplt.CW.df$STAID,
                            levels(factor(bxplt.CW.df$STAID))[STAID.ord]) # Factorize & Order STAID

bxplt.SP.df$STAID <- factor(bxplt.SP.df$STAID,
                            levels(factor(bxplt.SP.df$STAID))[STAID.ord]) # Factorize & Order STAID

# CREATE BOX PLOTS ----
# Plot and save Cold-water period box plots (Top / bot = GPP / ER)
# Cold-water plots

box.CW.GPP <- ggplot() + theme_bw() +
    geom_boxplot(data = subset(bxplt.CW.df, STAID != 38941),
                 aes(x = STAID, y = GPP.daily, group = STAID),
                 outlier.shape = 1, outlier.size = 1) + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0))) +
    annotate("text", 2, 7, label = "GROSS PRIMARY\nPRODUCTION", hjust = 0, size = 4) +
    scale_y_continuous(limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) +
    ylab(bquote("Mean Daily GPP (g " *~O[2] ~m^-2 ~day^-1*")"))

box.CW.ER <- ggplot() +
    geom_boxplot(data = subset(bxplt.CW.df, STAID != 38941),
                 aes(x = STAID, y = ER.daily, group = STAID), outlier.shape = 1, outlier.size = 1) +
    theme_bw() + theme(legend.position = "none", plot.title = element_blank()) +
    scale_y_continuous(limits = c(-8, 0), breaks = c(-8, -6, -4, -2, 0)) +
    xlab("Station") + ylab(bquote("Mean Daily ER (g " *~O[2] ~m^-2 ~day^-1*")")) +
    annotate("text", 2, -7, label = "ECOSYSTEM\nRESPIRATION", hjust = 0, size = 4)

CW.per <- grid.arrange(box.CW.GPP, box.CW.ER, nrow = 2, widths = 6.5, heights = c(3.25, 3.25))

ggsave(filename = "fig22_boxplt_coldwater_DO_Metab_wDEPTH.png", plot = CW.per,
       path = paste0(dir, svDir), width = 6.5, height = 6.5, units = "in", dpi = 300)

# Spawning plots

box.SP.GPP <- ggplot() + theme_bw() +
    geom_boxplot(data = subset(bxplt.SP.df, STAID != 38941),
                 aes(x = STAID, y = GPP.daily, group = STAID),
                 outlier.shape = 1, outlier.size = 1) + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0))) +
    annotate("text", 2, 7, label = "GROSS PRIMARY\nPRODUCTION", hjust = 0, size = 4) +
    scale_y_continuous(limits = c(0, 8), breaks = c(0, 2, 4, 6, 8)) +
    ylab(bquote("Mean Daily GPP (g " *~O[2] ~m^-2 ~day^-1*")"))

box.SP.ER <- ggplot() +
    geom_boxplot(data = subset(bxplt.SP.df, STAID != 38941),
                 aes(x = STAID, y = ER.daily, group = STAID), outlier.shape = 1, outlier.size = 1) +
    theme_bw() + theme(legend.position = "none", plot.title = element_blank()) +
    scale_y_continuous(limits = c(-8, 0), breaks = c(-8, -6, -4, -2, 0)) +
    xlab("Station") + ylab(bquote("Mean Daily ER (g " *~O[2] ~m^-2 ~day^-1*")")) +
    annotate("text", 2, -7, label = "ECOSYSTEM\nRESPIRATION", hjust = 0, size = 4)

SP.per <- grid.arrange(box.SP.GPP, box.SP.ER, nrow = 2, widths = 6.5, heights = c(3.25, 3.25))

ggsave(filename = "fig23_boxplt_spawning_DO_Metab_wDEPTH.png", plot = SP.per,
       path = paste0(dir, svDir), width = 6.5, height = 6.5, units = "in", dpi = 300)

# Reaeration rate plots 

box.CW.k600 <- ggplot() + theme_bw() +
    geom_boxplot(data = subset(bxplt.CW.df, STAID != 38941),
                 aes(x = STAID, y = K600.daily, group = STAID),
                 outlier.shape = 1, outlier.size = 1) + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0))) +
    annotate("text", 2, 18, label = "Cold-Water Reaeration\nRates", hjust = 0, size = 4) +
    scale_y_continuous(limits = c(0, 20), breaks = c(0, 4, 8, 12, 16, 20)) +
    ylab(bquote("Mean Daily " *~K[600]* ", " *~day^-1 *")"))

box.SP.k600 <- ggplot() + theme_bw() +
    geom_boxplot(data = subset(bxplt.SP.df, STAID != 38941),
                 aes(x = STAID, y = K600.daily, group = STAID),
                 outlier.shape = 1, outlier.size = 1) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0))) +
    annotate("text", 2, 18, label = "Spawning Reaeration\nRates", hjust = 0, size = 4) +
    scale_y_continuous(limits = c(0, 20), breaks = c(0, 4, 8, 12, 16, 20)) +
    ylab(bquote("Mean Daily " *~K[600]* ", " *~day^-1 *")")) +
    xlab("Station")

K600.per <- grid.arrange(box.CW.k600, box.SP.k600, nrow = 2, widths = 6.5, heights = c(3.25, 3.25))

ggsave(filename = "fig24_boxplt_DO_metab_K600.png", plot = K600.per,
       path = paste0(dir, svDir), width = 6.5, height = 6.5, units = "in", dpi = 300)
