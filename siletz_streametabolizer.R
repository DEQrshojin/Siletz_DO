# Script metadata ----
# July 2, 2018
# R script to process Siletz River continuous monitoring data
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# Adapted from WISE data processing by Dan Sobota

# LOAD LIBRARIES ----
library(streamMetabolizer)
library(unitted)
library(plyr)
library(lubridate)
library(Rcpp)
library(RcppEigen)
library(ggplot2)

# INTRO SCRIPT MODIFY AND USE, ONLY NECESSARY FOR BAYES MODEL ----
dir.out <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\metabolism\\"
dir.out2 <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\metabolism"
x <- readRDS(paste0(dir.out, "data.DO.metab.NAreplce.RData")) # This data set is the modified data frame with NA values (C, D or no data)
                                                              # And includes the PAR data
# interpolated from surrounding values.
STAID <- unique(x$STAID)
STA <- as.character(STAID)
# log.out <- paste0(dir.out, "log.out") # Log file to write to (Only use for Bayesian model)
t_brk = as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles") # Break for Cold-water (CW) and Spawning (SP) periods
# Set up variables
perc.comp = 0
ts = nrow(x)
metab.output.all <- list(list(), list())
plts_CW <- list()
plts_SP <- list()

# metab.all <- NULL
# met_CW <- list()
# met_SP <- list()

# Start of counter to report progress 
time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
print(paste0("Model started at ", time.stamp))
# cat(paste0("Model started at ", time.stamp, "\n"), file = log.out, append = FALSE)
sta.ord <- c("I", "E", "C", "J", "F", "D", "K", "G", "H", "M", "L", "A", "B") # Order for plotting Downstream => Upstream

# RUN METABOLIZER FOR GPP, ER, AND REAERATION USING MAXIMUM LIKELIHOOD ESTIMATION. DON'T NEED TO RUN -- LOAD DATA FROM: ----
# metab_data_all_MLE_dataframe.RData in dir.out (see below)
for (i in 1 : length(STAID))
{
    # Cold-water period calculations
    df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ])
    perc.comp <- perc.comp + df.dim[1] / ts
    if (df.dim[1] == 0)
    {
        met_CW[[i]] = "No Data"
        metab.output.all[[1]][[i]] = "No Data"
    } else {
        xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ]
        xsub <- xsub[ , -c(1, 3, 9)]
        xsub <- rename(xsub, c("DEP_CW" = "depth"))
        y <- metab(specs = specs(mm_name(type = 'mle')), data = xsub)
        metab.output.all[[1]][[i]] <- y
        # tmp.df <- as.data.frame(cbind(y@metab_daily, y@fit[["K600.daily"]], y@fit[["K600.daily.sd"]]))
        # tmp.df$STAID <- STAID[i]
        # names(tmp.df) <- c("date", "GPP", "GPP.L", "GPP.U", "ER", "ER.L", "ER.U", "msg", "wrn", "err", "K600", "k600.sd", "STAID")
        # tmp.df <- tmp.df[c(1, 13, 2, 5, 11, 3, 4, 6, 7, 12, 8, 9, 10)] # re-arrange the columns
        # metab.all <- rbind(metab.all, tmp.df) # combine the temporary data frame with the final output data framw
        # met_CW[[i]] <- tmp.df
        # plts_CW[[i]] <- plot_metab_preds(y, y_lim = list(GPP = c(0, 8), ER = c(-8, 0)))
        # ggsave(paste0("metab_MLE_CW_", sta.ord[i], "_STA_", STA[i], ".png"),
        #        plot = plts_CW[[i]], path = dir.out2, width = 6.5, height = 6.5, units = "in", dpi = 300)
    }
    time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
    print(paste0("Completed Station ", STA[i], " for Cold-water period at ", time.stamp,
                 " -- ", round(100 * perc.comp, 0), "% COMPLETE"))
    names(met_CW)[i] <- STA[i]
    
    # Spawning period calculations
    df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ])
    perc.comp <- perc.comp + df.dim[1] / ts
    if (df.dim[1] == 0)
    {
        met_SP[[i]] = "No Data"
        metab.output.all[[1]][[i]] = "No Data"
    } else {
        xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ]
        xsub <- xsub[ , -c(1, 3, 8)]
        xsub <- rename(xsub, c("DEP_SP" = "depth"))
        y <- metab(specs = specs(mm_name(type = 'mle')), data = xsub)
        metab.output.all[[2]][[i]] <- y
        # tmp.df <- as.data.frame(cbind(y@metab_daily, y@fit[["K600.daily"]], y@fit[["K600.daily.sd"]]))
        # tmp.df$STAID <- STAID[i]
        # names(tmp.df) <- c("date", "GPP", "GPP.L", "GPP.U", "ER", "ER.L", "ER.U", "msg", "wrn", "err", "K600", "k600.sd", "STAID")
        # tmp.df <- tmp.df[c(1, 13, 2, 5, 11, 3, 4, 6, 7, 12, 8, 9, 10)] # re-arrange the columns
        # metab.all <- rbind(metab.all, tmp.df) # combine the temporary data frame with the final output data framw
        # met_SP[[i]] <- tmp.df
        # plts_SP[[i]] <- plot_metab_preds(y, y_lim = list(GPP = c(0, 8), ER = c(-8, 0)))
        # ggsave(paste0("metab_MLE_SP_", sta.ord[i], "_STA_", STA[i], ".png"),
        #        plot = plts_SP[[i]], path = dir.out2, width = 6.5, height = 6.5, units = "in", dpi = 300)
    }
    time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
    print(paste0("Completed Station ", STA[i], " for Spawning period at ", time.stamp,
                 " -- ", round(100 * perc.comp, 0), "% COMPLETE"))
}

names(metab.output.all) <- c("Cold-water Period", "Spawning Period")
names(metab.output.all[[1]])

# PULL IN DATA, CLEAN UP, MODIFY FOR PLOTTING ----

# metab.all <- readRDS(paste0(dir.out, "metab_data_all_MLE_dataframe_noNA.RData"))

# BOX PLOTS ----

# add a dummy line to enable inclusion of stations without seasonal data (e.g., 11246 during Cold-water period)
# for (j in 1 : length(STAID))
# {
#     i = 
#     dummy <- metab.all[1, ]
#     
#     
#     metab.all <- rbing(metab.all, dummy)    
# # }
# 
# metab.all$STAID <- factor(metab.all$STAID, levels(factor(metab.all$STAID))[c(12, 13, 3, 6, 2, 5, 8, 9, 1, 4, 11, 10)]) # Factorize & Order STAID
# 
# # Plot and save Cold-water period box plots (Top / bot = GPP / ER)
# box.CW.GPP <- ggplot() + geom_boxplot(data = subset(metab.all, date < t_brk),
#                                       aes(x = STAID, y = GPP, group = STAID),
#                                       outlier.shape = 1, outlier.size = 1) +
#     theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
#     scale_y_continuous(limits = c(0, 8), breaks = c(2, 4, 6, 8)) +
#     xlab("Station") + ggtitle("GPP (g/m2/day)")
# 
# ggsave(paste0("metab_MLE_CW_Boxplot.png"), plot = box.CW.GPP, path = dir.out2, width = 10, height = 7.5, units = "in", dpi = 300)
# 
# # Plot and save Spawning period box plots (Top / bot = GPP / ER)
# box.SP.GPP <- ggplot() + geom_boxplot(data = subset(metab.all, date > t_brk),
#                                       aes(x = STAID, y = GPP, group = STAID),
#                                       outlier.shape = 1, outlier.size = 1) +
#     theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
#     scale_y_continuous(limits = c(0, 8), breaks = c(2, 4, 6, 8)) +
#     xlab("Station") + ggtitle("GPP (g/m2/day)")
# 
# ggsave(paste0("metab_MLE_SP_Boxplot.png"), plot = box.SP.GPP, path = dir.out2, width = 10, height = 7.5, units = "in", dpi = 300)

# RUN METABOLIZER FOR GPP, ER, AND REAERATION USING BAYESIAN METHOD WITH PARTIAL POOLING ***** WORKED ON 9/18/18 ***** ----
# ____(THIS ONE WORKED 9/18/18) ______________________________________________________________________________________________________________
# dir.out <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\metabolism\\"
# t_brk = as.POSIXct("9/1/2017 00:00", "%m/%d/%Y %H:%M", tz = "America/Los_Angeles") # Break date for Cold-water (CW) and Spawning (SP) periods
# met_CW <- list()
# met_SP <- list()
# perc.comp = 0
# time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
# print(paste0("Model started at ", time.stamp))
# for (i in 1 : length(STAID))
# {
#     # Cold-water period calculations
#     df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE < t_brk), ])
#     perc.comp <- perc.comp + df.dim[1] / nrow(ts)
#     if (df.dim[1] == 0)
#     {
#         met_CW[[i]] = "No Data"
#     } else {
#         xsub <- x[which(x$STAID == STAID[i] & x$DATE < t_brk), ]
#         xsub <- xsub[ , -c(1 : 2, 5, 9)]
#         xsub <- rename(xsub, c("DEP_CW" = "depth"))
#         y <- metab(specs = specs(mm_name(type = 'bayes', pool_K600 = 'normal')), data = xsub)
#         met_CW[[i]] <- as.data.frame(get_params(y))
#     }
#     time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
#     print(paste0("Completed Station ", STA[i], " for Cold-water period at ", time.stamp,
#                  " -- ", round(100 * perc.comp, 0), "% COMPLETE"))
#     names(met_CW)[i] <- STA[i]
#     # Spawning period calculations
#     df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE > t_brk), ])
#     perc.comp <- perc.comp + df.dim[1] / nrow(ts)
#     if (df.dim[1] == 0)
#     {
#         met_SP[[i]] = "No Data"
#     } else {
#         xsub <- x[which(x$STAID == STAID[i] & x$DATE > t_brk), ]
#         xsub <- xsub[ , -c(1 : 2, 5, 8)]
#         xsub <- rename(xsub, c("DEP_SP" = "depth"))
#         y <- metab(specs = specs(mm_name(type = 'bayes', pool_K600 = 'normal')), data = xsub)
#         met_SP[[i]] <- as.data.frame(get_params(y))
#     }
#     time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
#     print(paste0("Completed Station ", STA[i], " for Spawning period at ", time.stamp,
#                  " -- ", round(100 * perc.comp, 0), "% COMPLETE"))
# }
# names(met_CW) <- STA
# names(met_SP) <- STA

# RUN METABOLIZER FOR GPP, ER, AND REAERATION USING BAYESIAN METHOD WITH PARTIAL POOLING ***** NOT WORKING 9/19/18 ***** ----
# ____(THIS ONE WORKED 9/18/18) ______________________________________________________________________________________________________________
# for (i in 1 : length(STAID))
# {
#     # Cold-water period calculations
#     df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ])
#     perc.comp <- perc.comp + df.dim[1] / ts
#     if (df.dim[1] == 0)
#     {
#         met_CW[[i]] = "No Data"
#     } else {
#         xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME < t_brk), ]
#         xsub <- xsub[ , -c(1, 3, 9)]
#         xsub <- rename(xsub, c("DEP_CW" = "depth"))
#         y <- metab(specs = specs(mm_name(type = 'bayes', pool_K600 = 'normal')), data = xsub)
#         tmp.df <- as.data.frame(cbind(y@metab_daily, y@fit[["K600.daily"]], y@fit[["K600.daily.sd"]]))
#         tmp.df$STAID <- STAID[i]
#         names(tmp.df) <- c("date", "GPP", "GPP.L", "GPP.U", "ER", "ER.L", "ER.U", "msg", "wrn", "err", "K600", "k600.sd", "STAID")
#         tmp.df <- tmp.df[c(1, 13, 2, 5, 11, 3, 4, 6, 7, 12, 8, 9, 10)]
#         metab.all <- rbind(metab.all, tmp.df)
#         met_CW[[i]] <- tmp.df
#         plts_CW[[i]] <- plot_metab_preds(y, y_lim = list(GPP = c(0, 8), ER = c(-8, 0)))
#         ggsave(paste0("metab_SSPP_CW_", sta.ord[i], "_STA_", STA[i], ".png"),
#                plot = plts_CW[[i]], path = dir.out2, width = 6.5, height = 6.5, units = "in", dpi = 300)
#     }
#     time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
#     cat(paste0("Completed Station ", STA[i], " for Cold-water period at ", time.stamp,
#     " -- ", round(100 * perc.comp, 0), "% COMPLETE\n"), file = log.out, append = TRUE)
#     names(met_CW)[i] <- STA[i]
#     
#     # Spawning period calculations
#     df.dim <- dim(x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ])
#     perc.comp <- perc.comp + df.dim[1] / ts
#     if (df.dim[1] == 0)
#     {
#         met_SP[[i]] = "No Data"
#     } else {
#         xsub <- x[which(x$STAID == STAID[i] & x$DATE.TIME > t_brk), ]
#         xsub <- xsub[ , -c(1, 3, 8)]
#         xsub <- rename(xsub, c("DEP_SP" = "depth"))
#         y <- metab(specs = specs(mm_name(type = 'bayes', pool_K600 = 'normal')), data = xsub)
#         tmp.df <- as.data.frame(cbind(y@metab_daily, y@fit[["K600.daily"]], y@fit[["K600.daily.sd"]]))
#         tmp.df$STAID <- STAID[i]
#         names(tmp.df) <- c("date", "GPP", "GPP.L", "GPP.U", "ER", "ER.L", "ER.U", "msg", "wrn", "err", "K600", "k600.sd", "STAID")
#         tmp.df <- tmp.df[c(1, 13, 2, 5, 11, 3, 4, 6, 7, 12, 8, 9, 10)]
#         metab.all <- rbind(metab.all, tmp.df)
#         met_SP[[i]] <- tmp.df
#         plts_SP[[i]] <- plot_metab_preds(y, y_lim = list(GPP = c(0, 8), ER = c(-8, 0)))
#         ggsave(paste0("metab_SSPP_SP_", sta.ord[i], "_STA_", STA[i], ".png"),
#                plot = plts_SP[[i]], path = dir.out2, width = 6.5, height = 6.5, units = "in", dpi = 300)
#     }
#     time.stamp <- as.POSIXct(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angles")
#     cat(paste0("Completed Station ", STA[i], " for Spawning period at ", time.stamp,
#                " -- ", round(100 * perc.comp, 0), "% COMPLETE\n"), file = log.out, append = TRUE)
# }
# 
# 



