# September 2017
# Flow data figure for analysis report
# Ryan Shojinaga, DEQ

# Load libraries----
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)

# Read in USGS data from csv (URL not working on 12/26/18) ----
dir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
              'Middle_Siletz_River_1710020405/001_data/flow_data/')
fil <- 'siletz_q_data_2.csv'
st.38918.q <- read.csv(paste0(dir, fil), stringsAsFactors = FALSE)
st.38918.q$datetime <- as.POSIXct(st.38918.q$datetime,
                                  "%m/%d/%Y %H:%M",
                                  tz = 'America/Los_Angeles')
st.38918.q$instantaneous_flow_cfs <- as.numeric(st.38918.q$instantaneous_flow_cfs)
st.38918.q <- st.38918.q[complete.cases(st.38918.q), ]

# Calculate daily medians for plotting ----
source('')

medFlows <- daily_medians(st.38918.q) # Returns dF of (DOY, medFlow)

# Read in DEQ measured flow measurements at Hee Hee Ilahee Park ----
deq.Q <- read.csv(paste0(dir, 'deq_flow.csv'))
deq.Q$DATE_TIME <- as.POSIXct("09/14/2017 00:00",
                              format = "%m/%d/%Y %H:%M",
                              tz = "America/Los_Angeles")
deq.Q <- deq.Q[1, ]

# Monitoring Dates ----
mp <- read.csv(paste0(dir, 'monitoring_timeframes.csv'))
mp$STR <- as.POSIXct(mp$STR, "%m/%d/%Y", tz = "America/Los_Angeles")
mp$END <- as.POSIXct(mp$END, "%m/%d/%Y", tz = "America/Los_Angeles")
CW.STR <- as.POSIXct("2017-07-01", "%Y-%m-%d", tz = "America/Los_Angeles")
CW.END <- as.POSIXct("2017-09-01", "%Y-%m-%d", tz = "America/Los_Angeles")
SP.STR <- as.POSIXct("2017-09-01", "%Y-%m-%d", tz = "America/Los_Angeles")
SP.END <- as.POSIXct("2017-10-31", "%Y-%m-%d", tz = "America/Los_Angeles")

t.lims <- as.POSIXct(c('2017-07-01', '2017-11-01'), format = '%Y-%m-%d',
                     tz = "America/Los_Angeles")

# Create a timeseries of median flows using t.lims ----
qMed <- data.frame('date' = as.POSIXct(seq(t.lims[1], t.lims[2], 86400),
                                       origin = "1970-01-01",
                                       tz = "America/Los_Angeles"),
                   'DOY' = yday(seq(t.lims[1], t.lims[2], 86400)),
                   stringsAsFactors = FALSE)

qMed <- merge(qMed, medFlows, by.x = 'DOY', by.y = 'DOY', all.x = TRUE,
              all.y = FALSE)

# Combine into one dataset for plotting
q4plot <- data.frame('date' = as.POSIXct(seq(t.lims[1], t.lims[2], 3600),
                                         origin = "1970-01-01",
                                         tz = "America/Los_Angeles"))

q4plot <- merge(q4plot, st.38918.q, by.x = 'date', by.y = 'datetime',
                all.x = TRUE, all.y = FALSE)

q4plot <- merge(q4plot, qMed[, 2 : 3], by.x = 'date', by.y = 'date',
                all.x = TRUE, all.y = FALSE)

q4plot <- merge(q4plot, deq.Q[, 3 : 4], by.x = 'date', by.y = 'DATE_TIME',
                all.x = TRUE, all.y = FALSE)

names(q4plot) <- c('date', 'q_ins', 'q_med', 'q_deq')

q4plot <- melt(q4plot, id.vars = 'date', variable.name = 'Source',
               value.name = 'q_cfs', na.rm = TRUE)

# PLOT FLOW ----
Q.plot <- ggplot(data = q4plot, aes(x = date, y = q_cfs,
                                    color = Source,
                                    shape = Source,
                                    size = Source)) +
          geom_point() + xlab("Date Time") + ylab("Stream flow (cfs)") +
          scale_x_datetime(breaks = date_breaks("14 days"),
                           labels = date_format("%m/%d"), limits = t.lims) +
          scale_y_log10(limits = c(1, 50000),
                        breaks = c(1, 10, 100, 1000, 10000, 50000)) +
          theme_bw() + theme(legend.position = c(0.25, 0.65)) +
          scale_color_manual(values = c('darkblue', 'darkred', 'darkgreen'),
                             name = 'Legend',
                             breaks = c('q_ins', 'q_med', 'q_deq'),
                             labels = c('USGS Instantaneous Flow',
                                        'USGS Daily Median',
                                        'DEQ Estimated Flow')) + 
          scale_shape_manual(values = c(1, 2, 18),
                             name = 'Legend',
                             breaks = c('q_ins', 'q_med', 'q_deq'),
                             labels = c('USGS Instantaneous Flow',
                                        'USGS Daily Median',
                                        'DEQ Estimated Flow')) +
          scale_size_manual(values = c(1, 1.7, 3),
                            name = 'Legend',
                            breaks = c('q_ins', 'q_med', 'q_deq'),
                            labels = c('USGS Instantaneous Flow',
                                       'USGS Daily Median',
                                       'DEQ Estimated Flow')) + 
          geom_segment(aes(x = mp$END[1], y = mp$STV[1], xend = mp$STR[1],
                           yend = mp$STV[1]), color = "gray48", size = 1,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          geom_segment(aes(x = mp$STR[2], y = mp$STV[2], xend = mp$END[2],
                           yend = mp$STV[2]), color = "gray48", size = 1,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          geom_segment(aes(x = mp$STR[3], y = mp$STV[3], xend = mp$END[3], 
                           yend = mp$STV[3]), color = "gray48", size = 1,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          geom_segment(aes(x = mp$STR[4], y = mp$STV[4], xend = mp$END[4], 
                           yend = mp$STV[4]), color = "gray48", size = 1,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          geom_segment(aes(x = mp$END[1] + 86400 * 14, y = mp$STV[1] + 6.5, 
                           xend = mp$END[1] + 86400 * 1, yend = mp$STV[1]), 
                       color = "darkred", size = 0.5,
                       arrow = arrow(angle = 30, length = unit(0.05, "inches"), 
                                     ends = "last")) +
          geom_segment(aes(x = mp$STR[2] - 86400 * 14, y = mp$STV[1] + 6.5, 
                           xend = mp$STR[2] - 86400 * 1, yend = mp$STV[1]), 
                       color = "darkred", size = 0.5,
                       arrow = arrow(angle = 30, length = unit(0.05, "inches"), 
                                     ends = "last")) +
          geom_segment(aes(x = mp$END[3] - 86400 * 14, y = mp$STV[3] - 4, 
                           xend = mp$STR[3] + 86400 * 27.5, 
                           yend = mp$STV[3] - 1), color = "darkred", size = 0.5,
                       arrow = arrow(angle = 30, length = unit(0.05, "inches"), 
                                     ends = "last")) +
          geom_segment(aes(x = mp$STR[4], y = mp$STV[3] - 4, 
                           xend = mp$STR[4] + 86400 * 20.5, 
                           yend = mp$STV[3] - 1), color = "darkred", size = 0.5,
                       arrow = arrow(angle = 30, length = unit(0.05, "inches"), 
                                     ends = "last")) +
          geom_segment(aes(x = mp$END[2] + 86400 * 13, y = 50, 
                           xend = mp$STR[4] + 86400 * 8.5, yend = 60.8), 
                       color = "darkred", size = 0.5,
                       arrow = arrow(angle = 30, length = unit(0.05, "inches"), 
                                     ends = "last")) +
          geom_segment(aes(x = CW.STR, y = 10000, xend = CW.END, yend = 10000), 
                       color = "grey48", size = 0.5,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          geom_segment(aes(x = SP.STR, y = 10000, xend = SP.END, yend = 10000), 
                       color = "grey48", size = 0.5,
                       arrow = arrow(angle = 90, length = unit(0.05, "inches"), 
                                     ends = "both")) +
          annotate("text", mp$END[1] + 86400 * 14, mp$STV[1] + 12, 
                   label = "DEQ monitoring periods", hjust = 0) +
          annotate("text", mp$STR[3] + 86400 * 39, mp$STV[3] - 5, 
                   label = "LSWCD monitoring periods", hjust = 0) +
          annotate("text", mp$END[2] + 86400 * 14, 50, 
                   label = paste0("DEQ measured flow: 60.8 cfs\n", 
                                  "at Station 37848 on 09/14/2017"), 
                   hjust = 0) +
          annotate("text", SP.STR - 86400 * 45, 12000,
                   label = "Cold-water Criteria Period", hjust = 0) +
          annotate("text", SP.STR + 86400 * 10, 12000,
                   label = "Spawning Criteria Period", hjust = 0) +
          ggtitle("USGS 14305500 - SILETZ RIVER AT SILETZ, OR") + 
          theme(plot.title = element_text(size = 12, hjust = 0.5))

presoPath <- '//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_Siletz_River_1710020405/005_reporting/presentations/003_TWG_20190403/figures_maps'

ggsave(filename = "Fig2_usgs_q.png", plot = Q.plot, path = presoPath, width = 12,
       height = 9, units = "in", dpi = 300)

# ----
# PREVIOUS CODE -- URL NOT WORKING ON 12/26/2018 ----
# require(doBy)
# require(RODBC)
# # USGS DATA RETRIEVAL
# # Set URL input format and grab flow data from USGS Site 
# start <- "1986-10-01"
# end <- "2018-09-30"
# Station_num <- 14305500
# tmp.flow <- scan(paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=rdb&site_no=", Station_num,
#                         "&period=&begin_date=", start, "&end_date=", end), what = "character", sep = "\n")
# 
# # Interpret and format file for heading information and data
# header.ln <- grep("agency_cd", tmp.flow) # read header lines
# header.nm <- gsub("(^+ )|( +$)", "",
#                   do.call(cbind, strsplit(tmp.flow[header.ln], split="\t")))
# 
# # Find the first line of data and create vector of (unparsed) data lines
# num.1st <- header.ln + min(grep("USGS",
#                            tmp.flow[header.ln + 1:length(tmp.flow)]))
# tmp.data <- tmp.flow[num.1st:length(tmp.flow)]
# 
# # Create data table with fields from text columns
# tmp.list <- list()
# for (y in 1:length(tmp.data)) {
#   tmp.list[[y]] <- as.data.frame(strsplit(tmp.data[y], split = "\t"))
#   names(tmp.list[[y]]) <- y
#   tmp.list[[y]] <- t(tmp.list[[y]])
# }
# 
# tmp.df <- data.frame(stringsAsFactors = F)
# for (z in 1:length(tmp.list)) {
#   tmp.df <- bind_rows(tmp.df, as.data.frame(tmp.list[[z]],
#                                             stringsAsFactors = F))
# }
# header.nm[5,1] <- "instantaneous_flow_cfs"
# header.nm[6,1] <- "QC_code"
# names(tmp.df) <- header.nm
# tmp.df <- tmp.df[tmp.df$site_no==Station_num,]
# 
# data.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz"
# 
# # Reformat flow data to numeric and datetime from string to date/time
# st.38918.q <- tmp.df
# st.38918.q$instantaneous_flow_cfs <- as.numeric(st.38918.q$instantaneous_flow_cfs)
# st.38918.q$datetime <- as.POSIXct(st.38918.q$datetime, tz = "America/Los_Angeles")
# write.csv(st.38918.q, file = paste0(data.dir, "/001_data/test.csv"), row.names=FALSE)



