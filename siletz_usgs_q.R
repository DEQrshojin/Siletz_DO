# March 5, 2018

# Statistical analysis of dissolved oxygen data on the Upper Yaquina
# from September 2017

# Ryan Shojinaga, DEQ, adapted from Dan Sobota's original script

# Load libraries----
require(tidyverse)
require(doBy)
require(RODBC)
require(lubridate)
require(scales)

# USGS DATA RETRIEVAL----
# ________________________________________________________________________________
# Set URL input format and grab flow data from USGS Site 
chk1 <- as.POSIXct("09/16/2017", "%m/%d/%Y", tz = "America/Los_Angeles")
chk2 <- as.POSIXct("09/25/2017", "%m/%d/%Y", tz = "America/Los_Angeles")
start <- "2017-09-16"
end <- "2017-09-25"

# start <- "2017-07-01" # Both periods, but truncated at the end of a majority of the Spawning period data (Mid-Oct)
# end <- "2017-10-31"
Station_num <- 14305500
tmp.flow <- scan(paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=rdb&site_no=", Station_num,
                        "&period=&begin_date=", start, "&end_date=", end), what = "character", sep = "\n")

# Interpret and format file for heading information and data
header.ln <- grep("agency_cd", tmp.flow) # read header lines
header.nm <- gsub("(^+ )|( +$)", "", do.call(cbind, strsplit(tmp.flow[header.ln], split="\t")))

# Find the first line of data and create vector of (unparsed) data lines
num.1st <- header.ln + min(grep("USGS", tmp.flow[header.ln + 1:length(tmp.flow)]))
tmp.data <- tmp.flow[num.1st:length(tmp.flow)]

# Create data table with fields from text columns (CAN THIS BE DONE WTIH VECTORS INSTEAD OF LOOP)
tmp.list <- list()
for (y in 1:length(tmp.data)) {
  tmp.list[[y]] <- as.data.frame(strsplit(tmp.data[y], split = "\t"))
  names(tmp.list[[y]]) <- y
  tmp.list[[y]] <- t(tmp.list[[y]])
}

tmp.df <- data.frame(stringsAsFactors = F)
for (z in 1:length(tmp.list)) {
  tmp.df <- bind_rows(tmp.df, as.data.frame(tmp.list[[z]],
                                            stringsAsFactors = F))
}
header.nm[5,1] <- "instantaneous_flow_cfs"
header.nm[6,1] <- "QC_code"
names(tmp.df) <- header.nm
tmp.df <- tmp.df[tmp.df$site_no==Station_num,]

data.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz"

# Reformat flow data to numeric and datetime from string to date/time
st.38918.q <- tmp.df
st.38918.q$instantaneous_flow_cfs <- as.numeric(st.38918.q$instantaneous_flow_cfs)
st.38918.q$datetime <- as.POSIXct(st.38918.q$datetime, tz = "America/Los_Angeles")
write.csv(st.38918.q, file = paste0(data.dir, "/001_data/test.csv"), row.names=FALSE)

# Read in DEQ measured flow measurements at Hee Hee Ilahee Park
deq.Q <- read.csv(paste0(data.dir, "/001_data/wq/siletz_VolMon_data/deq_flow.csv"))
deq.Q$DATE_TIME <- as.POSIXct(deq.Q$DATE_TIME, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")

# COLLECTION PERIODS FOR DIFFERENT MONITORING PERIODS, i.e., DEQ vs LCWC
# ________________________________________________________________________________
# Monitoring Dates
mp <- read.csv(paste0(data.dir, "/003_scripts/monitoring_timeframes.csv"))
mp$STR <- as.POSIXct(mp$STR, "%m/%d/%Y", tz = "America/Los_Angeles")
mp$END <- as.POSIXct(mp$END, "%m/%d/%Y", tz = "America/Los_Angeles")

# # PRECIPITATION DATA
# # ________________________________________________________________________________
# # Read climate data from text file and format dates
# clm.data <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/climate/"
# p.tmp <- read.csv(paste0(clm.data, "precip.csv"), stringsAsFactors = F)
# p.tmp$DATE <- as.POSIXct(p.tmp$DATE, format = "%m/%d/%Y %H:%M")
 
#t.lims <- c(min(st.38918.q$datetime), max(st.38918.q$datetime))
t.lims <- c(chk1, chk2)

# Plot flow and (!) precipitation data
# ________________________________________________________________________________
Q.plot <- ggplot() + geom_point(data = st.38918.q, aes(x = datetime, y = instantaneous_flow_cfs), color = "darkblue", size = 1, shape = 1) +
          geom_point(data = deq.Q, aes(x = DATE_TIME[[1]], y = FLOW_CFS[[1]]), color = "darkgreen", size = 2.5) +
          xlab("Date Time") + ylab("Stream flow (cfs)") +
          scale_x_datetime(breaks=date_breaks("1 days"), labels=date_format("%m/%d %H:%M"), limits=t.lims) +
          scale_y_log10(limits = c(10, 1000), breaks = c(10, 100, 1000)) +
          theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) +
          ggtitle("USGS 14305500 - SILETZ RIVER AT SILETZ, OR") +  theme(plot.title = element_text(size = 12, hjust = 0.5))

Q.plot

# save.dir <- paste0(data.dir, "/002_figures")
# ggsave(filename = "Fig2_usgs_q.jpg", plot = Q.plot, path = save.dir,width = 10, height = 8, units = "in", dpi = 300)

#     geom_segment(aes(x = mp$END[1], y = mp$STV[1], xend = mp$STR[1], yend = mp$STV[1]), color = "gray48", size = 1,
#              arrow = arrow(angle = 90, length = unit(0.05, "inches"), ends = "both")) +
#     geom_segment(aes(x = mp$STR[2], y = mp$STV[2], xend = mp$END[2], yend = mp$STV[2]), color = "gray48", size = 1,
#                  arrow = arrow(angle = 90, length = unit(0.05, "inches"), ends = "both")) +
#     geom_segment(aes(x = mp$STR[3], y = mp$STV[3], xend = mp$END[3], yend = mp$STV[3]), color = "gray48", size = 1,
#                  arrow = arrow(angle = 90, length = unit(0.05, "inches"), ends = "both")) +
#     geom_segment(aes(x = mp$STR[4], y = mp$STV[4], xend = mp$END[4], yend = mp$STV[4]), color = "gray48", size = 1,
#                  arrow = arrow(angle = 90, length = unit(0.05, "inches"), ends = "both")) +
#     geom_segment(aes(x = mp$END[1] + 86400 * 14, y = mp$STV[1] + 6.5, xend = mp$END[1] + 86400 * 1, yend = mp$STV[1]), color = "darkred", size = 0.5,
#                  arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last")) +
#     geom_segment(aes(x = mp$STR[2] - 86400 * 14, y = mp$STV[1] + 6.5, xend = mp$STR[2] - 86400 * 1, yend = mp$STV[1]), color = "darkred", size = 0.5,
#                  arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last")) +
#     geom_segment(aes(x = mp$END[3] - 86400 * 14, y = mp$STV[3] - 4, xend = mp$STR[3] + 86400 * 27.5, yend = mp$STV[3] - 1), color = "darkred", size = 0.5,
#                  arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last")) +
#     geom_segment(aes(x = mp$STR[4], y = mp$STV[3] - 4, xend = mp$STR[4] + 86400 * 20.5, yend = mp$STV[3] - 1), color = "darkred", size = 0.5,
#                  arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last")) +
#     geom_segment(aes(x = mp$END[2] + 86400 * 13, y = 80, xend = mp$STR[4] + 86400 * 8.5, yend = 60.8), color = "darkred", size = 0.5,
#                  arrow = arrow(angle = 30, length = unit(0.05, "inches"), ends = "last")) +
#     annotate("text", mp$END[1] + 86400 * 14, mp$STV[1] + 12, label = "DEQ monitoring periods", hjust = 0) +
#     annotate("text", mp$STR[3] + 86400 * 39, mp$STV[3] - 5, label = "LSWCD monitoring periods", hjust = 0) +
#     annotate("text", mp$END[2] + 86400 * 14, 80, label = "DEQ measured flow: 60.8 cfs\nat Station 37848 on 09/14/2017", hjust = 0) +
    


