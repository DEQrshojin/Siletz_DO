dir1 <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\wq_data\\Monitoring 2017\\DEQ\\Grab_Samples\\estuary\\"
dat <- read.csv(paste0(dir, "salinity_master.csv"))
STAID <- read.csv(paste0(dir, "STAID.csv"))
zz <- unique(dat$STAID)

box.sal <- ggplot() + geom_boxplot(data = dat, aes(x = RM, y = Sal_ppt, group = RM, fill = DATA), outlier.shape = 1, outlier.size = 1) +
        theme_bw() + theme(panel.grid.minor=element_blank(),
                           legend.position = "none", plot.title = element_text(hjust = 0.5),
                           axis.title.y = element_text(margin = margin(r = 9))) +
        scale_x_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20)) +
        scale_y_continuous(limits = c(0, 40), breaks = c(0, 10, 20, 30)) +
        xlab("River Mile") + ylab("Salinity (ppt)") +
        annotate("text", x = STAID$RM, y = 30,
                 label = paste0(STAID$STAID, " - ", STAID$DESC), angle = 90, size = 3,
                 hjust = 0) +
        geom_hline(yintercept = 10, color = "blue", size = 0.4, linetype = 2) +
        annotate("text", 53, 8.15, label = "EPA Estuary Guidance Threshold", hjust = 0, size = 3.0)

ggsave(paste0("fig_YY_Salinity.png"), plot = box.sal, path = dir1, scale = 1,
       width = 12, height = 6, units = "in", dpi = 300)