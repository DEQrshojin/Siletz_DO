# SCRATCH ----
#_______________________________________________________________________________
# Check to see if definitive breaks in the topography -- See below
# sub <- rch[, c(2, 5)]
# 
# sub$ord <- 1 : 16
# 
# slpPlt <- ggplot(data = sub, aes(x = ord, y = S)) + geom_point()
# 
# slpPlt
# 
# Based on these plots separate the reaches into three  groups:
# Group 1: From Cedar Creek to Logsden 
# Group 2: Upper and Lower Moonshine
# Group 3: Nth, Sth Forks, Sunshine and Rock Creeks
#_______________________________________________________________________________
# # IMPORT AND VIEW CROSS SECTIONS (from LSWCD data)
# xsect <- read.csv("C:/siletz_old/scratch/xsect.csv",
#                   stringsAsFactors = FALSE)
# 
# # Remove points with no depth
# xsect <- xsect[-c(1, 55, 113, 114, 120, 141), ]
#
# plt <- ggplot(data = xsect, aes(x = DIST, y = DEPTH, group = SITE)) +
#   geom_line() + geom_point() + scale_y_reverse() +
#   facet_wrap(~SITE + DATE, ncol = 1)
# 
# ggsave("xsect.png", plot = plt, path = 'C:/siletz_old/scratch',
#        width = 15, height = 10, units = "in", dpi = 300)
#_______________________________________________________________________________
# Plot first
# sitesLong <- melt(sites, id.vars = 'bs_area',
#                   measure.vars = c('xs_area', 'depth', 'top_wid', 'bot_wid',
#                                    'flow'),
#                   variable.name = 'var',
#                   value.name = 'val')
# 
# scatPlt <- ggplot(data = sitesLong, aes(x = bs_area, y = val,
#                                         group = var, shape = var)) +
#   geom_line() + geom_point(size = 5) + scale_x_log10() + scale_y_log10()
# 
# ggsave("geom.png", plot = scatPlt, path = 'C:/siletz_old/scratch',
#        width = 15, height = 10, units = "in", dpi = 300)