#-------------------------------------------------------------------------------
# READ TEXT FILE OF SLOPE DATA 
# Ryan Shojinaga, Oregon DEQ
# shojinaga.ryan@deq.state.or.us
# 503-229-557
#-------------------------------------------------------------------------------

# Load Libraries ----



# File paths and object declarations ----
slope.txt <- list()
file.dir <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/dem/to_TetraTech/dem_tif/"
file.name <- c("ls2_hist", "ls1_hist", "mid_hist", "upp_hist", "rck_hist")
max.slope <- c(78.2787, 68.0967, 80.3116, 81.3362, 71.7564)
bin.size <- max.slope / 256

# Read and organize data ----
for (i in 1 : 5)
{
    slope.txt[[i]] <- read.csv(paste0(file.dir, file.name[i], ".csv"))
    slope.txt[[i]] <- rbind(slope.txt[[i]], slope.txt[[i]][1, ])
    slope.txt[[i]]$Rowid_ <- 1 : nrow(slope.txt[[i]])
    slope.txt[[i]][nrow(slope.txt[[i]]), 1] <- 999
    for(j in 2 : length(slope.txt[[i]]))
    {
        slope.txt[[i]][nrow(slope.txt[[i]]), j] <- sum(slope.txt[[i]][1 : nrow(slope.txt[[i]]) - 1, j])
    }
}
names(slope.txt) <- file.name

# Check for basins per slope data frames ----
basin.names <- paste0("HSPF__", c(1 : 17))
zero.column <- as.data.frame(rep(0, 257))
find.substring <- function(x) {substr(x, 7, 10)}
for (i in 1 : length(slope.txt))
{
    col.names <- names(slope.txt[[i]])
    for (j in 1 : length(basin.names))
    {
        if(basin.names[j] %in% col.names) {} else
        {
            names(zero.column) <- basin.names[j]
            slope.txt[[i]] <- cbind(slope.txt[[i]], zero.column)
        }
    }
    # order the columns based on the basin no.
    basin.numbers <- as.matrix(names(slope.txt[[i]][, 2 : 18]))
    basin.numbers <- as.numeric(apply(basin.numbers, 1, find.substring))
    basin.numbers <- as.data.frame(cbind(rep(0, length(basin.numbers)), basin.numbers))
    basin.numbers$V1 <- c(1 : nrow(basin.numbers))
    basin.numbers <- basin.numbers[order(basin.numbers$basin.numbers), ]
    new.order <- c(1, as.vector(basin.numbers$V1 + 1))
    slope.txt[[i]] <- slope.txt[[i]][, new.order]
    slope.txt[[i]][nrow(slope.txt[[i]]), 1] <- sum(slope.txt[[i]][nrow(slope.txt[[i]]), 2 : 18])
}
slope.mean <- as.data.frame(matrix(0, nrow = 5, ncol = 17))
names(slope.mean) <- basin.names
for (k in 1 : 5) {slope.txt[[k]] <- slope.txt[[k]][-nrow(slope.txt[[k]]), ]} # remove the last row of slope
for (i in 1 : 5)
{
    slope.txt[[i]]$bin.med <- round(slope.txt[[i]]$Rowid_ * bin.size[i] - bin.size[i] / 2, 3)
    for (j in 1 : 17)
    {
        slope.mean[i, j] <- sum(as.numeric(slope.txt[[i]][, j + 1]) * slope.txt[[i]][, 19]) / sum(as.numeric(slope.txt[[i]][, j + 1]))
    }
}
mean.slopes <- c(slope.mean[4, 1],
                 slope.mean[4, 2],
                 slope.mean[3, 3],
                 slope.mean[3, 4],
                 slope.mean[3, 5],
                 slope.mean[3, 6],
                 slope.mean[3, 7],
                 mean(slope.mean[2, 8], slope.mean[3, 8], slope.mean[5, 8]),
                 slope.mean[5, 9],
                 slope.mean[5, 10],
                 slope.mean[2, 11],
                 slope.mean[2, 12],
                 slope.mean[2, 13],
                 slope.mean[2, 14],
                 mean(slope.mean[1, 15], slope.mean[2, 15]),
                 slope.mean[1, 16],
                 slope.mean[1, 17])



# basin.totals <- rbind(slope.txt[[1]][nrow(slope.txt[[1]]), ],
#                       slope.txt[[2]][nrow(slope.txt[[2]]), ],
#                       slope.txt[[3]][nrow(slope.txt[[3]]), ],
#                       slope.txt[[4]][nrow(slope.txt[[4]]), ],
#                       slope.txt[[5]][nrow(slope.txt[[5]]), ])
# basin.pct <- basin.totals
# for (i in 1 : 5) {basin.pct[i, ] <- round(basin.totals[i, ] / basin.totals[i, 1] * 100, 2)}
# x <- basin.totals[1, ]
# for (i in 1 : 18) {x[, i] <- sum(basin.totals[, i])}
# x <- round(x * (9 * 0.3048)^2 / 10^6, 1) # cell -> sqm <- sqkm










