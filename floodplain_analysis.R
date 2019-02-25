# Extracts cross sectional data for floodplain analysis for FTABLES
pnts <- read.csv('C:/siletz_old/scratch/SLZ_shp/rchpnts_vrtx.csv',
              stringsAsFactors = FALSE)

pnts <- pnts[, c(1 : 2, 7 : 9)]

# remove jaybird creek
pnts <- pnts[pnts$Desc_ != 'Jaybird Creek', ]

# Semi-randomly select 5 points per section (Lo, mid, high gradient)
# By semi I mean look at each point and see it it's on a more or less straight
# Section, otherwise, select on that is near that one that is

basins <- data.frame(Desc = unique(pnts$Desc_), stringsAsFactors = FALSE)

basins$Sect <- c(3, 3, 2, 3, 3, 2, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1)

pnts <- merge(pnts, basins,
              by.x = 'Desc_', by.y = 'Desc',
              all.x = TRUE, all.y = TRUE)

# get indeces of the sections
indx <- list('Sect 1' = which(pnts$Sect == 1),
             'Sect 2' = which(pnts$Sect == 2),
             'Sect 3' = which(pnts$Sect == 3))

slctd <- data.frame(matrix(0, nrow = 5, ncol = 3,
                           dimnames = list(NULL, c('s1', 's2', 's3'))))

for (i in 1 : 3) {
  
  tmp <- sample(indx[[i]], 5, replace = FALSE)
  
  slctd[, i] <- pnts[tmp, 3]
    
}

library(reshape2)

slctd <- melt(slctd, id.vars = NULL, measure.vars = c('s1', 's2', 's3'))

write.csv(slctd, file = "C:/siletz_old/scratch/SLZ_shp/rand_pnts.csv",
          row.names = FALSE)

# Now audit slctd and select sections with straight runs (THESE ARE from FIDS)
slctd2 <- data.frame('sect' = c(  3,    3,    3,    3,    3,    2,    2,    2,
                                  2,    2,    1,    1,    1,    1,     1),
                     'fid1' = c(402, 3038, 8648, 6460, 7731, 3830, 3159, 5596,
                                5143, 5230, 6263, 5959, 9327, 10519, 10933),
                     'fid2' = c(403, 3039, 8649, 6461, 7732, 3831, 3160, 5597,
                                5144, 5231, 6264, 5960, 9328, 10520, 10934),
                     'dem'  = c('upp', 'upp', 'rck', 'rck', 'rck',
                                'mid', 'mid', 'mid', 'mid', 'mid',
                                'mid', 'mid', 'ls1', 'ls1', 'ls1'),
                     stringsAsFactors = FALSE)

# Create a matrix with the slctd2 fid1 and fid2 points:
xsct <- data.frame(cbind(pnts[which(pnts$fid_1 %in% slctd2$fid1), ],
                        pnts[which(pnts$fid_1 %in% slctd2$fid2), ]),
                  stringsAsFactors = FALSE)

xsct <- xsct[, c(1, 3 : 5, 9 : 12)]

names(xsct) <- c('rch', 'fd1', 'x1', 'y1', 'fd2', 'x2', 'y2', 'sct')

xsct$rch <- paste0(c('BRK', 'BRK', 'JMP', 'LRK', 'LOG',
                     'LOG', 'LMN', 'LMN', 'LMN', 'NFS',
                     'OJA', 'SFS', 'UMN', 'UMN', 'USG'),
                   '_', xsct$fd1)

# Calculate the parameters of the line perpendicular and bisecting the river
# segment for floodplain geometry analysis
xsct$xm <- (xsct$x1 + xsct$x2) / 2 # x coordinate of the midpoint

xsct$ym <- (xsct$y1 + xsct$y2) / 2 # y coordinate of the midpoint

# slope of perpendicular line
xsct$m <- -(1 / ((xsct$y2 - xsct$y1)/(xsct$x2 - xsct$x1)))

xsct$b <- xsct$ym - xsct$m * xsct$xm # intercept of perpendicular line

# Calculate points through the perpendicular line spaced 50m apart up to 500m
D <- seq(-150, 150, 3)

rnames <- c(paste0('m', seq(150, 3, -3)),
            paste0('z', 0),
            paste0('p', seq(3, 150, 3)))

xpts <- data.frame('ste' = 0,
                   'rnames' = 0,
                   'x2' = 0,
                   'y2' = 0)

xpts <- xpts[-1, ]

for (i in 1 : nrow(xsct)) {

  x2 <- D / (sqrt(xsct[i, 11]^2 + 1)) + xsct[i, 9]
  
  y2 <- xsct[i, 11] * x2 + xsct[i, 12]
  
  tmp <- data.frame(cbind('ste' = rep(xsct[i, 1], length(D)),
                          'rnames' = rnames,
                          'x2' = x2,
                          'y2' = y2),
                    stringsAsFactors = FALSE)
  
  xpts <- rbind(xpts, tmp)
  
}

xpts$fid1 <- as.numeric(substr(xpts$ste, 5, 10))

xpts <- merge(xpts, slctd2, by.x = 'fid1', by.y = 'fid1', all.x = TRUE)

write.csv(xpts, file = 'C:/siletz_old/scratch/SLZ_shp/xpts.csv',
          row.names = FALSE)

# write.csv(xsct, file = 'C:/siletz_old/scratch/SLZ_shp/xsct2.csv',
#           row.names = FALSE)












