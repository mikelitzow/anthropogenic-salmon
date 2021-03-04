library(ncdf4)
library(zoo)
library(gplots)
library(dplyr)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(ggplot2)
library(oce)


# script for calculating GOA sst anomalies wrt 1951-1980
# download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5.nc?sst[(1960-01-01):1:(2020-12-01T00:00:00Z)][(0.0):1:(0.0)][(54):1:(62)][(200):1:(226)]", "~temp")

# paste into browser for windows!


# load and process SST data
# nc <- nc_open("~temp")

nc <- nc_open("./data/nceiErsstv5_d299_760e_08b4.nc")

# extract dates

ncvar_get(nc, "time")   # seconds since 1-1-1970
raw <- ncvar_get(nc, "time")
h <- raw/(24*60*60)
d <- dates(h, origin = c(1,1,1970))

# extract study area
# 54-62 deg. N, 200-226 deg. E
x <- ncvar_get(nc, "longitude")
y <- ncvar_get(nc, "latitude")

SST <- ncvar_get(nc, "sst", verbose = F)

# Change data from a 3-D array to a matrix of monthly data by grid point:
SST <- aperm(SST, 3:1)  

SST <- matrix(SST, nrow=dim(SST)[1], ncol=prod(dim(SST)[2:3]))  

# Keep track of corresponding latitudes and longitudes of each column:
lat <- rep(y, length(x))   
lon <- rep(x, each = length(y))   
dimnames(SST) <- list(as.character(d), paste("N", lat, "E", lon, sep=""))

# plot to check

# need to drop Bristol Bay cells
BB <- c("N58E200", "N58E202", "N56E200")
SST[,BB] <- NA

# and check
temp.mean <- colMeans(SST, na.rm=T)
z <- t(matrix(temp.mean,length(y)))  
image.plot(x,y,z, col=oceColorsPalette(64), xlim=c(195,230), ylim=c(53,62))
contour(x, y, z, add=T)  
map('world2Hires',c('Canada', 'usa'), fill=T,xlim=c(130,250), ylim=c(20,66),add=T, lwd=1, col="lightyellow3")

# monthly means for GOA-wide
yr <- as.numeric(as.character(years(d)))
m <- months(d)

SST <- rowMeans(SST, na.rm = T)


# set up winter year
win.yr <- yr
win.yr[m %in% c("Nov", "Dec")] <- win.yr[m %in% c("Nov", "Dec")]+1

sst.win <- SST[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]
win.yr <- win.yr[m %in% c("Nov", "Dec", "Jan", "Feb", "Mar")]

win.sst <- tapply(sst.win, win.yr, mean)

plot(1961:2020, win.sst[2:(length(win.sst)-1)], type="o")

# and calculate annual means as well
sst.ann <- tapply(SST, yr, mean)
plot(1960:2020, sst.ann, type="o")

xprt <- data.frame(year=1961:2020,
                   ndjfm.sst=win.sst[2:(length(win.sst)-1)],
                   annual.sst=sst.ann[2:length(sst.ann)])

xprt$win.sst.3 <- zoo::rollmean(xprt$ndjfm.sst, 3, fill=NA)
xprt$ann.sst.3 <- zoo::rollmean(xprt$annual.sst, 3, fill=NA)

xprt$win.sst.2 <- zoo::rollmean(xprt$ndjfm.sst, 2, fill=NA, align="right")
xprt$ann.sst.2 <- zoo::rollmean(xprt$annual.sst, 2, fill=NA, align="right")

# now add FAR values from fish-FAR project
library(brms)

obs_far_fixef <- readRDS("./data/obs_far_fixef.rds")

obs.FAR <- conditional_effects(obs_far_fixef, probs = c(0.025, 0.975))

obs.FAR <- obs.FAR$year_fac %>%
  mutate(year = as.numeric(as.character(year_fac))) %>%
  select(year, estimate__,)

names(obs.FAR)[2] <- c("FAR")

obs.FAR$FAR.3 <- zoo::rollmean(obs.FAR$FAR, 3, fill=NA)

obs.FAR$FAR.2 <- zoo::rollmean(obs.FAR$FAR, 2, fill=NA, align="right")

xprt <- left_join(xprt, obs.FAR)

# change 'year' to 'entry.year' to match salmon data!
names(xprt)[1] <- "entry.year"

write.csv(xprt, "./data/sst.far.csv", row.names = F)


