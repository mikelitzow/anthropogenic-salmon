library(tidyverse)

dat <- read.csv("./data/total.goa.salmon.catch.csv", row.names=1)

# add a column of ocean entry

dat$entry.year <- if_else(dat$name == "sockeye", dat$year-2,
                          if_else(dat$name == "chum", dat$year-3, dat$year-1))

# set up eras based on catch year
dat$era <- if_else(dat$year < 1989, "1965-1988",
                      if_else(dat$year %in% 1989:2013, "1989-2013", "2014-2020"))

# load sst/far covariates

covar <- read.csv("./data/sst.far.csv")
names(covar)[2:3] <- c("win.sst", "ann.sst")

# scale covariates to plot
ff <- function(x) as.vector(scale(x))
sc.covar <- covar
sc.covar[,2:10] <- apply(covar[,2:10], 2, ff)


sc.dat <- left_join(dat, sc.covar) %>%
  pivot_longer(cols = c(win.sst, ann.sst, win.sst.3, ann.sst.3, win.sst.2, ann.sst.2, FAR, FAR.3, FAR.2),
               names_to = "covariate")




ggplot(sc.dat, aes(value, log.catch, color=era)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_grid(name ~ covariate)
  
dat <- left_join(dat, covar)

ggplot(dat, aes(FAR.3, log.catch, color=era)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~name, scales="free_y")
