library(tidyverse)

# load salmon catch data

se <- read.csv("./data/southeast.catch.csv")

head(se)

se <- se %>%
  select(-chinook) %>%
  mutate(area="Southeast Alaska")

pws <- read.csv("./data/pws.catch.csv")

head(pws)

pws <- pws %>%
  select(-chinook) %>%
  mutate(area="Prince William Sound")

ci <- read.csv("./data/cook.inlet.catch.csv")

head(ci)

ci <- ci %>%
  mutate(area="Cook Inlet")

kod <- read.csv("./data/kodiak.catch.csv")

head(kod)

kod <- kod %>% 
  select(-chinook)

chig <- read.csv("./data/chignik.catch.csv")

head(chig)

chig <- chig %>%
  select(-chinook) %>%
  mutate(area="Chignik")

spen <- read.csv("./data/south.peninsula.catch.csv")

head(spen)

spen <- spen %>%
  select(-chinook) %>%
  mutate(area="South Alaska Peninsula")

dat <- rbind(se,
             pws,
             ci,
             kod,
             chig,
             spen) %>%
  filter(year >= 1965) %>%
  pivot_longer(cols = c(-year, -area)) %>%
  group_by(year, name) %>%
  summarise(log.catch = log(sum(value),10))
  
# separate even/odd pink
dat$name <- if_else(dat$name=="pink" & gtools::even(dat$year)==T, "pink_even",
                    if_else(dat$name=="pink" & gtools::odd(dat$year)==T, "pink_odd", dat$name))


ggplot(dat, aes(year, log.catch, color=name)) +
  geom_line()

# and save for further analysis
write.csv(dat, "./data/total.goa.salmon.catch.csv")
