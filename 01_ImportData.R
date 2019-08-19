setwd("~/Documents/04_Projects//02_NBA_PlayerMap/Version_03")

library(tidyverse)
library(rvest)

# determine size of logo to show on the map
d <- 2
f <- 1
m <- "PPG"
  
# import stadium coordinates from CSV file
#url_csv2 <- 'https://raw.githubusercontent.com/alamine53/nbatrademap/master/raw_NbaCities_adj.csv'
nbacities <- read_csv("01_RawData/raw_NbaCities_adj.csv") %>%
  mutate(long_adj_left = long - f) %>%
  mutate(long_adj_right = long + f) %>%
  mutate(lat_adj_up = lat + f) %>%
  mutate(lat_adj_down = lat - f) %>%
#  mutate(long_new = replace(long, names=="LAL", long_adj_left)) %>%
 # mutate(long = replace(long, names=="LAC", long_adj_right)) %>%
  #mutate(long = replace(long, names=="BRK", long_adj_right)) %>%
  #mutate(long = replace(long, names=="NYK", long_adj_left)) %>%
  mutate(long_min = long - d) %>%
  mutate(long_max = long + d) %>%
  mutate(lat_min = lat - d) %>%
  mutate(lat_max = lat +d) %>%
  arrange(names)
  
saveRDS(nbacities, file = "nbacities")

transactions <- read_csv("01_RawData/IN_Transactions.csv") %>%
  arrange(Rk)

# add stadium coordinates
nbacities1 <- nbacities %>%
  select(names, long, lat, east) %>%
  rename(OTm = names)
nbacities2 <- nbacities %>%
  select(names, long, lat) %>%
  rename(NTm = names)
tran1 <- left_join(transactions, nbacities1, by = "OTm")
tran2 <- left_join(tran1, nbacities2, by = "NTm")
tran3 <- tran2 %>%
  rename(long.1 = long.x, long.10 = long.y, lat.1 = lat.x, lat.10 = lat.y)
rm(tran1, tran2)

# calculate intermediate coordinates (10) for flyover effect
tran4 <- tran3 %>% 
  mutate(long.2 = long.1 + (long.10 - long.1)/9 ) %>%
  mutate(long.3 = long.2 + (long.10 - long.1)/9 ) %>%
  mutate(long.4 = long.3 + (long.10 - long.1)/9 ) %>%
  mutate(long.5 = long.4 + (long.10 - long.1)/9 ) %>%
  mutate(long.6 = long.5 + (long.10 - long.1)/9 ) %>%
  mutate(long.7 = long.6 + (long.10 - long.1)/9 ) %>%
  mutate(long.8 = long.7 + (long.10 - long.1)/9 ) %>%
  mutate(long.9 = long.8 + (long.10 - long.1)/9 )

tran4 <- tran4 %>% 
  mutate(lat.2 = lat.1 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.3 = lat.2 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.4 = lat.3 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.5 = lat.4 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.6 = lat.5 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.7 = lat.6 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.8 = lat.7 + (lat.10 - lat.1)/9 ) %>%
  mutate(lat.9 = lat.8 + (lat.10 - lat.1)/9 )

#create empty rows
for(s in 1:14) {
  temp <- tran4 %>%
    filter(Rk == s)
  df <- rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    rbind(tran4, temp) %>%
    arrange(as.numeric(Rk))
}

mvmt <- df %>% 
  group_by(Player) %>%
  mutate(fr = 1:n()) %>%
  filter(fr < 15) %>%
  arrange(as.numeric(Rk), fr)

flyover <- mvmt %>% 
  mutate(long = ifelse(fr == 1, long.1,
                       ifelse(fr == 2, long.2, 
                       ifelse(fr == 3, long.3,
                       ifelse(fr == 4, long.4,
                       ifelse(fr == 5, long.5,
                       ifelse(fr == 6, long.6, 
                       ifelse(fr == 7, long.7, 
                       ifelse(fr == 8, long.8, 
                       ifelse(fr == 9, long.9,
                       ifelse(fr == 10, long.10, 
                       ifelse(fr == 11, long.10,
                       ifelse(fr == 12, long.10,
                       ifelse(fr == 13, long.10, long.10)))))))))))))) %>%
  mutate(lat = ifelse(fr == 1, lat.1,
                     ifelse(fr == 2, lat.2, 
                     ifelse(fr == 3, lat.3,
                     ifelse(fr == 4, lat.4,
                     ifelse(fr == 5, lat.5,
                     ifelse(fr == 6, lat.6, 
                     ifelse(fr == 7, lat.7, 
                     ifelse(fr == 8, lat.8, 
                     ifelse(fr == 9, lat.9,
                     ifelse(fr == 10, lat.10, 
                     ifelse(fr == 11, lat.10,
                     ifelse(fr == 12, lat.10,
                     ifelse(fr == 13, lat.10, lat.10)))))))))))))) %>%
  mutate(team = ifelse(fr == 2, OTm,
                       ifelse(fr == 13, NTm, NA)))  %>%
  select(Rk, Player, PPG, team, long, lat, fr, OTm, NTm, Date) %>%
  arrange(Date)

labs <- flyover %>%
  ungroup() %>%
  mutate(PPG.anim = ifelse(fr <11, as.numeric(PPG) / 10, 0)) %>%
  mutate(total = cumsum(PPG.anim)) %>%
  mutate(screen = 1:n())  %>%
  mutate(scale=cut(PPG, breaks=c(0, 15, 20, 25, 30), labels=c(1,2,3,4))) %>%
  mutate(scales = as.numeric(scale)*5)
         

View(labs)
rm(df, mvmt, tran3, tran4, nbacities1, nbacities2)
