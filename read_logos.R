setwd("~/Documents/04_Projects/2019/02_NBA_TradeAnimation")
setwd("~/Desktop")

#install.packages("ggmap")
#install.packages("maps")
#install.packages("magick")

library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(maps)
library(ggmap)
library(gganimate)
library(ggplot2)
library(magick)

url_csv2 <- 'https://raw.githubusercontent.com/alamine53/nbatrademap/master/raw_NbaCities.csv'
nbacities <- read_csv(url(url_csv2))
saveRDS(nbacities, file = "nbacities")

setwd("C:/Users/ralamine/Desktop/03_NBALogos")

teams <- c("ATL","BOS","BRK","CHI","CHO","CLE",
           "DAL", "DEN", "DET", "GSW", "HOU", "IND",
           "LAC", "LAL", "MEM", "MIA", "MIN", "NOP",
           "NYK", "OKC", "ORL", "PHI", "PHO", "POR",
           "SAC", "SAS", "TOR", "UTA", "WAS")

for(s in teams) {
pic <- image_read(paste0(s,".png")) %>%
  image_scale("35x35!") %>%
  image_blur(10,0) %>%
  image_modulate(brightness = 90) %>%
  image_modulate(saturation = 0) %>%
  image_modulate(hue = 60)
}

pic
