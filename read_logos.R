setwd("~/Documents/04_Projects/2019/02_NBA_TradeAnimation")
setwd("~/Desktop/03_NBALogos")

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
img <- list()
for(s in teams) {
img[[s]] <- image_read(paste0(s,".png")) %>%
  image_scale("35x35!") %>%
  image_blur(10,0) %>%
  image_modulate(brightness = 90) %>%
  image_modulate(saturation = 0) %>%
  image_modulate(hue = 60) %>%
  image_convert("png")
}


img[["ATL"]]
# load empty USA map // for map without states --> map_data("usa")
usa <- map_data("usa") 


gg11  <- ggplot() + 
  labs(title = "NBA Player Movements, 2019", 
       subtitle = "Trades and free agent signings that involved big names during the 2019 summer free agency period.",
       caption = "Data: BBRef | Creation: Ramzy Al-Amine | courtsidecoder.home.blog") + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "#dcdcd9", colour = "#f5f5f2") +
  geom_text(data = nbacities, aes(x = long, y = lat, label = names), 
             colour = "#4e4d47", alpha = 0.5, fontface = "bold", show.legend = FALSE) + 
        theme_void() +
        theme(text = element_text(color = "#22211d"),  
       # plot.background = element_rect(fill = NULL, color = NA), 
        #panel.background = element_rect(fill = NULL, color = NA), 
        #legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=100, unit = "cm") ),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.09)) 
gg11

pic
