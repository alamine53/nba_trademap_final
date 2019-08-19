#install.packages("sysfonts")
#load packages
library(maps)
library(ggmap)
library(gganimate)
library(ggplot2)
library(grid)
library(magick)
library(png)
library(ggforce)
library(sysfonts)
library(transformr)
library(extrafont)
library(ggimage)
library(ggpubr)
library(ggrepel)
#load main dataset
labs
setwd("~/Documents/04_Projects/02_NBA_PlayerMap/Version_03")
#source("03_Code/01_ImportData.R")
#source("03_Code/03_ThemeBlack.R")

# determine rate and length of animation (rate = 25, length = 20)
rate <- 25
length <- 20
total <- rate * length

labs$point.size <- labs$PPG*10

 labs# image_modulate(brightness = 90) %>%
 #  image_modulate(saturation = 45) %>%
#image_modulate(hue = 100) %>%
# load empty USA map // for map without states --> map_data("usa")
usa <- map_data("usa") 
#base_family <- if ("sysfonts" %in% installed.packages()) "xkcd" else ""

# generate static map
gg0 <- ggplot() + 
  labs(
    title = "NBA Player Movements, Summer 2019", 
    subtitle = "Player",
    caption = "Creation: Ramzy Al-Amine | © courtsidecoder.home.blog" 
    ) +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group, 
                               size = 1.25
                               ), 
               fill = "white",
               alpha = 0.05,
               colour = "#dcdcd9", show.legend = FALSE) +
  theme_void() +
  annotation_custom(ATL_logo, xmin= nbacities$long_min[1], ymin = nbacities$lat_min[1], xmax = nbacities$long_max[1], ymax = nbacities$lat_max[1]) +
  annotation_custom(BOS_logo, xmin= nbacities$long_min[2], ymin = nbacities$lat_min[2], xmax = nbacities$long_max[2], ymax = nbacities$lat_max[2]) +
  annotation_custom(BRK_logo, xmin= nbacities$long_min[3], ymin = nbacities$lat_min[3], xmax = nbacities$long_max[3], ymax = nbacities$lat_max[3]) +
  annotation_custom(CHI_logo, xmin= nbacities$long_min[4], ymin = nbacities$lat_min[4], xmax = nbacities$long_max[4], ymax = nbacities$lat_max[4]) +
  annotation_custom(CHO_logo, xmin= nbacities$long_min[5], ymin = nbacities$lat_min[5], xmax = nbacities$long_max[5], ymax = nbacities$lat_max[5]) +
  annotation_custom(CLE_logo, xmin= nbacities$long_min[6], ymin = nbacities$lat_min[6], xmax = nbacities$long_max[6], ymax = nbacities$lat_max[6]) +
  annotation_custom(DAL_logo, xmin= nbacities$long_min[7], ymin = nbacities$lat_min[7], xmax = nbacities$long_max[7], ymax = nbacities$lat_max[7]) +
  annotation_custom(DEN_logo, xmin= nbacities$long_min[8], ymin = nbacities$lat_min[8], xmax = nbacities$long_max[8], ymax = nbacities$lat_max[8]) +
  annotation_custom(DET_logo, xmin= nbacities$long_min[9], ymin = nbacities$lat_min[9], xmax = nbacities$long_max[9], ymax = nbacities$lat_max[9]) +
  annotation_custom(GSW_logo, xmin= nbacities$long_min[10], ymin = nbacities$lat_min[10], xmax = nbacities$long_max[10], ymax = nbacities$lat_max[10]) +
  annotation_custom(HOU_logo, xmin= nbacities$long_min[11], ymin = nbacities$lat_min[11], xmax = nbacities$long_max[11], ymax = nbacities$lat_max[11]) +
  annotation_custom(IND_logo, xmin= nbacities$long_min[12], ymin = nbacities$lat_min[12], xmax = nbacities$long_max[12], ymax = nbacities$lat_max[12]) +
  annotation_custom(LAC_logo, xmin= nbacities$long_min[13], ymin = nbacities$lat_min[13], xmax = nbacities$long_max[13], ymax = nbacities$lat_max[13]) +
  annotation_custom(LAL_logo, xmin= nbacities$long_min[14], ymin = nbacities$lat_min[14], xmax = nbacities$long_max[14], ymax = nbacities$lat_max[14]) +
  annotation_custom(MEM_logo, xmin= nbacities$long_min[15], ymin = nbacities$lat_min[15], xmax = nbacities$long_max[15], ymax = nbacities$lat_max[15]) +
  annotation_custom(MIL_logo, xmin= nbacities$long_min[16], ymin = nbacities$lat_min[16], xmax = nbacities$long_max[16], ymax = nbacities$lat_max[16]) +
  annotation_custom(MIA_logo, xmin= nbacities$long_min[17], ymin = nbacities$lat_min[17], xmax = nbacities$long_max[17], ymax = nbacities$lat_max[17]) +
  annotation_custom(MIN_logo, xmin= nbacities$long_min[18], ymin = nbacities$lat_min[18], xmax = nbacities$long_max[18], ymax = nbacities$lat_max[18]) +
  annotation_custom(NOP_logo, xmin= nbacities$long_min[19], ymin = nbacities$lat_min[19], xmax = nbacities$long_max[19], ymax = nbacities$lat_max[19]) +
  annotation_custom(NYK_logo, xmin= nbacities$long_min[20], ymin = nbacities$lat_min[20], xmax = nbacities$long_max[20], ymax = nbacities$lat_max[20]) +
  annotation_custom(OKC_logo, xmin= nbacities$long_min[21], ymin = nbacities$lat_min[21], xmax = nbacities$long_max[21], ymax = nbacities$lat_max[21]) +
  annotation_custom(ORL_logo, xmin= nbacities$long_min[22], ymin = nbacities$lat_min[22], xmax = nbacities$long_max[22], ymax = nbacities$lat_max[22]) +
  annotation_custom(PHI_logo, xmin= nbacities$long_min[23], ymin = nbacities$lat_min[23], xmax = nbacities$long_max[23], ymax = nbacities$lat_max[23]) +
  annotation_custom(PHO_logo, xmin= nbacities$long_min[24], ymin = nbacities$lat_min[24], xmax = nbacities$long_max[24], ymax = nbacities$lat_max[24]) +
  annotation_custom(POR_logo, xmin= nbacities$long_min[25], ymin = nbacities$lat_min[25], xmax = nbacities$long_max[25], ymax = nbacities$lat_max[25]) +
  annotation_custom(SAC_logo, xmin= nbacities$long_min[26], ymin = nbacities$lat_min[26], xmax = nbacities$long_max[26], ymax = nbacities$lat_max[26]) +
  annotation_custom(SAS_logo, xmin= nbacities$long_min[27], ymin = nbacities$lat_min[27], xmax = nbacities$long_max[27], ymax = nbacities$lat_max[27]) +
  annotation_custom(TOR_logo, xmin= nbacities$long_min[28], ymin = nbacities$lat_min[28], xmax = nbacities$long_max[28], ymax = nbacities$lat_max[28]) +
  annotation_custom(UTA_logo, xmin= nbacities$long_min[29], ymin = nbacities$lat_min[29], xmax = nbacities$long_max[29], ymax = nbacities$lat_max[29]) +
  annotation_custom(WAS_logo, xmin= nbacities$long_min[30], ymin = nbacities$lat_min[30], xmax = nbacities$long_max[30], ymax = nbacities$lat_max[30]) +
theme_black(base_size = 12) +
  scale_colour_viridis_c(option = "viridis", direction = -1) +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  guides(
    # size = guide_legend(
    #   title.position = "top", 
    #   title.hjust = 0.5, 
    #   label.position = "bottom"
    # ),
    colour = "none", 
    fill = "none"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x =  element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = rel(5 / 6), hjust = 0.5),
    plot.title = element_text(size=22)
  ) 
 
gg0
gg1 <- gg0 + 
 geom_text_repel(data = labs, 
                  aes(x = long, y = lat, label = Player),
                  size = 7, hjust = 2, direction = 'x', nudge_x = 1, colour = "yellow") +
  geom_point(data = labs, 
             aes(x = long, y = lat, 
                 fill = "white", 
                 group = Player), 
             shape = 21, 
             colour = "white",
             na.rm = TRUE,
             show.legend = FALSE) +
  geom_text(data = labs, 
      aes(x = long, y = lat, 
      size = scales, 
      label = gsub(".*_", "", PPG), 
      group = Player
    ),
    show.legend = FALSE,
    colour = "black"
  ) 

gg1


# for twitter
gganimate::animate(
   plot = gg1 + 
     transition_time(screen),
   #labs(title = "Player: {Player}"),
   width = 6.3 * 100, 
   fps = rate, duration = length, 
   height = 4.7 * 100, 
   units = "px",
   bg = gg1$theme$plot.background$colour, 
   renderer = gganimate::gifski_renderer(
     file = "twitter_map.gif"
   )
  )

    
# for website
gg2 <- gg1 + 
  transition_reveal(screen)
transition_time(screen) + 
  ease_aes(x = 'sine-out', y = 'sine-out') +
  # shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolour(colour = 'black') +
  transition_states(states = screen, transition_length = 1, state_length = 1) +
  # shadow_trail(alpha = 0.3, shape = 2, maxframes = 15) + 
  #  labs(subtitle = paste("July", "{round(Date, 0)}"))+
  enter_fade() + 
  exit_shrink() 
trademap <- animate(gg2, fps = rate, duration = length, 
                    height=750, width=1100,
                    bg = gg1$theme$plot.background$colour,
                    renderer = gganimate::gifski_renderer(
                      file = "Coeos_theatres_v7.gif")
) 
trademap

View(labs)
