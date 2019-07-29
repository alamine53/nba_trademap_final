#install.packages("gifski")
#load packages
library(maps)
library(ggmap)
library(gganimate)
library(ggplot2)
library(grid)
library(magick)
#load main dataset
labs <- flyover %>%
  ungroup() %>%
  mutate(WS.anim = ifelse(fr <11, as.numeric(WS) / 10, 0)) %>%
  mutate(total = cumsum(WS.anim)) %>%
  mutate(screen = 1:n())

labs
# load empty USA map // for map without states --> map_data("usa")
usa <- map_data("usa") 

gg11  <- ggplot() + 
  labs(title = "NBA Free Agent Tracker, 2019", 
       subtitle = "This is map of player movements that occurred during the 2019 summer free agency period.",
       caption = "Data: BBRef | Creation: Ramzy Al-Amine | courtsidecoder.home.blog") + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_label(data = nbacities, aes(x = long, y = lat, label = names), colour = "#22211d", fontface = "bold", show.legend = FALSE) + 
  theme_void() +
  theme(text = element_text(color = "#22211d"),  # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=100, unit = "cm") ),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.09)) 
gg11


# generate map using flyover data (no images)
gg2 <- gg11 + 
  geom_point(data = labs, aes(x = long, y = lat), shape = 21, color = "black", fill = "pink", size = 2) +
  geom_label(data = labs, aes(x = long, y = lat, label = Player, fill = "f5f5f2"), colour = "#22211d", fontface = "bold", show.legend = FALSE) 
  #  geom_text(data = labs, aes(x = long, y = lat, label = Player), vjust = 0, nudge_y = -1, hjust = 0, nudge_x = -3,colour = "black")

gg2

# animate gg2
ggm <- gg2 +
  transition_time(screen) + 
  #labs(title = paste("Day", "{round(frame_time,0)}")) +
  shadow_wake(wake_length = 0)

# create map object
trademap <- animate(ggm, fps = 24, duration = 20, height=450, width=600) 
anim_save("trademap.gif", tradebar, path = NULL)

# create bar plot
p <- ggplot(data=labs, aes(x=1, y=total, colour = Player), show.legend = FALSE) +
  geom_bar(stat='identity', position = "identity", width = 0.001, show.legend = FALSE, fill = "#FF6666") +
  geom_label(aes(label = total, colour = "#black", fontface = "bold")) + 
  theme(text = element_text(color = "black"),  # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=100, unit = "cm") ),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position="none") 
p

# animate
ggbar <- p +
  transition_time(screen) +
  shadow_wake(wake_length = 0)
tradebar <- animate(ggbar, fps = 24, duration = 20, height=450, width=60)
anim_save("tradebar.gif", tradebar, path = NULL)

trademap
tradebar
save.image(file='myEnvironment.RData')

# combine animations
a <- image_read(trademap)
b <- image_read(tradebar)

d<-image_blank(450,660)

the_frame<-d
for(i in 2:440){
  the_frame<-c(the_frame,d)
}

final_gif<-image_append(c(a[1], b[1]))
for(i in 2:440){
  combined <- image_append(c(a[i], b[i]))
  final_gif<-c(final_gif,combined)
}

final_gif
