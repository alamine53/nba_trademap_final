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
library(extrafont)
library(gtable)
setwd("~/Documents/04_Projects//02_NBA_PlayerMap/Version_04")
source("01_ImportData.R")
setwd("~/Documents/04_Projects//02_NBA_PlayerMap/Version_04")
source("02_ImportTeamLogos.R")
source("03_ThemeBlack.R")
setwd("~/Documents/04_Projects//02_NBA_PlayerMap/Version_04")
labs$total <- round(labs$total, digits = 1)

usa <- map_data("usa") 
nba_map <- ggplot() + 
  labs(
    title = "NBA Off-season Transactions", 
    caption = "Creation: Ramzy Al-Amine | ramzyalamine.com") +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group, size = 3), 
               alpha = 0.1, colour = "#dcdcd9", show.legend = FALSE) +
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
  guides( colour = "none",  fill = "none") +
  theme_black(base_size = 12) +
  theme(axis.text.y = element_blank(),
    axis.text.x =  element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 15, hjust = 1),
    plot.title = element_text(family="Arial", size=30, face = "bold"),
    legend.text=element_text(family="Helvetica"),
    text=element_text(family="Arial")) + 
  geom_text(data = labs, 
            aes(x = long, y = lat, label = Player),
            size = 8, vjust = -1, nudge_x = 0, colour = "aquamarine1", family = "Arial", fontface = "bold") +
  geom_point(data = labs, 
             aes(x = long, y = lat, size = PPG), 
             shape = 21, colour = "black", fill = "aquamarine1", stroke = 2,  na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = labs, mapping = aes(x = -122, y = 25, label = "Total PPG:", fontface = "bold"),
            colour = "aquamarine1", size = 8, hjust = 0, family="Arial") +
  geom_text(data = labs, mapping = aes(x = -108, y = 25, label = round(total, digits = 1)),
            colour = "aquamarine1", size = 8, hjust = 0, family="Arial", fontface = "bold")  
nba_map

# create bar plot
nba_bar <- ggplot(data=labs, aes(x=1, y=total, colour = Player), show.legend = FALSE) +
  geom_bar(stat='identity', position = "identity", width = 0.001, show.legend = FALSE) +
  geom_label(aes(label = round(total, digits = 1))) + 
  theme_void()+
  theme_black(base_size = 12) +
  theme(text = element_text(color = "black"),  
        # plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        #  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=100, unit = "cm") ),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position="none") 
nba_bar

# assemble
map_table <- ggplotGrob(nba_map)
bar_table <- ggplotGrob(nba_bar)
composite <- gtable(widths = unit(c(3, 0.15, 0.2, 0), 'null'), 
                    heights = unit(c(0.15, 0.6, 0.15, 1.4), 'null'), 
                    respect = TRUE)
composite <- gtable_add_grob(composite, map_table, 1, 1, 4, 2)
composite <- gtable_add_grob(composite, bar_table, 1,3,4,4)
grid.newpage()
grid.draw(composite)


# ------------------------ until here the code works ------------------------ #
map_anim <- animate(nba_map + transition_time(screen), fps = rate, duration = length)
bar_anim <- animate(nba_bar + transition_time(screen) , fps = rate, duration = length)


bar_anim
map_anim

#animate
map_anim <- function(plot, t) {
  has_appeared <- seq_along(screen) <= t
  plot$nba_map
  ggplot_gtable(plot)
}
for (f in seq_len(rate) -1) {
  t <- f / (rate - 1)
grid.newpage()
grid.draw(map_anim(nba_map, 0.1))
}

rate <- 15
length <- 15
for (f in seq_len(rate) -1) {
  t <- f / (rate - 1)
  grid.draw()
}

map_table <- ggplot_gtable(map_anim)
bar_table <- ggplotGrob(bar_anim)
composite <- gtable(widths = unit(c(3, 0.15, 0.2, 0), 'null'), 
                    heights = unit(c(0.15, 0.6, 0.15, 1.4), 'null'), 
                    respect = TRUE)
composite <- gtable_add_grob(composite, map_anim, 1, 1, 4, 2)
composite <- gtable_add_grob(composite, bar_anim, 1,3,4,4)
grid.newpage()
grid.draw(composite)
grid.newpage()
grid.draw(map_anim)


composite <- gtable(widths = unit(c(1.8, 0.15, 0.2, 0.15), 'null'), 
                    heights = unit(c(0.15, 0.6, 0.15, 1.4), 'null'), 
                    respect = TRUE)
for (f in seq_len(nframes) - 1) {
  tmp_comp <- composite
  t <- f / (nframes - 1)
  d_table <- D_anim(D_b, t, edge_order, edge_stagger, edge_draw_time)
  i_dot_table <- i_dot_anim(i_dot_b, t, node_order_i, edge_order_i, node_stagger_i, edge_draw_time)
  i_stem_table <- i_stem_anim(i_stem_b, t, fill_order, stroke_order, node_stagger_stem, node_draw_time)
  tmp_comp <- gtable_add_grob(tmp_comp, d_table, 1, 1, 4, 2)
  tmp_comp <- gtable_add_grob(tmp_comp, i_dot_table, 1, 2, 3, 4)
  tmp_comp <- gtable_add_grob(tmp_comp, i_stem_table, 4, 3)
  grid.newpage()
  grid.draw(tmp_comp)