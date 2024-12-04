library(sf)
library(tidyverse)
library(ggrepel)
library(egg)
library(raster)
library(ggthemes)
library(ggsn)

Erie <- st_read("./data/Map/Lake_Erie_Shoreline.shp")
sites<- read.csv("./data/Map/sites.csv")

st_geometry_type(Erie)
st_crs(Erie)
st_bbox(Erie)

# SB <- st_crop(Erie, xmin = -83.5, xmax = -82.6,
#               ymin = 41.39, ymax = 41.55)

ggplot(Erie) + 
  geom_sf(color = "black", 
          fill = "light grey",
          lwd = 1) + 
  ggtitle("Sandusky Bay Sampling Locations") + 
  coord_sf(expand = TRUE) + 
  theme_article()+
geom_point(data = sites, 
           aes(x = Longitude, 
               y = Latitude), 
           col = "black", 
           size = 1)+
  xlim(-83.070, -82.6)+
  ylim(41.42, 41.55)+
geom_label_repel(data = sites,
                 aes(x = Longitude, y = Latitude, 
                     label = site))+
                 # size = 2,
                 # box.padding = 0.5,
                 # force_pull   = 0,
                 # color = "black",
                 # direction = "y",
                 # angle = 0,
                 # hjust = 0.5,
                 # vjust = 0.5,
                 # segment.colour ="black",
                 # segment.size = .5,
                 # arrow = arrow(length = unit(1, "mm"),
                               # type = "closed"),
                 # nudge_y = .03)+
  # scalebar(location = "bottomright", dist = 2,
  #          dist_unit = "km",
  #          transform = TRUE, model = 'WGS84',           
  #          x.min = -83.070, x.max = -82.6, 
  #          y.min = 41.42, y.max = 41.55) +
  # north(x.min = -83.070, x.max = -82.6, 
  #       y.min = 41.42, y.max = 41.55,
  #       location = "topleft", scale = .15)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
        # axis.title.x = element_text(size = 5),
        # axis.title.y = element_text(size = 5),
        # axis.text.x = element_text(size = 4),
        # axis.text.y = element_text(size = 4),
        # plot.title = element_text(size = 6))
  



ggsave("Sandusky_Bay.jpeg", width = 5, height = 3)


 
