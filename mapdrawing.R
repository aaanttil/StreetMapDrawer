library(tidyverse)
library(osmdata) 
library(showtext) 
library(ggmap)
#library(rvest)
library(sf)
library(sjPlot)
library(tmap)
#install.packages("tmap")
#remotes::install_github('r-tmap/tmap')

drawmap <- function(place_name, mapsize = 0, v.shift = 0, h.shift = 0, street.tier = 3, street.col=c(1,1,1), bg.col = "white", water.col = "lightgrey" ,coastline.col = "black", rails.col="black") {
  mb <- getbb(place_name)
  if (mapsize != 0){
    newbb <- mb
    newbb[1,] <- c(mean(mb[1,]) - mapsize + h.shift, mean(mb[1,]) + mapsize + h.shift)
    newbb[2,] <- c(mean(mb[2,]) - mapsize + v.shift, mean(mb[2,]) + mapsize + v.shift)
    mb <- newbb
  }
  if (street.tier == 3) {
    str.t <- c("trunk","motorway", "primary", "motorway_link", "primary_link",
               "secondary", "tertiary", "secondary_link", "tertiary_link", "track", "path",               
               "unclassified", "residential", "service", "living_street")
  }
  
  if (street.tier == 2) {
    str.t <- c("trunk","motorway", "primary", "motorway_link", "primary_link",
               "secondary", "tertiary", "secondary_link", "tertiary_link")
  }
  if (street.tier == 1) {
    str.t <- c("trunk","motorway", "primary", "motorway_link", "primary_link")
  }
  
  water <- opq(bbox = mb, timeout=600) %>%
    add_osm_features(features = list (
      "natural" = "water",
      "natural" = "wetland",
      "waterway" = "riverbank",
      "waterway" = "stream",
      "landuse" = "reservoir",
      "landuse" = "basing",
      "natural" = "bay"
    )) %>%  osmdata_sf()
  
  
  rails <- opq(bbox = mb, timeout=600) %>%
    add_osm_feature(key = 'railway', value = c("rail","light_rail")) %>%  osmdata_sf()
  streets <- opq(bbox = mb, timeout=600) %>%
    add_osm_feature(key = 'highway', value = str.t) %>%  osmdata_sf()
  
  street_tier <- ifelse(streets$osm_lines$highway %in% 
                          c("trunk","motorway", "primary","residential" ,"motorway_link", "primary_link"), 3,
                        ifelse(
                          streets$osm_lines$highway %in% 
                            c("secondary", "tertiary", "secondary_link", "tertiary_link"), 2, 1))
  
  
  coasts <- opq(bbox = mb, timeout=600) %>%
    add_osm_feature(key='natural', value='coastline') %>%  osmdata_sf()
  
  ggplot() +
    geom_sf(data = streets$osm_lines,
            aes(color = factor(street_tier), size = street_tier)) +
    scale_color_manual(values = street.col) + 
    guides(color = guide_none(), size = guide_none()) +
    geom_sf(data = coasts$osm_lines, color = coastline.col) + 
    geom_sf(data = rails$osm_lines, color = rails.col) + 
    geom_sf(data = water$osm_multipolygons, fill=water.col) +
    xlim(mb[1,1], mb[1,2]) + 
    ylim(mb[2,1], mb[2,2]) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = bg.col)) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),  
      axis.ticks = element_blank(),   
      plot.margin = unit(c(1, 0, 1, 0), "lines"),
      plot.caption = element_text(hjust=0.5, face="bold", size=15),
      plot.caption.position = "plot",
      plot.subtitle = element_text(hjust=0.5, size=10)
    ) +
    labs(
      x = NULL, 
      y = NULL,  
      caption = place_name  
    )
}

