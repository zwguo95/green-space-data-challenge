library(showtext)
library(lubridate)
library(osmdata)
library(sf)
library(tidyverse)
library(MetBrewer)

gunshot <- read_csv("Shot_Spotter_Gun_Shots.csv")
gunshot <- gunshot %>%
  mutate(date = as_date(DATETIME),
         year = year(date)) %>%
  filter((year == 2021) & (TYPE %in% c("Single_Gunshot", "Multiple_Gunshots", "Multiple Gunshots", "Single Gunshot")))

city <- getbb("Washington, District of Columbia")

streets <- opq(city) %>%
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "teritiary", "residential")) %>%
  osmdata_sf()

rivers <- opq(city) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

greens <- opq(city) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "golf_course")) %>%
  osmdata_sf()



# theme
font_add_google("Pragati Narrow")
showtext_auto()


# customize theme
theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(
  
  # axis
  text = element_text(size = 45),
  
  # titles
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(20, 40, 20, 40),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  
  #legend.title.align=1,
  plot.title = element_text(
    color = "black", 
    size = 70, 
    face = "bold",
    margin = margin(t = 10),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "grey10", 
    size = 45,
    lineheight = 3,
    margin = margin(t = 5, b = 30),
    hjust = 0.5
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey20", 
    size = 35,
    lineheight = 0.5, 
    hjust = 0.5,
    margin = margin(t = 30))
)


ggplot() +
  #geom_sf(data = borders$osm_lines, inherit.aes = FALSE, color = "grey25", alpha = 0.5, size = 0.2) +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "grey25", alpha = 0.5, size = .2) +
  geom_sf(data = greens$osm_polygons, inherit.aes = FALSE, colour = "#47632a", fill = "#47632a", alpha = .5, size = 1) +
  geom_point(data = gunshot, aes(x = LONGITUDE, y = LATITUDE), color = "#c62320", size = 0.1, inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326), xlim = c(-77.12, -76.90), ylim = c(38.79, 39.01), expand = F) +
  labs(x = "", y = "", title = "GUNSHOT DETECTION MAP",
       subtitle = "Recorded shooting incidents in Washington D.C. during 2021",
       caption = str_wrap("Data comes from ShotSpotter gunshot detection system. Incidents of probable gunfires and firecrackers are excluded. Green spaces include parks, nature reserves, and golf courses | Visualization by Zhaowen Guo", width = 300))

ggsave("equity-gunshot-dc-map.png", width = 14, height = 14/1.618, units = "in")


