library(sf)
library(ggparliament)
library(MetBrewer)
library(showtext)
library(lubridate)
library(tidycensus)
library(tidyverse)

# retrieve variables that may need
vars <- load_variables(year = 2020, dataset= "acs5") %>%
  filter(geography == "block group")

# B28012_002 total population (3 and older)
# B02001_002 white; 
# B02001_003 black; 
# B02001_004 American Indian and Alaska Native; 
# B02001_005 Asian; 
# B02001_005 Naive Hawaiian and Pacific Islander
# B03003_003 Hispanic or Latino
# B19013_001 inflation adjusted median household income
# B23025_002 employment status for the adult population


# read race data from 2020 census
dc_demographics <- get_acs(
  geography = "block group",
  variables = c("B28012_002", # population
                "B02001_002","B02001_003", "B02001_004", "B02001_005", "B02001_006", # race
                "B03003_002","B03003_003", # ethnicity
                "B19013_001", "B23025_002"), # socioeconomic
  state = "DC", 
  year = 2020,
  geometry = TRUE
) 

# regroup race variable
dc_race_community <- dc_demographics %>%
  filter(grepl("B02001", variable)) %>%
  filter(!estimate == 0) %>%
  group_by(GEOID) %>%
  top_n(1, estimate) %>%
  mutate(variable = case_when(variable == "B02001_002" ~ "White",
                              variable == "B02001_003" ~ "African American",
                              T ~ "Others")) %>%
  st_transform(4326)

# read gunshot data
dc_gunshot <- read.csv("Shot_Spotter_Gun_Shots.csv") %>%
  mutate(date = as_date(DATETIME),
         year = year(date)) %>%
  filter((year == 2021) & (TYPE %in% c("Single_Gunshot", "Multiple_Gunshots", "Multiple Gunshots", "Single Gunshot"))) %>%
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")

# count gunshots within each racial community
dc_gunshot_counts <- st_join(dc_gunshot, dc_race_community, join = st_within) %>%
  as_tibble() %>%
  na.omit() %>%
  group_by(variable, GEOID) %>%
  summarise(total = n()) %>%
  ungroup() 

table(dc_gunshot_counts$variable) 
# 293 out of 555 communities experienced gunshot incidents; among these gun-exposure communities, 71.3% (209/293) are black communities


# prepare gunshot per racial group data for parliament visuals
community_gunshot <- data.frame(groups = c("White Community", "African-American Community"),
                                count = c(84, 209),
                                colors = c("#d39a2d","#591c19"))


## theme
font_add_google("Pragati Narrow")
showtext_auto()

# customize theme
theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(legend.position = "bottom",
             legend.text = element_text(color = "black",  size=35),
             
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.background = element_rect(fill = "#f5f5f2", color = NA), 
             plot.margin = margin(20, 45, 20, 45),
             panel.background = element_rect(fill = "#f5f5f2", color = NA),
             
             plot.title = element_text(
               color = "black", 
               size = 70, 
               face = "bold",
               margin = margin(t = 10),
               hjust = 0.5
             ),
             plot.subtitle = element_text(
               color = "grey10", 
               size = 50,
               lineheight = 3,
               margin = margin(t = 5, b=15),
               hjust = 0.5
             ),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(
               color = "grey20", 
               size = 30,
               lineheight = 0.3, 
               hjust = 0.5,
               margin = margin(t = 40))
)


# plot racial groups and gunshots
community_gunshot_data <- parliament_data(election_data = community_gunshot,
                                          type = "semicircle",
                                          parl_rows = 6,
                                          party_seats = community_gunshot$count)

community_gunshot_data %>%
  ggplot(aes(x=x, y=y, color=groups)) +
  geom_parliament_seats(size=8) +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_color_manual(values = community_gunshot_data$colors, limits = community_gunshot_data$groups) +
  labs(x="", y="", title = "COMMUNITY EXPOSURE TO GUN VIOLENCE",
       subtitle = "Communities that experienced incidents of gunshots in 2021",
       caption = str_wrap("Data comes from ShotSpotter gunshot detection system. Incidents of probable gunfires and firecrackers are excluded | Visualization by Zhaowen Guo", width = 200))

ggsave("gunshot-race.png", width = 14, height = 14/1.618, units = "in")
#
4/5 # 80%
289/550 # 53%

icon_path = "path://M544 64h-16V56C528 42.74 517.3 32 504 32S480 42.74 480 56V64H43.17C19.33 64 0 83.33 0 107.2v89.66C0 220.7 19.33 240 43.17 240c21.26 0 36.61 20.35 30.77 40.79l-40.69 158.4C27.41 459.6 42.76 480 64.02 480h103.8c14.29 0 26.84-9.469 30.77-23.21L226.4 352h94.58c24.16 0 45.5-15.41 53.13-38.28L398.6 240h36.1c8.486 0 16.62-3.369 22.63-9.373L480 208h64c17.67 0 32-14.33 32-32V96C576 78.33 561.7 64 544 64zM328.5 298.6C327.4 301.8 324.4 304 320.9 304H239.1L256 240h92.02L328.5 298.6zM480 160H64V128h416V160z"

hispanic = data.frame(ethnic = c("80% of Hispanic or Latino Communities", "53% of Non-Hispanic or \n Non-Latino Communities"),
                      ratio = c(40, 25),
                      path = c(icon_path,
                               icon_path))

hispanic %>% 
  e_charts(ethnic) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel = list(show=FALSE)) %>%
  e_y_axis(max=100, 
           splitLine=list(show = FALSE),
           axisTick=list(show=FALSE),
           axisLine=list(show=FALSE),
           axisLabel=list(show=FALSE)) %>%
  e_color(color = c('#811e18','grey'), background = "#f5f5f2") %>%
  e_pictorial(ratio, symbol = path, z=10, name = "",
              symbolBoundingData= 50, symbolClip= TRUE) %>% 
  e_pictorial(ratio, symbol = path, name= '', 
              symbolBoundingData= 50) %>%
  e_legend(show = FALSE) %>%
  e_grid(bottom = "35%") 
# save the graph by click on the "download" icon

library(magick)
library(ggpubr)
background <- image_read("ethnic-gun.png")
xaxis <- data.frame(xaxis = c(1, 2, 3),
                    labels = c("", "", ""))
yaxis <- data.frame(yaxis = c(1, 2, 3),
                    labels = c("", "", ""))

ggplot() +
  background_image(background) +
  geom_text(data = xaxis, aes(x = xaxis, y = 0, label = labels)) +
  geom_text(data = yaxis, aes(x = 0, y = yaxis, label = labels)) +
  labs(x="",y="") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA)) +
  annotate(geom = "text", x = 0.8, y = 0.8, label = "80% of Hispanic or Latino Communities",
           size = 15,  family = "Pragati Narrow") +
  annotate(geom = "text", x = 2.1, y = 0.8, label = "53% of Other Communities",
           size = 15,  family = "Pragati Narrow") +
  annotate(geom = "text", x = 1.45, y = 2.95, label = "COMMUNITY EXPOSURE TO GUN VIOLENCE",
           size = 26,  family = "Pragati Narrow", fontface = "bold") +
  annotate(geom = "text", x = 1.45, y = 2.82, label = "Communities that experienced inidents of gunshots in 2021",
           size = 19,  family = "Pragati Narrow") +
  annotate(geom = "text", x = 1.5, y = 0, label = "Data comes from ShotSpotter gunshot detection system. Incidents of probable gunfires and firecrackers are excluded | Visualization by Zhaowen Guo",
           size = 12,  family = "Pragati Narrow",color = "grey20")

ggsave("gunshot-ethnicity.png", width = 14, height = 14/1.618, units = "in")
