library(sf)
library(geojsonio)
library(rgeos)
library(scico)
library(showtext)
library(tidyverse)
states <- read.csv("state-names.csv") # downloaded https://worldpopulationreview.com/states/state-abbreviations

# prepare a function to read zip urls with shapefiles 
read_shape_URL <- function(URL){
  cur_tempfile <- tempfile()
  download.file(url = URL, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  read_sf(dsn = out_directory)
}

# extract tree equity score gap: max(tes) - min(tes)
state_names <- rep(NA, 51)
tes_gaps <- rep(NA, 51)
for (i in 1:nrow(states)){
  state <- states$lower_code[i]
  state_names[i] <- state
  print(state)
  URL <- paste0("https://national-tes-data-share.s3.amazonaws.com/national_tes_share/", state, ".zip.zip")
  if (RCurl::url.exists(URL) == T) {
    map <- read_shape_URL(paste0("https://national-tes-data-share.s3.amazonaws.com/national_tes_share/", state, ".zip.zip"))
    tes_gap <- max(map$tes) - min(map$tes)
    tes_gaps[i] <- tes_gap
  } else {
    tes_gaps[i] <- NA
  }
}

data <- data.frame(lower_code = state_names,
                   gap = tes_gaps) %>%
  cbind(states[1])


font_add_google("Pragati Narrow")
showtext_auto()

# create a base hexbin map of US
hex_states <- geojson_read("us_states_hexgrid.geojson", what = "sp") 

# extract state names
hex_states@data <- hex_states@data %>%
  mutate(google_name = str_replace(google_name, " \\(United States\\)", ""))

# create data frame for hexbin map
hex_states_fortify <- broom::tidy(hex_states, region = "google_name")

# match state names
data_map <- hex_states_fortify %>%
  right_join(data, by = c("id" = "state")) %>%
  mutate(id = state.abb[match(id, state.name)])
data_map$id[data_map$group == "District of Columbia.1"] <- "DC"

labels <- cbind.data.frame(data.frame(gCentroid(hex_states, byid = T),
                                      id = hex_states@data$iso3166_2))
data_map <- data_map %>%
  right_join(labels, by = "id") %>%
  filter(is.na(gap) == F)


# customize theme
theme_set(theme_minimal(base_family = "Pragati Narrow"))

theme_update(
  # legend
  legend.title = element_blank(),
  legend.position = 'top',
  legend.direction = 'horizontal',
  legend.key.width = unit(1.5, "cm"),
  legend.text = element_text(color = "black",  size=30),
  
  # axis
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  
  # titles
  panel.grid = element_blank(),
  plot.margin = margin(15, 30, 15, 30),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  legend.title.align=1,
  plot.title = element_text(
    color = "black", 
    size = 70, 
    face = "bold",
    margin = margin(t = 15),
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    color = "grey10", 
    size = 45,
    lineheight = 3,
    margin = margin(t = 5),
    hjust = 0.5
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey20", 
    size = 20,
    lineheight = 0.5, 
    hjust = 0.5,
    margin = margin(t = 40))
)

data_map %>% 
  ggplot () +
  geom_polygon(aes(x = long, y = lat, group = group, fill = gap), linewidth = 0.5) +
  scale_fill_scico(palette = "lajolla", direction = 1) + 
  geom_text(aes(x=x, y=y, label=id, color = gap < 60), size = 8, alpha = 0.5, 
             show.legend = F) +
  scale_color_manual(values = c("white", "black")) +
  coord_map(clip = "off") +
  labs(title = "TREE EQUITY GAP",
       subtitle = "Block-level disparities in tree equity scores within each state",
       x = "", y = "",
       caption= 
         str_wrap(
       "Data comes from the Green Space Data Challenge, 
       collected and shared by the American Forests. 
       Tree Equity Score (TES) computes how much tree canopy and surface temperature align with income, 
       employment, race, age and health factors in the U.S | Visualization by Zhaowen Guo", width=150))
ggsave("tree_equity.png", dpi = 320, width = 7, height = 6)


