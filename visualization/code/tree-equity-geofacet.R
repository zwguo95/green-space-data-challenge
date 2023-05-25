library(sf)
library(geofacet)
library(showtext)
library(tidyverse)
states <- read.csv("state-names.csv") # downloaded https://worldpopulationreview.com/states/state-abbreviations

font_add_google("Pragati Narrow")
showtext_auto()

# prepare a function to read zip urls with shapefiles 
read_shape_URL <- function(URL){
  cur_tempfile <- tempfile()
  download.file(url = URL, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  read_sf(dsn = out_directory)
}

url <- paste0("https://national-tes-data-share.s3.amazonaws.com/national_tes_share/", "al", ".zip.zip")
map <- read_shape_URL(url) %>% select(tes, state, priority)

data_lists <- list()
for (i in 1:nrow(states)){
  state <- states$lower_code[i]
  print(state)
  URL <- paste0("https://national-tes-data-share.s3.amazonaws.com/national_tes_share/", state, ".zip.zip")
  if (RCurl::url.exists(URL) == T) {
    map <- read_shape_URL(paste0("https://national-tes-data-share.s3.amazonaws.com/national_tes_share/", state, ".zip.zip"))
    data_lists[[i]] <- map %>% select(tes, state, priority)
  }
}
data <- do.call(rbind, data_lists)
saveRDS(data, "data.RDS")
data <- readRDS("data.RDS")

data <- na.omit(data)
summary(data$tes)

ggplot(data) +
  #geom_segment(aes(x = 1.5, xend = 100.5, y = -0.1, yend = -0.1), size = 0.2, stat = "unique") +
  #geom_text(data = freq_lab, aes(x = x, y = -0.5, label = x), vjust = 1, size = 1.5, color = "#23466E90") +
  geom_density(aes(x = tes), color = "#466c4b", fill = "#7fa074", alpha = 0.5) +
  coord_cartesian(clip = "off") +
  facet_geo(vars(state), scales = "free_y", grid = us_state_grid1[c(-2, -11), ], label = "name") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "", y = "",
       title = "ACCESS TO GREEN SPACE",
       subtitle = "Distribution of tree equity scores across census blocks in the US",
       caption = str_wrap("
       Tree Equity Score (TES) computes how much tree canopy and surface temperature align with income, 
       employment, race, age and health factors in the US, collected by American Forest | Visualization by Zhaowen Guo", width = 300)) +
  theme_void(base_family = "Pragati Narrow") +
  theme(strip.text = element_text(face = "bold", color = "grey20", size = 30),
        legend.position = "none",
        axis.text = element_text(color = "grey40", size = 30),
        strip.background = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.margin = margin(40, 15, 20, 15),
        plot.title = element_text(face = "bold", size = 70, margin = margin(l=0, t=5)),
        plot.subtitle = element_text(lineheight = 1, size = 50, margin(l=0, t=7)),
        plot.caption = element_text(margin = margin(t=35), color = "grey20", size = 30),
        plot.caption.position = "plot")

ggsave("tree-equity-geofacet.png", dpi = 320, width = 14, height = 10)



