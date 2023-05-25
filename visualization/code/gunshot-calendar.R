library(showtext)
library(lubridate)
library(tidyverse)
gunshot <- read_csv("Shot_Spotter_Gun_Shots.csv")

unique(gunshot$TYPE)

gunshot_daily <- gunshot %>%
  mutate(date = as_date(DATETIME),
         year = year(date),
         light = as.character(light)) %>%
  filter((year == 2021) & (TYPE %in% c("Single_Gunshot", "Multiple_Gunshots", "Multiple Gunshots", "Single Gunshot"))) %>%
  group_by(date) %>%
  summarise(shots = n()) %>%
  ungroup() %>%
  mutate(week_day = str_sub(weekdays(date), 1, 3),
         month_day = day(date),
         month = month(date),
         week_start = ifelse(month_day == 1 | week_day == "Sun", 1, 0)) %>%
  group_by(month) %>%
  mutate(week = cumsum(week_start),
         month_name = months(date)) %>%
  ungroup() %>%
  mutate(shots_range = case_when(shots <= 10 ~ "1",
                                 shots >10 & shots <= 20 ~ "2",
                                 shots >20 ~ "3"))

week_day_code <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
gunshot_daily$week_day <- factor(gunshot_daily$week_day, levels = week_day_code)
month_code <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 
gunshot_daily$month_name <- factor(gunshot_daily$month_name, levels = month_code)


# theme
font_add_google("Dancing Script")
showtext_auto()


# customize theme
theme_set(theme_minimal(base_family = "Dancing Script"))

theme_update(
  # legend
  legend.title = element_blank(),
  legend.position = 'bottom',
  legend.direction = 'horizontal',
  legend.key.width = unit(1.5, "cm"),
  legend.text = element_text(color = "black",  size=35),
  legend.box.margin = margin(t = 35),
  legend.spacing.x = unit(1, "cm"),
  legend.spacing.y = unit(0.5, "cm"),
  
  # axis
  axis.text.y = element_blank(),
  axis.text.x = element_text(vjust = 50),
  text = element_text(size = 40),
  strip.text.x = element_text(size = 43, margin = margin(b = 25)),
  
  # titles
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(20, 50, 20, 50),
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


data_daily %>%
  ggplot(aes(x = week_day, y = week)) +
  geom_tile(aes(fill = light), color = "white") + 
  scale_fill_manual(values = MetBrewer::met.brewer("Tam", n=2),
                    labels = c("below 10", "over 20"),
                    guide = guide_legend(label.position = "bottom", nrow = 1)) +
  facet_wrap(~month_name, scales = "free") +
  scale_y_reverse() +
  scale_x_discrete(position = "bottom") +
  labs(x = "", y = "", title = "GUNSHOT DETECTION CALENDAR",
       subtitle = "Recorded shooting incidents in Washington D.C. during 2021",
       caption = str_wrap("Data comes from ShotSpotter gunshot detection system. Incidents of probable gunfires and firecrackers are excluded | Visualization by Zhaowen Guo", width = 300))

library(ggview)
ggview(width = 14, height = 14/1.168, units = "in")
ggsave("gunshot-calendar.png", width = 14, height = 14/1.618, units = "in")

