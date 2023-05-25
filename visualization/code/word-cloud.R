
library(magick)
library(ggpubr)
background <- image_read("park_reviews.png")

xaxis <- data.frame(xaxis = c(1, 2, 3),
                    labels = c("", "", ""))
yaxis <- data.frame(yaxis = c(1, 2, 3),
                    labels = c("", "", ""))

library(showtext)
font_add_google("Pragati Narrow")
showtext_auto()

ggplot() +
  background_image(background) +
  geom_text(data = xaxis, aes(x = xaxis, y = 0, label = labels)) +
  geom_text(data = yaxis, aes(x = 0, y = yaxis, label = labels)) +
  labs(x="",y="") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA))  +
  annotate(geom = "text", x = 2, y = 2.95, label = "URBAN PARKS IN CIVIC LIFE",
           size = 30,  family = "Pragati Narrow", fontface = "bold") +
  annotate(geom = "text", x = 2, y = 2.87, label = "Online reviews of parks at Washington DC",
           size = 25,  family = "Pragati Narrow")
ggsave("dc-park-reviews.png", width = 10, height = 14, units = "in")
