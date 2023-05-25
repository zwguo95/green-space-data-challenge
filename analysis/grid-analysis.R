options(sciphen=999)
library(tidyverse)
data <- read.csv("grid-data.csv")
library(showtext)
font_add_google("Pragati Narrow")
showtext_auto()

# convert variables for effects to be seen
data <- data %>% filter(park == 0)
data$is_black <- ifelse(data$ward %in% c(7, 8), 1, 0)
data$small_park <- ifelse(data$area_of_nearest_park < 500000, 1, 0)
data$park_area <- log(data$area_of_nearest_park)

m1 <- glm(gunshot ~ meter_to_park + meter_to_police + streetlight_count + ntl + population, data = data, family = "binomial")
summary(m1)

# build logit model 
library(simcf)
model <- gunshot ~ park_area*is_black + meter_to_police + streetlight_count + ntl + population
mdata <- extractdata(model, data, na.rm=TRUE)

# run logit 
logit.result <- glm(model, family = "binomial", data = mdata)

# extract pe and vc
pe.glm <- logit.result$coefficients
vc.glm <- vcov(logit.result)

# simulate betas under multivariate normal distribution
sims <- 1000
sim.betas <- MASS::mvrnorm(sims, pe.glm, vc.glm)

# specify counterfactuals 
GreenSpace.hyp <- seq(min(mdata$park_area), max(mdata$park_area))
nscen <- length(GreenSpace.hyp)
blackScen <- nonblackScen <- cfMake(model, mdata, nscen)

# research question 1: expected probabilities for voting by age and marital status
for (i in 1:nscen) {
  # Married (loop over each age)
  blackScen <- cfChange(blackScen, "park_area", x = GreenSpace.hyp[i], scen = i)
  blackScen <- cfChange(blackScen, "is_black", x = 1, scen = i)
  
  # Not Married (loop over each age)
  nonblackScen <- cfChange(nonblackScen, "park_area", x = GreenSpace.hyp[i], scen = i)
  nonblackScen <- cfChange(nonblackScen, "is_black", x = 0, scen = i)
}

# Simulate expected probabilities for all scenarios
blackSims <- logitsimev(blackScen, sim.betas, ci=0.95)
nonblackSims <- logitsimev(nonblackScen, sim.betas, ci=0.95)

# make tibble for visualization
blackSims.tbl <- 
  blackSims %>%
  bind_rows() %>%
  mutate(GreenSpace = GreenSpace.hyp,
         black = "1")

nonblackSims.tbl <- 
  nonblackSims %>%
  bind_rows() %>%
  mutate(GreenSpace = GreenSpace.hyp,
         black = "0")

allSims.tbl <- bind_rows(blackSims.tbl, nonblackSims.tbl)

# visualize expected difference
ggplot(allSims.tbl, aes(x = GreenSpace, y = pe, ymin = lower, ymax = upper, color = black, fill = black)) +
  geom_line(linetype = "dashed") +
  geom_ribbon(alpha = 0.5) +
  scale_color_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") + 
  scale_fill_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") +
  labs(x = "Size of the Nearest Park", y = "Probability of Exposure to Gun Violence",
       title = "The Relationship between Green Space and Gun Violence Exposure",
       subtitle = paste0("Expected probabilities of exposure to gun violence in relation to the size of parks at the 0.001", " x 0.001", " grid level")) +
  theme_minimal(base_family = "Pragati Narrow") +
  theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        axis.text = element_text(size = 35, color = "grey10"),
        axis.title = element_text(size = 35, color = "grey10"),
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
        legend.position = "bottom",
        legend.text = element_text(color = "black",  size=35)) 


ggsave("logistic_regression_size_park.png", width = 14, height = 14/1.618, units = "in")

