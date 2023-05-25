options(sciphen = 999)
library(sf)
library(MetBrewer)
library(showtext)
library(lubridate)
library(tidycensus)
library(tidyverse)

font_add_google("Pragati Narrow")
showtext_auto()

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


# read demographics data from 2020 census
demographics <- get_acs(
  geography = "block group",
  variables = c("B28012_002", # population
                "B02001_002","B02001_003", "B02001_004", "B02001_005", "B02001_006", # race
                "B03003_002","B03003_003", # ethnicity
                "B19013_001", "B23025_002"), # socioeconomic
  state = "DC", 
  year = 2020,
  geometry = TRUE
) 
  
# widen demographics data
block_pop <- demographics %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  select(-moe) %>%
  replace(is.na(.), 0) %>%
  group_by(GEOID) %>%
  summarise(across(B02001_002:B28012_002, sum)) %>%
  ungroup() %>%
  st_transform(4326)

# construct racial group variable
black_community <- demographics %>%
  filter(grepl("B02001", variable)) %>%
  filter(!estimate == 0) %>%
  group_by(GEOID) %>%
  top_n(1, estimate) %>%
  filter(variable == "B02001_003")

hispanic_community <- demographics %>%
  filter(grepl("B03003", variable)) %>%
  filter(!estimate == 0) %>%
  group_by(GEOID) %>%
  top_n(1, estimate) %>%
  filter(variable == "B03003_003")

# read green space data per block
green <- read.csv("green_space_block.csv") %>%
  select(GreenSpace_Percent, GreenSpace_PerCapita, bgrp, Outside_500m_population_percent) %>%
  rename(GEOID = bgrp) %>%
  mutate(GEOID = as.character(GEOID))

# read tree equity score
tree <- read.csv("tree_equity_score.csv") %>%
  select(geoid, tes) %>%
  rename(GEOID = geoid) %>%
  mutate(GEOID = as.character(GEOID))

# read park data
parks_locations <- readRDS("parks_locations.RDS")

# read gunshot data
gunshot <- read.csv("Shot_Spotter_Gun_Shots.csv") %>%
  mutate(date = as_date(DATETIME),
         year = year(date)) %>%
  filter((year == 2021) & (TYPE %in% c("Single_Gunshot", "Multiple_Gunshots", "Multiple Gunshots", "Single Gunshot"))) %>%
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")

# count gunshots within each block
gunshot_counts <- st_join(gunshot, block_pop, join = st_within) %>%
  as_tibble() %>%
  drop_na(GEOID) %>%
  group_by(GEOID) %>%
  mutate(gunshots = n(),
         gunshots_percap = gunshots/B28012_002) %>%
  ungroup() 

# count parks within each block
park_counts <- st_join(parks_locations, block_pop, join = st_within) %>%
  as_tibble() %>%
  drop_na(GEOID) %>%
  group_by(GEOID) %>%
  summarise(small_parks = sum(USE_TYPE == "SMALL PARK"),
         parks = sum(USE_TYPE == "PARK"),
         large_parks = sum(USE_TYPE == "LARGE PARK"),
         total_parks_recreation = n()) %>%
  ungroup() 

# combine all data
data <- gunshot_counts %>%
  select(GEOID, gunshots, gunshots_percap) %>%
  full_join(green) %>%
  full_join(park_counts) %>%
  full_join(tree) %>%
  full_join(block_pop) %>%
  unique() %>%
  mutate(gunshots = ifelse(is.na(gunshots), 0, gunshots),
         gunshots_percap = ifelse(is.na(gunshots_percap), 0, gunshots_percap),
         is_gunshots = ifelse(gunshots > 0, 1, 0),
         B19013_001 = ifelse(B19013_001 == 0, 0.00001, B19013_001), # income levels
         B28012_002 = ifelse(B28012_002 == 0, 0.00001, B28012_002), # population
         is_black = ifelse(GEOID %in% black_community$GEOID, 1, 0),
         is_hispanic = ifelse(GEOID %in% hispanic_community$GEOID, 1, 0),
         total_parks_recreation = ifelse(is.na(total_parks_recreation), 0, total_parks_recreation),
         parks = ifelse(is.na(parks), 0, parks),
         small_parks = ifelse(is.na(small_parks), 0, small_parks),
         large_parks = ifelse(is.na(large_parks), 0, large_parks)) 

# green space and binary gunshot outcomes
m1 <- glm(is_gunshots ~ GreenSpace_Percent * is_black + log(B19013_001) + log(B28012_002), 
          data = data, family = "binomial")
summary(m1)

m2 <- glm(is_gunshots ~ tes * is_black, data = data,
          family = "binomial")
summary(m2)

# build logit model 
library(simcf)
model <- is_gunshots ~ GreenSpace_Percent * is_black + log(B19013_001) + log(B28012_002)
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
GreenSpace.hyp <- seq(min(mdata$GreenSpace_Percent), max(mdata$GreenSpace_Percent))
nscen <- length(GreenSpace.hyp)
blackScen <- nonblackScen <- cfMake(model, mdata, nscen)

# research question 1: expected probabilities for voting by age and marital status
for (i in 1:nscen) {
  # Married (loop over each age)
  blackScen <- cfChange(blackScen, "GreenSpace_Percent", x = GreenSpace.hyp[i], scen = i)
  blackScen <- cfChange(blackScen, "is_black", x = 1, scen = i)
  
  # Not Married (loop over each age)
  nonblackScen <- cfChange(nonblackScen, "GreenSpace_Percent", x = GreenSpace.hyp[i], scen = i)
  nonblackScen <- cfChange(nonblackScen, "is_black", x = 0, scen = i)
}

# Simulate expected probabilities for all scenarios
blackSims <- logitsimev(blackScen, sim.betas, ci=0.95)
nonblackSims <- logitsimev(nonblackScen, sim.betas, ci=0.95)

# make tibble for visualization
blackSims.tbl <- 
  blackSims %>%
  bind_rows() %>%
  mutate(GreenSpace_Percent = GreenSpace.hyp,
         is_black = "1")

nonblackSims.tbl <- 
  nonblackSims %>%
  bind_rows() %>%
  mutate(GreenSpace_Percent = GreenSpace.hyp,
         is_black = "0")

allSims.tbl <- bind_rows(blackSims.tbl, nonblackSims.tbl)

# visualize expected difference
ggplot(allSims.tbl, aes(x = GreenSpace_Percent, y = pe, ymin = lower, ymax = upper, color = is_black, fill = is_black)) +
  geom_line(linetype = "dashed") +
  geom_ribbon(alpha = 0.5) +
  scale_color_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") + 
  scale_fill_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") +
  labs(x = "Percentage of Green Space", y = "Probability of Exposure to Gun Violence",
       title = "The Relationship between Green Space and Gun Violence Exposure",
       subtitle = "Expected probabilities of exposure to gun violence in relation to the percentage of green space at the block level") +
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


ggsave("logistic_regression_greenspace_percentage.png", width = 14, height = 14/1.618, units = "in")


#### Discard this graph as the confidence intervals are too wide

model <- is_gunshots ~ GreenSpace_PerCapita * is_black + log(B19013_001) + log(B28012_002)
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
GreenSpace.hyp <- seq(min(mdata$GreenSpace_PerCapita), max(mdata$GreenSpace_PerCapita))
nscen <- length(GreenSpace.hyp)
blackScen <- nonblackScen <- cfMake(model, mdata, nscen)


for (i in 1:nscen) {
  # Married (loop over each age)
  blackScen <- cfChange(blackScen, "GreenSpace_PerCapita", x = GreenSpace.hyp[i], scen = i)
  blackScen <- cfChange(blackScen, "is_black", x = 1, scen = i)
  
  # Not Married (loop over each age)
  nonblackScen <- cfChange(nonblackScen, "GreenSpace_PerCapita", x = GreenSpace.hyp[i], scen = i)
  nonblackScen <- cfChange(nonblackScen, "is_black", x = 0, scen = i)
}

# Simulate expected probabilities for all scenarios
blackSims <- logitsimev(blackScen, sim.betas, ci=0.95)
nonblackSims <- logitsimev(nonblackScen, sim.betas, ci=0.95)

# make tibble for visualization
blackSims.tbl <- 
  blackSims %>%
  bind_rows() %>%
  mutate(GreenSpace_PerCapita = GreenSpace.hyp,
         is_black = "1")

nonblackSims.tbl <- 
  nonblackSims %>%
  bind_rows() %>%
  mutate(GreenSpace_PerCapita = GreenSpace.hyp,
         is_black = "0")

allSims.tbl <- bind_rows(blackSims.tbl, nonblackSims.tbl)

# visualize expected difference
ggplot(allSims.tbl, aes(x = GreenSpace_PerCapita, y = pe, ymin = lower, ymax = upper, color = is_black, fill = is_black)) +
  geom_line(linetype = "dashed") +
  geom_ribbon(alpha = 0.5) +
  scale_color_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") + 
  scale_fill_manual(values = c("#d39a2d","#591c19"), labels = c("Non-Black Community", "Black Community"), name = "") +
  labs(x = "Green Space Per Capita", y = "Probability of Exposure to Gun Violence",
       title = "The Relationship between Green Space and Gun Violence Exposure",
       subtitle = "Expected probabilities of exposure to gun violence in relation to green space per capita at the block level") +
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

ggsave("logistic_regression_green_percapita.png", width = 14, height = 14/1.618, units = "in")
