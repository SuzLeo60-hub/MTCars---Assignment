# Load tidyverse (includes dplyr, ggplot2, etc.)
library(tidyverse)

# Load the built-in mtcars dataset
data(mtcars)

# Preview the first 6 rows
head(mtcars)

# Select only mpg, cyl, disp, hp, drat
selected_cars <- mtcars %>%
  select(mpg, cyl, disp, hp, drat)

# Show result
head(selected_cars)

high_hp_cars <- mtcars %>%
  filter(hp > 150)

head(high_hp_cars)

# Arrange high-hp cars in descending order of mpg
sorted_cars <- high_hp_cars %>%
  arrange(desc(mpg))

# Show result
sorted_cars

# Do filtering, arranging, and selecting in one chain
sorted_cars <- mtcars %>%
  filter(hp > 150) %>%
  arrange(desc(mpg)) %>%
  select(mpg, cyl, disp, hp, drat)

# Show result
sorted_cars

# Add a mileage_category column
mtcars <- mtcars %>%
  mutate(mileage_category = ifelse(mpg >= 20, "HighMileage", "LowMileage"))

# Show mpg with mileage_category
head(mtcars[, c("mpg", "mileage_category")])

# Average horsepower by cylinder count
avg_hp_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(avg_hp = mean(hp, na.rm = TRUE))

avg_hp_by_cyl

# Average mpg by cylinder count (extra trend)
avg_mpg_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(avg_mpg = mean(mpg, na.rm = TRUE))

avg_mpg_by_cyl

# -------------------------------------------------------

# Findings:
# 1. Cars with more horsepower (>150) usually have lower mpg,
#    but some still perform relatively well.
# 2. Higher cylinder counts (8) correspond to high hp but low mpg.
# 3. Mileage categories clearly separate efficient vs inefficient cars.
# -------------------------------------------------------

