# ------------------------------------------------------------
# Lab: Exploring the mtcars Dataset
# Purpose:
#   Practice data manipulation with dplyr and the pipe operator
#   Explore automotive trends in the mtcars dataset
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# VISUALIZATIONS (Histograms, Bar Plots, Scatterplots, & Box Plots/Boxplots)
# ------------------------------------------------------------

# (1) Histograms
# Histogram of mpg
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of MPG", x = "Miles per Gallon", y = "Count")

# Density plot of horsepower
ggplot(mtcars, aes(x = hp)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Density Plot of Horsepower", x = "Horsepower", y = "Density")

# (2) Bar Plots 
# Frequency of cylinders
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Number of Cylinders", x = "Cylinders", y = "Count")

# Frequency of gears
ggplot(mtcars, aes(x = factor(gear))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Gear Types", x = "Number of Gears", y = "Count")

# (3) Scatter Plots
# Relationship between mpg and horsepower
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "MPG vs Horsepower", x = "Horsepower", y = "Miles per Gallon")

# Relationship between displacement and mpg
ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "MPG vs Displacement", x = "Displacement", y = "Miles per Gallon")

# (4) Box Plots and Violin Plots
# Boxplot: mpg by cylinder count
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "MPG by Cylinder Count", x = "Cylinders", y = "Miles per Gallon")

# Violin plot: hp by cylinder count
ggplot(mtcars, aes(x = factor(cyl), y = hp)) +
  geom_violin(fill = "pink") +
  labs(title = "Horsepower by Cylinder Count", x = "Cylinders", y = "Horsepower")

# ------------------------------------------------------------
# Findings and Insights:
# ------------------------------------------------------------
# 1. Cars with more horsepower (>150 hp) generally have lower mpg,
#    but some still manage reasonable fuel efficiency.
#
# 2. Higher cylinder cars (8 cylinders) show the highest horsepower
#    but the lowest mpg. In contrast, 4-cylinder cars have lower hp
#    but much higher mpg.
#
# 3. When classified by mileage category:
#    - Most 4-cylinder cars fall into "HighMileage".
#    - Most 8-cylinder cars fall into "LowMileage".
#
# 4. Grouping results:
#    - Average hp rises as the number of cylinders increases.
#    - Average mpg drops as the number of cylinders increases.
#
# Overall:
#   There is a clear tradeoff between engine power (hp, cyl)
#   and fuel efficiency (mpg). Smaller engines tend to be more
#   efficient, while larger engines prioritize power.

