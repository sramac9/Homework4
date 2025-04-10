#load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#read in final data
final.data <- read_rds("data/output/final_ma_data.rds")

final.data.clean <- final.data %>%
  filter(!is.na(avg_enrollment) & (year %in% 2010:2015) & !is.na(partc_score))
colnames(final.data.clean)


# 1. Remove all SNPs, 800-series plans, and prescription drug only plans. Provide a box and whisker plot showing the distribution of plan counts by county over time. 
final.data.clean <- final.data.clean %>%
  filter(
    snp == "No",
    !(planid >= 800 & planid < 900),
    !is.na(partc_score),
    year %in% 2010:2015,
    !is.na(avg_enrollment)
  )

## count plans by county and year 
plan.counts <- final.data.clean %>%
  group_by(fips, year) %>%
  summarise(plan_count = n(), .groups = "drop")


## boxplot of plan counts over time 
plan.counts.plot <- ggplot(plan.counts, aes(x = as.factor(year), y = plan_count)) +
  geom_boxplot() +
  labs(x = "Year",
       y = "Number of Plans per County") +
  theme_minimal()
print(plan.counts.plot)




# 2. Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
## filter data for selected years and count plans by star rating
star.dist <- final.data.clean %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  filter(!is.na(Star_Rating))

star.dist <- star.dist %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n(), .groups = "drop")

## create combined bar plot
star.dist.plot <- ggplot(star.dist, aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ year) +  
  labs(x = "Star Rating",
       y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

print(star.dist.plot)

## create seperate bar plots 
star.dist.10 <- ggplot(subset(star.dist, year == 2010), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.10)

star.dist.12 <- ggplot(subset(star.dist, year == 2012), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.12)

star.dist.15 <- ggplot(subset(star.dist, year == 2015), aes(x = as.factor(Star_Rating), y = count, fill = as.factor(Star_Rating))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Star Rating", y = "Count of Plans") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
print(star.dist.15)




# 3. Plot the average benchmark payment over time from 2010 through 2015.
## filter data 
avg.benchmark <- final.data.clean %>%
  group_by(year) %>%
  summarise(avg_benchmark = mean(ma_rate, na.rm = TRUE))

## plot of average benchmark payments over time
bench.plt <- ggplot(avg.benchmark, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Year",
       y = "Average Benchmark Payment ($)") +
  theme_minimal()

print(bench.plt)

save.image("submission1/hwk4_workspace.RData")

