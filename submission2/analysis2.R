# Meta --------------------------------------------------------------------
# Author:        Sammy Ramacher
# Date Created:  4/9/2025
# Date Edited:   4/9/2025
# Homework 4-2

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, broom, kableExtra, modelsummary)


#read in final data
final.data <- read_rds("submission1/data/output/final_ma_data.rds")

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
  coord_cartesian(ylim = c(500, 900)) +
  labs(x = "Year",
       y = "Average Benchmark Payment ($)") +
  theme_minimal() 

print(bench.plt)

# 4. Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
avg_share <- final.data.clean %>%
  filter(avg_eligibles > 0, avg_enrolled >= 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

## Line plot of average MA share over time
adv_share_plt <- ggplot(avg_share, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "#008cff", size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "MA Share") +
  theme_minimal(base_size = 14)
print(adv_share_plt) 

## ESTIMATE ATES
data_2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) %>%
  distinct(contractid, planid, county, .keep_all = TRUE)

# 5. Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

data_2010 <- data_2010 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, partd, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>% 
    mutate(mkt_share = avg_enrollment/avg_eligibles, 
          HMO=str_detect(plan_type, "HMO"))

# Rounding raw_rating to the nearest star rating category
data_2010 <- data_2010 %>%
  mutate(
    rounded_star = case_when(
      raw_rating >= 4.75 ~ 5,
      raw_rating >= 4.25 & raw_rating < 4.75 ~ 4.5, 
      raw_rating >= 3.75 & raw_rating < 4.25 ~ 4,   
      raw_rating >= 3.25 & raw_rating < 3.75 ~ 3.5,
      raw_rating >= 2.75 & raw_rating < 3.25 ~ 3,  
      TRUE ~ NA_real_))

# Count the number of plans rounded up to each star rating
## Calculate the rounded star ratings 
rounded_counts <- data_2010 %>%
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0), 
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0), 
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5.0,1,0)) %>%
  group_by(Star_Rating) %>% 
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>% 
  summarize(count_30=sum(rounded_30), 
            count_35=sum(rounded_35), 
            count_40=sum(rounded_40), 
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))%>% 
  mutate(rounded_up=count_30 + count_35 + count_40 + count_45 + count_50) %>% 
  select(Star_Rating, rounded_up)

kable(rounded_counts, format = "markdown", col.names = c("Star Rating", "Count"), 
    caption = "Rounded Star Ratings in 2010")

# Question 6 -------------------------------------------------------------
# Set bandwidth
b <- 0.125

# Estimate the effect of receiving a 3-star rating vs 2.5-star rating
star30 <- lm(mkt_share ~ treat + score, 
               data = data_2010 %>%
                 filter(raw_rating >= (2.75 - b),
                        raw_rating <= (2.75 + b),
                        Star_Rating %in% c(2.5, 3.0)) %>%
                 mutate(treat = (Star_Rating == 3), 
                        score = raw_rating - 2.75))     

# Estimate the effect of receiving a 3.5-star rating vs 3-star rating
star35 <- lm(mkt_share ~ treat + score, 
                data = data_2010 %>%
                  filter(raw_rating >= (3.25 - b),
                         raw_rating <= (3.25 + b),
                         Star_Rating %in% c(3.0, 3.5)) %>%
                  mutate(treat = (Star_Rating == 3.5),  
                         score = raw_rating - 3.25))    

# Estimate the effect of receiving a 4-star rating vs 3.5-star rating
star40 <- lm(mkt_share ~ treat + score, 
                data = data_2010 %>%
                  filter(raw_rating >= (3.75 - b),
                         raw_rating <= (3.75 + b),
                         Star_Rating %in% c(3.5, 4.0)) %>%
                  mutate(treat = (Star_Rating == 4.0),  
                         score = raw_rating - 3.75)) 

# Create a summary table for all comparisons
models <- list(star30, star35, star40)
names(models) <- c("2.5–3", "3–3.5", "3.5–4")

q6 <- modelsummary(
  models,
  keep = c("treatTRUE", "score"),
  coef_map = c("treatTRUE" = "Rounded",
               "score" = "Running Score"),
  gof_map = c("nobs", "r.squared"), 
  title = "Effect of Star Rating on Enrollment Near Thresholds")

# Question 7 -------------------------------------------------------------
# Define bandwidths
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Initialize an empty list to store results
results <- list()

# Loop over each bandwidth and estimate the effects for star30 and star35
for (b in bandwidths) {
  star30 <- lm(mkt_share ~ treat + score, 
               data = data_2010 %>%
                 filter(raw_rating >= (2.75 - b),
                        raw_rating <= (2.75 + b),
                        Star_Rating %in% c(2.5, 3.0)) %>%
                 mutate(treat = (Star_Rating == 3), 
                        score = raw_rating - 2.75))  
  star35 <- lm(mkt_share ~ treat + score, 
               data = data_2010 %>%
                 filter(raw_rating >= (3.25 - b),
                        raw_rating <= (3.25 + b),
                        Star_Rating %in% c(3.0, 3.5)) %>%
                 mutate(treat = (Star_Rating == 3.5),  
                        score = raw_rating - 3.25))  
  
  # Extract the coefficients (estimates) and standard errors for 'treat' from each model
  star30_estimate <- tidy(star30)$estimate[2]
  star35_estimate <- tidy(star35)$estimate[2]
  star30_se <- tidy(star30)$std.error[2]
  star35_se <- tidy(star35)$std.error[2]
  
  # Store the results for both star30 and star35 with bandwidth
  results[[as.character(b)]] <- tibble(
    Bandwidth = b,
    Comparison = c("2.5 to 3 Star", "3 to 3.5 Star"),
    Estimate = c(star30_estimate, star35_estimate),
    StdError = c(star30_se, star35_se)
  )}

# Combine all the results into a single dataframe
results_df <- bind_rows(results)

# Plot the results 
q7 <- ggplot(results_df, aes(x = Bandwidth, y = Estimate, shape = Comparison)) +
  geom_point(size = 4, color = "black", fill = "black", stroke = 1.5) + 
  geom_errorbar(aes(ymin = Estimate - StdError, ymax = Estimate + StdError), 
                width = 0.001, color = "black", size = 1) + 
  labs(title = "Effect of Star Rating Comparisons across Bandwidths",
       x = "Bandwidth",
       y = "Estimated Effect on Market Share",
       shape = "Comparison Type") +
  scale_shape_manual(values = c(16, 17), labels = c("2.5 to 3 Star", "3 to 3.5 Star")) +  
  theme_minimal(base_family = "Times") +  
  theme(
    legend.position = "top", 
    legend.title = element_text(face = "plain", size = 12),  
    legend.text = element_text(face = "plain", size = 10), 
    axis.title = element_text(face = "plain", size = 12),  
    axis.text = element_text(face = "plain", size = 10),  
    plot.title = element_text(face = "plain", size = 14, hjust = 0.5))
    
# Question 8 -------------------------------------------------------------
# Create a density plot for 'raw_rating' with vertical lines for each rounding threshold
q8 <- ggplot(data_2010, aes(x = raw_rating)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = 2.75, color = "black", linetype = "dashed", size = 0.5) +  
  geom_vline(xintercept = 3.25, color = "black", linetype = "dashed", size = 0.5) +  
  geom_vline(xintercept = 3.75, color = "black", linetype = "dashed", size = 0.5) +  
  geom_vline(xintercept = 4.25, color = "black", linetype = "dashed", size = 0.5) + 
  geom_vline(xintercept = 4.75, color = "black", linetype = "dashed", size = 0.5) + 
  labs(title = "Distribution of Raw Ratings with Rounding Thresholds",
       x = "Raw Rating", y = "Density") +
  theme_minimal(base_family = "Times")

# Question 9  -------------------------------------------------------------
# Subset the data for plans just above and below the threshold values
plans_above_below_threshold <- data_2010 %>%
  filter((Star_Rating >= 3.4 & Star_Rating < 3.6) |
    (Star_Rating >= 4.4 & Star_Rating < 4.6))

# Summarize plan characteristics for HMO and Part D status
plan_characteristics <- plans_above_below_threshold %>%
  group_by(Star_Rating) %>%
  summarize(avg_HMO = mean(grepl("HMO", plan_type), na.rm = TRUE),
    avg_part_d = mean(partd == "Yes", na.rm = TRUE),
    count = n())

plan_characteristics_clean <- plan_characteristics %>%
  rename(
    `Star Rating` = Star_Rating,
    `Percent HMO Plans` = avg_HMO,
    `Percent with Part D` = avg_part_d,
    `Number of Plans` = count) %>%
  mutate(
    `Percent HMO Plans` = percent(`Percent HMO Plans`, accuracy = 1),
    `Percent with Part D` = percent(`Percent with Part D`, accuracy = 1))

kable(plan_characteristics_clean, caption = "Plan Characteristics Around Star Rating Thresholds")

## CREATE WORKSPACE
rm(list=c("final.data"))


save.image("submission2/hwk4_workspace.RData")
