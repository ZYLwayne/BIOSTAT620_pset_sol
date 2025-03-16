# 1
library(HistData)
library(dplyr)


names(GaltonFamilies)


set.seed(2007)


heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%                    
  sample_n(1) %>%                          
  ungroup() %>%
  select(father, daughter = childHeight)   


head(heights)

# 2
mu_x <- mean(heights$father)
mu_y <- mean(heights$daughter)
sigma_x <- sd(heights$father)
sigma_y <- sd(heights$daughter)

# Calculate the correlation coefficient
rho <- cor(heights$father, heights$daughter)

# Calculate regression coefficients
beta_1 <- rho * (sigma_y / sigma_x)
beta_0 <- mu_y - beta_1 * mu_x

# Parameters of the output regression equation
cat("Intercept (β0):", beta_0, "\n")
cat("Slope (β1):", beta_1, "\n")

# 3
library(ggplot2)

ggplot(heights, aes(x = father, y = daughter)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(title = "Father vs Daughter Height Regression",
       x = "Father's Height",
       y = "Daughter's Height") +
  theme_minimal()

# 4
# Calculate the regression model using lm()
model <- lm(daughter ~ father, data = heights)


# Output the regression coefficients
summary(model)

beta_0_lm <- coef(model)[1]  
beta_1_lm <- coef(model)[2]  

cat("Intercept (β0) from lm():", beta_0_lm, "\n")
cat("Slope (β1) from lm():", beta_1_lm, "\n")


# 5
heights <- heights %>%
  mutate(father_centered = father - mean(father))

model_centered <- lm(daughter ~ father_centered, data = heights)

summary(model_centered)

beta_0_centered <- coef(model_centered)[1]  
beta_1_centered <- coef(model_centered)[2]  

cat("Intercept (β0) with centered father heights:", beta_0_centered, "\n")
cat("Slope (β1) with centered father heights:", beta_1_centered, "\n")

# 6
mu_y <- mean(heights$daughter)

cat("Mean daughter height:", mu_y, "\n")
cat("Intercept from centered regression:", beta_0_centered, "\n")


all.equal(beta_0_centered, mu_y)


# 7
library(excessmort)
library(dplyr)
library(lubridate)


head(puerto_rico_counts)


puerto_rico_counts <- puerto_rico_counts %>%
  mutate(year = year(date))  

unique(puerto_rico_counts$agegroup)


filtered_counts <- puerto_rico_counts %>%
  filter(year >= 2002 & year <= 2017, 
         agegroup %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85-Inf"))

head(filtered_counts)


# 8
landfall_date <- as.Date("2017-09-20")

day_of_week <- weekdays(landfall_date)
cat("Hurricane Maria made landfall in Puerto Rico on a", day_of_week, "\n")


# 10
library(lubridate)
library(dplyr)


weekly_counts <- puerto_rico_counts %>%
  mutate(week_start = date - (wday(date) - 4) %% 7)  


head(weekly_counts)

# 11

library(MMWRweek)

colnames(weekly_counts)
weekly_counts <- weekly_counts %>%
  group_by(week_start, sex, agegroup) %>%
  summarise(weekly_outcome = sum(outcome),  
            days_counted = n(),  
            .groups = "drop") %>%
  filter(days_counted == 7)  


weekly_counts <- weekly_counts %>%
  mutate(MMWR_year = year(week_start),
         MMWR_week = MMWRweek(week_start)$MMWRweek)

head(weekly_counts)

# 12
library(ggplot2)
library(dplyr)

puerto_rico_counts %>%
  ggplot(aes(x = agegroup, y = population, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +  
  labs(title = "Population Size Distribution by Age Group and Gender",
       x = "Age Group",
       y = "Population (log scale)") +
  theme_minimal()

# 13  
weekly_counts <- puerto_rico_counts %>%
  mutate(week_start = date - (wday(date) - 4) %% 7) %>%
  group_by(week_start, sex, agegroup) %>%
  summarise(
    weekly_outcome = sum(outcome, na.rm = TRUE), 
    population = mean(population, na.rm = TRUE),  
    days_counted = n(), 
    .groups = "drop"
  ) %>%
  filter(days_counted == 7) 

weekly_counts <- weekly_counts %>%
  mutate(MMWR_year = MMWRweek(week_start)$MMWRyear,
         MMWR_week = MMWRweek(week_start)$MMWRweek)


colnames(weekly_counts)
head(weekly_counts)

weekly_counts <- weekly_counts %>%
  mutate(mortality_rate = weekly_outcome / population)  

weekly_counts_pre2017 <- weekly_counts %>%
  filter(MMWR_year >= 2002 & MMWR_year <= 2016)

weekly_counts_2017 <- weekly_counts %>%
  filter(MMWR_year == 2017)

ggplot(weekly_counts_pre2017, aes(x = as.factor(MMWR_week), y = mortality_rate)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_jitter(data = weekly_counts_2017, aes(x = as.factor(MMWR_week), y = mortality_rate),
              color = "red", size = 1.5, alpha = 0.7) +  
  labs(title = "Weekly Mortality Rate Distribution (2002-2016) with 2017 Data",
       x = "MMWR Week",
       y = "Mortality Rate") +
  theme_minimal()


# 14
library(ggplot2)
library(dplyr)

yearly_counts <- weekly_counts %>%
  filter(MMWR_year >= 2002 & MMWR_year <= 2016) %>%
  group_by(MMWR_year) %>%
  summarise(
    total_deaths = sum(weekly_outcome, na.rm = TRUE),  
    total_population = sum(population, na.rm = TRUE),  
    mortality_rate = (total_deaths / total_population) * 1000  
  )

ggplot(yearly_counts, aes(x = MMWR_year, y = mortality_rate)) +
  geom_line(color = "blue", linewidth = 1) +  
  geom_point(size = 3, color = "red") +  
  labs(title = "Yearly Mortality Rate (per 1,000) for 2002-2016",
       x = "Year",
       y = "Mortality Rate (per 1,000)") +
  theme_minimal()


# 15
library(dplyr)
weekly_counts_65plus <- weekly_counts %>%
  filter(MMWR_year >= 2002 & MMWR_year <= 2016, agegroup %in% c("65-69", "70-74", "75-79", "80-84", "85-Inf")) %>%
  mutate(
    mortality_rate = weekly_outcome / population, 
    MMWR_week = as.factor(MMWR_week),  
    sex = as.factor(sex),  
    agegroup = as.factor(agegroup)
  )


model <- lm(mortality_rate ~ MMWR_year + MMWR_week + agegroup + sex, data = weekly_counts_65plus)

summary(model)





# 16

library(dplyr)
library(ggplot2)

weekly_counts_2017 <- weekly_counts %>%
  filter(MMWR_year == 2017) %>%  
  filter(agegroup %in% c("65-69", "70-74", "75-79", "80-84", "85-Inf")) %>%  
  mutate(
    agegroup = factor(agegroup, levels = levels(weekly_counts$agegroup)), 
    MMWR_week = factor(MMWR_week) 
  )


predictions_2017 <- predict(model, newdata = weekly_counts_2017, interval = "confidence", level = 0.95)


weekly_counts_2017 <- weekly_counts_2017 %>%
  mutate(
    predicted_rate = predictions_2017[, "fit"], 
    lower_CI = predictions_2017[, "lwr"], 
    upper_CI = predictions_2017[, "upr"], 
    excess_mortality = mortality_rate - predicted_rate  
  )

weekly_counts_2017 <- weekly_counts_2017 %>%
  filter(!is.na(excess_mortality))

ggplot(weekly_counts_2017, aes(x = week_start, y = excess_mortality)) +
  geom_line(color = "red", size = 1) +  
  geom_ribbon(aes(ymin = lower_CI - predicted_rate, ymax = upper_CI - predicted_rate), 
              fill = "gray80", alpha = 0.5) + 
  labs(title = "Weekly Excess Mortality in 2017",
       x = "Week",
       y = "Excess Mortality Rate") +
  theme_minimal()



# 17
library(dplyr)
library(ggplot2)

weekly_counts_pre2017 <- weekly_counts %>%
  filter(MMWR_year >= 2002 & MMWR_year <= 2016) %>%
  filter(agegroup %in% c("65-69", "70-74", "75-79", "80-84", "85-Inf")) %>%  
  mutate(
    agegroup = factor(agegroup, levels = levels(weekly_counts$agegroup)),
    MMWR_week = factor(MMWR_week) 
  )


predictions_pre2017 <- predict(model, newdata = weekly_counts_pre2017, interval = "confidence", level = 0.95)


weekly_counts_pre2017 <- weekly_counts_pre2017 %>%
  mutate(predicted_rate = predictions_pre2017[, "fit"])


all_data <- bind_rows(weekly_counts_pre2017, weekly_counts_2017)

all_data <- all_data %>%
  filter(!is.na(predicted_rate) & !is.na(mortality_rate))

ggplot(all_data, aes(x = predicted_rate, y = mortality_rate, color = agegroup)) +
  geom_point(alpha = 0.6) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Observed vs Predicted Mortality Rate (2002-2017)",
       x = "Predicted Mortality Rate",
       y = "Observed Mortality Rate") +
  theme_minimal()

ggplot(all_data, aes(x = predicted_rate, y = mortality_rate, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Observed vs Predicted Mortality Rate by Sex (2002-2017)",
       x = "Predicted Mortality Rate",
       y = "Observed Mortality Rate") +
  theme_minimal()





