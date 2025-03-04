# 1 Load necessary libraries
library(tidyverse)
library(rvest)

# Read data from provided URL
url <- "https://raw.githubusercontent.com/dmcable/BIOSTAT620/refs/heads/main/data/president_polls.csv"
raw_dat <- read_csv(url)

# Check first few rows
glimpse(raw_dat)

raw_dat %>%
  count(candidate_name, sort = TRUE)

raw_dat %>%
  count(population, sort = TRUE)

# 2 Clean and transform data
dat <- raw_dat %>%
  
  filter(population != "v") %>%
  # Remove hypothetical polls
  filter(!hypothetical) %>%
  # Convert date columns and set population as an ordered factor
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%y"),
    end_date = as.Date(end_date, format = "%m/%d/%y"),
    population = factor(population, levels = c("lv", "rv", "a"), ordered = TRUE)
  )


glimpse(dat)

# 3
library(dplyr)

dat <- dat %>%
  group_by(poll_id, question_id) %>%
  mutate(n = n()) %>%
  ungroup()

glimpse(dat)

# 4
library(tidyverse)
dat <- dat %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) %>%
  # Select relevant columns
  select(
    poll_id, question_id, state, pollster, start_date, end_date, 
    numeric_grade, sample_size, population, candidate_name, pct, n
  ) %>%
  # Convert candidate names into separate columns
  pivot_wider(names_from = candidate_name, values_from = pct) %>%
  # Compute the spread (percentage point difference between Harris and Trump)
  mutate(spread = (`Kamala Harris` - `Donald Trump`) / 100)


glimpse(dat)


# 5
library(dplyr)


if (!"population" %in% colnames(dat)) {
  dat <- dat |> 
    left_join(select(population_dat, poll_id, question_id, population), 
              by = c("poll_id", "question_id"))
}

dat <- dat |> 
  arrange(poll_id, question_id) |>  
  group_by(poll_id) |> 
  mutate(priority = case_when(
    population == "lv" ~ 1,  
    population == "rv" ~ 2,  
    population == "a"  ~ 3,  
    TRUE ~ 4
  )) |> 
  filter(priority == min(priority, na.rm = TRUE)) |>  
  filter(question_id == min(question_id, na.rm = TRUE)) |>  
  ungroup()


if ("priority" %in% colnames(dat)) {
  dat <- dat |> select(-priority)
}

glimpse(dat)

# 6
library(dplyr)


popular_vote <- dat |> 
  filter(is.na(state))  

polls <- dat |> 
  filter(!is.na(state))  

# 检查数据
glimpse(popular_vote)
glimpse(polls)


# 7
library(ggplot2)
library(lubridate)


popular_vote_filtered <- popular_vote |> 
  filter(start_date >= make_date(2024, 7, 21) & population != "a") |> 
  group_by(pollster) |> 
  mutate(pollster = ifelse(n() < 5, "Other", pollster)) |>  
  ungroup()


popular_vote_filtered <- popular_vote_filtered |> 
  mutate(start_date = as.Date(start_date)) |> 
  filter(!is.na(start_date))


popular_vote_filtered <- popular_vote_filtered |> 
  filter(!is.na(spread) & is.finite(spread)) |>
  filter(spread > -30 & spread < 30)


ggplot(popular_vote_filtered |> filter(population == "lv"), 
       aes(x = start_date, y = spread, color = pollster)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  scale_x_date(date_labels = "%b %Y") +  
  labs(title = "Popular Vote Trend for Likely Voters",
       x = "Start Date", y = "Spread (Harris - Trump)",
       color = "Pollster") +
  theme_minimal()


ggplot(popular_vote_filtered |> filter(population == "rv"), 
       aes(x = start_date, y = spread, color = pollster)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_date(date_labels = "%b %Y") +  
  labs(title = "Popular Vote Trend for Registered Voters",
       x = "Start Date", y = "Spread (Harris - Trump)",
       color = "Pollster") +
  theme_minimal()


# 8
library(ggplot2)
library(lubridate)

# Filter for likely voter polls after July 21, 2024
popular_vote_filtered <- popular_vote %>%
  filter(start_date > make_date(2024, 7, 21) & population == "lv") %>%
  group_by(pollster) %>%
  mutate(n_pollster = n()) %>%
  ungroup() %>%
  mutate(pollster = if_else(n_pollster < 5, "Other", pollster))

# Create a boxplot for spread across different pollsters
ggplot(popular_vote_filtered, aes(x = reorder(pollster, spread, median), y = spread * 100)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visibility
  labs(title = "Pollster Effect on Spread (Likely Voters)", 
       x = "Pollster", 
       y = "Spread (%)") +
  theme_minimal() +
  coord_flip()  # Flip axes for better readability

# 9
library(tidyverse)

lv_polls <- popular_vote %>%
  filter(start_date > make_date(2024, 7, 21), population == "lv") %>%
  pull(spread)  

t_test_result <- t.test(lv_polls)
prediction_popular <- t_test_result$estimate * 100 
ci_popular <- t_test_result$conf.int * 100  


print(prediction_popular)  
print(ci_popular)          

# 10
library(rvest)
url_ev <- "https://state.1keydata.com/state-electoral-votes.php"
h <- read_html(url_ev) %>% html_table()
ev <- h[[4]] %>% 
  rename(state = X2, electoral_votes = X3) %>%  
  select(state, electoral_votes) %>%
  mutate(electoral_votes = as.numeric(electoral_votes)) %>%
  add_row(state = "Maine CD-1", electoral_votes = 1) %>%
  add_row(state = "Maine CD-2", electoral_votes = 1) %>%
  add_row(state = "Nebraska CD-2", electoral_votes = 1) %>%
  add_row(state = "District of Columbia", electoral_votes = 3)


# 11
library(gsheet)
sheet_url <- "https://docs.google.com/spreadsheets/d/1D-edaVHTnZNhVU840EPUhz3Cgd7m39Urx7HM8Pq6Pus/edit?gid=29622862"
raw_res_2020 <- gsheet2tbl(sheet_url)

library(dplyr)
library(janitor)
raw_res_2020 <- raw_res_2020 %>% row_to_names(row_number = 1)
raw_res_2020 <- raw_res_2020 %>% clean_names()

res_2020 <- raw_res_2020 %>%
  select(state, biden_joe_democratic, trump_donald_republican) %>%
  mutate(
    biden = as.numeric(biden_joe_democratic),
    trump = as.numeric(trump_donald_republican),
    party = if_else(biden > trump, "D", "R")
  ) %>%
  select(state, party)

res_2020 <- res_2020 %>%
  add_row(state = "Maine CD-1", party = "D") %>%
  add_row(state = "Maine CD-2", party = "R") %>%
  add_row(state = "Nebraska CD-2", party = "D") %>%
  add_row(state = "District of Columbia", party = "D")

print(res_2020)

# 12
results <- polls %>%
  filter(start_date > make_date(2024, 7, 21)) %>%
  group_by(state) %>%
  summarise(avg = mean(spread),
            sd = if_else(n() > 1, sd(spread), NA_real_),
            n = n()) %>%
  left_join(ev, by = "state")

# 13

harris_start <- res_2020 %>%
  filter(party == "D") %>%
  left_join(ev, by = "state") %>% 
  summarise(total = sum(electoral_votes, na.rm = TRUE)) %>% 
  pull(total)

print(harris_start)


# 14
library(ggplot2)


prior_mean <- median(results$avg, na.rm = TRUE)
prior_n <- 5  

results <- results %>%
  mutate(posterior_mean = (n * avg + prior_n * prior_mean) / (n + prior_n))  


ggplot(results, aes(x = avg * 100, y = posterior_mean * 100, size = n)) +
  geom_point(alpha = 0.6) +  # 添加散点图
  labs(title = "Posterior Mean vs Observed Average (State-Level)",
       x = "Observed Average Spread (%)",
       y = "Posterior Mean Spread (%)",
       size = "Number of Polls") +
  theme_minimal()


# 15
results <- results %>%
  mutate(predicted_win = if_else(posterior_mean > 0, electoral_votes, 0))

# Sum the electoral votes from predicted wins and add guaranteed wins from states with no polls.
predicted_ev <- sum(results$predicted_win, na.rm = TRUE)

# For uncertainty, assume the variance in the poll estimate in each state is sd^2/n
results <- results %>%
  mutate(var = if_else(!is.na(sd) & n > 1, (sd^2) / n, 0))
total_variance <- sum(results$var, na.rm = TRUE)
se_total <- sqrt(total_variance)

# 95% Confidence interval (using normal approximation)
ci_ev <- c(predicted_ev - 1.96 * se_total, predicted_ev + 1.96 * se_total)

# 输出结果
cat("Predicted Electoral Votes for Harris:", round(predicted_ev), "\n")
ci_ev
