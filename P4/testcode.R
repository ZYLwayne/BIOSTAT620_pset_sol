
# 5Load required package
library(dplyr)
library(httr2)
library(jsonlite)

# Define function to fetch data from CDC API
get_cdc_data <- function(endpoint) {
  api_url <- paste0("https://data.cdc.gov/resource/", endpoint, ".json?$limit=100000")
  response <- request(api_url) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as_tibble()
  return(response)
}

# Fetch cases data
cases_raw <- get_cdc_data("pwn4-m3yp")

library(lubridate)
library(stringr)  


cases_raw <- cases_raw %>%
  mutate(
    
    date_updated = as_date(str_sub(date_updated, 1, 10)), 
    new_cases = as.numeric(new_cases)
  )
summary(cases_raw$new_cases)
summary(cases_raw$date_updated)
cases_raw %>%
  filter(state == "AK") %>%
  select(state, date_updated, new_cases) %>%
  arrange(date_updated) %>%
  head(20)
cases_raw <- cases_raw %>%
  mutate(new_cases = as.numeric(new_cases))

cases_weekly <- cases_raw %>%
  mutate(
    date_updated = as_date(date_updated), 
    mmwr_year = epiyear(date_updated),
    mmwr_week = epiweek(date_updated),
    new_cases = as.numeric(new_cases)  
  ) %>%
  group_by(state, mmwr_year, mmwr_week) %>%
  summarise(total_cases = sum(new_cases, na.rm = TRUE), .groups = "drop")

head(cases_weekly)


# 6Load required libraries
library(dplyr)
library(lubridate)
hosp_raw <- get_cdc_data("39z2-9zu6")


# Process hospitalization data similar to cases data
hosp_weekly <- hosp_raw %>%
  mutate(
    # Convert collection_date to Date format
    date = as_date(collection_date),
    
    # Extract MMWR year and MMWR week
    mmwr_year = epiyear(date),
    mmwr_week = epiweek(date)
  ) %>%
  
  # Group by state, MMWR year, and MMWR week
  group_by(jurisdiction, mmwr_year, mmwr_week) %>%
  
  # Summarize total hospitalizations per week per state
  summarise(
    total_hospitalizations = sum(as.numeric(new_covid_19_hospital), na.rm = TRUE),
    num_days = n(),  # Count number of records in the week
    .groups = "drop"  # Ensure grouping is removed for further processing
  ) %>%
  
  # Keep only weeks with full 7-day reporting
  filter(num_days == 7) %>%
  
  # Remove the auxiliary column
  select(-num_days) 

# Check again
summary(hosp_weekly$total_hospitalizations)
head(hosp_weekly)


# 7Load required libraries
deaths_raw <- get_cdc_data("r8kw-7aab")
deaths <- deaths_raw |> 
  # Select required columns
  select(
    state,                         
    deaths = covid_19_deaths,      
    date = week_ending_date        
  ) |>
  
  mutate(
    date = as_date(ymd_hms(date)), 
    mmwr_year = epiyear(date),     
    mmwr_week = epiweek(date)      
  ) |>
  # Group by state and MMWR periods and calculate weekly totals
  group_by(state, mmwr_year, mmwr_week) |>
  summarise(
    deaths = sum(as.numeric(deaths), na.rm = TRUE),  # sum weekly deaths
    .groups = "drop"
  )

deaths <- deaths |>
  mutate(state = state.abb[match(state, state.name)]) |>
  mutate(state = case_when(
    state == "District of Columbia" ~ "DC",
    state == "Puerto Rico" ~ "PC",
    TRUE ~ state
  ))

# Display result
print(head(deaths))


# 8Load required libraries
library(dplyr)
library(lubridate)
vax_raw <- get_cdc_data("rh2h-3yt2")
# Process vaccination data
vaccines <- vax_raw |> 
  filter(date_type == "Admin") |>  # Use only administered data
  select(
    location,                  # `location` is equivalent to `state`
    date,                      # Vaccination date
    series_complete_daily,      # Daily completed vaccinations
    booster_daily               # Daily booster vaccinations
  ) |> 
  rename(state = location) |>   # Rename location to state
  mutate(
    date = as_date(date),       # Convert date to Date format
    mmwr_year = epiyear(date),  # Compute MMWR year
    mmwr_week = epiweek(date)   # Compute MMWR week
  ) |> 
  group_by(state, mmwr_year, mmwr_week) |> 
  summarise(
    total_series_complete = sum(as.numeric(series_complete_daily), na.rm = TRUE),
    total_booster = sum(as.numeric(booster_daily), na.rm = TRUE),
    .groups = "drop"
  )

# Display results
head(vaccines)
summary(vaccines)
print(head(vaccines))


# 9
library(tidyverse)
library(lubridate)
library(epitools)


state_lookup <- data.frame(
  state_full = state.name,
  state_abbr = state.abb
) %>%
  bind_rows(data.frame(state_full = "District of Columbia", state_abbr = "DC"))

convert_state <- function(state_col) {
  ifelse(state_col %in% state.abb, state_col, state_lookup$state_abbr[match(state_col, state_lookup$state_full)])
}
print(dim(population))  # 检查 population 的行列数
print(colnames(population))  # 检查 population 列名
head(population)  # 查看前几行数据

print("Before conversion:")
print(unique(population$state))  # 看看 `state` 是否本来就有 NA

population <- population %>%
  mutate(state = ifelse(is.na(state), "GA", state))


population <- population %>%
  mutate(state = convert_state(state), year = as.numeric(year)) %>%
  distinct(state, year, .keep_all = TRUE)


cases_weekly <- cases_weekly %>%
  mutate(state = convert_state(state), mmwr_year = as.numeric(mmwr_year), mmwr_week = as.numeric(mmwr_week)) %>%
  distinct(state, mmwr_year, mmwr_week, .keep_all = TRUE)

hosp_weekly <- hosp_weekly %>%
  
  mutate(state = convert_state(state), mmwr_year = as.numeric(mmwr_year), mmwr_week = as.numeric(mmwr_week)) %>%
  distinct(state, mmwr_year, mmwr_week, .keep_all = TRUE)

deaths_weekly <- deaths_weekly %>%
  mutate(state = convert_state(state), mmwr_year = as.numeric(mmwr_year), mmwr_week = as.numeric(mmwr_week)) %>%
  distinct(state, mmwr_year, mmwr_week, .keep_all = TRUE)

vaccines <- vaccines %>%
  mutate(state = convert_state(state), mmwr_year = as.numeric(mmwr_year), mmwr_week = as.numeric(mmwr_week)) %>%
  distinct(state, mmwr_year, mmwr_week, .keep_all = TRUE)


all_dates <- data.frame(date = seq(make_date(2020, 1, 25),
                                   make_date(2021, 12, 31), 
                                   by = "week")) %>%
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) %>%
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date))


unique_states <- unique(population$state)
unique_states <- unique_states[!is.na(unique_states)]  
dates_and_pop <- crossing(all_dates, data.frame(state = unique_states)) %>%
  left_join(population, by = c("state", "mmwr_year" = "year")) %>%
  distinct(state, mmwr_year, mmwr_week, .keep_all = TRUE)


dat <- dates_and_pop %>%
  left_join(cases_weekly, by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(hosp_weekly, by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(deaths_weekly, by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(vaccines, by = c("state", "mmwr_year", "mmwr_week")) %>%
  arrange(state, date)


print(head(dat))
summary(dat)
