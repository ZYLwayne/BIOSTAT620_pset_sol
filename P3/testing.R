
# Load the file where the API Key is stored
source("census-key.R") 

# 2. Construct GET request using httr2
# Define the API URL
url <- "https://api.census.gov/data/2021/pep/population"

library(httr2)

# Make the API request
request <- request(url) %>%
  req_url_query(get = "POP_2020,POP_2021,NAME", `for` = "state:*", key = census_key)

# Perform the request and check the response
response <- request %>% req_perform()

# Check the status code
status_code <- resp_status(response)
print(paste("Status Code:", status_code))

# Print the response content
content <- response %>% resp_body_string()
print(content)


# 3.
response <- request %>% req_perform()

status_code <- resp_status(response)
print(paste("Status Code:", status_code))


# 4.
content_type <- resp_content_type(response)
print(paste("Content Type:", content_type))



# 5.

population_list <- response %>% 
  resp_body_json()

population_df <- as.data.frame(do.call(rbind, population_list))


str(population_df)
head(population_df)


# 6.
library(tidyverse)
library(janitor)

population_clean <- population_df %>%
  as_tibble() %>%
  row_to_names(row_number = 1)  

# remove state column
population_clean <- population_clean %>%
  select(-state)

# rename NAME column to state_name
population_clean <- population_clean %>%
  rename(state_name = NAME)

# parse all relevant colunns to numeric
population_clean <- population_clean %>%
  mutate(
    POP_2020 = as.numeric(POP_2020),
    POP_2021 = as.numeric(POP_2021)
  )

# add state abbreviations using state.abb variable mapped from the state.name variable
population_clean <- population_clean %>%
  mutate(
    state_abbr = state.abb[match(state_name, state.name)],
    state_abbr = case_when(
      state_name == "District of Columbia" ~ "DC",
      state_name == "Puerto Rico" ~ "PR",
      TRUE ~ state_abbr
    )
  )

population_clean <- population_clean %>%
  mutate(state_name = sapply(state_name, as.character))  

# 检查修复后的数据
head(population_clean)



# 7.
population_long <- population_clean %>%
  pivot_longer(cols = c(POP_2020, POP_2021), 
               names_to = "Year", 
               values_to = "Population") %>%
  mutate(Year = gsub("POP_", "", Year)) %>%  
  arrange(desc(Population))  


ggplot(population_long, aes(x = reorder(state_name, -Population), y = Population, fill = Year)) +
  geom_col(position = "dodge") +  
  coord_flip() +  
  facet_wrap(~Year, scales = "free_y") +  
  labs(title = "Population of US States in 2020 and 2021",
       x = "State",
       y = "Population") +
  theme_minimal()

head(population_long, 10)


# 8.
library(jsonlite)
library(purrr)
library(tidyverse)


url_regions <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions_data <- fromJSON(url_regions)


str(regions_data)
head(regions_data)

regions_clean <- regions_data %>%
  unnest(cols = c(states)) %>%
  rename(state_name = states)  # 只重命名 states 列，避免冲突

str(regions_clean)
head(regions_clean)

regions_clean <- regions_clean %>%
  mutate(region_name = case_when(
    region_name == "East South Central" ~ "ESC",  
    TRUE ~ region_name
  ))

regions_clean <- regions_clean %>%
  mutate(region_name = as.factor(region_name))

str(regions_clean)
head(regions_clean)

# 9.
population_with_region <- population_clean %>%
  left_join(regions_clean, by = "state_name")

# Convert region columns to integer types
population_with_region <- population_with_region %>%
  mutate(region = as.integer(unlist(region)))

head(population_with_region)


# 10.
# Load required library
library(httr2)
library(jsonlite)
library(dplyr)

# Define API endpoint
api <- "https://data.cdc.gov/resource/pwn4-m3yp.json"

# Send GET request and retrieve response
cases_raw <- request(api) |> 
  req_perform() |> 
  resp_body_json(simplifyVector = TRUE) |> 
  as_tibble()

# Print first few rows to check data
head(cases_raw)


