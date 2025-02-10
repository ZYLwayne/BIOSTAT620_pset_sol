library(httr2)
library(httr)
library(httr2)

census_key <- "1ae9bf658ee82e985ea5157846cb8c459ca78532"
# Send API request to retrieve population data
response <- GET("https://api.census.gov/data/2021/pep/population?get=POP_2020,POP_2021,NAME&for=state:*&key=1ae9bf658ee82e985ea5157846cb8c459ca78532")
# Define the API request correctly
request <- request("https://api.census.gov/data/2021/pep/population") |> 
  req_url_query(
    get = "POP_2020,POP_2021,NAME",
    `for` = "state:*",
    key = census_key
  )

# Perform the request
response <- request |> req_perform()
print(response)

# Extract the data into a matrix
population <- resp_body_json(response, simplifyVector = TRUE)

# Print the first few rows
head(population)

# Load necessary libraries
library(tidyverse)
library(janitor)

# Convert the first row to column names and remove the header row
population <- as_tibble(population) |>
  row_to_names(row_number = 1) |>
  rename(state_name = NAME) |>
  select(-state) |>  # Remove the state ID column
  pivot_longer(cols = starts_with("POP_"), names_to = "year", values_to = "population") |>
  mutate(
    year = str_remove(year, "POP_"),  # Remove "POP_" prefix
    population = as.numeric(population),  # Convert population to numeric
    state = case_when(
      state_name == "District of Columbia" ~ "DC",
      state_name == "Puerto Rico" ~ "PR",
      TRUE ~ state.abb[match(state_name, state.name)]
    )
  )

# Print the first few rows of the cleaned dataset
print(head(population))



