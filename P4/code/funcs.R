# Load necessary R packages
library(httr2)
library(httr)
library(jsonlite) 
library(dplyr)   

# Define a function to fetch CDC data
get_cdc_data <- function(endpoint) {
 
  api_base <- "https://data.cdc.gov/resource/"
  api_url <- paste0(api_base, endpoint, ".json")
  
 
  response <- request(api_url) |> 
    req_url_query("$limit" = 10000000) |> 
    req_perform()
  
  
  if (resp_status(response) != 200) {
    stop("Failed to fetch data from CDC API: ", api_url)
  }
  data <- response |> resp_body_json(simplifyVector = TRUE) |> as.data.frame()
  
  return(data)
}

