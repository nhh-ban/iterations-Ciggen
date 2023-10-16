
# --- Loading packages

library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(anytime)
library(glue)

# Function to unlists the elements if value is empty or NULL. 
unlist_safe <- function(x, na_value = NA_real_) {
  res <- unlist(x)
  if (is.null(res) || length(res) == 0) {
    na_value
  } else {
    res
  }
}

# Function to transform data from veivesenet-API to dataframe. 
transform_metadata_to_df <- function(metadata) {
  
  # Convert data to tibble.
  metadata_df <- metadata$trafficRegistrationPoints %>%
    map_dfr(as_tibble)  # Convert each list inside mainlist to a tibble and bind them together. 
  
  # Extract coordinates (lot, lan) in seperate columns
  metadata_df <- metadata_df %>%
    mutate(
      latestData = map_chr(latestData, ~ ifelse(is.null(.x), NA_character_, as.character(.x))) %>%
        map_chr(~ifelse(.x == "", NA_character_, .x)) %>%
        ymd_hms(tz = "UTC"),  # Convert timeformat to UTC. 
      # Using the unlist-safe function to handle NULL-values. 
      location = map(location, unlist_safe),  
      # Extracting lat from coordinates, and using the unlist-safe function to handle NULL-values. 
      lat = map_dbl(location, ~ unlist_safe(.x[["latLon.lat"]], NA_real_)),  
      # Extracting lot from coordinates, and using the unlist-safe function to handle NULL-values. 
      lon = map_dbl(location, ~ unlist_safe(.x[["latLon.lon"]], NA_real_))  
    ) %>%
    # Removing the original location column. 
    select(-location)  
  
  return(metadata_df)
}


### Problem 4 - getting volum data 

# Task 4a - time-function

to_iso8601 <- function(datetime, offset_days = 0) {
  # Add or remove days 
  adjusted_time <- datetime + lubridate::days(offset_days)
  
  # Convert to ISO8601 format and adding "Z" to indicate UTC- timezone. 
  iso_time <- anytime::iso8601(adjusted_time)
  iso_time_with_z <- paste0(iso_time, "Z")
  
  return(iso_time_with_z)
}





