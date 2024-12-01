library(ggplot2)
library(dplyr) 
library(tidyverse)  
library(skimr)  # For detailed summaries
library(DataExplorer)  # For automated EDA
library(Hmisc)  # For detailed descriptions
library(lubridate)  # For date manipulation

listings_dataset = read.csv("./listings.csv", header = TRUE, sep = ",")  # Read the data

# removing personal data function
clean_personal_data <- function(df) {
  personal_columns <- c(
    "host_name",
    "host_url",
    "host_thumbnail_url",
    "host_picture_url",
    "listing_url",
    "picture_url",
    "host_about",
    "host_location",
    "host_neighbourhood"
  )
  
  # Remove columns and create new dataframe
  cleaned_df <- df %>%
    select(-all_of(personal_columns))
  
  # Verify removal
  removed_cols <- setdiff(names(df), names(cleaned_df))
  cat("Removed", length(removed_cols), "columns containing personal information:\n")
  print(removed_cols)
  
  return(cleaned_df)
}

# Cleaning the data from personal info
cleaned_dataset <- clean_personal_data(listings_dataset)

# Function to create random host IDs
create_random_host_ids <- function(data) {
  
  unique_hosts <- unique(data$host_id)
  
  set.seed(123)  
  new_ids <- sample(1:length(unique_hosts), length(unique_hosts))
  
  # Creating mapping between old and new IDs
  host_id_map <- data.frame(
    old_id = unique_hosts,
    new_id = new_ids
  )
  
  # Replacing old IDs with new random IDs
  data_with_new_ids <- data %>%
    left_join(host_id_map, by = c("host_id" = "old_id")) %>%
    select(-host_id) %>%                # Remove old host_id
    rename(host_id = new_id)           # Rename new_id to host_id
  
  # Verifying the replacement
  cat("Original number of unique hosts:", length(unique_hosts), "\n")
  cat("New number of unique hosts:", length(unique(data_with_new_ids$host_id)), "\n")
  
  return(data_with_new_ids)
}

# Create new random host IDs
data = create_random_host_ids(cleaned_dataset)

glimpse(data)
skim(data)

#Formatting values and verifying proper formatting

# 1. Define verification function
verify_data_processing <- function(original_data, processed_data) {
  # Store sample size for verification
  sample_size <- 10
  
  # Price verification
  cat("\n=== Price Verification ===\n")
  sample_indices <- sample(1:nrow(original_data), sample_size)
  price_comparison <- data.frame(
    Original = original_data$price[sample_indices],
    Processed = processed_data$price[sample_indices]
  )
  print(price_comparison)
  
  # Date fields verification
  cat("\n=== Date Fields Verification ===\n")
  date_fields <- c("host_since", "last_scraped", "first_review", "last_review")
  for(field in date_fields) {
    cat("\nChecking", field, ":\n")
    date_comparison <- data.frame(
      Original = original_data[[field]][sample_indices],
      Processed = processed_data[[field]][sample_indices]
    )
    print(date_comparison)
  }
  
  # Percentage fields verification
  cat("\n=== Percentage Fields Verification ===\n")
  percentage_fields <- c("host_response_rate", "host_acceptance_rate")
  for(field in percentage_fields) {
    cat("\nChecking", field, ":\n")
    percentage_comparison <- data.frame(
      Original = original_data[[field]][sample_indices],
      Processed = format(processed_data[[field]][sample_indices] * 100, digits = 2)
    )
    print(percentage_comparison)
  }
  
  # Structure comparison
  cat("\n=== Column Types Comparison ===\n")
  structure_comparison <- data.frame(
    Column = names(processed_data),
    Original_Type = sapply(original_data[names(processed_data)], class),
    Processed_Type = sapply(processed_data, class)
  )
  print(structure_comparison)
}


# 2. Create processed data
data_processed <- data %>%
  mutate(
    price = as.numeric(gsub("[$,]", "", price)),
    host_since = as.Date(host_since),
    last_scraped = as.Date(last_scraped),
    first_review = as.Date(first_review),
    last_review = as.Date(last_review),
    host_response_rate = case_when(
      is.na(host_response_rate) ~ NA_real_,
      host_response_rate == "" ~ NA_real_,
      TRUE ~ as.numeric(str_remove(host_response_rate, "%"))/100
    ),
    host_acceptance_rate = case_when(
      is.na(host_acceptance_rate) ~ NA_real_,
      host_acceptance_rate == "" ~ NA_real_,
      TRUE ~ as.numeric(str_remove(host_acceptance_rate, "%"))/100
    )
  ) %>%
  select(-c(neighbourhood_group_cleansed, calendar_updated))

# 4. Run verification
verify_data_processing(data, data_processed)















