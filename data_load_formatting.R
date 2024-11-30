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

#preprocessing function
















