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

# formatting values and verifying proper formatting

# Define verification function
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
      Processed = processed_data[[field]][sample_indices]
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

#ADD PROPER COMMENTS
# Create processed data
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


# Running verification
verify_data_processing(data, data_processed)

glimpse(data_processed)


#checking for missing values processed dataset
analyze_missing_values <- function(data) {
  
      # Calculate missing values for each column
      missing_summary <- data.frame(
        variable = names(data),
        missing_count = sapply(data, function(x) sum(is.na(x))),
        empty_string_count = sapply(data, function(x) if(is.character(x)) sum(x == "", na.rm = TRUE) else 0)
      ) %>%
        mutate(
          total_missing = missing_count + empty_string_count,
          missing_percentage = round(total_missing / nrow(data) * 100, 2)
        ) %>%
        arrange(desc(missing_percentage))
      
      
      # Print summary statistics
      cat("\nTotal number of observations:", nrow(data))
      cat("\nTotal number of variables:", ncol(data))
      
      
      # Print variables with missing values
      cat("\n\nVariables with missing values (including empty strings):\n")
      print(missing_summary[missing_summary$total_missing > 0, ])
      
      
      # Create visualization of missing patterns
      missing_plot <- ggplot(
        missing_summary[missing_summary$total_missing > 0, ], 
        aes(x = reorder(variable, missing_percentage), y = missing_percentage)
      ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Missing Values Analysis",
          subtitle = paste("Dataset with", nrow(data), "observations"),
          x = "Variables",
          y = "Percentage Missing (%)"
        ) +
        theme(axis.text.y = element_text(size = 8))
      
      
      # Analyze missing patterns in key business metrics
      key_metrics <- c("price", "review_scores_rating", "host_response_rate", 
                       "host_acceptance_rate", "bathrooms", "bedrooms")
      
      
      cat("\n\nMissing patterns in key business metrics:\n")
      key_metrics_summary <- data %>%
        summarise(across(all_of(key_metrics), 
                         list(missing = ~sum(is.na(.)),
                              missing_pct = ~round(mean(is.na(.)) * 100, 2))))
      print(key_metrics_summary)
      
      
      # Return both the summary and the plot
      return(list(summary = missing_summary, plot = missing_plot))
}


# Run the analysis on your processed data
missing_analysis <- analyze_missing_values(data_processed)

# Print the summary
print(missing_analysis$plot)


# Additional analysis for specific business contexts
# Check if missing reviews are related to listing age
review_analysis <- data_processed %>%
  mutate(
    listing_age = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    has_missing_review = is.na(review_scores_rating)
  ) %>%
  group_by(has_missing_review) %>%
  summarise(
    avg_listing_age = mean(listing_age, na.rm = TRUE),
    count = n(),
    percentage = n() / nrow(data_processed) * 100
  )

cat("\n\nAnalysis of missing reviews by listing age:\n")
print(review_analysis)



# Function to format boolean fields to 1/0 with empty string handling
format_boolean_fields <- function(data) {
  # List of columns that should be boolean
  boolean_columns <- c(
    "has_availability",
    "host_is_superhost",
    "host_has_profile_pic",
    "host_identity_verified",
    "instant_bookable"
  )
  
  # Handle has_availability separately due to different value encoding
  data_formatted <- data %>%
    mutate(
      # Convert regular boolean fields
      across(
        all_of(boolean_columns),
        ~case_when(
          . == "t" ~ 1,
          . == "f" ~ 0,
          . == "" ~ NA_real_,
          TRUE ~ NA_real_
        )
      ),
      
    )
  
  # Verify the conversion
  cat("\nAfter conversion - unique values in boolean fields:\n")
  for(col in c(boolean_columns)) {
    if(col %in% names(data_formatted)) {
      cat("\n", col, ":", toString(unique(data_formatted[[col]])))
    }
  }
  
  return(data_formatted)
}

# Apply the formatting
data_processed <- format_boolean_fields(data_processed)

glimpse(data_processed)
skim(data_processed)

#save data processed
save(data_processed, file = "data_processed.RData")


