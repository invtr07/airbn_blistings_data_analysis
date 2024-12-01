library(ggplot2)
library(dplyr) 
library(tidyverse)  
library(skimr)  # For detailed summaries
library(DataExplorer)  # For automated EDA
library(Hmisc)  # For detailed descriptions
library(lubridate)  # For date manipulation

load("data_processed.RData")
data_EDA = data_processed

# ==============================================================================
# Neighborhood analysis
# ==============================================================================

# 1. Listings distribution by neighborhood
neighborhood_summary <- data_EDA %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    number_of_listings = n(),
    # Also calculating percentage of total listings
    percentage = n() / nrow(data_EDA) * 100
  ) %>%
  arrange(desc(number_of_listings))

# Let's view the top neighborhoods
print(neighborhood_summary)

# 2. Visualizing the distribution
ggplot(neighborhood_summary, 
       aes(x = reorder(neighbourhood_cleansed, number_of_listings), 
           y = number_of_listings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip coordinates for horizontal bars
  theme_minimal() +
  labs(
    title = "Distribution of Listings Across Rome neighboorhoods",
    x = "Neighborhood",
    y = "Number of Listings",
  ) +
  theme(
    axis.text.y = element_text(size = 8), 
    plot.title = element_text(size = 12, face = "bold")
  )


#==============================================================================
# Availability distribution accross neighboorhoods 
#==============================================================================

# Analyze non-available listings distribution across neighborhoods

# Create a summary of non-availability by neighborhood - to see for available 
#listings just change 0 to 1
non_availability_by_neighborhood <- data_EDA %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    total_listings = n(),
    # Count listings that are not available (has_availability = 0)
    non_available_listings = sum(has_availability == 0),
    # Calculate the percentage of non-available listings
    non_availability_rate = (sum(has_availability == 0) / n()) * 100
  ) %>%
  # Sort by non-availability rate in descending order
  arrange(desc(non_availability_rate))

# Visualize the non-availability rates across neighborhoods
ggplot(non_availability_by_neighborhood, 
       aes(x = reorder(neighbourhood_cleansed, non_availability_rate), 
           y = non_availability_rate)) +
  geom_bar(stat = "identity", fill = "coral") +  
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Property Non-Availability Rates Across Rome Neighborhoods",
    subtitle = "Percentage of listings marked as unavailable",
    x = "Neighborhood",
    y = "Non-Availability Rate (%)",
    caption = "Data source: Inside Airbnb"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  # Add percentage labels to the bars
  geom_text(aes(label = sprintf("%.1f%%", non_availability_rate)), 
            hjust = -0.1, 
            size = 3)

# Calculate comprehensive statistics about non-availability
non_availability_stats <- non_availability_by_neighborhood %>%
  summarise(
    overall_non_availability_rate = mean(non_availability_rate),
    sd_non_availability = sd(non_availability_rate),
    min_non_availability = min(non_availability_rate),
    max_non_availability = max(non_availability_rate),
    total_non_available = sum(non_available_listings),
    total_listings = sum(total_listings),
    overall_percentage = (total_non_available / total_listings) * 100
  )

print("Non-Availability Statistics by Neighborhood:")
print(non_availability_by_neighborhood)
print("\nOverall Non-Availability Statistics:")
print(non_availability_stats)

# Examine the relationship between neighborhood size and non-availability rate
correlation_test <- cor.test(
  non_availability_by_neighborhood$total_listings,
  non_availability_by_neighborhood$non_availability_rate
)
print(correlation_test)


#==============================================================================
# Property type distribution
#==============================================================================
property_distribution <- data_EDA %>%
  group_by(property_type) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(data_EDA) * 100)
  ) %>%
  arrange(desc(count))

print(n=72, property_distribution)


#==============================================================================
#Room type distribution
#==============================================================================
room_distribution <- data_EDA %>%
  group_by(room_type) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(data_EDA) * 100)
  ) %>%
  arrange(desc(count))

print(room_distribution)

# Visualize the distribution of room types
ggplot(room_distribution, 
       aes(x = reorder(room_type, count), 
           y = count,
           fill = room_type)) +    # Just add 'fill = room_type' here
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Room Types in Rome Listings",
    x = "Room Type",
    y = "Number of Listings"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold")
  )


#==============================================================================
# Price distribution analysis
#==============================================================================

# Calculate neighborhood price statistics
neighborhood_prices <- data_EDA %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    listing_count = n(),
    # missing prices: "" or Na
    missing_prices = sum(is.na(price) | price == "")
    
  ) %>%
  arrange(desc(median_price))

print(neighborhood_prices)








