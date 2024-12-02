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
# Price analysis
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

# identifying potential outliers using the Interquartile Range (IQR) method
neighborhood_outliers <- data_EDA %>%
  group_by(neighbourhood_cleansed) %>%
  mutate(
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    is_outlier = price > upper_bound | price < lower_bound
  )

# Calculate percentage of outliers per neighborhood
outlier_summary <- neighborhood_outliers %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    total_listings = n(),
    outliers = sum(is_outlier, na.rm = TRUE),
    outlier_percentage = (outliers/total_listings) * 100
  )

print(outlier_summary)

# Visualize the median prices by neighborhood
ggplot(neighborhood_prices, 
       aes(x = reorder(neighbourhood_cleansed, median_price), 
           y = median_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  
  theme_minimal() +
  labs(
    title = "Median Property Prices by Rome Neighborhood",
    subtitle = "Neighborhoods ordered by median price",
    x = "Neighborhood",
    y = "Median Price (€)"
  ) +
  
  geom_text(aes(label = sprintf("€%.0f", median_price)), 
            hjust = -0.1, 
            size = 3)

# First, let's check if we have missing values in the price column
missing_prices <- sum(is.na(data_EDA$price))
print(paste("Number of missing prices:", missing_prices))


# Now let's fix the boxplot code by adding na.rm = TRUE to the quantile function
ggplot(data_EDA, aes(x = reorder(neighbourhood_cleansed, price, FUN = median), 
                     y = price)) +
  geom_boxplot(fill = "darkblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Price Distribution Across Rome Neighborhoods",
    subtitle = "Box plots showing price ranges and outliers",
    x = "Neighborhood",
    y = "Price (€)",
    caption = "Note: Y-axis limited to 95th percentile for better visualization"
  ) +
  scale_y_continuous(limits = c(0, quantile(data_EDA$price, 0.95, na.rm = TRUE))) +
  theme(axis.text.y = element_text(size = 8))

# Analyse the distribution of prices by room type
room_type_prices <- data_EDA %>%
  group_by(room_type) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    count = n()
  )

print(room_type_prices)

ggplot(data_EDA, aes(x = room_type, y = price, fill = room_type)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  labs(
    title = "Price Distribution by Room Type",
    x = "Room Type",
    y = "Price (€)"
  ) +
  scale_y_continuous(limits = c(0, quantile(data_EDA$price, 0.95, na.rm = TRUE))) +
  theme(legend.position = "none")


# Correlation between price and review scores
price_correlations <- data_EDA %>%
  summarise(
    price_review_correlation = cor(price, review_scores_rating, use = "complete.obs"),
  )

print(price_correlations)

# Visualize the relationship between price and review scores
ggplot(data_EDA, aes(x = price, y = review_scores_rating)) +
  geom_point(alpha = 0.1) +  # Make points semi-transparent
  geom_smooth(method = "lm", color = "red") +  # Add a trend line
  theme_minimal() +
  labs(
    title = "Price vs Review Scores in Rome Airbnb Listings",
    subtitle = "Showing near-zero correlation",
    x = "Price (€)",
    y = "Review Score"
  ) +
  scale_x_continuous(limits = c(0, quantile(data_EDA$price, 0.95, na.rm = TRUE)))





