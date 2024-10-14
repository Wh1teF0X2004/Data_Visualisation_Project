# Load necessary libraries
library(dplyr)
library(tidyverse)

# Note: Please move this file to root folder when you intend to run

################################################################################
# Manipulate to get Month and Year
################################################################################

# Read CSV file
data <- read.csv("arrivals_soe.csv")
head(data)

# Ensure the date column is in date format before extraction
data$date <- as.Date(data$date, format = "%Y-%m-%d") # Originally YYYY-MM-DD format

# Extract the month and year from the date
data$month_num <- format(data$date, "%m")  # Extract month number in "YYYY-MM" format
data$month <- format(data$date, "%B")  # Extract month in "YYYY-MM" format
data$year <- format(data$date, "%Y")  # Extract year in "YYYY-MM" format

# Summarize data done
summary_data <- data %>%
  group_by(soe, month, month_num, year) %>%
  summarise(
    total_arrivals = sum(arrivals, na.rm = TRUE),
    arrivals_male = sum(arrivals_male, na.rm = TRUE),
    arrivals_female = sum(arrivals_female, na.rm = TRUE)
  )

# Check output before exporting to CSV file
print(summary_data)

# Export to CSV file to be used for Vegalite
write.csv(summary_data, "arrivals_soe_monthyear.csv", row.names = FALSE)

################################################################################
# Manipulate to add in longitude and latitude from a different CSV file
################################################################################

# Read CSV file
country_lat_long <- read.csv("arrivals_country_longlat.csv")

# Merge summary data with latitude and longitude
final_data <- summary_data %>%
  left_join(country_lat_long, by = "country")

# Check output before exporting to CSV file
print(final_data)

# Export to CSV file to be used for Vegalite
write.csv(final_data, "arrivals_country_coords.csv", row.names = FALSE)

################################################################################
# Manipulate to have gender and thier total arrivals
################################################################################

# using orginal file for this
new_summarized_data <- data %>%
  pivot_longer(
    cols = c(arrivals_male, arrivals_female),
    names_to = "gender",
    values_to = "total_arrivals"
  ) %>%
  mutate(gender = ifelse(gender == "arrivals_male", "Male", "Female")) %>%
  group_by(gender) %>%
  summarize(total_arrivals = sum(total_arrivals), .groups = "drop")

# Check output before exporting to CSV file
print(new_summarized_data)

# Export to CSV file to be used for Vegalite
write.csv(new_summarized_data, "arrivals_gender.csv", row.names = FALSE)

################################################################################
# Manipulate to have soe, country, month, year
################################################################################
# Read CSV file
head(data)

# Ensure the date column is in date format before extraction
data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Originally YYYY-MM-DD format

# Extract the month and year from the date
data$month_num <- format(data$date, "%m")  # Extract month number
data$month <- format(data$date, "%B")  # Extract full month name
data$year <- format(data$date, "%Y")  # Extract year

# Summarize data and include the country column
summary_data <- data %>%
  group_by(soe, country, month, month_num, year) %>%
  summarise(
    total_arrivals = sum(arrivals, na.rm = TRUE),
    arrivals_male = sum(arrivals_male, na.rm = TRUE),
    arrivals_female = sum(arrivals_female, na.rm = TRUE)
  )

# Check output before exporting to CSV file
print(summary_data)

# Export to CSV file to be used for Vegalite
write.csv(summary_data, "arrivals_soe_monthyear_country.csv", row.names = FALSE)
