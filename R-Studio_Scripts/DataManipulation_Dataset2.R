# Load necessary library
library(dplyr)

# Read the data from the CSV file
data <- read.csv("holiday_activities.csv")

################################################################################
# Manipulate to get Major_Activities, Year, Country, Tourist_Arrivals
################################################################################

# Select the required columns: Major_Activities, Year, Country, Tourist_Arrivals
new_data <- data %>% select(Major_Activities, Year, Country, Tourist_Arrivals)

# Check output before exporting to CSV file
head(new_data)

# Export to CSV file to be used for Vegalite
write.csv(new_data, "filtered_holiday_activities.csv", row.names = FALSE)

################################################################################
# Manipulate to get Major_Activities, Tourist_Arrivals
################################################################################

# Read the data from the CSV file
another_data <- read.csv("filtered_holiday_activities.csv")

# Group by Major_Activities and summarize the total Tourist_Arrivals
new_data <- another_data %>%
  group_by(Major_Activities) %>%
  summarise(Total_Tourist_Arrivals = sum(Tourist_Arrivals, na.rm = TRUE))

# Check the output before exporting to CSV file
head(new_data)

# Export to a new CSV file for use with Vega-Lite
write.csv(new_data, "filtered_by_holiday_activities.csv", row.names = FALSE)

