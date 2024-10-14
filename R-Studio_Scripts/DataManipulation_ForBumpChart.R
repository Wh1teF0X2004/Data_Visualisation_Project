# Load necessary libraries
library(dplyr)

# Read the CSV file
original_dataset <- read.csv("arrivals_soe.csv")

# Convert the date column to Date type 
original_dataset$date <- as.Date(original_dataset$date)

# Extract year from the date
original_dataset$year <- format(original_dataset$date, "%Y")

# Summarize the total arrivals by year and state of origin 
total_arrivals <- original_dataset %>%
  group_by(year, soe) %>%
  summarise(total_arrivals = sum(arrivals, na.rm = TRUE), .groups = 'drop') %>%
  arrange(year, desc(total_arrivals))  # Sort by year and total arrivals in descending order

# Rank the states of origin based on total arrivals within each year
total_arrivals <- total_arrivals %>%
  group_by(year) %>%
  mutate(rank = rank(-total_arrivals, ties.method = "first")) %>%
  ungroup()

# View the final data frame
print(total_arrivals)

# Optionally, save the result to a new CSV file
write.csv(total_arrivals, "ranking_soe_by_popularity.csv", row.names = FALSE)
