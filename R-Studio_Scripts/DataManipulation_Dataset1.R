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

################################################################################
# Manipulate to have full country names
################################################################################

# Load the dplyr package
library(dplyr)

# Read the CSV data
data <- read.csv("arrivals_country_coords.csv")

# Create a data frame with country codes and their full names
country_names <- data.frame(
  country = c("ABW", "AFG", "AGO", "AIA", "ALB", "AND", "ANT", "ARE", "ARG", "ARM", "ATA", "ATG", "AUS", "AUT", "AZE", 
              "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BMU", "BOL", "BRA", "BRB",
              "BRN", "BTN", "BVT", "BWA", "CAF", "CAN", "CCK", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COK",
              "COL", "COM", "CPV", "CRI", "CUB", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY",
              "ERI", "ESP", "EST", "ETH", "FIN", "FJI", "FRA", "FSM", "GAB", "GBR", "GEO", "GHA", "GIB", "GIN", "GLP",
              "GMB", "GNB", "GNQ", "GRC", "GRD", "GRL", "GTM", "GUY", "HKG", "HMD", "HND", "HRV", "HTI", "HUN", "IDN",
              "IND", "IOT", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM",
              "KIR", "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX", "LVA",
              "MAC", "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP",
              "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NCL", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "NRU", "NZL",
              "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", "PRK", "PRT", "PRY", "PSE", "QAT", "REU",
              "ROU", "RUS", "RWA", "SAU", "SDN", "SEN", "SGP", "SGS", "SHN", "SLB", "SLE", "SLV", "SMR", "SOM", "SPM",
              "SRB", "SSD", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK",
              "TKL", "TKM", "TLS", "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB",
              "VAT", "VCT", "VEN", "VNM", "VUT", "WSM", "XXX", "YEM", "ZAF", "ZAR", "ZMB", "ZWE"),
  full_name = c("Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "Andorra", "Netherlands Antilles", "United Arab Emirates", 
                "Argentina", "Armenia", "Antarctica", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", "Burundi", 
                "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", "Bahrain", "Bahamas", "Bosnia and Herzegovina", 
                "Belarus", "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei", "Bhutan", "Bouvet Island", 
                "Botswana", "Central African Republic", "Canada", "Cocos (Keeling) Islands", "Switzerland", "Chile", "China", 
                "Ivory Coast", "Cameroon", "Democratic Republic of the Congo", "Congo", "Cook Islands", "Colombia", "Comoros", 
                "Cape Verde", "Costa Rica", "Cuba", "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", 
                "Dominican Republic", "Algeria", "Ecuador", "Egypt", "Eritrea", "Spain", "Estonia", "Ethiopia", "Finland", 
                "Fiji", "France", "Federated States of Micronesia", "Gabon", "United Kingdom", "Georgia", "Ghana", "Gibraltar", 
                "Guinea", "Guadeloupe", "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Greece", "Grenada", "Greenland", 
                "Guatemala", "Guyana", "Hong Kong", "Heard Island and McDonald Islands", "Honduras", "Croatia", "Haiti", 
                "Hungary", "Indonesia", "India", "British Indian Ocean Territory", "Ireland", "Iran", "Iraq", "Iceland", 
                "Israel", "Italy", "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", "Kyrgyzstan", "Cambodia", "Kiribati", 
                "Saint Kitts and Nevis", "South Korea", "Kuwait", "Laos", "Lebanon", "Liberia", "Libya", "Saint Lucia", 
                "Liechtenstein", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", "Macau", "Morocco", "Monaco", 
                "Moldova", "Madagascar", "Maldives", "Mexico", "Marshall Islands", "North Macedonia", "Mali", "Malta", 
                "Myanmar", "Montenegro", "Mongolia", "Northern Mariana Islands", "Mozambique", "Mauritania", "Mauritius", 
                "Malawi", "Malaysia", "Namibia", "New Caledonia", "Niger", "Nigeria", "Nicaragua", "Netherlands", "Norway", 
                "Nepal", "Nauru", "New Zealand", "Oman", "Pakistan", "Panama", "Peru", "Philippines", "Palau", "Papua New Guinea", 
                "Poland", "Puerto Rico", "North Korea", "Portugal", "Paraguay", "Palestine", "Qatar", "Reunion", "Romania", 
                "Russia", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore", "South Georgia and the South Sandwich Islands", 
                "Saint Helena", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", "Somalia", 
                "Saint Pierre and Miquelon", "Serbia", "South Sudan", "Sao Tome and Principe", "Suriname", "Slovakia", 
                "Slovenia", "Sweden", "Eswatini", "Seychelles", "Syria", "Turks and Caicos Islands", "Chad", "Togo", 
                "Thailand", "Tajikistan", "Tokelau", "Turkmenistan", "Timor-Leste", "Tonga", "Trinidad and Tobago", "Tunisia", 
                "Turkey", "Tuvalu", "Taiwan", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", "Uzbekistan", 
                "Vatican City", "Saint Vincent and the Grenadines", "Venezuela", "Vietnam", "Vanuatu", "Samoa", "Other/Unknown", 
                "Yemen", "South Africa", "Zaire", "Zambia", "Zimbabwe")
)

# Merge the full names with the main dataset
data <- data %>% 
  left_join(country_names, by = "country")

print(data)

# Save the updated data back to a CSV file
write.csv(data, "arrivals_country_coords.csv", row.names = FALSE)

################################################################################
# Manipulate to have ranking, country, country_name, total_arrivals, longitude and latitude columns
################################################################################

# Load necessary library
library(dplyr)

# Read the CSV data
data <- read.csv("arrivals_country_coords.csv")

# Create a ranking based on the 'total_arrivals'
data <- data %>%
  arrange(desc(total_arrivals)) %>%  # Arrange in descending order of total_arrivals
  mutate(ranking = row_number()) %>%  # Add a ranking column based on the order
  select(ranking, country, country_name, total_arrivals, longitude, latitude)  # Select the desired columns
print(data)

# Write the modified data to a new CSV file (optional)
write.csv(data, "flowmap_dataset.csv", row.names = FALSE)
