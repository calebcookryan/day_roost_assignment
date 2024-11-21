# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
load(pinery_readercapture_upto2024)
# Function to fetch sunrise and sunset data for a given date
get_sun_data <- function(date) {
  response <- GET(
    url = "https://api.sunrise-sunset.org/json",
    query = list(
      lat = 43.25176,   # Latitude for Pinery Provincial Park
      lng = -81.84901,  # Longitude for Pinery Provincial Park
      date = date,
      formatted = 0    # Use UTC time for easier conversion
    )
  )
  
  # Parse response as text
  raw_data <- content(response, as = "text", encoding = "UTF-8")
  
  # Parse JSON and handle errors
  parsed_data <- tryCatch({
    jsonlite::fromJSON(raw_data)
  }, error = function(e) {
    warning(paste("Failed to parse JSON for date:", date))
    return(NULL)
  })
  
  # Check if parsed data contains results
  if (!is.null(parsed_data) && !is.null(parsed_data$results)) {
    sunrise_utc <- as.POSIXct(parsed_data$results$sunrise, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    sunset_utc <- as.POSIXct(parsed_data$results$sunset, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    
    # Convert to UTC-4 (Etc/GMT+4)
    sunrise_utc4 <- with_tz(sunrise_utc, tz = "Etc/GMT+4")
    sunset_utc4 <- with_tz(sunset_utc, tz = "Etc/GMT+4")
    
    # Return formatted results
    return(data.frame(
      date = as.character(date),
      sunrise = format(sunrise_utc4, "%Y-%m-%d %H:%M:%S"),
      sunset = format(sunset_utc4, "%Y-%m-%d %H:%M:%S")
    ))
  } else {
    # Handle missing or invalid results
    warning(paste("No valid results for date:", date))
    return(data.frame(
      date = as.character(date),
      sunrise = NA,
      sunset = NA
    ))
  }
}

# Generate date sequences for 2022, 2023, 2024
all_dates <- seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by = "day")

# Fetch sunrise/sunset data for all dates
sun_data_list <- lapply(all_dates, get_sun_data)

# Combine all data into a single data frame
sun_data_df <- do.call(rbind, sun_data_list)

# Add a reader_date column for joining
sun_data_df <- sun_data_df %>%
  mutate(reader_date = as.Date(date))

# Example: Joining with reader capture data
# Assuming `pinery_readercapture_upto2024` is your existing data frame
data_with_sun <- pinery_readercapture_upto2024 %>%
  mutate(
    reader_datetime = as.POSIXct(reader_dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  ) %>%
  left_join(sun_data_df, by = "reader_date") %>%
  mutate(
    sunrise_datetime = as.POSIXct(sunrise, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    sunset_datetime = as.POSIXct(sunset, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    day_tf = reader_datetime >= sunrise_datetime & reader_datetime <= sunset_datetime
  )

# Filter for daytime records
day_recordsPPP <- data_with_sun %>%
  filter(day_tf != TRUE) %>%
  group_by(pit, reader_date) %>%
  slice(which.min(abs(sunset_datetime - reader_datetime))) %>%
  filter(reader_datetime <= sunrise_datetime) %>%
  filter(reader_datetime >= (sunrise_datetime - dminutes(210)))

# Save the resulting data
save(day_recordsPPP, file = "day_recordsPPP.RDATA")