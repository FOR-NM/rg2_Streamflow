##==============================================================================
## Project: QuEST
## Getting pressure data from Meteostat site using API
## Meteostat site: https://meteostat.net/en/place/us/fayetteville-ar?s=KFYV0&t=2025-05-21/2025-05-28
## API site: https://rapidapi.com/meteostat/api/meteostat/playground/apiendpoint_5db9ec45-3df4-417b-a481-a037efc8cd9d
## press Command+Option+O to collapse all sections and get an overview of the workflow
## used Fayetteville as closest station
##==============================================================================

##################
#### Packages ####
##################
library(httr)
library(jsonlite)

# Define the API endpoint for point data (using latitude and longitude)
# Note: Changed from /stations/hourly to /point/hourly
url <- "https://meteostat.p.rapidapi.com/stations/hourly"

# Define the query parameters
# Had to download data month by month since it is only allowed to do 30 days at a time for hourly data
queryString <- list(
  station = "KASG0", # this code is for Springdale and not Fayetteville
  start = "2025-04-01",
  end = "2025-04-30"
)

# Make the GET request to the API
# Include your RapidAPI key and host in the headers
response <- VERB(
  "GET",
  url,
  query = queryString,
  add_headers(
    'x-rapidapi-key' = '8c5b0bbf44msh34e55c34f4f108ep15ff6ajsn86cf4e522ba0', # My RapidAPI Key
    'x-rapidapi-host' = 'meteostat.p.rapidapi.com' # The RapidAPI Host
  )
)

# Check the status of the response
# A status code of 200 indicates success. If 400 something is wrong
print(paste("HTTP Status Code:", status_code(response)))

# Extract and print the content of the response as text
# This will be a JSON string
mes <- content(response, "text", encoding = "UTF-8")
print(mes)

# Assuming 'api_response_content' holds the JSON string from the API call
parsed_mes <- fromJSON(mes)

# The actual weather data will likely be in a 'data' element within the parsed_data object
# You can inspect the structure to find it:
str(parsed_mes)

# For example, if the weather data is directly under a 'data' field:
mes_df <- parsed_mes$data

# save files
write.csv(mes_df, "2025-04.csv")
