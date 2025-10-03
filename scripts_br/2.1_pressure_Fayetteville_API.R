##==============================================================================
## Project: QuEST
## Getting pressure data from Meteostat site using API
## Meteostat site: https://meteostat.net/en/place/us/fayetteville-ar?s=KFYV0&t=2025-05-21/2025-05-28
## API site: https://rapidapi.com/meteostat/api/meteostat/playground/apiendpoint_5db9ec45-3df4-417b-a481-a037efc8cd9d
## used Fayetteville as closest station
##==============================================================================

##################
#### Packages ####
##################
library(httr)
library(jsonlite)

######################################
#### Download data month my month ####
######################################
# define the API endpoint for point data (using latitude and longitude)
# note: changed from /stations/hourly to /point/hourly
url <- "https://meteostat.p.rapidapi.com/stations/hourly"

# define the query parameters
# had to download data month by month since it is only allowed to do 30 days at a time for hourly data
queryString <- list(
  station = "KFYV0", # this code is for Fayetteville and not Springdale (KASG0)
  start = "2025-09-01",
  end = "2025-09-30"
)

# make the GET request to the API
# include your RapidAPI key and host in the headers
response <- VERB(
  "GET",
  url,
  query = queryString,
  add_headers(
    'x-rapidapi-key' = '8c5b0bbf44msh34e55c34f4f108ep15ff6ajsn86cf4e522ba0', # My RapidAPI Key
    'x-rapidapi-host' = 'meteostat.p.rapidapi.com' # The RapidAPI Host
  )
)

# check the status of the response
# a status code of 200 indicates success. If 400 something is wrong
print(paste("HTTP Status Code:", status_code(response)))

# extract and print the content of the response as text
# this will be a JSON string
mes <- content(response, "text", encoding = "UTF-8")
print(mes)

# assuming 'api_response_content' holds the JSON string from the API call
parsed_mes <- fromJSON(mes)

# the actual weather data will likely be in a 'data' element within the parsed_data object
# you can inspect the structure to find it:
str(parsed_mes)

# for example, if the weather data is directly under a 'data' field:
mes_df <- parsed_mes$data

# save files
write.csv(mes_df, "2025-09.csv")

