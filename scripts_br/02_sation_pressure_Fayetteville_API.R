##==============================================================================
## Project: QuEST
## Here I'm just fixing pressure data for Brush Creek 2024 
## Data is taken from National Weather Service from the Fayetteville Drake Field station
## API documentation can be found here: https://www.weather.gov/documentation/services-web-api
## An examle on how to use it can be found here https://rpubs.com/anacolla2/USNWS-API
##==============================================================================

# load libraries
library(httr2)
library(tidyverse)
library(repurrrsive)
library(ggplot2)
library(weathR)


# assigning values
api_base_url <-  "https://api.weather.gov/"
ny_coords <- "https://api.weather.gov/points/36.1338,-93.9514" # Brush Creek outlet coodinates

# requesting from the API
ask_api<- request(ny_coords) %>% 
  req_perform()

api_response <- resp_body_json(ask_api) %>% 
  glimpse()

# assigning endpoint value
forecast <- "https://api.weather.gov/gridpoints/TSA/115,104/forecast"

# requesting from the API again
ask_again <- 
  request(forecast) %>% 
  req_perform()

response_again <- 
  resp_body_json(ask_again) %>% 
  glimpse()

# converting to Tibble
response_df<- tibble(response_again)

str(response_df) %>% 
  print()

str(response_df$response_again[[4]]) %>% 
  glimpse()

# selecting the lists to unnest
forecast <- tibble(response_df$response_again[[4]])
forecast <- tibble(forecast$`response_df$response_again[[4]]`[[7]])

forecast <- forecast %>%
  unnest_wider(`forecast$\`response_df$response_again[[4]]\`[[7]]`) %>% 
  print()

# isolating columns of interest
forecast <- forecast %>% 
  select(number, name, startTime, temperature, temperatureUnit, shortForecast) %>% 
  print()

# finalizing the data frame clean
forecast$startTime <- as.Date(forecast$startTime)

forecast <- forecast %>%
  mutate(day= weekdays(startTime)) %>% 
  arrange(startTime) %>% 
  group_by(startTime)

# creating a forecast visualization using ggplot
forecast %>%
  ggplot(aes(day, temperature, fill= temperature)) +
  geom_line(linewidth = 50) +
  facet_wrap(~day, scales = "free_x",
             ncol = 7, axis.labels = "all_y", drop = TRUE) +
  ylab("Temperature (F)") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid = element_line(colour ="#FFFFFF")) +
  labs(title = "7 DAY FORECAST", caption = "")
