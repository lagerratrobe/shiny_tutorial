library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

# Read in the entire data and convert obsTimeLocal to POSIXct data type
getData <- function() {
  df <- readRDS(
    url("https://github.com/lagerratrobe/weather_station/raw/main/Data/station_obs.RDS")) |> 
    filter(stationID == "KWASEATT2743") |>
    mutate(stationID,
           "Time" = lubridate::parse_date_time(
             obsTimeLocal,
             "Ymd HMS",
             tz = "UTC",
             truncated = 0,
             quiet = FALSE,
             exact = FALSE) ,
           "Temperature" = imperial.temp,
           "Precip" = imperial.precipTotal,
           .keep = "none") |>
    arrange(desc(Time)) %>% 
    head(n=48)
  return(df)
}

