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

getPlot <- function(weather_data, weather_variable) {
  if (weather_variable == "Temperature") {
    max_temp = max(weather_data$Temperature)
    min_temp = min(weather_data$Temperature)
    max_temp_time = df$Time[which(df$Temperature == max_temp)]
    min_temp_time = df$Time[44] # hard-code to lower left
    plot = ggplot(weather_data, mapping = aes(x = Time, y=Temperature)) + 
      geom_line() +
      geom_hline(yintercept=max(max_temp),color="red") +
      annotate("text", 
               x=max_temp_time , 
               y=max_temp + 1, 
               label=sprintf("High = %s", max_temp),
               color="red") +
      geom_hline(yintercept=max(min_temp),color="blue") +
      annotate("text", 
               x=min_temp_time, 
               y=min_temp + 1, 
               label=sprintf("Low = %s", min_temp),
               color="blue") +
      ggtitle("48-Hour Temps") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Time",
           y = "Temperature")
    
  }
  if (weather_variable == "Precip") {
    plot = ggplot(df, mapping = aes(x=Time, y=Precip)) +
      geom_line()
    
  }
  return(plot)
}

