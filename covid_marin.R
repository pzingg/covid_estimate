library(dplyr)
library(growthrates)
library(ggplot2)
library(lubridate)
library(tidyverse)

file_name <- "us-counties.csv"
# Source: New York Times counties data, updated nightly:
us_counties_url <- "https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"
download.file(us_counties_url, file_name, "wget", quiet = FALSE, cacheOK = FALSE)

# date,county,state,fips,cases,deaths
us_counties <- read_csv(file_name)
marin <- us_counties %>% filter(fips == "06041") # (county == "Marin", state == "California")

# Number of days of recent data to use
use_days <- 15L
# Number of days to predict and plot
num_days <- use_days + 7L

covid <- marin %>% tail(use_days)
covid$time <- seq_along(covid$date)

# Defaults for fit_easylinear are h = 5, quota = 0.95
# Using a smaller h value generally results in a higher mumax
# (growth rate), because of lack of testing in early days.
fit <- fit_easylinear(covid$time, covid$cases)
est <- predict(fit, newdata = list(time = 1:num_days))
est <- full_join(covid, est, by = "time")
est$y <- round(est$y)
est$date <- seq(est$date[[1]], by = "days", length.out = num_days)

mu <- fit@par["mumax"]
names(mu) <- NULL
percent_increase <- (exp(mu) - 1.0) * 100.0
doubling_time <- log(2.0) / mu
most_recent_cases <- covid[nrow(covid),]$cases
most_recent_deaths <- covid[nrow(covid),]$deaths
most_recent_date <- covid[nrow(covid),]$date
title <- paste0(
  "COVID-19 estimates\n",
  "most recent data, on ", most_recent_date, ": ",
  format(most_recent_cases, big.mark = ",", scientific = FALSE), " cases and ",
  format(most_recent_deaths, big.mark = ",", scientific = FALSE), " deaths\n",
  "mumax: ", round(mu, digits = 2), "\n",
  "daily percentage increase: ", round(percent_increase, digits = 2), "\n",
  "doubling time in days: ", round(doubling_time, digits = 2))

bar_plot <- function(data, title, log_y = FALSE) {
  scale_y <- ifelse(log_y, scale_y_log10, scale_y_continuous)
  ggplot(data, aes(x = date)) +
    geom_point(aes(y = cases), colour = "blue", shape = "circle filled", size = 5, stroke = 2) +
    geom_point(aes(y = deaths), colour = "black", shape = "circle filled", size = 5, stroke = 2) +
    geom_col(aes(y = y), colour = "red", alpha = 0.3) +
    scale_y(labels = function(n) { format(n, big.mark = ",", scientific = FALSE) }) +
    labs(x = "Date", y = "Marin Predicted Cases") +
    ggtitle(title)
}

bar_plot(est, title)
ggsave("covid_marin.png", device = "png")
