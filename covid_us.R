library(dplyr)
library(growthrates)
library(ggplot2)
library(lubridate)
library(tidyverse)

# Source: https://github.com/CSSEGISandData/COVID-19. Checked every night:
confirmed_global_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
file_name <- "time_series_covid19_confirmed_global.csv"
download.file(confirmed_global_url, file_name, "wget", quiet = FALSE, cacheOK = FALSE)

confirmed_global <- read_csv(file_name)
confirmed_us <- confirmed_global %>% filter(`Country/Region` == "US") %>% unlist()
len <- length(confirmed_us)
cases <- confirmed_us[5:len] %>% as.double()
reliable <- cases
dates <- names(confirmed_us)[5:len] %>% parse_date_time(orders = "mdy") %>% as.Date()

# Number of days of recent data to use
use_days <- 15L
# Number of days to predict and plot
num_days <- use_days + 7L

covid <- data.frame(
  cases = cases,
  dates = dates
) %>% tail(use_days)
covid$time <- seq_along(covid$cases)

# Defaults for fit_easylinear are h = 5, quota = 0.95
# Using a smaller h value generally results in a higher mumax
# (growth rate), because of lack of testing in early days.
fit <- fit_easylinear(covid$time, covid$cases)
est <- predict(fit, newdata = list(time = 1:num_days))
est <- full_join(covid, est, by = "time")
est$y <- round(est$y)
est$dates <- seq(est$dates[[1]], by = "days", length.out = num_days)

mu <- fit@par["mumax"]
names(mu) <- NULL
percent_increase <- (exp(mu) - 1.0) * 100.0
doubling_time <- log(2.0) / mu
most_recent_cases <- covid[nrow(covid),]$cases
most_recent_date <- covid[nrow(covid),]$dates
title <- paste0(
  "COVID-19 estimates\n",
  "most recent data: ", format(most_recent_cases, big.mark = ",", scientific = FALSE), " cases on ", most_recent_date, "\n",
  "mumax: ", round(mu, digits = 2), "\n",
  "daily percentage increase: ", round(percent_increase, digits = 2), "\n",
  "doubling time in days: ", round(doubling_time, digits = 2))

bar_plot <- function(data, title, log_y = FALSE) {
  scale_y <- ifelse(log_y, scale_y_log10, scale_y_continuous)
  ggplot(data, aes(x = dates)) +
    geom_point(aes(y = cases), colour = "black", shape = "circle filled", size = 5, stroke = 2) +
    geom_col(aes(y = y), colour = "red", alpha = 0.3) +
    scale_y(labels = function(n) { format(n, big.mark = ",", scientific = FALSE) }) +
    labs(x = "Date", y = "USA Predicted Cases") +
    ggtitle(title)
}

bar_plot(est, title)
ggsave("covid_plot.png", device = "png")
