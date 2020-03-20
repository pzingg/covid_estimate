library(dplyr)
library(growthrates)
library(ggplot2)

bar_plot <- function(data, title, log_y = FALSE) {
  scale_y <- ifelse(log_y, scale_y_log10, scale_y_continuous)
  ggplot(data, aes(x = data$dates)) +
    geom_point(aes(y = data$cases), colour = "black", shape = "circle filled", size = 5, stroke = 2) +
    geom_col(aes(y = data$y), colour = "red", alpha = 0.3, width = 0.75) +
    scale_x_date(date_labels = "%m/%d") +
    scale_y(labels = function(n) { format(n, big.mark = ",", scientific = FALSE) }) +
    labs(x = "Date", y = "USA Predicted Cases") +
    ggtitle(title)
}

# Source: CNN. Checked every evening at
# https://www.cnn.com/2020/03/03/health/us-coronavirus-cases-state-by-state/index.html
cases_since_march_1 <- c(
     89,   105,   125,   159,   227,   331,   444,   564,   728,  1000, # 3/1 to 3/10
   1267,  1587,  2131,  2795,  3482,  4158,  5748,  8736, 13229, 16520  # 3/11 to 3/20
)
len <- length(cases_since_march_1)

# Starting date in March 2020 to do analysis on
start_date <- 14

# Number of days to predict and plot
num_days <- 32

reliable_cases <- cases_since_march_1
if (start_date > 1) {
  reliable_cases[1:(start_date - 1)] <- NA
}
covid <- data.frame(
  time = seq_along(cases_since_march_1),
  cases = cases_since_march_1,
  reliable = reliable_cases
)

# Defaults for fit_easylinear are h = 5, quota = 0.95
# Using a smaller h value generally results in a higher mumax
# (growth rate), because of lack of testing in early days.
fit <- fit_easylinear(covid$time, covid$reliable)
est <- predict(fit, newdata = list(time = 1:num_days))
est <- full_join(covid, est, by = "time")
est$y <- round(est$y)
est$dates <- seq(as.Date("2020-03-01"), by = "days", length = num_days)

mu <- fit@par["mumax"]
names(mu) <- NULL
percent_increase <- (exp(mu) - 1.0) * 100.0
doubling_time <- log(2.0) / mu
title <- paste0(
  "COVID-19 estimates\n",
  "mumax = ", round(mu, digits = 2), "\n",
  "daily percentage increase = ", round(percent_increase, digits = 2), "\n",
  "doubling time in days = ", round(doubling_time, digits = 2))

# bar_plot(est, title)
