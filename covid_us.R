library(dplyr)
library(growthrates)
library(ggplot2)

bar_plot <- function(data, log_y = FALSE) {
  scale_y <- ifelse(log_y, scale_y_log10, scale_y_continuous)
  ggplot(data, aes(x = data$dates)) +
    geom_point(aes(y = data$cases), colour = "black", shape = "circle filled", size = 5, stroke = 2) +
    geom_col(aes(y = data$y), colour = "red", alpha = 0.3, width = 0.75) +
    scale_x_date(date_labels = "%m/%d") +
    scale_y(labels = function(n) { format(n, big.mark = ",", scientific = FALSE) }) +
    labs(x = "Date", y = "USA Predicted Cases")
}

# Source: CNN. Checked every evening at
# https://www.cnn.com/2020/03/03/health/us-coronavirus-cases-state-by-state/index.html
cases_since_march_1 <- c(
    89,  105,  125,  159,  227,  331,  444,  564,  728, 1000, # 3/1 to 3/10
  1267, 1587, 2131, 2795, 3482 # 3/11 to 3/20
)

# Number of days to predict and plot
num_days <- 32

covid <- data.frame(
  time = seq_along(cases_since_march_1),
  cases = cases_since_march_1
)
fit <- fit_easylinear(covid$time, covid$cases, h = 10)

est <- predict(fit, newdata = list(time = 1:num_days))
est <- full_join(covid, est, by = "time")
est$dates <- seq(as.Date("2020-03-01"), by = "days", length = num_days)

bar_plot(est)
