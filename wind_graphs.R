#plot montlhy avg wind speed for each station
#plot(crosby_monthly_avg$month, crosby_monthly_avg$avg)
library(ggfortify)
ggplot(crosby_monthly_avg, aes(x = month, y = avg)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal()

ggplot(crosby_monthly_avg, aes(x = month, y = adjusted_avg)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal()

ggplot(all_monthly_avg, aes(x = month, y = avg)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal() +
  ggplot2::facet_grid(.~ station)


# create autocorrelation function for observed data
autoplot(acf(adj_crosby_data$adj_wind_speed_avg))
autoplot(acf(crosby_markov_estimate$values))

# plot probability distributions
probs <- calculate_probability_distribution(adj_crosby_data$adj_wind_speed_avg)
ggplot(probs, aes(x = wind_values, y = probability)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal()


probs2 <- calculate_probability_distribution(crosby_markov_estimate$values)
probs2$wind_values <- as.numeric(probs2$wind_values)
probs2 <- probs2[order(probs2$wind_values),] 
probs2$wind_values <- as.factor(probs2$wind_values)
ggplot(probs2, aes(x = wind_values, y = probability)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal()