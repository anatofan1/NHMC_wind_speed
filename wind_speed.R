library(readr)
library(ggplot2)
library(dplyr)
library(OHPL)
library(rflow)
library(markovchain)
library(ggfortify)

# in the mc_model_functions all the functions that 
# hide the complexity of the algorithm can be found
source("mc_model_functions.R")


wind_speed <- read_data("wind_speed.csv")
crosby_data <- prepare_crosby_df(wind_speed)

all_monthly_avg <- wind_speed %>%
  dplyr::group_by(station, month = as.factor(month)) %>%
  dplyr::summarise(avg = mean(wind_speed_avg))

# try to set up proper cache for time consuming function for optimal partitioning.
# If the cache is not yet configured, create a new one
tryCatch({
    rflow::set_current_eddy("crosby_eddy")
  }, error = function(e) {
    configure_function_cache("cache", "crosby_eddy")
    rflow::set_current_eddy("crosby_eddy")
})

### --------- STEP 1: SEASONAL EFFECT PARTITION  ------------------

# Given the fact that Fisher Optimal partitioning method 
# takes a lot of time to run, we cache the results on the disk with the help of rflow package.
# This way, instead of recomputing partitioning everytime the program is run,
# the already computed results are extracted from local cache.
# Following the article instructions, a number of 10 segments(partitions) has been chosen
crosby_matrix <- matrix(as.numeric(crosby_data$wind_speed_avg), ncol= 1)
flow_function_crosby <- rflow::flow_fn(crosby_matrix, 10, fn = perform_fisher_partitioning)
collected_crosby_partitions <- flow_function_crosby %>%
                                rflow::collect()
# add obtained seasonal segment to id to each corresponding
# observation from crosby data
crosby_data$seasonal_segment <- collected_crosby_partitions

### --------- STEP 2: SEQUENCE PERIOD EXTRACTION  ------------------
autoplot(acf(crosby_data$wind_speed_avg, plot = FALSE)) + 
  ggplot2::ggtitle("Autocorrelation function of the observed wind speed")
# by interpreting the autocorrelation plot, we get to the conclustion
# that the cyclic period is 24 hours

### --------- STEP 3: NHMC MODEL CONSTRUCTION   ------------------
# lower seasonal impact by calculating seaonal index and 
# using it to re-compute adjusted values of speed
adj_crosby_data <- adjust_seasonal_impact(crosby_data)

# Calculate monthly averages for crosby and for the entire dataset
crosby_monthly_avg <- adj_crosby_data %>%
  dplyr::group_by(station, month = as.factor(month)) %>%
  dplyr::summarise(avg = mean(wind_speed_avg),
                   adjusted_avg = mean(adj_wind_speed_avg))


# Use the adjusted data in order to compute the NHMC estimates
crosby_markov_model <- create_nhmc_model(adj_crosby_data)

cumulative_transition_probs <- calculate_cumulative_trans_matr(crosby_markov_model)

#transform crosby markov model in markovChainList object in order
# to perform estimation of wind speed based on nhmc model
crosby_markov_model <- create_markov_chain_object(crosby_markov_model)
crosby_markov_estimate <- rmarkovchain(nrow(crosby_data)/24, crosby_markov_model)

estimated_values <- as.numeric(crosby_markov_estimate$values)
  
# reverse the seasonal adjustments made at step 1, in order to
# obtain realistic, seasonal results
# calculate annual mean
annual_mean <- mean(crosby_data$wind_speed_avg)

# calculate seasonal index for each seasonal segment
seasonal_means_data <- crosby_data %>%
  dplyr::group_by(seasonal_segment) %>%
  dplyr::summarise(seasonal_mean = mean(wind_speed_avg)) %>%
  dplyr::mutate(seasonal_index = seasonal_mean/annual_mean) 

crosby_data$nhmc_estimates <- estimated_values

crosby_data <- readjust_seasonal_impact(crosby_data)

# Calculate monthly averages for crosby and for the entire dataset
crosby_monthly_avg_new <- crosby_data %>%
  dplyr::group_by(station, month = as.factor(month)) %>%
  dplyr::summarise(avg = mean(wind_speed_avg),
                   adjusted_avg = mean(nhmc_estimates))


autoplot(acf(crosby_data$nhmc_estimates, plot = FALSE)) + 
  ggplot2::ggtitle("Autocorrelation function of estimated wind speed values")

ggplot(crosby_monthly_avg, aes(x = month, y = avg)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Monthly averages of observed wind speed")

ggplot(crosby_monthly_avg_new, aes(x = month, y = adjusted_avg)) +
  ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Monthly averages of NHMC estimates")

### ----------- STEP 4: CHECK HOMOGENEITY ---------------------

#homogenous_transition <- generate_transition_matrix(adj_crosby_data)
#sequence <- adj_crosby_data$adj_wind_speed_avg
#transition_matrix <- generate_transition_matrix(adj_crosby_data)
#theoreticalMc <- as(transition_matrix, "markovchain")
#verifyEmpiricalToTheoretical(data=sequence, object=theoreticalMc)

#chisq <- 1
#frequency_matrix <- calculate_frequency_matrix(adj_crosby_data)
#for (l in 1:24){
#  for(i in 1:nrow(crosby_markov_model[[1]])) {
#    for(j in 1:col(crosby_markov_model[[1]])) {
#      probability_ratio <- crosby_markov_model[[l]][i][j]/homogenous_transition[i, j]
#      ln_probability_ratio <- abs(log(probability_ratio))
#      p <- frequency_matrix[[l]][i][j] * ln_probability_ratio
#    }
#  }
#  chisq <- chisq * p
#}
#chisq <- chisq * 2