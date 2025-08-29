library(tidyverse)
library(dbplyr)
library(grid)
library(StatCompLab)
# Stewart Ross (s2078228)

data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

#' Permutation Test
#' 
#' @param station_id The ID of the station of which the approximated p value and 
#' Monte Carlo Standard Deviation are to be computed
#' @param N The number of permutations
#' 
#' @return A `data.frame` with the associated approximated p value and Monte Carlo Standard Deviation 

permutation_test <- function(station_id, N) {
  station_data <- ghcnds %>%
    filter(ID == station_id, Element == "PRCP")

  # Approximate the winter average precipitation for the given station  
  winter_average <- station_data %>%
    filter(Season == "Winter") %>%
    summarise(Winter_Average = mean(Value))$Winter_Average

  # Approximate the summer average precipitation for the given station   
  summer_average <- station_data %>%
    filter(Season == "Summer") %>%
    summarise(Summer_Average = mean(Value))$Summer_Average

  # Compute the observed test statistic as the average of the winter and summer precipitation
  observed_difference <- abs(winter_average - summer_average)

  # Perform N permutations using the replicate function and calculate permuted differences
  permuted_differences <- replicate(N, {
    permuted_data <- station_data %>%
      mutate(Value = sample(Value, size = nrow(station_data), replace = FALSE)) %>%
      group_by(Season) %>%
      summarise(Permuted_Average = mean(Value), .groups = "drop")
    winter_average <- filter(permuted_data, Season == "Winter")$Permuted_Average
    summer_average <- filter(permuted_data, Season == "Summer")$Permuted_Average
    abs(winter_average - summer_average)})
 
  # Compute the approximated p value for the given station 
  p_value <- mean(abs(permuted_differences) >= abs(observed_difference))
  SD_value <- sqrt(p_value * (1 - p_value) / N)
  return(data.frame(p_value, SD_value))
}

#' Compute the confidence regions
#' 
#' @param p_values List of approximated p values for each station
#' @param SD_Values List of Monte Carlo Standard Deviations for each station 
#' 
#' @return A data frame with each station ID, Name, P value, Monte Carlo standard deviation and 
#' upper and lower confidence limit boundaries

p_value_CI <- function(p_values, SD_values, N) { 
  CI_lower <- numeric(length(p_values))
  CI_upper <- numeric(length(p_values))
  # Treat P=0 as special case
  for (i in 1:length(p_values)){
    if (p_values[i] == 0) {
      upper_limit <- 1 - (0.025^(1 / N))
      CI <- c(0, upper_limit)} 
    else {
      # Compute lower and upper CI values
      lower_limit <- p_values[i] - qnorm(0.975) * SD_values[i]
      upper_limit <- p_values[i] + qnorm(0.975) * SD_values[i]
      CI <- c(lower_limit, upper_limit)}
    CI_lower[i] <- CI[1]
    CI_upper[i] <- CI[2]}
  results_df <- data.frame(ID=ghcnd_stations$ID, Name = ghcnd_stations$Name,
                           P_values = p_values, SD_values = SD_values, 
                           CI_lower = CI_lower, CI_upper = CI_upper)
  return(results_df)
}

# Question 2

#' Model Generator
#' 
#' @param K The model formula to be computed i.e M_k
#' 
#' @return The specified formula as a formula object which can be utilized by the lm function

Model_Generator <- function(K) {
  for (k in 0:K) {
    if (K == 0) {
      model_formula <- "Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYearAvg"} 
    else {
      model_formula <- "Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYearAvg"
      for (k in 1:K) {
        model_formula <- paste0(model_formula, " + I(cos(2 * pi * ", k, " * DecYearAvg)) + I(sin(2 * pi * ", k, " * DecYearAvg))")
      }
    }
    model <- as.formula(model_formula)
  }
   return(model)
}

#' Cross-Validated Scores
#' 
#' @param data The data frame to be analysed
#' @param formula The given M_K formula to be analysed
#' 
#' @return A data frame with the same number of rows as the original data frame
#' and the columns being the given station name, year, month, standard error 
#' and Dawid-Sebastiani scores for each  data entry 

cross_validated_scores <- function(data, formula) {
  result <- data.frame(
    Name=data$Name,
    Year=data$Year,
    Month = data$Month,
    SE = numeric(nrow(data)),
    DS = numeric(nrow(data)))
 for (id in unique(data$ID)) {
    fit <- lm(formula, data %>% filter(ID != id))
    pred <- predict(fit, newdata = data %>% filter(ID == id), se.fit = TRUE)
    residual_var <- sum(fit$residuals^2)/fit$df.residual
    sd_pred <- sqrt(pred$se.fit^2 + residual_var)
    result$SE[data$ID == id] <- (data$Value_sqrt_avg[data$ID == id] - pred$fit)^2
    result$DS[data$ID == id] <- ((data$Value_sqrt_avg[data$ID == id] - pred$fit)^2 / sd_pred^2) + log(sd_pred^2)
    }
  result
} 

# Place your function definitions that may be needed in the report.Rmd, including function documentation.
# You can also include any needed library() calls here
