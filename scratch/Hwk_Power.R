# Install and load necessary packages
#install.packages(c("pwr", "MASS"))
library(pwr)
library(MASS)

# Function to simulate data and fit a model
simulate_and_fit <- function(n, effect_size) {
  # Simulate data with specified effect size
  sim_data <- hwk_data
  sim_data$Social_Index <- rbinom(nrow(sim_data), mean = effect_size * sim_data$Social_Index)

  # Fit the model
  # model <- glm(Mid_Join_Index2 ~ Social_Index + SpeciesID + Pre_Height + Pre_Dense,
    #           family = poisson(link = "log"), data = sim_data)
  model <- glm(alarm ~ Social_Index + SpeciesID + Pre_Height + Pre_Dense, data = hwk_data, family = binomial(link = "logit"))

  # Check if the coefficient for Social_Index is statistically significant
  p_value <- summary(model)$coefficients["Social_Index", "Pr(>|z|)"]

  # Return 1 if the p-value is less than alpha, 0 otherwise
  return(as.numeric(p_value < alpha))
}

# Set values for effect size, alpha, and power
effect_size <- 0.2
alpha <- 0.05
power <- 0.80

# For logistic regression, df1 (degree of freedom for the numerator) is typically set to 1
sample_size <- pwr::pwr.2p.test(h = sqrt(effect_size^2 / (1 + effect_size^2)), sig.level = alpha, power = power)$n

# Perform power analysis with the estimated sample size
set.seed(123)  # For reproducibility
power_analysis <- replicate(1000, simulate_and_fit(sample_size, effect_size))

# Calculate power
power_estimate <- mean(power_analysis)
power_estimate

# Print the estimated sample size
cat("Estimated Sample Size:", round(sample_size), "\n")
