######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################

# Calculate inverse variance weights to account for uncertainty
###################################################################

data = adataAC %>% filter(Pathogen=="ESCCOL" & Antibiotic=="CRO" & Specimen=="BLOOD" 
                          & Year %in%c("2021"))


x = data$Resistant
n = data$InterpretableAST
cor = 100000

ivw_pop = function(x, n, data, pathogen, cor = 100000){ # Cor = number to divide the population by to avoid large numbers to work with
  # Loop over the different isolates, and drugs
  for(i in unique(Specimen))
  # Calculate the resistance rate for each country
  # Apply continuity correction by adding 0.5 to numerator and 1 to denominator
  data$p_hat <- (x + 0.5) / (n + 1)
  
  # Calculate the variance of the resistance rate for each country
  data$var_p_hat <- data$p_hat * (1 - data$p_hat) / (n + 1)
  
  # Calculate the inverse variance weights for each country; also weighting for country size
  data$w <- data$TotalPopulation /cor/ data$var_p_hat
  
  # Get weighted average and 95% CI
  ######################################################
  
  # Calculate the weighted average resistance rate
  p_IVW <- sum(data$w * data$p_hat) / sum(data $w)
  
  # Calculate the variance of the combined resistance rate
  var_p_IVW <- 1 / sum(data$w)
  
  # Calculate the standard error of the combined resistance rate
  SE_IVW <- sqrt(var_p_IVW)
  
  # Calculate the 95% confidence interval
  z_score <- 1.96
  CI_lower <- p_IVW - z_score * SE_IVW
  CI_upper <- p_IVW + z_score * SE_IVW
  
  # Calculate the weighted median and IQR
  ########################################################
  # Sort the data by the estimated resistance rates
  data <- data[order(data$p_hat), ]
  
  # Calculate cumulative weights
  # To find the weighted median, we need to determine the point where the cumulative weight reaches
  # or exceeds 50% of the total weight. This is where the weighted median lies.
  data$cum_w <- cumsum(data$w)
  
  # Calculate total weight
  total_w <- sum(data$w)
  
  # Calculate the weighted median; total_weight / 2 finds the halfway point of the total weight.
  median_index <- which(data$cum_w >= total_w / 2)[1]
  weighted_median <- data$p_hat[median_index]
  
  # Calculate the 25th percentile
  percentile_25_index <- which(data$cum_w <= 0.25 * total_w)[1]
  weighted_percentile_25 <- data$p_hat[percentile_25_index]
  
  # Calculate the 75th percentile
  percentile_75_index <- which(data$cum_w >= 0.75 * total_w)[1]
  weighted_percentile_75 <- data$p_hat[percentile_75_index]


  # Display the results
  print(data)
  cat("Combined Resistance Rate (p_IVW):", p_IVW, "\n")
  cat("Variance of Combined Resistance Rate (var_p_IVW):", var_p_IVW, "\n")
  cat("Standard Error (SE_IVW):", SE_IVW, "\n")
  cat("95% Confidence Interval (CI): [", CI_lower, ", ", CI_upper, "]\n")
  cat("Weighted Median:", weighted_median, "\n")
  cat("Weighted 25th-75th Percentile: [", weighted_percentile_25, ", ", weighted_percentile_75, "]\n")
)


