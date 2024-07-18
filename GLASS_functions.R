######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################
library(dplyr)

# Plot raw AMR rates
######################################################
plot_amr_map <- function(shapefile, amr_data,
                         specimen, pathogen_name, antibiotic_name, na_color = "lightgrey") {
  
  # Load the shapefile
  world <- shapefile
  
  # Filter the AMR data
  filtered_rrates <- amr_data %>%
    filter(Specimen == specimen & PathogenName == pathogen_name & AntibioticName == antibiotic_name)
  
  # Ensure the ISO3 column is named correctly for merging
  world <- world %>%
    rename(Iso3 = "ISO_A3")
  
  # Merge the filtered data with the shapefile data
  merged_data <- world %>%
    left_join(filtered_rrates, by = "Iso3")
  
  # Plot the map with AMR rates
  ggplot(data = merged_data) +
    geom_sf(aes(fill = amr_rate)) +
    scale_fill_viridis_c(option = "viridis", na.value = na_color) +  # Handle NA values with the specified color
    theme_minimal() +
    labs(
      title = paste("AMR Rates for", specimen),
      subtitle = paste(pathogen_name, "-", antibiotic_name),
      fill = "AMR Rate"
    )
}


# Calculate inverse variance weights to get weighted estimates
###################################################################
# The function can be used to
# - weight for country size or not by defining pop = Yes or No.
# - Use arbitrary correction for countryi size by defining Cor = number
#   By dividing the population by a large number, it is avoided to workd with large numbers
# - Normalise the weights or not. When using normalised weights, it is avoided that one country has
#   a disproportionaly large effect on the estimate

# ASTdata = adataAC
# year = c("2021")
# pop = "yes"
# pathogen = "Klebsiella pneumoniae"
# s="BLOOD"

ivw <- function(ASTdata, dbdata, year, pathogen, cor = 1000000, pop = c("yes", "no"), normalise_w = c("yes", "no")) {
  rates <- NULL
  
  # Ensure pop and normalise_w are valid
  #pop <- match.arg(pop)
  #normalise_w <- match.arg(normalise_w)
  
  # Loop over the different isolates and drugs
  d <- ASTdata %>% filter(PathogenName == pathogen & Year %in% year)
  for (s in unique(d$Specimen)) {
    db <- dbdata %>% filter(Pathogen == pathogen & Specimen == s)
    data_specimen <- d %>% filter(Specimen == s)
    ra <- NULL
    
    for (a in unique(data_specimen$AntibioticName)) {
      data <- data_specimen %>% filter(AntibioticName == a)
      
      # Calculate the resistance rate for each country
      # Apply continuity correction by adding 0.5 to numerator and 1 to denominator
      data$p_hat <- (data$Resistant + 0.5) / (data$InterpretableAST + 1)
      
      # Calculate the variance of the resistance rate for each country
      data$var_p_hat <- data$p_hat * (1 - data$p_hat) / (data$InterpretableAST + 1)
      
      # Calculate the inverse variance weights for each country; also weighting for country population number
      # Divide by country pop by arbitrary cor = large number to avoid having to work with large numbers
      if (pop == "yes") {
        data$w <- data$TotalPopulation / cor / data$var_p_hat
      } else {
        data$w <- 1 / data$var_p_hat
      }
      
      data$w_norm <- data$w / sum(data$w)
      
      # Calculate the weighted average resistance rate
      p_IVW <- if (normalise_w == "no") {
        sum(data$w * data$p_hat) / sum(data$w)
      } else {
        sum(data$w_norm * data$p_hat)
      }
      
      # Calculate the variance of the combined resistance rate
      var_p_IVW <- if (normalise_w == "no") {
        1 / sum(data$w)
      } else {
        1 / sum(data$w_norm / data$var_p_hat)
      }
      
      # Calculate the standard error of the combined resistance rate
      SE_IVW <- sqrt(var_p_IVW)
      
      # Calculate the 95% confidence interval
      z_score <- 1.96
      CI_lower <- p_IVW - z_score * SE_IVW
      CI_upper <- p_IVW + z_score * SE_IVW
      
      # Calculate the weighted median and IQR
      ########################################################
      # Sort the data by the estimated resistance rates
      # data <- data[order(data$p_hat), ]
      # 
      # # Calculate cumulative weights
      # if (normalise_w == "no") {
      #   data$cum_w <- cumsum(data$w)
      #   total_w <- sum(data$w)
      # } else {
      #   data$cum_w <- cumsum(data$w_norm)
      #   total_w <- sum(data$w_norm)
      # }
      # 
      # # Calculate the weighted median
      # median_index <- which(data$cum_w >= total_w / 2)[1]
      # weighted_median <- data$p_hat[median_index]
      # 
      # # Calculate the 25th percentile
      # percentile_25_index <- which(data$cum_w <= 0.25 * total_w)[1]
      # weighted_percentile_25 <- data$p_hat[percentile_25_index]
      # 
      # # Calculate the 75th percentile
      # percentile_75_index <- which(data$cum_w >= 0.75 * total_w)[1]
      # weighted_percentile_75 <- data$p_hat[percentile_75_index]
      # 
      # Store results
      Year <- year
      Specimen <- s
      PathogenName <- pathogen
      AbTargets <- a
      r <- data.frame(Year, Specimen, PathogenName, AbTargets, p_IVW, SE_IVW, CI_lower, CI_upper)
      
      # r <- data.frame(Year, Specimen, PathogenName, AbTargets, p_IVW, SE_IVW, CI_lower, CI_upper,
      #                 med_IVW = weighted_median, Q1_IVW = weighted_percentile_25,
      #                 Q3_IVW = weighted_percentile_75)
      ra <- rbind(ra, r)
      
      print(a)
    }
    
    rates <- rbind(rates, ra)
    print(s)
  }
  
  # Round the results
  rates[, c("p_IVW", "SE_IVW", "CI_lower", "CI_upper")] <- round(rates[, c("p_IVW", "SE_IVW", "CI_lower", "CI_upper")], 4)
  rates[, c("p_IVW", "CI_lower", "CI_upper")] <- rates[, c("p_IVW", "CI_lower", "CI_upper")] * 100
  
  return(data.frame(rates))
}


  
# Calculate the weighted median and IQR
  ########################################################
  # Sort the data by the estimated resistance rates
  # data <- data[order(data$p_hat), ]
  # 
  # # Calculate cumulative weights
  # # To find the weighted median, we need to determine the point where the cumulative weight reaches
  # # or exceeds 50% of the total weight. This is where the weighted median lies.
  # data$cum_w <- cumsum(data$w)
  # 
  # # Calculate total weight
  # total_w <- sum(data$w)
  # 
  # # Calculate the weighted median; total_weight / 2 finds the halfway point of the total weight.
  # median_index <- which(data$cum_w >= total_w / 2)[1]
  # weighted_median <- data$p_hat[median_index]
  # 
  # # Calculate the 25th percentile
  # percentile_25_index <- which(data$cum_w <= 0.25 * total_w)[1]
  # weighted_percentile_25 <- data$p_hat[percentile_25_index]
  # 
  # # Calculate the 75th percentile
  # percentile_75_index <- which(data$cum_w >= 0.75 * total_w)[1]
  # weighted_percentile_75 <- data$p_hat[percentile_75_index]


  # Display the results
  #print(data)
  #cat("Weighted Median:", weighted_median, "\n")
  #cat("Weighted 25th-75th Percentile: [", weighted_percentile_25, ", ", weighted_percentile_75, "]\n")
#}


# Use Bayesian regression for estimating AMR rates
################################################################
      
# Define the function to fit the model
fit_amr_model <- function(data_subset, formula, priors) {
  # Check if the data_subset is not empty
  if (nrow(data_subset) == 0) {
    warning("The data subset is empty. Model fitting cannot proceed.")
    return(NULL)
  }
  
  # Fit the model using brms
  fit <- brm(formula, 
             data = data_subset, 
             family = binomial(), 
             prior = priors, 
             chains = 4, 
             cores = 4, 
             iter = 2000,
             control = list(adapt_delta = 0.95))  # Optional control settings
  
  # Extract posterior samples for country-specific effects using as_draws_matrix
  post_samples <- as_draws_matrix(fit)
  
  # Extract the overall intercept
  intercept_samples <- post_samples[, "b_Intercept"]
  
  # Extract country-specific random effects
  country_effects <- post_samples[, grep("^r_Iso3\\[", colnames(post_samples))]
  
  # Calculate the log-odds for each country by adding the overall intercept to the random effects
  log_odds <- sweep(as.matrix(country_effects), 1, intercept_samples, FUN = "+")
  
  # Transform log-odds to probabilities
  resistance_rates <- plogis(log_odds)
  
  # Summarize the resistance rates
  resistance_rates_summary <- apply(resistance_rates, 2, quantile, probs = c(0.025, 0.5, 0.975))
  
  # Convert the summary to a dataframe
  resistance_rates_df <- as.data.frame(t(resistance_rates_summary))
  colnames(resistance_rates_df) <- c("2.5%", "50%", "97.5%")
  resistance_rates_df$Country <- rownames(resistance_rates_df)
  resistance_rates_df$Specimen <- unique(data_subset$Specimen)
  resistance_rates_df$PathogenName <- unique(data_subset$PathogenName)
  resistance_rates_df$AntibioticName <- unique(data_subset$AntibioticName)
  
  # Return the model fit and the resistance rates summary as a dataframe
  return(list(fit = fit, summary = resistance_rates_df))
}
