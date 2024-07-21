######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################
library(dplyr)

# Plot raw AMR rates - by drug-bug combination
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


# Use Bayesian regression for estimating AMR rates
################################################################

# Function to identify all unique levels for each factor to generate a list of dataframes
# that ensures that all levels are present in each of the different datasets for each
# of the drug bug combinations. This as otherwise brm_multiple does not work
# Hence the model does not need to compile each time.

get_unique_levels <- function(datasets) {
  combined_levels <- list()
  
  for (df in datasets) {
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        combined_levels[[col]] <- union(combined_levels[[col]], unique(df[[col]]))
      }
    }
  }
  
  return(combined_levels)
}

# Function to apply unique levels to each factor column
apply_unique_levels <- function(df, combined_levels) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- factor(df[[col]], levels = combined_levels[[col]])
    }
  }
  return(df)
}


# Define the function to fit the model
get_fit_model <- function(model_fit) {
  
  # Extract posterior samples for country-specific effects using as_draws_matrix
  post_samples <- as_draws_matrix(model_fit)
  
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
  colnames(resistance_rates_df) <- c("low2.5", "med50", "high97.5")
  resistance_rates_df$Country <- rownames(resistance_rates_df)
  
  # Return the model fit and the resistance rates summary as a dataframe
  return(list(fit = fit, summary = resistance_rates_df))
}

# Plot Model AMR rates by drug bug combination
################################################################################################
plot_model_AMRdb_region <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50, col=WHORegionCode)) +
    geom_errorbar(aes(ymin = low2.5, ymax = high97.5), alpha=0.2, size=2, width = 0.5) +  # Error bars
    geom_point(size = 4) +  # Points
    #geom_point(aes(x=AntibioticName, y=median/100), size = 4, col="red") +
    #geom_errorbar(aes(ymin = Q1/100, ymax = Q3/100), alpha = 0.2, size=2,width=0.5, col="red") +
    scale_color_manual(values = brewer.pal(7, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates: ", model_estimates$PathogenName, " - ",
                     model_estimates$Specimen),
      x = " ",
      y = "Resistance % - Median (95% Credible Interval)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),  # X-axis label color
      axis.title = element_text(size = 12, color = "black"),  # Axis title styling
      plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
      axis.text = element_text(size = 10),  # Axis text size
      panel.grid.major = element_line(color = "grey80"),  # Major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = "black")  # Background styling
    ) +
    facet_wrap(. ~ WHORegionCode, ncol=7) + 
    coord_flip()
}


# Plot AMR rates by Specimen and pathogen
#######################################################################
plot_model_AMRdb <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50, col = Total)) +
  geom_errorbar(aes(ymin = low2.5, ymax = high97.5), alpha=0.2, width = 0.2, size=2) +  # Error bars
  geom_point(size = 4) +  # Points
  scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
  labs(
    title = paste0("Model estimates: ", model_estimates$PathogenName," - ", model_estimates$Specimen),
    x = " ",
    y = "Resistance % \n Median (95% Credible Interval)"
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    axis.text.x = element_text(color = "black"),  # X-axis label color
    axis.title = element_text(size = 12, color = "black"),  # Axis title styling
    plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
    axis.text = element_text(size = 10),  # Axis text size
    panel.grid.major = element_line(color = "grey80"),  # Major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = "black")  # Background styling
  ) +
  facet_wrap(.~Specimen, ncol=2) + 
  coord_flip()
}

plot_model_AMRdb_withdata <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50)) +
    geom_errorbar(aes(ymin = low2.5, ymax = high97.5), alpha=0.2, size=2, width = 0.5, col="red") +  # Error bars
    geom_point(size = 4, col="red") +  # Points
    geom_point(aes(x=AntibioticName, y=median/100), size = 4, col="black") +
    geom_errorbar(aes(ymin = Q1/100, ymax = Q3/100), alpha = 0.2, size=2,width=0.5, col="black") +
    #scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates (All countries): ", model_estimates$PathogenName, 
                     " (Estimates = red, Data (75th percentile) = black)"),
      x = " ",
      y = "Resistance % - Median (95% Credible Interval)"
    ) +
    theme_minimal() +
    theme(
      legend.position="none",
      axis.text.x = element_text(color = "black"),  # X-axis label color
      axis.title = element_text(size = 12, color = "black"),  # Axis title styling
      plot.title = element_text(size = 14, face = "bold", color = "darkblue"),  # Plot title styling
      axis.text = element_text(size = 10),  # Axis text size
      panel.grid.major = element_line(color = "grey80"),  # Major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_rect(fill = "white", color = "black")  # Background styling
    ) +
    facet_wrap(. ~ Specimen, ncol=2) + 
    coord_flip()
}
