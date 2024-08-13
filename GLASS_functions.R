######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################
library(dplyr)

# Plot sum of isolates by region
plot_isolates_db_asRegion <- function(data, year, pathogen, specimen, in_report, xlim_max = 1000, ncol_facet = 4, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year == year, PathogenName == pathogen, Specimen == specimen, InReport == in_report)
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = s_interpretableAST, y = DemographicsOrigin, col = WHORegionCode)) +
    geom_point(size = 2) +
    facet_wrap(. ~ AntibioticName, scales = "free_x", ncol = ncol_facet) +
    theme_minimal() + 
    geom_vline(xintercept = 100, col = "red", linetype = 2) + 
    geom_vline(xintercept = 50, col = "seagreen", linetype = 2) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    scale_color_manual(values = palette) +
    xlim(0, xlim_max) + 
    labs(
      title = "Number of isolates with interpretable AST - by region, age & sex",
      subtitle = paste0(pathogen, " - ", specimen), 
      x = "Number of Isolates",
      y = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}

plot_isolates_db_as <- function(data, year, specimen, in_report, exclude_antibiotic = NULL, ncol_facet = 3, xlim_max = 1000) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year == year, InReport == in_report, Specimen %in% specimen) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotic), character(0), exclude_antibiotic))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = s_interpretableAST, y = DemographicsOrigin, col = AntibioticName)) +
    geom_point(size = 3) +
    theme_minimal() +
    facet_wrap(. ~ PathogenName, ncol = ncol_facet, scales = "free") +
    geom_vline(xintercept = 100, col = "red", linetype = 2) + 
    geom_vline(xintercept = 50, col = "seagreen", linetype = 2) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    labs(
      title = "Number of isolates with interpretable AST - by age & sex",
      subtitle = specimen, 
      x = "Number of Isolates",
      y = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}

# plot country-level isolates by age and sex per region - boxplot
plot_isolatesdb_as_regionCOUNTRY <- function(data, year, pathogen, specimen, in_report, exclude_antibiotics = NULL, 
                                             ylim_max, facet_colors, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year %in% year, PathogenName == pathogen, Specimen == specimen, InReport == in_report) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotics), character(0), exclude_antibiotics))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(y = InterpretableAST, x = AgeCat10, fill = Sex)) +
    geom_jitter(aes(alpha = 0.6, col = Sex)) +
    geom_boxplot(na.rm = TRUE) +
    facet_grid2(AntibioticName ~ WHORegionCode, scales = "free_y",
                strip = strip_themed(
                  background_x = list(
                    element_rect(fill = facet_colors["AFR"]),
                    element_rect(fill = facet_colors["AMR"]),
                    element_rect(fill = facet_colors["EMR"]),
                    element_rect(fill = facet_colors["EUR"]),
                    element_rect(fill = facet_colors["SEA"]),
                    element_rect(fill = facet_colors["WPR"])
                  )
                )) +
    theme_minimal() +
    ylim(0, ylim_max) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    scale_fill_manual(values = palette) +
    scale_alpha(guide = "none") + 
    labs(
      title = "Number of isolates with interpretable AST - by region, age & sex",
      subtitle = paste0(pathogen, " - ", specimen), 
      y = "Number of isolates",
      x = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}


# plot raw AMR rates by age and sex per region
plot_AMRdb_as_region <- function(data, year, pathogen, specimen, in_report, exclude_antibiotics = NULL, facet_colors, palette = palette5) {
  # Filter the data based on the provided parameters
  filtered_data <- data %>%
    filter(Year %in% year, PathogenName == pathogen, Specimen == specimen, InReport == in_report) %>%
    filter(!AntibioticName %in% ifelse(is.null(exclude_antibiotics), character(0), exclude_antibiotics))
  
  # Create the plot
  p <- ggplot(filtered_data, aes(y = amr_rate, x = AgeCat10, fill = Sex)) +
    geom_jitter(aes(alpha = 0.6, col = Sex)) +
    geom_boxplot(na.rm = TRUE) +
    geom_smooth(aes(group = Sex, color = Sex), method = "loess", se = T, linetype = "solid") +
    facet_grid2(AntibioticName ~ WHORegionCode,
                strip = strip_themed(
                  background_x = list(
                    element_rect(fill = facet_colors["AFR"]),
                    element_rect(fill = facet_colors["AMR"]),
                    element_rect(fill = facet_colors["EMR"]),
                    element_rect(fill = facet_colors["EUR"]),
                    element_rect(fill = facet_colors["SEA"]),
                    element_rect(fill = facet_colors["WPR"])
                  )
                )) +
    theme_minimal() +
    ylim(0,1) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    scale_fill_manual(values = palette) +
    scale_alpha(guide = "none") + 
    labs(
      title = "Proportion of resistant isolates - by region, age & sex",
      subtitle = paste0(pathogen, " - ", specimen), 
      y = "AMR prevalence",
      x = "Age group (10 year bands)"
    )
  
  # Return the plot
  return(p)
}


# Plot map of raw AMR rates - by drug-bug combination
######################################################
plot_amr_map <- function(shapefile, amr_data, year,
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
      title = paste("AMR Rates for", specimen, "-", year),
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
# 
# get_unique_levels <- function(datasets) {
#   combined_levels <- list()
#   
#   for (df in datasets) {
#     for (col in names(df)) {
#       if (is.character(df[[col]])) {
#         combined_levels[[col]] <- union(combined_levels[[col]], unique(df[[col]]))
#       }
#     }
#   }
#   
#   return(combined_levels)
# }
# 
# # Function to apply unique levels to each factor column
# apply_unique_levels <- function(df, combined_levels) {
#   for (col in names(df)) {
#     if (is.character(df[[col]])) {
#       df[[col]] <- factor(df[[col]], levels = combined_levels[[col]])
#     }
#   }
#   return(df)
# }


# Function to create adjacency matrix for all years
# Counts 1 when countries are neighbours, 0 otherwise
create_combined_adj_matrix <- function(data, world_data) {
  # Get unique countries across all years
  all_countries <- unique(data$Iso3)
  # Filter world data
  world <- world_data %>% filter(iso_a3_eh %in% all_countries)
  
  # Convert to spatial polygons
  world_sp <- as(world, "Spatial")
  
  # Compute neighboring countries
  neighbors <- poly2nb(world_sp)
  
  # Create adjacency matrix
  adj_matrix <- nb2mat(neighbors, style = "B", zero.policy = TRUE)
  rownames(adj_matrix) <- colnames(adj_matrix) <- world$iso_a3_eh
  
  return(adj_matrix)
}

# Function to expand adjacency matrix to match the dataset structure (i.e. by age- and sex)
expand_adj_matrix <- function(adj_matrix, data) {
  # Extract unique combinations of country, year, age category, and sex
  unique_combinations <- data %>%
    select(Iso3, Year, AgeCat10, Sex) %>%
    distinct() %>%
    arrange(Iso3, Year, AgeCat10, Sex)  # Ensure ordering
  
  # Number of unique combinations
  num_combinations <- nrow(unique_combinations)
  
  # Create an expanded matrix with zeros
  expanded_matrix <- Matrix(0, nrow = num_combinations, ncol = num_combinations, sparse = TRUE)
  colnames(expanded_matrix) = unique_combinations$Iso3
  rownames(expanded_matrix) = unique_combinations$Iso3
  
  # Get the row and column names of the adjacency matrix
  row_names <- rownames(adj_matrix)
  col_names <- colnames(adj_matrix)
  
  # Populate the expanded matrix
  for (i in seq_len(nrow(adj_matrix))) {
    #print(i)
    row_iso3 <- row_names[i]
    neighbors <- which(adj_matrix[i, ] == 1)
    
    for (neighbor in neighbors) {
      col_iso3 <- col_names[neighbor]
      
      # Get the indices in the expanded matrix
      row_indices <- which(unique_combinations$Iso3 == row_iso3)
      col_indices <- which(unique_combinations$Iso3 == col_iso3)
      
      # Set values to 1 for neighbors
      expanded_matrix[row_indices, col_indices] <- 1
    }
  }
  
  return(expanded_matrix)
}


# Define the function to fit the model
# get_fit_model <- function(model_fit) {
#   
#   # Extract posterior samples for country-specific effects using as_draws_matrix
#   post_samples <- as_draws_matrix(model_fit)
#   
#   # Extract the overall intercept
#   intercept_samples <- post_samples[, "b_Intercept"]
#   
#   # Extract country-specific random effects
#   country_effects <- post_samples[, grep("^r_Iso3\\[", colnames(post_samples))]
#   
#   # Calculate the log-odds for each country by adding the overall intercept to the random effects
#   log_odds <- sweep(as.matrix(country_effects), 1, intercept_samples, FUN = "+")
#   
#   # Transform log-odds to probabilities
#   resistance_rates <- plogis(log_odds)
#   
#   # Summarize the resistance rates
#   resistance_rates_summary <- apply(resistance_rates, 2, quantile, probs = c(0.025, 0.5, 0.975))
#   
#   # Convert the summary to a dataframe
#   resistance_rates_df <- as.data.frame(t(resistance_rates_summary))
#   colnames(resistance_rates_df) <- c("low", "med50", "high")
#   resistance_rates_df$Country <- rownames(resistance_rates_df)
#   
#   # Return the model fit and the resistance rates summary as a dataframe
#   return(list(fit = fit, summary = resistance_rates_df))
# }
# 
# # Model with age and sex
# # Extract posterior predictions for each combination of country, age, and sex
# get_posterior_predictions <- function(model_fit, population_data, Year) {
#   
#   # Create a new dataset that includes all combinations of country, age, and sex
#   new_data <- population_data %>%
#     distinct(Iso3, Year, AgeCat10, Sex, InterpretableAST, Resistant, TotalPopulation) 
#   
#   # Get posterior samples for each combination
#   posterior_samples <- posterior_epred(model_fit, newdata = new_data, re_formula = NULL)
#   
#   # Average the predictions over the posterior samples
#   new_data$predicted_median <- apply(posterior_samples, 2, median)
# #  new_data$predicted_low <- apply(posterior_samples, 2, quantile(probs = 0.025))
#   
#   # Standardize predictions using population data
#   new_data = new_data %>%
#   mutate(
#     exp_resistant = predicted_median * TotalPopulation
#   )
#   new_data
#   return(new_data)
# }


# Plot Model AMR rates by drug bug combination
################################################################################################
plot_model_AMRdb_region <- function(model_estimates) {
  ggplot(model_estimates, aes(x = AntibioticName, y = med50*100, col=WHORegionCode)) +
    geom_errorbar(aes(ymin = low25*100, ymax = high75*100), size=2, width = 0.5) +  # Error bars
    geom_point(size = 4) +  # Points
    geom_point(aes(x=AntibioticName, y=rawAMRmed50*100), size = 4, col="black",alpha=0.2, shape=17) +  # Points
    geom_errorbar(aes(ymin = rawAMR25*100, ymax = rawAMR75*100), alpha = 0.2, size=2,width=0.5, col="black",
                  linetype=1) +
    #geom_point(aes(x=AntibioticName, y=median/100), size = 4, col="red") +
    #geom_errorbar(aes(ymin = Q1/100, ymax = Q3/100), alpha = 0.2, size=2,width=0.5, col="red") +
    scale_color_manual(values = brewer.pal(7, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates: ", model_estimates$PathogenName, " - ",
                     model_estimates$Specimen),
      x = " ",
      y = "Resistance % - Median [IQR]"
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
  geom_errorbar(aes(ymin = low25, ymax = high75), alpha=0.2, width = 0.2, size=2) +  # Error bars
  geom_point(size = 4) +  # Points
  scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
  labs(
    title = paste0("Model estimates: ", model_estimates$PathogenName," - ", model_estimates$Specimen),
    x = " ",
    y = "Resistance % \n Median [IQR]"
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
  ggplot(model_estimates, aes(x = AntibioticName, y = med50*100)) +
    geom_point(aes(x=AntibioticName, y=median), size = 4, col="darkblue") +
    geom_errorbar(aes(ymin = Q1, ymax = Q3), alpha = 0.2, size=2,width=0.5, col="darkblue") +
    geom_point(aes(x=AntibioticName, y=rawAMRmed50*100), size = 4, col="black") +  # Points
    geom_errorbar(aes(ymin = rawAMR25*100, ymax = rawAMR75*100), alpha = 0.2, size=2,width=0.5, col="black",
                  linetype=1) +
    geom_errorbar(aes(ymin = low25*100, ymax = high75*100), alpha=0.5, size=2, width = 0.5, col="red") +  # Error bars
    geom_point(size = 4, col="red") +  # Points
    #scale_color_manual(values = brewer.pal(3, "Set1")) +  # Custom color scale
    labs(
      title = paste0("Model estimates (All countries): "),
      subtitle = paste0("Red = Model; \nBlack = Data all; \nBlue = Data 75th perc. countries"),
      x = " ",
      y = "Resistance % - Median [IQR]"
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
