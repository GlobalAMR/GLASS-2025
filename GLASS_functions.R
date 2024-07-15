######################################################
# FUNCTIONS USED IN GLASS REPORT
######################################################
library(dplyr)

# Calculate inverse variance weights to account for uncertainty
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

      