#################################################
# GLASS REPORT - ESTIMATE AMR RATES
#################################################

# Author Esther van Kleef
# Date created: 12 August 2024
# Date last updated: 12 August 2024

rm(list=ls())
# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet,
               PHEindicatormethods, RColorBrewer, wesanderson, ggforce,ggh4x,
               brms, spdep,doParallel, foreach)

#install.packages("rnaturalearth")
library("rnaturalearth")

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables"
dirOutputModel = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Model_output"

dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/2021/"

# Load in functions
source("./GLASS_functions.R")
source("./multiplot.R")

##############################################################
# LOAD IN DATA
##############################################################
# Population data
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")           # Population data
pdataDM = read.csv(paste0(dirDataNew, "/EI_PopdtaDM_180724_EV.csv"), sep=",")       # Population data by age and sex

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")       # Country data

# Surveillance indicator data
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",") 

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/EI_HAQIdta_080724_EV.csv"), sep=",")

# AMR data
#adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_pop_180724_EV.csv"), sep=",")   # Country AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_pop_ABX_adapted_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_pop_ABX_adapted_180724_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read_excel(paste0(dirDataNew, "/updated_summary_dbc_longformat.xlsx"), sheet=1)

# rrates 2021
rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
  Q1 = as.numeric(Q1),
  Q3 = as.numeric(Q3),
  median = as.numeric(median)
)

# Drug bug combinations to include in report
combinations2022 = dbdata %>% filter(Period %in% c("2016-","2016-2022")) %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

###################################################################
# PREAMBLE
###################################################################

# Take out all the lines which specify the origin of the sample (as we don't have denominator population data for those)
# Add InReport to specify if drug-bug combination should be reported on
adataNT$AntibioticName <- sub(" $", "", adataNT$AntibioticName)

adataAS = adataNT %>% filter(!is.na(TotalPopulation)) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No"),
    amr_rate = Resistant/InterpretableAST, 
    # The above rates are based on very small numbers
    # Would be better to calculate prevalence by age globally
    BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  ) %>% 
  filter(Iso3 != "XKX") # Remove Kosovo

adataAC = adataAC %>% filter(Iso3 != "XKX")

##########################################################
# MODEL SET UP
##########################################################

# Understanding the Parameters:
# -------------------------------------------------------------
# NUTS (No-U-turn) No-U-Turn Criterion:
# The name "No-U-Turn" comes from the algorithm's mechanism to prevent the sampler
# from making unnecessary U-turns in the parameter space,
# which helps in exploring the posterior more efficiently.

# Diagnostics
# -------------------------------------------------------------
#Rhat < 1.1: If Rhat is below 1.1, it generally suggests that the chains have converged reasonably well. 
# This threshold is often used as a practical criterion to conclude that convergence is likely achieved. 
# However, this is not a strict cutoff and depending on the context, stricter thresholds (e.g., <1.05) might be used for higher confidence 
# in convergence.

#Rhat ≥ 1.1: If Rhat is 1.1 or above, it indicates that there may be convergence issues. 
# The chains might not have mixed well or explored the posterior distribution adequately. 
# Further investigation and potentially more iterations might be required.

# Improve convergence
# ---------------------------------------------------------------
# adapt_delta: Controls the target acceptance rate of the NUTS algorithm. 
# The default value is 0.8, but increasing it (e.g., to 0.99) 
# can help with convergence, particularly in models with complex posterior distributions. 
# A higher adapt_delta might lead to longer sampling times but can improve accuracy.
# max_treedepth: Limits the depth of the trees used by the HMC algorithm. 
# If the sampler needs to build deeper trees, increasing this value can help, 
# but it may also increase computation time. The default is 10, but increasing it (e.g., to 15) 
# can sometimes resolve convergence issues.

# For now decided to not specify adapt_delta

# Priority pathogens (First fit only to list of priority drugs and bugs of last year)

# BLOOD
# ---------------------------------------
# Acinetobacter - Imipenem, Meropenem, Doripenem
# E. coli - Ceftazidime, Cefotaxime, Cefepime, Ceftriaxone
# K. pneumoniae - Imipenem, Meropenem, Ertapenem, Doripenem
# S. aureus - Methicillin-resistance

# GASTRO
# ---------------------------------------
# Salmonella - ciprofloxacin, levofloxacin

# URINE
# ---------------------------------------
# E. coli - co-trimoxazole

# Unique drug-bug combinations

# PRIORITY
# ----------------------------------------
priority_drug_bug_combinations <- combinations2022 %>% filter(priority_yn_2022_report=="Yes" & data_yn == "Yes")
which(priority_drug_bug_combinations$combined%in%adataAS$combined) # All 12 can be identified

# To try, start with just drug-bug combination
priority_drug_bug_combinations = priority_drug_bug_combinations[c(1,6,9,10,12),]



######################################################################
# FIT THE MODEL
######################################################################

# Define weakly informative priors 
common_priors <- c(
  prior(normal(0, 5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b"),           # Fixed effects
  prior(normal(0, 1), class = "sd")             # Random effects standard deviation
)

car_priors <- prior(
  normal(0, 1),  # Example prior for CAR precision if needed
  class = "sdcar"
)

# Define your model formulas
model0_formula = bf(Resistant | trials(InterpretableAST) ~ 1 + (1 | Iso3) + Year + AgeCat10 + Sex)
model1_formula = bf(Resistant | trials(InterpretableAST) ~ 1 + (1 | Iso3) + Year + AgeCat10 + Sex + AgeCat10*Sex)
model2_formula = bf(Resistant | trials(InterpretableAST) ~ 1 + Year + AgeCat10 + Sex + car(adj_matrix, gr=Iso3, type = "icar"))

# With random-effects for age 
#model2_formula = bf(Resistant | trials(InterpretableAST) ~  1 + Year + (1 + AgeCat10 | Iso3) + Sex) 


# Define the log file path
log_file <- "model_fitting_log.txt"

# Open a connection to the log file
log_con <- file(log_file, open = "wt")

# List to store model fits
model_fits <- list()
convergence_info <- list()

# Set seed
set.seed(12)

# Loop through each drug-bug combination
for (drug_bug in priority_drug_bug_combinations$combined) {
  
  # Log the start of processing
  writeLines(paste("Processing drug-bug combination:", drug_bug), log_con)
  
  # Subset the data for the current drug-bug combination
  subset_data <- subset(adataAS, combined == drug_bug)
  subset_data <- subset_data %>% filter(!Year == 2016)
  
  # Create covariance matrix from weights list
  # Get world map data once
  world_data <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create the combined adjacency matrix for all years
  combined_adj_matrix <- create_combined_adj_matrix(subset_data, world_data)
  
  # Expand the adjacency matrix for the dataset
  expanded_matrix <- expand_adj_matrix(combined_adj_matrix, subset_data)
  
  
  # Initialize a list to store model diagnostics
  model_diagnostics <- list()
  
  # Fit Model 0
  # -------------------------------------------------
  
  # Record start time
  start_time <- Sys.time()
  print(paste("Start Time of Model 0 for", drug_bug,":", start_time))
  
  model0 <- brm(
    model0_formula,
    data = subset_data,
    family = binomial(),  
    #prior = common_priors,
    iter = 3000,   
    warmup = 1000, 
    chains = 4,    
    cores = 4      
  )
  model_diagnostics$model0_converged <- summary(model0)$fixed$Rhat < 1.1  # Threshold can be adjusted
  
  if(length(which(model_diagnostics$model0_converged == FALSE)) == 0){
    print(paste("Model 0 for", drug_bug,"converged!")) 
  }else{ 
    print(paste("Model 0 for", drug_bug,"did not converge for", length(which(model_diagnostics$model0_converged == FALSE)), "parameters"))
  }
  
  # Record end time
  end_time <- Sys.time()
  
  # Calculate elapsed time
  elapsed_time <- end_time - start_time
  print(paste("Computational Time of Model 0 for",drug_bug,":"))
  print(elapsed_time)
  
  # Fit Model 1
  # -------------------------------------------------
  # Record start time
  start_time <- Sys.time()
  print(paste("Start Time of Model 1 for",drug_bug,":", start_time))
  
  model1 <- brm(
    model1_formula,
    data = subset_data,
    family = binomial(),  
    #prior = common_priors,
    iter = 3000,   
    warmup = 1000, 
    chains = 4,    
    cores = 4     
  )
  model_diagnostics$model1_converged <- summary(model1)$fixed$Rhat < 1.1  # Threshold can be adjusted
  
  if(length(which(model_diagnostics$model1_converged == FALSE)) == 0){
    print(paste("Model 1 for", drug_bug,"converged!")) 
  }else{ 
    print(paste("Model 1 for", drug_bug,"did not converge for", length(which(model_diagnostics$model1_converged == FALSE)), "parameters"))
  }
  
  # Record end time
  end_time <- Sys.time()
  
  # Calculate elapsed time
  elapsed_time <- end_time - start_time
  print(paste("Computational Time of Model 1 for",drug_bug,":"))
  print(elapsed_time)
  
  # Fit Model 2
  # -------------------------------------------------
  
  # Record start time
  start_time <- Sys.time()
  print(paste("Start Time of Model 2 for",drug_bug,":", start_time))
  
  # Fit Model 2
  model2 <- brm(
    model2_formula,
    data = subset_data,
    family = binomial(),  
    #prior = common_priors[1:2,]+car_priors,
    iter = 4000,   
    warmup = 1000, 
    chains = 4,    
    cores = 4,     
    data2 = list(adj_matrix = expanded_matrix)
  )
  model_diagnostics$model2_converged <- summary(model2)$fixed$Rhat < 1.1  # Threshold can be adjusted
  
  if(length(which(model_diagnostics$model2_converged == FALSE)) == 0){
    print(paste("Model 2 for", drug_bug,"converged!")) 
  }else{ 
    print(paste("Model 2 for", drug_bug,"did not converge for", length(which(model_diagnostics$model2_converged == FALSE)), "parameters"))
  }
  
  # Record end time
  end_time <- Sys.time()
  
  # Calculate elapsed time
  elapsed_time <- end_time - start_time
  print(paste("Computational Time of Model 2 for",drug_bug,":"))
  print(elapsed_time)
  
  # # Fit Model 3
  # model3 <- brm(
  #   model3_formula,
  #   data = subset_data,
  #   family = binomial(),  
  #   iter = 4000,   
  #   warmup = 1000, 
  #   chains = 4,    
  #   cores = 4      
  # )
  # model_diagnostics$model3_converged <- summary(model3)$fixed$Rhat < 1.1  # Threshold can be adjusted
  # print(model_diagnostics$model3_converged)
  # 
  # 
  
  # Store the fitted models
  model_fits[[drug_bug]] <- list(
    model0 = model0,
    model1 = model1,
    model2 = model2
    #model3 = model3
  )
  
  convergence_info[[drug_bug]] <- model_diagnostics
  
  # Log the completion of processing
  writeLines(paste("Completed processing for drug-bug combination:", drug_bug), log_con)
}

# Log the completion of all processing
writeLines("All drug-bug combinations have been processed.", log_con)

# Close the connection to the log file
close(log_con)

print(convergence_info)

# Save the results to an RDS file
saveRDS(model_fits, file = paste0(dirOutputModel,"/model_fits_AcinetobacterBLOOD-Imipenem_defaultpriors.rds"))


###########################################################################################
# CHECK MODEL FIT
###########################################################################################

# Initialize list for model comparison
comparison_results <- list()

# Compare models for each drug-bug combination
for (drug_bug in unique(priority_drug_bug_combinations$combined)) {
  models <- model_fits[[drug_bug]]
  
  # Extract models
  model0 <- models$model0
  model1 <- models$model1
  model2 <- models$model2
  
  # Compute WAIC for model comparison
  waic0 <- waic(model0)
  waic1 <- waic(model1)
  waic2 <- waic(model2)
  
  # Store WAIC results
  comparison_results[[drug_bug]] <- list(
    WAIC_model0 = waic0,
    WAIC_model1 = waic1, #,
    WAIC_model2 = waic2
  )
}

# View comparison results
comparison_results

# Using Loo
# Compare models for each drug-bug combination
for (drug_bug in unique(priority_drug_bug_combinations$combined)) {
  models <- model_fits[[drug_bug]]
  
  # Extract models
  model0 <- models$model0
  model1 <- models$model1
  model2 <- models$model2
  
  # Compute LOO for model comparison
  loo0 <- loo(model0)
  loo1 <- loo(model1)
  loo2 <- loo(model2)
  
  # Store LOO results
  comparison_results[[drug_bug]] <- list(
    LOO_model0 = loo0,
    LOO_model1 = loo1,
    LOO_model2 = loo2
  )
}

# View comparison results
comparison_results

plot(model_fits[[1]]$model0)


# Fit the model using brms
# Fit with default priors
# fit <- brm(formula, 
#            data = data_sets[[6]], 
#            family = binomial(), 
#            chains = 4, 
#            cores = 4,
#            iter = 3000,
#            warmup = 1000
#            #,
#            # control = list(
#            #   adapt_delta = 0.99,  # Higher value for better convergence (default is 0.8)
#            #   max_treedepth = 15   # Maximum depth of the trees (default is 10)
#            # )
# )
# 
# summary(fit)
# plot(fit)
# 
# # Get standardized rates
# popdata = pdataDM %>% filter(Iso3 %in% unique(data_sets[[6]]$Iso3))
# 
# # Get posterior predictions
# posterior_predictions <- get_posterior_predictions(fit, data_sets[[6]])
# 
# 
# # Then update this fit using new data, so model does not need to recompile for each drug bug
# model_fits <- lapply(data_sets, function(x) update(fit, newdata = x,
#                                                    chains=6, cores=6))
# 
# # Extract summaries from each model fit
# model_summaries <- lapply(model_fits, function(x) get_fit_model(model_fit=x))

# Plot the posterior means of the intercepts
#################################################################

# Extract the summary of the first model for plotting
# model_estimates = NULL
# 
# for(i in 1:length(data_sets)){
#   d = ecolidata # Change to type of bug-drug combinations to look at
#   model_summaries[[i]]$summary$Iso3 <- sub("r_Iso3\\[(.*),Intercept\\]", "\\1", model_summaries[[i]]$summary$Country)
#   model_summaries[[i]]$summary$Specimen <- d$Specimen[i]
#   model_summaries[[i]]$summary$PathogenName <- d$PathogenName[i]
#   model_summaries[[i]]$summary$AntibioticName <- d$AntibioticName[i]
#   
#   model_summaries[[i]]$summary <- left_join(model_summaries[[i]]$summary,cdata %>% select(Iso3, WHORegionCode, WHORegionName))
#   
#   # Create overall and regional values
#   total = model_summaries[[i]]$summary %>%
#     summarise(
#       med50 = quantile(med50, probs = 0.5),
#       low25 = quantile(low25, probs = 0.25),
#       high75 = quantile(high75, probs = 0.75),
#       Specimen = unique(Specimen),
#       PathogenName = unique(PathogenName),
#       AntibioticName = unique(AntibioticName),
#       CTA_total = length(unique(Iso3)),
#       Total = "Yes"
#     )
#   #print(total)
#   data_total = d %>% filter(combined == d$combined[i]) %>%
#     summarise(
#       rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
#       rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
#       rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
#     )
#   
#   total = cbind(total,data_total)
#   
#   region = model_summaries[[i]]$summary %>% group_by(WHORegionCode) %>%
#     summarise(
#       med50 = quantile(med50, probs = 0.5),
#       low25 = quantile(low25, probs = 0.5),
#       high75 = quantile(high75, probs = 0.5),
#       Specimen= unique(Specimen),
#       PathogenName = unique(PathogenName),
#       AntibioticName = unique(AntibioticName),
#       CTA_total = length(unique(Iso3)),
#       Total = "No"
#     )
#   
#   data_region = d %>% filter(combined == d$combined[i]) %>% 
#     group_by(WHORegionCode) %>%
#     summarise(
#       rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
#       rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
#       rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
#     )
#   region = left_join(region, data_region)
#   total$WHORegionCode = "1.ALL"
#   #total = total[,names(total.region)]
#   total.region = rbind(total,region)
#   model_summaries[[i]]$summary.overall = total.region
#   model_estimates = rbind(model_estimates,total.region)
# }

###############################################################
# ESTIMATES BASED ON inverse variance weights
###############################################################
pn = unique(adataAC$PathogenName)

rrates_norm <- NULL

# Loop through each pathogen in pn - normalised
for (p in pn) {
  pathogen <- p
  r <- ivw(adataAC, dbdata, year = c("2021"), pathogen =pathogen, cor = 1000000,
           pop = "yes", normalise_w = "yes")
  rrates_norm <- rbind(rrates_norm, r)  # Append the results to rrates
}

rrates <- NULL
# Loop through each pathogen in pn - not normalised
for (p in pn) {
  pathogen <- p
  r <- ivw(adataAC, dbdata, year = c("2021"), pathogen =pathogen, cor = 1000000,
           pop = "no", normalise_w = "yes")
  rrates <- rbind(rrates, r)  # Append the results to rrates
}

# Convert rrates to a data frame - normalised
rrates_norm <- data.frame(rrates_norm)
rrates_norm$Year = as.character(rrates_norm$Year)
rrates_norm$w_normalised = "yes"
rrates_norm$AbTargets[rrates_norm$AbTargets=="Cefotaxime "] = "Cefotaxime"

# Convert rrates to a data frame - not normalised
rrates <- data.frame(rrates)
rrates$w_normalised = "no"
rrates$Year = as.character(rrates$Year)
rrates$AbTargets[rrates$AbTargets=="Cefotaxime "] = "Cefotaxime"

# Link normalised and not normalised to compare
#rrates_norm_notnorm <- rbind(rrates,rrates_norm)
rrates_norm_notnorm <- full_join(rrates,rrates_norm, by=c("Specimen", "PathogenName", "AbTargets","Year"))
rrates_norm_notnorm$drugbug = c(1:length(rrates_norm_notnorm$Year))
# Change to numeric otherwise no linkage possible if not in same format
rrates2021$Year = as.character(rrates2021$Year)


# Link with 2021 data to check - not normalised
rrates_m <- full_join(rrates, rrates2021) %>% filter(Year =="2021")
#rrates <- merge(rrates2021 %>% select(Specimen, PathogenName, AbTargets), rrates)
# Print the final results
print(rrates_m)
rrates_m$drugbug = c(1:length(rrates_m$Year))

####################################################################
# PLOT ESTIMATES
####################################################################

# Plot not normalised weights vs 2021 median and IQR last year
p1 = ggplot(rrates_m%>%filter(Specimen=="BLOOD"), aes(x = AbTargets, y = p_IVW, group=drugbug)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.1, size=2,width=0.5) +
  geom_point(aes(x=AbTargets, y=median), size = 3, col="red") +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), alpha = 0.1, size=2,width=0.5, col="red") +
  labs(title = "Resistance Rate with 95% Confidence Intervals",
       x = "Antibiotic Target",
       y = "Resistance Rate") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  facet_wrap(.~ PathogenName , scales = "free_y",ncol=2) + 
  ggtitle("Inverse variance weighted mean [[95%CI] (black) vs 75% median [IQR] (red)")

# Plot not normalised weights vs normalised weights
p2 = ggplot(rrates_norm_notnorm%>%filter(Specimen=="BLOOD"), aes(x = AbTargets, y = p_IVW.x, group=drugbug)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower.x, ymax = CI_upper.x), alpha = 0.1, size=2,width=0.5) +
  geom_point(aes(x=AbTargets, y= p_IVW.y), size = 3, col="red") +
  geom_errorbar(aes(ymin = CI_lower.y, ymax = CI_upper.y), alpha = 0.1, size=2,width=0.5, col="red") +
  labs(title = "Resistance Rate with 95% Confidence Intervals",
       x = "Antibiotic Target",
       y = "Resistance Rate") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  facet_wrap(.~ PathogenName , scales = "free_y",ncol=2) + 
  ggtitle("Inverse variance weighted mean [95%CI] - not normalised (black) vs normalised (red)")

# Plot figures
pdf(file = paste0(dirOutput, "./Analyses/pop_ivw_rrates_vs_75med - BLOOD.pdf"), width=10,height=12)
print(p1)
dev.off()

# Plot figures
pdf(file = paste0(dirOutput, "./Analyses/pop_ivw_rrates_norm_vs_notnorm - BLOOD.pdf"), width=10,height=12)
print(p2)
dev.off()

write.csv(rrates_norm,file=paste0(dirOutput, "/Analyses/pop_ivw_rates_normalised.csv"))
write.csv(rrates,file=paste0(dirOutput, "/Analyses/pop_ivw_rates.csv"))
