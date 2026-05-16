################################################################
# CALCULATE AGE- AND SEX-WEIGHTED AMR PREVALENCES
################################################################

# Author: Esther van Kleef
# Date last updated: 04 March 2026

# Purpose
# This script takes the selected AMR prevalence models and calculates
# age- and sex-weighted country-, regional-, and global prevalence estimates
# for each drug–bug combination, then summarises temporal trends and slopes.
#
# Key steps
# - Load population, country, AMR, and model-comparison inputs.
# - Relevel AgeCat10 and merge testing cut-off information into `adataAS`.
# - Define plotting colours and helper functions.
# - Read the fitted model objects and best-model table.
# - Predict age- and sex-specific prevalences for each best model.
# - Weight posterior predictions by population to obtain country estimates.
# - Aggregate country estimates to regional and global prevalence using
#   inverse-variance weighting on the logit scale.
# - Identify countries contributing data in 2023 and flag overlap/change
#   between 2018 and 2023.
# - Estimate regional and global slope changes from the fitted models.
# - Create heatmaps and trend plots, and export summary CSV tables.
#
# Main outputs
# - CTA_w_prev_*.csv
# - CTA_w_prev_all.csv
# - Figure_3.12_REGION_prevalence_allyears.csv
# - Figure_3.12_REGION_trends_2023change.csv
# - Figure_4.12_REGION_prevalence_slope.csv
#
# Notes
# - Countries with limited data are filtered using the minimum years / 2023
#   submission rules.
# - Regional estimates are calculated manually on the logit scale with
#   between-country heterogeneity (tau2).
#################################################################

rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, brms, loo, wesanderson, ggplot2, rstan, purrr,
               tidyr, data.table, bayesplot, patchwork, stringr, tidybayes,scales, ggh4x, ggtext)


# Locate directories
dirDataRaw   <- here("Data", "raw")
dirDataClean <- here("Data", "cleaned")
dirOutput    <- here("Output")

# Load in functions
source(here("Scripts", "functions", "GLASS_functions.R"))
source(here("Scripts", "functions", "multiplot.R"))

##############################################################
# LOAD IN DATA
##############################################################

# Population data
pdata = read.csv(paste0(dirDataRaw, "/EI_Popdta_110325_EV.csv"), sep=",")       # Population data
pdataDM = read.csv(paste0(dirDataClean, "/EI_PopdtaDM_140325_EV.csv"), sep=",")       # Population data

# Country data
cdata = read.csv(paste0(dirDataClean, "/EI_Countrydta_AST_140325_EV.csv"), sep=",")   # Country data

# AMR data
adataAC = read.csv(paste0(dirDataClean, "/EI_AMRdtaAC_Pop_country_HAQI_140325_EV.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataClean, "/final_linked_data/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataClean, "/updated_summary_dbc_longformat.csv"))

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))


###################################################################
# DATA WRANGLING
###################################################################

adataAS = adataAS %>%
  mutate(AgeCat10 = relevel(factor(AgeCat10), ref="05<14")
  )

# Adding cut off to the data, so to predict prevalences for higher testing rate
cutoff = read.csv(paste0(dirDataClean, "/final_linked_data/estimated_testing_cutoff.csv"), sep=",")       # Population data
cutoff$combined2 = paste0(cutoff$Specimen,"-",cutoff$PathogenName, "-", cutoff$Grouping)

d = cutoff %>%filter(!combined2 %in% c("BLOOD-Escherichia coli-Carbapenems","BLOOD-Klebsiella pneumoniae-Carbapenems",
                                       "URINE-Escherichia coli-Fluoroquinolones","URINE-Klebsiella pneumoniae-Fluoroquinolones")) %>%
  ungroup()%>%
  group_by(Specimen, PathogenName) %>%
  distinct()

adataAS = left_join(adataAS, d, by=c("Specimen", "PathogenName"))


###################################################################
# COLOUR SPECIFICATION
###################################################################

palette <- wes_palette("Darjeeling1", n = 5)
palette2 <- wes_palette("BottleRocket2", n = 1)
palette3 <- wes_palette("GrandBudapest1", n = 2)[2]
palette4 <- wes_palette("BottleRocket2", n = 2)[2]
palette5 = c(palette3, palette[2],palette2,palette[5], palette[4],palette4)

palette_map = c(palette2, palette[2],palette3)

# Define colors for each WHORegionCode
facet_colors <- c(
  "AFR" = palette5[1],
  "AMR" = palette5[2],
  "EMR" = palette5[3],
  "EUR" = palette5[4],
  "SEA" = palette5[5],
  "WPR" = palette5[6]
)


###################################################################################
# LOAD IN MODEL OUTPUTS
###################################################################################

#----------------------------------------------------------------------------
# GET BEST MODELS PER DRUG BUG
#----------------------------------------------------------------------------
get_best_model <- function(results) {
  
  # Remove models with divergences
  #d <- results %>% filter(divergent == 0)
  d <- results # !! STILL DEFINE HOW TO CHECK FOR DIVERGENCES, HERE OR IN PREVIOUS STEP 
  # Group by drug_bug and calculate numeric values and differences
  d2 <- d %>%
    group_by(drug_bug) %>%
    mutate(
      # Assign numeric values to models
      num_model = case_when(
        model == "model0" ~ 0,
        model == "model1" ~ 1,
        model == "model2" ~ 2,
        model == "model3" ~ 3,
        TRUE ~ NA_real_
      ),
      
      # Find num_model where elpd_diff == 0 for each drug_bug group (or smallest)
      num_model_elpd_zero = first(num_model[elpd_diff == 0], default = NA_real_),
      
      # Calculate the difference for each row
      diff_num_model = num_model - num_model_elpd_zero
    ) %>%
    ungroup() %>%
    mutate(
      # Identify the best model based on looic_significant and diff_num_model
      best_model = ifelse(looic_significant == FALSE & diff_num_model < 0, 1, 0)
    )
  
  # Filter for the best model based on elpd_diff == 0 and best_model == 1
  model_best_wp <- d2 %>% filter(elpd_diff == 0)
  model_best_simple <- d2 %>% filter(best_model == 1)
  
  # Find simpler models
  db_simpler_model <- model_best_simple %>% filter(!duplicated(drug_bug))
  
  # Replace rows in bsi_model_best_wp with simpler models where applicable
  model_best_wp[which(model_best_wp$drug_bug %in% db_simpler_model$drug_bug), ] <- db_simpler_model
  
  # Return the final simplified model set
  return(model_best_wp)
}

# BSI
model_fit_bsi = readRDS(paste0(dirDataModeloutput,"/Model_comparison/model_fits_bsi_wp.rds"))
results_bsi = read.csv(paste0(dirDataModeloutput,"/Model_comparison/compare_results_bsi_wp.csv"))

# UTI
model_fit_uti = readRDS(paste0(dirDataModeloutput,"/Model_comparison/model_fits_uti_wp.rds"))
results_uti = read.csv(paste0(dirDataModeloutput,"/Model_comparison/compare_results_uti_wp.csv"))

# STOOL
model_fit_stool = readRDS(paste0(dirDataModeloutput,"/Model_comparison/model_fits_stool_wp.rds"))
results_stool = read.csv(paste0(dirDataModeloutput,"/Model_comparison/compare_results_stool_wp.csv"))

# GONORRHOEA
model_fit_uro = readRDS(paste0(dirDataModeloutput,"/Model_comparison/model_fits_uro_wp.rds"))
results_uro = read.csv(paste0(dirDataModeloutput,"/Model_comparison/compare_results_uro_wp.csv"))

# Find the best model per drug bug
bsi_model_best_wp <- get_best_model(results_bsi)
uti_model_best_wp <- get_best_model(results_uti)
stool_model_best_wp <- get_best_model(results_stool)
uro_model_best_wp <- get_best_model(results_uro)

# STORE BEST FITTING MODELS FOR HEATMAP
#----------------------------------------------------------------------------

# Using LOOIC_significant as cut-off for simplere model or not. 
# When using elpd_diff_significant, more often simpler models are better fit
bsi_model_best_wp$Specimen = "BLOOD"
uti_model_best_wp$Specimen = "URINE"
stool_model_best_wp$Specimen = "STOOL"
uro_model_best_wp$Specimen = "UROGENITAL"

model_best_wp <- rbind(bsi_model_best_wp, uti_model_best_wp, stool_model_best_wp, uro_model_best_wp)
model_best_wp$combined = paste0(model_best_wp$Specimen,"-", model_best_wp$drug_bug)

write.csv(model_best_wp,file.path(dirOutput, "/AMR_prevalence/weakip/Model_comparison/best_model_fit_all.csv"))
#model_best_wp = read.csv(file.path(dirOutput, "/AMR_prevalence/weakip/Model_comparison/best_model_fit_all.csv))

# MAKE EXPLORATORY PLOT

# Create the heatmap
p = ggplot(model_best_wp, aes(x = model, y = combined)) +
  geom_tile(aes(fill = model), color = "white") +
  scale_fill_viridis_d() +  # You can adjust color scale as needed
  theme_minimal() +
  theme(
    plot.background = element_rect(
    fill = "white",
    colour = "white"))+
  labs(title = "Heatmap of Drug-Bug Combinations by Model Type",
       x = "Model Type",
       y = "Drug-Bug Combination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

ggsave(filename = paste0(dirOutput, "/Descriptives/Model_comparison_plots/Heatmap_bestmodel_prioritypath.png"), plot = p, 
       width = 10, height = 20)

#----------------------------------------------------------------------------
# CALCULATE WEIGHTED PREVALENCES
#----------------------------------------------------------------------------

# Function to calculate weighted prevalence
calculate_weighted_prevalence <- function(model_best_wp, model_fit, specimen, adataAS, cdata, pdataDM, adataAC) {
  
  # Create an empty list to store the results
  w_prev_results <- list()
  
  # Loop through the drug-bug combinations
  for (i in seq_along(model_best_wp$drug_bug)) {
    print(i)
    
    # Extract drug-bug combination and best model name
    model_best <- model_best_wp
    drug_bug_ns <- model_best$drug_bug[i]
    
    drug_bug <- drug_bug_ns
    
    drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
    drug_bug <- gsub("Staphylococcus aureus-Methicillin Resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
    drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
    
    drug_bug <- paste0(specimen, "-", drug_bug)
    
    best_model_name <- model_best$model[i]
    number <- as.numeric(str_extract(best_model_name, "\\d+"))+1
    
    model <- model_fit[[specimen]][[drug_bug_ns]][[number]]
    
    # Pre-filter and modify dataset
    subset_data_new <- adataAS %>%
      filter(combined == drug_bug, !Year %in% c(2016, 2017)) %>%
      select(WHORegionCode, Iso3, Year, AgeCat10, s_AgeCat10, Sex, Resistant, InterpretableAST, st_BCI_million, st_BCI_million_imp, testcoverage_cutoff) %>%
      mutate(InterpretableAST = 1, 
             st_BCI_million_imp = case_when(
               st_BCI_million > testcoverage_cutoff ~ st_BCI_million,
               st_BCI_million<= testcoverage_cutoff ~  testcoverage_cutoff,
               TRUE ~  testcoverage_cutoff),
             Year_c = scale(Year, center=TRUE, scale=FALSE)[,1]
      )
    
    # year center
    unique_years <- unique(subset_data_new[, c("Year", "Year_c")])
    
    # Countries to predict for
    iso3 <- unique(subset_data_new$Iso3)
    age <- unique(adataAS$AgeCat10)
    age <- age[age != "UnkAge"]
    sex <- unique(adataAS$Sex)
    sex <- sex[!sex %in% c("Unknown", "Other")]
    year <- c(2018:2023)

    # Create expanded dataset for prediction
    d <- expand.grid(
      Iso3 = iso3,
      AgeCat10 = age,
      Sex = sex,
      Year = year
    )
    d = left_join(d,unique_years)
    
    d2 <- left_join(d, cdata %>% select(Iso3, WHORegionCode, WHORegionName), by = "Iso3")
    d3 <- d2 %>%
      mutate(
        InterpretableAST = 1
      )
    d4 <- left_join(d3, subset_data_new%>%select(Iso3, Year, st_BCI_million_imp)%>%distinct(), by=c("Iso3", "Year"))
    d5 <- d4 %>%
      mutate(st_BCI_million_imp = ifelse(is.na(st_BCI_million_imp), subset_data_new$testcoverage_cutoff, st_BCI_million_imp)
      )
    
    # Countries that reported in 2023
    Iso3_2023 <- unique(subset_data_new$Iso3[subset_data_new$Year == 2023]) # Here need to also be at least 10 isolates
    
    # Countries that reported at least 3 years
    Iso3_2023_3y = subset_data_new %>% filter(Iso3 %in% Iso3_2023) %>%
       group_by(Iso3) %>%
      summarise(n = length(unique(Year))) %>%
      filter(n>2) %>% select(Iso3)
    
    # Create new data for prediction
    new_data <- d5
    new_data$RowID = as.character(1:dim(new_data)[1])
    new_data = left_join(new_data, pdataDM, by = c("Iso3", "Year", "AgeCat10", "Sex"))
    
    # Compute posterior for country-level age-sex weighted prevalence
    posterior_pred <- posterior_epred(model, newdata = new_data, type = "response")
    
    # !! IN BELOW I AM DOING EVERYTHING MANUALLY BUT ALTERNATIVELY COULD USE THE TIDYBAYES PACKAGE (epred_draws)
    # I THINK IN TERMS OF SPEED, TAKES THE SAME TIME !!
    #---------------------------------------------------------------------------------------------
    # tidy_posterior_pred <- model %>% 
    #   epred_draws(newdata = new_data)
    # 
    # 
    # posterior_weighted_country <- tidy_posterior_pred %>%
    #   group_by(.draw, WHORegionCode, Iso3, Year) %>%
    #   summarise(
    #     weighted_prevalence = sum(.epred * (TotalPopulation / sum(TotalPopulation))),
    #     .groups = "drop"
    #   ) %>%
    # median_hdi(weighted_prevalence)
    
     # Make long format so to combine posterior predictions with population data
     num_groups <- nrow(new_data)  # Total unique groups, i.e. number of posterior draws (10 000)
     
     posterior_long <- posterior_pred %>%
       as.data.frame() %>%
       pivot_longer(
         cols = everything(),
         names_to = "RowID",  # Each posterior draw
         values_to = "Estimate"
       ) %>%
       mutate(RowID = as.character(gsub("[^0-9]", "", RowID)),
              GroupID = rep(1:(n() / num_groups), each = num_groups)  # Assign GroupID in blocks of the number of observations in the data, so each posterior draw can be summarised
       ) 
     
     # Combine with population data
     posterior_long <- posterior_long %>%
      left_join(new_data, by = "RowID")
     
    
    # Calculate age-sex weighted mean prevalence for each posterior draw
     posterior_weighted_country <- posterior_long %>%
       group_by(GroupID, WHORegionCode, Iso3, Year) %>%
       summarise(
         weighted_prevalence = sum(Estimate * (TotalPopulation / sum(TotalPopulation))),
         .groups = "drop"
       )
     
     # Calculate median of all draws for each country to get age-sex weighted prevalences
     country_level_results <- posterior_weighted_country %>%
       group_by(WHORegionCode, Iso3, Year) %>%
       summarise(
         w_prev = median(weighted_prevalence),        # Median of the posterior
         w_prev_lower = quantile(weighted_prevalence, 0.025),  # 2.5% quantile
         w_prev_upper = quantile(weighted_prevalence, 0.975),  # 97.5% quantile
         Variance = var(weighted_prevalence),          # Variance of the posterior
         Precision = 1 / var(weighted_prevalence),     # Precision is inverse variance
         .groups = "drop"
       )
     
    # Add variance to country-level prevalences for each draw for the regional estimates
    posterior_weighted_country = left_join(posterior_weighted_country, country_level_results 
                                           %>%dplyr::select(Iso3, Year, Variance, Precision))
    
    # Final join with AMR data
    w_pred <- country_level_results %>%
      left_join(adataAC %>% # filter(InterpretableAST>10) %>% Here I should actually only link with those countries which had at least 10 isolates
                            # Didn't do this for the estimates created on 22 October 2024. Will filter them out afterwards.      
                  filter(combined == drug_bug) %>%
                  select(Iso3, Year, Resistant, InterpretableAST) %>%
                  mutate(amr_rate = Resistant / InterpretableAST), by = c("Iso3", "Year"))
    
    
    
    # Store the results
    w_prev_results[[drug_bug]] <- w_pred
  }
  
  return(w_prev_results)
}


# BSI estimates
w_prev_results_bsi <- calculate_weighted_prevalence(bsi_model_best_wp, model_fit_bsi,
                                                    specimen = "BLOOD", 
                                                    adataAS, cdata, pdataDM, adataAC)

# UTI estimates
w_prev_results_uti <- calculate_weighted_prevalence(uti_model_best_wp, model_fit_uti,
                                                    specimen = "URINE", 
                                                    adataAS, cdata, pdataDM, adataAC)


# GI estimates
w_prev_results_stool <- calculate_weighted_prevalence(stool_model_best_wp, model_fit_stool,
                                                    specimen = "STOOL", 
                                                    adataAS, cdata, pdataDM, adataAC)

# Gonorhoea estimates
w_prev_results_uro <- calculate_weighted_prevalence(uro_model_best_wp, model_fit_uro,
                                                    specimen = "UROGENITAL", 
                                                    adataAS, cdata, pdataDM, adataAC)


# STORE OUTPUT
#------------------------------------------------------------------------------------------
# Create Model_comparison directory if it does not exist yet
output_dir <- file.path(dirOutput, "AMR_prevalence","weakip", "Model_prevalence_estimates")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# UPDATE FROM HERE STILL!!

# BSI
saveRDS(w_prev_results_bsi, file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/predicted_w_prev_weakip_set1_bsi.rds"))
combined_w_prev_bsi <- w_prev_results_bsi %>%
  imap_dfr(~ .x %>% mutate(drug_bug = .y))  # Adds a new column with drug-bug names from list names

# Create Model_comparison directory if it does not exist yet
output_dir <- file.path(dirOutput,  "AMR_prevalence","weakip", "Model_prevalence_estimates", "Data_tables_prevalence")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write.csv(combined_w_prev_bsi,file.path(dirOutput, "AMR_prevalence","weakip", "Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_bsi.csv"))

# UTI
saveRDS(w_prev_results_uti, file.path(dirOutput, "AMR_prevalence","weakip", "Model_prevalence_estimates/predicted_w_prev_weakip_set1_uti.rds"))
combined_w_prev_uti <- w_prev_results_uti %>%
  imap_dfr(~ .x %>% mutate(drug_bug = .y))  

write.csv(combined_w_prev_uti,file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uti.csv"))

# STOOL
saveRDS(w_prev_results_stool, file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/predicted_w_prev_weakip_set1_stool.rds"))
combined_w_prev_stool <- w_prev_results_stool %>%
  imap_dfr(~ .x %>% mutate(drug_bug = .y))  

write.csv(combined_w_prev_stool,file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_stool.csv"))

# URO
saveRDS(w_prev_results_uro, file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/predicted_w_prev_weakip_set1_uro.rds"))
combined_w_prev_uro <- w_prev_results_uro %>%
  imap_dfr(~ .x %>% mutate(drug_bug = .y))  

write.csv(combined_w_prev_uro,file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uro.csv"))




# FILTER DATA ON THOSE THAT REPORTED IN 2023 AND STORE THIS INFORMATION IN DATATABLES
#------------------------------------------------------------------------------------------

# View(combined_w_prev_bsi %>% filter(InterpretableAST<10)) # These ones were excluded in the model fitting procedure
# Decide how to handle their predictions and whether to include any of them
# What I can do is only include those in the analyses that have at least three 
# years of data with a minimum of 10 isolates.

# !! THE BELOW CODE CAN STILL BE MADE CLEANER AND COMBINED WITH THE ABOVE SECTION!!


# SPECIFY PRIORSET FOR STORAGE
priorset = "all_one_model_weakip_set1_centered"

# CHECK FIRST IF NUMBERS ADD UP
bsi_db = unique(combined_w_prev_bsi$drug_bug)

# What data is used for model fitting
a = adataAS %>% filter(Year %in% c(2018,2019,2020,2021,2023) & combined%in%bsi_db) %>%
  distinct(Iso3, Year, combined)
a2023 = a %>% filter(Year==2023)

# Check with the original country level data 
b = adataAC %>% filter(InterpretableAST>10 & Year %in% c(2018,2019,2020,2021,2023)
                       &combined%in%bsi_db) # Yes same number of countries
b2023 = b %>% filter(Year==2023)

# BSI
c2023 = combined_w_prev_bsi %>% filter(
  InterpretableAST>10, Year==2023) # Yes same number of observations

c2023 = c2023 %>%
  mutate(data_submitted2023 = "Yes") %>%
  ungroup()

c2 = left_join(combined_w_prev_bsi, c2023%>%select(WHORegionCode,Iso3,drug_bug, data_submitted2023))
c3 = c2 %>% 
  mutate(data_submitted2023 = ifelse(is.na(data_submitted2023), "No", data_submitted2023),
         data_submitted = ifelse(InterpretableAST<10 | is.na(InterpretableAST), "No", "Yes"),
         predicted_nodata = ifelse(data_submitted=="No", "Yes", "No"))

drug_bug = c3$drug_bug
c3$combined = gsub("Co-trimoxazole","Co trimoxazole", c3$drug_bug)
c3$combined = gsub("Methicillin-resistance","Methicillin resistance", c3$combined)
c3$combined = gsub("Third-generation cephalosporins","Third generation cephalosporins", c3$combined)

c4 <- c3 %>%
  separate(combined, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")%>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)) %>%
  select(-c(drug_bug))

combined_w_prev_bsi2023 = c4

write.csv(combined_w_prev_bsi2023,file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_bsi.csv"))
#write.csv(combined_w_prev_bsi2023,file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables/", priorset, "/CTA_w_prev_bsi.csv"))

# UTI
c2023 = combined_w_prev_uti %>% filter(
  InterpretableAST>10, Year==2023) 

c2023 = c2023 %>%
  mutate(data_submitted2023 = "Yes") %>%
  ungroup()

c2 = left_join(combined_w_prev_uti, c2023%>%select(WHORegionCode,Iso3,drug_bug, data_submitted2023))
c3 = c2 %>% 
  mutate(data_submitted2023 = ifelse(is.na(data_submitted2023), "No", data_submitted2023),
         data_submitted = ifelse(InterpretableAST<10 | is.na(InterpretableAST), "No", "Yes"),
         predicted_nodata = ifelse(data_submitted=="No", "Yes", "No"))

drug_bug = c3$drug_bug
c3$combined = gsub("Co-trimoxazole","Co trimoxazole", c3$drug_bug)
c3$combined = gsub("Methicillin-resistance","Methicillin resistance", c3$combined)
c3$combined = gsub("Third-generation cephalosporins","Third generation cephalosporins", c3$combined)

c4 <- c3 %>%
  separate(combined, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")%>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)) %>%
  select(-c(drug_bug))

combined_w_prev_uti2023 = c4

write.csv(combined_w_prev_uti2023,file.path(dirOutput, "AMR_prevalence","weakip", "Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uti.csv"))
#write.csv(combined_w_prev_uti2023,file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables/", priorset, "/CTA_w_prev_uti.csv"))


# STOOL
c2023 = combined_w_prev_stool %>% filter(
  InterpretableAST>10, Year==2023) 

c2023 = c2023 %>%
  mutate(data_submitted2023 = "Yes") %>%
  ungroup()

c2 = left_join(combined_w_prev_stool, c2023%>%select(WHORegionCode,Iso3,drug_bug, data_submitted2023))
c3 = c2 %>% 
  mutate(data_submitted2023 = ifelse(is.na(data_submitted2023), "No", data_submitted2023),
         data_submitted = ifelse(InterpretableAST<10 | is.na(InterpretableAST), "No", "Yes"),
         predicted_nodata = ifelse(data_submitted=="No", "Yes", "No"))

c4 <- c3 %>%
  separate(drug_bug, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")%>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

combined_w_prev_stool2023 = c4

write.csv(combined_w_prev_stool2023,file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_stool.csv"))
#write.csv(combined_w_prev_stool2023,file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables/", priorset, "/CTA_w_prev_stool.csv"))

# URO
c2023 = combined_w_prev_uro %>% filter(
  InterpretableAST>10, Year==2023) 

c2023 = c2023 %>%
  mutate(data_submitted2023 = "Yes") %>%
  ungroup()

c2 = left_join(combined_w_prev_uro, c2023%>%select(WHORegionCode,Iso3,drug_bug, data_submitted2023))
c3 = c2 %>% 
  mutate(data_submitted2023 = ifelse(is.na(data_submitted2023), "No", data_submitted2023),
         data_submitted = ifelse(InterpretableAST<10 | is.na(InterpretableAST), "No", "Yes"),
         predicted_nodata = ifelse(data_submitted=="No", "Yes", "No")) 

c4 <- c3 %>%
  separate(drug_bug, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")%>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))


combined_w_prev_uro2023 = c4

write.csv(combined_w_prev_uro2023,file.path(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uro.csv"))
#write.csv(combined_w_prev_uro2023,file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables/", priorset, "/CTA_w_prev_uro.csv"))


# READ ALL TABLES IN AND COMBINE TO ONE DATA TABLE
combined_w_prev_bsi2023 = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_bsi.csv"))
combined_w_prev_uti2023 = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uti.csv"))
combined_w_prev_stool2023 = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_stool.csv"))
combined_w_prev_uro2023 = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_uro.csv"))

combined_w_prev_all2023 = rbind(combined_w_prev_bsi2023, combined_w_prev_uti2023, 
                                combined_w_prev_stool2023, combined_w_prev_uro2023)

write.csv(combined_w_prev_all2023,file.path(ddirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))
#write.csv(combined_w_prev_all2023,file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables/", priorset, "/CTA_w_prev_all.csv"))


# CALCULATE REGIONAL ESTIMATES MANUALLY (SIMILAR AS mva() function in metafor package)
#------------------------------------------------------------------------------------------------
# Initialize an empty list to store results for each drug_bug

combined_w_prev_all2023 <- read.csv(paste0(dirOutput, "AMR_prevalence","weakip","Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))
model_best_wp = read.csv(file.path(dirOutput,"AMR_prevalence", "weakip", "Model_comparison/best_model_fit_all.csv"))

results_list <- list()

# Unique drug_bug combinations
drug_bugs <- unique(combined_w_prev_all2023$combined)

# Loop over each drug_bug
for (drug_bug in drug_bugs) {
  
  # Subset data for the current drug_bug
  d0 <- combined_w_prev_all2023 %>%
    filter(combined == drug_bug)
  
    # Keep countries with at least three years of data
    iso_with_3_years <- d0 %>%
      filter(data_submitted == "Yes") %>%
      group_by(Iso3) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 2) %>%
      pull(Iso3)
    
    # Keep countries with data submitted in 2023
    iso_with_2023_data <- d0 %>%
      filter(data_submitted2023 == "Yes" & Year == 2023) %>%
      pull(Iso3)
    
    # Filter valid rows
    d <- d0 %>%
      filter(Iso3 %in% iso_with_3_years | Iso3 %in% iso_with_2023_data)
  
  # Logit transformation
  d1 <- d %>%
    mutate(
      logit_prev = log(w_prev / (1 - w_prev)),
      logit_lower = log(w_prev_lower / (1 - w_prev_lower)),
      logit_upper = log(w_prev_upper / (1 - w_prev_upper)),
      Bayesian_Variance = ((logit_upper - logit_lower) / (2 * 1.96))^2
    )
  
  # Calculate Regional Estimates on the logit scale
  d2 <- d1 %>%
    group_by(WHORegionCode, Year, combined) %>%
    summarise(
      preliminary_mean = sum(logit_prev / Bayesian_Variance) / sum(1 / Bayesian_Variance),
      Q = sum((logit_prev - preliminary_mean)^2 / Bayesian_Variance),
      df = n() - 1,
      tau2 = max(0, (Q - df) / (sum(1 / Bayesian_Variance) - (sum(1 / sqrt(Bayesian_Variance))^2 / sum(1 / Bayesian_Variance)))),
      .groups = "drop"
    )
  
  d3 <- left_join(d2, d1, by = c("WHORegionCode", "Year", "combined"))
  d4 <- d3 %>%
    mutate(
      Total_Variance = Bayesian_Variance + tau2
    )
  
  regional_results <- d4 %>%
    group_by(WHORegionCode, Year, Specimen, PathogenName, AntibioticName, combined) %>%
    summarise(
      logit_weighted_mean = sum(logit_prev / Total_Variance) / sum(1 / Total_Variance),
      regional_variance = 1 / sum(1 / Total_Variance),
      logit_Q2.5 = logit_weighted_mean - 1.96 * sqrt(regional_variance),
      logit_Q97.5 = logit_weighted_mean + 1.96 * sqrt(regional_variance),
      n_countries = n_distinct(Iso3[data_submitted == "Yes"]),
      regional_cutoff = ifelse(n_countries < 5, "No", "Yes"),
      .groups = "drop"
    ) %>%
    mutate(
      regional_weighted_mean = exp(logit_weighted_mean) / (1 + exp(logit_weighted_mean)),
      Q2.5 = exp(logit_Q2.5) / (1 + exp(logit_Q2.5)),
      Q97.5 = exp(logit_Q97.5) / (1 + exp(logit_Q97.5))
    )
  
  # Global Estimates
  d1_g <- d1 %>%
    mutate(WHORegionCode = "Global")
  
  d2_g <- d1 %>%
    group_by(Year, combined) %>%
    summarise(
      preliminary_mean = sum(logit_prev / Bayesian_Variance) / sum(1 / Bayesian_Variance),
      Q = sum((logit_prev - preliminary_mean)^2 / Bayesian_Variance),
      df = n() - 1,
      tau2 = max(0, (Q - df) / (sum(1 / Bayesian_Variance) - (sum(1 / sqrt(Bayesian_Variance))^2 / sum(1 / Bayesian_Variance)))),
      .groups = "drop"
    )
  
  d3_g <- left_join(d2_g, d1_g, by = c("Year", "combined"))
  d4_g <- d3_g %>%
    mutate(
      Total_Variance = Bayesian_Variance + tau2
    )
  
  global_results <- d4_g %>%
    group_by(Year, Specimen, PathogenName, AntibioticName, combined) %>%
    summarise(
      logit_weighted_mean = sum(logit_prev / Total_Variance) / sum(1 / Total_Variance),
      regional_variance = 1 / sum(1 / Total_Variance),
      logit_Q2.5 = logit_weighted_mean - 1.96 * sqrt(regional_variance),
      logit_Q97.5 = logit_weighted_mean + 1.96 * sqrt(regional_variance),
      n_countries = n_distinct(Iso3[data_submitted == "Yes"]),
      regional_cutoff = ifelse(n_countries < 5, "No", "Yes"),
      .groups = "drop"
    ) %>%
    mutate(
      regional_weighted_mean = exp(logit_weighted_mean) / (1 + exp(logit_weighted_mean)),
      Q2.5 = exp(logit_Q2.5) / (1 + exp(logit_Q2.5)),
      Q97.5 = exp(logit_Q97.5) / (1 + exp(logit_Q97.5)),
      WHORegionCode = "Global"
    )
  
  # Combine regional and global results
  combined_results <- bind_rows(regional_results, global_results)
  
  # Add results to the list
  results_list[[drug_bug]] <- combined_results
}

# Combine all results into a single dataframe
final_results <- bind_rows(results_list, .id = "drug_bug")

# Combine all results into a single dataframe
final_results <- bind_rows(results_list, .id = "drug_bug")
#final_results$Q2.5 = ifelse(final_results$Q2.5<0,0,final_results$Q2.5)

p = ggplot(final_results, aes(x=Year, regional_weighted_mean, group=WHORegionCode, col=WHORegionCode))+
  geom_line(size=1)+
  geom_point()+
  #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill =WHORegionCode, alpha=0.2))+
  facet_wrap(Specimen~combined)+
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = ggtext::element_markdown(size = 13, hjust = 0),
    strip.text.x.top = ggtext::element_markdown(
      size = 16,  # Adjust font size
      margin = margin(r = 5)  # Reduce right margin to move strip closer
    ),
    panel.background = element_rect(fill = "white", color = NA),
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),  # Reduce space between panels
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),  # Dotted major grid
    panel.grid.minor = element_line(color = "grey80", linetype = "dotted"),
    legend.text = element_text(size = 12)  # Change legend label size
  )
p

# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Provisional/Figure_4.7_REGION_trends_plot.svg"),
#                plot = p,
#                width = 13, height = 15, dpi=300)

da = final_results
da$regional_weighted_mean = da$regional_weighted_mean*100
da$Q2.5 = da$Q2.5*100
da$Q97.5 = da$Q97.5*100

#write.csv(x=da, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.7_REGION_trends_allyears.csv"))
  
  
# Add whether overlapping uncertainty intervals between 2018 and 2023
# Filter for relevant years (2018 and 2023) and calculate overlap
comparison_results <- final_results %>%
  filter(Year %in% c(2018, 2023)) %>%
  group_by(WHORegionCode, combined, Specimen) %>%
  summarise(
    Q2.5_2018 = Q2.5[Year == 2018],
    Q97.5_2018 = Q97.5[Year == 2018],
    Q2.5_2023 = Q2.5[Year == 2023],
    Q97.5_2023 = Q97.5[Year == 2023],
    overlap = ifelse(
      max(Q2.5_2018, Q2.5_2023) <= min(Q97.5_2018, Q97.5_2023), 
      "Yes", 
      "No"
    ),
    .groups = "drop"
  )

# Bind overlap information back to final_results
final_results <- final_results %>%
  left_join(
    comparison_results %>% select(WHORegionCode, combined, Specimen, overlap),
    by = c("WHORegionCode", "combined", "Specimen")
  ) %>% filter(combined!="BLOOD-Streptococcus pneumoniae-Cefotaxime") # THIS ONE GOT ACCIDENTLY INCLUDED

final_results <- final_results %>%
  mutate(regional_weighted_mean = regional_weighted_mean*100,
         Q2.5 = Q2.5*100,
         Q97.5 = Q97.5*100)

results_change <- final_results %>%
  group_by(drug_bug, WHORegionCode) %>%
  filter(Year == min(Year) | Year == max(Year)) %>%  # Keep only first and last year
  summarise(
    f_year = min(Year),
    l_year = max(Year),
    f_weighted_mean = regional_weighted_mean[Year == min(Year)],  # Value in the first year
    l_weighted_mean = regional_weighted_mean[Year == max(Year)],   # Value in the last year
    f_Q2.5 = Q2.5[Year == min(Year)],  # Value in the first year
    l_Q2.5 = Q2.5[Year == max(Year)],
    f_Q97.5 = Q97.5[Year == min(Year)],  # Value in the first year
    l_Q97.5 = Q97.5[Year == max(Year)],
    change = ((l_weighted_mean - f_weighted_mean) / f_weighted_mean) * 100,  # Calculate %
    Q97.5_change = ((l_Q97.5 - f_Q97.5) / f_Q97.5) * 100,
    Q2.5_change = ((l_Q2.5 - f_Q2.5) / f_Q2.5) * 100
  )

final_results2023 = final_results%>%filter(Year%in%c(2023))
final_results2023 = left_join(final_results2023,results_change%>%dplyr::select(drug_bug,WHORegionCode, change,Q2.5_change,Q97.5_change))


############################################################################
# CALCULATE REGIONAL SLOPES
############################################################################


summarize_regional_and_global_slopes <- function(model_results, year_popdata, pdata, cdata, region_var = "WHORegionName", specimen, drug_bug, weighting_method = "population") {
  # Extract posterior samples for fixed and random effects
  posterior_samples <- as_draws_df(model_results)
  
  # Extract fixed effect for Year_c
  fixed_effect_samples <- posterior_samples$b_Year_c
  
  # Extract column names for random effects of Year_c
  random_effect_columns <- grep("^r_Iso3\\[.*?,Year_c\\]$", colnames(posterior_samples), value = TRUE)
  
  # Subset the random effects for Year_c
  random_effect_samples <- posterior_samples[, random_effect_columns]
  
  # Combine fixed and random slopes for each posterior draw
  posterior_country_slopes <- sweep(as.matrix(random_effect_samples), 1, fixed_effect_samples, FUN = "+")
  colnames(posterior_country_slopes) <- gsub("r_Iso3\\[|,Year_c\\]", "", colnames(posterior_country_slopes))  # Clean column names
  
  # Calculate standard error (posterior SD) for each country slope if using inverse variance weighting
  if (weighting_method == "inverse_variance") {
    slope_se <- apply(posterior_country_slopes, 2, sd)  # SD across posterior draws
    slope_se_df <- data.frame(
      Iso3 = names(slope_se),
      country_slope_se = slope_se
    )
  }
  
  # Filter population data for the specified year
  pdata2023 <- pdata %>%
    filter(Year == year_popdata) %>%
    dplyr::select(Iso3, TotalPopulation)
  
  # Join with region data
  pdata2023 <- left_join(pdata2023, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  
  # Create long format of the country-level slopes
  posterior_country_long <- posterior_country_slopes %>%
    as.data.frame() %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(
      cols = -draw,
      names_to = "Iso3",
      values_to = "country_slope"
    )
  
  # Join with population data
  posterior_country_long <- left_join(posterior_country_long, pdata2023, by = "Iso3")
  
  # Add standard error if using inverse variance weighting
  if (weighting_method == "inverse_variance") {
    posterior_country_long <- left_join(posterior_country_long, slope_se_df, by = "Iso3") %>%
      mutate(weight = 1 / (country_slope_se^2))
  } else {
    posterior_country_long <- posterior_country_long %>%
      mutate(weight = TotalPopulation)
  }
  
  # ONLY SELECT COUNTRIES WITH MINIMUM 3 YEARS OF DATA AND DATA IN 2023
  combined = paste0(specimen, "-", drug_bug)
  
  # Keep countries with at least three years of data
  iso_with_3_years <- adataAC %>%
    filter(InterpretableAST > 10, combined == combined) %>%
    group_by(Iso3) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 2) %>%
    pull(Iso3)
  
  # Compute weighted mean slopes for each region
  posterior_regional_slopes <- posterior_country_long %>%
    filter(Iso3 %in% iso_with_3_years) %>%
    group_by(draw, !!sym(region_var)) %>%
    summarise(
      weighted_mean_slope = sum(country_slope * weight) / sum(weight),
      .groups = "drop"
    )
  
  # Summarize posterior trends for each region
  regional_trend_summary <- posterior_regional_slopes %>%
    group_by(!!sym(region_var)) %>%
    summarise(
      mean_slope = mean(weighted_mean_slope),
      median_slope = median(weighted_mean_slope),
      Q2.5 = quantile(weighted_mean_slope, 0.025),
      Q97.5 = quantile(weighted_mean_slope, 0.975),
      .groups = "drop"
    )
  
  # Compute the global weighted mean slope
  posterior_global_trends <- posterior_country_long %>%
    filter(Iso3 %in% iso_with_3_years) %>%
    group_by(draw) %>%
    summarise(
      global_weighted_mean_slope = sum(country_slope * weight) / sum(weight),
      .groups = "drop"
    )
  
  # Summarize global trend
  global_trend_summary <- posterior_global_trends %>%
    summarise(
      mean_slope = mean(global_weighted_mean_slope),
      median_slope = median(global_weighted_mean_slope),
      Q2.5 = quantile(global_weighted_mean_slope, 0.025),
      Q97.5 = quantile(global_weighted_mean_slope, 0.975)
    ) %>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(WHORegionName, mean_slope, median_slope, Q2.5, Q97.5)
  
  # Combine regional and global summaries
  regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)
  regional_trend_summary$specimen = specimen
  regional_trend_summary$drug_bug = drug_bug
  
  return(regional_trend_summary)
}


# BSI
bsi_slopes = NULL
incb = which(unique(model_best_wp$combined)%in%c("BLOOD-Acinetobacter spp.-Imipenem", "BLOOD-Escherichia coli-Cefotaxime",                     
                                              "BLOOD-Escherichia coli-Imipenem","BLOOD-Escherichia coli-Third generation cephalosporins",
                                              "BLOOD-Klebsiella pneumoniae-Cefotaxime","BLOOD-Klebsiella pneumoniae-Imipenem" ,                 
                                              "BLOOD-Staphylococcus aureus-Methicillin resistance",            
                                              "BLOOD-Streptococcus pneumoniae-Penicillin G"))
for(i in incb){
  model_best <- model_best_wp
  drug_bug_ns <- model_best$drug_bug[i]
  
  drug_bug <- drug_bug_ns
  
  drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
  drug_bug <- gsub("Staphylococcus aureus-Methicillin resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
  drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
  
  #drug_bug <- paste0(specimen, "-", drug_bug)
  
  best_model_name <- model_best$model[i]
  number <- as.numeric(str_extract(best_model_name, "\\d+"))+1
  specimen = "BLOOD"
  model <- model_fit_bsi[[specimen]][[drug_bug_ns]][[number]]
  d = summarize_regional_and_global_slopes(model, year_popdata = 2023, pdata, cdata, specimen="BLOOD",drug_bug=drug_bug, weighting_method = "population")
  bsi_slopes = rbind(bsi_slopes,d)
}

# GI
gi_slopes = NULL
incg = which(unique(model_best_wp$combined)%in%c("STOOL-Salmonella spp.-Ciprofloxacin",                   
                                            "STOOL-Shigella spp.-Ciprofloxacin" ))
for(i in incg){
  model_best <- model_best_wp
  drug_bug_ns <- model_best$drug_bug[i]
  
  drug_bug <- drug_bug_ns
  
  drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
  drug_bug <- gsub("Staphylococcus aureus-Methicillin Resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
  drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
  
  #drug_bug <- paste0(specimen, "-", drug_bug)
  
  best_model_name <- model_best$model[i]
  number <- as.numeric(str_extract(best_model_name, "\\d+"))+1
  specimen = "STOOL"
  model <- model_fit_stool[[specimen]][[drug_bug_ns]][[number]]
  d = summarize_regional_and_global_slopes(model, year_popdata = 2023, pdata, cdata, specimen="STOOL",drug_bug=drug_bug, weighting_method = "population")
  gi_slopes = rbind(gi_slopes,d)
}

# UTI
uti_slopes = NULL
incu = which(unique(model_best_wp$combined)%in%c("URINE-Escherichia coli-Cefotaxime",                     
                                              "URINE-Escherichia coli-Imipenem","URINE-Klebsiella pneumoniae-Cefotaxime",                
                                              "URINE-Klebsiella pneumoniae-Imipenem"))
for(i in incu){
  model_best <- model_best_wp
  drug_bug_ns <- model_best$drug_bug[i]
  
  drug_bug <- drug_bug_ns
  
  drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
  drug_bug <- gsub("Staphylococcus aureus-Methicillin Resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
  drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
  
  #drug_bug <- paste0(specimen, "-", drug_bug)
  
  best_model_name <- model_best$model[i]
  number <- as.numeric(str_extract(best_model_name, "\\d+"))+1
  specimen = "URINE"
  model <- model_fit_uti[[specimen]][[drug_bug_ns]][[number]]
  d = summarize_regional_and_global_slopes(model, year_popdata = 2023, pdata, cdata, specimen="URINE",drug_bug=drug_bug, weighting_method = "population")
  uti_slopes = rbind(uti_slopes,d)
}

# gon
gon_slopes = NULL
incgon = which(unique(model_best_wp$combined)%in%c("UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone"))

for(i in incgon){
  model_best <- model_best_wp
  drug_bug_ns <- model_best$drug_bug[i]
  
  drug_bug <- drug_bug_ns
  
  drug_bug <- gsub("Co trimoxazole", "Co-trimoxazole", drug_bug)
  drug_bug <- gsub("Staphylococcus aureus-Methicillin Resistance", "Staphylococcus aureus-Methicillin-resistance", drug_bug)
  drug_bug <- gsub("Escherichia coli-Third generation cephalosporins", "Escherichia coli-Third-generation cephalosporins", drug_bug)
  
  #drug_bug <- paste0(specimen, "-", drug_bug)
  
  best_model_name <- model_best$model[i]
  number <- as.numeric(str_extract(best_model_name, "\\d+"))+1
  specimen = "UROGENITAL"
  model <- model_fit_uro[[specimen]][[drug_bug_ns]][[number]]
  d = summarize_regional_and_global_slopes(model, year_popdata = 2023, pdata, cdata, specimen="UROGENITAL",drug_bug=drug_bug, weighting_method = "population")
  gon_slopes = rbind(gon_slopes,d)
}

slopes_all = rbind(bsi_slopes,gi_slopes, uti_slopes,gon_slopes)

slopes_all = slopes_all %>% 
  mutate(WHORegionName = factor(WHORegionName, 
                                levels = c("Global", 
                                           "African Region",
                                           "Region of the Americas",
                                           "South-East Asia Region",
                                           "European Region",
                                           "Eastern Mediterranean Region",
                                           "Western Pacific Region")),
         mean_slope_p = (exp(mean_slope)-1)*100,
         Q2.5_p = (exp(Q2.5)-1)*100,
         Q97.5_p = (exp(Q97.5)-1)*100,
         drug_bug = ifelse(drug_bug=="Staphylococcus aureus-Methicillin-resistance", "Staphylococcus aureus-Methicillin resistance", 
                           ifelse(drug_bug == "Escherichia coli-Third-generation cephalosporins", "Escherichia coli-Third generation cephalosporins", drug_bug)),
         combined = paste0(specimen,"-",drug_bug),
         specimen = factor(specimen, levels=c("BLOOD", "STOOL", "STOOL", "UROGENITAL"),
                           labels = c("Bloodstream", "Gastrointestinal", "Urinary tract", "Gonorrhoea"))
  ) %>%
  separate(combined, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")

# IDENTIFY IF AT LEAST 5 COUNTRIES PER REGION
# Keep countries with at least three years of data
combined = unique(paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName))
combined = ifelse(combined=="BLOOD-Staphylococcus aureus-Methicillin resistance", "BLOOD-Staphylococcus aureus-Methicillin-resistance", 
       ifelse(combined == "BLOOD-Escherichia coli-Third generation cephalosporins", "BLOOD-Escherichia coli-Third-generation cephalosporins", combined))

nyears_cta <- adataAC %>%
  filter(InterpretableAST>10 & combined %in% combined & Year>2017) %>%
  group_by(WHORegionName, Iso3,combined) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 2) #%>%
  #pull(Iso3)

ncountry_region = nyears_cta %>%
  group_by(WHORegionName, combined) %>%
  summarise(n = n())

ncountry_global = nyears_cta %>%
  group_by(combined) %>%
  summarise(n = n()) %>%
  mutate(WHORegionName = "Global") %>%
  select(WHORegionName, combined,n)

ncountry = rbind(ncountry_region, ncountry_global)
ncountry$combined = ifelse(ncountry$combined=="BLOOD-Staphylococcus aureus-Methicillin-resistance", "BLOOD-Staphylococcus aureus-Methicillin resistance", 
                           ifelse(ncountry$combined == "BLOOD-Escherichia coli-Third-generation cephalosporins", "BLOOD-Escherichia coli-Third generation cephalosporins", ncountry$combined))

slopes_all$combined = paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName)


t0 = left_join(slopes_all, ncountry)

t0$Significant = ifelse(t0$Q2.5_p>=1&t0$n>5|t0$Q97.5_p<=-1&t0$n>5, "Yes", "No")

# Keep countries with data submitted in 2023
# iso_with_2023_data <- adataAC %>%
#   filter(InterpretableAST>10, combined == combined & Year == 2023) %>%
#   pull(Iso3)

t1 <- t0%>%
  dplyr::select(drug_bug, WHORegionName, Specimen, PathogenName, AntibioticName, mean_slope_p,Q2.5_p,Q97.5_p, Significant, n) %>%
  arrange(Specimen, AntibioticName, PathogenName) %>%
  group_by(Specimen, AntibioticName) %>%
  mutate(
    AntibioticName2 = case_when(
      AntibioticName == "Third generation cephalosporins" ~ '3rd-gen. cephalosporins',
      TRUE ~ AntibioticName
    ),
    PathogenName2 = factor(
      PathogenName,
      levels = c("Acinetobacter spp.", "Escherichia coli", "Klebsiella pneumoniae",
                 "Salmonella spp.", "Staphylococcus aureus", "Streptococcus pneumoniae",
                 "Shigella spp.", "Neisseria gonorrhoeae"),
      labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                 "Salmonella spp.", "S. aureus", "S. pneumoniae",
                 "Shigella spp.", "N. gonorrhoeae")
    ),
    custom_label = case_when(
      grepl("spp\\.", PathogenName2) ~ paste0("<i>", sub(" spp\\.", "", PathogenName2), "</i> spp. - ", AntibioticName2),
      TRUE ~ paste0("<i>", PathogenName2, "</i> - ", AntibioticName2)
    ),
    custom_label = factor(custom_label,
                          levels = c(
                            "<i>N. gonorrhoeae</i> - Ceftriaxone",
                            "<i>Shigella</i> spp. - Ciprofloxacin",
                            "<i>Salmonella</i> spp. - Ciprofloxacin",
                            "<i>S. pneumoniae</i> - Penicillin G",
                            "<i>S. aureus</i> - Methicillin resistance",
                            "<i>K. pneumoniae</i> - Imipenem",
                            "<i>K. pneumoniae</i> - Cefotaxime",
                            "<i>E. coli</i> - Imipenem",
                            "<i>E. coli</i> - 3rd-gen. cephalosporins",
                            "<i>E. coli</i> - Cefotaxime",
                            "<i>Acinetobacter</i> spp. - Imipenem" )),
    Specimen_label = case_when(
      Specimen == "BLOOD" ~ "Bloodstream",
      Specimen == "URINE" ~ "Urinary tract",
      Specimen == "STOOL" ~ "Gastrointestinal",
      Specimen == "UROGENITAL" ~ "Gonorrhoea",
      TRUE ~ NA_character_
    ),
    Specimen_label = factor(Specimen_label, levels = c("Bloodstream",
                                                       "Gastrointestinal", "Urinary tract",  "Gonorrhoea")),
    region_labels = factor(WHORegionName, levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region", "European Region",
                                                     "Eastern Mediterranean Region", "Western Pacific Region"),
                             labels = c(
      "Global" = "Global",
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region")
    ),
    text_with_star = ifelse(
      Significant == "Yes",
      paste0(format(round(mean_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")*"),
      paste0(format(round(mean_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")")
    )
  ) %>%
  ungroup()


# Visualise slope change (Pathogen)
# Flag cells with a star
# Correctly assign linetype for starred tiles based on credible intervals
t1 <- t1 %>%
  mutate(
    has_star = grepl("\\*", text_with_star),
    linetype = ifelse(has_star & Q2.5_p >= 1&t1$n>5, "solid",  # Solid for significant positive effect
                      ifelse(has_star & Q97.5_p <= -1&t1$n>5, "none", NA))  # Dotted for significant negative effect
  )

# Updated ggplot with bold and styled borders around starred tiles
p1 = ggplot(t1, aes(x = region_labels, y = factor(custom_label))) +
  geom_tile(aes(fill = mean_slope_p), linewidth = 0.8) +
  ggtext::geom_richtext(
    aes(label = text_with_star), 
    size = 3.5, 
    color = "black",
    fill = NA, 
    label.color = NA
  ) +
  # Add borders around tiles with a star and apply linetype for significant positive/negative slopes
  # geom_tile(
  #   data = t1 %>% filter(has_star & !is.na(linetype)),  # Correct subsetting
  #   aes(linetype = linetype),
  #   color = "black",
  #   linewidth = 1,  # Thicker border for emphasis
  #   fill = NA  # Transparent fill to show tile background
  # ) +
  #scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) +  # Define line styles
  scale_fill_gradient2(
    low = "dodgerblue", 
    mid = "white", 
    high = "firebrick", 
    midpoint = 0, 
    name = "% Change",
    na.value = "grey90"
  ) +
  labs(
    x = "",
    y = NULL,
    title = ""
  ) +
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "top") +
  ggh4x::facet_grid2(Specimen_label ~ ., scales = "free_y", space = "free_y", switch = "both",
                     labeller = labeller(Specimen_label = c(
                       "Bloodstream" = "Bloodstream\n", 
                       "Urinary tract" = "Urinary tract\n",
                       "Gastrointestinal" = "Gastrointestinal\n",
                       "Gonorrhoea" = "Gonorrhoea\n"
                     ))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.y.left = element_text(
      angle = 90, 
      size = 12,  
      margin = margin(r = 5)
    ),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "lines"),
    legend.position = "bottom",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(linetype = "none", colour = "none")  # Hide linetype legend

p1

p2 = ggplot(t1%>%filter(Specimen=="BLOOD"), aes(x = region_labels, y = factor(custom_label))) +
  geom_tile(aes(fill = mean_slope_p), linewidth = 0.8) +
  ggtext::geom_richtext(
    aes(label = text_with_star), 
    size = 3.5, 
    color = "black",
    fill = NA, 
    label.color = NA
  ) +
  # Add borders around tiles with a star and apply linetype for significant positive/negative slopes
  geom_tile(
    data = t1 %>% filter(has_star & !is.na(linetype) &Specimen=="BLOOD"),  # Correct subsetting
    aes(linetype = linetype),
    color = "black",
    linewidth = 1,  # Thicker border for emphasis
    fill = NA  # Transparent fill to show tile background
  ) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "none")) +  # Define line styles
  scale_fill_gradient2(
    low = "dodgerblue", 
    mid = "white", 
    high = "firebrick", 
    midpoint = 0, 
    name = "% Change",
    na.value = "grey90"
  ) +
  labs(
    x = "",
    y = NULL,
    title = ""
  ) +
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "top") +
  ggh4x::facet_grid2(Specimen_label ~ ., scales = "free_y", space = "free_y", switch = "both",
                     labeller = labeller(Specimen_label = c(
                       "Bloodstream" = "Bloodstream\n", 
                       "Urinary tract" = "Urinary tract\n",
                       "Gastrointestinal" = "Gastrointestinal\n",
                       "Gonorrhoea" = "Gonorrhoea\n"
                     ))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.y.left = element_text(
      angle = 90, 
      size = 14,  
      margin = margin(r = 5)
    ),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "lines"),
    legend.position = "bottom",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(linetype = "none", colour = "none")  # Hide linetype legend

p2



# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11_trend_regional_amr.png"), 
#        plot = p1,
#        device = "png",
#        dpi = 300,
#        width = 10, height = 12)  
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Provisional/Figure_4.11_trend_regional_amr_BSI_nodecline.png"), 
#        plot = p2,
#        device = "png",
#        dpi = 300,
#        width = 11, height = 6)  


write.csv(x=final_results%>%select(-c(overlap)), file = paste0(dirOutput, "/Chapter_3/Ch3_summary_stats/Figure_3.12_REGION_prevalence_allyears.csv"))
write.csv(x=t1, file = paste0(dirOutput, "/Chapter_3/Ch3_summary_stats/Figure_4.12_REGION_prevalence_slope.csv"))

#write.csv(x=final_results2023, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_trends_2023change.csv"))


final_results = final_results %>%
  mutate(
    WHORegionName = case_when(
      WHORegionCode == "Global" ~ "Global", 
      WHORegionCode == "AFR" ~ "African Region",
      WHORegionCode == "AMR" ~ "Region of the Americas",
      WHORegionCode == "SEA" ~ "South-East Asia Region", 
      WHORegionCode == "EUR" ~ "European Region",
      WHORegionCode == "EMR" ~ "Eastern Mediterranean Region",
      WHORegionCode == "WPR" ~ "Western Pacific Region",
      TRUE ~ NA_character_
    ),
    WHORegionName = factor(WHORegionName, levels = c(
      "Global",
      "African Region",
      "Region of the Americas",
      "South-East Asia Region",
      "European Region",
      "Eastern Mediterranean Region",
      "Western Pacific Region")
    ),
    region_labels = factor(WHORegionName, levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region", "European Region",
                                                     "Eastern Mediterranean Region", "Western Pacific Region"),
                             labels = c(
      "Global" = "Global",
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region")
    ),
    formatted_drug_bug = case_when(
      drug_bug == "BLOOD-Klebsiella pneumoniae-Imipenem" ~ "<i>K. pneumoniae</i> - Imipenem",
      TRUE ~ drug_bug
    ))

# Visualize Regional Trends (Specimen)

p3 = ggplot(final_results %>% filter(drug_bug %in% c("BLOOD-Klebsiella pneumoniae-Imipenem","URINE-Klebsiella pneumoniae-Imipenem")), aes(x = Year, y = regional_weighted_mean)) +
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5),
    alpha = 0.2, fill = "#377EB8"
  ) +
  geom_line(size = 1, colour = "#377EB8") +  # Line for median predictions
  ggh4x::facet_grid2(
    Specimen ~ region_labels, scales = "free_y", independent = "y"  # Use markdown-compatible labeller
  ) +
  labs(
    title = "",
    y = "Percentage resistant",
    x = ""
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  theme(
    strip.placement = "outside",
    strip.text.y = ggtext::element_markdown(angle = 0, hjust = 0.5, size = 16, face = "bold"),  # Ensure markdown support for facet labels
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.x = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted")
  ) +
  theme_minimal() +
  guides(
    color = "none",
    fill = "none",
    size = "none"
  )
p3

combined_plot <- p3 / p1 +
  plot_layout(heights = c(1, 3))
combined_plot

write.csv(x=final_results, file = paste0(dirOutput, "/Chapter_3/Ch3_summary stats/Figure_4.12_REGION_trends_allyears.csv"))
write.csv(x=final_results2023, file = paste0(dirOutput, "/Chapter_3/Ch3_summary_stats/Figure_3.12_REGION_trends_2023change.csv"))

