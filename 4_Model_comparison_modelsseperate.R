################################################################
# MODEL COMPARISON
################################################################

# Author: Esther van Kleef
# Date created: July 2024
# Date last updated: 03 March 2026


# Threshold for significance of different fits between models:  
# -------------------------------------------------------------------------------------- 
# ELPD difference (Difference in Expected Log Predictive Density):

# A common rule of thumb is to compare the magnitude of elpd_diff to its standard error (se_diff).
#   - If elpd_diff > 2x se_diff, you can be confident that the model with the higher elpd_loo has better predictive performance.
#   - If elpd_diff < 2x se_diff, the models have similar predictive performance, and the difference is likely not meaningful.

# looic (Leave-One-Out Information Criterion):

# looic differences are often interpreted similarly to AIC or DIC. 
# If the difference in looic values between models is more than 4, it generally indicates a meaningful improvement in model fit. 
# If the difference is greater than 10, it suggests strong evidence that one model is better than the other.

# - Difference < 4: The models are essentially indistinguishable in terms of predictive accuracy.
# - Difference between 4 and 10: Moderate evidence that one model is better.
# - Difference > 10: Strong evidence that one model is better.


rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, brms, loo)

# Locate directories
dirDataRaw   <- here("Data", "raw")
dirDataClean <- here("Data", "cleaned")
dirOutput    <- here("Output")
dirDataModeloutput = paste0(dirOutput,"/Model_output/AMR_prevalence/weakip/")

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
adataAC_crude = read.csv(paste0(dirDataRaw, "/EI_AMRdtaAC_110325_EV.csv"), sep=",")   # Country AMR data; use CRUDE data as wrangled by OLGA, where E.coli and MRSA are not manually changed

# AMR data
adataAC = read.csv(paste0(dirDataClean, "/EI_AMRdtaAC_Pop_country_HAQI_140325_EV.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataClean, "/final_linked_data/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataClean, "/updated_summary_dbc_longformat.csv"))

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

###################################################################################
# LOAD IN MODEL OUTPUTS
###################################################################################

# Function to load RDS files and store them in a list
load_models_from_rds <- function(rds_files) {
  models <- list()
  
  # Loop through each RDS file and load it into the models list
  for (file in rds_files) {
    # Extract the model name from the file name
    model_name <- gsub(".*/|\\.rds", "", file)
    print(model_name)
    # Read the model and store it in the list
    models[[model_name]] <- readRDS(file)
  }
  
  return(models)
}

# Function to adjust the naming for Staphylococcus aureus-Methicillin-resistance; 3GC and Co-trimoxazole so 
# the split_model_names function works
update_model_names <- function(model_names) {
  # Define the pattern and replacement pairs
  patterns <- c("Staphylococcus aureus-Methicillin-resistance",
                "Co-trimoxazole",
                "Third-generation cephalosporins")
  replacements <- c("Staphylococcus aureus-Methicillin Resistance",
                    "Co trimoxazole",
                    "Third generation cephalosporins")
  
  # Loop through each pattern and apply the replacement
  for (i in seq_along(patterns)) {
    model_names[grep(patterns[i], model_names)] <- 
      gsub(patterns[i], replacements[i], model_names[grep(patterns[i], model_names)])
  }
  
  return(model_names)  # Return the updated list
}

# Create a function to split names
split_model_names <- function(model_name) {
  split_name <- unlist(strsplit(model_name, "-|_", perl = TRUE))
  list(
    specimen = split_name[1],
    drug_bug = paste(split_name[2], split_name[3], sep = "-"),
    model = split_name[4]
  )
}

# First a list with all specimen
organize_models <- function(models, split_names) {
  models_by_specimen <- list()  # Initialize the list to store models by specimen
  
  for (i in seq_along(models)) {
    # Extract specimen, drug_bug, and model from the split names
    specimen <- split_names[[i]]$specimen
    drug_bug <- split_names[[i]]$drug_bug
    model <- split_names[[i]]$model
    
    # Create a new list for the specimen if it doesn't exist
    if (!specimen %in% names(models_by_specimen)) {
      models_by_specimen[[specimen]] <- list()
    }
    
    # Create a new list for the drug_bug if it doesn't exist
    if (!drug_bug %in% names(models_by_specimen[[specimen]])) {
      models_by_specimen[[specimen]][[drug_bug]] <- list()
    }
    
    # Check if the list of models for the current drug_bug has fewer than four models
    if (length(models_by_specimen[[specimen]][[drug_bug]]) < 4) {
      # Append the model to the list
      models_by_specimen[[specimen]][[drug_bug]] <- c(models_by_specimen[[specimen]][[drug_bug]])
    }
    
    # Assign the model to the respective drug_bug and model slot
    models_by_specimen[[specimen]][[drug_bug]][[model]] <- models[[i]]
  }
  
  return(models_by_specimen)  # Return the organized list of models
}

# Function to store model fit and comparison in a table
process_model_fit <- function(model_fit, loo_fit) {
  
  # Initialize lists to store results
  loo_compare_results <- list()
  convergence_results <- list()
  
  # Loop through each drug_bug combination
  for (drug_bug in names(model_fit[[1]])) {
    print(drug_bug)
    models <- model_fit[[1]][[drug_bug]]
    names(model_fit[[1]][[drug_bug]])
    
    #loo_objects <- list()
    loo_objects <- loo_fit[grep(drug_bug, names(loo_fit))]
    
    # Perform loo_compare if there are at least two models
    if (length(loo_objects) > 1) {
      loo_results <- loo_compare(loo_objects)
      loo_df <- as.data.frame(loo_results)
      loo_df$drug_bug <- drug_bug
      loo_compare_results[[drug_bug]] <- loo_df
    }
    
    # Check convergence for each model
    for (model_name in names(models)) {
      if (!is.null(model_fit[[1]][[drug_bug]][[model_name]])) {
        #model <- model_fit[[drug_bug]][[model_name]]$result$value$results$model
        model <- model_fit[[1]][[drug_bug]][[model_name]]
        model_summary <- summary(model)
        rhat_values <- model_summary$fixed[, "Rhat"]
        converged <- all(rhat_values <= 1.1)
        divergent <- n_divergent(model)
        
        convergence_results[[paste(drug_bug, model_name, sep = "_")]] <- list(
          drug_bug = drug_bug,
          model = model_name,
          converged = converged,
          rhat_values = rhat_values,
          divergent = divergent
        )
      }
    }
  }
  
  # Combine and clean up LOO results
  all_loo_results <- do.call(rbind, loo_compare_results)
  dbtext = rownames(all_loo_results)
  # model_var
  # all_loo_results$model <- model_var
  all_loo_results$model <- unlist(regmatches(rownames(all_loo_results), gregexpr("model[0-9]+", rownames(all_loo_results))))
  all_loo_results$drug_bug <- sub("\\.model.*", "", all_loo_results$drug_bug)
  #all_loo_results$drug_bug <- rep(drug_bug, length(all_loo_results$drug_bug))
  rownames(all_loo_results) <- NULL
  
  # Select relevant columns for LOO results
  all_loo_results <- all_loo_results %>%
    dplyr::select(drug_bug, model, elpd_diff, se_diff, elpd_loo, se_elpd_loo, p_loo, se_p_loo, looic, se_looic)
  
  # Convert convergence results to a dataframe
  convergence_df <- do.call(rbind, lapply(convergence_results, function(x) {
    data.frame(
      drug_bug = x$drug_bug,
      model = sub("(model\\d+).*", "\\1", x$model), # keep model0, model1 etc, remove anything after the number so to be able to link with the loo results
      converged = x$converged,
      rhat_values = paste(names(x$rhat_values), round(x$rhat_values, 3), collapse = ", "),
      divergent = x$divergent
    )
  }))
  
  # Return both LOO and convergence results
  return(list(loo_results = all_loo_results, convergence_results = convergence_df))
}

# Function do run above functions for each specimen
analyze_specimen_models <- function(specimen, dirDataModeloutput, output_dir) {
  cat("\n Processing:", specimen, "\n")
  
  # Load model RDS files
  rds_files <- list.files(path = file.path(dirDataModeloutput, specimen), pattern = "_fit.rds", full.names = TRUE)
  models <- load_models_from_rds(rds_files)
  model_names <- names(models)
  model_names_new <- update_model_names(model_names)
  names(models) <- model_names_new
  
  # Split and organize
  split_names <- lapply(model_names_new, split_model_names)
  models_by_specimen <- organize_models(models, split_names)
  model_fit <- models_by_specimen
  
  # Load LOO RDS files
  rds_loo <- list.files(path = file.path(dirDataModeloutput, specimen), pattern = "_loo.rds", full.names = TRUE)
  loo_models <- load_models_from_rds(rds_loo)
  loo_names_new <- update_model_names(names(loo_models))
  names(loo_models) <- loo_names_new
  
  # Process model fit and LOO comparison
  results <- process_model_fit(model_fit, loo_models)
  results_c <- left_join(results[[1]], results[[2]], by = c("drug_bug", "model"))
  
  # Add significance
  results_c <- results_c %>%
    group_by(drug_bug) %>%
    mutate(looic_best = looic[elpd_diff == 0]) %>%
    ungroup() %>%
    mutate(
      elpd_significant = abs(elpd_diff) > 2 * se_diff,
      elpd_significant = ifelse(elpd_diff == 0, NA, elpd_significant),
      looic_diff = abs(looic - looic_best),
      looic_significant = looic_diff > 4,
      looic_significant = ifelse(elpd_diff == 0, NA, looic_significant)
    )
  
  # Save outputs
  spec = ifelse(specimen == "BLOOD", "bsi",
                ifelse(specimen=="URINE", "uti",
                       ifelse(specimen=="STOOL", "stool","uro")))
  
  csv_path <- file.path(output_dir, paste0("compare_results_", spec, "_wp.csv"))
  rds_path <- file.path(dirDataModeloutput, "Model_comparison", paste0("Model_fits_", spec, "_wp.rds"))
  write.csv(results_c, csv_path, row.names = FALSE)
  saveRDS(model_fit, rds_path)
  
  cat("Saved outputs for:", specimen, "\n")
  
  # Memory cleanup
  rm(models, model_names, model_names_new,
     split_names, models_by_specimen, model_fit,
     rds_files, rds_loo, loo_models, loo_names_new,
     results, results_c)
}

# RUN FULL PROCESS
#-----------------------------------------------------------------------------------------------------------
specimens <- c("BLOOD", "URINE", "STOOL", "UROGENITAL")

# Ensure output directory exists
output_dir <- file.path(dirDataModeloutput, "Model_comparison")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Loop one by one (slow but memory-safe, i.e. the code is removing the memory taken up by the previous specimen)
# This function replaces the manual running of the different steps for each drug bug
for (specimen in specimens) {
  analyze_specimen_models(specimen, dirDataModeloutput, output_dir)
}

# MANUALLY RUNNING THE PROCESS FOR BSI

# Get all RDS files in the folder
# rds_files_bsi <- list.files(path = paste0(dirDataModeloutput, "/BLOOD"), pattern = "_fit.rds", full.names = TRUE)
# models_bsi <- load_models_from_rds(rds_files_bsi)
# model_names_bsi <- names(models_bsi) 
# models_names_bsi_new <- update_model_names(model_names_bsi)
# names(models_bsi) <- models_names_bsi_new
# 
# #---------------------------------------------------------------------------------------------
# # NOW STORE MODEL OUTPUT IN A LIST PER SPECIMEN TYPE
# #---------------------------------------------------------------------------------------------
#   
# # Apply the function to all model names
# split_names_bsi <- lapply(models_names_bsi_new, split_model_names)
# models_by_specimen_bsi <- organize_models(models_bsi, split_names_bsi)
# 
# # Distribute models into the specific lists based on specimen type
# model_fit_bsi <- models_by_specimen_bsi
# 
# #------------------------------------------------------------------------------------------
# # GET MODEL FIT AND CONVERGENCE INFO AND STORE IN DATAFRAME
# #------------------------------------------------------------------------------------------
# #---------------------------------------------------------------------------------------------
# # !! (IF THIS GIVES 'Error: object 'rds_loo_bsi' not found' TRY COPY THE CODE OF 
# #  the rds_loo_bsi line of code, AND PASTE IN THE CONSOLE, THEN IT SHOULD RUN. !!
# #---------------------------------------------------------------------------------------------
# rds_loo_bsi = list.files(path = paste0(dirDataModeloutput, "/BLOOD"), pattern = "_loo.rds", full.names = TRUE)
# 
# loo_bsi <- load_models_from_rds(rds_loo_bsi)
# 
# loo_names_bsi_new <- update_model_names(names(loo_bsi))
# names(loo_bsi) = loo_names_bsi_new
# 
# # Generate data.frames with comparable model fit
# results_bsi <- process_model_fit(model_fit_bsi, loo_bsi)
# 
# # Save dataframes to CSV files
# results_bsi_c = left_join(results_bsi[[1]], results_bsi[[2]]) 
# 
# # Threshold for significance of different fits between models:  
# # -------------------------------------------------------------------------------------- 
# # ELPD difference (Difference in Expected Log Predictive Density):
# 
# # A common rule of thumb is to compare the magnitude of elpd_diff to its standard error (se_diff).
# #   - If elpd_diff > 2x se_diff, you can be confident that the model with the higher elpd_loo has better predictive performance.
# #   - If elpd_diff < 2x se_diff, the models have similar predictive performance, and the difference is likely not meaningful.
# 
# # looic (Leave-One-Out Information Criterion):
# 
# # looic differences are often interpreted similarly to AIC or DIC. 
# # If the difference in looic values between models is more than 4, it generally indicates a meaningful improvement in model fit. 
# # If the difference is greater than 10, it suggests strong evidence that one model is better than the other.
# 
# # - Difference < 4: The models are essentially indistinguishable in terms of predictive accuracy.
# # - Difference between 4 and 10: Moderate evidence that one model is better.
# # - Difference > 10: Strong evidence that one model is better.
# 
# results_bsi_c = results_bsi_c %>%
#   group_by(drug_bug) %>%
#   mutate(looic_best = looic[elpd_diff == 0]) %>%
#   ungroup %>%
#   mutate(
#     elpd_significant = abs(elpd_diff) > 2 * se_diff,
#     elpd_significant = ifelse(elpd_diff == 0, NA, elpd_significant),
#     looic_diff = abs(looic - looic_best),
#     looic_significant = looic_diff > 4,
#     looic_significant = ifelse(elpd_diff == 0, NA, looic_significant)
#   )
# 
# # Create Model_comparison directory if it does not exist yet
# output_dir <- file.path(dirDataModeloutput, "Model_comparison")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # Store results in a csv file
# write.csv(results_bsi_c, file.path(output_dir, "compare_results_bsi_wp.csv"), row.names = FALSE)
# 
# # Save model fit by specimen type
# saveRDS(model_fit_bsi, file = file.path(dirDataModeloutput, "Model_comparison","Model_fits_bsi_wp.rds"))
# 
# rm(list(c(model_fit_bsi,loo_bsi,models_by_specimen_bsi, split_names_bsi)))
# 







