#------------------------------------------------------------------
# GLASS REPORT - ESTIMATE AMR RATES 
#------------------------------------------------------------------

# RUN ALL MODELS FOR EACH DRUG BUG 
#------------------------------------------------------------------

# Author Esther van Kleef
# Date created: 21 August 2024
# Date last updated: 24 March 2025

rm(list=ls())

# Load R packages
pacman::p_load(brms, future, mgcv, loo, furrr, dplyr)

# Get array task ID and model type from SLURM environment variable
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
MODEL <- as.character(Sys.getenv("MODEL", "model0c"))  # Default to "model0" if not set

# Drug bug numbers to run with this script
dbnum = task_id
ncore = 4
priorchoice = "weakip"
iter = 4000
warmup = 2000

# List of drug bug combinations
dbdata = read.csv("./data/GLASS_final_curated_linked/updated_summary_dbc_longformat.csv", sep=",")

# Local
# dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
# dbdata = read.csv(paste0(dirDataNew, "/updated_summary_dbc_longformat.csv"))
# dirDataModeloutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 R code EV/Cluster"

dbdata$combined = paste0(dbdata$Specimen, "_", dbdata$PathogenName, "_", dbdata$AntibioticName)


# Load in functions
source("./model-code/0_GLASS_functions.R")
source("./model-code/0_multiplot.R")



##############################################################
# LOAD IN DATA
##############################################################
# When running local

# AMR data
# adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_Pop_Country_HAQI_030924_EV.csv"), sep=",")   # Country AMR data
# adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_Country_030924_EV.csv"), sep=",")   # Country AMR data
# adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_Pop_Country_030924_EV.csv"), sep=",")   # Country AMR data
# adataAS = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

adataAC = read.csv("./data/GLASS_final_curated_linked/EI_AMRdtaAC_ANALYSES.csv", sep=",")   # Country AMR data
#adataDM = read.csv("./data/GLASS_final_curated_linked/EI_AMRdtaDM_Country_030924_EV.csv", sep=",")   # Country AMR data
adataAS = read.csv("./data/GLASS_final_curated_linked/EI_AMRdtaINT_ANALYSES.csv", sep=",")   # Country AMR data

#adataNT = read.csv("./data/GLASS_final_curated_linked/EI_AMRdtaINT_Pop_Country_030924_EV.csv", sep=",")   # Country AMR data

adataAS = adataAS %>%
  mutate(AgeCat10 = relevel(factor(AgeCat10), ref="05<14")
  )


# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

#specimen = unique(combinations2022 %>% filter(number %in% dbnum) %>% select(Specimen))
#spec_db = paste0(specimen, "-", min(dbnum), "-", max(dbnum))
spec_db = combinations2022 %>% filter(number %in% dbnum) %>% select(combined) 

###################################################################
# DEFINE DRUG BUG COMBINATIONS TO RUN
###################################################################

drug_bug = combinations2022 %>% filter(number %in% dbnum) %>% select(combined) 
drug_bug = unique(drug_bug$combined)
#print(drugbugrun)


###################################################################
# PREAMBLE
###################################################################


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

# Centering of year
# ---------------------------------------------------------------
# Centering a variable in regression analysis involves subtracting the mean 
# (or another central value, like the median) of the variable from each observation. 
# This transformation alters the scale of the variable but not its relative differences. 
# Centering is a common practice in regression analysis, especially in the context of interaction terms 
# or hierarchical models. Centering can be useful to:
# - Reduce multicollinearity, hence help convergence of the model. 
# In a hierarchical model with random slopes and intercepts, the raw (uncentered) covariates, e.g. for time
# (Year) can cause strong correlations between the random intercept and slope.
# Centering reduces this correlation, as the intercept and slope are now calculated relative to the mean year rather than zero.
# - Interpretation: e.g. when centering year, the other coefficients can be interpreted relative to the 
# mean year. 

# Define model formulas 
# Set 2
model0c_formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 + Year_c | Iso3) + Year_c + poly(st_BCI_million_imp, 2))  
model1c_formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 + Year_c + AgeCat10 | Iso3) + Year_c + AgeCat10 + poly(st_BCI_million_imp, 2))
model2c_formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 + Year_c + AgeCat10 | Iso3) + Year_c + AgeCat10 + Sex + poly(st_BCI_million_imp, 2))
model3c_formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 + Year_c + AgeCat10 | Iso3) + Year_c + AgeCat10 + Sex + AgeCat10*Sex + poly(st_BCI_million_imp, 2))

# Define weakly informative priors
weak_priors <- c(
  prior(normal(0, 2), class = "Intercept"),
  prior(normal(0, 1), class = "b"),  # Weakly informative prior for fixed effects (Year, AgeCat10, Sex)
  prior(normal(0, 0.5), class = "b", coef = "polyst_BCI_million_imp21"), # Stronger prior for testing coverage
  prior(normal(0, 0.5), class = "b", coef = "polyst_BCI_million_imp22"),
  prior(normal(0, 1), class = "sd", lb=0)  #  
)


cor_prior <- c(
  prior(lkj(0, 2), class = "cor", group = "Iso3")
)

cor_prior2 <- c(
  prior(lkj(0, 2), class = "cor", group = "WHORegionCode")
)

# Function to fit a model and return the result
fit_and_save_model <- function(drug_bug, model_formula, subset_data, model_name, warmup, iter, output_dir, prior_default,
                               prior_cor) {
  if(prior_default == "yes"){ 
    prior = get_prior(formula=model_formula, data=subset_data,  family=binomial)
  }else{
    prior = weak_priors
  }
  
  if(prior_default == "no" & prior_cor == "yes"){
    prior = c(weak_priors, cor_prior)
  }
  
  model <- brm(
    model_formula,
    data = subset_data,
    family = binomial(),
    iter = iter,  # Number of iterations specified here
    warmup = warmup,
    chains = 4,
    cores = 4,
    init = "random", 
    prior = prior,
    control = list(max_treedepth = 15),
    set.seed(123)
  )
  
  # Save the model to disk
  print(paste0("Saving model to: ", file.path(output_dir, paste0(drug_bug, "_", model_name, "_fit.rds"))))
  
  saveRDS(model, file = file.path(output_dir, paste0(drug_bug, "_", model_name, "_fit.rds")))
  
  # Compute LOO
  loo_result <- loo(model)
  saveRDS(loo_result, file = file.path(output_dir, paste0(drug_bug, "_", model_name, "_loo.rds")))
  
  converged <- ifelse(length(which(summary(model)$fixed$Rhat < 1.05)) == 0, "NO", "YES")
  results <- list(
    model = model, 
    est_fixed = as.data.frame(summary(model)$fixed), 
    est_ran = as.data.frame(summary(model)$random),
    converged = as.data.frame(cbind(drug_bug, converged)), 
    loo = loo_result
  )
  # Return results to the caller (instead of directly modifying model_fits or loo_model)
  return(list(drug_bug = drug_bug, model_name = model_name, results = results, loo_result = loo_result))
}


subset_data <- adataAS %>% filter(combined == drug_bug) %>%
    filter(!Year %in%c(2016,2017)) %>%
    select(WHORegionCode, Iso3, Year, PathogenName, AntibioticName, Specimen, 
           AgeCat10, s_AgeCat10, Sex, Resistant, InterpretableAST, st_BCI_million_imp, combined)

subset_data$Year_c = as.numeric(scale(subset_data$Year, center=TRUE, scale=FALSE))
  


specimen = strsplit(drug_bug, "-")[[1]][1]

output_dir <- paste0("./model-output/all_one_model_", priorchoice, "_set1_centered/", specimen)

# Local
#output_dir <- paste0(dirDataModeloutput)  

# Select the model formula based on the MODEL variable
model_formula <- switch(MODEL,
                        "model0c" = model0c_formula,
                        "model1c" = model1c_formula,
                        "model2c" = model2c_formula,
                        "model3c" = model3c_formula,
			"model4c" = model4c_formula,
                        stop("Unknown model specified"))

# Check that model_formula is valid
if (is.null(model_formula)) stop("No valid model formula was selected")


fit = fit_and_save_model(drug_bug, 
                         model_formula, 
                         subset_data, 
                         MODEL, 
                         warmup = warmup,
                         iter = iter, 
                         output_dir,
                         prior_default="no",
                         prior_cor = "no")

saveRDS(fit, file = paste0("./model-output/all_", priorchoice,"/models_combined/", spec_db, ".rds"))
