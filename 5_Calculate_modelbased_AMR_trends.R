############################################################################
# CALCULATE REGIONAL AMR SLOPES AND TRENDS
############################################################################

# AUTHOR: ESTHER VAN KLEEF
# DATE CREATED:
# DATE LASTE UPDATED: 3 March 2026

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

#adataAS = left_join(adataAS, d, by=c("Specimen", "PathogenName"))

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


#----------------------------------------------------------------------------------
# LOAD IN MODEL OUTPUTS
#----------------------------------------------------------------------------------

# BSI
model_fit_bsi = readRDS(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/model_fits_bsi_wp.rds"))
results_bsi = read.csv(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/compare_results_bsi_wp.csv"))

# UTI
model_fit_uti = readRDS(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/model_fits_uti_wp.rds"))
results_uti = read.csv(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/compare_results_uti_wp.csv"))

# STOOL
model_fit_stool = readRDS(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/model_fits_stool_wp.rds"))
results_stool = read.csv(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/compare_results_stool_wp.csv"))

# GONORRHOEA
model_fit_uro = readRDS(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/model_fits_uro_wp.rds"))
results_uro = read.csv(paste0(dirOutput,"/AMR_prevalence/weakip/Model_comparison/compare_results_uro_wp.csv"))

#----------------------------------------------------------------------------------
# READ IN BEST FITTING MODEL
#----------------------------------------------------------------------------------
model_best_wp = read.csv(file.path(dirOutput,"/AMR_prevalence/weakip/Model_comparison/best_model_fit_all.csv"))

model_best_wp$drug_bug = gsub("Co trimoxazole","Co-trimoxazole", model_best_wp$drug_bug)
model_best_wp$combined = gsub("Co trimoxazole","Co-trimoxazole", model_best_wp$combined)

unique(model_best_wp$drug_bug)
unique(model_best_wp$combined)


#----------------------------------------------------------------------------------
# CALCULATE REGIONAL AMR SLOPES AND TRENDS
#----------------------------------------------------------------------------------

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
                                                 "BLOOD-Salmonella spp.-Ciprofloxacin",
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
         median_slope_p = (exp(median_slope)-1)*100,
         Q2.5_p = (exp(Q2.5)-1)*100,
         Q97.5_p = (exp(Q97.5)-1)*100,
         drug_bug = ifelse(drug_bug=="Staphylococcus aureus-Methicillin-resistance", "Staphylococcus aureus-Methicillin resistance", 
                           ifelse(drug_bug == "Escherichia coli-Third-generation cephalosporins", "Escherichia coli-Third generation cephalosporins", drug_bug)),
         combined = paste0(specimen,"-",drug_bug),
         specimen = factor(specimen, levels=c("BLOOD", "STOOL", "STOOL", "UROGENITAL"),
                           labels = c("Bloodstream", "Gastrointestinal", "Urinary tract", "Gonorrhoea"))
  ) %>%
  separate(combined, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")

# IDENTIFY IF AT LEAST 5 COUNTRIES PER REGION AND ADD NUMBER OF ISOLATES
#----------------------------------------------------------------------------

# DECIDED TO REPORT ISOLATES AND COUNTRIES OF ALL YEARS AND COUNTRIES IN ANNEX 8 (now Annex 7 in report)
cta_w_prev <- read.csv(paste0(dirOutput, "AMR_prevalence", "weakip", "Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))
#slopes_all = read.csv(file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_prevalence_slope_UPDATED.csv"))
#slopes_all = slopes_all[2:10]

db = unique(paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName))
db = c(db, "BLOOD-Staphylococcus aureus-Methicillin-resistance","BLOOD-Escherichia coli-Third-generation cephalosporins")

# N ISOLATES OF ALL COUNTRIES FROM 2018-2023
n_isolates <- cta_w_prev %>% filter(data_submitted=="Yes", combined!="BLOOD-Escherichia coli-Ampicillin") %>%
  group_by(WHORegionCode, combined) %>%
  summarise(n_isolates = sum(InterpretableAST, na.rm = T))

n_isolates_global <- cta_w_prev %>% filter(data_submitted=="Yes", combined!="BLOOD-Escherichia coli-Ampicillin") %>%
  group_by(combined) %>%
  summarise(n_isolates = sum(InterpretableAST, na.rm = T))%>%
  mutate(WHORegionCode = "Global") %>%
  select(WHORegionCode, combined, n_isolates)

n_isolates_total = rbind(n_isolates, n_isolates_global) 
d = cdata%>%select(c(WHORegionCode, WHORegionName)) %>% distinct()
d = rbind(d,data.frame(cbind(WHORegionCode="Global", WHORegionName="Global")))  
  
n_isolates_total = left_join(n_isolates_total, d) %>%
  ungroup()%>%
  select(-c(WHORegionCode)) %>%
  filter(combined%in%db) #%>%

unique(n_isolates_total$combined)  

# YEARS OF ALL COUNTRIES FROM 2018-2023
nyears_cta <- adataAC %>%
  filter(InterpretableAST>10 & combined %in% db & Year>2017) %>%
  group_by(WHORegionName, Iso3,combined) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 0) #%>%
#pull(Iso3)

nyears_cta_3y <- adataAC %>%
  filter(InterpretableAST>10 & combined %in% db & Year>2017) %>%
  group_by(WHORegionName, Iso3,combined) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 2) 

#unique(nyears_cta$combined)

# Check if same number of countries with the cta prevalence table
# nyears_cta2 <- cta_w_prev %>%
#   filter(data_submitted=="Yes", combined%in%db) %>%
#   group_by(WHORegionCode, Iso3,combined) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   filter(n > 2) %>%
#   mutate(combined = ifelse(combined=="BLOOD-Escherichia coli-Third generation cephalosporins", "BLOOD-Escherichia coli-Third-generation cephalosporins",combined))
 
#d = left_join(nyears_cta,nyears_cta2, by=c("Iso3","combined"))
#d$n.x - d$n.y # Yes same number of countries

# ALL
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

# COUNTRIES WITH 3YEAR data
ncountry_region_3y = nyears_cta_3y %>%
  group_by(WHORegionName, combined) %>%
  summarise(n_3y = n())

ncountry_global_3y = nyears_cta_3y %>%
  group_by(combined) %>%
  summarise(n_3y = n()) %>%
  mutate(WHORegionName = "Global") %>%
  select(WHORegionName, combined,n_3y)

ncountry_3y = rbind(ncountry_region_3y, ncountry_global_3y)
ncountry_3y$combined = ifelse(ncountry_3y$combined=="BLOOD-Staphylococcus aureus-Methicillin-resistance", "BLOOD-Staphylococcus aureus-Methicillin resistance", 
                           ifelse(ncountry_3y$combined == "BLOOD-Escherichia coli-Third-generation cephalosporins", "BLOOD-Escherichia coli-Third generation cephalosporins", ncountry$combined))

ncountry = left_join(ncountry, ncountry_3y)

n_isolates_total$combined = ifelse(n_isolates_total$combined=="BLOOD-Staphylococcus aureus-Methicillin-resistance", "BLOOD-Staphylococcus aureus-Methicillin resistance", 
                           ifelse(n_isolates_total$combined == "BLOOD-Escherichia coli-Third-generation cephalosporins", "BLOOD-Escherichia coli-Third generation cephalosporins", n_isolates_total$combined))


slopes_all$combined = paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName)

d = cdata%>%select(c(WHORegionCode, WHORegionName)) %>% distinct()
d = rbind(d,data.frame(cbind(WHORegionCode="Global", WHORegionName="Global")))  

n_isolates_total = left_join(n_isolates_total, d) %>%
  ungroup()%>%
  select(-c(WHORegionCode)) %>%
  filter(combined%in%db) 

unique(n_isolates_total$combined)  
unique(ncountry$combined)  

# ISOLATES FROM COUNTRIES WITH 3 YEARS OF DATA
data3y = left_join(nyears_cta_3y, adataAC %>%
                     filter(InterpretableAST>10 & combined %in% db & Year>2017))

n_isolates_3y <- data3y %>%
  group_by(WHORegionName, combined) %>%
  summarise(n_isolates_3y = sum(InterpretableAST, na.rm = T))

n_isolates_global_3y <- data3y %>%  group_by(combined) %>%
  summarise(n_isolates_3y = sum(InterpretableAST, na.rm = T))%>%
  mutate(WHORegionName = "Global") %>%
  select(WHORegionName, combined, n_isolates_3y)

n_isolates_total_3y = rbind(n_isolates_3y, n_isolates_global_3y) 

n_isolates_total_3y = n_isolates_total_3y %>%
  mutate(combined = ifelse(combined=="BLOOD-Escherichia coli-Third-generation cephalosporins", "BLOOD-Escherichia coli-Third generation cephalosporins",combined)
  )
  
unique(n_isolates_total_3y$combined)  

nisolates = left_join(n_isolates_total, n_isolates_total_3y) %>%
  select(c(WHORegionName, combined, n_isolates, n_isolates_3y))

t0 = left_join(slopes_all, ncountry)

t0 = left_join(t0, nisolates)

# IF READING IN RESULTS (i.e. not running the results in the above)
#-------------------------------------------------------------
#t0 <- read.csv(paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_prevalence_slope.csv"))
#t0$combined = t0$combined = paste0(t0$Specimen, "-",t0$PathogenName, "-", t0$AntibioticName)
#t0 = left_join(t0, n_isolates_total, by = c("WHORegionName", "combined"))
#write.csv(t0, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_prevalence_slope.csv"))

#t0 = left_join(t0, n_isolates_total)
table(t0$n_isolates, useNA="always") # No NAs, all matched
table(t0$n, useNA="always") # No NAs

t0$Significant = ifelse(t0$Q2.5_p>=1&t0$n_3y>5|t0$Q97.5_p<=-1&t0$n_3y>5, "Yes", "No")

# Keep countries with data submitted in 2023
# iso_with_2023_data <- adataAC %>%
#   filter(InterpretableAST>10, combined == combined & Year == 2023) %>%
#   pull(Iso3)

t1 <- t0%>%
  dplyr::select(drug_bug, WHORegionName, Specimen, PathogenName, AntibioticName, mean_slope_p,median_slope_p, Q2.5_p,Q97.5_p, Significant, n,n_3y, n_isolates,n_isolates_3y) %>%
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
      paste0(format(round(median_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")*"),
      paste0(format(round(median_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")")
    )
  ) %>%
  ungroup()

# FIGURE 4.11
#---------------------------------------------------------------------------------------

# Visualise slope change (Pathogen)
# Flag cells with a star
# Correctly assign linetype for starred tiles based on credible intervals
t1 <- t1 %>%
  mutate(
    has_star = grepl("\\*", text_with_star),
    linetype = ifelse(has_star & Q2.5_p >= 1&t1$n_3y>5, "solid",  # Solid for significant positive effect
                      ifelse(has_star & Q97.5_p <= -1&t1$n_3y>5, "dotted", NA))  # Dotted for significant negative effect
  )

# Updated ggplot with bold and styled borders around starred 
p1 = ggplot(t1, aes(x = region_labels, y = factor(custom_label))) +
  geom_tile(aes(fill = median_slope_p), linewidth = 0.8) +
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
                       "Gonorrhoea" = "Urogenital\n"
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
  geom_tile(aes(fill = median_slope_p), linewidth = 0.8) +
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
  scale_linetype_manual(values = c("solid" = "solid", "dotted" = "dashed")) +  # Define line styles
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
                       "Gonorrhoea" = "Urogenital\n"
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



ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11_trend_regional_amr_UPDATED.png"), 
       plot = p1,
       device = "png",
       dpi = 300,
       width = 10, height = 12)  

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Provisional/Figure_4.11_trend_regional_amr_BSI_nodecline_UPDATED.png"), 
       plot = p2,
       device = "png",
       dpi = 300,
       width = 11, height = 6)  


#write.csv(x=final_results%>%select(-c(overlap)), file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_prevalence_allyears.csv"))
write.csv(x=t1, file = paste0(dirOutput, "/Chapter_3/Ch3_summary_stats/Figure_3.12_REGION_prevalence_slope_UPDATED_2.csv"))

final_results = read.csv(file = paste0(dirOutput, "/Chapter_3/Ch3_summary_stats/Figure_4.12_REGION_trends_allyears.csv"))


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
# 
# ggsave(filename = paste0(dirOutput, "/Chapter_3/Ch3_Figures/Provisional/Figure_3.12_trend_kpn_UPDATED.png"), 
#        plot = combined_plot,
#        device = "png",
#        dpi = 300,
#        width = 14, height = 8)  

  
#write.csv(x=final_results, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_trends_allyears.csv"))
#write.csv(x=final_results2023, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.7_REGION_trends_2023change.csv"))
