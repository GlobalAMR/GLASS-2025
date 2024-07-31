#################################################
# GLASS REPORT - ESTIMATE AMR RATES
#################################################

# Author: Esther van Kleef
# Date created: 5 July 2024
# Date latest update: 31 July 2024

rm(list=ls())
# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet,
               meta, metafor,PHEindicatormethods)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables"
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
#sdata = read.csv(paste0(dirDataNew, "/EI_SurveillanceSites_180724_EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data
idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/HAQI/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_180724_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read_excel(paste0(dirDataNew, "/updated_summary_dbc_longformat.xlsx"), sheet=1)

# rrates 2021
rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
  Q1 = as.numeric(Q1),
  Q3 = as.numeric(Q3),
  median = as.numeric(median)
)

##############################################################
# PREAMBLE
##############################################################

# Country data
##############################################################
# Remove empty columns
#cdata = cdata %>% select(-c(X, X.1,X.2))
cdata = cdata[order(cdata$Iso3),] %>%
  mutate(
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
      TRUE ~ CountryTerritoryArea)
  )

##############################################################
# Population data by age and sex
##############################################################

##############################################################
# Universal health coverage data
##############################################################
# Source:
# https://www.who.int/publications/i/item/9789240080379
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-index-of-service-coverage


# Health Quality and Safety Index (IHME data)
haqidata2019 <- haqidata %>% filter(year_id == 2019, indicator_name=="HAQ Index", haq_index_age_type=="Overall")
haqidata2019 <- haqidata2019[order(haqidata2019$location_name),] %>%
  mutate(
    CountryTerritoryArea = location_name,
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "Netherlands" ~ "Netherlands (Kingdom of the)",
      CountryTerritoryArea == "Türkiye" ~ "Turkey",
      CountryTerritoryArea == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
      TRUE ~ CountryTerritoryArea)
  )

# Few left               
#"Palestine" = "not recognised by WHO"
#"Taiwan (Province of China)" = "not regonised by WHO, probably listed as China"
#"United States Virgin Islands" = "probably listed as US?"

haqidata2019 <- left_join(haqidata2019, cdata, by="CountryTerritoryArea")
unique(haqidata2019$CountryTerritoryArea[is.na(haqidata2019$Iso3)])

# AMR data
##############################################################
# Link population data
adataAC = left_join(adataAC, pdata, by=c("Iso3", "Year"))

# Link country data so we can join HAQ data
adataAC = left_join(adataAC, cdata, by=c("Iso3"))
unique(adataAC$CountryTerritoryArea)

# Link HAQ data
adataAC = left_join(adataAC, haqidata2019)

##############################################################
## DESCRIPTIVES
##############################################################

# Plot HAQ index vs raw isolates


# GLASS AMR Country data
##############################################################

# Countries that reported
creport = unique(adataAC$Iso3[which(adataAC$SpecimenIsolateswithAST>1)])

# GLASS Surveillance sites
##############################################################



# GLASS Implementation data
##############################################################
names(idata)

# Surveillance indicators summary - by AST reporting y/n
im_table1 = table1(~ factor(AMR_NCC)+ 
                     factor(AMR_NLR)+
                     factor(EQA_to_NRL)+
                    factor(AMR_AST_standards)+
                   amr_amr_acute_care_number+
                   amr_amr_hospitals_number+
                   amr_amr_inpatient_adm_number+
                   amr_amr_inpatient_day_number+
                   amr_amr_outpatient_cons_number+
                   amr_glass_acute_care_number+
                   amr_glass_hospitals_number+
                   amr_glass_inpatient_adm_number+
                   amr_glass_inpatient_day_number+
                   amr_glass_outpatient_cons_number+
                   lab_number_data_call+
                   local_lab_eqa_number_data_call| factor(AMR_GLASS_AST), data=idata_country%>%filter(!is.na(EnrolledAMR)&EnrollmentYearAMR<"2023"))

im_table1 # So 87 countries report to GLASS among those that have also filled out the implementation survey
table(idata_country$AMR_GLASS_AST,useNA="always") # 87 countries reported to GLASS at least one isolate with AST

# Surveillance indicators summary - by GLASS enrollment == Yes and region
im_table1_regionGLASS = table1(~ factor(AMR_NCC)+ 
                            factor(AMR_NLR)+
                            factor(EQA_to_NRL)+
                            factor(AMR_AST_standards)+
                            amr_amr_acute_care_number+
                            amr_amr_hospitals_number+
                            amr_amr_inpatient_adm_number+
                            amr_amr_inpatient_day_number+
                            amr_amr_outpatient_cons_number+
                            amr_glass_acute_care_number+
                            amr_glass_hospitals_number+
                            amr_glass_inpatient_adm_number+
                            amr_glass_inpatient_day_number+
                            amr_glass_outpatient_cons_number+
                            lab_number_data_call+
                            local_lab_eqa_number_data_call| factor(WHORegionCode), data=idata_country%>%filter(!is.na(EnrolledAMR)&EnrollmentYearAMR<"2023"))

im_table1_regionGLASS # So 116 countries report to GLASS in 2022 (2023 data call)

# Surveillance indicators summary - by AST reporting == Yes and region
im_table1_regionAST = table1(~ factor(AMR_NCC)+ 
                     factor(AMR_NLR)+
                     factor(EQA_to_NRL)+
                     factor(AMR_AST_standards)+
                     amr_amr_acute_care_number+
                     amr_amr_hospitals_number+
                     amr_amr_inpatient_adm_number+
                     amr_amr_inpatient_day_number+
                     amr_amr_outpatient_cons_number+
                     amr_glass_acute_care_number+
                     amr_glass_hospitals_number+
                     amr_glass_inpatient_adm_number+
                     amr_glass_inpatient_day_number+
                     amr_glass_outpatient_cons_number+
                     lab_number_data_call+
                     local_lab_eqa_number_data_call| factor(WHORegionCode), data=idata_country%>%filter(!is.na(AMR_GLASS_AST)& AMR_GLASS_AST=="Yes" &EnrollmentYearAMR<"2023"))

im_table1_regionAST # So 87 countries report to GLASS among those that have also filled out the implementation survey

write.table(im_table1, paste0(dirOutput,"/Descriptive/im_2023_table1.csv"), col.names = T, row.names=F, append= F, sep=';')
write.table(im_table1_regionGLASS, paste0(dirOutput,"/Descriptive/im_regionGLASS_2023_table1.csv"), col.names = T, row.names=F, append= F, sep=';')
write.table(im_table1_regionAST, paste0(dirOutput,"/Descriptive/im_regionAST_2023_table1.csv"), col.names = T, row.names=F, append= F, sep=';')

###############################################################
# ESTIMATES BASED ON Modelled resistance rates 
###############################################################

# First trial with just E. coli 
#combinations2022ec_BLOOD = combinations2022 %>% filter(PathogenName%in% c("Escherichia coli")&
#                                                       Specimen=="BLOOD")
ecolidata = adataAC %>% filter(PathogenName == "Escherichia coli" & Year == 2021 & InReport=="Yes")
unique(ecolidata$combined) # Count as 27 so all should be there

# Create list of data.frames
data_sets = list()

for(i in 1:length(unique(ecolidata$combined))) {
  d = ecolidata # Change to data to use
  data_subset <- adataAC %>% 
    filter(Specimen == d$Specimen[i],
           PathogenName == d$PathogenName[i],
           AntibioticName == d$AntibioticName[i],
           Year == 2021) %>%
    group_by(Iso3) %>%
    reframe(AMRrate = Resistant/InterpretableAST,
            Specimen = d$Specimen[i],
            PathogenName = d$PathogenName[i],
            AntibioticName = d$AntibioticName[i],
            Resistant = Resistant,
            InterpretableAST = InterpretableAST,
            TotalPopulation = TotalPopulation,
            BCI_pop100000 = InterpretableAST/TotalPopulation*100000,
            Year = 2021)
  print(paste0("Num", i))
  #print(data_subset[1,])
  data_sets[[i]] = data_subset
  #print(c(i,combinations2022$Specimen[i], combinations2022$PathogenName[i],combinations2022$AntibioticName[i]))
  
  #print(head(data_subset))
  
}

# For URINE e. coli doripenem only 10 countries provided isolates
# We need to make a distinction between no resistance found vs not tested, can we?
# Yes, all interpretable ASTs should be ASTs including the 100% susceptible ones

# Fit the model to each dataset
##########################################################
# First fit model to the first dataset
#fit <- fit_amr_model(data_sets[[1]], formula, priors)

# Define the model formula
formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 | Iso3)) #
#formula2 <- bf(Resistant | trials(InterpretableAST) + weights(TotalPopulation) ~ 1 + (1 | Iso3)) # This is weighting by population size

# Define the priors
priors <- c(
  prior(normal(0, 1), class = "Intercept"),    # Prior for the fixed effects (including intercept)
  prior(cauchy(0, 1), class = "sd")            # Prior for the standard deviation of the random effect
)

priors

# Fit the model using brms
fit <- brm(formula, 
           data = data_sets[[1]], 
           family = binomial(), 
           prior = priors, 
           chains = 6, 
           cores = 6, 
           iter = 2000)

# Then update this fit using new data, so model does not need to recompile for each drug bug
model_fits <- lapply(data_sets, function(x) update(fit, newdata = x,
                                                   chains=6, cores=6))

# Extract summaries from each model fit
model_summaries <- lapply(model_fits, function(x) get_fit_model(model_fit=x))

# Plot the posterior means of the intercepts
#################################################################

# Extract the summary of the first model for plotting
model_estimates = NULL

for(i in 1:length(data_sets)){
  d = ecolidata # Change to type of bug-drug combinations to look at
  model_summaries[[i]]$summary$Iso3 <- sub("r_Iso3\\[(.*),Intercept\\]", "\\1", model_summaries[[i]]$summary$Country)
  model_summaries[[i]]$summary$Specimen <- d$Specimen[i]
  model_summaries[[i]]$summary$PathogenName <- d$PathogenName[i]
  model_summaries[[i]]$summary$AntibioticName <- d$AntibioticName[i]
  
  model_summaries[[i]]$summary <- left_join(model_summaries[[i]]$summary,cdata %>% select(Iso3, WHORegionCode, WHORegionName))
  
  # Create overall and regional values
  total = model_summaries[[i]]$summary %>%
    summarise(
      med50 = quantile(med50, probs = 0.5),
      low25 = quantile(low25, probs = 0.25),
      high75 = quantile(high75, probs = 0.75),
      Specimen = unique(Specimen),
      PathogenName = unique(PathogenName),
      AntibioticName = unique(AntibioticName),
      CTA_total = length(unique(Iso3)),
      Total = "Yes"
    )
  #print(total)
  data_total = d %>% filter(combined == d$combined[i]) %>%
    summarise(
      rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
      rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
      rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
    )
  
  total = cbind(total,data_total)
  
  region = model_summaries[[i]]$summary %>% group_by(WHORegionCode) %>%
    summarise(
      med50 = quantile(med50, probs = 0.5),
      low25 = quantile(low25, probs = 0.5),
      high75 = quantile(high75, probs = 0.5),
      Specimen= unique(Specimen),
      PathogenName = unique(PathogenName),
      AntibioticName = unique(AntibioticName),
      CTA_total = length(unique(Iso3)),
      Total = "No"
    )
  
  data_region = d %>% filter(combined == d$combined[i]) %>% 
    group_by(WHORegionCode) %>%
    summarise(
      rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
      rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
      rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
    )
  region = left_join(region, data_region)
  total$WHORegionCode = "1.ALL"
  #total = total[,names(total.region)]
  total.region = rbind(total,region)
  model_summaries[[i]]$summary.overall = total.region
  model_estimates = rbind(model_estimates,total.region)
}

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
