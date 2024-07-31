#################################################
# GLASS REPORT - Data wrangling
#################################################

# Author: Esther van Kleef
# Date created: 8 July 2024
# Date latest update: 8 July 2024
rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, RColorBrewer,
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, purrr, brms, cowplot)

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
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")       # Population data
pdataF_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexMale_180724_EV.xlsx"))
pdataM_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexMale_180724_EV.xlsx"))

cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")   # Country data
#sdata = read.csv(paste0(dirDataNew, "/EI_SurveillanceSites_180724_EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data
idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data

haqidata = read.csv(paste0(dirDataNew, "/HAQI/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_180724_EV.csv"), sep=",")   # Country AMR data

#############################################################
# CLEAN DATA
#############################################################

# Country data
#############################################################

# Countries that reported AST to GLASS
creport = adataAC %>% group_by(Iso3, Year) %>%
  summarise(n_ast = sum(SpecimenIsolateswithAST, na.rm=T)) %>% 
  filter(Year == 2021)

cdata = cdata %>%mutate(
  AMR_GLASS_AST = 
  case_when(
    Iso3 %in% creport$Iso3 ~ "Yes", 
    is.na(EnrolledAMR) ~ NA,
    TRUE ~ "No"),
  EnrollmentDateAMR = as.Date(EnrollmentDateAMR, format= "%d/%m/%Y"),
  EnrollmentDateAMC = as.Date(EnrollmentDateAMC, format= "%d/%m/%Y"),
  EnrollmentYearAMR = year(EnrollmentDateAMR),
  EnrollmentYearAMC = year(EnrollmentDateAMC),
) 

cdata = cdata[order(cdata$Iso3),] %>%
  mutate(
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
      TRUE ~ CountryTerritoryArea)
  )

##############################################################
# Population data by age and sex
##############################################################

# Estimates are per 1000, so multiply by 1000

# Female
pdataF_raw[,4:length(names(pdataF_raw))] = round(sapply(pdataF_raw[,4:length(names(pdataF_raw))], function(x)
                                                  x*1000),0)
names(pdataF_raw)[4:length(names(pdataF_raw))] = paste0("F_", names(pdataF_raw[4:length(names(pdataF_raw))]))
names(pdataF_raw)[4:length(names(pdataF_raw))] = gsub("-", "<", names(pdataF_raw[4:length(names(pdataF_raw))]))

# Convert from wide to long format
pdataDM_F <- pdataF_raw %>%
  pivot_longer(
    cols = starts_with("F_"),  # Select columns to pivot
    names_to = "DemographicsOrigin",      # Name for the new column that will hold the names of the original columns
    values_to = "TotalPopulation"          # Name for the new column that will hold the values
  )


# Male
pdataM_raw[,4:length(names(pdataM_raw))] = round(sapply(pdataM_raw[,4:length(names(pdataM_raw))], function(x)
  x*1000),0)

names(pdataM_raw)[4:length(names(pdataM_raw))] = paste0("M_", names(pdataM_raw[4:length(names(pdataM_raw))]))
names(pdataM_raw)[4:length(names(pdataM_raw))] = gsub("-", "<", names(pdataM_raw[4:length(names(pdataM_raw))]))

# Convert from wide to long format
pdataDM_M <- pdataM_raw %>%
  pivot_longer(
    cols = starts_with("M_"),  # Select columns to pivot
    names_to = "DemographicsOrigin",      # Name for the new column that will hold the names of the original columns
    values_to = "TotalPopulation"          # Name for the new column that will hold the values
  )


pdataDM = rbind(pdataDM_F, pdataDM_M) %>%
  mutate(Id = paste0("Id", Iso3, Year),
         PopulationSource = "United Nations",
         PopSourceComments = "WPP2024July") %>%
  select(Iso3, Id, Year, DemographicsOrigin, TotalPopulation, PopulationSource, PopSourceComments)

# Combine all from French territory and Female and Male data
pdataDM = pdataDM %>% group_by(DemographicsOrigin, Year) %>%
   mutate(
     TotalPopulation = ifelse(Iso3 =="FRA", sum(TotalPopulation[Iso3 %in% c("GUF","GLP","MTQ","MYT","REU","FRA")]), TotalPopulation)
   ) %>% 
  filter(!Iso3 %in% c("GUF","GLP","MTQ","MYT","REU")
         )

# Export data
write.csv(pdataDM, paste0(dirDataNew, "/EI_PopdtaDM_180724_EV.csv"))

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

# Implementation data
#############################################################

# Clean names
names(idata) = idata[3,]
names(idata)[1:17] = idata[4,c(1:17)] 
names(idata)[7:9] = c("NLR_YES","NLR_NO","NLR_UNK")
names(idata)[10:12] = c("NLR_EQA_YES","NLR_EQA_NO","NLR_EQA_UNK")
names(idata)[13:17] = c("LAB_EUCAST","LAB_CLSI","LAB_BOTH", "LAB_OTHER", "LAB_UNK")
names(idata) = tolower(names(idata))

# Remove first rows
idata = idata[5:length(idata$period),]

# Make names in line with previous data, so we can reproduce code
unique(idata_old$Iso3)  # Org unit = Iso3 in historical data
unique(idata$period)    # Period = Year

idata = idata %>%
  mutate(AMR_NCC = 
           case_when(ncc_yes_=="Yes" ~ "Established", 
                     ncc_no =="Yes"  ~ "Not established",
                     ncc_in_progress == "Yes"  ~ "Establishment in progress",
                     ncc_yes_== "" & ncc_no== "" &  ncc_in_progress== "" ~ "Not reported",
                     TRUE ~ NA),
         AMR_NLR = 
            case_when(nlr_yes=="Yes" ~ "Established", 
                      nlr_no =="Yes"  ~ "Not established",
                      nlr_unk == "Yes"  ~ "Unknown",
                      nlr_yes== "" & nlr_no== "" &  nlr_unk== "" ~ "Not reported",
                      TRUE ~ NA),
         EQA_to_NRL = 
           case_when(nlr_eqa_yes=="Yes" ~ "Established", 
                     nlr_eqa_no =="Yes"  ~ "Not established",
                     nlr_eqa_unk == "Yes"  ~ "Unknown",
                     nlr_eqa_yes== "" & nlr_eqa_no== "" &  nlr_eqa_unk== "" ~ "Not reported",
                     TRUE ~ NA),
         AMR_AST_standards = 
           case_when(lab_clsi =="Yes" ~ "CLSI", 
                     lab_eucast =="Yes"  ~ "EUCAST",
                     lab_both == "Yes"  ~ "CLSI_EUCAST",
                     lab_other== "Yes" ~ "Other",
                     lab_unk == "Yes" ~ "Unknown",
                     TRUE ~ NA)
  ) %>%
  rename(
    Iso3 = `org unit *`,
    Year = "period"
  ) %>% 
  mutate(
    AMR_GLASS_AST = ifelse(Iso3 %in% creport$Iso3, "Yes", "No")
  )

# table(idata$AMR_NCC,idata$ncc_yes_)
# table(idata$AMR_NCC,idata$ncc_no)
# table(idata$AMR_NCC,idata$ncc_in_progress)
# 

# Remove merged variables
idata = idata %>% select(-c(ncc_yes_, ncc_no, ncc_in_progress, nlr_yes, nlr_no, nlr_unk,
                            nlr_eqa_yes, nlr_eqa_no, nlr_eqa_unk, lab_clsi,lab_eucast,
                            lab_both, lab_other, lab_unk, options, amr_glass_data))
names(idata)

# Change characters to numbers
idata[,c(4:14)] = sapply(idata[,c(4:14)], function(x) as.numeric(x))

describe(idata)
sapply(idata, function(x) class(x))

# Merge implementation and country data
idata_country = merge(cdata,idata,by=c("Iso3"), all = T) %>%
  rename(AMR_GLASS_AST = "AMR_GLASS_AST.x") %>%
  select(-c(AMR_GLASS_AST.y))


# Export data
write.csv(idata, paste0(dirOutput, "/EI_Implementationdta_080724_EV.csv"))
write.csv(idata_country, paste0(dirOutput, "/EI_ImplementationCdta_080724_EV.csv"))

