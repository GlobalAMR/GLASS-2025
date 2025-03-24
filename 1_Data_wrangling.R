#################################################
# GLASS REPORT - Data wrangling
#################################################

# Author: Esther van Kleef
# Date created: 8 July 2024
# Date latest update: 14 March 2025
rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, RColorBrewer,
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, purrr, brms, cowplot)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"
#dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/2021/"

# Load in functions
source("./0_GLASS_functions.R")
source("./0_multiplot.R")

##############################################################
# LOAD IN DATA
##############################################################

# Population data
#Population file (EI_PopDta) was reconstituted de-novo
# All population data for 2016-2023 taken from World Population Prospects 2024 (mid-year [1 July] estimates; 2025 prospect not yet out)
# https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Population

# All countries, including Cyprus, use UN data for all years (2016-2023)
# Population for France is the sum of UN populations in "GUF","GLP","MTQ","MYT","REU","FRA". This applies to each year (2016-2023)

pdata = read.csv(paste0(dirDataNew, "/GLASS_final_curated/EI_Popdta_110325_EV.csv"), sep=",")       
pdataF_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexFemale_180724_EV.xlsx"))
pdataM_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexMale_180724_EV.xlsx"))

# Country data
cdata = read.csv(paste0(dirDataNew, "/GLASS_final_curated/EI_Countrydta_110325_EVK.csv"), sep=",")

# Surveillance implementation data
idata = readxl::read_xlsx(paste0(dirDataNew,"/GLASS_final_raw/Implementation Questionnaire_110325_EV.xlsx"))      

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/GLASS_final_curated/EI_AMRdtaAC_110325_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/GLASS_final_curated/EI_AMRdtaDM_110325_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/GLASS_final_curated/EI_AMRdtaINT_110325_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/updated_summary_dbc_longformat.csv"))
dbdatamodel = read.csv("./models_and_scenarios_to_run.csv")

# Create scenario file in long format
dbdatamodel_long <- pivot_longer(dbdatamodel, cols = starts_with("Model"), names_to = "Model", values_to = "Run")
dbdatamodel_long <- dbdatamodel_long %>% filter(Run == 1) %>%
  mutate(taskid = c(1:length(Model)),
         Model_number = case_when(
           Model == "Model0" ~ 0,
           Model == "Model1" ~ 1,
           Model == "Model2" ~ 2,
           Model == "Model3" ~ 3,
           Model == "Model4" ~ 4)
  )
         
write.csv(dbdatamodel_long, "./models_and_scenarios_to_run_long.csv")  

#############################################################
# CLEAN DATA
#############################################################

# Country data
#############################################################

cdata = cdata[order(cdata$Iso3),] %>%
  mutate(
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
      TRUE ~ CountryTerritoryArea)
  )


# Countries that reported AST to GLASS
creport = adataAC %>% group_by(Iso3, Year) %>%
  summarise(n_ast = sum(SpecimenIsolateswithAST, na.rm=T)) %>% 
  filter(Year == 2022)

cdata = cdata %>%mutate(
  AMR_GLASS_AST_2022 = 
  case_when(
    Iso3 %in% creport$Iso3 ~ "Yes", 
    is.na(EnrolledAMR) ~ NA,
    TRUE ~ "No"),
  EnrollmentDateAMR = as.Date(EnrollmentDateAMR, format= "%d/%m/%Y"),
  EnrollmentDateAMC = as.Date(EnrollmentDateAMC, format= "%d/%m/%Y"),
  EnrollmentYearAMR = year(EnrollmentDateAMR),
  EnrollmentYearAMC = year(EnrollmentDateAMC),
) 

# Export data
write.csv(cdata, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_Countrydta_AST_140325_EV.csv"))

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
  )%>% 
  mutate(
    Sex = "Female",
    AgeCat5 = gsub("F_", "", DemographicsOrigin)
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
  ) %>% 
  mutate(
    Sex = "Male",
    AgeCat5 = gsub("M_", "", DemographicsOrigin)
  )


pdataDM = rbind(pdataDM_F, pdataDM_M) %>%
  mutate(Id = paste0("Id", Iso3, Year),
         PopulationSource = "United Nations",
         PopSourceComments = "WPP2024July") %>%
  select(Iso3, Id, Year, DemographicsOrigin, AgeCat5, Sex, TotalPopulation, PopulationSource, PopSourceComments)

# Combine all from French territory and Female and Male data
pdataDM = pdataDM %>% group_by(DemographicsOrigin, Year) %>%
   mutate(
     TotalPopulation = ifelse(Iso3 =="FRA", sum(TotalPopulation[Iso3 %in% c("GUF","GLP","MTQ","MYT","REU","FRA")]), TotalPopulation)
   ) %>% 
  filter(!Iso3 %in% c("GUF","GLP","MTQ","MYT","REU")
         )

# Now population data is in 5 years age band but we need them in 10 to link with amr data
pdataDM = pdataDM %>%
  group_by(Year, Sex) %>%
  mutate(
    AgeCat10 = case_when(
      AgeCat5 %in% c("0<4") ~ "0<04",
      AgeCat5 %in% c("5<9", "10<14") ~ "05<14",
      AgeCat5 %in% c("15<19", "20<24") ~ "15<24",
      AgeCat5 %in% c("25<29", "30<34") ~ "25<34",
      AgeCat5 %in% c("35<39", "40<44") ~ "35<44",
      AgeCat5 %in% c("45<49", "50<54") ~ "45<54",
      AgeCat5 %in% c("45<49", "50<54") ~ "45<54",
      AgeCat5 %in% c("55<59", "60<64") ~ "55<64",
      AgeCat5 %in% c("65<69", "70<74") ~ "65<74",
      AgeCat5 %in% c("75<79", "80<84") ~ "75<84",
      AgeCat5 %in% c("85<89", "90<94","95<99","100+") ~ "85+",
      TRUE ~ NA_character_ 
    ),
    Prefix = ifelse(Sex=="Female", "F", "M"),
    DemographicsOrigin = paste0(Prefix, "_", AgeCat10)
  )

pdataDM = pdataDM %>% select(-(AgeCat5)) %>%
  group_by(Iso3, Id, Year, DemographicsOrigin, Sex, AgeCat10, PopulationSource, PopSourceComments) %>%
  summarise(
    TotalPopulation = if(all(is.na(TotalPopulation))) NA else sum(TotalPopulation, na.rm = TRUE)
      )
  
  
# Export data
write.csv(pdataDM, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_PopdtaDM_140325_EV.csv"))

##############################################################
# Universal health coverage data
##############################################################

# Health Quality and Safety Index (IHME data)
haqidata <- haqidata[order(haqidata$location_name),] %>%
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

haqidata2019 <- haqidata %>% filter(year_id == 2019, indicator_name=="HAQ Index", haq_index_age_type=="Overall")

haqidata2019 <- left_join(haqidata2019, cdata, by="CountryTerritoryArea")
unique(haqidata2019$CountryTerritoryArea[is.na(haqidata2019$Iso3)])

# Export data
write.csv(haqidata2019, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_HAQIdta_Country_140325_EV.csv"))

#############################################################
# AMR data
#############################################################

# Drug-bug combinations for 2023 (using still those reported up until 2022)
##############################################################################
# Remove space at the end of antibiotic name so can be found in both datasets
adataAC$AntibioticName <- sub(" $", "", adataAC$AntibioticName)

combinations2022 = dbdata %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

adataAC = adataAC %>%
  mutate(
  combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
  InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
)

adataAC$combined = gsub("\\s+", " ", trimws(adataAC$combined))

# Check if all are present
present = which(!combinations2022$combined %in% unique(adataAC$combined))
combinations2022$combined[present] # 3 not featuring; "BLOOD-Klebsiella pneumoniae-Ampicillin", "URINE-Klebsiella pneumoniae-Ampicillin", "BLOOD-Staphylococcus aureus-Methicillin-resistance"
                                   # Makes sense, MRSA needs to be created and KPN Ampicillin is no longer reported.

d = adataAC %>% filter(Specimen=="BLOOD" & PathogenName=="Klebsiella pneumoniae")
unique(d$combined) # --> Misses in the raw data Ampicillin; check with Olga, as this one is depreciated for 2023 onwards

d = adataAC %>% filter(PathogenName=="Staphylococcus aureus")
unique(d$combined) # Misses MRSA as still needs to be created

# Link AMR data with country data
adataAC = left_join(adataAC, pdata, by=c("Iso3", "Year"))


# CALCULATE NUMBER OF ISOLATES FOR CERTAIN ANTIBIOTIC CLASSES
##---------------------------------------------------------------------------------------------------
# Resistance are mostly calculated for individual antibiotics, but in a few cases we must group some of the antibiotics. 
# This is the case of SDGs (third generation cephalosporins in E. coli and methicillin resistance in S. aureus) 
# and also sulfonamides and trimethoprim that is the same as co-trimoxazole (all pathogens). 

# Merge Sulfonamides and trimethoprim and Co-trimoxazole, as the former is a class, but just one abx,
# i.e. Co-trimoxazole. Merging should be done by taking among the countries with both or Sulfonamides and trimethoprim as well as Co-trimoxazole
# the one with the max isolate count, as there may be overlap.

# Target antibiotics for 3GC and Methicillin-resistance
f_rm <- adataAC %>% 
  group_by(Year, Specimen, PathogenName, Iso3) %>% 
  summarise(tgc_ab=sum(Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO"),
            mt_ab=sum(Antibiotic =="OXA" | Antibiotic =="FOX"))%>% 
  as.data.frame() 


d <- adataAC %>%
  mutate(AntibioticName2 = case_when(
    #(Antibiotic == "CTX") & (Specimen == "BLOOD") & (Pathogen == "KLEPNE") ~ AntibioticName,
    (Antibiotic %in% c("CTX", "CAZ", "CRO")) & (Specimen == "BLOOD") & (Pathogen == "ESCCOL") ~ 'Third generation cephalosporins',
    (Year %in% c(2017, 2018)) & (Antibiotic == "J01DD") & (Specimen == "BLOOD") & (Pathogen == "ESCCOL") ~ 'Third generation cephalosporins',
    (Antibiotic %in% c("OXA", "FOX")) & (Specimen == "BLOOD") & (Pathogen == "STAAUR") ~ 'Methicillin resistance',
    (Year %in% c(2017, 2018)) & (Antibiotic == "J01DC") & (Specimen == "BLOOD") & (Pathogen == "STAAUR") ~ 'Methicillin resistance',
    (Antibiotic %in% c("J01EE", "SXT")) & (Specimen %in% c("BLOOD", "URINE")) & (Pathogen == "ESCCOL") ~ "Co-trimoxazole",
    (Antibiotic %in% c("J01EE", "SXT")) & (Specimen %in% c("BLOOD", "URINE")) & (Pathogen == "KLEPNE") ~ "Co-trimoxazole",
    TRUE ~ AntibioticName
  ))

d2<-merge(d,f_rm, by=c("Year", "Specimen", "PathogenName", "Iso3"), all.x=TRUE)

# Remove J01DD and Jo1DC for countries with both single abx in 2017 and 2018 and class abx
d3 <- d2 %>%
  mutate(RemoveRecord = case_when((Antibiotic =="J01DD") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") & tgc_ab>0 ~  "remove",
                                  (Antibiotic =="J01DC") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="STAAUR") & mt_ab>0 ~  "remove"))

d4 <- d3 %>%
  #filter(!is.na(AbTargets))%>% 
  filter(is.na(RemoveRecord))%>% 
  droplevels()


##calculate resistance
d5 <- d4  %>% 
  mutate(PercentResistant = ((coalesce(Resistant,0)/(coalesce(InterpretableAST,0))*100)))


#for a given grouping we want row with max (percentageResistance). However, we do not want to select max(InterpretableAST) because we 
# instead want the InterpretableAST that was used to calculate max (percentageResistance). So instead of summarise(max) we use slice_max

d6 <- d5 %>% 
  as.data.frame() %>%
  group_by(Specimen, PathogenName, AntibioticName2, Iso3, Year) %>% 
  slice_max(PercentResistant) %>%
  as.data.frame() 

d6$combined = paste0(d6$Specimen, "-", d6$PathogenName, "-", d6$AntibioticName2)
unique(d6$combined)

# Check if duplicates removed
check = d6 %>%
  count(combined, Iso3, Year) %>%
 distinct() 
length(check$n) - length(d6$combined) # No duplicates

d7 <- d6 %>% select(-c(AntibioticName, tgc_ab, mt_ab, RemoveRecord)) %>%
  rename(AntibioticName = AntibioticName2) %>%
  select(c(Year, Iso3, Specimen, PathogenName,Pathogen, Antibiotic, AntibioticName, DemographicsOrigin, 
         NumSampledPatients, TotalSpecimenIsolates, SpecimenIsolateswithAST, 
         TotalPathogenIsolates, PathogenIsolateswithAST, TotalASTpathogenAntibiotic, InterpretableAST, Susceptible, 
         Intermediate, Resistant, UninterpretableAST, combined, InReport, 
         Id, TotalPopulation, PopulationSource, PopSourceComments, PercentResistant))

names(d7)

# NOW I NEED TO PUT BACK IN THE SINGLE 3GC ANTIBIOTICS!!
a1 <- adataAC %>% 
  filter(Antibiotic %in%c("CTX", "CAZ", "CRO") & Pathogen=="ESCCOL"& Specimen=="BLOOD") %>%
  mutate(PercentResistant = ((coalesce(Resistant,0)/(coalesce(InterpretableAST,0))*100))
         )%>%
  select(c(Year, Iso3, Specimen, PathogenName,Pathogen, Antibiotic, AntibioticName, DemographicsOrigin, 
                    NumSampledPatients, TotalSpecimenIsolates, SpecimenIsolateswithAST, 
                    TotalPathogenIsolates, PathogenIsolateswithAST, TotalASTpathogenAntibiotic, InterpretableAST, Susceptible, 
                    Intermediate, Resistant, UninterpretableAST, combined, InReport, 
                    Id, TotalPopulation, PopulationSource, PopSourceComments, PercentResistant))
    
names(a1)
names(d7)

d8 = rbind(d7,a1)
check = d8%>% filter(Antibiotic %in%c("CTX", "CAZ", "CRO")|AntibioticName=="Third generation cephalosporins" & Pathogen=="ESCCOL"& Specimen=="BLOOD") 
table(check$Iso3, check$AntibioticName)

# Link country data so we can join HAQI data
d9 = left_join(d8, cdata, by=c("Iso3"))

unique(d9$CountryTerritoryArea)

d10 = left_join(d9, haqidata2019)

# Export data
write.csv(d10, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_AMRdtaAC_Pop_country_HAQI_140325_EV.csv"))

adataAC_cb = d9

write.csv(adataAC_cb, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_AMRdtaAC_ANALYSES.csv"))

#############################################################
# AMR data by age and sex
#############################################################
adataNT$DemographicsOrigin2 <- gsub("(?<![0-9])<1(?![0-9])", "0<04", adataNT$DemographicsOrigin, perl = TRUE)
adataNT$DemographicsOrigin2 <- gsub("01<04", "0<04", adataNT$DemographicsOrigin2)
adataNT$DemographicsOrigin2 <- gsub("85<", "85+", adataNT$DemographicsOrigin2)

table(adataNT$DemographicsOrigin)

# Generate separate age and sex columns
adataNT <- adataNT %>%
  mutate(
    row = c(1:nrow(adataNT)),
    # Extract the prefix before the first underscore
    Prefix = sub("_.*", "", DemographicsOrigin),
    
    # Map the extracted prefix to the correct category
    Sex = case_when(
      Prefix == "M" ~ "Male",
      Prefix == "F" ~ "Female",
      Prefix == "O" ~ "Other",
      Prefix == "UnkGender" ~ "Unknown",
      TRUE ~ NA_character_  # Default case, if something unexpected appears
    ),
    # Step 1: Remove everything before the first underscore
    AgeCat10 = sub("^[^_]*_", "", DemographicsOrigin2),
    AgeCat10 = sub("_.*", "", AgeCat10),
    AgeCat10 = case_when(
      AgeCat10 %in% c("HO", "UnkOrigin", "CO") ~ NA_character_,
      #AgeCat10 %in% c("<1","01<04") ~ "0<4",
      #AgeCat10 %in% c("85<") ~ "85+",
      TRUE ~ AgeCat10
    ),
    Origin = sub("^[^_]*_[^_]*_", "", DemographicsOrigin),
    Origin = sub("_.*", "", Origin),
    Origin = case_when(
      Origin == "CO" ~ "Community",
      #Origin == "F" ~ NA,
      #Origin == "O" ~ NA,
      #Origin == "UnkGender" ~ NA,
      Origin == "HO" ~ "Hospital",
      Origin == "UnkOrigin" ~ "Unknown",
      TRUE ~ NA_character_  # Default case, if something unexpected appears
    )
  )
table(adataNT$AgeCat10)   

# Group by 'DemographicsOrigin' and sum up the selected columns
adataNT_c <- adataNT %>%
  group_by(Iso3, Year, DemographicsOrigin2, PathogenName, Pathogen, AntibioticName, Antibiotic, Specimen, AgeCat10, Sex, Origin) %>%
  summarise(
    NumSampledPatients = if(all(is.na(NumSampledPatients))) NA else sum(NumSampledPatients, na.rm = TRUE),
    TotalSpecimenIsolates = if(all(is.na(TotalSpecimenIsolates))) NA else sum(TotalSpecimenIsolates, na.rm = TRUE),
    SpecimenIsolateswithAST = if(all(is.na(SpecimenIsolateswithAST))) NA else sum(SpecimenIsolateswithAST, na.rm = TRUE),
    TotalPathogenIsolates = if(all(is.na(TotalPathogenIsolates))) NA else sum(TotalPathogenIsolates, na.rm = TRUE),
    PathogenIsolateswithAST = if(all(is.na(PathogenIsolateswithAST))) NA else sum(PathogenIsolateswithAST, na.rm = TRUE),
    TotalASTpathogenAntibiotic = if(all(is.na(TotalASTpathogenAntibiotic))) NA else sum(TotalASTpathogenAntibiotic, na.rm = TRUE),
    InterpretableAST = if(all(is.na(InterpretableAST))) NA else sum(InterpretableAST, na.rm = TRUE),
    Susceptible = if(all(is.na(Susceptible))) NA else sum(Susceptible, na.rm = TRUE),
    Intermediate = if(all(is.na(Intermediate))) NA else sum(Intermediate, na.rm = TRUE),
    Resistant = if(all(is.na(Resistant))) NA else sum(Resistant, na.rm = TRUE),
    UninterpretableAST = if(all(is.na(UninterpretableAST))) NA else sum(UninterpretableAST, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(
    DemographicsOrigin = "DemographicsOrigin2"
  )

adataNT_c$row = c(1:nrow(adataNT_c))

# Check if same size between the different datasets (i.e. should be 0)
summarized_data <- adataNT_c %>%
  group_by(DemographicsOrigin) %>%
  summarise(TotalSpecimenIsolatesSum = sum(TotalSpecimenIsolates, na.rm = TRUE))

summarized_data2 <- adataNT %>%
  group_by(DemographicsOrigin) %>%
  summarise(TotalSpecimenIsolatesSum = sum(TotalSpecimenIsolates, na.rm = TRUE))

#summarized_data %>% filter(DemographicsOrigin%in% c("F_0<04")) %>%
#   summarise(
#     n = sum(TotalSpecimenIsolatesSum)
#   ) - summarized_data2 %>% filter(DemographicsOrigin%in% c("F_<1", "F_01<04")) %>%
#   summarise(
#     n = sum(TotalSpecimenIsolatesSum)
#     ) # Yes!
  

# Merge Sulfonamides and trimethoprim and Co-trimoxazole, as the former is a class, but just one abx,
# i.e. Co-trimoxazole. Merging should be done by taking among the countries with both or Sulfonamides and trimethoprim as well as Co-trimoxazole
# the one with the max isolate count, as there may be overlap.

# Target antibiotics for 3GC and Methicillin-resistance
f_rm <- adataNT_c %>% 
  group_by(Year, Specimen, PathogenName, Iso3, DemographicsOrigin) %>% 
  summarise(tgc_ab=sum(Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO"),
            mt_ab=sum(Antibiotic =="OXA" | Antibiotic =="FOX"))%>% 
  as.data.frame() 


d <- adataNT_c %>%
  mutate(AntibioticName2 = case_when(
   # (Antibiotic == "CTX") & (Specimen == "BLOOD") & (Pathogen == "KLEPNE") ~ AntibioticName,
    (Antibiotic %in% c("CTX", "CAZ", "CRO")) & (Specimen == "BLOOD") & (Pathogen == "ESCCOL") ~ 'Third generation cephalosporins',
    (Year %in% c(2017, 2018)) & (Antibiotic == "J01DD") & (Specimen == "BLOOD") & (Pathogen == "ESCCOL") ~ 'Third generation cephalosporins',
    (Antibiotic %in% c("OXA", "FOX")) & (Specimen == "BLOOD") & (Pathogen == "STAAUR") ~ 'Methicillin resistance',
    (Year %in% c(2017, 2018)) & (Antibiotic == "J01DC") & (Specimen == "BLOOD") & (Pathogen == "STAAUR") ~ 'Methicillin resistance',
    (Antibiotic %in% c("J01EE", "SXT")) & (Specimen %in% c("URINE","BLOOD")) & (Pathogen == "ESCCOL") ~ "Co-trimoxazole",
    (Antibiotic %in% c("J01EE", "SXT")) & (Specimen %in% c("URINE", "BLOOD")) & (Pathogen == "KLEPNE") ~ "Co-trimoxazole",
    TRUE ~ AntibioticName
  ))

d2<-merge(d,f_rm, by=c("Year", "Specimen", "PathogenName", "Iso3", "DemographicsOrigin"), all.x=TRUE)
#d2$tgc_ab = ifelse(!d2$AntibioticName2 %in% c('Third generation cephalosporins'), NA, d2$tgc_ab)
#d2$mt_ab = ifelse(!d2$AntibioticName2 %in% c('Methicillin resistance'), NA, d2$mt_ab)

d3 <- d2 %>%
  mutate(RemoveRecord = case_when((Antibiotic =="J01DD") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") & tgc_ab>0 ~  "remove",
                                  (Antibiotic =="J01DC") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="STAAUR") & mt_ab>0 ~  "remove"))

d4 <- d3 %>%
  #filter(!is.na(AbTargets))%>% 
  filter(is.na(RemoveRecord))%>% 
  droplevels()


##calculate resistance
d5 <- d4  %>% 
  mutate(PercentResistant = ((coalesce(Resistant,0)/(coalesce(InterpretableAST,0))*100)))


#for a given grouping we want row with max (percentageResistance). However, we do not want to select max(InterpretableAST) because we 
# instead want the InterpretableAST that was used to calculate max (percentageResistance). So instead of summarise(max) we use slice_max

d6 <- d5 %>%
  as.data.frame() %>%
  group_by(Specimen, PathogenName, AntibioticName2, Iso3, Year, DemographicsOrigin) %>%
  slice_max(PercentResistant, with_ties = FALSE) %>%
  as.data.frame()

d6$combined = paste0(d6$Specimen, "-", d6$PathogenName, "-", d6$AntibioticName2)
unique(d6$combined)

# Check if duplicates removed
n_unique_observations <- d6 %>%
  distinct(combined, Iso3, Year, DemographicsOrigin) %>%
  nrow()

n_unique_observations - length(d6$combined) # No duplicates

d7 <- d6 %>% select(-c(AntibioticName, tgc_ab, mt_ab, RemoveRecord)) %>%
  rename(AntibioticName = AntibioticName2) %>%
  select(c(Year, Iso3, Specimen, PathogenName,Pathogen, Antibiotic, AntibioticName, DemographicsOrigin, 
           NumSampledPatients, TotalSpecimenIsolates, SpecimenIsolateswithAST, 
           TotalPathogenIsolates, PathogenIsolateswithAST, TotalASTpathogenAntibiotic, InterpretableAST, Susceptible, 
           Intermediate, Resistant, UninterpretableAST, combined, PercentResistant))

names(d7)

adataNT_c$combined = paste0(adataNT_c$Specimen, "-", adataNT_c$PathogenName, "-", adataNT_c$AntibioticName)

# NOW I NEED TO PUT BACK IN THE SINGLE 3GC ANTIBIOTICS!!
a1 <- adataNT_c %>% 
  filter(Antibiotic %in%c("CTX", "CAZ", "CRO") & Pathogen=="ESCCOL" & Specimen=="BLOOD") %>%
  mutate(PercentResistant = ((coalesce(Resistant,0)/(coalesce(InterpretableAST,0))*100))
  )%>%
  select(c(Year, Iso3, Specimen, PathogenName,Pathogen, Antibiotic, AntibioticName, DemographicsOrigin, 
           NumSampledPatients, TotalSpecimenIsolates, SpecimenIsolateswithAST, 
           TotalPathogenIsolates, PathogenIsolateswithAST, TotalASTpathogenAntibiotic, InterpretableAST, Susceptible, 
           Intermediate, Resistant, UninterpretableAST, combined, PercentResistant))

names(a1)
names(d7)
d8 = rbind(d7,a1)

# Link AMR data by demographics with country data
d8 = left_join(d8, cdata %>% select(c(Iso3, CountryTerritoryArea, WHORegionCode, WHORegionName, IncomeWorldBankJune2023)))

d9 = d8 %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
  )

d9$AntibioticName[d9$AntibioticName=="Cefotaxime "] = "Cefotaxime"
#table(d9$AntibioticName[d9$Year==2022&d9$AntibioticName%in%c("Cefotaxime","Ceftazidime","Ceftriaxone","Third generation cephalosporins")], d9$Iso3[d9$Year==2022&d9$AntibioticName%in%c("Cefotaxime","Ceftazidime","Ceftriaxone","Third generation cephalosporins")])

d10 = left_join(d9, pdataDM)

# Export data# ExporpdataDMt data
write.csv(d10, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_AMRdtaINT_Pop_Country_140325_EV.csv"))


# Remove space at the end of antibiotic name so can be found in both datasets
adataDM$AntibioticName <- sub(" $", "", adataDM$AntibioticName)
adataDM = adataDM %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
  )

# Export data
write.csv(adataDM, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_AMRdtaDM_Country_140325_EV.csv"))



# CREATE DATASET TO FIT THE MODELS TO
#----------------------------------------------------------------------------
# DATA WRANGLING TO DATA WHICH MODELS ARE FITTED TO
#----------------------------------------------------------------------------

# Take out all the lines which specify the origin of the sample as well have a missing age
# (as we don't have denominator population data for those)
adataNT_cbc = d10
adataNT_cbc$AntibioticName <- sub(" $", "", adataNT_cbc$AntibioticName)

# Filter out all origin categories
filtered_cat <- adataNT_cbc$DemographicsOrigin[!grepl("_CO$|_HO$|_UnkOrigin$", adataNT_cbc$DemographicsOrigin)]
unique(filtered_cat)

d1 = adataNT_cbc %>% filter(!is.na(TotalPopulation)|DemographicsOrigin%in%filtered_cat) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    amr_rate = Resistant/InterpretableAST,
    BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  )

# Filter out those countries with <10 isolates
d2 = adataAC_cb %>% filter(InterpretableAST>10) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)
  )

# Filter d1 based on the matching combinations in df2
adataAS = d1 %>%
  semi_join(d2, by = c("combined", "Iso3", "Year"))

# Check if all are matched
unmatched_d2 <- d2 %>%
  anti_join(d1, by = c("combined", "Iso3", "Year")) # All matched

# Check if numbers add up for totals in adataAS with adataAC
d3 = adataAS %>%group_by(Iso3, Year, combined) %>%
  summarise(InterpretableAST=sum(InterpretableAST, na.rm=T),
            Resistant=sum(Resistant, na.rm = T))

d2$Key <- paste(d2$Iso3, d2$Year, d2$combined)
d3$Key <- paste(d3$Iso3, d3$Year, d3$combined)

d4 = left_join(d2 %>% select(Iso3, Year, combined, InterpretableAST, Resistant, Key),d3, by=c("Iso3","Year", "combined", "Key")) %>%
  mutate(
    diff_AST = InterpretableAST.x - InterpretableAST.y,
    diff_R = Resistant.x - Resistant.y
  )

# CHECK IF NUMBERS ADD UP
no_add = unique(d4$combined[d4$diff_AST!=0]) # FOR THE CLASSES THEY DON'T ADD UP PROPERLY, BUT THAT MAKES SENSE AS USED DIFFERENT REFERENCES THERE WHEN SPLIT BY AGE
no_add

f = d4 %>% filter(combined==no_add[5])
f2 = d4 %>% filter(combined==no_add[2])


# Make age a numeric co-variate so we can fit a non-linear trend
adataAS <- adataAS %>%
  mutate(s_AgeCat10 = case_when(
    AgeCat10=="0<04" ~ as.numeric(1),
    AgeCat10=="05<14" ~ as.numeric(2),
    AgeCat10=="15<24" ~ as.numeric(3),
    AgeCat10=="25<34" ~ as.numeric(4),
    AgeCat10=="35<44" ~ as.numeric(5),
    AgeCat10=="45<54" ~ as.numeric(6),
    AgeCat10=="55<64" ~ as.numeric(7),
    AgeCat10=="65<74" ~ as.numeric(8),
    AgeCat10=="75<84" ~ as.numeric(9),
    AgeCat10=="85+" ~ as.numeric(10),
    AgeCat10=="UnkAge" ~ NA
  ))

# Export data
write.csv(adataAS, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_AMRdtaINT_ANALYSES.csv")) 
# I SAVED THIS DATASET FROM ANOTHER SCRIPT; NEED TO CHECK STILL WHICH SCRIPT. FOR NOW HAVE WRITTEN IT 
# HERE AS THE ABOVE CODE SHOULD DO WHAT I DID THERE



# Drug-bug combinations for 2023 
#########################################################################



#############################################################
# Implementation data
#############################################################

# Clean names
d = idata
na = names(idata)
names(d)[4:6] = c("NCC_YES","NCC_NO","NCC_IN_PROGRESS")
names(d)[7:9] = c("NLR_YES","NLR_NO","NLR_UNK")
names(d)[10:12] = c("NLR_EQA_YES","NLR_EQA_NO","NLR_EQA_UNK")
names(d)[13:17] = c("LAB_EUCAST","LAB_CLSI","LAB_BOTH", "LAB_OTHER", "LAB_UNK")
names(d) = tolower(names(d))
names(d)[1:3] = c("Iso3", "Year", "options")

d = d[2:length(d$Year),]

idatac = d
idatac = left_join(cdata, idatac, by="Iso3")

# Make names in line with previous data, so we can reproduce code
#unique(idata_old$Iso3)  # Org unit = Iso3 in historical data
#unique(idata$period)    # Period = Year


idatac = idatac %>%
  mutate(AMR_NCC = 
           case_when(ncc_yes=="Yes" ~ "Established", 
                     ncc_no =="Yes"  ~ "Not established",
                     ncc_in_progress == "Yes"  ~ "Establishment in progress",
                     is.na(ncc_yes) & is.na(ncc_no) & is.na(ncc_in_progress) & EnrolledAMR == "Yes"  ~ "Not reported",
                     is.na(EnrolledAMR) ~ "Not enrolled",
                     TRUE ~ NA),
         AMR_NLR = 
            case_when(nlr_yes=="Yes" ~ "Established", 
                      nlr_no =="Yes"  ~ "Not established",
                      nlr_unk == "Yes"  ~ "Not reported",
                      is.na(nlr_yes) & is.na(nlr_no) & is.na(nlr_unk) & EnrolledAMR == "Yes" ~ "Not reported",
                      is.na(EnrolledAMR) ~ "Not enrolled",
                      TRUE ~ NA),
         EQA_to_NLR = 
           case_when(nlr_eqa_yes=="Yes" ~ "Provided", 
                     nlr_eqa_no =="Yes"  ~ "Not provided",
                     nlr_eqa_unk == "Yes"  ~ "Not reported",
                     is.na(nlr_eqa_yes) & is.na(nlr_eqa_no) & is.na(nlr_eqa_unk) & EnrolledAMR == "Yes" ~ "Not reported",
                     is.na(EnrolledAMR) ~ "Not enrolled",
                     TRUE ~ NA),
         AMR_AST_standards = 
           case_when(lab_clsi =="Yes" ~ "CLSI", 
                     lab_eucast =="Yes"  ~ "EUCAST",
                     lab_both == "Yes"  ~ "CLSI|EUCAST",
                     lab_other== "Yes" ~ "O",
                     lab_unk == "Yes"|(!is.na(EnrolledAMR)&is.na(lab_clsi)&is.na(lab_eucast)&is.na(lab_both)
                                       &is.na(lab_other))~ "Not reported",
                     
                     is.na(EnrolledAMR) ~ "Not enrolled",
                     TRUE ~ NA)
  )  %>% 
  mutate(
    AMR_GLASS_AST_2022 = ifelse(Iso3 %in% creport$Iso3, "Yes", "No")
  )

# table(idatac$AMR_NCC,idatac$ncc_yes)
# table(idatac$AMR_NCC,idatac$ncc_no)
# table(idatac$AMR_NCC,idatac$ncc_in_progress)
# 

# CHECK with curated data olga
idatao = read.csv(paste0(dirDataNew,"/GLASS_final_curated/EI_Implementationdta_110325_EV.csv"), sep=",")      

# NCC
table(idatao$AMR_NCC)
table(idatac$AMR_NCC)

# NRL
table(idatao$AMR_NRL)
table(idatac$AMR_NLR)

# NRL to EQA
table(idatao$EQA_to_NRL)
table(idatac$EQA_to_NLR)

# AMR_AST_standards
table(idatao$AMR_AST_standards)
table(idatac$AMR_AST_standards)

# Remove merged variables
idatac = idatac %>% select(-c(ncc_yes, ncc_no, ncc_in_progress, nlr_yes, nlr_no, nlr_unk,
                            nlr_eqa_yes, nlr_eqa_no, nlr_eqa_unk, lab_clsi,lab_eucast,
                            lab_both, lab_other, lab_unk, options, amr_glass_data))
names(idatac)

# Change characters to numbers
idatac[,c(19:30)] = sapply(idatac[,c(19:30)], function(x) as.numeric(x))
sapply(idatac, function(x) class(x))

# Merge with population data to do quality checks
idatac = left_join(idatac, pdata%>%filter(Year=="2022"), by="Iso3")

# Now quality indicator for surveillance variables
idatacs = idatac %>%
  mutate(
    amr_amr_hospitals_number = ifelse(amr_amr_hospitals_number >= amr_amr_acute_care_number, "not_reliable",amr_amr_hospitals_number),
    amr_amr_acute_care_number = ifelse(amr_amr_acute_care_number <10 |amr_amr_acute_care_number<amr_amr_hospitals_number, "not_reliable", amr_amr_acute_care_number),
    amr_amr_inpatient_adm_number = ifelse(amr_amr_inpatient_adm_number< amr_amr_hospitals_number | amr_amr_inpatient_adm_number<0.01*TotalPopulation,
                                          "not_reliable", amr_amr_inpatient_adm_number),
    amr_amr_inpatient_day_number = ifelse(amr_amr_inpatient_day_number<amr_amr_inpatient_adm_number, "not_reliable", amr_amr_inpatient_day_number),
    #amr_amr_outpatient_cons_number = ifelse(amr_amr_outpatient_cons_number< (2*as.numeric(amr_amr_inpatient_adm_number)), "not_reliable", amr_amr_outpatient_cons_number),
    amr_glass_acute_care_number = ifelse(amr_glass_acute_care_number>amr_amr_acute_care_number, "not_reliable",amr_glass_acute_care_number),
    amr_glass_hospitals_number = ifelse(amr_glass_hospitals_number>amr_amr_hospitals_number | amr_glass_hospitals_number>amr_glass_acute_care_number,
                                        "not_reliable", amr_glass_hospitals_number),
    amr_glass_inpatient_adm_number = ifelse(amr_glass_inpatient_adm_number<amr_glass_hospitals_number|amr_glass_inpatient_adm_number>amr_amr_inpatient_adm_number,
                                            "not_reliable", amr_glass_inpatient_adm_number),
    amr_glass_inpatient_day_number = ifelse(amr_glass_inpatient_day_number<=amr_glass_inpatient_adm_number|amr_glass_inpatient_day_number>amr_amr_inpatient_day_number,
                                            "not_reliable", amr_glass_inpatient_day_number),
    amr_glass_outpatient_cons_number = ifelse(amr_glass_outpatient_cons_number>amr_amr_outpatient_cons_number, "not_reliable", amr_glass_outpatient_cons_number),
    lab_number_data_call = ifelse(lab_number_data_call>amr_amr_acute_care_number, "not_reliable",lab_number_data_call)   
  ) %>%
  select(Iso3, CountryTerritoryArea, WHORegionCode, WHORegionName, amr_amr_hospitals_number,amr_amr_acute_care_number,
         amr_amr_inpatient_adm_number, amr_amr_inpatient_day_number, amr_amr_outpatient_cons_number,amr_glass_acute_care_number,
         amr_glass_hospitals_number, amr_glass_inpatient_adm_number, amr_glass_inpatient_day_number, amr_glass_outpatient_cons_number,
         lab_number_data_call, AMR_NCC, AMR_NLR, EQA_to_NLR, AMR_AST_standards, AMR_GLASS_AST_2022)


# CHECK with curated data olga
sdatao = read.csv(paste0(dirDataNew,"/GLASS_final_curated/EI_SurveillanceSites_110325_EV.csv"), sep=",")      
unique(sdatao$Iso3)

enrolled = unique(cdata$Iso3[cdata$EnrolledAMR=="Yes"])
enrolled = enrolled[!is.na(enrolled)]

sdata = idatacs %>% select(Iso3, CountryTerritoryArea, WHORegionCode, WHORegionName, amr_amr_hospitals_number,amr_amr_acute_care_number,
                           amr_amr_inpatient_adm_number, amr_amr_inpatient_day_number, amr_amr_outpatient_cons_number,amr_glass_acute_care_number,
                           amr_glass_hospitals_number, amr_glass_inpatient_adm_number, amr_glass_inpatient_day_number, amr_glass_outpatient_cons_number,
                           lab_number_data_call) %>%
  filter(Iso3 %in% enrolled)

# Which are present in the above, but not in curated dataset of Olga
missing = unique(sdata$Iso3)[which(!unique(sdata$Iso3)%in%unique(sdatao$Iso3))]
View(sdata %>% filter(Iso3%in%missing)) # Descrepancy comes from countries which have NA or not reliable in all variables. BIH the exception

sdata = sdata %>% filter(Iso3 %in% unique(sdatao$Iso3))

# I AM GETTING COMPLETELY DIFFERENT NUMBERS SO LEAVE FOR NOW/

idatacss = idatacs %>% select(Iso3, CountryTerritoryArea, WHORegionCode, WHORegionName, AMR_NCC, AMR_NLR, EQA_to_NLR, AMR_AST_standards, AMR_GLASS_AST_2022)

# Not sure why the outpatient variable is not working
#table(idatac$amr_amr_outpatient_cons_number,useNA="always")
#table(idatacs$amr_amr_outpatient_cons_number,useNA="always")

# 
# table(sdatao$GLASS_Num_Acute_HCF, useNA="always")
# table(idatacs$amr_glass_acute_care_number, useNA="always")
# 
# table(cdata$EnrolledAMR)
# table(cdata$AMR_GLASS_AST_2022)

# Export data
write.csv(idatacss, paste0(dirDataNew, "/GLASS_final_curated/GLASS_final_curated_linked/EI_Implementationdta_Country_140325_EV.csv"))
#write.csv(sdata, paste0(dirDataNew, "/EI_SurveillanceSitesdta_Country_03092024_EV.csv"))

