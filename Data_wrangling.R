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
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV"
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
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")       
pdataF_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexFemale_180724_EV.xlsx"))
pdataM_raw = readxl::read_xlsx(paste0(dirDataNew, "/UN_pop_raw/EI_Popdta_byagesexMale_180724_EV.xlsx"))

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")

# Surveillance implementation data
idata = read.csv(paste0(dirDataNewO,"/Implementation Questionnaire_AMR_2022_250724.csv"), sep=";")      

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_180724_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read_excel(paste0(dirDataNew, "/updated_summary_dbc_longformat.xlsx"), sheet=1)

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

# Export data
write.csv(cdata, paste0(dirDataNew, "/EI_CountrydtaDM_180724_EV.csv"))

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
write.csv(pdataDM, paste0(dirDataNew, "/EI_PopdtaDM_180724_EV.csv"))

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
write.csv(haqidata2019, paste0(dirDataNew, "/EI_HAQIdta_080724_EV.csv"))

#############################################################
# AMR data
#############################################################

# Drug-bug combinations for 2023 
#########################################################################
combinations2022 = dbdata %>% filter(Period %in% c("2016-","2016-2022")) %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

# Check if all are present
present = which(!unique(combinations2022$combined) %in% unique(adataAC$combined))
unique(combinations2022$combined[present])

# Link AMR data with country data
adataAC = left_join(adataAC, pdata, by=c("Iso3", "Year"))

# Remove space at the end of antibiotic name so can be found in both datasets
adataAC$AntibioticName <- sub(" $", "", adataAC$AntibioticName)
adataAC = adataAC %>%
  mutate(
  combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
  InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
)

# Merge Sulfonamides and trimethoprim and Co-trimoxazole, as the former is a class, but just one abx,
# i.e. Co-trimoxazole. Merging should be done by taking among the countries with both or Sulfonamides and trimethoprim as well as Co-trimoxazole
# the one with the max isolate count, as there may be overlap.
adataAC_c = adataAC %>% 
  mutate(
    sulf = ifelse(AntibioticName == "Sulfonamides and trimethoprim", "Yes", "No"),
    AntibioticName = ifelse(AntibioticName == "Sulfonamides and trimethoprim", "Co-trimoxazole", AntibioticName),
  ) %>% group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  ungroup() %>%
  filter(sulf != "Yes") %>% 
  select(-c("sulf"))

# CHECK
# View(adataAC_c %>% filter(AntibioticName=="Co-trimoxazole") %>%
#         group_by(Iso3,AntibioticName, PathogenName, Specimen,Year) %>%
#         summarise(n=sum(InterpretableAST)))
# 
# View(adataAC %>% filter(AntibioticName %in% c("Co-trimoxazole","Sulfonamides and trimethoprim")) %>%
#        group_by(Iso3,AntibioticName, PathogenName, Specimen,Year) %>%
#        summarise(n=sum(InterpretableAST)))
# table(adataAC$AntibioticName=="Sulfonamides and trimethoprim")


# Change 3rd generation cephalosporin for E. coli in BLOOD to max value in CTX, CAZ and CRO
ceph_ecoli_bl = adataAC %>% 
  filter(Antibiotic %in% c("CTX", "CAZ", "CRO") & Specimen == "BLOOD" & PathogenName=="Escherichia coli") %>%
  mutate(
    AntibioticName = "Third-generation cephalosporins",
    Antibiotic = "J01DD"
  ) %>% group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  distinct(Iso3, Year, .keep_all = TRUE)

# Check for which countries single abx where missing in 2017 and 2018 but J01DD was there
single = adataAC %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("CTX", "CAZ", "CRO") & Specimen == "BLOOD" & PathogenName=="Escherichia coli")%>%
  group_by(Iso3, Year) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

single3gc = adataAC %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("J01DD") & Specimen == "BLOOD" & PathogenName=="Escherichia coli")%>%
  group_by(Iso3, Year) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

# Join the tow to see which ones do not have single 3GC but do have the class 3GC
total = left_join(single3gc, single, by=c("Iso3",  "Year"))
countries2017 = unique(total$Iso3[is.na(total$n.y) & total$Year==2017])
countries2018 = unique(total$Iso3[is.na(total$n.y) & total$Year==2018])

ceph_ecoli_bl_add = adataAC %>% 
  filter((Year == 2017 & Iso3 %in% countries2017 &
            Antibiotic == "J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli") | 
           (Year == 2018 & Iso3 %in% countries2018&
              Antibiotic == "J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli") )
# %>%
#   mutate(
#     combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
#     InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
#   ) 
  
ceph_ecoli_bl = rbind(ceph_ecoli_bl, ceph_ecoli_bl_add)

adataAC_cb = adataAC_c %>% filter(!(Antibiotic=="J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli"))

adataAC_cb = rbind(adataAC_cb,ceph_ecoli_bl)

# # Checking for duplicates where Antibiotic == "J01DD"
# duplicates_J01DD <- adataAC_cb %>%
#   filter(Antibiotic == "J01DD" & Specimen == "BLOOD" & PathogenName == "Escherichia coli") %>%
#   group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
#   filter(n() > 1) # Keep only groups with more than 1 row
# 
# # Check if any duplicates were found
# print(duplicates_J01DD) # 0 duplicates

# Change Methicillin-resistance for S. Aureus in BLOOD to max value in OXA and FOX
ceph_staph_bl = adataAC %>% 
  filter(Antibiotic %in% c("OXA", "FOX") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") %>%
  mutate(
    AntibioticName = "Methicillin-resistance",
    Antibiotic = "J01DC"
  ) %>% group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  distinct(Iso3, Year, .keep_all = TRUE)
# %>%
#   mutate(
#     combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
#     InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
#   ) 

# Check for which countries single abx where missing in 2017 and 2018 but J01DC was there
single = adataAC %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("OXA", "FOX") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus")%>%
  group_by(Iso3, Year) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

single2gc = adataAC %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("J01DC") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus")%>%
  group_by(Iso3, Year) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

total = left_join(single2gc, single, by=c("Iso3",  "Year"))
countries2017 = unique(total$Iso3[is.na(total$n.y) & total$Year==2017])
countries2018 = unique(total$Iso3[is.na(total$n.y) & total$Year==2018])

ceph_staph_bl_add = adataAC %>% 
  filter((Year == 2017 & Iso3 %in% countries2017 &
            Antibiotic == "J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") | 
           (Year == 2018 & Iso3 %in% countries2018&
              Antibiotic == "J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") ) %>%
  ungroup()

ceph_staph_bl = rbind(ceph_staph_bl, ceph_staph_bl_add)

adataAC_cb = adataAC_cb %>% filter(!(Antibiotic=="J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus"))

adataAC_cb = rbind(adataAC_cb,ceph_staph_bl)

# Checking for duplicates where Antibiotic == "J01DD"
# duplicates_J01DC <- adataAC_cb %>%
#   filter(Antibiotic == "J01CD" & Specimen == "BLOOD" & PathogenName == "Staphylococcus aureus") %>%
#   group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
#   filter(n() > 1) # Keep only groups with more than 1 row
# 
# # Check if any duplicates were found
# print(duplicates_J01DC) # 0 duplicates


# Link country data so we can join HAQI data
adataAC_cb = left_join(adataAC_cb, cdata, by=c("Iso3"))
unique(adataAC_cb$CountryTerritoryArea)
#adataAC$CountryTerritoryArea[adataAC$CountryTerritoryArea=="C\xf4te d'Ivoire"] = "Côte d'Ivoire"
#unique(haqdata$location_name)

adataAC_cb = left_join(adataAC_cb, haqidata2019)

# Export data
write.csv(adataAC_cb, paste0(dirDataNew, "/EI_AMRdtaAC_pop_ABX_adapted_180724_EV.csv"))
write.csv(adataAC, paste0(dirDataNew, "/EI_AMRdtaAC_pop_180724_EV.csv"))

#############################################################
# AMR data by age and sex
#############################################################
adataNT$DemographicsOrigin2 <- gsub("(?<![0-9])<1(?![0-9])", "0<04", adataNT$DemographicsOrigin, perl = TRUE)
adataNT$DemographicsOrigin2 <- gsub("01<04", "0<04", adataNT$DemographicsOrigin2)
adataNT$DemographicsOrigin2 <- gsub("85<", "85+", adataNT$DemographicsOrigin2)

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
      Origin == "F" ~ NA,
      Origin == "O" ~ NA,
      Origin == "UnkGender" ~ NA,
      Origin == "HO" ~ "Hospital",
      Origin == "UnkOrigin" ~ "Unknown",
      TRUE ~ NA_character_  # Default case, if something unexpected appears
    )
  )
     
# Group by 'DemographicsOrigin' and sum up the selected columns
adataNT_c <- adataNT %>%
  group_by(Iso3, Year, DemographicsOrigin2, PathogenName, AntibioticName, Antibiotic, Specimen, AgeCat10, Sex, Origin) %>%
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

# summarized_data %>% filter(DemographicsOrigin%in% c("F_0<04")) %>%
#   summarise(
#     n = sum(TotalSpecimenIsolatesSum)
#   ) - summarized_data2 %>% filter(DemographicsOrigin%in% c("F_<1", "F_01<04")) %>%
#   summarise(
#     n = sum(TotalSpecimenIsolatesSum)
#     ) # Yes!
  

# Merge Sulfonamides and trimethoprim and Co-trimoxazole, as the former is a class, but just one abx,
# i.e. Co-trimoxazole. Merging should be done by taking among the countries with both or Sulfonamides and trimethoprim as well as Co-trimoxazole
# the one with the max isolate count, as there may be overlap.
adataNT_cb = adataNT_c %>% 
  mutate(
    sulf = ifelse(AntibioticName == "Sulfonamides and trimethoprim", "Yes", "No"),
    AntibioticName = ifelse(AntibioticName == "Sulfonamides and trimethoprim", "Co-trimoxazole", AntibioticName),
  ) %>% group_by(Iso3, Year, DemographicsOrigin, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  ungroup() %>%
  filter(sulf != "Yes") %>% 
  select(-c("sulf"))

# Change 3rd generation cephalosporin for E. coli in BLOOD to max value in CTX, CAZ and CRO
ceph_ecoli_bl = adataNT_c %>% 
  filter(Antibiotic %in% c("CTX", "CAZ", "CRO") & Specimen == "BLOOD" & PathogenName=="Escherichia coli") %>%
  mutate(
    AntibioticName = "Third-generation cephalosporins",
    Antibiotic = "J01DD"
  ) %>% group_by(Iso3, Year,DemographicsOrigin, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  distinct(Iso3, Year,DemographicsOrigin, .keep_all = TRUE)

# Check for which countries single abx where missing in 2017 and 2018 but J01DD was there
single = adataNT_c %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("CTX", "CAZ", "CRO") & Specimen == "BLOOD" & PathogenName=="Escherichia coli")%>%
  group_by(Iso3, Year, DemographicsOrigin) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

single3gc = adataNT_c %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("J01DD") & Specimen == "BLOOD" & PathogenName=="Escherichia coli")%>%
  group_by(Iso3, Year, DemographicsOrigin) %>%
  summarise(n = max(InterpretableAST, na.rm=T))


# Join the two to see which ones do not have single 3GC but do have the class 3GC
total = left_join(single3gc, single, by=c("Iso3",  "Year", "DemographicsOrigin"))
countries2017 = unique(total$Iso3[is.na(total$n.y) & total$Year==2017])
countries2018 = unique(total$Iso3[is.na(total$n.y) & total$Year==2018])

ceph_ecoli_bl_add = left_join(total[is.na(total$n.y),] %>% select(-c(n.x,n.y)),adataNT_c) %>% 
  filter((Year == 2017 & Iso3 %in% countries2017 &
            Antibiotic == "J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli") | 
           (Year == 2018 & Iso3 %in% countries2018&
              Antibiotic == "J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli") )

ceph_ecoli_bl = rbind(ceph_ecoli_bl, ceph_ecoli_bl_add)
ceph_ecoli_bl = ceph_ecoli_bl[!duplicated(ceph_ecoli_bl$row),]

adataNT_cbc = adataNT_cb %>% filter(!(Antibiotic=="J01DD"& Specimen == "BLOOD" & PathogenName=="Escherichia coli"))

adataNT_cbc = rbind(adataNT_cbc,ceph_ecoli_bl)

# Checking for duplicates where Antibiotic == "J01DD"
duplicates_J01DD <- adataNT_cb %>%
   filter(Antibiotic == "J01DD" & Specimen == "BLOOD" & PathogenName == "Escherichia coli") %>%
   group_by(Iso3, Year, DemographicsOrigin, PathogenName, AntibioticName, Specimen) %>%
   filter(n() > 1) # Keep only groups with more than 1 row

# Check if any duplicates were found
print(duplicates_J01DD) # 0 duplicates

# Change 2nd generation cephalosporin for S. Aureus in BLOOD to max value in OXA and FOX
ceph_staph_bl = adataNT_c %>% 
  filter(Antibiotic %in% c("OXA", "FOX") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") %>%
  mutate(
    AntibioticName = "Methicillin-resistance",
    Antibiotic = "J01DC"
  ) %>% group_by(Iso3, Year, DemographicsOrigin, PathogenName, AntibioticName, Specimen) %>%
  mutate(
    NumSampledPatients = max(NumSampledPatients),
    TotalSpecimenIsolates = max(TotalSpecimenIsolates),
    SpecimenIsolateswithAST = max(SpecimenIsolateswithAST),
    TotalPathogenIsolates = max(TotalPathogenIsolates),
    PathogenIsolateswithAST = max(PathogenIsolateswithAST),
    TotalASTpathogenAntibiotic = max(TotalASTpathogenAntibiotic),
    InterpretableAST = max(InterpretableAST),          
    Susceptible = max(Susceptible),
    Intermediate = max(Intermediate),
    Resistant = max(Resistant),
    UninterpretableAST = max(UninterpretableAST)
  ) %>%
  distinct(Iso3, Year, DemographicsOrigin, .keep_all = TRUE)

# Check for which countries single abx where missing in 2017 and 2018 but J01DC was there
single = adataNT_c %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("OXA", "FOX") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus")%>%
  group_by(Iso3, Year, DemographicsOrigin) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

single2gc = adataNT %>% 
  filter(Year %in% c(2017,2018) & Antibiotic %in% c("J01DC") & Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus")%>%
  group_by(Iso3, Year, DemographicsOrigin) %>%
  summarise(n = max(InterpretableAST, na.rm=T))

total = left_join(single2gc, single, by=c("Iso3",  "Year", "DemographicsOrigin"))
countries2017 = unique(total$Iso3[is.na(total$n.y) & total$Year==2017])
countries2018 = unique(total$Iso3[is.na(total$n.y) & total$Year==2018])

ceph_staph_bl_add = left_join(total[is.na(total$n.y),] %>% select(-c(n.x,n.y)),adataNT_c) %>% 
  filter((Year == 2017 & Iso3 %in% countries2017 &
            Antibiotic == "J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") | 
           (Year == 2018 & Iso3 %in% countries2018&
              Antibiotic == "J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus") ) %>%
  ungroup()

ceph_staph_bl = rbind(ceph_staph_bl, ceph_staph_bl_add)
ceph_staph_bl = ceph_staph_bl[!duplicated(ceph_staph_bl$row),]

adataNT_cbc = adataNT_cbc %>% filter(!(Antibiotic=="J01DC"& Specimen == "BLOOD" & PathogenName=="Staphylococcus aureus"))

adataNT_cbc = rbind(adataNT_cbc,ceph_staph_bl)

# Checking for duplicates where Antibiotic == "J01DD"
# duplicates_J01DC <- adataAC_cb %>%
#   filter(Antibiotic == "J01CD" & Specimen == "BLOOD" & PathogenName == "Staphylococcus aureus") %>%
#   group_by(Iso3, Year, PathogenName, AntibioticName, Specimen) %>%
#   filter(n() > 1) # Keep only groups with more than 1 row
# 
# # Check if any duplicates were found
# print(duplicates_J01DC) # 0 duplicates


# Link AMR data with country data 
adataNT_cbc = left_join(adataNT_cbc, pdataDM)
adataNT_cbc = left_join(adataNT_cbc, cdata %>% select(c(Iso3, CountryTerritoryArea, WHORegionCode, WHORegionName, IncomeWorldBankJune2022)))


# Export data
write.csv(adataNT_cbc, paste0(dirDataNew, "/EI_AMRdtaINT_pop_ABX_adapted_180724_EV.csv"))


# Drug-bug combinations for 2023 
#########################################################################



#############################################################
# Implementation data
#############################################################

# Clean names
names(idata) = idata[3,]
names(idata)[1:17] = idata[4,c(1:17)] 
names(idata)[1] = "org_unit"
names(idata)[7:9] = c("NLR_YES","NLR_NO","NLR_UNK")
names(idata)[10:12] = c("NLR_EQA_YES","NLR_EQA_NO","NLR_EQA_UNK")
names(idata)[13:17] = c("LAB_EUCAST","LAB_CLSI","LAB_BOTH", "LAB_OTHER", "LAB_UNK")
names(idata) = tolower(names(idata))

# Remove first rows
idata = idata[5:length(idata$period),]

# Make names in line with previous data, so we can reproduce code
#unique(idata_old$Iso3)  # Org unit = Iso3 in historical data
#unique(idata$period)    # Period = Year

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
    Iso3 = "org_unit",
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
idata[,c(3:14)] = sapply(idata[,c(3:14)], function(x) as.numeric(x))

describe(idata)
sapply(idata, function(x) class(x))

# Merge implementation and country data
idata_country = merge(cdata,idata,by=c("Iso3"), all = T) %>%
  rename(AMR_GLASS_AST = "AMR_GLASS_AST.x") %>%
  select(-c(AMR_GLASS_AST.y))


# Export data
write.csv(idata, paste0(dirDataNew, "/EI_Implementationdta_080724_EV.csv"))
write.csv(idata_country, paste0(dirDataNew, "/EI_ImplementationCdta_080724_EV.csv"))

