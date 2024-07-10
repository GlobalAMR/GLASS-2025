#################################################
# GLASS REPORT - Data wrangling
#################################################

# Author: Esther van Kleef
# Date created: 8 July 2024
# Date latest update: 8 July 2024

rm(list=ls())
# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet)


# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Master_Raw_Data_GLASS_2024"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"

##############################################################
# LOAD IN DATA
##############################################################

idata = read.csv(paste0(dirDataNew,"/Implementation Questionnaire_040724_EV.csv"), sep=";")                   # Implementation data
idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Implementation data
cdata = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Countrydta_071123 EV.csv"), sep=",")   # Country data

# AMR data
adata = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaAC_071123 EV.csv"), sep=",")   # Country data

#############################################################
# CLEAN DATA
#############################################################

# Country data
#############################################################

# Countries that reported AST to GLASS
creport = adata %>% group_by(Iso3, Year) %>%
  summarise(n_ast = sum(SpecimenIsolateswithAST, na.rm=T)) %>% 
  filter(Year == 2021)

cdata = cdata %>%mutate(
  AMR_GLASS_AST = 
  case_when(
    Iso3 %in% creport$Iso3 ~ "Yes", 
    is.na(EnrolledAMR) ~ NA,
    TRUE ~ "No")
)

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

