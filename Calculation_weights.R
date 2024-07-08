#################################################
# GLASS REPORT - Calculation of weights
#################################################

# Author: Esther van Kleef
# Date created: 5 July 2024
# Date latest update: 

rm(list=ls())
# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet)


# Locate directories
dirData = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNew = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report"
dirOutput = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/"

##############################################################
# LOAD IN DATA
##############################################################
pdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Popdta_071123 EV.csv"), sep=",")       # Population data
cdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Countrydta_071123 EV.csv"), sep=",")   # Country data
sdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_SurveillanceSites_071123 EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/Implementation Questionnaire_040724_EV.csv"), sep=";")                   # Implementation data
idata_old = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data

##############################################################
# CLEAN DATA
##############################################################

# Population data
##############################################################


# GLASS Country data
##############################################################

# GLASS Surveillance sites
##############################################################

# GLASS Implementation data
##############################################################

# Clean names
names(idata) = idata[3,]
names(idata)[1:17] = idata[4,c(1:17)] 
names(idata)[7:9] = c("NLR_YES","NLR_NO","NLR_UNK")
names(idata)[10:12] = c("NLR_EQA_YES","NLR_EQA_NO","NLR_EQA_UNK")
names(idata)[13:17] = c("LAB_EUCAST","LAB_CLSI","LAB_BOTH", "LAB_OTHER", "LAB_UNK")

names(idata) = tolower(names(idata))
names(idata)[1] = "org_unit"
names(idata)[4] = "ncc_yes"

names(idata_old)
names(idata)
      
# Universal health coverage data
##############################################################
# Source:
# https://www.who.int/publications/i/item/9789240080379
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-index-of-service-coverage


