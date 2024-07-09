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
dirDataNew = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"
dirOutput = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables"

##############################################################
# LOAD IN DATA
##############################################################
pdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Popdta_071123 EV.csv"), sep=",")       # Population data
cdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Countrydta_071123 EV.csv"), sep=",")   # Country data
sdata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_SurveillanceSites_071123 EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_old = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data

idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data

# AMR data
adata = read.csv(paste0(dirData, "/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaAC_071123 EV.csv"), sep=",")   # Country AMR data

# DESCRIBE DATA
##############################################################

# GLASS Implementation data
##############################################################
names(idata)

# R0 characteristics among those with four observation rounds
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
                   amr_glass_hospitals_number +
                   amr_glass_outpatient_cons_number+
                   lab_number_data_call+
                   local_lab_eqa_number_data_call| factor(AMR_GLASS_AST), data=idata_country%>%filter(!is.na(AMR_GLASS_AST)))

im_table1 # So 78 countries report to GLASS among those that have also filled out the implementation survey
table(idata_country$AMR_GLASS_AST,useNA="always") # 97 countries reported to GLASS at least one isolate with AST

write.table(im_table1, paste0(dirOutput,"/Descriptive/im_2024_table1.csv"), col.names = T, row.names=F, append= F, sep=';')

# Population data
##############################################################


# GLASS AMR Country data
##############################################################

# Countries that reported
creport = unique(adata$Iso3[which(adata$SpecimenIsolateswithAST>1)])

# GLASS Surveillance sites
##############################################################


# Universal health coverage data
##############################################################
# Source:
# https://www.who.int/publications/i/item/9789240080379
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-index-of-service-coverage


