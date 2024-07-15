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
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables"

##############################################################
# LOAD IN DATA
##############################################################
pdata = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Popdta_071123 EV.csv"), sep=",")       # Population data
cdata = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Countrydta_071123 EV.csv"), sep=",")   # Country data
sdata = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_SurveillanceSites_071123 EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data

idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data

# AMR data
adataAC = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaAC_071123 EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaDM_071123 EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaINT_071123 EV.csv"), sep=",")   # Country AMR data


##############################################################
# PREAMBLE
##############################################################

# Country data
##############################################################
# Remove empty columns
cdata = cdata %>% select(-c(X, X.1,X.2))

# AMR data
##############################################################
# Link country data
adataAC = left_join(adataAC, cdata, by="Iso3") %>% select(-c())

# DESCRIBE DATA
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


# Health Quality and Safety Index (IHME data)


###############################################################
# Account for uncertainty by calculating inverse variance weights
###############################################################


# Display the results
print(data)
cat("Combined Resistance Rate (p_IVW):", p_IVW, "\n")
cat("Variance of Combined Resistance Rate (var_p_IVW):", var_p_IVW, "\n")
