#######################################
# TRIAL FIGURES
#######################################
rm(list=ls())

pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
       table1, flextable, magrittr, officer)

dirData = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirOutput = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/"

EI_AMRdtaAC <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaAC_071123 EV.csv")) # All cases
EI_AMRdtaDM <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaDM_071123 EV.csv")) # Demographics (age and sex specified)
EI_AMRdtaINT <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaINT_071123 EV.csv")) # Interaction between age and sex specified

names(EI_AMRdtaAC)

###############################

table(EI_AMRdtaAC$Specimen)
