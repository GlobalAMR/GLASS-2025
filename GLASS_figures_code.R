#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
       table1, flextable, magrittr, officer)

# Locate directories
dirData = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirOutput = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/"

# Load in data
EI_AMRdtaAC <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaAC_071123 EV.csv")) # All cases
EI_AMRdtaDM <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaDM_071123 EV.csv")) # Demographics (age and sex specified)
EI_AMRdtaINT <- read_csv(paste0(dirData,"/Final_Curated_Data_GLASS_2023_EV/EI_AMRdtaINT_071123 EV.csv")) # Interaction between age and sex specified

names(EI_AMRdtaAC)

###################################################################
# FIGURES
###################################################################

# 3	GLOBAL AND REGIONAL AMR SURVEILLANCE COVERAGE	 
###################################################################

# 3.1	Participation in GLASS 
###################################################################

# Aim of this section is to 

# 3.2	National AMR surveillance implementation indicators
###################################################################

# 3.3	GLASS-AMR surveillance coverage 
###################################################################

# 4	GLOBAL AND REGIONAL RESISTANCE TO ANTIBIOTICS
###################################################################

# 4.1	Resistance to antibiotics under surveillance in 2022	 
###################################################################

# 4.2	Time series of resistance to selected antibiotics, 2017-2022
###################################################################

