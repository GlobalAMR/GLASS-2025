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

# Aim of this section is to contextualise AMR data contributed to GLASS, i.e. does passive surveillance
# result in improving surveillance activities?:

# Participation of countries, territories and areas in GLASS has increased steadily each year 
# since the initiation of GLASS in 2016. The absolute number of infection isolates with 
# antibiotic susceptibility test results reported to GLASS has also risen.  
# However, it is unclear whether reliance on passive surveillance alone has resulted in improved surveillance
# coverage at national level over time. Specific considerations include: 

# 1) Identify the syndromes and pathogen-antibiotic combinations for which improved surveillance coverage 
#    may have occurred. 
# 2) Determine which regions have shown these improvements.  
# 3) Determine any differences in testing coverage over time among gender groups. 

# Found the below in the visualisation notes, do we want to define thresholds?
# 1) Show where coverage has increased or not (facilities, facility types, HC utilisation)
# 2) potentially defining “thresholds” above or below which targeted action 
#    is needed, and standards to track progress of routine surveillance


# 3.2	National AMR surveillance implementation indicators
###################################################################

# 3.3	GLASS-AMR surveillance coverage 
###################################################################

# 4	GLOBAL AND REGIONAL RESISTANCE TO ANTIBIOTICS
###################################################################

# Generate a single global estimate of resistance by using weights to account for 
# testing coverage and potential bias in the data from each setting
# Hence also allow for comparison between regions and potentially over time.

# 4.1	Resistance to antibiotics under surveillance in 2022	 
###################################################################

# 4.2	Time series of resistance to selected antibiotics, 2017-2022
###################################################################

