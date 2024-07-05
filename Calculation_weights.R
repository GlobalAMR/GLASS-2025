#################################################
# GLASS REPORT - Calculation of weights
#################################################

# Author: Esther van Kleef
# Date created: 5 July 2024
# Date latest update: 

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer)

# Locate directories
dirData = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirOutput = "C:/Users/Esther/World Health Organization/GLASS Data Visualization - Documents/General/Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/"

# Load in data
# Population data
##############################################################

# GLASS Implementation data
##############################################################

# Universal health coverage data
##############################################################
# Source:
# https://www.who.int/publications/i/item/9789240080379
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-index-of-service-coverage


