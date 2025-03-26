######################################################
# Age-sex standardised testing rate
######################################################

# Author: Esther van Kleef
# Date last updated: 24 March 2025

#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, RColorBrewer,
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, purrr, brms, cowplot, ggrepel, grid, dplyr, wesanderson,
               patchwork, ggh4x, rio)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO= "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"


#dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/2021/"

# Load in functions
source("./0_GLASS_functions.R")
source("./0_multiplot.R")

##############################################################
# LOAD IN DATA
##############################################################

# Population data
pdata = read.csv(paste0(dirDataNewO, "/EI_Popdta_110325_EV.csv"), sep=",")       # Population data
pdataDM = read.csv(paste0(dirDataNew, "/EI_PopdtaDM_140325_EV.csv"), sep=",")       # Population data

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_AST_140325_EV.csv"), sep=",")   # Country data

# Surveillance indicator data
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_Country_140325_EV.csv"), sep=",")                   # Implementation data
# Including healthcare usage data
udata = read.csv(paste0(dirDataNew, "/EI_Implementation_usage_dta_140325_EV.csv"), sep=",")

sdata = read.csv(paste0(dirDataNew, "/EI_SurveillanceSites_140325_EV.csv"), sep=",") # Surveillance sites
#idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data
#idata_country = read.csv(paste0(dirDataNew,"/EI_Implementationdta_Country_030924_EV.csv"), sep=",")                   # Implementation data

# HAQI data
#haqidata = read.csv(paste0(dirDataRaw, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_Pop_country_HAQI_140325_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_Country_140325_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_Pop_Country_140325_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "/updated_summary_dbc_longformat.csv"))

# EAR data
#edata = read_excel(paste0(dirDataNew, "/EAR_events_2019_2024_EV.xlsx"))

# rrates 2021
#rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
#rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
#  Q1 = as.numeric(Q1),
#  Q3 = as.numeric(Q3),
#  median = as.numeric(median)
#)

# Drug bug combinations to include in report
combinations2022 = dbdata %>% #filter(Period %in% c("2016-","2016-2022")) %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
         combined = ifelse(combined == "BLOOD-Staphylococcus aureus-Methicillin-resistance",
                           "BLOOD-Staphylococcus aureus-Methicillin resistance", combined))

# Standard population
# See also here https://epirhandbook.com/new_pages/standardization.html
standard_pop_data <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/world_standard_population_by_sex.csv")

adataAS =  read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES_NOTESTING.csv"), sep=",")   # Country AMR data

###################################################################
# PREAMBLE
###################################################################

# Link bed days with country data to get regions
#bdays = left_join(bdays, cdata, by="Iso3")

#adataAC = left_join(adataAC, bdays%>%select(Iso3, Year, BedDensityPer100000Pop), by=c("Iso3", "Year"))
#adataAC = left_join(adataAC, udata%>%select(Iso3, Year, amr_glass_inpatient_day_number), by=c("Iso3", "Year"))


# Take out all the lines which specify the origin of the sample as well have a missing age
# (as we don't have denominator population data for those)
# Add InReport to specify if drug-bug combination should be reported on
#adataNT$AntibioticName <- sub(" $", "", adataNT$AntibioticName)

# Filter out all origin categories
#filtered_cat <- adataNT$DemographicsOrigin[!grepl("_CO$|_HO$|_UnkOrigin$", adataNT$DemographicsOrigin)]
#unique(filtered_cat)

#d1 = adataNT %>% filter(!is.na(TotalPopulation)|DemographicsOrigin%in%filtered_cat) %>%
#  mutate(
#    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
#    amr_rate = Resistant/InterpretableAST, 
#    BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
#  )

# Filter out those countries with <10 isolates
#d2 = adataAC %>% filter(InterpretableAST>10) %>%
#  mutate(
#    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)
#  )

# Filter d1 based on the matching combinations in df2
# adataAS = d1 %>%
#   semi_join(d2, by = c("combined", "Iso3", "Year"))
# 
# # Check if all are matched
# unmatched_d2 <- d2 %>%
#   anti_join(d1, by = c("combined", "Iso3", "Year")) # All matched
# 
# # Check if numbers add up for totals in adataAS with adataAC
# d3 = adataAS %>%group_by(Iso3, Year, combined) %>%
#   summarise(InterpretableAST=sum(InterpretableAST, na.rm=T),
#             Resistant=sum(Resistant, na.rm = T))
# 
# d2$Key <- paste(d2$Iso3, d2$Year, d2$combined)
# d3$Key <- paste(d3$Iso3, d3$Year, d3$combined)

#d4 = left_join(d2 %>% select(Iso3, Year, combined, InterpretableAST, Resistant, Key),d3, by=c("Iso3","Year", "combined", "Key")) %>%
#  mutate(
#    diff_AST = InterpretableAST.x - InterpretableAST.y,
#    diff_R = Resistant.x - Resistant.y
#  )

#table(d4$diff_AST)
#table(d4$diff_R)

#View(d4 %>%filter(diff_AST!=0))
# For which countries is there a difference?
#unique(d4$Key[d4$diff_AST!=0])
#unique(d4$combined[d4$diff_AST!=0]) # Only for the classes, something is not adding up, so I have miscalculated something there.
#unique(d4$Iso3[d4$diff_AST!=0])
#unique(d4$Year[d4$diff_AST!=0])

# FOR NOW, LEAVE THE CLASSES, WILL RECALCULATE THOSE
#combined_not_run = unique(d4$combined[d4$diff_AST!=0])
#combined_not_run

# Make age a numeric co-variate so we can fit a non-linear trend
# adataAS <- adataAS %>%
#   mutate(
#     AgeCat10 = factor(AgeCat10, labels=c("0<04","05<14","15<24","25<34","35<44","45<54","55<64","65<74","75<84","85+",   
#                                          "UnkAge"),
#                       levels=c("0<04","05<14","15<24","25<34","35<44","45<54","55<64","65<74","75<84","85+",   
#                                "UnkAge")),
#     AgeCat10 = relevel(AgeCat10, ref="0<04"),
#     s_AgeCat10 = case_when(
#       AgeCat10=="0<04" ~ as.numeric(1),
#       AgeCat10=="05<14" ~ as.numeric(2),
#       AgeCat10=="15<24" ~ as.numeric(3),
#       AgeCat10=="25<34" ~ as.numeric(4),
#       AgeCat10=="35<44" ~ as.numeric(5),
#       AgeCat10=="45<54" ~ as.numeric(6),
#       AgeCat10=="55<64" ~ as.numeric(7),
#       AgeCat10=="65<74" ~ as.numeric(8),
#       AgeCat10=="75<84" ~ as.numeric(9),
#       AgeCat10=="85+" ~ as.numeric(10),
#       AgeCat10=="UnkAge" ~ NA
#     ))
# 
# table(adataAS$AgeCat10, adataAS$s_AgeCat10, useNA="always")

standard_pop_data = standard_pop_data %>%
  mutate(
    AgeCat10 = case_when(
      AgeGroup %in% c("0-4 years") ~ "0<04",
      AgeGroup %in% c("5-9 years","10-14 years") ~ "05<14",
      AgeGroup %in% c("15-19 years", "20-24 years") ~ "15<24",
      AgeGroup %in% c("25-29 years", "30-34 years") ~ "25<34",
      AgeGroup %in% c("35-39 years", "40-44 years") ~ "35<44",
      AgeGroup %in% c("45-49 years", "50-54 years") ~ "45<54",
      AgeGroup %in% c("55-59 years", "60-64 years") ~ "55<64",
      AgeGroup %in% c("65-69 years", "70-74 years") ~ "65<74",
      AgeGroup %in% c("75-79 years", "80-84 years") ~ "75<84",
      AgeGroup %in% c("85plus years") ~ "85+"
    )
  )

table(standard_pop_data$AgeGroup,standard_pop_data$AgeCat10)

pdataDMstandard <- standard_pop_data %>%
  group_by(Sex, AgeCat10) %>%
  summarise(TotalPopulationStandard = sum(WorldStandardPopulation), .groups = "drop") %>%
  mutate(AgeCat10 = factor(AgeCat10, levels = c("0<04", "05<14", "15<24", "25<34", "35<44", "45<54", "55<64", "65<74", "75<84", "85+", "UnkAge"))) %>%
  mutate(AgeCat10 = relevel(AgeCat10, ref = "0<04"))

#adataAS = left_join(adataAS, cdata %>%select(c(Iso3, WHORegionCode, WHORegionName, IncomeWorldBankJune2022)))

#adataAS = adataAS %>% 
#  mutate(
#  combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)
#)
###################################################################
# COLOUR SPECIFICATION
###################################################################

palette <- wes_palette("Darjeeling1", n = 5)
palette2 <- wes_palette("BottleRocket2", n = 1)
palette3 <- wes_palette("GrandBudapest1", n = 2)[2]
palette4 <- wes_palette("BottleRocket2", n = 2)[2]
palette5 = c(palette3, palette[2],palette2,palette[5], palette[4],palette4)

palette_map = c(palette2, palette[2],palette3)

# Define colors for each WHORegionCode
facet_colors <- c(
  "AFR" = palette5[1],
  "AMR" = palette5[2],
  "EMR" = palette5[3],
  "EUR" = palette5[4],
  "SEA" = palette5[5],
  "WPR" = palette5[6]
)

#################################################################################################
# PREAMBLE
#################################################################################################
adataAC = adataAC %>% 
  #filter(!Id %in% c("IdKOS2016","IdKOS2017","IdKOS2018","IdKOS2019","IdKOS2020")) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No"),
    amr_rate = Resistant/InterpretableAST, 
    # The above rates are based on very small numbers
    # Would be better to calculate prevalence by age globally
    BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  )


adataAS = left_join(adataAS, pdataDMstandard, by=c("AgeCat10", "Sex"))


# First check how many have InterpretableAST below 10, to see whether we need larger age-groups. Combine sex groups, is the underlying population is probably 50-50 male-female 
# For most countries.
# Then also plot this by year, to see if different.
#----------------------------------------------------------------------------------------------------

# First plot raw rates
# BSI
p1 <- plot_amr_test(adataAC, "Escherichia coli", "BLOOD", palette5, xaxis="BCI_1000000pop")
p2 <- plot_amr_test(adataAC, "Klebsiella pneumoniae", "BLOOD", palette5, xaxis="BCI_1000000pop")
p3 <- plot_amr_test(adataAC, "Salmonella spp.", "BLOOD", palette5, xaxis="BCI_1000000pop")
p4 <- plot_amr_test(adataAC, "Acinetobacter spp.", "BLOOD", palette5, xaxis="BCI_1000000pop")
p5 <- plot_amr_test(adataAC, "Staphylococcus aureus", "BLOOD", palette5, xaxis="BCI_1000000pop")
p6 <- plot_amr_test(adataAC, "Streptococcus pneumoniae", "BLOOD", palette5, xaxis="BCI_1000000pop")

# UTI
p7 <- plot_amr_test(adataAC, "Escherichia coli", "URINE", palette5, xaxis="BCI_1000000pop")
p8 <- plot_amr_test(adataAC, "Klebsiella pneumoniae", "URINE", palette5, xaxis="BCI_1000000pop")

# GI
p9 <- plot_amr_test(adataAC, "Shigella spp.", "STOOL", palette5, xaxis="BCI_1000000pop")
p10 <- plot_amr_test(adataAC, "Salmonella spp.", "STOOL", palette5, xaxis="BCI_1000000pop")

# GONORRHOEA
p11 <- plot_amr_test(adataAC, "Neisseria gonorrhoeae", "UROGENITAL", palette5, xaxis="BCI_1000000pop")

# Combine plots with patchwork
combined_plot <- p1/ p2 /p3 / p4 / p5 / p6 
combined_plotUTI <- p7/p8
combined_plotGI_GON <- p9/p10/p11


# Save as PDF
ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Testing_vs_amrBLOOD.pdf"), 
                   plot = combined_plot, width = 12, height = 40)

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Testing_vs_amrUTI.pdf"), 
       plot = combined_plotUTI, width = 12, height = 30)

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Testing_vs_amrGI_GON.pdf"), 
       plot = combined_plotGI_GON, width = 12, height = 30)

# APPROACH
# -------------------------------------------------------------------------------------------

# Explore if it makes sense to standardise according to the testing coverage proxies for each of the pathogens and specimens, i.e.:
# BLOOD
# * Acinetobacter spp = Carbapenems
# * E. coli = Third-generation cephalosporins OR Carbapenems
# * Klebsiella = Third-generation cephalosporins OR Carbapenems
# * Salmonella spp = Fluoroquinelones
# * Staphylococcus aureus = Cefoxitin
# * Streptococcus pneumoniae = Penicillins

# STOOL
# * Shigella spp. = Third-generation cephalosporins
# * Salmonella spp. = Fluoroquinelones

# URINE
# * E. coli = Third-generation cephalosporins OR Carbapenems
# * Klebsiella = Third-generation cephalosporins OR Carbapenems

# UROGENITAL
# * Neisseria gonorrhoeae = Macrolides

# FIRST OVERALL
#--------------------------------------------------------------------------------------------
# Age categories (for testing rate, can go for age-standardisation only, as for
# most countries, population men and women is 50-50 anyways). However, out of curiousity 
# Check how many age-sex strata have more than 10 isolates
enough_age = adataAS %>% 
  group_by(Year, WHORegionCode, Iso3, Specimen, PathogenName, AntibioticName, AgeCat10) %>%
  summarise(InterpretableAST = sum(InterpretableAST)) %>%
  mutate(enough = ifelse(InterpretableAST>=10, "Yes", "No")) 

# How many age
enough_age_sum = enough_age %>% filter(AgeCat10!="UnkAge") %>%
  group_by(Year, Iso3, enough) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

p1 = ggplot(enough_age_sum, aes(x = Year, y = proportion, fill = enough)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Proportion", fill = "Enough", title = "Fraction of age-sex strata with >10 isolates") +
  theme_minimal() +
  facet_wrap(~Iso3)
p1

pdf(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/proportion_with10isolatesormore_age_strata.pdf"))
print(p1)
dev.off()

# Make bigger age categories: 0-4; 5-14; 15-64; 65+
adataAS <- adataAS %>%
  mutate(AgeCatBig = case_when(
    AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
    AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
    AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
    TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
  ))
table(adataAS$AgeCat10, adataAS$AgeCatBig)

enough_agebig = adataAS %>% ungroup()%>% filter(AgeCatBig!="UnkAge") %>%
  group_by(WHORegionCode, Iso3, Year, Specimen, PathogenName, AntibioticName, AgeCatBig) %>%
  summarise(InterpretableAST = sum(InterpretableAST)) %>%
  mutate(enough = ifelse(InterpretableAST>=10, "Yes", "No")) 

enough_agebig_sum = enough_agebig %>%
  group_by(WHORegionCode, Iso3, Year, enough) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

p2 = ggplot(enough_agebig_sum, aes(x = Year, y = proportion, fill = enough)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Proportion", fill = "Enough", title = "Fraction of age-sex strata with >10 isolates") +
  theme_minimal() +
  facet_wrap(~Iso3)
p2

# Show counts of not enough by age group
p3 = ggplot(enough_agebig, aes(x = AgeCatBig, fill = enough)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(x = "Age Category", y = "Count", fill = "Count", title = "Count of age strata with >10 isolates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  facet_wrap(~Iso3)
p3

pdf(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/proportion_with10isolatesormore_age_stratabig.pdf"))
print(p2)
dev.off()

# Now select for the proxy pathogens and only age-strata, as well as make selection
# based on how many total isolates per drug(class)-bug and country (not per age category)

# BLOOD
# -----------------------------
# Acinetobacter - Imipenem
# Escherichia coli - Cefotaxime
# Klebsiella pneumoniae - Cefotaxime
# Salmonella spp. - Ciprofloxacin
# Staphylococcus aureus - Cefoxitin
# Streptococcus pneumoniae - Penicillin G

# STOOL
# ---------------------------------
# Salmonella spp. - Ciprofloxacin
# Shigella spp. - Ciprofloxacin

# URINE
# ---------------------------------
# E. coli - Cefotaxime
# Klebsiella pneumoniae - Cefotaxime

# URIGENITAL
# ---------------------------------
# Neisseria Gonorrhoeae - Ceftriaxone


# GROUPING BY ANTIBIOTICS THAT ARE ALWAYS TESTED PER SYNDROME
#------------------------------------------------------------------------------------------------------
# d1 <- adataAS %>%
#   mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") & (Specimen =="BLOOD")& (PathogenName=="Acinetobacter spp."|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Carbapenems',
#                               (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Neisseria gonorrhoeae"|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Third generation cephalosporins',
#                               (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Shigella spp.") ~ 'Third generation cephalosporins',
#                               (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (PathogenName=="Salmonella spp.") ~ 'Fluoroquinolones',
#                               (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Fluoroquinolones',
#                               (Antibiotic =="J01FA" | Antibiotic =="AZM") & (PathogenName=="Neisseria gonorrhoeae") ~ 'Macrolides',
#                               (Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX") & (PathogenName=="Staphylococcus aureus") ~ 'Methicillin resistance',
#                               (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") & (PathogenName=="Streptococcus pneumoniae") ~ 'Penicillins',
#                               (Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Sulfonamides and trimethoprim'))
# 
# table(d1$PathogenName, d1$Grouping)
# table(d1$Grouping, useNA="always")
# 
# d1<- d1%>%
#   filter(!is.na(Grouping))%>% 
#   droplevels()
# 
# d2 <- select(d1, c(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping, TotalPathogenIsolates, InterpretableAST, TotalPopulation))
# 
# d2 <- d2  %>%
#   filter(TotalPathogenIsolates>0)%>% 
#   droplevels()
# 
# d3 <- select(d2, c(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping, InterpretableAST, TotalPopulation))
# 
# d4 <- d3 %>% 
#   as.data.frame() %>%
#   group_by(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping) %>% 
#   summarize_all(max) %>%
#   as.data.frame() 
# 
# 
# # Make bigger age categories: 0-4; 5-14; 15-64; 65+
# d4 <- d4 %>%
#   mutate(AgeCatBig = case_when(
#     AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
#     AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
#     AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
#     TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
#   ))
# 
# table(d4$AgeCat10, d4$AgeCatBig)
# # Age categories
# 
# # Consider enough if total_ast >10 (and missing age <15%?)
# # Collapse over all years
# enough_age_small <- d4 %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, AgeCat10) %>%
#   summarise(
#     total_AST = sum(InterpretableAST, na.rm = TRUE),
#     #p_missingage = sum(InterpretableAST[AgeCatBig == "UnkAge"], na.rm = TRUE) /  sum(InterpretableAST, na.rm = TRUE),
#     enough = ifelse(total_AST >= 10, "Yes", "No"),
#     .groups = "drop"  # Optional: Ungroup after summarizing
#   ) %>%
#   distinct(Iso3, AgeCat10, Specimen, PathogenName, Grouping, total_AST, enough)
# 
# enough_age_small <- d4 %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, AgeCat10) %>%
#   summarise(
#     total_AST = sum(InterpretableAST, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping) %>%
#   mutate(
#     proportion_ASTAge = total_AST / sum(total_AST),  # Calculate proportion within each AgeCat10
#     enough = ifelse(total_AST >= 10, "Yes", "No"),
#     p_age_missingok = ifelse(proportion_ASTAge <0.15, "Yes", "No"),
#     p_age_missingok = ifelse(AgeCat10!="UnkAge", NA, p_age_missingok)
#   ) 
# 
# enough_age <- d4 %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, AgeCatBig) %>%
#   summarise(
#     total_AST = sum(InterpretableAST, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping) %>%
#   mutate(
#     proportion_ASTAge = total_AST / sum(total_AST),  # Calculate proportion within each AgeCat10
#     enough = ifelse(total_AST >= 10, "Yes", "No"),
#     p_age_missingok = ifelse(proportion_ASTAge <0.15, "Yes", "No"),
#     p_age_missingok = ifelse(AgeCatBig!="UnkAge", NA, p_age_missingok)
#   ) 
# 
# table(enough_age$PathogenName[enough_age$AgeCatBig!="UnkAge"],enough_age$enough[enough_age$AgeCatBig!="UnkAge"])
# 
# summary(enough_age$total_AST)
# 
# # Join with population data
# pdataAge = pdataDM %>%
#   mutate(AgeCatBig = case_when(
#     AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
#     AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
#     AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
#     TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
#   )) %>%
#   ungroup() %>%
#   group_by(Iso3, AgeCatBig) %>%
#   summarise(
#     TotalPopulation = sum(TotalPopulation)
#   )
# 
# standard_pop_data_AgeBig = standard_pop_data %>%
#   mutate(AgeCatBig = case_when(
#     AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
#     AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
#     AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
#     TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
#   )) %>%
#   ungroup() %>%
#   group_by(AgeCatBig) %>%
#   summarise(
#     TotalPopulationStandard = sum(WorldStandardPopulation)
#   )
# 

# enough_age = left_join(enough_age, pdataAge, by=c("Iso3", "AgeCatBig"))
# enough_age = left_join(enough_age, standard_pop_data_AgeBig, by=c("AgeCatBig"))
# 
# 
# # CHECK IF IDEA TO use the medians per region to fill up the gaps for those where there is not enough ASTs
# # i.e. comparable age distributions within countries in a region?
# 
# # Smaller age groups
# enough_age_small$combined = paste0(enough_age_small$Specimen,"-", enough_age_small$PathogenName, "-", enough_age_small$Grouping)
# 
# plots <- list()
# 
# for(i in unique(enough_age_small$combined)){
#   d <- enough_age_small %>% filter(combined == i)
#   
#   p <- ggplot(d, aes(x = Iso3, y = proportion_ASTAge, fill = AgeCat10)) +
#     geom_bar(stat = "identity", position = "fill") +  # stacked bars to see age distribution
#     labs(
#       title = "Proportional Distribution of Total AST by Age Group within Regions and Countries",
#       subtitle = i,
#       x = "Country (Iso3)",
#       y = "Proportion of Total AST",
#       fill = "Age Group"
#     ) +
#     facet_wrap(. ~ WHORegionCode, scales = "free_x", ncol = 3) +  # separate regions with country comparison
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   
#   # Store plot in list
#   plots[[i]] <- p
# }
# 
# pdf(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/age_distributions_testing_proxies_smallagegr.pdf"))
# for(i in 1:length(plots)){
#   print(plots[[i]])
# }
# dev.off()
# 
# # Interprepation
# # For certain syndromes and pathogens, AFRO and EMRO younger age groups, notably for:
# # - Blood Salmonella - Fluoroquinelones
# # - Blood MRSA
# # - Blood E. coli/knp - 3GC/Carb
# # - Blood A.bacter - Carb
# # - Stool shigella - 3GC
# # - Urine E.coli / Kpn - 3GC
# 
# # BIGGER AGE GROUPS
# enough_age$combined = paste0(enough_age$Specimen,"-", enough_age$PathogenName, "-", enough_age$Grouping)
# 
# plots <- list()
# 
# for(i in unique(enough_age$combined)){
#   d <- enough_age %>% filter(combined == i)
#   
#   p <- ggplot(d, aes(x = Iso3, y = proportion_ASTAge, fill = AgeCatBig)) +
#     geom_bar(stat = "identity", position = "fill") +  # stacked bars to see age distribution
#     labs(
#       title = "Proportional Distribution of Total AST by Age Group within Regions and Countries",
#       subtitle = i,
#       x = "Country (Iso3)",
#       y = "Proportion of Total AST",
#       fill = "Age Group"
#     ) +
#     facet_wrap(. ~ WHORegionCode, scales = "free_x", ncol = 3) +  # separate regions with country comparison
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   
#   # Store plot in list
#   plots[[i]] <- p
# }
# 
# pdf(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/age_distributions_testing_proxies.pdf"))
# for(i in 1:length(plots)){
#   print(plots[[i]])
# }
# dev.off()
# 
# 
# summary_insufficient <- enough_age %>% filter(!AgeCatBig=="UnkAge") %>%
#   group_by(Iso3, combined) %>%  # Group by country
#   summarise(
#     has_insufficient = any(enough == "No")  # Check if any combination has insufficient samples
#   ) %>%
#   filter(has_insufficient)  # Filter to keep only countries with insufficient samples
# length(unique(summary_insufficient$Iso3)) # Seems that all countries do have for some of the drug bug combinations not enough samples
# 
# 
# # FOR THOSE COUNTRIES WHERE NOT ENOUGH DATA TO CALCULATE THE TESTING RATE, I WILL IMPUTE WITH THE REGIONAL ESTIMATE
# a1 = enough_age %>% filter(AgeCatBig!="UnkAge") %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, AgeCatBig,combined) %>%
#   mutate(
#     BCI_1000000pop = total_AST/TotalPopulation*1000000,
#     BCI_1000000pop = ifelse(enough=="No", NA, BCI_1000000pop)
#   ) %>%
#   group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping,combined) %>% 
#   summarise(
#     st_BCI_1000000pop = sum(BCI_1000000pop * (TotalPopulationStandard / sum(TotalPopulationStandard))) # this leaves out the countries with insufficient isolates for any of the age categories
#   )
# 
# ggplot(a1, aes(x = Iso3, y = st_BCI_1000000pop, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# table(is.na(a1$st_BCI_1000000pop))
# 
# 
# # Calculate regional medians
# a1m = a1 %>%
#   # Now get regional averages
#   group_by(WHORegionCode, Specimen, PathogenName, Grouping, combined) %>%
#   summarise(
#     st_BCI_1000000pop_median = median(st_BCI_1000000pop,na.rm=T)
#   )
# 
# p0 = ggplot(a1m, aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# # Compare with standardised rates when not accounting for any minimum number of isolates
# #----------------------------------------------------------------------------------------------------------------
# 
# # Calculate crude standardized rates without accounting for any minimum number of isolates
# st_rates <- adataAS %>% filter(InReport == "Yes") %>% ungroup() %>%
#   group_by(WHORegionCode, Iso3, Specimen,PathogenName, AntibioticName, combined) %>%
#   summarise(
#     # Calculate the sum of standardized testing rates
#     st_BCI_1000000pop = sum(BCI_1000000pop * (TotalPopulationStandard / sum(TotalPopulationStandard, na.rm = TRUE)), na.rm = TRUE),
#     .groups = "drop"  # Ensure grouping is dropped after summarizing
#   ) 
# 
# # Calculate regional medians
# st_ratesm = st_rates %>% 
#   # Now get regional averages
#   group_by(WHORegionCode, Specimen, PathogenName, AntibioticName, combined) %>%
#   summarise(
#     st_BCI_1000000pop_median = median(st_BCI_1000000pop,na.rm=T)
#   )
# 
# 
# p1 = ggplot(st_ratesm %>%filter(PathogenName=="Escherichia coli"), aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     subtitle = "Escherichia coli",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# p2 = ggplot(st_ratesm %>%filter(PathogenName=="Acinetobacter spp."), aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     subtitle = "Acinetobacter spp.",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# p3 = ggplot(st_ratesm %>%filter(PathogenName=="Klebsiella pneumoniae"), aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     subtitle = "Klebsiella pneumoniae",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# p4 = ggplot(st_ratesm %>%filter(PathogenName=="Neisseria gonorrhoeae"), aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     subtitle = "Neisseria gonorrhoeae",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# p5 = ggplot(st_ratesm %>%filter(PathogenName=="Shigella spp."), aes(x = WHORegionCode, y = st_BCI_1000000pop_median, color = WHORegionCode)) +
#   geom_point(size = 3, alpha = 0.7) +
#   labs(
#     title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
#     subtitle = "Shigella spp.",
#     x = "Country (Iso3)",
#     y = "Standardized BCI per 1,000,000 Population",
#     color = "Drug-Bug Group",
#     shape = "Specimen"
#   ) +
#   facet_wrap(~ combined, scales = "free", ncol=4) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_proxyclasses_all.pdf"), 
#        plot = p0, width = 12, height = 10)
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_crude_ecoli.pdf"), 
#        plot = p1, width = 12, height = 10)
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_crude_abacter.pdf"), 
#        plot = p2, width = 12, height = 8)
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_crude_kpn.pdf"), 
#        plot = p3, width = 12, height = 10)
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_crude_neiss.pdf"), 
#        plot = p4, width = 12, height = 8)
# 
# ggsave(file=paste0(dirOutput,"/Descriptive/Section3.3_Surveillance_coverage/st_testing_crude_shig.pdf"), 
#        plot = p5, width = 12, height = 8)

# Only for Neisseria geonorrhoea comes with different estimates for the crude vs class based regional averages. 
# That is as many countries did not have at least 10 isolates in all of the age groups when calculating this one.
# 


# AFTER EXPLORATION IN THE ABOVE, WHAT I WILL DO IS FIRST COMPUTE THE AVERAGE TESTING RATES OVER ALL YEARS FOR EACH COUNTRY
# THEN FOR THOSE COUNTRIES WHERE THERE ARE NOT AT LEAST 10 ISOLATES OR MANY MISSING AGE, I WILL TAKE THE REGIONAL STANDARDISED
# TESTING RATES

# GROUPING BY ANTIBIOTICS THAT ARE ALWAYS TESTED PER SYNDROME 
# USE FOR E. COLI and KPN 1 CLASS, i.e 3GC FOR THE ST_BCI_PerMillion IN REGRESSION MODELS
#------------------------------------------------------------------------------------------------------

# adataAS should be all countries with at least 10 isolates reported per drug-bug
d1 <- adataAS %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") & (Specimen =="BLOOD")& (PathogenName=="Acinetobacter spp."|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Neisseria gonorrhoeae"|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Shigella spp.") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (PathogenName=="Salmonella spp.") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") & (PathogenName=="Neisseria gonorrhoeae") ~ 'Macrolides',
                              (Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX") & (PathogenName=="Staphylococcus aureus") ~ 'Methicillin resistance',
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") & (PathogenName=="Streptococcus pneumoniae") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Sulfonamides and trimethoprim'))

table(d1$PathogenName, d1$Grouping)
table(d1$Grouping, useNA="always")

d1<- d1%>%
  filter(!is.na(Grouping))%>% 
  droplevels()

# Create bigger age categories in population data
pdataAge = pdataDM %>%
  mutate(AgeCatBig = case_when(
    AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
    AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
    AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
    TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
  )) %>%
  ungroup() %>%
  group_by(Iso3, AgeCatBig) %>%
  summarise(
    TotalPopulation = sum(TotalPopulation)
  )

standard_pop_data_AgeBig = standard_pop_data %>%
  mutate(AgeCatBig = case_when(
    AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
    AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
    AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
    TRUE ~ as.character(AgeCat10)  # Default case to return AgeCat10 as a character
  )) %>%
  ungroup() %>%
  group_by(AgeCatBig) %>%
  summarise(
    TotalPopulationStandard = sum(WorldStandardPopulation)
  )


d2 <- dplyr::select(d1, c(WHORegionCode, Iso3, Year, AgeCat10,  Sex, Specimen, PathogenName, Grouping, TotalPathogenIsolates, InterpretableAST, TotalPopulation))

# d2 <- d2  %>%
#   filter(TotalPathogenIsolates>0)%>% 
#   droplevels()

d3 <- dplyr::select(d2, c(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping, InterpretableAST, TotalPopulation))

d4 <- d3 %>% 
  as.data.frame() %>%
  group_by(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping) %>% 
  summarize_all(max) %>%
  as.data.frame() 


# # Make bigger age categories: 0-4; 5-14; 15-64; 65+
d4 <- d4 %>%
  mutate(AgeCatBig = case_when(
    AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
    AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
    AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
    TRUE ~ as.character(AgeCat10)
    ),  # Default case to return AgeCat10 as a character
    combined = paste0(Specimen,"-", PathogenName, "-", Grouping)
  )

table(d4$AgeCat10, d4$AgeCatBig, useNA='always')

# CONTINUE WORKING WITH d4 - Collapse over all years of data in bigger age categories
d5 <- d4 %>%
  group_by(WHORegionCode, Iso3, AgeCatBig, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    total_AST = sum(InterpretableAST, na.rm = TRUE),
  #  .groups = "drop"
  ) %>%
  group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, combined) %>%
  mutate(
    proportion_ASTAge = total_AST / sum(total_AST),  # Calculate proportion within each AgeCat10
    enough = ifelse(total_AST >= 10, "Yes", "No")
  )

da = d5 %>% filter(AgeCatBig=="UnkAge") %>%
  group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, combined) %>%
  summarise(p_age_ok = ifelse(proportion_ASTAge<0.15, "Yes", "No"),
            p_age_m = proportion_ASTAge)

d6 = left_join(d5,da)

d7 = d6 %>% filter(AgeCatBig!="UnkAge") %>%
  mutate(
    p_age_ok = ifelse(is.na(p_age_m), "Yes", p_age_ok),
    p_age_m = ifelse(is.na(p_age_m), 0, p_age_m),
    use_for_st = ifelse(p_age_ok == "Yes" & enough=="Yes", "Yes", "No"),
    use_for_st2 = ifelse(p_age_ok == "Yes", "Yes", "No"),
    use_for_st4 = ifelse(enough=="Yes", "Yes", "No")# Based on only max level of age missing only
)

table(d7$use_for_st)
table(d7$use_for_st2)
table(d7$use_for_st4)

# Criteria 1: Based on minimum 10 isolates per age group and not more than 15% missing
d_insuf <- d7 %>% filter(AgeCatBig!="UnkAge") %>%
 group_by(Iso3, combined) %>%  # Group by country
   summarise(
     has_insufficient = any(use_for_st == "No")  # Check if any combination has insufficient samples
   ) %>%
   filter(has_insufficient)  # Filter to keep only countries with insufficient samples

#length(unique(summary_insufficient$Iso3)) # Seems that all countries do have for some of the drug bug combinations not enough samples

counts <- d_insuf %>%
  group_by(combined) %>%
  summarise(Total = n_distinct(Iso3))

ggplot(counts, aes(x=reorder(combined,Total), y=Total)) + geom_bar(stat="identity") +
  labs(title = "Number of Countries with not enough data for age-standardisetion per drug-Bug Combination",
       subtitle = "criteria 1: Percentage missing age <15%; CTA with min 10 isolates per drug-bug and age-strata",
        x = "Drug-bug combination",
        y = "Number of countries",
        color = "Drug-Bug Group",
        shape = "Specimen"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

d_insuf = left_join(d_insuf, counts)
d_insuf = left_join(d_insuf, cdata%>%dplyr::select(WHORegionCode, Iso3))

p = ggplot(d_insuf, aes(x=reorder(Iso3,WHORegionCode), y=has_insufficient, fill=WHORegionCode)) + geom_bar(stat="identity") +
  labs(title = "Countries with not enough data for age-standardisation per antibiotic class-Pathogen Combination",
       subtitle = "criteria 1: Percentage missing age <15%; CTA with min 10 isolates per drug-bug and age-strata",
       y = "Country",
       x = "",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  geom_text(aes(
    x = Inf,  # Places text at the far right of each facet
    y = Inf,  # Places text at the top of each facet
    label = paste0("Total: ", Total)
  ), hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
  facet_wrap(~combined, scales="free_y",ncol=6) +
  coord_flip()+
  theme_minimal() 
p

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/CTA_not_enoughdata_agestand_testing_criteria1.pdf"), 
               plot = p, width = 25, height = 15)

table(d_insuf$Iso3, d_insuf$has_insufficient)


# Criteria 2: Based on minimum not more than 15% missing missing per age group only
d_insuf2 <- d7 %>% filter(AgeCatBig!="UnkAge") %>%
  group_by(Iso3, combined) %>%  # Group by country
  summarise(
    has_insufficient2 = any(use_for_st2 == "No")  # Check if any combination has insufficient samples
  ) %>%
  filter(has_insufficient2)  # Filter to keep only countries with insufficient samples

counts2 <- d_insuf2 %>%
  group_by(combined) %>%
  summarise(Total2 = n_distinct(Iso3))

ggplot(counts2, aes(x=reorder(combined,Total2), y=Total2)) + geom_bar(stat="identity") +
  labs(title = "Number of Countries with not enough data for age-standardisetion per drug-Bug Combination",
       subtitle = "criteria 2: Percentage missing age <15%; CTA with min 10 isolates per drug-bug",
       x = "Drug-bug combination",
       y = "Number of countries",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

d_insuf2 = left_join(d_insuf2, counts2)
d_insuf2 = left_join(d_insuf2, cdata%>%dplyr::select(WHORegionCode, Iso3))

p2 = ggplot(d_insuf2, aes(x=reorder(Iso3,WHORegionCode), y=has_insufficient2, fill=WHORegionCode)) + geom_bar(stat="identity") +
  labs(title = "Countries with not enough data for age-standardisation per antibiotic class-Pathogen Combination",
       subtitle = "criteria 2: Percentage missing age <15%; CTA with min 10 isolates per drug-bug",
       y = "Country",
       x = "",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  geom_text(aes(
    x = Inf,  # Places text at the far right of each facet
    y = Inf,  # Places text at the top of each facet
    label = paste0("Total: ", Total2)
  ), hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
  facet_wrap(~combined, scales="free_y",ncol=6) +
  coord_flip()+
  theme_minimal() 
p2

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/CTA_not_enoughdata_agestand_testing_criteria2.pdf"), 
       plot = p2, width = 25, height = 15)

# Go ahead with checking and comparing both criteria 
# (as sensitivity analyses will repeat this with >=50 isolates per drug bug combination)
d8 = left_join(d7, d_insuf)
d8 = left_join(d8, d_insuf2)

# Join with population estimates
d9 = left_join(d8, pdataAge, by = c("Iso3", "AgeCatBig"))
d10 = left_join(d9, standard_pop_data_AgeBig, by = c("AgeCatBig"))

# Now filter on those countries and drug-bug combinations that have enough data
# Criteria 1
d11 = d10 %>% filter(is.na(has_insufficient), AgeCatBig!="UnkAge") %>%
  mutate(BCI_million = total_AST/TotalPopulation*1000000)

st_rates = d11 %>% group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping,combined) %>% 
    summarise(
    st_BCI_million = sum(BCI_million * (TotalPopulationStandard / sum(TotalPopulationStandard)))
    )

# Now get regional averages
st_ratesm = st_rates %>%
  group_by(WHORegionCode, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    st_BCI_million_median = median(st_BCI_million)
  )%>%
  mutate(criteria = "Age_missing<15% & >=10 AST per age strata")

p0 = ggplot(st_ratesm, aes(x = WHORegionCode, y = st_BCI_million_median, color = WHORegionCode)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    subtitle = "criteria 1: Percentage missing age <15%; min 10 isolates per drug-bug and age strata",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p0

# Criteria 2
d12 = d10 %>% filter(is.na(has_insufficient2), AgeCatBig!="UnkAge") %>%
  mutate(BCI_million = total_AST/TotalPopulation*1000000)

st_rates2 = d12 %>% group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping,combined) %>% 
  summarise(
    st_BCI_million = sum(BCI_million * (TotalPopulationStandard / sum(TotalPopulationStandard)))
  )

# Now get regional averages
st_ratesm2 = st_rates2 %>%
  group_by(WHORegionCode, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    st_BCI_million_median = median(st_BCI_million)
  )%>%
  mutate(criteria = "Age_missing<15% & no minimum AST per age strata")

p1 = ggplot(st_ratesm2, aes(x = WHORegionCode, y = st_BCI_million_median, color = WHORegionCode)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    subtitle = "criteria 2: Percentage missing age <15%; CTA with min 10 isolates per drug-bug",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p1

st_ratesm = rbind(st_ratesm, st_ratesm2)

p2 = ggplot(st_ratesm, aes(x = WHORegionCode, y = st_BCI_million_median, color = criteria)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Regional_age_st_testing_criteria_compared.pdf"), 
       plot = p2, width = 25, height = 15)


# NOW USE THE APPROACH OF FILTERING OUT COUNTRIES WITH <50 isolates and prop.age missing >0.15% (sensitivity analyses)
#---------------------------------------------------------------------------------------------------

# Filter out those countries with <50 isolates
b1 = adataAC %>% filter(InterpretableAST>50) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)
  )

# Filter d1 based on the matching combinations in df2
c1 = adataAS %>%
  semi_join(b1, by = c("combined", "Iso3", "Year"))


c1 <- c1 %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") & (Specimen =="BLOOD")& (PathogenName=="Acinetobacter spp."|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Neisseria gonorrhoeae"|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Shigella spp.") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (PathogenName=="Salmonella spp.") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") & (PathogenName=="Neisseria gonorrhoeae") ~ 'Macrolides',
                              (Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX") & (PathogenName=="Staphylococcus aureus") ~ 'Methicillin resistance',
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") & (PathogenName=="Streptococcus pneumoniae") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Sulfonamides and trimethoprim'))

table(c1$PathogenName, c1$Grouping)

c1<- c1%>%
  filter(!is.na(Grouping))%>% 
  droplevels()

c2 <- dplyr::select(c1, c(WHORegionCode, Iso3, Year, AgeCat10,  Sex, Specimen, PathogenName, Grouping, TotalPathogenIsolates, InterpretableAST, TotalPopulation))

c3 <- dplyr::select(c2, c(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping, InterpretableAST, TotalPopulation))

c4 <- c3 %>% 
  as.data.frame() %>%
  group_by(WHORegionCode, Iso3, Year, AgeCat10, Sex, Specimen, PathogenName, Grouping) %>% 
  summarize_all(max) %>%
  as.data.frame() 


# # Make bigger age categories: 0-4; 5-14; 15-64; 65+
c4 <- c4 %>%
  mutate(AgeCatBig = case_when(
    AgeCat10 %in% c("0<04", "05<14") ~ "0<14",
    AgeCat10 %in% c("15<24","25<34", "35<44", "45<54", "55<64") ~ "15<64",
    AgeCat10 %in% c("65<74","75<84", "85+") ~ "65+",
    TRUE ~ as.character(AgeCat10)
  ),  # Default case to return AgeCat10 as a character
  combined = paste0(Specimen,"-", PathogenName, "-", Grouping)
  )

table(c4$AgeCat10, c4$AgeCatBig)

# CONTINUE WORKING WITH d4 - Collapse over all years of data in bigger age categories
c5 <- c4 %>%
  group_by(WHORegionCode, Iso3, AgeCatBig, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    total_AST = sum(InterpretableAST, na.rm = TRUE),
    #  .groups = "drop"
  ) %>%
  group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, combined) %>%
  mutate(
    proportion_ASTAge = total_AST / sum(total_AST),  # Calculate proportion within each AgeCat10
  )

da = c5 %>% filter(AgeCatBig=="UnkAge") %>%
  group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping, combined) %>%
  summarise(p_age_ok = ifelse(proportion_ASTAge<0.15, "Yes", "No"),
            p_age_m = proportion_ASTAge)

c6 = left_join(c5,da)

c7 = c6 %>% filter(AgeCatBig!="UnkAge") %>%
  mutate(
    p_age_ok = ifelse(is.na(p_age_m), "Yes", p_age_ok),
    p_age_m = ifelse(is.na(p_age_m), 0, p_age_m),
    use_for_st3 = ifelse(p_age_ok == "Yes", "Yes", "No") # Based on only max level of age missing only
  )

table(c7$use_for_st3)

c_insuf <- c7 %>% filter(AgeCatBig!="UnkAge") %>%
  group_by(Iso3, combined) %>%  # Group by country
  summarise(
    has_insufficient3 = any(use_for_st3 == "No")  # Check if any combination has insufficient samples
  ) %>%
  filter(has_insufficient3)  # Filter to keep only countries with insufficient samples

counts <- c_insuf %>%
  group_by(combined) %>%
  summarise(Total3 = n_distinct(Iso3))

ggplot(counts, aes(x=reorder(combined,Total3), y=Total3)) + geom_bar(stat="identity") +
  labs(title = "Number of Countries with not enough data for age-standardisetion per drug-Bug Combination",
       subtitle = "criteria 3: Percentage missing age <15%; CTA with min 50 isolates per drug-bug",
       x = "Drug-bug combination",
       y = "Number of countries",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

c_insuf = left_join(c_insuf, counts)
c_insuf = left_join(c_insuf, cdata%>%dplyr::select(WHORegionCode, Iso3))

p3 = ggplot(c_insuf, aes(x=reorder(Iso3,WHORegionCode), y=has_insufficient3, fill=WHORegionCode)) + geom_bar(stat="identity") +
  labs(title = "Countries with not enough data for age-standardisation per antibiotic class-Pathogen Combination",
       subtitle = "criteria 3: Percentage missing age <15%; CTA with min 50 isolates per drug-bug",
       y = "Country",
       x = "",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  geom_text(aes(
    x = Inf,  # Places text at the far right of each facet
    y = Inf,  # Places text at the top of each facet
    label = paste0("Total: ", Total3)
  ), hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
  facet_wrap(~combined, scales="free_y",ncol=6) +
  coord_flip()+
  theme_minimal() 
p3

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/CTA_not_enoughdata_agestand_testing_criteria3.pdf"), 
       plot = p3, width = 25, height = 15)

c8 = left_join(c7, c_insuf)

# Join with population estimates
c9 = left_join(c8, pdataAge, by = c("Iso3", "AgeCatBig"))
c10 = left_join(c9, standard_pop_data_AgeBig, by = c("AgeCatBig"))

# Now filter on those countries and drug-bug combinations that have enough data
# Criteria 3
c11 = c10 %>% filter(is.na(has_insufficient3), AgeCatBig!="UnkAge") %>%
  mutate(BCI_million = total_AST/TotalPopulation*1000000)

st_rates3 = c11 %>% group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping,combined) %>% 
  summarise(
    st_BCI_million = sum(BCI_million * (TotalPopulationStandard / sum(TotalPopulationStandard)))
  )

# Now get regional averages
st_ratesm3 = st_rates3 %>%
  group_by(WHORegionCode, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    st_BCI_million_median = median(st_BCI_million)
  )%>%
  mutate(criteria = "Age_missing<15% & >=50 AST per age strata")

p1 = ggplot(st_ratesm3, aes(x = WHORegionCode, y = st_BCI_million_median, color = WHORegionCode)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    subtitle = "criteria 3: Percentage missing age <15%; CTA with min 50 isolates per drug-bug",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p1

st_ratesm_all = rbind(st_ratesm, st_ratesm2, st_ratesm3)

p2 = ggplot(st_ratesm_all, aes(x = WHORegionCode, y = st_BCI_million_median, color = criteria)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Regional_age_st_testing_criteria_compared.pdf"), 
       plot = p2, width = 25, height = 15)

# FINAL APPROACH: USE ALL OBSERVATIONS WITH AT LEAST 10 OBSERVATIONS IN A STRATA, SO FOR COUNTRIES OK TO USE PARTIAL DATA
#-------------------------------------------------------------------------------------------------------------------------

# Criteria 4:
d_insuf4 <- d7 %>% filter(enough=="No") %>%  # So keep countries if one of strata has less than 10
 group_by(Iso3, combined) %>%  # Group by country
  summarise(
    has_insufficient4 = all(use_for_st4 == "No")  # Check if any combination has insufficient samples in all age categories per country
  ) %>%
  filter(has_insufficient4)  # Filter to keep only countries with insufficient samples
length(unique(d_insuf4$Iso3))

counts4 <- d_insuf4 %>%
  group_by(combined) %>%
  summarise(Total4 = n_distinct(Iso3))

ggplot(counts4, aes(x=reorder(combined,Total4), y=Total4)) + geom_bar(stat="identity") +
  labs(title = "Number of Countries with not enough data for age-standardisetion per drug-Bug Combination",
       subtitle = "criteria 4: Age category with min 10 isolates per drug-bug and age-strata (regardless if some strata per CTA do not meet this criteria",
       x = "Drug-bug combination",
       y = "Number of countries",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


d_insuf4 = left_join(d_insuf4, counts4)
d_insuf4 = left_join(d_insuf4, cdata%>%dplyr::select(WHORegionCode, Iso3))


p = ggplot(d_insuf4, aes(x=reorder(Iso3,WHORegionCode), y=has_insufficient4, fill=WHORegionCode)) + geom_bar(stat="identity") +
  labs(title = "Countries with not enough data for age-standardisation per antibiotic class-Pathogen Combination",
       subtitle = "criteria 4: CTA with at least some age groups with min 10 isolates per drug-bug and age-strata",
       y = "Country",
       x = "",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  geom_text(aes(
    x = Inf,  # Places text at the far right of each facet
    y = Inf,  # Places text at the top of each facet
    label = paste0("Total: ", Total4)
  ), hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
  facet_wrap(~combined, scales="free_y",ncol=6) +
  coord_flip()+
  theme_minimal() 
p

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/CTA_not_enoughdata_agestand_testing_criteria4.pdf"), 
       plot = p, width = 25, height = 15)

table(d_insuf4$Iso3, d_insuf4$has_insufficient4)

d8 = left_join(d7, d_insuf4)

# Join with population estimates
d9 = left_join(d8, pdataAge, by = c("Iso3", "AgeCatBig"))
d10 = left_join(d9, standard_pop_data_AgeBig, by = c("AgeCatBig"))

# Now filter on those countries and drug-bug combinations that have enough data
# Criteria 4
d11 = d10 %>% filter(use_for_st4=="Yes", AgeCatBig!="UnkAge") %>%
  mutate(BCI_million = total_AST/TotalPopulation*1000000)

st_rates4 = d11 %>% group_by(WHORegionCode, Iso3, Specimen, PathogenName, Grouping,combined) %>% 
  summarise(
    st_BCI_million = sum(BCI_million * (TotalPopulationStandard / sum(TotalPopulationStandard)))
  )

# Now get regional averages
st_ratesm4 = st_rates4 %>%
  group_by(WHORegionCode, Specimen, PathogenName, Grouping, combined) %>%
  summarise(
    st_BCI_million_median = median(st_BCI_million)
  )%>%
  mutate(criteria = ">=10 AST per age strata")

p0 = ggplot(st_ratesm4, aes(x = WHORegionCode, y = st_BCI_million_median, color = WHORegionCode)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    subtitle = "criteria 4: min 10 isolates per drug-bug and age strata",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p0

st_ratesm_all = rbind(st_ratesm, st_ratesm2, st_ratesm3, st_ratesm4)

p2 = ggplot(st_ratesm_all, aes(x = WHORegionCode, y = st_BCI_million_median, color = criteria)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Standardized BCI per 1,000,000 Population by Country and Drug-Bug Combination",
    x = "Country (Iso3)",
    y = "Standardized BCI per 1,000,000 Population",
    color = "Drug-Bug Group",
    shape = "Specimen"
  ) +
  facet_wrap(~ combined, scales = "free", ncol=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Regional_age_st_testing_criteria_compared.pdf"), 
       plot = p2, width = 25, height = 15)


# OK HAVE CALCULATED AGE STANDARDISED TESTING RATES PER REGION WITH DIFFERENT CRITERIA
# Will go ahead with criteria 4. Seems most in line with crude rates per drug-bug and more stable

# IMPUTE AGE-STANDARDISED RATES FOR DRUG-BUG COMBINATIONS WHERE NOT ENOUGH DATA
#----------------------------------------------------------------------------------------------------
f1 <- adataAS 

# Select standardised rates of interest (i.e. chose 1 antibiotic class per pathogen)
e1 = st_rates4 %>% filter(!combined %in% c("BLOOD-Klebsiella pneumoniae-Carbapenems", "BLOOD-Escherichia coli-Carbapenems",
                                           "URINE-Escherichia coli-Sulfonamides and trimethoprim","URINE-Klebsiella pneumoniae-Sulfonamides and trimethoprim",
                                           "URINE-Escherichia coli-Fluoroquinolones","URINE-Klebsiella pneumoniae-Fluoroquinolones",
                                           "UROGENITAL-Neisseria gonorrhoeae-Macrolides")) 

# Of note, based on email Daniel Marcano Zamora (23 Sept 2024), I think I should take fluoroquinelones for shigella
unique(e1$combined)

e2 = e1%>%ungroup()%>%dplyr::select(c(Iso3,Specimen,PathogenName, st_BCI_million))

# Combine by Specimen and pathogen, so to get age-standardised testing rate at that level
f2 = left_join(f1,e2, by=c("Iso3", "Specimen", "PathogenName"))

# Combine by specimen and pathogen the regional age-standardised rates so those can be imputed

g1 = st_ratesm4 %>% filter(!combined %in% c("BLOOD-Klebsiella pneumoniae-Carbapenems", "BLOOD-Escherichia coli-Carbapenems",
                                           "URINE-Escherichia coli-Sulfonamides and trimethoprim","URINE-Klebsiella pneumoniae-Sulfonamides and trimethoprim",
                                           "URINE-Escherichia coli-Fluoroquinolones","URINE-Klebsiella pneumoniae-Fluoroquinolones",
                                           "UROGENITAL-Neisseria gonorrhoeae-Macrolides")) 

g2 = g1%>%ungroup()%>%dplyr::select(c(WHORegionCode,Specimen,PathogenName, st_BCI_million_median))

f3 = left_join(f2,g2, by=c("WHORegionCode", "Specimen", "PathogenName"))

f3 = f3 %>%
  mutate(
    st_BCI_million_imp = ifelse(is.na(st_BCI_million), st_BCI_million_median, st_BCI_million)
  )

# Also link with the overall data (not stratified by age and sex)
h1 <- adataAC %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") & (Specimen =="BLOOD")& (PathogenName=="Acinetobacter spp."|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Neisseria gonorrhoeae"|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Shigella spp.") ~ 'Third-generation cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (PathogenName=="Salmonella spp.") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") & (PathogenName=="Neisseria gonorrhoeae") ~ 'Macrolides',
                              (Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX") & (PathogenName=="Staphylococcus aureus") ~ 'Methicillin resistance',
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") & (PathogenName=="Streptococcus pneumoniae") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Sulfonamides and trimethoprim'))

h2 = left_join(h1,e2, by=c("Iso3", "Specimen", "PathogenName"))

h3 = left_join(h2,g2, by=c("WHORegionCode", "Specimen", "PathogenName"))

h3 = h3 %>%
  mutate(
    st_BCI_million_imp = ifelse(is.na(st_BCI_million), st_BCI_million_median, st_BCI_million)
  )


# NOW PLOT STANDARDISED RATES
# ----------------------------------------------------------------------------------------------
# BSI
p1 <- plot_amr_test(h3, "Escherichia coli", "BLOOD", palette5, xaxis="st_BCI_million")
p2 <- plot_amr_test(h3, "Klebsiella pneumoniae", "BLOOD", palette5, xaxis="st_BCI_million")
p3 <- plot_amr_test(h3, "Salmonella spp.", "BLOOD", palette5, xaxis="st_BCI_million")
p4 <- plot_amr_test(h3, "Acinetobacter spp.", "BLOOD", palette5, xaxis="st_BCI_million")
p5 <- plot_amr_test(h3, "Staphylococcus aureus", "BLOOD", palette5, xaxis="st_BCI_million")
p6 <- plot_amr_test(h3, "Streptococcus pneumoniae", "BLOOD", palette5, xaxis="st_BCI_million")

# UTI
p7 <- plot_amr_test(h3, "Escherichia coli", "URINE", palette5, xaxis="st_BCI_million")
p8 <- plot_amr_test(h3, "Klebsiella pneumoniae", "URINE", palette5, xaxis="st_BCI_million")

# GI
p9 <- plot_amr_test(h3, "Shigella spp.", "STOOL", palette5, xaxis="st_BCI_million")
p10 <- plot_amr_test(h3, "Salmonella spp.", "STOOL", palette5, xaxis="st_BCI_million")

# GONORRHOEA
p11 <- plot_amr_test(h3, "Neisseria gonorrhoeae", "UROGENITAL", palette5, xaxis="st_BCI_million")

# Combine plots with patchwork
combined_plot <- p1/ p2 /p3 / p4 / p5 / p6 
combined_plotUTI <- p7/p8
combined_plotGI_GON <- p9/p10/p11


# Save as PDF
ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/ST_Testing_vs_amrBLOOD.pdf"), 
       plot = combined_plot, width = 12, height = 40)

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/ST_Testing_vs_amrUTI.pdf"), 
       plot = combined_plotUTI, width = 12, height = 30)

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/ST_Testing_vs_amrGI_GON.pdf"), 
       plot = combined_plotGI_GON, width = 12, height = 30)

# OK DOING IT BY SPECIMEN AND PATHOGEN GIVES VERY NOISY RELATIONSHIPS BETWEEN TESTING AND AMR
# Criteria 1: Based on minimum 10 isolates per age group and not more than 15% missing
d_insuf <- d7 %>% filter(!is.na(AgeCatBig)) %>%
 group_by(Iso3, combined) %>%  # Group by country
   summarise(
     has_insufficient = any(use_for_st == "No")  # Check if any combination has insufficient samples
   ) %>%
   filter(has_insufficient)  # Filter to keep only countries with insufficient samples

#length(unique(summary_insufficient$Iso3)) # Seems that all countries do have for some of the drug bug combinations not enough samples

counts <- d_insuf %>%
  group_by(combined) %>%
  summarise(Total = n_distinct(Iso3))

ggplot(counts, aes(x=reorder(combined,Total), y=Total)) + geom_bar(stat="identity") +
  labs(title = "Number of Countries with not enough data for age-standardisetion per drug-Bug Combination",
       subtitle = "criteria 1: Percentage missing age <15%; CTA with min 10 isolates per drug-bug and age-strata",
        x = "Drug-bug combination",
        y = "Number of countries",
        color = "Drug-Bug Group",
        shape = "Specimen"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

d_insuf = left_join(d_insuf, counts)
d_insuf = left_join(d_insuf, cdata%>%dplyr::select(WHORegionCode, Iso3))

p = ggplot(d_insuf, aes(x=reorder(Iso3,WHORegionCode), y=has_insufficient, fill=WHORegionCode)) + geom_bar(stat="identity") +
  labs(title = "Countries with not enough data for age-standardisation per antibiotic class-Pathogen Combination",
       subtitle = "criteria 1: Percentage missing age <15%; CTA with min 10 isolates per drug-bug and age-strata",
       y = "Country",
       x = "",
       color = "Drug-Bug Group",
       shape = "Specimen"
  ) +
  geom_text(aes(
    x = Inf,  # Places text at the far right of each facet
    y = Inf,  # Places text at the top of each facet
    label = paste0("Total: ", Total)
  ), hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
  facet_wrap(~combined, scales="free_y",ncol=6) +
  coord_flip()+
  theme_minimal() 
p

ggsave(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/CTA_not_enoughdata_agestand_testing_criteria1.pdf"), 
               plot = p, width = 25, height = 15)

table(d_insuf$Iso3, d_insuf$has_insufficient)


# Fit BCI vs AMR prevalence with quadratic spline
data_ecoli_BSI = h3 %>% filter(Specimen=="BLOOD", PathogenName =="Escherichia coli",
                               Grouping=="Third-generation cephalosporins"
                               )

fit_e.coli_BSI <- glm(cbind(Resistant, InterpretableAST - Resistant) ~ st_BCI_million^2, 
                 data = data_ecoli_BSI, 
                 family = binomial(link = "logit"))
summary(fit_e.coli_BSI)

data_ecoli_BSI$residuals <- data_ecoli_BSI$amr_rate - predict(fit_e.coli_BSI, 
                                                              newdata = data_ecoli_BSI)

# Visualize residuals along BCI/100000population
ggplot(data_ecoli_BSI, aes(x = st_BCI_million, y = residuals)) +
  geom_vline(xintercept = 40, col="red", linetype=2, size=1.5)+
  theme_minimal()+
  geom_point() +
  geom_smooth() +
  labs(title = "Residuals Plot", x = "BCI/1,000,000 Population", y = "Residuals",
       subtitle = paste0(data_ecoli_BSI$Specimen,"-", data_ecoli_BSI$PathogenName, "-", data_ecoli_BSI$Grouping)
  ) 

# Define a function to fit model, calculate residuals, and return both fit and plot
cutoff = read.csv(paste0(dirDataNew, "/estimated_testing_cutoff.csv"), sep=",")       # Population data
cutoff$PathogenName
cutoff$Grouping
cutoff$Specimen

cutoff$PathogenName[cutoff$PathogenName=="Acinetobacter spp.       "] = "Acinetobacter spp."
cutoff$PathogenName[cutoff$PathogenName=="Klebsiella pneumoniae "] = "Klebsiella pneumoniae"
cutoff$PathogenName[cutoff$PathogenName=="Streptococcus pneumoniae "] = "Streptococcus pneumoniae"
cutoff$PathogenName[cutoff$PathogenName== "Staphylococcus aureus "] =  "Staphylococcus aureus"
cutoff$PathogenName[cutoff$PathogenName=="Neisseria gonorrhoeae " ] = "Neisseria gonorrhoeae"

write.csv(cutoff, paste0(dirDataNew, "/estimated_testing_cutoff.csv"))

cutoff$combined2 = paste0(cutoff$Specimen,"-",cutoff$PathogenName, "-", cutoff$Grouping)
h3$combined2 = paste0(h3$Specimen,"-",h3$PathogenName, "-", h3$Grouping)

h4 = left_join(h3, cutoff%>%dplyr::select(combined2,testcoverage_cutoff), by="combined2")
table(h4$combined2, h4$testcoverage_cutoff)

unique(h3$combined2)

fit_and_plot <- function(data) {
  # Fit the binomial model with a quadratic term
  fit <- glm(cbind(Resistant, InterpretableAST - Resistant) ~ st_BCI_million + I(st_BCI_million^2), 
             data = data, 
             family = binomial(link = "logit"))
  
  # Add residuals to the dataframe
  data$residuals <- data$amr_rate - predict(fit, newdata = data, type = "response")
  print(unique(data$testcoverage_cutoff))
  # Create residual plot
  plot <- ggplot(data, aes(x = st_BCI_million, y = residuals)) +
    #geom_vline(xintercept = 40, col="red", linetype=2, size=1.5) +
    geom_point() +
    geom_vline(xintercept = unique(data$testcoverage_cutoff), col="red", size=1.5)+
    geom_smooth(type="loess") +
    theme_minimal() +
    #scale_colour_manual(values = palette5) +
    labs(
      title = "Residuals Plot",
      x = "BCI/1,000,000 Population",
      y = "Residuals",
      subtitle = paste0(data$Specimen[1], "-", data$PathogenName[1], "-", data$Grouping[1])
    )
  
  return(list(fit = fit, plot = plot))
}

# Apply the function to each unique combination of Specimen, PathogenName, and Grouping
results <- h4 %>% filter(!is.na(Grouping), InterpretableAST>10) %>% 
  group_by(Specimen, PathogenName, Grouping) %>%
  group_split() %>%
  setNames(map_chr(., ~ paste(.$Specimen[1], .$PathogenName[1], .$Grouping[1], sep = "-"))) %>%
  map(fit_and_plot)

# Display all plot names
print(names(results))

# Display all plots
for (name in names(results)) {
  print(paste("Displaying plot for:", name))
  print(results[[name]]$plot)
}


panel_plot <- wrap_plots(map(results, "plot"), ncol = 3)  # Adjust `ncol` to control the number of columns
panel_plot

pdf(file=paste0(dirOutput,"/Analyses/Section3.3_Surveillance_coverage/Testing_coverage_cutoff2.pdf"),
    width=15, height=15)
#for(i in 1:length(names(results))){
#print(results[[i]]$plot)
print(panel_plot)
#}
dev.off()

# MAKE SURE THAT UNKNOWN AGE IS RECOGNISED AS CHARACTER, AS OTHERWISE WILL HAVE DIFFERENT DATASETS WHEN FITTING MODEL WITHOUT AGE AND SEX
table(f3$AgeCat10, useNA="always")

# NOW SAVE DATASETS
write.csv(f3, paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"))
write.csv(h3, paste0(dirDataNew, "/EI_AMRdtaAC_ANALYSES.csv"))


# BASED ON THESE PLOTS, FOLLOWING CUT-OFFS
# For the proxy antibiotic classes

# BLOOD
# -----------------------------
# Acinetobacter - Carb                        15       
# Escherichia coli - 3GC                      50               
# Escherichia coli - Carb                     25
# Klebsiella pneumoniae - 3GC                 50
# Klebsiella pneumoniae - Carb                25      
# Salmonella spp. - Fluoroquinelone             ?             
# Staphylococcus aureus - methicillin         25
# Streptococcus pneumoniae - Penicillin       5
 
# STOOL
# ---------------------------------
# Salmonella spp. - Fluoroquinelone          10
# Shigella spp. - 3GC                      ? 2.5?

# URINE
# ---------------------------------
# E. coli - Fluoroquinelones                 250
# E. coli - 3GC                              250
# E. coli - Sulf and trim                     50
# Klebsiella pneumoniae - Fluoroquinelones    50
# Klebsiella pneumoniae - 3GC                 50
# Klebsiella pneumoniae - Sulf and trim       50

# URIGENITAL
# ---------------------------------
# Neisseria Gonorrhoeae - 3GC                 5?


# You can access individual models or plots like this
# fit_e_coli_BSI <- results[["BLOOD-Escherichia coli-Third generation cephalosporins"]]$fit
# plot_e_coli_BSI <- results[["BLOOD-Escherichia coli-Third generation cephalosporins"]]$plot


# NEW APPROACH
# -------------------------------------------------------------------

# Use age-standardised testing per actual drug-bug