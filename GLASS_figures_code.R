#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, RColorBrewer,
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, purrr, brms, cowplot)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Final_Curated_Data_GLASS_2024"
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
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")       # Population data

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")   # Country data

# Surveillance indicator data
#sdata = read.csv(paste0(dirDataNew, "/EI_SurveillanceSites_180724_EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
#idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data
idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_180724_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read_excel(paste0(dirDataNew, "/updated_summary_dbc_longformat.xlsx"), sheet=1)

# rrates 2021
rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
  Q1 = as.numeric(Q1),
  Q3 = as.numeric(Q3),
  median = as.numeric(median)
)



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
# Descriptive map
world_un <- st_read("C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/NewDatabaseEV/2022 GLASS data - New DB - for 2024 report/Master_Raw_Data_GLASS_2024/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp")
head(world_un)

# Get unique combinations of Specimen, PathogenName, and AntibioticName
# combinations <- adataAC %>%
#   group_by(Specimen, PathogenName, AntibioticName) %>%
#   summarise(n=n()) %>% select(-c(n)) %>%
#   mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName)) %>%
#   as.data.frame()


# Resistance per drug-bug per country
plot_amr_map(shapefile = world_un, 
             amr_data = rrates, 
             specimen = "BLOOD", 
             pathogen_name = "Escherichia coli",
             antibiotic_name = "Ceftriaxone", 
             na_color = "lightgrey")
  

# Loop over all drug-bug combinations
ecoli = combinations2022 %>% filter(PathogenName=="Escherichia coli")
kpn = combinations2022 %>% filter(PathogenName=="Klebsiella pneumoniae")
abact = combinations2022 %>% filter(PathogenName=="Acinetobacter spp.")
salm = combinations2022 %>% filter(PathogenName=="Salmonella spp.")

# Create a list of plots
###################################

# E. coli
ecoli_maps <- pmap(
  list(
    ecoli$Specimen,
    ecoli$PathogenName,
    ecoli$AntibioticName
  ),
  ~ plot_amr_map(
    world_un,
    rrates %>% filter(PathogenName=="Escherichia coli"),
    specimen = ..1,
    pathogen_name = ..2,
    antibiotic_name = ..3
  )
)

# Klebsiella pneumoniae
kpn_maps <- pmap(
  list(
    kpn$Specimen,
    kpn$PathogenName,
    kpn$AntibioticName
  ),
  ~ plot_amr_map(
    world_un,
    rrates %>% filter(PathogenName=="Klebsiella pneumoniae"),
    specimen = ..1,
    pathogen_name = ..2,
    antibiotic_name = ..3
  )
)

# Acinetobacter spp.
abact_maps <- pmap(
  list(
    abact$Specimen,
    abact$PathogenName,
    abact$AntibioticName
  ),
  ~ plot_amr_map(
    world_un,
    rrates %>% filter(PathogenName=="Acinetobacter spp."),
    specimen = ..1,
    pathogen_name = ..2,
    antibiotic_name = ..3
  )
)

# Salmonella spp.
salm_maps <- pmap(
  list(
    salm$Specimen,
    salm$PathogenName,
    salm$AntibioticName
  ),
  ~ plot_amr_map(
    world_un,
    rrates %>% filter(PathogenName=="Salmonella spp."),
    specimen = ..1,
    pathogen_name = ..2,
    antibiotic_name = ..3
  )
)


# Flatten the list of plots if necessary (ensure it's a simple list of ggplot objects)
# Use marrangeGrob to arrange plots into a grid layout, and save to a file if needed
pdf(paste0(dirOutput, "/Descriptive/Ecoli_AMR_Maps.pdf"), width = 11, height = 8.5) # Open a PDF device for saving
marrangeGrob(grobs = ecoli_maps, ncol = 2, nrow = 2) # Adjust ncol and nrow as needed
dev.off() # Close the PDF device

pdf(paste0(dirOutput, "/Descriptive/Klebsiella_AMR_Maps.pdf"), width = 11, height = 8.5) # Open a PDF device for saving
marrangeGrob(grobs = kpn_maps, ncol = 2, nrow = 2) # Adjust ncol and nrow as needed
dev.off() # Close the PDF device

pdf(paste0(dirOutput, "/Descriptive/Acinetobacter_AMR_Maps.pdf"), width = 11, height = 8.5) # Open a PDF device for saving
marrangeGrob(grobs = abact_maps, ncol = 2, nrow = 2) # Adjust ncol and nrow as needed
dev.off() # Close the PDF device

pdf(paste0(dirOutput, "/Descriptive/Salmonella_AMR_Maps.pdf"), width = 11, height = 8.5) # Open a PDF device for saving
marrangeGrob(grobs = salm_maps, ncol = 2, nrow = 2) # Adjust ncol and nrow as needed
dev.off() # Close the PDF device


# Plot the AMR estimates - per pathogen, antibiotics combined
#################################################################################
palette <- brewer.pal(7, "Set1")  # Choose a palette with three colors

# Escherichia coli
####################################

# Extract raw data rates of 75 percentile countries
rrates_ecoli = rrates2021 %>% filter(PathogenName=="Escherichia coli", Year==2021) %>%
  rename(
    AntibioticName = "AbTargets"
  )

model_estimates_ecoli_total = left_join(model_estimates %>% filter(Total=="Yes"), rrates_ecoli)

p1 = plot_model_AMRdb_withdata(model_estimates_ecoli_total%>% filter(Specimen=="BLOOD"))
p2 = plot_model_AMRdb_withdata(model_estimates_ecoli_total%>% filter(Specimen=="URINE"))

p3 = plot_model_AMRdb_region(model_estimates %>% filter(Specimen=="BLOOD"))
p4 = plot_model_AMRdb_region(model_estimates %>% filter(Specimen=="URINE"))

# Plot E.coli AMR estimates
pdf(paste0(dirOutput, "/Analyses/model_ecoli_db.pdf"), width = 15, height = 5) # Open a PDF device for saving
multiplot(p1,p2, cols=2)
dev.off()

pdf(paste0(dirOutput, "/Analyses/model_ecoli_db_byregion.pdf"), width = 17, height = 6) # Open a PDF device for saving
# Arrange the plots in a grid with 4 plots per row
multiplot(p3,p4, cols=1)
dev.off()


#  4.2	Time series of resistance to selected antibiotics, 2017-2022
###################################################################

# 4.2.1 Resistance incidence by number of samples tested
###################################################################

# Check completeness
num_test = adataAC %>% group_by(Year, Iso3) %>%
  reframe(
    num.test.total = sum(NumSampledPatients, rm.na=T),
  )  %>% group_by(Year) %>%
  reframe(
    total.missing.tested = sum(is.na(num.test.total)),
    total.not.missing.tested = sum(!is.na(num.test.total)),
    prop.missing = round(total.missing.tested/(total.not.missing.tested+total.missing.tested),2)
  )

num_test_byregion = adataAC %>% group_by(Year, Iso3,WHORegionCode) %>%
  reframe(
    num.test.total = sum(NumSampledPatients, rm.na=T),
  )  %>% group_by(Year, WHORegionCode) %>%
  reframe(
    total.missing.tested = sum(is.na(num.test.total)),
    total.not.missing.tested = sum(!is.na(num.test.total)),
    prop.missing = round(total.missing.tested/(total.not.missing.tested+total.missing.tested),2)
  )

View(num_test_byregion)

p1 = ggplot(num_test_byregion, aes(x = WHORegionCode, y = prop.missing*100, fill = WHORegionCode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "WHO Region Code", y = "% Countries with missing data", title = "% countries with missing data on number of tested patients - by region") +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  facet_wrap(. ~ Year)

p2 = ggplot(num_test, aes(x = Year, y = prop.missing*100, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "% Countries with missing data", title = "% countries with missing data on number of tested patients - Overall") +
  theme_minimal() + 
  theme(
    legend.position = "none"
  )

pdf(paste0(dirOutput, "/Descriptive/missing_data_n_tested.pdf"), width = 8, height = 8) # Open a PDF device for saving
multiplot(p2, p1)
dev.off()

------------------------------------------------------------
# Decided that number tested is not complete enough
------------------------------------------------------------
  
 
# Resistance map with bubbles for Specimen and Pathogen
#######################################################################