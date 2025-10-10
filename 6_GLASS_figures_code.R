#######################################
# GLASS REPORT - FIGURES
#######################################

# Author: Esther van Kleef
# Date created: August 2024
# Last update: 14 July 2025

rm(list=ls())

# Load R packages
pacman::p_load(readxl, rio, lubridate, zoo, ggplot2, Hmisc, stringr,dplyr,openxlsx,
               magrittr, officer, janitor, sf, gtsummary, leaflet, ggtext,ggh4x,
               gridExtra, grid, purrr, brms, cowplot, ggrepel, grid, wesanderson,
               patchwork, ggh4x, ggalluvial, networkD3, tidyr, forcats, ggtext,
               glue, RColorBrewer, scales, svglite, ggrepel, ggnewscale,matrixStats, ragg)

remotes::install_github("glaziou/whomap")
library(whomap)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
dirDataRaw = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/"

#dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"
dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"
dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables/2021/"
#dirDataModeloutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Model_output/model_output/ALL/all_one_model_weakip_set1"
dirDataModeloutput="C:/Users/esthe/OneDrive - VanKleefBV/Documenten/Documenten/WHO/WHO_GLASS/2025 ANALYSIS EV/2025 Model_output/all_one_model_weakip_set1_centered"

dirDataModeloutputCh3 = "C:/Users/esthe/OneDrive - VanKleefBV/Documenten/Documenten/WHO/WHO_GLASS/2025 ANALYSIS EV/2025 Model_output/model_output_surveillance_coverage/"

dirOutputReport = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/FINAL 2023 FIGURES TABLES AND RESULTS"

dirOutputReportNEW = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/FINAL 2023 FIGURES TABLES AND RESULTS LATEST"

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
haqidata = read.csv(paste0(dirDataNew, "/EI_HAQIdta_Country_140325_EV.csv"), sep=",")

# AMR data
adataAC_crude = read.csv(paste0(dirDataNewO, "/EI_AMRdtaAC_110325_EV.csv"), sep=",")   # Country AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_ANALYSES.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_Country_140325_EV.csv"), sep=",")   # Country AMR data
#adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_Pop_Country_030924_EV.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "./updated_summary_dbc_longformat.csv"), sep=",")

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

adataAC_crude = adataAC_crude %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
         combined = gsub("\\s+", " ", trimws(combined)),
         InReport = ifelse(combined %in% combinations2022$combined, "Yes","No")
  )

unique(adataAC_crude$combined)
combinations2022$combined[!combinations2022$combined %in% adataAC_crude$combined]

sort(unique(adataAC_crude$combined))

adataAC_crude = left_join(adataAC_crude, cdata)
adataAC_crude = left_join(adataAC_crude, pdata)

adataAC_crude = adataAC_crude %>% 
  #filter(!Id %in% c("IdKOS2016","IdKOS2017","IdKOS2018","IdKOS2019","IdKOS2020")) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No"),
    amr_rate = Resistant/InterpretableAST, 
    #BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  )

unique(adataAC$combined[adataAC$InReport=="Yes"]) # 93 drug-bug specimen combinations
unique(combinations2022$combined)
unique(adataAC$combined)

# Bed density
#bdays = read.csv(paste0(dirDataNewO, "/EI_BedDensitydta_180724_EV.csv"), sep=",")

# Standard population
# See also here https://epirhandbook.com/new_pages/standardization.html
standard_pop_data <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/world_standard_population_by_sex.csv")

###################################################################
# PREAMBLE
###################################################################

# Link bed days with country data to get regions
#bdays = left_join(bdays, cdata, by="Iso3")

#adataAC = left_join(adataAC, bdays%>%dplyr::select(Iso3, Year, BedDensityPer100000Pop), by=c("Iso3", "Year"))
#adataAC = left_join(adataAC, udata%>%dplyr::select(Iso3, Year, amr_glass_inpatient_day_number), by=c("Iso3", "Year"))


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
  dplyr::summarise(TotalPopulationStandard = sum(WorldStandardPopulation), .groups = "drop") %>%
  mutate(AgeCat10 = factor(AgeCat10, levels = c("0<04", "05<14", "15<24", "25<34", "35<44", "45<54", "55<64", "65<74", "75<84", "85+", "UnkAge"))) %>%
  mutate(AgeCat10 = relevel(AgeCat10, ref = "0<04"))


# Check if all drug bug are in adataAC
#-----------------------------------------------------------------------
dbdata$combined = paste0(dbdata$Specimen, "-", dbdata$PathogenName, "-", dbdata$AntibioticName)
length(dbdata$combined)

dbtotal = (unique(adataAC$combined[adataAC$InReport=="Yes"])) # 95 two are missing
length(dbtotal)

dbdata$combined[which(!dbdata$combined %in% dbtotal)] # both are in the dataset
dbtotal[!dbtotal%in%dbdata$combined] # 


# List of priority pathogens
#-----------------------------------------------------------------------
# BLOOD
ppbsi = c("BLOOD-Acinetobacter spp.-Imipenem","BLOOD-Escherichia coli-Cefotaxime",
          "BLOOD-Escherichia coli-Imipenem", "BLOOD-Klebsiella pneumoniae-Cefotaxime",
          "BLOOD-Klebsiella pneumoniae-Imipenem", "BLOOD-Salmonella spp.-Ciprofloxacin",
          "BLOOD-Streptococcus pneumoniae-Penicillin G")

SDG = c("BLOOD-Staphylococcus aureus-Methicillin-resistance",
        "BLOOD-Escherichia coli-Third-generation cephalosporins")


# UTI
pputi = c("URINE-Escherichia coli-Cefotaxime",
          "URINE-Escherichia coli-Imipenem",
          "URINE-Klebsiella pneumoniae-Cefotaxime",
          "URINE-Klebsiella pneumoniae-Imipenem")


# GI
ppgi = c("STOOL-Shigella spp.-Ciprofloxacin",
         "STOOL-Salmonella spp.-Ciprofloxacin")


# NEISS
ppuro = c("UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone")

pp = c(ppbsi,SDG,pputi,ppgi,ppuro)

###################################################################
# COLOUR SPECIFICATION
###################################################################

palette <- wes_palette("Darjeeling1", n = 5)
palette2 <- wes_palette("BottleRocket2", n = 1)
palette3 <- wes_palette("GrandBudapest1", n = 2)[2]
palette4 <- wes_palette("BottleRocket2", n = 2)[2]
palette5 = c(palette3, palette[2],palette2,palette[5], palette[4],palette4)

palette_map = c(palette2, palette[2],palette3)

palette_map2 = c("#e0f3db",  
                 "#a8ddb5",   # Light green
                 "#66c2a4",  # Medium teal
                 "#41b6c4", # Light blue
                 "#1d91c0",# Medium blue
                 "#225ea8",# Dark blue
                 "#0c2c84"    # Deep navy
)

palette_map3 = c(
  "#e0f3db",  # Very light green
  "#a8ddb5",  # Light green
  "#66c2a4",  # Medium teal
  "#5ec8b4",  # # Teal-blue shade
  "#41b6c4",  # Light blue
  "#1d91c0",  # Medium blue
  "#225ea8",  # Dark blue
  "#0c2c84"   # Deep navy
) 

palette_map4 = c(
  "#fff7bc",  # Pale yellow
  "#fee391",  # Light golden yellow
  "#fec44f",  # Warm orange-yellow
  "#fe9929",  # Deep orange
  "#ec7014",  # Burnt orange
  "#cc4c02",  # Dark orange-brown
  "#993404",  # Deep reddish brown
  "#662506"   # Dark brownish purple
)

facet_colors <- c(
  "AFR" = palette5[1],
  "AMR" = palette5[2],
  "EMR" = palette5[3],
  "EUR" = palette5[4],
  "SEA" = palette5[5],
  "WPR" = palette5[6]
)

# Define colors for each WHORegionCode
facet_colors2 <- c(
  "Global" = "grey",
  "AFR" = palette5[1],
  "AMR" = palette5[2],
  "EMR" = palette5[3],
  "EUR" = palette5[4],
  "SEA" = palette5[5],
  "WPR" = palette5[6]
)

facet_colors3 <- c(
  "Global" = "grey",
  "African Region" = palette5[1],
  "Region of the Americas" = palette5[2],
  "South-East Asia Region" = palette5[5],
  "European Region" = palette5[4],
  "Eastern Mediterranean Region" = palette5[3],
  "Western Pacific Region" = palette5[6]
)

facet_colors3a <- c(
  "African Region" = palette5[1],
  "Region of the Americas" = palette5[2],
  "South-East Asia Region" = palette5[5],
  "European Region" = palette5[4],
  "Eastern Mediterranean Region" = palette5[3],
  "Western Pacific Region" = palette5[6]
)

# Load in shape file
#-------------------------------------------------------------------------------------------------------------
world_un <- st_read(paste0(dirDataNew, "/MapShapeFiles/Detailed_Boundary_ADM0_2916330977523091315/GLOBAL_ADM0.shp"))
head(world_un)

world_disp <- st_read(paste0(dirDataNew,"/MapShapeFiles/Detailed_Boundary_Disputed_Areas/Detailed_Boundary_Disputed_Areas.shp"))
world_disp_borders <- st_read(paste0(dirDataNew,"/MapShapeFiles/Detailed_Boundary_Disputed_Borders/Detailed_Boundary_Disputed_Borders.shp"))

world_un <- st_transform(world_un, crs = 4326)
world_disp <- st_transform(world_disp, crs = 4326)
world_disp_borders <- st_transform(world_disp_borders, crs = 4326)

# Exclude sea and lakes
excluded <- c("Lake", "Sea", "Lakes")
world_disp_land <- world_disp %>%
  filter(!grepl(paste(excluded, collapse = "|"), NAME, ignore.case = TRUE))

# Combine All Shapefiles
world_combined <- world_un %>%
  mutate(category = "Recognized Territory") %>%
  bind_rows(world_disp %>% mutate(category = "Disputed Area")) %>%
  bind_rows(world_disp_borders %>% mutate(category = "Disputed Border"))

world_combined$category = ifelse(world_combined$NAME%in%world_disp_land$NAME, "Disputed Land", world_combined$category)


###################################################################
# FIGURES
###################################################################

# 2	National AMR surveillance implementation indicators
###################################################################

# 2	GLOBAL AND REGIONAL AMR SURVEILLANCE COVERAGE	 
###################################################################

# 2.1	Participation in GLASS 
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

# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 2.1: Map of CTAs enrolled in GLASS
# --------------------------------------------------------------------------------------------------------------------------------------

e1 = read.csv(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.1_GLASS_enrolled_or_with_data_300325_EV.csv"))
names(e1)

#e1 = read.csv(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/DataForWHSMap.csv"))
#names(e1)

e1 = e1 %>%
  rename(Iso3 = iso3)

unique(e1$var)

# Summarise number of countries enrolled
e1 %>%
  filter(var %in% c("2023 AMR data", "AMR data before 2023", "No AMR data")) %>%
  group_by(WHORegionName) %>%
  summarise(unique_Iso3 = length(unique(Iso3)), .groups = "drop") %>%
  pivot_wider(names_from = WHORegionName, values_from = unique_Iso3)


world <- world_combined %>%
  rename(Iso3 = "ISO_3_CODE")

unique(world$Iso3)
unique(e1$Iso3)

# Merge the filtered data with the shapefile data
e2 <- world %>%
  left_join(e1, by = c("Iso3")) #%>%

which(!e1$Iso3 %in% e2$Iso3)
e1$Iso3[which(!e1$Iso3 %in% e2$Iso3)]

# Currently Kosovo and Hongkong did submit 2023 data, but are not plotted as not in the shape file

e3 <- left_join(e2%>%select(-c(WHORegionCode)), cdata%>%select(Iso3, WHORegionCode), by="Iso3")

e4 <- e3 %>%
  mutate(var = ifelse(category =="Disputed Land", "Not applicable", var),
         var = ifelse(is.na(var), "Not enrolled in GLASS", var), # This is for all the islands etc
         var = factor(var, levels=c("2023 AMR data", "AMR data before 2023", 
                                    "No AMR data", "Not enrolled in GLASS", "Not applicable")),
  )

unique(e4$var)


labels =  levels(e4$var)

p = ggplot(data = e4) +
  geom_sf(aes(fill = var), color = "black") +  # Fill countries based on BCI categories
  geom_sf(data = e4 %>% filter(category =="Disputed Border"),linetype="dashed", size=0.05, color="orangered4")+
  scale_fill_manual(
    values = c(
      "cyan4",
      "darkseagreen2",
      "khaki",
      "white",
      "grey"
    ),
    labels = c(
      labels[1], labels[2], labels[3], labels[4],labels[5]
    ),  # Color for NA values
    na.translate = TRUE,  # Include NA values in the legend
    drop = FALSE          # Retain all legend levels
  ) +
  theme_void() +
  labs(
    title = "",
    fill = ""  # Legend title
  ) +
  theme(
    legend.position = "bottom",         # Move the legend to the bottom
    legend.spacing.y = unit(1, "cm"),     # Remove vertical spacing
    title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, vjust = 1.5, margin = margin(b = 20)),  # Center the title
    plot.subtitle = element_text(hjust = 0.5),            # Center the subtitle
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = grid::unit(c(0, 1, 0, 1), "mm"),         # Top, Right, Bottom, Left,
    panel.border = element_rect(color = "white", size = 0.5) 
  ) +
  guides(
    fill = guide_legend(                       # Place all legend items in one row
      label.position = "right",       # Position labels below the keys
      label.hjust = -0.2,#,                  # Adjust vertical alignment of labels,
      nrow=1,
      override.aes = list(color = c(NA, NA,NA, "black",NA)))
  ) +
  coord_sf(expand = FALSE)
p

# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.1_enrolledglass_2023.svg"), 
#        plot = p,
#        device = svg,
#        dpi = 300,
#        width = 8, height = 5)


ggsave(
  filename = file.path(dirOutputReportNEW, "Chapter 2/Ch2 Figures/Final/Figure_2.1_enrolledglass_2023.jpg"),
  plot = p,
  device = ragg::agg_jpeg, # better anti-aliasing
  width = 8, height = 5, units = "in",
  dpi = 150,
  quality = 90,
  background = "white"
)

ggsave(
  filename = file.path(dirOutputReportNEW, "Chapter 2/Ch2 Figures/Final/Figure_2.1_enrolledglass_2023.png"),
  plot = p,
  device = ragg::agg_png,       
  width = 8, height = 5, units = "in",
  dpi = 150,
  background = "white"
)

p <- whomap(data.frame(iso3 = e4$Iso3, var = e4$var), colours = c(
  "cyan4",
  "darkseagreen2",
  "khaki",
  "white",
  "grey"), line.col =  "black", map.title = "", legend.title = "BCI per million",
            water.col = "white", na.label = "No data",na.col = "grey95", disclaimer = FALSE, legend.pos = "bottom", recentre = 12) +
  labs(
    title = "",
    subtitle =""
  )
p

ggsave(filename = paste0(dirOutputReportNEW, "/Chapter 2/Ch2 Figures/Final/Figure_2.1_enrolledglass_2023_whomap.svg"),
       plot = p,
       device = svg,
       dpi = 300,
       width = 8, height = 5)


# SDG SPECIFIC FOR WORLD HEALTH STATS REPORT
# Summarise number of countries enrolled
# e1 = read.csv(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/DataForWHSMap.csv"))
# names(e1)
# 
# e1 = e1 %>%
#   rename(Iso3 = iso3)
# 
# unique(e1$var)
# 
# 
# e1 %>%
#   #filter(var %in% c("2022 AMR data", "AMR data before 2022", "No AMR data")) %>%
#   group_by(WHORegionName) %>%
#   summarise(unique_Iso3 = length(unique(Iso3)), .groups = "drop") %>%
#   pivot_wider(names_from = WHORegionName, values_from = unique_Iso3)
# 
# 
# world <- world_combined %>%
#   rename(Iso3 = "ISO_3_CODE")
# 
# # Merge the filtered data with the shapefile data
# e2 <- world %>%
#   left_join(e1, by = c("Iso3")) #%>%
# 
# e3 <- left_join(e2%>%select(-c(WHORegionCode)), cdata%>%select(Iso3, WHORegionCode), by="Iso3")
# 
# e4 <- e3 %>%
#   mutate(var = ifelse(category =="Disputed Land", "Not applicable", var),
#          var = ifelse(is.na(var), "No data", var), # This is for all the islands etc
#          var = factor(var, levels=c("Reported resistance in both E. coli and S. aureus", 
#                                     "Reported resistance to third generation cephalosporins in E. coli", 
#                                     "Reported methicillin resistance in S. aureus",
#                                     "No data", "Not applicable")),
#   )
# 
# unique(e4$var)
# table(e4$var, useNA="always")
# 
# labels =  levels(e4$var)
# 
# p = ggplot(data = e4) +
#   geom_sf(aes(fill = var), color = "black") +  # Fill countries based on BCI categories
#   geom_sf(data = e4 %>% filter(category == "Disputed Border"), 
#           linetype = "dashed", size = 0.05, color = "orangered4") +
#   scale_fill_manual(
#     values = c("cyan4", "coral2", "khaki", "white", "grey"),
#     labels = c(
#       expression("Reported resistance in both"~italic("E. coli")~"and"~italic("S. aureus")),
#       expression("Reported resistance to third-generation cephalosporins in"~italic("E. coli")),
#       expression("Reported methicillin-resistance in"~italic("S. aureus")),
#       "No data",
#       "Not applicable"
#     ),
#     na.translate = TRUE,  # Include NA values in the legend
#     drop = FALSE          # Retain all legend levels
#   ) +
#   theme_void() +
#   labs(
#     title = "",
#     fill = ""  # Legend title
#   ) +
#   theme(
#     legend.position = "bottom",         
#     legend.spacing.y = unit(1, "cm"),   
#     title = element_text(size = 20),
#     legend.text = element_text(size = 10)  # Ensure readability
#   ) +
#   guides(
#     fill = guide_legend(
#       label.position = "right",
#       label.hjust = 0,
#       nrow = 2,
#       override.aes = list(color = c(NA, NA, NA, "black", NA))
#     )
#   ) +
#   coord_sf(expand = FALSE)
# 
# p
# 
# 
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.1_enrolledglass_2022_SDG.svg"), 
#        plot = p,
#        device = svg,
#        dpi = 300,
#        width = 10, height = 6)


# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.2: Implementation status, quality assurance, and standards of national AMR surveillance systems in 2022, for CTAs reporting to GLASS-AMR
# --------------------------------------------------------------------------------------------------------------------------------------

# Of those with NRLs, 
# (i) what proportion have an EQA programme (and among them, what proportion also have EQA of local labs, if relevant), 
# (ii) and what proportion are following international standards for AST? 
# (iii) what proportion have an NCC. 
# Also, of those with an NSP, (i) what proportion have an NCC, (ii) what proportion have an NTRL

###################################################################### AMR DATA ####################################################################

#f1 = read.csv(paste0(dirDataNewO, "/EI_Implementationdta_110325_EV.csv"), header=T)

#f1<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Implementationdta_280824.csv", header=T)
#attach(f1)
#fix(f1)

#f2 = read.csv(paste0(dirDataNewO, "/EI_Countrydta_110325_EVK.csv"), header=T)

#f2<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Countrydta_180724.csv", header=T)
#attach(f2)
#fix(f2)


# f2 = f2[order(f2$Iso3),] %>%
#   mutate(
#     CountryTerritoryArea = case_when(
#       CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
#       TRUE ~ CountryTerritoryArea)
#   )
# 
# f3<-merge(f1,f2,by="Iso3",all.x=TRUE)
# f4 <-  dplyr::select(f3, - c("Year", "CountryTerritoryArea", "IncomeWorldBankJune2023", "EnrolledAMR", "EnrollmentDateAMR", "EnrolledAMC", "EnrollmentDateAMC", "WHOLegalStatus", "Candida", "EGASP",
#                       "AttribMort", "Tricycle")) 
# 
# 
# 
# f5<- f4 %>%
#   filter(AMR_NCC != "Not_enrolled")%>% 
#   droplevels()
# 
# 
# 
# f7 <- f5 %>%
#   mutate(NCC = case_when((AMR_NCC =="Established")  ~ 'Available',
#                          (AMR_NCC =="Establishment in progress" | AMR_NCC =="Not established") ~ 'Not available',
#                          (AMR_NCC =="Not reported") ~ 'No data'))
# 
# f8 <- f7 %>%
#   mutate(NRL = case_when((AMR_NRL =="Established")  ~ 'Available',
#                          (AMR_NRL =="Not established") ~ 'Not available',
#                          (AMR_NRL =="Not reported") ~ 'No data'))
# 
# 
# f9 <- f8 %>%
#   mutate(EQA_NRL = case_when((EQA_to_NRL =="Provided")  ~ 'Available',
#                              (EQA_to_NRL =="Not provided") ~ 'Not available',
#                              (EQA_to_NRL =="Not reported") ~ 'No data'))
# 
# 
# f10 <- f9 %>%
#   mutate(International_AST_Standards = case_when((AMR_AST_standards =="O")  ~ 'Not available',
#                                                  (AMR_AST_standards =="CLSI" | AMR_AST_standards =="EUCAST"| AMR_AST_standards =="EUCAST|CLSI") ~ 'Available',
#                                                  (AMR_AST_standards =="Not reported") ~ 'No data'))
# 
# f11 <- f10 %>%
#   mutate(All_GLASS_labs_With_EQA = case_when((AMR_EQA_GLASS_labs =="Provided to all laboratories")  ~ 'Available',
#                                              (AMR_EQA_GLASS_labs =="Not provided to all laboratories") ~ 'Not available',
#                                              (AMR_EQA_GLASS_labs =="Not reported") ~ 'No data'))
# 
# f12 <-  dplyr::select(f11, - c("AMR_NCC", "AMR_NRL", "EQA_to_NRL", "AMR_AST_standards", "AMR_EQA_GLASS_labs")) 
# 
# 
# 
# f12b <- f12 %>%
#   mutate(Core = case_when((NCC == "Available" & NRL == "Available" & EQA_NRL =="Available" & International_AST_Standards =="Available")  ~ "Available",
#                           (NCC =="No data" | NRL == "No data" | EQA_NRL == "No data" | International_AST_Standards == "No data") ~ "No data",
#                           TRUE ~ "Not available"))
# 
# f13<-f12b%>% 
#   as.data.frame() %>%
#   group_by(WHORegionName,Core) %>% 
#   summarise(total_count=n()) %>%
#   as.data.frame() 
# 
# f13b<-f12b%>%
#    as.data.frame() %>%
#    group_by(Core) %>%
#    summarise(total_count=n()) %>%
#    as.data.frame() %>%
#    mutate(WHORegionName = "Global")
# 
# f14 = rbind(f13, f13b)
# 
# f15 = f14 %>%
#   group_by(WHORegionName)%>%
#   summarise(n = sum(total_count))
# 
# f16 = left_join(f14,f15)
# 
# f17 = f16 %>% mutate(
#   Region = factor(WHORegionName,
#                    levels = c("Global",
#                      "Western Pacific Region",
#                      "Eastern Mediterranean Region",
#                      "European Region",
#                      "South-East Asia Region",
#                      "Region of the Americas",
#                      "African Region"
#                    ))
#   )
# 
# f18<-f12b%>% 
#   as.data.frame() %>%
#   group_by(WHORegionName,All_GLASS_labs_With_EQA) %>% 
#   summarise(total_count=n()) %>%
#   as.data.frame() 
# 
# f18b = f12b%>% 
#    as.data.frame() %>%
#    group_by(All_GLASS_labs_With_EQA) %>% 
#    summarise(total_count=n()) %>%
#    as.data.frame() %>%
#    mutate(WHORegionName = "Global")
# 
# f19 = rbind(f18, f18b)
# f20 = f19 %>%
#   group_by(WHORegionName)%>%
#   summarise(n = sum(total_count))
# 
# f21 = left_join(f19,f20)
# 
# f21 = f21 %>% mutate(
#   Region = factor(WHORegionName,
#                   levels = c("Global",
#                              "Western Pacific Region",
#                              "Eastern Mediterranean Region",
#                              "European Region",
#                              "South-East Asia Region",
#                              "Region of the Americas",
#                              "African Region"
#                   ))
# )

# fig3.2a = f17 %>% mutate(
#   variable = "Core surveillance components"
# )%>%
#   rename(
#     Value = Core
#   ) %>%
#   mutate(
#     Value = factor(Value, levels= c("Available", "Not available","No data"))
#   )
# 
# fig3.2b = f21 %>% mutate(
#   variable = "GLASS laboratories all with EQA"
# ) %>%
#   rename(
#     Value = All_GLASS_labs_With_EQA
#   ) %>%
#   mutate(
#     Value = factor(Value, levels= c("Available", "Not available","No data"))
#   )
# 
# fig3.2both = rbind(fig3.2a,fig3.2b)

# IF WANT TO USE ALL COUNTRIES IN THE WHOREGION AS OUR DENOMINATOR
#-----------------------------------------------------------------------------
# countrytotals = data.frame(table(cdata$WHORegionName)) 
# names(countrytotals) = c("WHORegionName", "Ntotal")
# 
# countrytotals = rbind(countrytotals, data.frame(cbind(WHORegionName="Global", Ntotal=216)))
# 
# fig3.2both = left_join(fig3.2both, countrytotals) %>%
#   mutate(Ntotal = as.numeric(Ntotal))
# 
# cols = brewer.pal(n = 3, name = 'Dark2')
# 
# dummy_data <- fig3.2both %>%
#   distinct(Region, variable) %>% # Keep unique regions and facets
#   mutate(
#     total_count = 100, # Set total to 100 for the background bar
#     Value = "Not GLASS enrolled" # New category for the grey bar
#   )

# Combine dummy and original data
#fig3.2both_with_bg <- bind_rows(dummy_data, fig3.2both)

# cols = brewer.pal(n = 3, name = 'Dark2')
# scales::show_col(cols,cex_label = .7) 
# 
# p1 <- ggplot(fig3.2a, aes(x = Region, y = total_count / n * 100, fill = Value)) + 
#   # geom_bar(
#   #   data = dummy_data, # Plot the grey bars first
#   #   aes(y = total_count),
#   #   stat = "identity",
#   #   fill = "grey97",
#   #   color = NA
#   # ) +
#   geom_bar(stat = "identity", position =position_stack(reverse = TRUE)) + # Overlay the actual data
#   # geom_text(
#   #   aes(label = paste0(total_count)), # Add region and number of countries
#   #   position = position_stack(reverse = TRUE, vjust=0.5), # Position text in the middle of the bar
#   #   size = 5, # Adjust size of the text
#   #   color = "white"
#   # ) +
#   theme_minimal() +
#   scale_fill_manual(
#     values = c(
#       "Available" = 'lightseagreen',
#       "Not available" = "brown1",
#       "No data" = "grey77"
#     ),
#     name = "",
#     labels = c(
#       "Available" = "All available",
#       "Not available" = "Not all available",
#       "No data" = "Information is incomplete or not reported"
#     )
#   ) +
#   labs(
#     x = "",
#     y = 'Percentage of enrolled CTAs (%)',
#     title = "Core surveillance components",
#     subtitle = ""
#   ) +
#   # Add total number annotation only to the right-hand side panel
#   # geom_text(
#   #   data = fig3.2both %>% filter(variable == "GLASS laboratories all with EQA"), # Filter for the right panel
#   #   aes(
#   #     label = paste0("n=", n), 
#   #     y =102  # Position slightly outside the bars
#   #   ),
#   #   hjust = 0,  # Align to the right of the bar
#   #   size = 7
#   # ) +
#   # annotate("text", x = 6, y = 130, label = "Core surveillance components:
#   #  National coordination centre; National reference laboratory \n with participation in EQA; 
#   #           Use of international AST standards", size = 4)+
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.1)),
#     breaks = c(0,20,40,60,80,100)# Add 20% space on the upper end of the y-axis
#   ) +
#   theme(
#     plot.subtitle = element_text(hjust = 0, size=16),
#     plot.title = element_text(size = 24),
#     axis.text.x = element_text(size = 22),
#     axis.text.y = element_text(size = 24),
#     axis.title.x = element_text(size = 22,
#                                 margin = margin(t = 16), hjust=0.5), # X-axis label
#     axis.title.y = element_text(size = 20),
#     legend.text = element_text(size = 19),
#     legend.title = element_blank(),
#     #legend.position = "bottom",
#     strip.text.x = element_text(size = 22, hjust = 0),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     plot.margin = unit(c(1, 1, 2.7, 1), "cm"),
#     legend.position = c(0, 0),              # Move legend to bottom-left corner
#     legend.justification = c(0, 2.2)
#     )+
#   guides(fill = guide_legend(nrow = 3))+
#   coord_flip()
# 
# p1
# 
# p2 <- ggplot(fig3.2b, aes(x = Region, y = total_count / n * 100, fill = Value)) + 
#   # geom_bar(
#   #   data = dummy_data, # Plot the grey bars first
#   #   aes(y = total_count),
#   #   stat = "identity",
#   #   fill = "grey97",
#   #   color = NA
#   # ) +
#   geom_bar(stat = "identity", position = position_stack(reverse=TRUE)) + # Overlay the actual data
#   # geom_text(
#   #   aes(label = paste0(total_count)), # Add region and number of countries
#   #   position = position_stack(reverse = TRUE, vjust=0.5), # Position text in the middle of the bar
#   #   size = 5, # Adjust size of the text
#   #   color = "white"
#   # ) +
#   theme_minimal() +
#   scale_fill_manual(
#     values = c(
#       "Available" = 'lightseagreen',
#       "Not available" = "brown1",
#       "No data" = "grey77"
#     ),
#     name = "",
#     labels = c(
#       "Available" = "Provided to all laboratories",
#       "Not available" = "Not provided to all laboratories",
#       "No data" = "Information is incomplete or not reported"
#     )
#   ) +
#   labs(
#     x = "",
#     y = 'Percentage of enrolled CTAs (%)',
#     title = "External quality assurance of laboratories reporting to GLASS",
#     subtitle = ""
#   ) +
#   # Add total number annotation only to the right-hand side panel
#   geom_text(
#     data = fig3.2both %>% filter(variable == "GLASS laboratories all with EQA"), # Filter for the right panel
#     aes(
#       label = paste0("n=", n), 
#       y =102  # Position slightly outside the bars
#     ),
#     hjust = 0,  # Align to the right of the bar
#     size = 7
#   ) +
#   # annotate("text", x = 6, y = 130, label = "Core surveillance components:
#   #  National coordination centre; National reference laboratory \n with participation in EQA; 
#   #           Use of international AST standards", size = 4)+
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.1)),
#     breaks = c(0,20,40,60,80,100)# Add 20% space on the upper end of the y-axis
#   ) +
#   theme(
#     plot.subtitle = element_text(hjust = 0, size=16),
#     plot.title = element_text(size = 24),
#     axis.text.x = element_text(size = 22),
#     axis.text.y = element_blank(),
#     axis.title.x = element_text(size = 22,
#                                 margin = margin(t = 16), hjust=0.5), # X-axis label
#     axis.title.y = element_text(size = 20),
#     legend.text = element_text(size = 19),
#     legend.title = element_blank(),
#     #legend.position = "bottom",
#     strip.text.x = element_text(size = 22, hjust = 0),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     plot.margin = unit(c(1, 1, 2.7, 1), "cm"),
#     legend.position = c(0, 0),              # Move legend to bottom-left corner
#     legend.justification = c(0, 2.2)
#   )+
#   guides(fill = guide_legend(nrow = 3))+
#   coord_flip()
# 
# p2
# combined_plot <- p1 + p2 + plot_layout(ncol = 2)  # Arrange in 2 columns
# combined_plot

fig2.2 = read.csv(paste0(dirDataNewO, "/Figure_2.2_NewCoreComponentsPlot.csv"), header=T)
fig2.2$CoreComponents = factor(fig2.2$CoreComponents, levels = c("All available", "Not all available", "Information is incomplete or not reported"))

fig2.2 = fig2.2 %>% mutate(
  Region = factor(Region,
                  levels = c("Global",
                             "Western Pacific Region",
                             "Eastern Mediterranean Region",
                             "European Region",
                             "South-East Asia Region",
                             "Region of the Americas",
                             "African Region"
                  ))
)
cols = brewer.pal(n = 3, name = 'Dark2')
scales::show_col(cols,cex_label = .7) 

p1 <- ggplot(fig2.2, aes(x = Region, y = Percentage, fill = CoreComponents)) + 
  # geom_bar(
  #   data = dummy_data, # Plot the grey bars first
  #   aes(y = total_count),
  #   stat = "identity",
  #   fill = "grey97",
  #   color = NA
  # ) +
  geom_bar(stat = "identity", position =position_stack(reverse = TRUE)) + # Overlay the actual data
  # geom_text(
  #   aes(label = paste0(total_count)), # Add region and number of countries
  #   position = position_stack(reverse = TRUE, vjust=0.5), # Position text in the middle of the bar
  #   size = 5, # Adjust size of the text
  #   color = "white"
  # ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "All available" = 'lightseagreen',
      "Not all available" = "brown1",
      "Information is incomplete or not reported" = "grey77"
    ),
    name = "",
    labels = c(
      "All available" = "All available",
      "Not all available" = "Not all available",
      "Information is incomplete or not reported" = "Information is incomplete or not reported"
    )
  ) +
  labs(
    x = "",
    y = 'Percentage of countries reporting 2023 AMR data to GLASS (%)',
    title = "Core surveillance components",
    subtitle = ""
  ) +
  #Add total number annotation only to the right-hand side panel
    geom_text(
      data = fig2.2,
      aes(
        label = paste0("n=", TotalCountries),
        y =101  # Position slightly outside the bars
      ),
      hjust = 0,  # Align to the right of the bar
      size = 7
    ) +
  scale_y_continuous(
    limits = c(0, 110),  # Ensure labels at y = 101 are within bounds
    expand = expansion(mult = c(0, 0)),
    breaks = c(0, 20, 40, 60, 80, 100)
  )+
  theme(
    plot.subtitle = element_text(hjust = 0, size=16),
    plot.title = element_text(size = 24),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 22,
                                margin = margin(t = 16), hjust=0.5), # X-axis label
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.title = element_blank(),
    #legend.position = "bottom",
    strip.text.x = element_text(size = 22, hjust = 0),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    plot.margin = unit(c(1, 2, 4, 1), "cm"),
    legend.position = c(0, 0),              # Move legend to bottom-left corner
    legend.justification = c(0, 2.2)
  )+
  guides(fill = guide_legend(nrow = 3))+
  coord_flip()

p1


# For report
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.2_Implementation_status_2023.svg"), 
       plot = p1,
       device = svg,
       dpi = 300,
       width = 16, height = 12)

# For writing
#write.csv(fig3.2a, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.2a_Implementation_status.csv"))
#write.csv(fig3.2b, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.2b_Implementation_status.csv"))
write.csv(fig2.2, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.2_Implementation_status_UPDATED.csv"))

write.csv(f12b, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Ch3_Implementation_status_all_variables.csv"))

# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.3(a-d): Map of BCIs with AST results reported to GLASS-AMR per one million population by CTA per infectious syndrome in 2022; 
# a) Bloodstream infections; b) Urinary tract infections; c) Gastrointestinal tract infections; and d) Urogenital infections.
# --------------------------------------------------------------------------------------------------------------------------------------
# FOR CHAPTER 3, THE BCI PER MILLION, WE WORK WITH THE CRUDE DATA THAT O. TOSAS HAS WRANGLED; 
# NOT THE DATASET HERE 3GCE and MRSA ARE ADDED AS SEPERATE CLASSES

# Get testing and AMR rates
m1 = adataAC_crude %>% # Use crude data, i.e. where the MRSA and 3gc e. coli is not data wrangled 
  filter(TotalSpecimenIsolates>0 & !AntibioticName%in%c("Ampicillin")) %>%   # Filter the data first; remove ampicillin isolates
  droplevels() %>%
  select(c(WHORegionCode, Iso3, Year, Specimen, SpecimenIsolateswithAST, TotalPopulation)) %>%
  group_by(WHORegionCode, Iso3, Year, Specimen) %>% 
  summarize_all(max) %>%
  mutate(
    BCI_permillion = ((coalesce(SpecimenIsolateswithAST,0)/coalesce(TotalPopulation,0))*1000000)
  )

# FOR ECCMID ABSTRACT - SYSTEMATIC REVIEW
#m1.1 = m1 %>% filter(Year %in% c(2019,2020,2021,2022) & Specimen %in%c("BLOOD","URINE","STOOL")) %>%
#  group_by(WHORegionCode, Iso3, Specimen) %>%
#  summarise(SpecimenIsolateswithAST = sum(SpecimenIsolateswithAST),
#            TotalPopulation = median(TotalPopulation))

# For ECCMID
#write.csv(m1.1, file = "../../../PRESENTATION/CONFERENCE_ABSTRACTS/TBL_BCIperMillion_specimen2019-2022.csv")

# CREATE DIFFERENT DATA BINS. THE BINS ARE PERCENTILES, AND WILL HAVE SIMILAR COUNTS OF ISO3 EACH. IN THIS INSTANCE, WE CREATE THE SAME BINS FOR ALL SPECIMEN TYPES AND YEARS
# WE CHOOSE TO PLOT 8 DATA BINS WHICH, BEING BASED ON ALL DATA, WILL BE APPLICABLE TO ALL YEARS (2016, 2018, 2020, 2022) AND SPECIMEN TYPES (bloodstream, urine, genital, stool)

m2 <-m1 %>% ungroup() %>% 
  mutate(Bins8q = cut(BCI_permillion, 
                                breaks = quantile(BCI_permillion, probs = seq(0, 1, by = 1/8), na.rm = TRUE),
                                include.lowest = TRUE, labels = FALSE)) %>%
  as.data.frame()

table(m2$Bins8q[m2$Year==2023])

m3<-m2 %>% mutate(var  = Bins8q)

m3$var <- factor(m3$var, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))


breaks = m3 %>%
  filter(!is.na(var)) %>%   # Remove rows where 'var' is NA
  group_by(var) %>%         # Group by 'var'
  summarise(max_BCI_permillion = max(BCI_permillion, na.rm = TRUE))
breaks$max_BCI_permillion = round(breaks$max_BCI_permillion,0)

breaks = breaks[[2]]
labels <- as.character(c(0,round(breaks,1)[1:7]))

fig3.3 = m3 %>%
  filter(!(Specimen%in%c("BLOOD", "URINE", "STOOL") & Iso3=="GBR" & Year==2023))


p1 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2023, specimen = "BLOOD", palette = palette_map4, na_color = "white", breaks=breaks, labels=labels)
p2 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2023, specimen = "URINE", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)
p3 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2023, specimen = "STOOL", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)
p4 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2023, specimen = "UROGENITAL", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)

#combined_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)  # Arrange in 2 columns
#combined_plot

# ggsave(filename = paste0(dirOutput, "/Report/Chapter3_surveillance_coverage/Figure_3.3_Map_BCIs_per_million_chlor.png"), 
#        plot = combined_plot, 
#        width = 15, height = 20)

# ggsave(filename = paste0(dirOutput, "/Report/Chapter3_surveillance_coverage/Figure_3.3_Map_BCIs_per_million_chlorBSI.svg"), plot = p1, 
#        width = 15, height = 10)

# pdf(paste0(dirOutput, "/Report/chapter3_surveillance_coverage/Figure_3.3_Map_BCIs_per_million_chlor_alt2.pdf"), 
#     width = 15, height = 15) 
# multiplot(p1,p2,p3,p4, cols = 2)  # Arrange in 2 columns
# dev.off()

# For writing
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3_BCIperMillion_specimen.svg"), plot = combined_plot, 
#        width = 24, height = 10)

#ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3a_BCIperMillion_BSI_2023.svg"), 
ggsave(filename = paste0(dirOutputReportNEW, "/CHAPTER 2/Ch2 Figures/Final/Figure_2.3_Bloodstream_110825.svg"), 
       plot = p1,
       device = svg, width = 10, height = 5, dpi = 300)

#ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3c_BCIperMillion_UTI_2023.svg"), 
ggsave(filename = paste0(dirOutputReportNEW, "/CHAPTER 2/Ch2 Figures/Final/Figure_2.3_UrinaryTract_110825.svg"), 
       plot = p2,
       device = svg, width = 10, height = 5, dpi = 300)

#ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3b_BCIperMillion_GI_2023.svg"), 
ggsave(filename = paste0(dirOutputReportNEW, "/CHAPTER 2/Ch2 Figures/Final/Figure_2.3_Gastrointestinal_110825.svg"), 
       plot = p3,
       device = svg, width = 10, height = 5, dpi = 300)

#ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3d_BCIperMillion_URO_2023.svg"), 
ggsave(filename = paste0(dirOutputReportNEW, "/CHAPTER 2/Ch2 Figures/Final/Figure_2.3_Urogenital_110825.svg"), 
       plot = p4,
       device = svg, width = 10, height = 5, dpi = 300)

# For writing
#write.csv(fig3.3, file = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITTING/FINAL 2023 FIGURES TABLES AND RESULTS/Chapter 3/Ch3 summary stats/Figure_3.3_BCIperMillion_specimen.csv")
write.csv(fig3.3, file = paste0(dirOutputReportNEW, "/CHAPTER 2/Ch2 summary stats/Figure_3.3_BCIperMillion_specimen.csv"))


# 3.3	GLASS-AMR surveillance coverage 
###################################################################

# TABLE 3.1 BCI PER PATHOGEN
#-------------------------------------------------------------------------------------------------
# Get testing by pathogen
m0 <- adataAC_crude %>%
  filter(TotalSpecimenIsolates > 0 & !AntibioticName%in%c("Ampicillin")) %>%   # Filter valid reports
  droplevels() %>%
  select(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName, 
         PathogenIsolateswithAST, TotalPopulation) %>%
  group_by(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName) %>% 
  summarize_all(max, na.rm = TRUE) %>%
  mutate(
    BCI_permillion = (coalesce(PathogenIsolateswithAST, 0) / 
                        coalesce(TotalPopulation, 0)) * 1e6
  )

m1 <- m0 %>%
  group_by(WHORegionCode, WHORegionName, Year, Specimen, PathogenName) %>%
  summarise(
    TotalPathogenIsolateswithAST = sum(PathogenIsolateswithAST, na.rm = TRUE),
    .groups = "drop"
  )

# Number of countries per specimen
cta_region = m0 %>% group_by(WHORegionName, Specimen, Year) %>%
  summarise(n = n_distinct(Iso3), .groups = "drop")


cta_region_2016_2023 = cta_region %>% 
  group_by(WHORegionName, Specimen) %>%
  summarise(n = max(n))%>%
  mutate(Period = "2016-2023")

cta_region_2023 = cta_region %>% filter(Year==2023)%>%
  mutate(Period = "2023") %>%
  select(WHORegionName, Specimen, Period,n)

cta2016_2023 = cta_region_2016_2023 %>%
  group_by(Specimen) %>%
  summarise(
    n = sum(n)
  ) %>%
  mutate(Period = "2016-2023")

cta2023= cta_region_2023 %>%
  group_by(Specimen) %>%
  summarise(
    n = sum(n)
  ) %>%
  mutate(Period = "2023")

# Summarize for 2016–2023
summary_2016_2023 <- m1 %>%
  group_by(WHORegionName, Specimen, PathogenName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2016-2023")

# Summarize for 2023
summary_2023 <- m1 %>%
  filter(Year == 2023) %>%
  group_by(WHORegionName, Specimen, PathogenName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2023")

# Combine Summaries
summary_combined <- bind_rows(summary_2016_2023, summary_2023)

# Compute unique country counts (per Pathogen/Specimen/Period)
country_counts <- m0 %>%
  filter(PathogenIsolateswithAST > 0) %>%
  group_by(WHORegionName, Specimen, PathogenName, Year) %>%
  summarise(CountryCount = n_distinct(Iso3), .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2023, "2023", "2016-2023")) %>%
  group_by(WHORegionName, Specimen, PathogenName, Period) %>%
  summarise(CountryCount = max(CountryCount, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = CountryCount) %>%
  mutate(`2016-2023` = ifelse(!is.na(`2023`) & `2023` > `2016-2023`, `2023`, `2016-2023`)) %>%
  pivot_longer(cols = c("2016-2023", "2023"), names_to = "Period", values_to = "CountryCount")

# Merge country counts into Summary
summary_with_counts <- summary_combined %>%
  left_join(country_counts, by = c("WHORegionName", "Specimen", "PathogenName", "Period")) %>%
  mutate(
    TotalWithCountries = paste0(
      format(TotalIsolates, big.mark = ","),
      " (", CountryCount, ")"
    )
  )

# Add Specimen Totals
specimen_totals <- summary_with_counts %>%
  group_by(Specimen, WHORegionName, Period) %>%
  summarise(
    TotalIsolates = sum(TotalIsolates, na.rm = TRUE),
    CountryCount = max(CountryCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PathogenName = "Total",
    TotalWithCountries = paste0(
      format(TotalIsolates, big.mark = ","),
      " (", CountryCount, ")"
    )
  )

# Combine with rest of data
summary_with_totals <- bind_rows(summary_with_counts, specimen_totals)

# Pivot wider (manually)
summary_with_totals_2023 = summary_with_totals %>%filter(Period == 2023)
summary_with_totals_2016_2023 = summary_with_totals %>%filter(Period != 2023)
TotalwithCountries2016_2023 = summary_with_totals_2016_2023$TotalWithCountries
TotalwithCountries2023 = summary_with_totals_2023$TotalWithCountries

summary_table = cbind(summary_with_totals_2023%>%select(WHORegionName, Specimen,PathogenName),
                      TotalwithCountries2016_2023,TotalwithCountries2023)

# Add global totals
global_summary <- summary_with_counts %>%
  group_by(Specimen, PathogenName, Period) %>%
  summarise(
    TotalIsolates = sum(TotalIsolates, na.rm = TRUE),
    CountryCount = sum(CountryCount, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(
    WHORegionName = "Global",
    TotalWithCountries = paste0(
      format(TotalIsolates, big.mark = ","),
      " (", CountryCount, ")"
    )
  )

# Add Specimen totals for global
specimen_totals_global <- summary_with_counts %>%
  group_by(Specimen, Period) %>%
  summarise(
    TotalIsolates = sum(TotalIsolates, na.rm = TRUE),
    #CountryCount = sum(CountryCount, na.rm = TRUE),
    .groups = "drop"
  ) 

cta_c = rbind(cta2016_2023,cta2023) # If this gives error, copy paste in the console and it works
cta_c = cta_c %>%
  rename(CountryCount = n)

specimen_totals_global = left_join(specimen_totals_global, cta_c)

specimen_totals_global = specimen_totals_global %>%
  mutate(
    PathogenName = "Total",
    WHORegionName = "Global",
    TotalWithCountries = paste0(
      format(TotalIsolates, big.mark = ","),
      " (", CountryCount, ")"
    )
  )

# Combine global summaries
final_global_table <- bind_rows(global_summary, specimen_totals_global)

# Pivot wider (manually) for final table 
summary_with_totals_2023_global = final_global_table %>%filter(Period == 2023)
summary_with_totals_2016_2023_global = final_global_table %>%filter(Period != 2023)
TotalwithCountries2016_2023_global = summary_with_totals_2016_2023_global$TotalWithCountries
TotalwithCountries2023_global = summary_with_totals_2023_global$TotalWithCountries

summary_table_global = cbind(summary_with_totals_2023_global%>%select(WHORegionName, Specimen,PathogenName),
                             TotalwithCountries2016_2023_global,TotalwithCountries2023_global) 

summary_table_global = summary_table_global %>%
  rename(
    TotalwithCountries2016_2023 = TotalwithCountries2016_2023_global,
    TotalwithCountries2023 = TotalwithCountries2023_global
  )

# Combine regional and global Summaries
final_table <- bind_rows(summary_table, summary_table_global) %>%
  arrange(Specimen, PathogenName, WHORegionName)

summary_table_wide <- final_table %>%
  pivot_wider(
    names_from = c(WHORegionName),  # Combine region and period for column names
    values_from = c("TotalwithCountries2016_2023", "TotalwithCountries2023"),           # Values come from TotalIsolates
    values_fill = "0 (0)"                        # Fill missing values with 0
  ) %>% dplyr::select(Specimen, PathogenName, 'TotalwithCountries2016_2023_Global','TotalwithCountries2023_Global',
                      'TotalwithCountries2016_2023_African Region','TotalwithCountries2023_African Region',
                      'TotalwithCountries2016_2023_Region of the Americas','TotalwithCountries2023_Region of the Americas',
                      'TotalwithCountries2016_2023_South-East Asia Region','TotalwithCountries2023_South-East Asia Region',
                      'TotalwithCountries2016_2023_European Region','TotalwithCountries2023_European Region',
                      'TotalwithCountries2016_2023_Eastern Mediterranean Region','TotalwithCountries2023_Eastern Mediterranean Region',
                      'TotalwithCountries2016_2023_Western Pacific Region','TotalwithCountries2023_Western Pacific Region')

# I NOTICED THAT THERE IS A SLIGHT DIFFERENCE BETWEEN THE TOTAL ISOLATES FOR Streptococcus pneumoniae
# i.e. 21006 total from website. This is 20960 here. All others are the same numbers

# Prepare headers for multi-level formatting
header_row1 <- c("Specimen", "PathogenName", 
                 rep("Global", 2), 
                 rep("African Region", 2), 
                 rep("Region of the Americas", 2), 
                 rep("South-East Asia Region", 2), 
                 rep("European Region", 2), 
                 rep("Eastern Mediterranean Region", 2), 
                 rep("Western Pacific Region", 2))

header_row2 <- c("", "", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023", 
                 "2016-2023", "2023")

# Combine headers and data
formatted_table <- rbind(
  header_row1,  # Add the first header row
  header_row2,  # Add the second header row
  summary_table_wide  # Add the actual data
)

# SLIGHT ADJUSTMENTS
df <- as.data.frame(lapply(formatted_table, function(x) {
  if (is.character(x)) {
    gsub(",", " ", x)  # Replace commas with spaces
  } else {
    x  # Leave non-character columns unchanged
  }
}))

df$Specimen = c("", "","","","","","","","Bloodstream","","",
                "Gastrointestinal tract","", "","Urinary tract", "", "Gonorrhoea (N. gonorrhoea)")
df = df[df$PathogenName!="Neisseria gonorrhoeae",]
df$PathogenName = c("","", "Acinetobacter spp.","E. coli","K. pneumoniae",   
                    "Salmonella spp.","S. aureus","S. pneumoniae", "Total bloodstream","Salmonella spp.",         
                    "Shigella spp.","Total gastrointestinal","E. coli","K. pneumoniae","Total urinary tract",                   
                     "Total gonorrhoea (N. gonorrhoea)")

#df = df[c(1,2,9,3,4,5,6,7,8,12,10,11,15,13,14,16),]

# Add overall totals
country_counts <- m0 %>%
  filter(PathogenIsolateswithAST > 0) %>%  # Only include isolates with AST data
  group_by(WHORegionName, Year) %>%
  summarise(CountryCount = n_distinct(Iso3), .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2023, "2023", "2016-2023")) %>%
  group_by(WHORegionName, Period) %>%
  summarise(CountryCount = max(CountryCount, na.rm = TRUE), .groups = "drop")%>%
  pivot_wider(names_from = Period, values_from = CountryCount) %>%
  mutate(`2016-2023` = ifelse(!is.na(`2023`) & `2023` > `2016-2023`, `2023`, `2016-2023`))%>%
  pivot_longer(cols = c("2016-2023", "2023"), names_to = "Period", values_to = "CountryCount")


global_counts <- m0 %>%
  filter(PathogenIsolateswithAST > 0) %>%  # Only include isolates with AST data
  group_by(Year) %>%
  summarise(CountryCount = n_distinct(Iso3), .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2023, "2023", "2016-2023")) %>%
  group_by(Period) %>%
  summarise(CountryCount = max(CountryCount, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = CountryCount) %>%
  mutate(`2016-2023` = ifelse(!is.na(`2023`) & `2023` > `2016-2023`, `2023`, `2016-2023`))%>%
  pivot_longer(cols = c("2016-2023", "2023"), names_to = "Period", values_to = "CountryCount")%>%
  mutate(
    WHORegionName = "Global"
  ) %>%
  select(WHORegionName, Period, CountryCount)

global_counts = rbind(country_counts,global_counts)

summary_2016_2023 <- m1 %>%
  group_by(WHORegionName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2016-2023")

# Summarize for 2023
summary_2023 <- m1 %>%
  filter(Year == 2023) %>%
  group_by(WHORegionName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2023")

# Combine Summaries global
summary_combined <- bind_rows(summary_2016_2023, summary_2023)

summary_2016_2023 <- m1 %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2016-2023")

# Summarize for 2023
summary_2023 <- m1 %>%
  filter(Year == 2023) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2023")

summary_combined_global <- bind_rows(summary_2016_2023, summary_2023) %>%
  mutate(WHORegionName = "Global") %>%
  select(c(WHORegionName, TotalIsolates, Period))

summary_combined_global = rbind(summary_combined, summary_combined_global)


totals_global = left_join(summary_combined_global, global_counts)

totals_global = totals_global %>%
  mutate(
    PathogenName = "Total",
    WHORegionName = WHORegionName,
    TotalWithCountries = paste0(
      format(TotalIsolates, big.mark = ","),
      " (", CountryCount, ")"
    )
  )

summary_table_wide <- totals_global %>%
  mutate(Region_Period = paste(WHORegionName, Period, sep = "_")) %>%
  select(Region_Period, TotalWithCountries) %>%
  pivot_wider(
    names_from = Region_Period,
    values_from = TotalWithCountries
  )

region_order <- c(
  "Global_2016-2023",  # 
  "Global_2023",
  "African Region_2016-2023",
  "African Region_2023",
  "Region of the Americas_2016-2023",
  "Region of the Americas_2023",
  "South-East Asia Region_2016-2023",
  "South-East Asia Region_2023",
  "European Region_2016-2023",
  "European Region_2023",
  "Eastern Mediterranean Region_2016-2023",
  "Eastern Mediterranean Region_2023",
  "Western Pacific Region_2016-2023",
  "Western Pacific Region_2023"
)

# Sort column names by custom region order and then by Period
summary_table_wide = summary_table_wide[,region_order]
totals = cbind(data.frame("", "Total"), summary_table_wide)
names(totals) =names(df)

dfs = rbind(df, totals)

# FOR SUMMARY TABLE
#tab3.1 = rbind(summary_with_totals, final_global_table)

# For writing
#write.csv(tab3.1, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.3b_BCIperMillion_specimenbypathogen.csv"))

# For report
write.csv(dfs, file = paste0(dirOutputReport, "/Chapter 3/Ch3 Tables/Table3.1_BCIperMillion_specimenbypathogen.csv"))



# 3.4 CRITICAL INFORMATION GAPS in GLASS-AMR
###################################################################

# Table 3.1: Information gaps in GLASS-AMR that impact the interpretation of AMR surveillance data by type  
#---------------------------------------------------------------------------------------------------------------

c1 = read.csv(file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata_NEW2.csv"))
#c1 = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata_definitions.xlsx"))
names(c1)
c1 = c1 %>% dplyr::select(-c("X", "WHORegionName")) 

c2 <- c1 %>%
  pivot_longer(-Iso3, names_to = "Category", values_to = "Value") %>%
    mutate(
      Datatype = case_when(
        Category == "colB_nssi" ~ "Surveillance_core_components",
        Category %in% c("colC_infs_inp","colD_util_inp","colE_util_outp","ColF_GLASS_infs_inp", 
                        "ColG_GLASS_util_inp","ColH_GLASS_util_outp") ~ "Surveillance_coverage",
        Category %in% c("ColI_nsamp","ColK_age","ColL_sex","ColM_origin") ~ "AMR_covariate_denominator",
        Category %in% c("TotalScore") ~ "Total_Score",
        TRUE ~ "AMR_specimen"
      ),
      Datatype = factor(Datatype, levels = c(
        "Surveillance_core_components", "Surveillance_coverage", "AMR_specimen",
        "AMR_covariate_denominator", "Total_Score"
      )),
      Value_label = case_when(
        Category == "TotalScore" & Value %in% 0:6 ~ "Low completeness",         # 0-20%
        Category == "TotalScore" & Value %in% 7:15 ~ "Medium-low completeness", # 21-50%
        Category == "TotalScore" & Value %in% 16:24 ~ "Medium-high completeness", # 51-80%
        Category == "TotalScore" & Value %in% 25:30 ~ "High completeness",      # >80%
        is.na(Value) ~ "No data",
        
        # for non-TotalScore categories
        Value == 0 ~ "No data",
        Value == 1 ~ "Some data",
        Value == 2 ~ "Good data",
        TRUE ~ NA_character_
      )
    )
    
c2$Value = factor(c2$Value, levels=sort(unique(c2$Value)))

unique(c2$Value)
c2$Value_label = factor(c2$Value_label, levels = c("Low completeness","No data", "Medium-low completeness","Some data",
                                                                 "Medium-high completeness", "Good data", "High completeness"))

c3 = left_join(c2, cdata %>%select(Iso3,CountryTerritoryArea, WHORegionName, WHORegionCode, IncomeWorldBankJune2023), by="Iso3")

c3$CountryTerritoryArea2 = ifelse(c3$CountryTerritoryArea=="Kosovo (in accordance with UN Security Council resolution 1244 (1999))", "Kosovo",c3$CountryTerritoryArea)
c3$CountryTerritoryArea2 = ifelse(c3$CountryTerritoryArea2=="occupied Palestinian territory, including east Jerusalem", "Palestinian territory",c3$CountryTerritoryArea2)
c3$CountryTerritoryArea2 = ifelse(c3$CountryTerritoryArea2=="Netherlands (Kingdom of the)" , "Netherlands",c3$CountryTerritoryArea2)
c3$CountryTerritoryArea2 = ifelse(c3$CountryTerritoryArea2=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom",c3$CountryTerritoryArea2)

labels = c(
  "colB_nssi" = "Core surveillance\npresent",
  "colC_infs_inp" = "N facilities\ninpatient",
  "colD_util_inp" = "N admissions/bed days\ninpatient",
  "colE_util_outp" = "N consultations\noutpatient",
  "ColF_GLASS_infs_inp" = "N GLASS facilities\ninpatient",
  "ColG_GLASS_util_inp" = "N GLASS admissions/beddays\ninpatient",
  "ColH_GLASS_util_outp" = "N GLASS consultations\noutpatient",
  "ColI_nsamp" = "N tested \nreported",
  "ColK_age" = "Age \nreported",
  "ColL_sex" = "Sex \nreported",
  "ColM_origin" = "Infection origin\nreported",
  "ColN_nBLOOD_BLOOD" = "BSI >10 with AST",
  "ColP_nSTOOL_STOOL" = "GI >10 with AST",
  "ColO_nURINE_URINE" = "UTI >10 with AST",
  "ColQ_nURO_UROGENITAL" = "Urogenital >10 with AST"
)

# value_colors <- c(
#   "No data" = "white",
#   "Some data" = reds[3],
#   "Good data" = blues[3],
#   "Low completeness" = "#e0f3db",
#   "Medium-low completeness" = "#a8ddb5",
#   "Medium-high completeness" = "mediumseagreen",
#   "High completeness" = "darkgreen",
#   "Not applicable (no 2022 GLASS data)" = "#f0f0f0"
# )

value_colors <- c(
  "No data" = "#D32F2F",  # Red
  "Some data" = "#FFC107",  # Yellow
  "Good data" = "#4CAF50",  # Green
  "Low completeness" ="#e1f5fe",         # Very pale light blue
  "Medium-low completeness" =  "#BBDEFB",  # Very light blue
  "Medium-high completeness" = "#4FC3F7", # Light-medium blue
  "High completeness" = "#01579B",         # Darker blue
  "Not applicable (no 2023 GLASS data)" = "grey90"
)

c3$WHORegionName = factor(c3$WHORegionName,
                levels = c("African Region",
                           "Region of the Americas",
                           "South-East Asia Region",
                           "European Region",
                           "Eastern Mediterranean Region",
                           "Western Pacific Region"
                )
)

unique(c3$Category)
c3$Category = factor(c3$Category, levels = c(
  "colB_nssi",
  "colC_infs_inp",
  "colD_util_inp",
  "colE_util_outp",
  "ColF_GLASS_infs_inp",
  "ColG_GLASS_util_inp",
  "ColH_GLASS_util_outp",
  "ColI_nsamp",
  "ColK_age",
  "ColL_sex",
  "ColM_origin",
  "ColN_nBLOOD_BLOOD",
  "ColP_nSTOOL_STOOL",
  "ColO_nURINE_URINE",
  "ColQ_nURO_UROGENITAL",
  "TotalScore"
))
# Create the heatmap with top-aligned x-axis and grouped categories
p = ggplot(c3, aes(x = Category, y = fct_rev(fct_reorder(CountryTerritoryArea2, CountryTerritoryArea2)), fill = as.factor(Value_label))) +
  geom_tile(fill = NA, color = NA, width = 1, height = 1)+
  geom_point(aes(color = as.factor(Value_label)), shape = 15, size = 3, fill = NA)+
  scale_colour_manual(
    values = value_colors,
    name = "",
    labels = c(
      "No data" = "No data",
      "Some data" = "Incomplete data",
      "Good data" = "All data available",
      "Low completeness" = "Low (≤20%)",
      "Medium-low completeness" = "Medium-low (>20-50%)",
      "Medium-high completeness" = "Medium-high (>50-80%)",
      "High completeness" = "High (>80%)",
      "Not applicable (no 2023 GLASS data)" = "Not applicable for 2023"
    )
  ) +
  labs(x = "", y = "") +
  facet_grid(WHORegionName ~ Datatype, scales = "free", space = "free",
              labeller = labeller(
               Datatype = as_labeller(c(
                  "Surveillance_core_components" = "National \nAMR \nsurveillance \nsystem",
                  "Surveillance_coverage" = "National coverage of GLASS",
                  "AMR_covariate_denominator" ="Epidemiologic, demographic and \nclinical data",
                  "AMR_specimen" =  "Data reported to GLASS",
                  "Total_Score" = "Score"
                )),
                WHORegionName = as_labeller(c(
                  "African Region" = "African Region",
                  "Region of the Americas" = "Region \nof the \nAmericas",
                  "South-East Asia Region" = "South-East \nAsia \nRegion",
                  "European Region" = "European Region",
                  "Eastern Mediterranean Region" = "Eastern \nMediterranean Region",
                  "Western Pacific Region" = "Western \nPacific Region"
                ))
              )
  ) +
  scale_x_discrete(
    position = "top",                
    labels =  c(
      "colB_nssi" = "Implementation \nstatus of core \ncomponents",
      "colC_infs_inp" = "Total health \nfacilities in country",
      "colD_util_inp" = "Inpatient \nadmissions and \ndays of care per \ncalendar year in country",
      "colE_util_outp" = "Outpatient \nconsultations per \ncalendar year in country",
      "ColF_GLASS_infs_inp" = "Total health \nfacilities in GLASS",
      "ColG_GLASS_util_inp" = "Inpatient \nadmissions and \ndays of care per \ncalendar year in GLASS",
      "ColH_GLASS_util_outp" = "Outpatient \nconsultations per \ncalendar year in GLASS",
      "ColI_nsamp" =  "Number of \nsampled patients",
      "ColK_age" ="Patient’s age",
      "ColL_sex" = "Patient’s gender",
      "ColM_origin" =  "Community or \nhospital \ninfection origin",
      "ColN_nBLOOD_BLOOD" = "Bloodstream",
      "ColP_nSTOOL_STOOL" = "Gastrointestinal", 
      "ColO_nURINE_URINE" = "Urinary tract",
      "ColQ_nURO_UROGENITAL" ="Gonorrhoea",
      "TotalScore" = "Completeness \nand quality"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 12, face = "bold", margin = margin(b = 5)),
    strip.text.y = element_text(size = 13, face = "bold", margin = margin(b = 5)),
    axis.title.x = element_blank(),
    axis.text.x.top = element_text(angle = 90, hjust = 0, vjust=0.5, size = 11),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size=13),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.background = element_rect(fill = "white", color = "white"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines")  # Ensures proper spacing between facets
  )

p

# FOR REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.5_missingdata.svg"), 
       plot = p, 
       bg = "white",
       device="svg",
       width = 17, height = 21)



# Ch3.3 BCI per million trends
#-------------------------------------------------------------------

# FIGURE 3.4 - BCI trends by specimen
#-------------------------------------------------------------------

# SLOPES
#-----------------------------------------------------------------
slopes_all = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends_pathogen.xlsx"), sheet=2)
slopes_allS = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), sheet=2)

# Create datatset with crude data - Pathogen specific
d1 <- adataAC_crude %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") & (Specimen =="BLOOD")& (PathogenName=="Acinetobacter spp."|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Neisseria gonorrhoeae"|PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Third generation cephalosporins',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") & (PathogenName=="Shigella spp.") ~ 'Third generation cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (PathogenName=="Salmonella spp.") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") & (PathogenName=="Neisseria gonorrhoeae") ~ 'Macrolides',
                              (Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX") & (PathogenName=="Staphylococcus aureus") ~ 'Methicillin resistance',
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") & (PathogenName=="Streptococcus pneumoniae") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (PathogenName=="Escherichia coli"|PathogenName=="Klebsiella pneumoniae") ~ 'Sulfonamides and trimethoprim'))

d2 <- d1 %>%
  filter(!is.na(Grouping)) %>%
  droplevels()

d3 <- d2 %>% 
  filter(TotalPathogenIsolates>0) %>%   
  droplevels() 

d4 <- d3 %>% dplyr::select(c(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName, Grouping, InterpretableAST, TotalPopulation)) 

d5 <- d4 %>% 
  as.data.frame() %>%
  group_by(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName, Grouping) %>%
  summarize_all(max) %>%
  as.data.frame()


d6 <- d5 %>%
  mutate(BCI_permillion = ((coalesce(InterpretableAST,0)/(coalesce(TotalPopulation,0))*1000000)),
         combined = paste0(Specimen, "-", PathogenName, "-", Grouping),
         WHORegionCode=relevel(factor(WHORegionCode),ref="EUR")
  )

# Create dataset with crude data - specimen specific
m1 = adataAC_crude %>% 
  filter(TotalSpecimenIsolates>0) %>%   # Filter the data first
  droplevels() 

m2<- m1 %>% 
  as.data.frame() %>%
  group_by(WHORegionCode, Iso3, Year, Specimen) %>% 
  summarize_all(max) %>%
  as.data.frame() %>% 
  dplyr::select(WHORegionName,WHORegionCode, Iso3, Year, Specimen, SpecimenIsolateswithAST, TotalPopulation)


m3<-m2 %>% 
  mutate(BCI_permillion = ((coalesce(SpecimenIsolateswithAST,0)/(coalesce(TotalPopulation,0))*1000000)),
         Year_scaled = scale(Year, center = TRUE, scale = TRUE)[, 1]) # Scale centers and scales Year

# CTA PER REGION
d = m2 %>%
  group_by(WHORegionName, Specimen) %>%
  summarise(length(unique(Iso3)))

d_wide <- m2 %>%
  group_by(WHORegionName, Specimen) %>%
  summarise(n = length(unique(Iso3)), .groups = "drop") %>%
  pivot_wider(names_from = Specimen, values_from = n, values_fill = list(n = 0))

d_wide

d_wide <- m2 %>%
  group_by(Year, WHORegionName, Specimen) %>%
  summarise(n = length(unique(Iso3)), .groups = "drop") %>%
  pivot_wider(names_from = Specimen, values_from = n, values_fill = list(n = 0))

d_wide %>%filter(Year==2023)


# LOAD IN MODEL RESULTS
#-------------------------------------------------------------------

# Load in model results - Pathogen specific
#model_results = readRDS(paste0(dirDataModeloutputCh3, "weakip/Pathogen/Model_fits_scoverage_Pathogen.rds"))

# Load in model results - Specimen specific
#model_results_specimen = readRDS(paste0(dirDataModeloutputCh3, "weakip/Specimen/Model_fits_scoverage_specimen.rds"))


# SLOPES FOR SPECIMEN SPECIFIC DATA
#slopes_allS = NULL

#for(specimen in unique(m2$Specimen)){
#  d = summarize_regional_and_global_slopes(model_results_specimen[[specimen]]$model1, year_popdata = 2023, pdata, cdata, specimen=specimen)
#  slopes_allS = rbind(slopes_allS,d)
#}

#slopes_allS = slopes_allS %>% 
#  mutate(WHORegionName = factor(WHORegionName, 
#                                levels = c("Global",
#                                           "Western Pacific Region",
#                                           "Eastern Mediterranean Region",
#                                           "European Region",
#                                           "South-East Asia Region",
#                                           "Region of the Americas",
#                                           "African Region"
#                                )),
#         mean_slope_p = (exp(mean_slope)-1)*100,
#         Q2.5_p = (exp(Q2.5)-1)*100,
#        Q97.5_p = (exp(Q97.5)-1)*100,
#         combined = specimen
#  ) 

#nyears_cta <- m3 %>%
#  group_by(WHORegionName, Iso3, Specimen) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 2) #%>%
#pull(Iso3)

#ncountry_region = nyears_cta %>%
#  group_by(WHORegionName, Specimen) %>%
#  summarise(n = n())

#ncountry_global = nyears_cta %>%
#  group_by(Specimen) %>%
#  summarise(n = n()) %>%
#  mutate(WHORegionName = "Global") %>%
#  dplyr::select(WHORegionName, Specimen,n)

#ncountry = rbind(ncountry_region, ncountry_global)

#slopes_allS = slopes_allS %>%
#  rename(
#    Specimen = "specimen"
#  )

#slopes_allS = left_join(slopes_allS, ncountry)

#slopes_allS$Significant = ifelse(slopes_allS$Q2.5_p>=1&slopes_allS$n>5|slopes_allS$Q97.5_p<=-1&slopes_allS$n>5, "Yes", "No")

#slopes_allSP <- slopes_allS %>%
#  mutate(
#    text = paste0("Δt =", sprintf("%.2f", round(mean_slope_p, 1)), "%\n[", sprintf("%.2f", round(Q2.5_p, 1)), ", ", sprintf("%.2f", round(Q97.5_p, 1)), "]"),
    #text = paste0("% Change =", sprintf("%.2f", round(mean_slope_p, 1)), "\n[95%CrI:", sprintf("%.2f", round(Q2.5_p, 1)), ", ", sprintf("%.2f", round(Q97.5_p, 1)), "]")
#    Specimen=specimen,
#    region_labels= factor(WHORegionName, labels = c(
#      "Global" = "Global",
#      "African Region" = "African\nRegion",
#      "Region of the Americas" = "Region of the \nAmericas",
#      "South-East Asia Region" = "South-East\nAsia Region",
#      "European Region" = "European\nRegion",
#      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
#      "Western Pacific Region" = "Western\nPacific Region")
#    )
#  )


# TIME TRENDS FOR SPECIMEN SPECIFIC DATA
# GENERATE PREDICTIONS
#bsi_pred =  generate_predictions_summary(fit=model_results_specimen[["BLOOD"]]$model1,data=m2, year_range = c(2016,2023), pdata, cdata, specimen="BSI", specimen_filter = "BLOOD", pathogen=F) # need to change year_centre to the mean of year? 
#uti_pred =  generate_predictions_summary(fit=model_results_specimen[["URINE"]]$model1, data=m2, year_range = c(2016,2023), pdata, cdata,specimen="UTI",specimen_filter = "URINE", pathogen=F)
#gi_pred =  generate_predictions_summary(fit=model_results_specimen[["STOOL"]]$model1, data=m2, year_range = c(2016,2022), pdata, cdata, specimen="GI",specimen_filter = "STOOL", pathogen=F)
#uro_pred =  generate_predictions_summary(fit=model_results_specimen[["UROGENITAL"]]$model1, data=m2, year_range = c(2016,2022), pdata, cdata, specimen="Gonorrhoea",specimen_filter = "UROGENITAL", pathogen=F)

#specimen_pred = rbind(bsi_pred, uti_pred, gi_pred, uro_pred)
#specimen_pred = specimen_pred %>%
#  mutate(
#    Specimen=(
#      case_when(
#        specimen=="BSI" ~ "BLOOD",
#        specimen=="UTI" ~ "URINE",
#        specimen=="GI" ~ "STOOL",
#        specimen=="Gonorrhoea" ~ "UROGENITAL"
#      )
#    ),
#    region_labels = factor(WHORegionName, labels = c(
#      "Global" = "Global",
#      "African Region" = "African\nRegion",
#      "Region of the Americas" = "Region of the \nAmericas",
#      "South-East Asia Region" = "South-East\nAsia Region",
#      "European Region" = "European\nRegion",
#      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
#      "Western Pacific Region" = "Western\nPacific Region")
#    )
#  )


# TIME TRENDS FOR PATHOGEN SPECIFIC DATA
#pathogen_pred = NULL
#year_c = round(mean(unique(d6$Year)),0)

#for(drug_bug in unique(d6$combined)){
#  d = generate_predictions_summary(fit=model_results[[drug_bug]]$model1, data=d6, year_range = c(2016,2023), pdata, cdata, specimen=drug_bug) # need to change year_centre to the mean of year? 
#  pathogen_pred = rbind(pathogen_pred,d)
#}

#pathogen_all = pathogen_pred %>% 
#  mutate(WHORegionName = factor(WHORegionName, 
#                                levels = c("Global", 
#                                           "African Region",
#                                           "Region of the Americas",
#                                           "South-East Asia Region",
#                                           "European Region",
#                                           "Eastern Mediterranean Region",
#                                           "Western Pacific Region")),
#         combined = specimen,
#         region_labels = factor(WHORegionName, labels = c(
#           "Global" = "Global",
#           "African Region" = "African\nRegion",
#           "Region of the Americas" = "Region of the \nAmericas",
#           "South-East Asia Region" = "South-East\nAsia Region",
#           "European Region" = "European\nRegion",
#           "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
#           "Western Pacific Region" = "Western\nPacific Region")
#         )
#  ) %>%
#  separate(specimen, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-") %>%
#  dplyr::select(WHORegionName, region_labels, Year, mean_prediction, median_prediction, Q2.5, Q97.5, Specimen, PathogenName, AntibioticName, combined)


nyears_cta <- m3 %>%
  group_by(WHORegionName, Iso3, Specimen) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 2) #%>%
#pull(Iso3)

ncountry_region = nyears_cta %>%
  group_by(WHORegionName,Specimen) %>%
  summarise(n = n())

ncountry_global = nyears_cta %>%
  group_by(Specimen) %>%
  summarise(n = n()) %>%
  mutate(WHORegionName = "Global") %>%
  dplyr::select(WHORegionName, Specimen,n)

ncountry = rbind(ncountry_region, ncountry_global)
ncountry$specimen = ifelse(ncountry$Specimen=="BLOOD","Bloodstream",
                           ifelse(ncountry$Specimen=="URINE", "Urinary tract",
                                  ifelse(ncountry$Specimen == "STOOL", "Gastrointestinal", "Urogenital"))
)
#slopes_all$combined = paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName)


slopes_allS = left_join(slopes_allS, ncountry)

slopes_allS$Significant = ifelse(slopes_allS$Q2.5_p>=1&slopes_allS$n>5|slopes_allS$Q97.5_p<=-1&slopes_allS$n>5, "Yes", "No")


#df_plot <- slopes_all %>%
#  mutate(variable = fct_reorder(WHORegionName, mean_slope_p))

slopes_allS$label_95CrI <- sprintf("%.1f (%.1f–%.1f)", 
                                  slopes_allS$mean_slope_p,
                                  slopes_allS$Q2.5_p, 
                                  slopes_allS$Q97.5_p)
unique(slopes_allS$WHORegionName)

slopes_allS = slopes_allS %>%
  mutate(
    WHORegionName = factor(
      WHORegionName,
      levels = c("Western Pacific Region","Eastern Mediterranean Region","European Region",  "South-East Asia Region","Region of the Americas","African Region","Global")),
    specimen = factor(
      specimen,
      levels = c("Bloodstream",  "Gastrointestinal","Urinary tract", "Gonorrhoea"),
      labels = c("Bloodstream",  "Gastrointestinal", "Urinary tract", "Urogenital")
    )
  )

unique(slopes_allS$WHORegionName)
unique(slopes_allS$specimen)


p = ggplot(slopes_allS, aes(y = WHORegionName)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  # 95% CrI band
  geom_rect(aes(xmin = Q2.5_p, xmax = Q97.5_p,
                ymin = as.numeric(WHORegionName) - 0.3,
                ymax = as.numeric(WHORegionName) + 0.3,
                fill = WHORegionName),
            alpha = 0.3) +
  # 80% CrI band
  geom_rect(aes(xmin = Q10_p, xmax = Q90_p,
                ymin = as.numeric(WHORegionName) - 0.2,
                ymax = as.numeric(WHORegionName) + 0.2,
                fill = WHORegionName),
            alpha = 0.5) +
  # 50% CrI band
  #geom_rect(aes(xmin = Q25_p, xmax = Q75_p,
  #              ymin = as.numeric(WHORegionName) - 0.1,
  #              ymax = as.numeric(WHORegionName) + 0.1,
  #              fill = WHORegionName),
  #          alpha = 0.8) +
  # Posterior mean point
  geom_point(aes(x = mean_slope_p), size = 3) +
  # Vertical zero line
  facet_wrap(~ specimen, scales = "free_y") +
  scale_fill_manual(values = facet_colors3) +
  scale_color_manual(values = facet_colors3) +
  labs(
    title = "",
    subtitle = "",
    x = "Percentage (%) change",
    y = ""
  ) +
  #  geom_text(aes(x = max(Q97.5_p) + 0.5, label = label_95CrI),
  #            hjust = 0.1,
  #            size = 4,
  #            family = "Fira Sans", 
  #            fontface = "plain")+
  theme_minimal(base_size = 13) +
  geom_text(
    aes(x = max(Q97.5_p) + 0.5, 
        y = as.numeric(WHORegionName), 
        label = ifelse(Significant == "Yes", "*", "")),
    hjust = 0.1,  # adjust vertically if needed
    size = 6,
    family = "Fira Sans",
    fontface = "bold"
  )+
  #  coord_cartesian(clip = "off") +
  #  theme(plot.margin = margin(5.5, 90, 5.5, 8)) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 16, family = "Fira Sans SemiBold"),
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray85", linetype = "dotted"),
    panel.grid.major.x = element_line(color = "gray85", linetype = "dotted")
  )

p

p <- p + ggh4x::facetted_pos_scales(
  y = list(
    specimen == "Bloodstream" ~ scale_y_discrete(),
    specimen == "Urinary tract" ~ scale_y_discrete(),
    TRUE ~ scale_y_discrete(labels = NULL) # Hide labels for other facets
  )
)
p

# FIGURE 3.4
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.4_slopechange_regional_UPDATED.svg"), 
       plot = p,
       device = svg,
       dpi = 300,
       bg = "white",
       width = 11, height = 8)  


#######################################################################################################
# ANNEX 5
#######################################################################################################

# SLOPES
#-----------------------------------------------------------------
slopes_all = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends_pathogen.xlsx"), sheet=2)
slopes_allS = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), sheet=2)

slopes_all = slopes_all %>%
  mutate(
    specimen = ifelse(Specimen=="BLOOD","Bloodstream",
                      ifelse(Specimen=="URINE", "Urinary tract",
                             ifelse(Specimen == "STOOL", "Gastrointestinal", "Gonorrhoea"))),
    region_labels = factor(WHORegionName, labels = c(
           "Global" = "Global",
           "African Region" = "African\nRegion",
           "Region of the Americas" = "Region of the \nAmericas",
           "South-East Asia Region" = "South-East\nAsia Region",
           "European Region" = "European\nRegion",
           "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
           "Western Pacific Region" = "Western\nPacific Region")
    )
  )

slopes_allS = slopes_allS %>%
  mutate(
    region_labels = factor(WHORegionName, labels = c(
      "Global" = "Global",
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region")),
      Specimen = ifelse(specimen=="Bloodstream", "BLOOD",
                        ifelse(specimen=="Urinary tract", "URINE",
                               ifelse(specimen=="Gastrointestinal", "STOOL", "UROGENITAL")))
    )

# BIND TWO TOGETHER FOR ANNEX 5
spec_slope = slopes_allS %>%
  mutate(
    PathogenName = "",
    AntibioticName = "",
    combined = ""
  )%>%
  dplyr::select(WHORegionName,nCTA_3ydata, Specimen,specimen, PathogenName, AntibioticName, combined, mean_slope_p, Q2.5_p, Q97.5_p, region_labels)

path_slope = slopes_all %>%
  dplyr::select(WHORegionName,nCTA_3ydata, Specimen,specimen,PathogenName, AntibioticName, combined, mean_slope_p, Q2.5_p, Q97.5_p, region_labels)

change_all = rbind(spec_slope, path_slope)
change_all = change_all %>%
  mutate('Annual_change(%)' =  sprintf("%.1f (%.1f–%.1f)", 
                                  mean_slope_p,
                                  Q2.5_p, 
                                  Q97.5_p)
)

ds1 <- change_all %>%
  rename(
    Annual_change_pct = `Annual_change(%)`
  ) %>%
  pivot_wider(
    id_cols = c(Specimen, PathogenName, AntibioticName),
    names_from = WHORegionName,
    values_from = c(nCTA_3ydata, Annual_change_pct),
    names_sep = "_"
  )

ds2 = ds1 %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName, "-",AntibioticName),
    combined = factor(combined, levels=c(
      "BLOOD--",
      "BLOOD-Acinetobacter spp.-Carbapenems",
      "BLOOD-Escherichia coli-Third generation cephalosporins",
      "BLOOD-Escherichia coli-Carbapenems",
      "BLOOD-Klebsiella pneumoniae-Third generation cephalosporins",
      "BLOOD-Klebsiella pneumoniae-Carbapenems",
      "BLOOD-Salmonella spp.-Fluoroquinolones",
      "BLOOD-Staphylococcus aureus-Methicillin resistance",
      "BLOOD-Streptococcus pneumoniae-Penicillins",
      "STOOL--",
      "STOOL-Salmonella spp.-Fluoroquinolones",
      "STOOL-Shigella spp.-Third generation cephalosporins",
      "URINE--",
      "URINE-Escherichia coli-Third generation cephalosporins",
      "URINE-Escherichia coli-Fluoroquinolones",
      "URINE-Escherichia coli-Sulfonamides and trimethoprim", 
      "URINE-Klebsiella pneumoniae-Third generation cephalosporins",
      "URINE-Klebsiella pneumoniae-Fluoroquinolones",
      "URINE-Klebsiella pneumoniae-Sulfonamides and trimethoprim",
      "UROGENITAL--",
      "UROGENITAL-Neisseria gonorrhoeae-Macrolides",
      "UROGENITAL-Neisseria gonorrhoeae-Third generation cephalosporins"
    ))
  ) 


# ABSOLUTE PREDICTIONS
specimen_pred = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), sheet=3)
path_pred = read_excel(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends_pathogen.xlsx"), sheet=3)

specimen_pred = specimen_pred %>%
  mutate(
  #text = paste0("% Change =", sprintf("%.2f", round(mean_slope_p, 1)), "\n[95%CrI:", sprintf("%.2f", round(Q2.5_p, 1)), ", ", sprintf("%.2f", round(Q97.5_p, 1)), "]")
  Specimen=specimen,
  Specimen = ifelse(Specimen=="BSI","Bloodstream",
  ifelse(Specimen=="UTI", "Urinary tract",
         ifelse(Specimen == "GI", "Gastrointestinal", "Gonorrhoea"))),
  region_labels= factor(WHORegionName, labels = c(
    "Global" = "Global",
    "African Region" = "African\nRegion",
    "Region of the Americas" = "Region of the \nAmericas",
    "South-East Asia Region" = "South-East\nAsia Region",
    "European Region" = "European\nRegion",
    "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
    "Western Pacific Region" = "Western\nPacific Region")
  )
)

path_pred = path_pred %>%
  mutate(WHORegionName = factor(WHORegionName, 
                                levels = c("Global", 
                                           "African Region",
                                           "Region of the Americas",
                                           "South-East Asia Region",
                                           "European Region",
                                           "Eastern Mediterranean Region",
                                           "Western Pacific Region")),
         Specimen = ifelse(Specimen=="BLOOD","Bloodstream",
                           ifelse(Specimen=="URINE", "Urinary tract",
                                  ifelse(Specimen == "STOOL", "Gastrointestinal", "Gonorrhoea"))),
         region_labels = factor(WHORegionName, labels = c(
           "Global" = "Global",
           "African Region" = "African\nRegion",
           "Region of the Americas" = "Region of the \nAmericas",
           "South-East Asia Region" = "South-East\nAsia Region",
           "European Region" = "European\nRegion",
           "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
           "Western Pacific Region" = "Western\nPacific Region")
         )
  ) 

# BIND TWO TOGETHER FOR ANNEX 5
specimen_all = specimen_pred %>%
  mutate(
    PathogenName = "",
    AntibioticName = "",
    combined = ""
  ) %>% dplyr::select(-c(specimen)) %>%
  dplyr::select(WHORegionName,Year, Specimen,PathogenName, AntibioticName, combined, mean_weighted_prediction, median_weighted_prediction, Q2.5_weighted_prediction, Q97.5_weighted_prediction, region_labels)

pathogen_all = path_pred %>%
  dplyr::select(WHORegionName,Year, Specimen,PathogenName, AntibioticName, combined, mean_weighted_prediction, median_weighted_prediction, Q2.5_weighted_prediction, Q97.5_weighted_prediction, region_labels)

pred_all = rbind(specimen_all, pathogen_all)
  
da1 = pred_all %>% filter(Year%in%c(2016,2023))
da1$combined = paste0(da1$Specimen, "-", da1$PathogenName,"-", da1$AntibioticName)

da2 <- da1 %>%
  pivot_wider(
    id_cols = c(WHORegionName, Specimen, PathogenName, AntibioticName), # Columns to keep as identifiers
    names_from = Year,                                  # Column to pivot wider (Year)
    values_from = c(mean_weighted_prediction, median_weighted_prediction, Q2.5_weighted_prediction, Q97.5_weighted_prediction), # Columns to spread
    names_sep = "_"
    )

da3 = da2 %>%
  mutate(
    #'2016' = paste0(round(median_weighted_prediction_2016,1),"\n(",round(Q2.5_weighted_prediction_2016,1),"-",round(Q97.5_weighted_prediction_2016,1),")"),
    '2023' = paste0(round(median_weighted_prediction_2023,1),"\n(",round(Q2.5_weighted_prediction_2023,1),"-",round(Q97.5_weighted_prediction_2023,1),")"),
  ) %>%
  dplyr::select(WHORegionName, Specimen, PathogenName, AntibioticName, '2023', '2023_mean')

da4 <- da3 %>%
  pivot_wider(
    id_cols = c(Specimen, PathogenName, AntibioticName),         # Columns to keep as identifiers
    names_from = WHORegionName,                                   # New columns from Region_Year
    values_from = c('2023'),
    names_sep = "_"# Values to fill the cells
  )

da5 = da4 %>%
  mutate(
    specimen = ifelse(Specimen == "Bloodstream", "BLOOD",
                      ifelse(Specimen=="Urinary tract", "URINE",
                             ifelse(Specimen=="Gastrointestinal", "STOOL", "UROGENITAL"))),
    combined = paste0(specimen,"-", PathogenName, "-",AntibioticName),
    combined = factor(combined, levels=c(
      "BLOOD--",
      "BLOOD-Acinetobacter spp.-Carbapenems",
      "BLOOD-Escherichia coli-Third generation cephalosporins",
      "BLOOD-Escherichia coli-Carbapenems",
      "BLOOD-Klebsiella pneumoniae-Third generation cephalosporins",
      "BLOOD-Klebsiella pneumoniae-Carbapenems",
      "BLOOD-Salmonella spp.-Fluoroquinolones",
      "BLOOD-Staphylococcus aureus-Methicillin resistance",
      "BLOOD-Streptococcus pneumoniae-Penicillins",
      "STOOL--",
      "STOOL-Salmonella spp.-Fluoroquinolones",
      "STOOL-Shigella spp.-Third generation cephalosporins",
      "URINE--",
      "URINE-Escherichia coli-Third generation cephalosporins",
      "URINE-Escherichia coli-Fluoroquinolones",
      "URINE-Escherichia coli-Sulfonamides and trimethoprim", 
      "URINE-Klebsiella pneumoniae-Third generation cephalosporins",
      "URINE-Klebsiella pneumoniae-Fluoroquinolones",
      "URINE-Klebsiella pneumoniae-Sulfonamides and trimethoprim",
      "UROGENITAL--",
      "UROGENITAL-Neisseria gonorrhoeae-Macrolides",
      "UROGENITAL-Neisseria gonorrhoeae-Third generation cephalosporins"
    ))
  ) 

da6 = left_join(da5, ds2, by=c("combined"))
da6 = da6 %>%
  select(-c("Specimen.y", "PathogenName.y","AntibioticName.y")) %>%
  rename(
    Specimen = "Specimen.x",
    PathogenName = "PathogenName.x",
    AntibioticName = "AntibioticName.x",
  )

desired_order <- levels(factor(da5$combined)) # Adjust this with your actual factor levels

da6 = da6 %>%
  arrange(factor(combined, levels = desired_order)) %>%
  dplyr::select(c(combined, Specimen, PathogenName, AntibioticName,
           'Global', 'nCTA_3ydata_Global', 'Annual_change_pct_Global',
           'African Region','nCTA_3ydata_African Region', 'Annual_change_pct_African Region',
           'Region of the Americas','nCTA_3ydata_Region of the Americas', 'Annual_change_pct_Region of the Americas',
           'South-East Asia Region','nCTA_3ydata_South-East Asia Region', 'Annual_change_pct_South-East Asia Region',
           'European Region','nCTA_3ydata_European Region', 'Annual_change_pct_European Region',
           'Eastern Mediterranean Region','nCTA_3ydata_Eastern Mediterranean Region', 'Annual_change_pct_Eastern Mediterranean Region',
           'Western Pacific Region','nCTA_3ydata_Western Pacific Region', 'Annual_change_pct_Western Pacific Region'))

region_names <- c("Global", "African Region", "Region of the Americas",
                  "South-East Asia Region", "European Region",
                  "Eastern Mediterranean Region", "Western Pacific Region")

# Combine estimate and change for each region
for (region in region_names) {
  estimate_col <- region
  change_col   <- paste0("Annual_change_pct_", region)
  new_col      <- paste0("EstimatePlusChange_", region)
  
  da6[[new_col]] <- paste0(da6[[estimate_col]], "\n", da6[[change_col]])
}

region_names <- c("Global", "African Region", "Region of the Americas",
                  "South-East Asia Region", "European Region",
                  "Eastern Mediterranean Region", "Western Pacific Region")

# Build long table with two rows per region per combination
long_table <- lapply(region_names, function(region) {
  est_col   <- region
  change_col <- paste0("Annual_change_pct_", region)
  ncta_col  <- paste0("nCTA_3ydata_", region)
  
  da6 %>%
    transmute(
      combined, Specimen, PathogenName, AntibioticName,
      WHORegion = region,
      
      # First row: Latest year estimate
      label = "2023",
      value = !!sym(est_col),
      CTAs = !!sym(ncta_col)
    ) %>%
    bind_rows(
      da6 %>%
        transmute(
          combined, Specimen, PathogenName, AntibioticName,
          WHORegion = region,
          label = "Annual change (%)",
          value = !!sym(change_col),
          CTAs = !!sym(ncta_col)
        )
    )
}) %>% bind_rows()

table_wide <- long_table %>%
  pivot_wider(
    id_cols = c(combined, Specimen, PathogenName, AntibioticName, label),
    names_from = WHORegion,
    values_from = c(value, CTAs)  # Prevents ambiguity like "value_Global"
  ) %>%
  arrange(combined, label)

final_table = table_wide %>%
  select(c(Specimen, PathogenName, AntibioticName, label, value_Global,CTAs_Global,
           `value_African Region`, `CTAs_African Region`, `value_Region of the Americas`, `CTAs_Region of the Americas`,
           `value_South-East Asia Region`, `CTAs_South-East Asia Region`,`value_European Region`,`CTAs_European Region`,
           `value_Eastern Mediterranean Region`,`CTAs_Eastern Mediterranean Region`,`value_Western Pacific Region`,`CTAs_Western Pacific Region`))

annex5 = final_table


write.xlsx(annex5, file = paste0(dirOutputReport, "/Annexes/Final/Annex5_predicted_trend_BCIpermillion.xlsx"))



# VISUALISE PREDICTED TREND AGAINST DATA
m3 = m3 %>%
  mutate(
    WHORegionName = factor(WHORegionName,
                           levels = c("Global",
                                      "Western Pacific Region",
                                      "Eastern Mediterranean Region",
                                      "European Region",
                                      "South-East Asia Region",
                                      "Region of the Americas",
                                      "African Region"
                           )),
    region_labels = factor(WHORegionName, labels = c(
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region")
    )
  )

m3_g = m3 %>%
  mutate(
    region_labels ="Global",
    region_labels = factor(region_labels, levels=c("Global"))
  )

m3_filtered <- m3 %>%
  group_by(Iso3) %>%  # Group by country or unique identifier
  filter(n_distinct(Year[!is.na(SpecimenIsolateswithAST)]) >= 4) %>%  # Filter for at least 4 years of data
  ungroup()


m3_b = m3 %>% filter(Specimen=="BLOOD")

# ggplot(m3_b, aes(x=Year, y=BCI_permillion, size=Iso3))+
#   geom_point()+
#   facet_wrap(~WHORegionName)
# 
#abs_all = NULL
#year_c = round(mean(unique(d6$Year)),0)

# ESTIMATE AVERAGE ABSOLUTE YEARLY CHANGE (EVENTUALLY NOT USED)
# for(drug_bug in unique(d6$combined)){
#   d = generate_absolute_change_summary(model_results[[drug_bug]]$model1,data=d6, year_range = c(2016:2022), pdata, cdata, specimen=drug_bug,specimen_filter = NULL, pathogen=T) # need to change year_centre to the mean of year? 
#   abs_all = rbind(abs_all,d)
# }
# 
# abs_all = abs_all %>% 
#   mutate(WHORegionName = factor(WHORegionName, 
#                                 levels = c("Global",
#                                            "Western Pacific Region",
#                                            "Eastern Mediterranean Region",
#                                            "European Region",
#                                            "South-East Asia Region",
#                                            "Region of the Americas",
#                                            "African Region"
#                                 )),
#          combined = specimen
#   ) %>%
#   separate(specimen, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")


# SLOPES FOR PATHOGEN SPECIFIC DATA
e1 = slopes_all
e2 = e1 %>% dplyr::select(WHORegionName, Specimen, PathogenName, AntibioticName,
                                               median_slope_p, Q2.5_p, Q97.5_p, combined, Significant)

s1 = slopes_allS %>% dplyr::select(WHORegionName, Specimen,
                                   median_slope_p, Q2.5_p, Q97.5_p, Significant)

s1 = s1 %>% mutate(
  PathogenName = "",
  AntibioticName = "",
  combined = Specimen)
  
s1 = s1 %>% mutate(
  PathogenName = case_when(
    combined == "BLOOD" ~ "Bloodstream",
    combined == "URINE" ~ "Urinary tract",
    combined == "STOOL" ~ "Gastrointestinal tract",
    combined == "UROGENITAL" ~ "Gonorrhoea"
  ),
  AntibioticName = case_when(
    combined == "BLOOD" ~ "Bloodstream",
    combined == "URINE" ~ "Urinary tract",
    combined == "STOOL" ~ "Gastrointestinal tract",
    combined == "UROGENITAL" ~ "Gonorrhoea",
    
  )
)

s1$combined = ""

s2 = s1 %>% dplyr::select(WHORegionName, Specimen, PathogenName, AntibioticName,
                                  median_slope_p, Q2.5_p, Q97.5_p, combined, Significant)


e3 = rbind(e2,s2)  

e4 <- e3 %>%
  arrange(Specimen, AntibioticName, PathogenName) %>%
  group_by(Specimen, AntibioticName) %>%
  mutate(
    AntibioticName2 = case_when(
      AntibioticName == "Carbapenems" ~ 'Carbapenems',
      AntibioticName == "Third generation cephalosporins" ~ '3rd-gen. cephalosporins',
      AntibioticName == 'Fluoroquinolones' ~ 'Fluoroquinolones',
      AntibioticName == "Methicillin resistance" ~ "Methicillin resistance",
      AntibioticName == "Sulfonamides and trimethoprim" ~ 'TMP-sulfa',
      AntibioticName == "Penicillins" ~ "Penicillins",
      AntibioticName == "Macrolides" ~ 'Macrolides'
    ),
    PathogenName2 = factor(
      PathogenName,
      levels = c("Bloodstream","Urinary tract","Gastrointestinal","Gonorrhoea", "Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                 "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae",
                 "Shigella spp.","Neisseria gonorrhoeae"),
      labels = c("Bloodstream","Urinary tract","Gastrointestinal","Gonorrhoea","Acinetobacter spp.", "E. coli", "K. pneumoniae",
                 "Salmonella spp.", "S. aureus", "S. pneumoniae","Shigella spp.","N. gonorrhoeae")
    ),
    custom_label = ifelse(
      grepl("spp\\.", PathogenName2),
      paste0("<i>", sub(" spp\\.", "", PathogenName2), "</i> spp. - ", AntibioticName2),
      paste0("<i>", PathogenName2, "</i> - ", AntibioticName2)
    ),
    # custom_label = case_when(
    #   PathogenName == "Bloodstream" ~ "Bloodstream",
    #   PathogenName == "Urinary tract" ~ "Urinary tract",
    #   PathogenName == "Gastrointestinal tract" ~ "Gastrointestinal tract",
    #   PathogenName == "Gonorrhoea" ~ "Gonorrhoea",
    #   grepl("spp\\.", PathogenName2) ~ paste0("<i>", sub(" spp\\.", "", PathogenName2), "</i> spp. - ", AntibioticName2),
    #   TRUE ~ paste0("<i>", PathogenName2, "</i> - ", AntibioticName2)
    # ),
    custom_label = factor(custom_label,
                          levels = c("Bloodstream",
                                     "Gastrointestinal",
                                     "Urinary tract", 
                                     "Gonorrhoea",
                                     "<i>N. gonorrhoeae</i> - Macrolides",
                                     "<i>N. gonorrhoeae</i> - 3rd-gen. cephalosporins",
                                     "<i>Shigella</i> spp. - 3rd-gen. cephalosporins",
                                     "<i>S. pneumoniae</i> - Penicillins",
                                     "<i>S. aureus</i> - Methicillin resistance",
                                     "<i>Salmonella</i> spp. - Fluoroquinolones",
                                     "<i>K. pneumoniae</i> - TMP-sulfa",
                                     "<i>K. pneumoniae</i> - Fluoroquinolones",
                                     "<i>K. pneumoniae</i> - Carbapenems", 
                                     "<i>K. pneumoniae</i> - 3rd-gen. cephalosporins",
                                     "<i>E. coli</i> - TMP-sulfa",
                                     "<i>E. coli</i> - Fluoroquinolones",
                                     "<i>E. coli</i> - Carbapenems",
                                     "<i>E. coli</i> - 3rd-gen. cephalosporins",
                                     "<i>Acinetobacter</i> spp. - Carbapenems"
                          )),
    Specimen_label = case_when(
      Specimen == "BLOOD" ~ "Bloodstream",
      Specimen == "URINE" ~ "Urinary tract",
      Specimen == "STOOL" ~ "Gastrointestinal",
      Specimen == "UROGENITAL" ~ "Gonorrhoea",
      TRUE ~ NA_character_
    ),
    Specimen_label = factor(Specimen_label, levels = c("Bloodstream",
                                                       "Gastrointestinal", "Urinary tract",  "Gonorrhoea")),
    region_labels = factor(WHORegionName, 
                           levels = c("Global",
                                      "African Region",
                                      "Region of the Americas",
                                      "South-East Asia Region",
                                      "European Region",
                                      "Eastern Mediterranean Region",
                                      "Western Pacific Region"
                           ),
                           labels = c(
      "Global" = "Global",
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region"
      )
    ),
    text_with_star = ifelse(
      Significant == "Yes",
      paste0(
        comma(round(median_slope_p, 1)), "<br>(",
        comma(round(Q2.5_p, 1)), ", ",
        comma(round(Q97.5_p, 1)), ") <b>*</b>"
      ),
      paste0(
        comma(round(median_slope_p, 1)), "<br>(",
        comma(round(Q2.5_p, 1)), ", ",
        comma(round(Q97.5_p, 1)), ")"
      )
    )
  ) %>%
  ungroup()


# Visualise slope change (Pathogen)
p1 = ggplot(e4, aes(x = region_labels, y = custom_label)) +
  geom_tile(aes(fill = median_slope_p), linewidth = 0.8) +
  ggtext::geom_richtext(
    aes(label = text_with_star), 
    size = 3.5, 
    color = "black",
    fill = NA, 
    label.color = NA
  ) +
  scale_fill_gradient2(
    low = "coral", 
    mid = "white", 
    high = "seagreen", 
    midpoint = 0, 
    name = "Percentage (%) Change",
    na.value = "grey90"
  ) +
  #scale_color_manual(values = c("white", "black")) +
  labs(
    x = "",
    y = NULL,
    title = ""
  ) +
  scale_y_discrete(position = "right")+
  scale_x_discrete(position = "top") +
  ggh4x::facet_grid2(Specimen_label ~ ., scales = "free_y", space = "free_y", switch = "both",
                     labeller = labeller(Specimen_label=c("Bloodstream" = "Bloodstream",
                                                          "Gastrointestinal" = "Gastrointestinal",
                                                          "Urinary" = "Urinary tract",
                                                          "Gonorrhoea" = "Gonorrhoea"))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.y.left = element_text(
      angle = 90, 
      size = 12,  # Adjust font size
      margin = margin(r = 5)  # Reduce right margin to move strip closer
    ),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "lines"),  # Reduce space between panels
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) 


p1

slopes_allS$Specimen = ifelse(slopes_allS$specimen == "Bloodstream", "BSI",
                              ifelse(slopes_allS$specimen == "Urinary tract", "UTI",
                                     ifelse(slopes_allS$specimen == "Gastrointestinal", "GI", "Gonorrhoea")))

specimen_pred$Specimen = specimen_pred$specimen

sp1 = left_join(specimen_pred, slopes_allS%>%dplyr::select(c(region_labels, Specimen,Significant)), by=c("region_labels", "Specimen"))

sp1 = sp1 %>% 
  mutate(
    WHORegionName = factor(WHORegionName,
                             levels = c("Global",
                                        "African Region",
                                        "Region of the Americas",
                                        "South-East Asia Region",
                                        "European Region",
                                        "Eastern Mediterranean Region",
                                        "Western Pacific Region")),
    region_labels = factor(WHORegionName, labels = c(
  "Global" = "Global",
  "African Region" = "African\nRegion",
  "Region of the Americas" = "Region of the \nAmericas",
  "South-East Asia Region" = "South-East\nAsia Region",
  "European Region" = "European\nRegion",
  "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
  "Western Pacific Region" = "Western\nPacific Region"))
  )

# Visualize Regional Trends (Specimen)
p2 = ggplot(sp1 %>% filter(Specimen %in% c("BSI", "UTI")), aes(x = Year, y = median_weighted_prediction)) +
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction),
    alpha = 0.2, fill = "#377EB8"
  ) +
  geom_line(size = 1, colour = "#377EB8") +  # Line for median predictions
  # geom_jitter(
  #   data = m3 %>% filter(Specimen %in% c("BLOOD", "URINE")),  # Add jittered points
  #   aes(x = Year, y = BCI_permillion, size = BCI_permillion),
  #   width = 0.2, height = 0.2, alpha = 0.1
  # ) +
  geom_text(
    aes(x = 2016, y = 0, label = ifelse(Significant =="Yes", "*", "")), 
    color = "red", size = 8
  ) +
  ggh4x::facet_grid2(Specimen ~ region_labels, scales = "free_y", independent = "y",
                     switch = "y",
                     labeller = labeller(Specimen = c("BSI" = "Bloodstream", "UTI" = "Urinary tract"))) +
  labs(
    title = "",
    y = "BCIs with AST per million population",
    x = "",
    size = "BCI per Million"
  ) +
  scale_y_continuous(
    limits = c(0, NA),  # Start y-axis at 0
    breaks = scales::extended_breaks(),  # Use extended breaks for better labeling
    labels = function(x) {
      ifelse(x >= 1000, paste0(x / 1000, "k"), x)  # Format numbers conditionally
    },
    expand = expansion(mult = c(0, 0.1))  # No padding below 0, only above max
  )+
  theme(
    strip.placement = "outside",  # Place facet titles outside the plot
    strip.text.y.left = element_text(angle = 0, hjust = 0.5, size = 16, face = "bold"),  # Increase row label size
    strip.text.x = element_blank(),  # Remove column label text
    panel.spacing = unit(1, "lines"),  # Add space between panels
    axis.text.y.right = element_text(hjust = 0.5),  # Center y-axis text on the right
    axis.title.y.right = element_text(),  # Add title to the right-hand side
    axis.ticks.y.right = element_line()  # Add ticks on the right y-axis
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none")  # Enable size legend
p2


combined_plot <- p2 / p1 +
plot_layout(heights = c(1, 3))
combined_plot

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/For_writing/Figure_3.4_trend_regional_pathogen.svg"), 
       plot = combined_plot,
       bg = "white",
       device = "svg",
       dpi = 300,
       width = 15, height = 19)  

# For writing
#write.csv(slopes_all, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_slopechange_regional_pathogen.csv"))
#write.csv(slopes_allS, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_slopechange_regional_specimen.csv"))
#write.csv(specimen_pred, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_predicted_trend_regional_specimen.csv"))
#write.csv(annex4, file = paste0(dirOutputReport, "/Annexes/Provisional/Annex4_predicted_trend_BCIpermillion.csv"))





# 4	GLOBAL AND REGIONAL RESISTANCE TO ANTIBIOTICS
###################################################################

# Generate a single global estimate of resistance by using weights to account for 
# testing coverage and potential bias in the data from each setting
# Hence also allow for comparison between regions and potentially over time.

# 4.1	Resistance to antibiotics under surveillance in 2023	 
###################################################################

#r = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/REGION_w_prev_allALLYEARSnopredict.csv"))
#priorset = "all_one_model_weakip_set1_centered"
#sensitivity = "fixed_allyears_nopredict"

# 
#r = read.csv(file.path("C:/Users/esthe/OneDrive - VanKleefBV/Documenten/Documenten/WHO/WHO_GLASS/2025 ANALYSIS EV/2025 Model_output/model_output_regional_estimates/", priorset,sensitivity, "REGION_w_prev_all.csv"))
r = read.csv(file.path(dirDataModeloutput, "Model_prevalence_estimates/Data_tables_prevalence/REGION_w_prev_all.csv"))

drug_bug = r$drug_bug
r$combined = gsub("Co trimoxazole","Co-trimoxazole", r$drug_bug)
r$combined = gsub("Methicillin resistance","Methicillin-resistance", r$combined)
r$combined = gsub("Third generation cephalosporins","Third-generation cephalosporins", r$combined)

#r$AntibioticName[r$AntibioticName=="Co"] = "Co-trimoxazole"
#r$AntibioticName[r$AntibioticName=="Third generation cephalosporins"] = "Third-generation cephalosporins"
r1 = r %>%
  mutate(
    drug_bug = combined
  )

r1 = r1 %>% filter(combined!="BLOOD-Escherichia coli-Ampicillin")

a = adataAC %>% select(AntibioticName, Antibiotic, Pathogen, PathogenName) %>%
  distinct() %>% 
  filter(!(AntibioticName == "Third-generation cephalosporins" & Antibiotic !="J01DD")&
           !(AntibioticName == "Co-trimoxazole" & Antibiotic =="J01EE")&
           !(AntibioticName == "Methicillin resistance" & Antibiotic !="J01DC"))

r1 = left_join(r,a)

r1$drug_bug = gsub("Co trimoxazole","Co-trimoxazole", r1$drug_bug)
r1$drug_bug = gsub("Methicillin resistance","Methicillin-resistance", r1$drug_bug)
r1$drug_bug = gsub("Third generation cephalosporins","Third-generation cephalosporins", r1$drug_bug)


#r1$AntibioticName[r1$Antibiotic=="COL"] = "Colistin"
r1$AntibioticName = gsub("Co trimoxazole","Co-trimoxazole", r1$AntibioticName)

r1$AntibioticName = gsub("Methicillin resistance","Methicillin-resistance", r1$AntibioticName)
r1$AntibioticName = gsub("Third generation cephalosporins","Third-generation cephalosporins", r1$AntibioticName)

r2 = r1 %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") ~ '3rd-gen. cephalosporins',
                              (Antibiotic =="FEP") ~ '4th-gen. cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") ~ 'Macrolides',
                              #(Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX"| AntibioticName == "Methicillin-resistance") ~ NA,
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT" | AntibioticName == "Co-trimoxazole" ) ~ 'TMP-sulfa',
                              (Antibiotic =="AMK" | Antibiotic =="GEN"|Antibiotic=="SPT") ~ 'Aminoglycocides',
                              (Antibiotic == "COL") ~ 'Polymyxins',
                              (Antibiotic == "MNO" | Antibiotic == "TGC") ~ 'Tetracyclines',
                              (AntibioticName=="Third-generation cephalosporins"| AntibioticName == "Methicillin-resistance") ~ "SDG"
  ),
  Grouping = factor(Grouping, levels=c("Aminoglycocides", "Carbapenems", "3rd-gen. cephalosporins", "4th-gen. cephalosporins",
                                       "Fluoroquinolones", "Macrolides","Penicillins", "Polymyxins", 
                                       "TMP-sulfa", "Tetracyclines","SDG")),
  AntibioticName = factor(AntibioticName, levels= c("Gentamicin","Amikacin","Spectinomycin","Ertapenem",
                                                    "Imipenem", "Meropenem", "Doripenem", "Ceftriaxone","Cefotaxime","Ceftazidime",
                                                    "Cefixime",  "Cefepime", "Ciprofloxacin","Levofloxacin", "Azithromycin",
                                                    "Oxacillin",   "Penicillin G","Ampicillin", "Colistin",
                                                    "Co-trimoxazole",  "Minocycline" , "Tigecycline", "Methicillin-resistance", "Third-generation cephalosporins")) 
  )
unique(r2$AntibioticName[is.na(r2$Grouping)])

# 

table(r2$AntibioticName,r2$Grouping)
table(r2$AntibioticName, useNA="always")
table(r2$AntibioticName, useNA="always")

#View(r2 %>% filter(is.na(AntibioticName)))

region = cdata %>% 
  ungroup() %>% select(WHORegionCode, WHORegionName) %>%
  distinct()

r3 = r2 %>% 
  left_join(region) %>%
  mutate(
    WHORegionName = ifelse(is.na(WHORegionName), "Global", WHORegionName)
  )


r4 = r3 %>%
  dplyr::select(WHORegionName, Year, regional_weighted_mean, Q2.5, Q97.5, Specimen,
         PathogenName, AntibioticName, Grouping, drug_bug, n_countries, n_isolates) 

r5 = r4 %>%
  mutate(Region = factor(WHORegionName)
         )%>%
           mutate(
             Region= factor(Region,levels = c(
                            "Global",
                            "African Region",
                            "Region of the Americas",
                            "South-East Asia Region",
                            "European Region",
                            "Eastern Mediterranean Region", 
                            "Western Pacific Region"
                          ))
  )

r5 = r5 %>%
  mutate(
  n_cutoff = ifelse(Region=="Global"& n_countries<10, "No",
                    ifelse(Region!="Global" & n_countries<5, "No", "Yes"))
  )

r6 = r5%>%filter(Year==2023)

unique(r6$drug_bug)
r6 = r6 %>% filter(drug_bug!="URINE-Escherichia coli-Ampicillin") # No longer reported from 2022 onwards

d = r6 %>% 
  group_by(WHORegionName, drug_bug) %>%
  distinct()

unique(d$Region)

# ANNEX 6: Regional estimates of all drug bug combinations
#---------------------------------------------------------------------------------------------------------

df <- d %>%
  mutate(
    stat_label = sprintf(
      "%.1f (%.1f - %.1f)\n%d (%d)",
      #prettyNum(n_isolates, big.mark = " ", scientific = FALSE),
      #format(n_countries, big.mark = "\u202f", scientific = FALSE),
      #format(n_isolates, big.mark = "\u202f", scientific = FALSE),
      regional_weighted_mean, Q2.5, Q97.5, n_countries, n_isolates
    )
  )

unique(df$Grouping)

# Region to columns
df1 <- df %>%
  ungroup()%>%
  arrange(Specimen, PathogenName, Grouping, AntibioticName) %>%
  select(Specimen, PathogenName, drug_bug,Grouping, AntibioticName, Region, stat_label) %>%
  pivot_wider(
    names_from = Region,
    values_from = stat_label
  ) %>%
  select(-c(drug_bug))

df2 <- df1 %>%
  group_by(Specimen, PathogenName) %>%
  mutate(
    Grouping = ifelse(duplicated(Grouping), "", as.character(Grouping)),
    AntibioticName = ifelse(duplicated(AntibioticName), "", as.character(AntibioticName)),
    PathogenName = ifelse(duplicated(PathogenName), "", as.character(PathogenName))
  ) %>%
  ungroup() %>%
  dplyr::select(c(Specimen, PathogenName, Grouping, AntibioticName, Global, `African Region`,`Region of the Americas`,
           `South-East Asia Region`,`European Region`,`Eastern Mediterranean Region`, `Western Pacific Region`))

# Function to export table
export_prevalence_table_to_excel <- function(summary_table, filename = NULL) {
  wb <- createWorkbook()
  addWorksheet(wb, "")
  
  # Write data
  writeData(wb, "", summary_table, startCol = 1, startRow = 1)
  
  # Auto-wrap text and adjust row height for better readability
  n_rows <- nrow(summary_table) + 1  # +1 for header row
  n_cols <- ncol(summary_table)
  
  for (col in 1:n_cols) {
    setColWidths(wb, sheet = "", cols = col, widths = "auto")
  }
  
  # Wrap text and increase row height
  addStyle(
    wb, sheet = "",
    style = createStyle(wrapText = TRUE, valign = "top"),
    rows = 2:n_rows,
    cols = 1:n_cols,
    gridExpand = TRUE
  )
  
  # Save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("Saved Excel file to:", filename, "\n")
}

export_prevalence_table_to_excel(df2, paste0(dirOutputReport, "/Annexes/Provisional/Annex6_regionalprevalence2023.xlsx"))

fig4.1 = r6 %>%filter(!Grouping=="SDG")

# A. bacter + E. coli + Klebs 
p4.1 <- plot_pathogen_antibiotic(
  df = fig4.1,
  pathogens = c("Acinetobacter spp.", "Escherichia coli", "Klebsiella pneumoniae",
                "Salmonella spp.", "Streptococcus pneumoniae"),
  specimen = "BLOOD",
  year = 2023,
  custom_labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                    "Salmonella spp.", "S. pneumoniae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
p4.1

p4.3a <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "URINE",
  pathogens = c("Escherichia coli", "Klebsiella pneumoniae"),
  year=2023,
  custom_labels = c("E. coli", "K. pneumoniae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
#p4.3a

p4.3b <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "STOOL",
  pathogens = c("Salmonella spp.", "Shigella spp."),
  year=2023,
  custom_labels = c("Salmonella spp.", "Shigella spp."),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
p4.3b

p4.3c <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "UROGENITAL",
  pathogens = c("Neisseria gonorrhoeae"),
  year=2023,
  custom_labels = c("N. gonorrhoeae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
p4.3c

# FOR REPORT 
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.1_bsi.svg"), 
       plot = p4.1, 
       device = "svg",
       width = 30, height = 25, limitsize=F)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.3a_uti.svg"), 
       plot = p4.3a, 
       device="svg",
       width = 30, height = 13)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.3b_gi.svg"), 
       plot = p4.3b, 
       device="svg",
       width = 30, height = 9)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.3c_gon.svg"), 
       plot = p4.3c, 
       width = 30, height = 5)

write.csv(fig4.1, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.1_4.2_4.3_REGIONALprevalence2023.csv"))

# 4.1.1f SDG
fig4.2 = r5%>%filter(drug_bug=="BLOOD-Escherichia coli-Third-generation cephalosporins"&Year==2023)

p4.2a <- ggplot() +
  geom_col(data = fig4.2, 
           aes(x = reorder(WHORegionName, regional_weighted_mean), y = regional_weighted_mean, fill = WHORegionName), 
           alpha = 0.5) +
  geom_errorbar(data = fig4.2, 
                aes(x = reorder(WHORegionName, regional_weighted_mean), ymin = Q2.5, ymax = Q97.5), 
                width = 0.3) +
  #geom_text(data = fig4.1f, 
  #          aes(x = reorder(WHORegionName, median), y = median * 100 + 5, 
  #              label = paste0("n = ", n)), 
  #          size = 6, hjust = -0.7) +
  theme_minimal() +
  scale_fill_manual(name = " ", values = facet_colors3) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme(
    plot.background = element_rect(
    fill = "white",
    colour = "white"),
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    plot.subtitle = element_markdown(size = 20)
  ) +
  coord_flip() +
  labs(
    title = "", 
    subtitle = "&nbsp; <i> E. coli</i> - 3<sup>rd</sup> gen cephalosporins resistance",
    y = "Percentage (%)",
    x = ""
  )
p4.2a

fig4.2b = r5%>%filter(drug_bug=="BLOOD-Staphylococcus aureus-Methicillin-resistance"&Year==2023)

p4.2b <- ggplot() +
  geom_col(data = fig4.2b, 
           aes(x = reorder(WHORegionName, regional_weighted_mean), y = regional_weighted_mean, fill = WHORegionName), 
           alpha = 0.5) +
  geom_errorbar(data = fig4.2b, 
                aes(x = reorder(WHORegionName, regional_weighted_mean), ymin = Q2.5, ymax = Q97.5), 
                width = 0.3) +
  #geom_text(data = fig4.1f, 
  #          aes(x = reorder(WHORegionName, median), y = median * 100 + 5, 
  #              label = paste0("n = ", n)), 
  #          size = 6, hjust = -0.7) +
  theme_minimal() +
  scale_fill_manual(name = " ", values = facet_colors3) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = "white"),
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    title = element_text(size = 20),
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    plot.subtitle = element_markdown(size = 20)
  ) +
  coord_flip() +
  labs(
    title = "", 
    subtitle = "&nbsp; <i> S. aureus</i> - Methicillin resistance",
    y = "Percentage (%)",
    x = ""
  )
p4.2b

combined_plot = p4.2a+p4.2b+plot_layout(ncol=2)

# REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.2_SDG.svg"), 
       plot = combined_plot, 
       device = "svg",
       width = 20, height = 5, limitsize=F)

write.csv(rbind(fig4.2,fig4.2b), file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.2a_REGIONALprevalence2023_SDG.csv"))

# 4.1.2 Country map
#--------------------------------------------------------------------

d = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))

d$AntibioticName[d$AntibioticName=="Co trimoxazole"] = "Co-trimoxazole"
d$AntibioticName[d$AntibioticName=="Third generation cephalosporins"] = "Third-generation cephalosporins"
d$AntibioticName = gsub("Methicillin resistance","Methicillin-resistance", d$AntibioticName)

d1 = d

a = adataAC %>% select(AntibioticName, Antibiotic, Pathogen, PathogenName) %>%
  distinct()

d2 = left_join(d1,a, multiple="first")
unique(d2$AntibioticName)

d3 = d2 %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") ~ '3rd-gen. cephalosporins',
                              (Antibiotic =="FEP") ~ '4th-gen. cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") ~ 'Macrolides',
                              #(Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX"| AntibioticName == "Methicillin-resistance") ~ NA,
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT" | AntibioticName == "Co-trimoxazole" ) ~ 'TMP-sulfa',
                              (Antibiotic =="AMK" | Antibiotic =="GEN"|Antibiotic=="SPT") ~ 'Aminoglycocides',
                              (Antibiotic == "COL") ~ 'Polymyxins',
                              (Antibiotic == "MNO" | Antibiotic == "TGC") ~ 'Tetracyclines',
                              (AntibioticName=="Third-generation cephalosporins"| AntibioticName == "Methicillin-resistance") ~ "SDG"
  ),
  Grouping = factor(Grouping, levels=c("Aminoglycocides", "Carbapenems", "3rd-gen. cephalosporins", "4th-gen. cephalosporins",
                                       "Fluoroquinolones", "Macrolides","Penicillins", "Polymyxins", 
                                       "TMP-sulfa", "Tetracyclines","SDG")),
  AntibioticName = factor(AntibioticName, levels= c("Gentamicin","Amikacin","Spectinomycin","Ertapenem",
                                                    "Imipenem", "Meropenem", "Doripenem", "Ceftriaxone","Cefotaxime","Ceftazidime",
                                                    "Cefixime",  "Cefepime", "Ciprofloxacin","Levofloxacin", "Azithromycin",
                                                    "Oxacillin",   "Penicillin G","Ampicillin", "Colistin",
                                                    "Co-trimoxazole",  "Minocycline" , "Tigecycline", "Methicillin-resistance", "Third-generation cephalosporins")),
    PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                    "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae",
                                                    "Shigella spp.","Neisseria gonorrhoeae"),
                           labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                      "Salmonella spp.", "S. aureus", "S. pneumoniae","Shigella spp.","N. gonorrhoeae"))
  )
unique(d3$AntibioticName)

table(d3$AntibioticName,d3$Grouping)
table(d3$AntibioticName, useNA="always")
table(d3$AntibioticName, useNA="always")


region = cdata %>% 
  ungroup() %>% select(WHORegionCode, WHORegionName) %>%
  distinct()

d4 = d3 %>% 
  left_join(region)


d5 = d4 %>%
  dplyr::select(WHORegionName, Iso3, Year, w_prev, w_prev_lower, w_prev_upper, amr_rate, Specimen,
         PathogenName,PathogenName2, AntibioticName, Grouping, combined)%>%
  mutate(
    drug_bug = combined
)

d5$drug_bug[d5$drug_bug=="BLOOD-Escherichia coli-Third generation cephalosporins"] = "BLOOD-Escherichia coli-Third-generation cephalosporins"
d5$drug_bug[d5$drug_bug=="BLOOD-Staphylococcus aureus-Methicillin resistance"] = "BLOOD-Staphylococcus aureus-Methicillin-resistance"

alpha = 0.5

# MAKE PLOTS - FOR LAYOUT COMBINED EXAMPLES
#--------------------------------------------------------------------------

fig4.2 = d5 %>% filter(drug_bug%in%c(ppbsi,SDG,pputi,ppgi,ppuro))

unique(fig4.2$drug_bug)


# MAKE TABLE FOR ANNEX 7
#-------------------------------------------------------------
a1 = fig4.2 %>% filter(Year==2023 & !is.na(amr_rate)) %>%
  select(WHORegionName, Iso3, Year, drug_bug, w_prev,w_prev_lower,w_prev_upper)# Filter out countries that did not submit data

e1 = read.csv(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.1_GLASS_enrolled_or_with_data_300325_EV.csv"))

iso3_data2023 = unique(e1$iso3[e1$var=="2023 AMR data"]) # This is 104, whereas in a1 there is 103. That is because
 # Togo did not submit a minimum of 10 isolates for any of the drug-bug combinations. We leave this one out from Annex 7

adataAC$combined = ifelse(adataAC$combined=="BLOOD-Staphylococcus aureus-Methicillin resistance",
                          "BLOOD-Staphylococcus aureus-Methicillin-resistance",adataAC$combined)

da = adataAC %>%filter(Year==2023, InterpretableAST>10)%>%select(Iso3, combined, InterpretableAST) %>%
 rename(drug_bug = 'combined') # Only those with more than 10 isolates

# Update above with including data of all years
da_all = adataAC %>%filter(Year%in%c(2018:2023), InterpretableAST>10)%>%select(Iso3, combined, InterpretableAST) %>%
  rename(drug_bug = 'combined') # Only those with more than 10 isolates

da_sum = da_all %>%
  group_by(Iso3, drug_bug) %>%
  summarise(InterpretableAST_all=sum(InterpretableAST)) 

# ALL COUNTRIES THAT REPORTED DATA ACROSS ALL YEARS
#crep = fig4.2 %>%  filter(!is.na(amr_rate)) %>%
#  select(c(WHORegionName, Iso3, drug_bug)) %>%
#  distinct()

da_both = left_join(da,da_sum)

#a1b = left_join(crep, a1) # Ensure that all countries are present, also those ones that did not report in 2023

a2 = left_join(a1,da_both, by=c("Iso3", "drug_bug"))
a3 = a2
# a3 = a2 %>%
#   filter(!is.na(InterpretableAST)) # Filter out those with <=10 isolates

#a2 = left_join(a1,da, multiple="first")

a4 <- a3 %>%
  pivot_wider(
    id_cols = c(WHORegionName, Iso3), # Columns to keep as identifiers
    names_from = drug_bug,                                  # Column to pivot wider (Year)
    values_from = c(w_prev,w_prev_lower,w_prev_upper, InterpretableAST, InterpretableAST_all) # Columns to spread
  )

a5 = a4 %>%
  mutate(
    #1
    B_Acinetobacter_spp_imipenem = paste0(round(`w_prev_BLOOD-Acinetobacter spp.-Imipenem`*100,1)," (", 
                                              round(`w_prev_lower_BLOOD-Acinetobacter spp.-Imipenem`*100,1), "-",
                                              round(`w_prev_upper_BLOOD-Acinetobacter spp.-Imipenem`*100,1),")\n",
                                          `InterpretableAST_BLOOD-Acinetobacter spp.-Imipenem`, " (",
                                          `InterpretableAST_all_BLOOD-Acinetobacter spp.-Imipenem`, ")"),
    #2
    B_E.coli_Cefotaxime = paste0(round(`w_prev_BLOOD-Escherichia coli-Cefotaxime`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Escherichia coli-Cefotaxime`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Escherichia coli-Cefotaxime`*100,1),")\n",
                                 `InterpretableAST_BLOOD-Escherichia coli-Cefotaxime`, " (",
                                 `InterpretableAST_all_BLOOD-Escherichia coli-Cefotaxime`, ")"),
    #3
    B_E.coli_Imipenem = paste0(round(`w_prev_BLOOD-Escherichia coli-Imipenem`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Escherichia coli-Imipenem`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Escherichia coli-Imipenem`*100,1),")\n",
                               `InterpretableAST_BLOOD-Escherichia coli-Imipenem`, " (",
                               `InterpretableAST_all_BLOOD-Escherichia coli-Imipenem`, ")"),
    #4
    B_E.coli_3_gen.cephalosporins = paste0(round(`w_prev_BLOOD-Escherichia coli-Third-generation cephalosporins`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Escherichia coli-Third-generation cephalosporins`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Escherichia coli-Third-generation cephalosporins`*100,1),")\n",
                                        `InterpretableAST_BLOOD-Escherichia coli-Third-generation cephalosporins`, " (",
                                        `InterpretableAST_all_BLOOD-Escherichia coli-Third-generation cephalosporins`, ")"),
    #5
    B_K.pneumoniae_Cefotaxime = paste0(round(`w_prev_BLOOD-Klebsiella pneumoniae-Cefotaxime`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Klebsiella pneumoniae-Cefotaxime`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Klebsiella pneumoniae-Cefotaxime`*100,1),")\n",
                                       `InterpretableAST_BLOOD-Klebsiella pneumoniae-Cefotaxime`, " (",
                                       `InterpretableAST_all_BLOOD-Klebsiella pneumoniae-Cefotaxime`, ")"),
    #6
    B_K.pneumoniae_Imipenem = paste0(round(`w_prev_BLOOD-Klebsiella pneumoniae-Imipenem`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Klebsiella pneumoniae-Imipenem`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Klebsiella pneumoniae-Imipenem`*100,1),")\n",
                                     `InterpretableAST_BLOOD-Klebsiella pneumoniae-Imipenem`, " (",
                                     `InterpretableAST_all_BLOOD-Klebsiella pneumoniae-Imipenem`, ")"),
    #7
    B_Salmonella_spp._Ciprofloxacin = paste0(round(`w_prev_BLOOD-Salmonella spp.-Ciprofloxacin`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Salmonella spp.-Ciprofloxacin`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Salmonella spp.-Ciprofloxacin`*100,1),")\n",
                                        `InterpretableAST_BLOOD-Salmonella spp.-Ciprofloxacin`, " (",
                                        `InterpretableAST_all_BLOOD-Salmonella spp.-Ciprofloxacin`, ")"),
    #8
    B_S.aureus_Methicillin_resistance = paste0(round(`w_prev_BLOOD-Staphylococcus aureus-Methicillin-resistance`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Staphylococcus aureus-Methicillin-resistance`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Staphylococcus aureus-Methicillin-resistance`*100,1),")\n",
                                        `InterpretableAST_BLOOD-Staphylococcus aureus-Methicillin-resistance`, " (",
                                        `InterpretableAST_all_BLOOD-Staphylococcus aureus-Methicillin-resistance`, ")"),
    #9
    B_S.pneumoniae_PenicillinG = paste0(round(`w_prev_BLOOD-Streptococcus pneumoniae-Penicillin G`*100,1)," (", 
                                        round(`w_prev_lower_BLOOD-Streptococcus pneumoniae-Penicillin G`*100,1), "-",
                                        round(`w_prev_upper_BLOOD-Streptococcus pneumoniae-Penicillin G`*100,1),")\n",
                                        `InterpretableAST_BLOOD-Streptococcus pneumoniae-Penicillin G`, " (",
                                        `InterpretableAST_all_BLOOD-Streptococcus pneumoniae-Penicillin G`, ")"),
    #10
    G_Salmonella_spp._Ciprofloxacin = paste0(round(`w_prev_STOOL-Salmonella spp.-Ciprofloxacin`*100,1)," (", 
                                        round(`w_prev_lower_STOOL-Salmonella spp.-Ciprofloxacin`*100,1), "-",
                                        round(`w_prev_upper_STOOL-Salmonella spp.-Ciprofloxacin`*100,1),")\n",
                                        `InterpretableAST_STOOL-Salmonella spp.-Ciprofloxacin`, " (",
                                        `InterpretableAST_all_STOOL-Salmonella spp.-Ciprofloxacin`, ")"),
    #11
    G_Shigella_spp._Ciprofloxacin = paste0(round(`w_prev_STOOL-Shigella spp.-Ciprofloxacin`*100,1)," (", 
                                        round(`w_prev_lower_STOOL-Shigella spp.-Ciprofloxacin`*100,1), "-",
                                        round(`w_prev_upper_STOOL-Shigella spp.-Ciprofloxacin`*100,1),")\n",
                                        `InterpretableAST_STOOL-Shigella spp.-Ciprofloxacin`, " (",
                                        `InterpretableAST_all_STOOL-Shigella spp.-Ciprofloxacin`, ")"),
    #12
    U_E.coli_Cefotaxime = paste0(round(`w_prev_URINE-Escherichia coli-Cefotaxime`*100,1)," (", 
                                        round(`w_prev_lower_URINE-Escherichia coli-Cefotaxime`*100,1), "-",
                                        round(`w_prev_upper_URINE-Escherichia coli-Cefotaxime`*100,1),")\n",
                                 `InterpretableAST_URINE-Escherichia coli-Cefotaxime`, " (",
                                 `InterpretableAST_all_URINE-Escherichia coli-Cefotaxime`, ")"),
    #13
    U_E.coli_Imipenem = paste0(round(`w_prev_URINE-Escherichia coli-Imipenem`*100,1)," (", 
                                        round(`w_prev_lower_URINE-Escherichia coli-Imipenem`*100,1), "-",
                                        round(`w_prev_upper_URINE-Escherichia coli-Imipenem`*100,1),")\n",
                               `InterpretableAST_URINE-Escherichia coli-Imipenem`, " (",
                               `InterpretableAST_all_URINE-Escherichia coli-Imipenem`, ")"),
    #14
    U_K.pneumoniae_Cefotaxime = paste0(round(`w_prev_URINE-Klebsiella pneumoniae-Cefotaxime`*100,1)," (", 
                                        round(`w_prev_lower_URINE-Klebsiella pneumoniae-Cefotaxime`*100,1), "-",
                                        round(`w_prev_upper_URINE-Klebsiella pneumoniae-Cefotaxime`*100,1),")\n",
                                       `InterpretableAST_URINE-Klebsiella pneumoniae-Cefotaxime`, " (",
                                       `InterpretableAST_all_URINE-Klebsiella pneumoniae-Cefotaxime`, ")"),
    #15
    U_K.pneumoniae_Imipenem = paste0(round(`w_prev_URINE-Klebsiella pneumoniae-Imipenem`*100,1)," (", 
                                        round(`w_prev_lower_URINE-Klebsiella pneumoniae-Imipenem`*100,1), "-",
                                        round(`w_prev_upper_URINE-Klebsiella pneumoniae-Imipenem`*100,1),")\n",
                                     `InterpretableAST_URINE-Klebsiella pneumoniae-Imipenem`, " (",
                                     `InterpretableAST_all_URINE-Klebsiella pneumoniae-Imipenem`, ")"),
    #16
    N.N.gonorrhoeae_Ceftriaxone = paste0(round(`w_prev_UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone`*100,1)," (", 
                                        round(`w_prev_lower_UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone`*100,1), "-",
                                        round(`w_prev_upper_UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone`*100,1),")\n",
                                        `InterpretableAST_UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone`, " (",
                                        `InterpretableAST_all_UROGENITAL-Neisseria gonorrhoeae-Ceftriaxone`, ")")
  ) %>%
  select(c(WHORegionName, Iso3, B_Acinetobacter_spp_imipenem,B_E.coli_Cefotaxime,
           B_E.coli_Imipenem,B_E.coli_3_gen.cephalosporins,
           B_K.pneumoniae_Cefotaxime,B_K.pneumoniae_Imipenem,
           B_Salmonella_spp._Ciprofloxacin,B_S.aureus_Methicillin_resistance,
           B_S.pneumoniae_PenicillinG, G_Salmonella_spp._Ciprofloxacin,
           G_Shigella_spp._Ciprofloxacin,U_E.coli_Cefotaxime,
           U_E.coli_Imipenem, U_K.pneumoniae_Cefotaxime,
           U_K.pneumoniae_Imipenem, N.N.gonorrhoeae_Ceftriaxone))

a6 = left_join(a5, cdata %>% select(Iso3, CountryTerritoryArea))
a7 = a6 %>% select(-c(Iso3)) 

annex7 = a7 %>% 
  select(c(WHORegionName,CountryTerritoryArea, B_Acinetobacter_spp_imipenem,
           B_E.coli_Cefotaxime, B_E.coli_Imipenem, B_E.coli_3_gen.cephalosporins, 
           B_K.pneumoniae_Cefotaxime, B_K.pneumoniae_Imipenem,
           B_Salmonella_spp._Ciprofloxacin, B_S.aureus_Methicillin_resistance,
           B_S.pneumoniae_PenicillinG, G_Salmonella_spp._Ciprofloxacin,
           G_Shigella_spp._Ciprofloxacin, U_E.coli_Cefotaxime,
           U_E.coli_Imipenem, U_K.pneumoniae_Cefotaxime,
           U_K.pneumoniae_Imipenem, N.N.gonorrhoeae_Ceftriaxone)) %>%
  mutate(WHORegionName = factor(WHORegionName,levels = c(
    "Global",
    "African Region",
    "Region of the Americas",
    "South-East Asia Region",
    "European Region",
    "Eastern Mediterranean Region", 
    "Western Pacific Region"
  )))


annex7 = annex7[order(annex7$WHORegionName),]

annex7

# FOR REPORT ANNEX 7
write.csv(annex7, file = paste0(dirOutputReport, "/Annexes/Provisional/Annex7_CTAprevalence_2023_UPDATED.csv"))

fig4.2_c = fig4.2 %>% filter(!(Specimen%in%c("BLOOD", "URINE", "STOOL") & Iso3%in%c("GBR", "CAN") & Year==2023)&
                               !(combined%in%c("BLOOD-Salmonella spp.-Ciprofloxacin" ) & Iso3=="IND" & Year==2023)&
                               !(Specimen%in%c("UROGENITAL") & Iso3=="CAN" & Year==2023))

# BSI
bsi_plot_list <- list()

for (pp in ppbsi) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
               amr_data = fig4.2_c, 
               year = 2023,
               palette = palette_map2,
               title = "No",
               estimated = "Yes",
               specimen = specimen, 
               pathogen_name = pathogen,
               antibiotic_name = antibiotic, 
               antibiotic_label = antibiotic, 
               na_color = "white",
               subtitle = TRUE,
               show_fill=ifelse(pathogen=="Streptococcus pneumoniae", T,F))
  bsi_plot_list[[pp]] <- p
}
combined_plot_bsi = bsi_plot_list[[1]]+bsi_plot_list[[2]]+bsi_plot_list[[3]]+
  bsi_plot_list[[4]]+bsi_plot_list[[5]]+bsi_plot_list[[6]]+bsi_plot_list[[7]]+plot_layout(ncol = 2)  # Arrange in 2 columns

#combined_plot_bsi

# SDG 
SDG_plot_list <- list()

for (pp in SDG) {
  # Split each element into specimen, pathogen, and antibiotic
  i = 1
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen_name <- ifelse(i==1, "E. coli", "S. aureus")
  
  antibiotic <- parts[3]
  
  antibiotic <- ifelse(antibiotic=="Third", "Third-generation cephalosporins","Methicillin-resistance")
  antibiotic_name <- ifelse(antibiotic=="Methicillin-resistance", "Methicillin resistance", antibiotic)
  #print(antibiotic_name)
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic_name,
                   na_color = "white",
                   show_fill=ifelse(pathogen=="Escherichia coli", T,F))
  SDG_plot_list[[pp]] <- p
  i=1+1
}
combined_plot_sdg = SDG_plot_list[[1]]+SDG_plot_list[[2]]+plot_layout(ncol = 1)  # Arrange in 2 columns


# UTI 
uti_plot_list <- list()

for (pp in pputi) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   show_fill=ifelse(pathogen=="Klebsiella pneumoniae" & antibiotic=="Imipenem", T,F))
  uti_plot_list[[pp]] <- p
  i=1+1
}

combined_plot_uti = uti_plot_list[[1]]+uti_plot_list[[2]]+uti_plot_list[[3]]+
  uti_plot_list[[4]]+plot_layout(ncol = 2)  # Arrange in 2 columns

#combined_plot_uti

# GI 
gi_plot_list <- list()

for (pp in c(ppgi)) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   show_fill=ifelse(pathogen=="Salmonella spp.", T,F))
  gi_plot_list[[pp]] <- p
  i=1+1
}
combined_plot_gi = gi_plot_list[[2]]+gi_plot_list[[1]]+plot_layout(ncol = 2)  # Arrange in 2 columns

# N. Gonorrhoe
ng_plot_list <- list()

for (pp in c(ppuro)) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   show_fill=T)
  ng_plot_list[[pp]] <- p
}
combined_plot_gon = ng_plot_list[[1]]+plot_layout(ncol = 1) 

# FOR REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Layout_combined_examples/Figure_4.4_CTA_bsi_prioritypathogen.svg"), 
       device="svg",
       plot = combined_plot_bsi, 
       width = 20, height = 20, dpi=300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Layout_combined_examples/Figure_4.5_CTA_sdg_prioritypathogen.svg"), 
       plot = combined_plot_sdg, 
       device="svg",
       width = 20, height = 15, dpi=300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Layout_combined_examples/Figure_4.6_CTA_uti_prioritypathogen.svg"), 
       plot = combined_plot_uti, 
       device = "svg",
       width = 20, height = 12, dpi=300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Layout_combined_examples/Figure_4.7_CTA_stool_prioritypathogen.svg"), 
       plot = combined_plot_gi, 
       device="svg",
       width = 20, height = 7, dpi=300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Layout_combined_examples/Figure_4.8_CTA_gon_prioritypathogen.svg"), 
       plot = ng_plot_list[[1]], 
       device="svg",
       width = 8, height = 5, dpi=300)

write.csv(fig4.2, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/CTAprevalence_prioritypathogen2023.csv"))

# ECCMID
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_4.2b_CTA_sdg_E.coli.svg"), 
#        device = "svg",
#        plot = SDG_plot_list[[2]], 
#        width = 15, height = 10)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_4.2b_CTA_sdg_mrsa.svg"), 
#        device = "svg",
#        plot = SDG_plot_list[[1]], 
#        width = 15, height = 10)

# MAKE PLOTS - STORE INDIVIDUALLY
#--------------------------------------------------------------------------
fig4.2 = d5 %>% filter(drug_bug%in%c(ppbsi,SDG,pputi,ppgi,ppuro))
fig4.2_c = fig4.2 %>% filter(!(Specimen%in%c("BLOOD", "URINE", "STOOL") & Iso3%in%c("GBR", "CAN") & Year==2023)&
                               !(combined%in%c("BLOOD-Salmonella spp.-Ciprofloxacin" ) & Iso3=="IND" & Year==2023)&
                               !(Specimen%in%c("UROGENITAL") & Iso3=="CAN" & Year==2023))

unique(fig4.2_c$drug_bug)

# BSI
bsi_plot_list_i <- list()

for (pp in ppbsi) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=T,
  show_fill=ifelse(pathogen=="Streptococcus pneumoniae", T,F))
  bsi_plot_list_i[[pp]] <- p
}

#combined_plot_bsi

# SDG 
SDG_plot_list_i <- list()

for (pp in SDG) {
  # Split each element into specimen, pathogen, and antibiotic
  i = 1
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen_name <- ifelse(i==1, "E. coli", "S. aureus")
  
  antibiotic <- parts[3]
  
  antibiotic <- ifelse(antibiotic=="Third", "Third-generation cephalosporins","Methicillin-resistance")
  antibiotic_name <- ifelse(antibiotic=="Methicillin-resistance", "Methicillin resistance", antibiotic)
  #print(antibiotic_name)
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic_name,
                   na_color = "white",
                   subtitle=T,
  show_fill=ifelse(pathogen=="Escherichia coli", T,F))
  SDG_plot_list_i[[pp]] <- p
  i=1+1
}


# UTI 
uti_plot_list_i <- list()

for (pp in pputi) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=T)#,
  show_fill=ifelse(pathogen=="Klebsiella pneumoniae" & antibiotic=="Imipenem", T,F)
  uti_plot_list_i[[pp]] <- p
}

#combined_plot_uti

# GI 
gi_plot_list_i <- list()

for (pp in c(ppgi)) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=T,
                   show_fill=ifelse(pathogen=="Salmonella spp.", T,F))
  gi_plot_list_i[[pp]] <- p
}

# N. Gonorrhoe
ng_plot_list_i <- list()

for (pp in c(ppuro)) {
  # Split each element into specimen, pathogen, and antibiotic
  parts <- unlist(strsplit(pp, "-"))
  
  specimen <- parts[1]
  pathogen <- parts[2]
  #pathogen <- fig4.2$PathogenName2[fig4.2$PathogenName==pathogen][1]
  antibiotic <- parts[3]
  
  # Call the plot function
  p = plot_amr_map(shapefile = world_combined, 
                   amr_data = fig4.2_c, 
                   year = 2023,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=T,
                   show_fill=T)
  ng_plot_list_i[[pp]] <- p
}


# Individual storing of figures
# BSI
for (plot_name in names(bsi_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.4_CTA_", plot_name, ".svg"),
    plot = bsi_plot_list_i[[plot_name]],
    device = svg, width = 11, height = 7, dpi = 300
  )
}

# SDG
for (plot_name in names(SDG_plot_list_i)) {
ggsave(
  filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.5_CTA_", plot_name, ".svg"),
  plot = SDG_plot_list_i[[plot_name]],
  device = "svg", width = 11, height = 7, dpi = 300
)
}

# UTI
for (plot_name in names(uti_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.6_CTA_", plot_name, ".svg"),
    plot = uti_plot_list_i[[plot_name]],
    device = "svg", width = 11, height = 7, dpi = 300
  )
}

# STOOL
for (plot_name in names(gi_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.7_CTA_", plot_name, ".svg"),
    plot = gi_plot_list_i[[plot_name]],
    device = "svg", width = 11, height = 7, dpi = 300
  )
}

# Gonorrhoea
for (plot_name in names(ng_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.8_CTA_", plot_name, ".svg"),
    plot = ng_plot_list_i[[plot_name]],
    device = svg, width = 11, height = 7, dpi = 300
  )
}

#  4.3	Time series of resistance to selected antibiotics, 2018-2022
###################################################################


# FIGURE 4.11
#---------------------------------------------------------------------------------------
t1 = read.csv(file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_prevalence_slope_UPDATED.csv"))
final_results = read.csv(file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.11_REGION_trends_allyears.csv"))

t1 <- t1%>%
  dplyr::select(drug_bug, WHORegionName, Specimen, PathogenName, AntibioticName, median_slope_p, Q2.5_p,Q97.5_p, Significant, n, n_isolates,n_3y, n_isolates_3y) %>%
  arrange(Specimen, AntibioticName, PathogenName) %>%
  group_by(Specimen, AntibioticName) %>%
  mutate(
    AntibioticName2 = case_when(
      AntibioticName == "Third generation cephalosporins" ~ '3rd-gen. cephalosporins',
      TRUE ~ AntibioticName
    ),
    PathogenName2 = factor(
      PathogenName,
      levels = c("Acinetobacter spp.", "Escherichia coli", "Klebsiella pneumoniae",
                 "Salmonella spp.", "Staphylococcus aureus", "Streptococcus pneumoniae",
                 "Shigella spp.", "Neisseria gonorrhoeae"),
      labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                 "Salmonella spp.", "S. aureus", "S. pneumoniae",
                 "Shigella spp.", "N. gonorrhoeae")
    ),
    custom_label = case_when(
      grepl("spp\\.", PathogenName2) ~ paste0("<i>", sub(" spp\\.", "", PathogenName2), "</i> spp. - ", AntibioticName2),
      TRUE ~ paste0("<i>", PathogenName2, "</i> - ", AntibioticName2)
    ),
    custom_label = factor(custom_label,
                          levels = c(
                            "<i>N. gonorrhoeae</i> - Ceftriaxone",
                            "<i>Shigella</i> spp. - Ciprofloxacin",
                            "<i>S. pneumoniae</i> - Penicillin G",
                            "<i>S. aureus</i> - Methicillin resistance",
                            "<i>Salmonella</i> spp. - Ciprofloxacin",
                            "<i>K. pneumoniae</i> - Imipenem",
                            "<i>K. pneumoniae</i> - Cefotaxime",
                            "<i>E. coli</i> - Imipenem",
                            "<i>E. coli</i> - 3rd-gen. cephalosporins",
                            "<i>E. coli</i> - Cefotaxime",
                            "<i>Acinetobacter</i> spp. - Imipenem" )),
    Specimen_label = case_when(
      Specimen == "BLOOD" ~ "Bloodstream",
      Specimen == "URINE" ~ "Urinary tract",
      Specimen == "STOOL" ~ "Gastrointestinal",
      Specimen == "UROGENITAL" ~ "Gonorrhoea",
      TRUE ~ NA_character_
    ),
    Specimen_label = factor(Specimen_label, levels = c("Bloodstream",
                                                       "Gastrointestinal", "Urinary tract",  "Gonorrhoea")),
    region_labels = factor(WHORegionName, levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region", "European Region",
                                                     "Eastern Mediterranean Region", "Western Pacific Region"),
                           labels = c(
                             "Global" = "Global",
                             "African Region" = "African\nRegion",
                             "Region of the Americas" = "Region of the \nAmericas",
                             "South-East Asia Region" = "South-East\nAsia Region",
                             "European Region" = "European\nRegion",
                             "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
                             "Western Pacific Region" = "Western\nPacific Region")
    ),
    text_with_star = ifelse(
      Significant == "Yes",
      paste0(format(round(median_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")*"),
      paste0(format(round(median_slope_p, 1), trim = TRUE), "<br>(",
             format(round(Q2.5_p, 1), trim = TRUE), ", ",
             format(round(Q97.5_p, 1), trim = TRUE), ")")
    )
  ) %>%
  ungroup()


# FIGURE 4.11B
p1 = ggplot(t1, aes(x = region_labels, y = factor(custom_label))) +
  geom_tile(aes(fill = median_slope_p), linewidth = 0.8) +
  ggtext::geom_richtext(
    aes(label = text_with_star), 
    size = 3.5, 
    color = "black",
    fill = NA, 
    label.color = NA
  ) +
  # Add borders around tiles with a star and apply linetype for significant positive/negative slopes
  # geom_tile(
  #   data = t1 %>% filter(has_star & !is.na(linetype)),  # Correct subsetting
  #   aes(linetype = linetype),
  #   color = "black",
  #   linewidth = 1,  # Thicker border for emphasis
  #   fill = NA  # Transparent fill to show tile background
  # ) +
  #scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) +  # Define line styles
  scale_fill_gradient2(
    low = "dodgerblue", 
    mid = "white", 
    high = "firebrick", 
    midpoint = 0, 
    name = "% Change",
    na.value = "grey90"
  ) +
  labs(
    x = "",
    y = NULL,
    title = ""
  ) +
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "top") +
  ggh4x::facet_grid2(Specimen_label ~ ., scales = "free_y", space = "free_y", switch = "both",
                     labeller = labeller(Specimen_label = c(
                       "Bloodstream" = "Bloodstream\n", 
                       "Urinary tract" = "Urinary tract\n",
                       "Gastrointestinal" = "Gastrointestinal\n",
                       "Gonorrhoea" = "Urogenital\n"
                     ))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.y.left = element_text(
      angle = 90, 
      size = 12,  
      margin = margin(r = 5)
    ),
    strip.placement = "outside",
    panel.spacing = unit(0.2, "lines"),
    legend.position = "bottom",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(linetype = "none", colour = "none")  # Hide linetype legend

p1


ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11B_slopes_UPDATED_NEW.svg"), 
       plot = p1,
       device = "svg",
       dpi = 300,
       width = 11, height = 12) 

# p2 = ggplot(t1%>%filter(Specimen=="BLOOD"), aes(x = region_labels, y = factor(custom_label))) +
#   geom_tile(aes(fill = median_slope_p), linewidth = 0.8) +
#   ggtext::geom_richtext(
#     aes(label = text_with_star), 
#     size = 3.5, 
#     color = "black",
#     fill = NA, 
#     label.color = NA
#   ) +
#   # Add borders around tiles with a star and apply linetype for significant positive/negative slopes
#   geom_tile(
#     data = t1 %>% filter(has_star & !is.na(linetype) &Specimen=="BLOOD"),  # Correct subsetting
#     aes(linetype = linetype),
#     color = "black",
#     linewidth = 1,  # Thicker border for emphasis
#     fill = NA  # Transparent fill to show tile background
#   ) +
#   scale_linetype_manual(values = c("solid" = "solid", "dotted" = "dashed")) +  # Define line styles
#   scale_fill_gradient2(
#     low = "dodgerblue", 
#     mid = "white", 
#     high = "firebrick", 
#     midpoint = 0, 
#     name = "% Change",
#     na.value = "grey90"
#   ) +
#   labs(
#     x = "",
#     y = NULL,
#     title = ""
#   ) +
#   scale_y_discrete(position = "right") +
#   scale_x_discrete(position = "top") +
#   ggh4x::facet_grid2(Specimen_label ~ ., scales = "free_y", space = "free_y", switch = "both",
#                      labeller = labeller(Specimen_label = c(
#                        "Bloodstream" = "Bloodstream\n", 
#                        "Urinary tract" = "Urinary tract\n",
#                        "Gastrointestinal" = "Gastrointestinal\n",
#                        "Gonorrhoea" = "Gonorrhoea\n"
#                      ))) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
#     strip.text.y.left = element_text(
#       angle = 90, 
#       size = 14,  
#       margin = margin(r = 5)
#     ),
#     strip.placement = "outside",
#     panel.spacing = unit(0.2, "lines"),
#     legend.position = "bottom",
#     axis.title.y = element_blank(),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   guides(linetype = "none", colour = "none")  # Hide linetype legend
# 
# p2

# SAVE PLOT FOR REPORT
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11_trend_regional_amr.png"), 
#        plot = p1,
#        device = "png",
#        dpi = 300,
#        width = 10, height = 12)  


# FIGURE 4.11A: LINE PLOT 
final_results = final_results %>%
  mutate(
    WHORegionName = case_when(
      WHORegionCode == "Global" ~ "Global", 
      WHORegionCode == "AFR" ~ "African Region",
      WHORegionCode == "AMR" ~ "Region of the Americas",
      WHORegionCode == "SEA" ~ "South-East Asia Region", 
      WHORegionCode == "EUR" ~ "European Region",
      WHORegionCode == "EMR" ~ "Eastern Mediterranean Region",
      WHORegionCode == "WPR" ~ "Western Pacific Region",
      TRUE ~ NA_character_
    ),
    WHORegionName = factor(WHORegionName, levels = c(
      "Global",
      "African Region",
      "Region of the Americas",
      "South-East Asia Region",
      "European Region",
      "Eastern Mediterranean Region",
      "Western Pacific Region")
    ),
    region_labels = factor(WHORegionName, levels = c("Global","African Region", "Region of the Americas", "South-East Asia Region", "European Region",
                                                     "Eastern Mediterranean Region", "Western Pacific Region"),
                           labels = c(
                             "Global" = "Global",
                             "African Region" = "African\nRegion",
                             "Region of the Americas" = "Region of the \nAmericas",
                             "South-East Asia Region" = "South-East\nAsia Region",
                             "European Region" = "European\nRegion",
                             "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
                             "Western Pacific Region" = "Western\nPacific Region")
    ),
    formatted_drug_bug = case_when(
      drug_bug == "BLOOD-Klebsiella pneumoniae-Imipenem" ~ "<i>K. pneumoniae</i> - Imipenem",
      TRUE ~ drug_bug
    ))

# Visualize Regional Trends (Specimen)

p3 = ggplot(final_results %>% filter(drug_bug %in% c("BLOOD-Klebsiella pneumoniae-Imipenem","URINE-Klebsiella pneumoniae-Imipenem")), aes(x = Year, y = regional_weighted_mean)) +
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5),
    alpha = 0.2, fill = "#377EB8"
  ) +
  geom_line(size = 1, colour = "#377EB8") +  # Line for median predictions
  ggh4x::facet_grid2(
    Specimen ~ region_labels, scales = "free_y", independent = "y",labeller = labeller(
      Specimen = c(
        "BLOOD" = "Bloodstream",
        "URINE" = "Urinary tract"
      )  # Use markdown-compatible labeller
  )) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 20)
  ) +
  scale_x_continuous(breaks = seq(2018, 2023, by = 2))+
  theme(
    strip.placement = "outside",
    strip.text.y = ggtext::element_markdown(angle = 0, hjust = 0.5, size = 16, face = "bold"),  # Ensure markdown support for facet labels
    axis.text.y = ggtext::element_markdown(size = 10, hjust = 0),
    strip.text.x = element_text(size = 14, face = "bold"),
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 16),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted")
  ) +
  labs(
    title =  "",
    y = "Percentage (%)",
    x = ""
  ) +
  theme_minimal() +
  guides(
    color = "none",
    fill = "none",
    size = "none"
  )
p3

combined_plot <- p3 / p1 +
  plot_layout(heights = c(1, 3))
combined_plot

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11A_trend_kpn.svg"), 
       plot = p3,
       device = "svg",
       dpi = 300,
       width = 12, height = 4)  



ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.11_amr_trend.svg"), 
       plot = combined_plot,
       device = "svg",
       dpi = 300,
       width = 12, height = 17)  

# Annex 8
#-----------------------------------------------------------------

df <- t1 %>%
  mutate(
    stat_label = sprintf(
      "%s (%s)",
      format(n_3y, big.mark = "\u202f", scientific = FALSE, justify="centre"),
      format(n_isolates_3y, big.mark = "\u202f", scientific = FALSE,justify="centre")
    )
  )

# Region to columns
df1 <- df %>%
  ungroup()%>%
  arrange(Specimen_label, PathogenName2, AntibioticName2) %>%
  select(Specimen_label, PathogenName2, AntibioticName2,WHORegionName, stat_label) %>%
  pivot_wider(
    names_from = WHORegionName,
    values_from = stat_label
  )

df2 = df1[c(1,3:6,8,2,7,9:16),]
df2 <- df2  %>% 
  ungroup() %>%
  group_by(Specimen_label, PathogenName2) %>%
  mutate(
    PathogenName2 = ifelse(duplicated(PathogenName2), "", as.character(PathogenName2))
    )%>%
  dplyr::select(c(Specimen_label, PathogenName2, AntibioticName2, Global, `African Region`,`Region of the Americas`,
                  `South-East Asia Region`,`European Region`,`Eastern Mediterranean Region`, `Western Pacific Region`))

export_prevalence_table_to_excel(df2, paste0(dirOutputReport, "/Annexes/Provisional/Annex8_regionalprevalence_trends_UPDATED2.xlsx"))

# 4.2.2 Pathogen distributions
###################################################################
year = 2023
d = adataAC_crude

# Load in weighted prevalence data

d1 = d %>% filter(Specimen == "BLOOD" & TotalPathogenIsolates>0, Year>2017) %>% # As estimates are produced from 2018 onwards
  mutate(
    Region = factor(WHORegionName, 
                    levels = c("African Region",
                               "Region of the Americas",
                               "South-East Asia Region",
                               "European Region",
                               "Eastern Mediterranean Region",
                               "Western Pacific Region"))
  ) %>%
  mutate(Region = relevel(Region, ref = "African Region"))

unique(d1$combined)


# REGIONAL DISTRIBUTIONS IN PATHOGENS
#--------------------------------------------------------------------

# PathogenIsolateswithAST = Within each Iso3, Year, Specimen type, 
# DemographicsOrigin type and pathogen type, corresponds to the total number of 
# isolates of this pathogen type with interpretable AST results for at least one 
# antibiotic. Within the specified grouping, corresponds to the maximum count of "InterpretableAST"

# COUNTRY TOTALS - AT LEAST 3 YEARS OF DATA
d2 <- d1 %>%
  ungroup() %>%
  #filter(AntibioticName != "Third-generation cephalosporins", Year > 2017) %>%
  dplyr::select(Region, Iso3, Year, Specimen, PathogenName, TotalPathogenIsolates) %>%
  group_by(Region, Iso3, Year, PathogenName) %>%
  summarise_all(max, na.rm = TRUE) %>%
  ungroup() %>%
  group_by(Iso3, Year) %>%
  #filter(n_distinct(PathogenName) >= 3) %>%   # 
  #ungroup() %>%
  group_by(Iso3) %>%
  filter(n_distinct(Year) >= 3) %>%           # 
  as.data.frame()

dc = d2 %>%
  ungroup() %>%
  group_by(Iso3, Year) %>%
  summarise(TotalSpecimen = sum(TotalPathogenIsolates, na.rm = T))


dc2 = left_join(d2,dc, by = c("Iso3", "Year"))

dc3 = dc2 %>%
  mutate(Prop = TotalPathogenIsolates/TotalSpecimen,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                           "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
  dplyr::select(Region,Iso3, Year, PathogenName, PathogenName2, TotalPathogenIsolates, TotalSpecimen, Prop)

dc4 = dc3 %>% filter(Year==year)

# COUNTRY TOTALS - ALL
a2 <- d1 %>%
  ungroup() %>%
  #filter(AntibioticName != "Third-generation cephalosporins", Year > 2017) %>%
  dplyr::select(Region, Iso3, Year, Specimen, PathogenName, TotalPathogenIsolates) %>%
  group_by(Region, Iso3, Year, PathogenName) %>%
  summarise_all(max, na.rm = TRUE) %>%
  ungroup() %>%
  as.data.frame()

ac = a2 %>%
  ungroup() %>%
  group_by(Iso3, Year) %>%
  summarise(TotalSpecimen = sum(TotalPathogenIsolates, na.rm = T))


ac2 = left_join(a2,ac, by = c("Iso3", "Year"))

ac3 = ac2 %>%
  mutate(Prop = TotalPathogenIsolates/TotalSpecimen,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                           "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
  dplyr::select(Region,Iso3, Year, PathogenName, PathogenName2, TotalPathogenIsolates, TotalSpecimen, Prop)

ac4 = ac3 %>% filter(Year==year)

ac4$three_year_data = ifelse(ac4$Iso3%in%dc4$Iso3, "Yes", "No") 
table(ac4$three_year_data) # 501, yes

#####################################################################################
# ANNEX 9
#####################################################################################

# Create table for Annex 9
ac5 <- ac4 %>%
  pivot_wider(
    id_cols = c(Region, Iso3), # Columns to keep as identifiers
    names_from = PathogenName2,                                  # Column to pivot wider (Year)
    values_from = c(TotalPathogenIsolates,TotalSpecimen, Prop) # Columns to spread
  )

ac6 = ac5 %>%
  select(c(Region, Iso3,`TotalPathogenIsolates_Acinetobacter spp.`,`Prop_Acinetobacter spp.`,
           `TotalPathogenIsolates_E. coli`, `Prop_E. coli`,
           `TotalPathogenIsolates_K. pneumoniae`, `Prop_K. pneumoniae`,
           `TotalPathogenIsolates_Salmonella spp.`,`Prop_Salmonella spp.`,
             `TotalPathogenIsolates_S. aureus`,`Prop_S. aureus`,
           `TotalPathogenIsolates_S. pneumoniae`, `Prop_S. pneumoniae`))

ac6[is.na(ac6)] <- 0

ac7 = ac6 %>% 
  mutate(
    Total = c(`TotalPathogenIsolates_Acinetobacter spp.`+`TotalPathogenIsolates_E. coli`+`TotalPathogenIsolates_K. pneumoniae`+
                `TotalPathogenIsolates_Salmonella spp.`+`TotalPathogenIsolates_S. aureus`+`TotalPathogenIsolates_S. pneumoniae`),
    Acinetobacter_spp = paste0(`TotalPathogenIsolates_Acinetobacter spp.`, " (", round(`Prop_Acinetobacter spp.`*100,1), "%)"),
    E.coli = paste0(`TotalPathogenIsolates_E. coli`, " (", round(`Prop_E. coli`*100,1), "%)"),
    K.pneumoniae = paste0(`TotalPathogenIsolates_K. pneumoniae`, " (", round(`Prop_K. pneumoniae`*100,1), "%)"),
    Salmonella_spp = paste0(`TotalPathogenIsolates_Salmonella spp.`, " (", round(`Prop_Salmonella spp.`*100,1), "%)"),
    S.aureus = paste0(`TotalPathogenIsolates_S. aureus`, " (", round(`Prop_S. aureus`*100,1), "%)"),
    S.pneumoniae = paste0(`TotalPathogenIsolates_S. pneumoniae`, " (", round(`Prop_S. pneumoniae`*100,1), "%)")
  ) %>%
  select(Region, Iso3, Total, Acinetobacter_spp, E.coli, K.pneumoniae, Salmonella_spp, S.aureus, S.pneumoniae)

ac8 = left_join(ac7, cdata %>% select(c(Iso3,CountryTerritoryArea)), by="Iso3")
ac8 = ac8 %>%select(Region, CountryTerritoryArea, Total, Acinetobacter_spp, E.coli, K.pneumoniae, Salmonella_spp, S.aureus, S.pneumoniae)

ac8$Total = prettyNum(ac8$Total, big.mark = " ", scientific = FALSE)

# ADD REGIONAL TOTALS
#---------------------------------------------------------------
# Regional crude calculations
dr1 <- ac4 %>%
  filter(PathogenName2 %in% c(
    "Acinetobacter spp.", "E. coli", "K. pneumoniae", "Salmonella spp.",
    "S. aureus", "S. pneumoniae"
  )) %>%
  group_by(Region, PathogenName2) %>%
  summarise(
    TotalPathogenIsolates = sum(TotalPathogenIsolates, na.rm = TRUE),
    TotalSpecimen = sum(TotalSpecimen, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Prop = ifelse(TotalSpecimen == 0, NA, TotalPathogenIsolates / TotalSpecimen),
    Label = paste0(TotalPathogenIsolates, " (", round(Prop * 100, 1), "%)")
  )

# Pivot wider to have each pathogen as a column
dr2 <- dr1 %>%
  select(Region, PathogenName2, Label) %>%
  pivot_wider(names_from = PathogenName2, values_from = Label)

# Add total isolates per region
dr3 <- dr1 %>%
  group_by(Region) %>%
  summarise(Total = sum(TotalPathogenIsolates, na.rm = TRUE), .groups = "drop") %>%
  mutate(Total = prettyNum(Total, big.mark = " ", scientific = FALSE))

# Combine
dr4 <- left_join(dr3, dr2, by = "Region") %>%
  mutate(CountryTerritoryArea = "Regional total") %>%
  select(Region, CountryTerritoryArea, Total, everything())

# REPEAT FOR GLOBAL
drg1 <- ac4 %>%
  filter(PathogenName2 %in% c(
    "Acinetobacter spp.", "E. coli", "K. pneumoniae", "Salmonella spp.",
    "S. aureus", "S. pneumoniae"
  )) %>%
  group_by(PathogenName2) %>%
  summarise(
    TotalPathogenIsolates = sum(TotalPathogenIsolates, na.rm = TRUE),
    TotalSpecimen = sum(TotalSpecimen, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Prop = ifelse(TotalSpecimen == 0, NA, TotalPathogenIsolates / TotalSpecimen),
    Label = paste0(TotalPathogenIsolates, " (", round(Prop * 100, 1), "%)")
  )

# Pivot wider to have each pathogen as a column
drg2 <- drg1 %>%
  select(PathogenName2, Label) %>%
  pivot_wider(names_from = PathogenName2, values_from = Label)

# Add total isolates per region
drg3 <- drg1 %>%
  summarise(Total = sum(TotalPathogenIsolates, na.rm = TRUE), .groups = "drop") %>%
  mutate(Total = prettyNum(Total, big.mark = " ", scientific = FALSE))

# Combine
drg4 <- cbind(Region="Global", drg2,drg3) %>%
  mutate(CountryTerritoryArea = "Regional total") %>%
  select(Region, CountryTerritoryArea, Total, everything())

dr5 = rbind(dr4, drg4)
names(dr5) = names(ac8)

ac9 = rbind(ac8, dr5)
ac10 = left_join(ac9, cdata%>%select(Iso3, CountryTerritoryArea))

ac10$three_years_data = ifelse(ac10$Iso3 %in% dc4$Iso3, "Yes", "No")
table(ac10$three_years_data) # Should be 89 countries

ac10 = ac10 %>% select(-c(Iso3))

# WRITE CSV
write.csv(ac10, file = paste0(dirOutputReport, "/Annexes/Provisional/Annex9_Table_CTA_Pathogen_distr_crude_UPDATED.csv"))

################################################################################
# PLOT REGIONAL TOTALS
################################################################################

d3 = d2 %>%
  ungroup() %>%
  group_by(Region, Year, PathogenName) %>%
  summarise(TotalPathogenIsolates = sum(TotalPathogenIsolates, na.rm = T))

t1 = d2 %>%
  group_by(Region, Year) %>%
  summarise(TotalSpecimen = sum(TotalPathogenIsolates, na.rm = T))

# TO CHECK
# a1 = d1 %>%
#   group_by(Region, Year, Iso3) %>%
#   summarise(TotalSpecimenwithAST =unique(SpecimenIsolateswithAST))
# 
# a2 = a1 %>%
#   group_by(Region, Year) %>%
#   summarise(TotalSpecimenwithAST = sum(TotalSpecimenwithAST)) # Yes same numbers

d4 = left_join(d3,t1, by = c("Region", "Year"))

d5 = d4 %>%
  mutate(Prop = TotalPathogenIsolates/TotalSpecimen,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                                         "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
          dplyr::select(Region, Year, PathogenName, PathogenName2, TotalPathogenIsolates, TotalSpecimen, Prop)

p = left_join(pdata, cdata, by =c("Iso3"))

p1 = p %>%
  group_by(WHORegionName, Year) %>%
  summarise(TotalPopulation = sum(TotalPopulation)) %>%
  mutate(
    Region = factor(WHORegionName, 
                    levels = c("African Region",
                               "Region of the Americas",
                               "South-East Asia Region",
                               "European Region",
                               "Eastern Mediterranean Region",
                               "Western Pacific Region"))
  ) %>%
  mutate(Region = relevel(Region, ref = "African Region"))

d6 = left_join(d5, p1, by=c("Region","Year"))

# GLOBAL DISTRIBTIONS
# REGIONAL DISTRIBUTIONS
dg1 = d2 %>%
  ungroup() %>%
  group_by(Year, PathogenName) %>%
  summarise(TotalPathogenIsolates = sum(TotalPathogenIsolates, na.rm=T))

tg1 = dg1 %>%
  group_by(Year) %>%
  summarise(TotalSpecimen = sum(TotalPathogenIsolates))

dg2 = left_join(dg1,tg1) 

dg2$Region = "Global"

dg3 = dg2 %>%
  mutate(Prop = TotalPathogenIsolates/TotalSpecimen,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                           "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
  dplyr::select(Region, Year, PathogenName, PathogenName2, TotalPathogenIsolates, TotalSpecimen, Prop)

d7 = rbind(d6,dg3)

d7$Region = factor(d7$Region, 
                   levels = c("Global", 
                              "African Region",
                              "Region of the Americas",
                              "South-East Asia Region",
                              "European Region",
                              "Eastern Mediterranean Region",
                              "Western Pacific Region"))
d7$Region <- relevel(d7$Region, ref = "Global")


#  colors for 3 Ecoli
blues <- brewer.pal(3, "Blues") 
greens<-brewer.pal(3,"Greens")
#  colors for Acinetobacter 
yellows <- c("gold")
orange <- c("orange")
#  colors for KPN
reds <- brewer.pal(4, "Reds")
#  colors for MRSA and strep
greys <- c("grey20","grey50")   
# combine all colors
#colors.db <- c(reds[1], blues[1], reds[2], blues[2], blues[3], reds[3], yellows,orange, greys)
colors.db <- c(greens[2], reds[3], blues[2], "lightgrey", "lemonchiffon1","darkslategrey")

colors.db %>% scales::show_col(cex_label = .7) 


reporting_summary <- d1 %>%
  group_by(Region, Year) %>%
  summarise(NumCountriesReporting = n_distinct(Iso3), .groups = "drop")

reporting_summary_global <- d1 %>%
  group_by(Year) %>%
  summarise(NumCountriesReporting = n_distinct(Iso3), .groups = "drop") %>%
  mutate(
    Region = "Global"
  )

reporting_summary = rbind(reporting_summary, reporting_summary_global)

label_y_position <- 620000

d8 = left_join(d7, reporting_summary)
d9 = d8 %>% 
  group_by(Region, Year) %>%
  mutate(NumCountriesReporting = ifelse(duplicated(NumCountriesReporting),NA,NumCountriesReporting)) %>%
  ungroup()

# AMRO = d1 %>% filter(Region=="Region of the Americas") %>% select(Iso3, Year) %>%
#   distinct()


#table(AMRO$Iso3, AMRO$Year)

regions <- unique(d9$Region)

# Create ordered dataset for each region with largest frequency on top
ordered_datasets <- map(regions, function(region) {
  d9 %>%
    filter(Region == region) %>%
    group_by(PathogenName2) %>%
    mutate(last_year_value = ifelse(Year == year, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(PathogenName2 = fct_reorder(PathogenName2, last_year_value, .desc = TRUE))  # Order by largest to smallest
})


# Distribution over time
d9_ordered <- bind_rows(ordered_datasets)

regions = c("Global", "African Region", "Region of the Americas","South-East Asia Region",       
            "European Region", "Eastern Mediterranean Region","Western Pacific Region")

colors.db <- c(greens[2], reds[3], blues[2], "lightgrey", "lemonchiffon1","darkslategrey")
colors.db %>% scales::show_col(cex_label = .7) 

value_colorsP = c("Acinetobacter spp." = colors.db[1],
                 "E. coli" = colors.db[2],
                 "K. pneumoniae" = colors.db[3],
                 "Salmonella spp."=colors.db[4],
                 "S. aureus" = colors.db[5],
                 "S. pneumoniae" = colors.db[6])


# Generate a list of ggplot objects for each region
plots <- map(regions, function(region) {
  # Filter and reorder `drug_bug2` within the specific region
  region_data <- d9_ordered %>%
    filter(Region == region) %>%
    group_by(PathogenName2) %>%
    mutate(last_year_value = ifelse(Year == year, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(PathogenName2 = fct_reorder(PathogenName2, last_year_value, .desc = TRUE))
  
  # Create the plot for the current region
  ggplot(region_data, aes(x = Year, fill = PathogenName2, y = TotalPathogenIsolates)) +
    geom_alluvium(aes(alluvium = PathogenName2), alpha = 1) +
    scale_fill_manual(name = " ", values = value_colorsP) +
    labs(
      x = "",
      y = if(region == "South-East Asia Region") 'Number of BCIs' else NULL,
      title = if (region == regions[1]) "Number of BCIs by \nbacterial pathogen" else NULL,  # Title only on the first plot
      subtitle = region
    ) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    theme_minimal() +
    scale_y_continuous(labels = label_number(big.mark = " ")) +
    theme(
      legend.text = element_text(face = "italic", size = 20),
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 34),
      plot.subtitle = element_text(size = 30, hjust = 0.5),  # Center the subtitle
      axis.text.x = element_text(size = 28),
      axis.text.y = ggtext::element_markdown(size = 28),
      strip.text = element_text(size = 28, face = "bold"),  # Strip text for region names
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none"
    ) +
    guides(fill = guide_legend(nrow = 10))
})

combined_plot1 <- wrap_plots(plots, ncol = 1) +
  plot_layout(guides = "collect") 
combined_plot1

# Generate a list of ggplot objects for each region
plots2 <- map(regions, function(region) {
  # Filter and reorder `drug_bug2` within the specific region
  region_data <- d9_ordered %>%
    filter(Region == region) %>%
    group_by(PathogenName2) %>%
    mutate(last_year_value = ifelse(Year == 2023, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(PathogenName2 = fct_reorder(PathogenName2, last_year_value, .desc = TRUE))
  
  # Create the plot for the current region
  ggplot(region_data, aes(x = Year, fill = PathogenName2, y = Prop * 100)) +
    geom_alluvium(aes(alluvium = PathogenName2), alpha = 1) +
    scale_fill_manual(name = " ", values = value_colorsP) +
    labs(
      x = "",
      y = if(region == "South-East Asia Region") 'Percentage (%)' else NULL,
      title = if (region == regions[1]) "Percentage of BCIs by \nbacterial pathogen" else NULL,  # Title only on the first plot
      subtitle = region
    ) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
    theme(
      legend.text = element_text(face = "italic", size = 20),
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 34),
      plot.subtitle = element_text(size = 30, hjust = 0.5),  # Center the subtitle
      axis.text.x = element_text(size = 28),
      axis.text.y = ggtext::element_markdown(size = 28),
      strip.text = element_text(size = 28, face = "bold"),  # Strip text for region names
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none"
    ) +
    guides(fill = guide_legend(nrow = 10))
})

combined_plot2 <- wrap_plots(plots2, ncol = 1) +
  plot_layout(guides = "collect") 
combined_plot2


# FOR DRUG BUG COMBINATIONS
#------------------------------------------------------------------------------------------

d0 = adataAC_crude
d0 = left_join(d0, cdata) %>% select(c(WHORegionName, WHORegionCode, Iso3, Year, Specimen, PathogenName,
                                       TotalPathogenIsolates)) %>%
  distinct()

# Load in weighted prevalence data
prev = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))

d = left_join(prev %>% select(c(WHORegionCode, Iso3, Year,Specimen, PathogenName, AntibioticName, combined, 
                                w_prev, w_prev_lower, w_prev_upper, amr_rate)), d0)
unique(d$combined)
#unique(d$combined[d$InReport=="Yes"])


# EXCLUDE THE CLASS COMBINATIONS + THIS IS BASED ON REPORTS WITH MINIMUM OF 10 ISOLATES PER DRUG BUG COMBINATION
d$combined[d$combined=="BLOOD-Staphylococcus aureus-Methicillin-resistance"] = "BLOOD-Staphylococcus aureus-Methicillin resistance"
#unique(d$combined[d$InReport=="Yes"])

d1 = d %>% filter(Specimen == "BLOOD" & combined%in%dbdata$combined, TotalPathogenIsolates>0,Year>2017, AntibioticName!="Minocycline") %>% # As estimates are produced from 2018 onwards
  mutate(
    Region = factor(WHORegionName, 
                    levels = c("African Region",
                               "Region of the Americas",
                               "South-East Asia Region",
                               "European Region",
                               "Eastern Mediterranean Region",
                               "Western Pacific Region"))
  ) %>%
  mutate(Region = relevel(Region, ref = "African Region"))
unique(d1$combined) # 43 combinations

# COUNTRY TOTALS
r1 <- d1 %>%
  filter(!AntibioticName %in% c("Third-generation cephalosporins")) %>%
  ungroup() %>%
  dplyr::select(Region, Iso3, Year, Specimen, PathogenName, AntibioticName, TotalPathogenIsolates, w_prev, w_prev_lower, w_prev_upper, amr_rate) %>%
  group_by(Region, Iso3, Year, PathogenName, AntibioticName) %>%
  summarise_all(max) %>%
  ungroup() %>%
  #group_by(Iso3, Year) %>%
  #filter(n_distinct(PathogenName) >= 3) %>%
  #ungroup() %>%
  group_by(Iso3) %>%
  filter(
    2023 %in% Year,  # Ensure the country reported in 2023
    n_distinct(Year) >= 3
  ) %>%
  as.data.frame()

# REGIONAL TOTALS
r2 = r1 %>% 
  mutate(Freq = TotalPathogenIsolates*amr_rate, # 17 February: Idea is, when contract is coming through, to update this with the Bayesian prevalence rates
         Freq_est = TotalPathogenIsolates*w_prev,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                           "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
  dplyr::select(Region, Iso3, Year, PathogenName, PathogenName2,AntibioticName, TotalPathogenIsolates, Freq, Freq_est)

# r3 = r2 %>%
#   ungroup() %>%
#   group_by(Region, Year, PathogenName, AntibioticName) %>%
#   summarise(InterpretableAST = sum(InterpretableAST, na.rm=T),
#             Resistant = sum(Resistant, na.rm=T),
#             Freq = sum(Freq, na.rm=T)) 

r3 <- r2 %>%
  ungroup() %>%
  group_by(Region, Year, PathogenName, AntibioticName) %>%
  summarise(Freq = sum(Freq, na.rm = TRUE),
            Freq_est = sum(Freq_est, na.rm = TRUE),
            .groups = "drop") 

# s1 = r1 %>%
#   group_by(Region, Year, PathogenName) %>%
#   summarise(TotalPathogenIsolates = sum(unique(TotalPathogenIsolates))
#   )


#r4 = left_join(r3,s1, by = c("Region", "Year", "PathogenName"))

l1 = r3 %>%
  group_by(Region, Year) %>%
  summarise(
    TOTALTotalPathogenIsolates = sum(Freq, na.rm=T),
    TOTALTotalPathogenIsolates_est = sum(Freq_est, na.rm=T))

r4 = left_join(r3,l1) 
r5 = r4 %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq),
         Freq_est = ifelse(is.na(Freq_est), 0, Freq_est),
         Prop = Freq/TOTALTotalPathogenIsolates,
         Prop_est = Freq_est/TOTALTotalPathogenIsolates_est
  )


p = left_join(pdata, cdata, by =c("Iso3"))

# GLOBAL DISTRIBTIONS
# rg1 = r2 %>%
#   ungroup() %>%
#   group_by(Year, PathogenName, AntibioticName) %>%
#   summarise(InterpretableAST = sum(InterpretableAST),
#             Resistant = sum(Resistant))
# 
# sg1 = r1 %>%
#   ungroup() %>%
#   group_by(Year, PathogenName) %>%
#   summarise(TotalPathogenIsolates = sum(unique(TotalPathogenIsolates))
#   )
# 
# rg2 = left_join(rg1,sg1, by = c("Year", "PathogenName"))
# 
# rg2$Region = "Global"

rg1 <- r2 %>%
  ungroup() %>%
  group_by(Year, PathogenName, AntibioticName) %>%
  summarise(Freq = sum(Freq, na.rm = TRUE),
            Freq_est = sum(Freq_est, na.rm = TRUE),
            .groups = "drop") 


lg1 = rg1 %>%
  group_by(Year) %>%
  summarise(
    TOTALTotalPathogenIsolates = sum(Freq, na.rm=T),
    TOTALTotalPathogenIsolates_est = sum(Freq_est, na.rm=T))

rg2 = left_join(rg1,lg1) 

rg3 = rg2 %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq),
         Freq_est = ifelse(is.na(Freq_est), 0, Freq_est),
         Prop = Freq/TOTALTotalPathogenIsolates,
         Prop_est = Freq_est/TOTALTotalPathogenIsolates_est
  )

rg3$Region = "Global"

rg3 = rg3 %>% dplyr::select(Region, Year, PathogenName, AntibioticName, Freq, Freq_est, Prop, Prop_est, TOTALTotalPathogenIsolates,TOTALTotalPathogenIsolates_est)

r6 = rbind(r5,rg3)

r6$Region = factor(r6$Region, 
                   levels = c("Global", 
                              "African Region",
                              "Region of the Americas",
                              "South-East Asia Region",
                              "European Region",
                              "Eastern Mediterranean Region",
                              "Western Pacific Region"))
r6$Region <- relevel(r6$Region, ref = "Global")



r7 = r6 %>% mutate(
  PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                  "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                         labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                    "Salmonella spp.", "S. aureus", "S. pneumoniae")),
  drug_bug = paste0(PathogenName2, "-", AntibioticName),
  id = paste0(drug_bug, "-", Region)
)

r7_complete <- r7 %>% filter(Year%in%c("2023")) %>%
  group_by(Region) %>%
  arrange(desc(Freq_est)) %>%
  slice(1:10) %>%
  mutate(
    id = paste0(drug_bug, "-", Region)
  )

r7_top = r7%>%filter(id%in%r7_complete$id)

table(r7_top$Year) # 2018 and 2019 not complete for all regions/DB leave out
table(r7_top$Year, r7_top$Region) # The america's missing drug bug combinations
# unique(r7_top$drug_bug[r7_top$Region=="Region of the Americas"&r7_top$Year==2020])
# unique(r7_top$drug_bug[r7_top$Region=="Region of the Americas"&r7_top$Year==2018]) #  "K. pneumoniae-Cefotaxime" and "E. coli-Levofloxacin" missing
# unique(r7_top$drug_bug[r7_top$Region=="Region of the Americas"&r7_top$Year==2019]) #   "E. coli-Levofloxacin" missing

# afro2017abactmino = r7_top %>% filter(Region == "African Region", Year==2019,PathogenName=="Acinetobacter spp.", AntibioticName=="Minocycline") %>%
#   mutate(Year = 2018,
#          Freq = 0,
#          Prop = 0)

# amro2019ecollevo = r7_top %>% filter(Region == "Region of the Americas", Year==2020,PathogenName=="Escherichia coli", AntibioticName=="Levofloxacin") %>%
#    mutate(Year = 2019,
#           Freq = 0,
#           Freq.est = 0,
#          Prop = 0,
#          Prop.est=0)%>%
#   select(names(r7_top))
# 
# amro2018ecollevo = r7_top %>% filter(Region == "Region of the Americas", Year==2020,PathogenName=="Escherichia coli", AntibioticName=="Levofloxacin") %>%
#   mutate(Year = 2018,
#          Freq = 0,
#          Freq_est =0,
#          Prop = 0,
#          Prop_est=0)%>%
#   select(names(r7_top))
# 
# amro2018kpncef = r7_top %>% filter(Region == "Region of the Americas", Year==2020,PathogenName =="Klebsiella pneumoniae", AntibioticName=="Cefotaxime") %>%
#   mutate(Year = 2018,
#          Freq = 0,
#          Freq_est=0,
#          Prop = 0,
#          Prop_est = 0) %>%
#   select(names(r7_top))
# 
# r8_top = rbind(r7_top, amro2019ecollevo,amro2018kpncef,amro2018ecollevo)
# unique(r8_top$drug_bug[r8_top$Region=="Region of the Americas"&r8_top$Year==2020])
# unique(r8_top$drug_bug[r8_top$Region=="Region of the Americas"&r8_top$Year==2018]) #  "K. pneumoniae-Cefotaxime" and "E. coli-Levofloxacin" missing
# unique(r8_top$drug_bug[r8_top$Region=="Region of the Americas"&r8_top$Year==2019]) #   "E. coli-Levofloxacin" missing

r8_top = r7_top
unique(r8_top$drug_bug)

# IF CRUDE DATA
# r8_top = r8_top %>% mutate(
#   drug_bug2 = factor(drug_bug, levels=c("Acinetobacter spp.-Gentamicin",
#                                         "Acinetobacter spp.-Imipenem",
#                                         "Acinetobacter spp.-Meropenem",
#                                         "Acinetobacter spp.-Amikacin",
#                                          "E. coli-Co-trimoxazole",
#                                          "E. coli-Cefepime",
#                                          "E. coli-Ceftazidime",
#                                          "E. coli-Cefotaxime",
#                                          "E. coli-Ceftriaxone",
#                                          "E. coli-Levofloxacin", 
#                                          "E. coli-Ciprofloxacin",
#                                          "K. pneumoniae-Co-trimoxazole",
#                                          "K. pneumoniae-Cefepime",
#                                          "K. pneumoniae-Ceftazidime",
#                                          "K. pneumoniae-Ceftriaxone",
#                                          "K. pneumoniae-Cefotaxime", 
#                                          "K. pneumoniae-Levofloxacin",
#                                          "K. pneumoniae-Ciprofloxacin",
#                                          "S. aureus-Methicillin resistance")
#     
#   ))

# IF ESTIMATES DATA
r8_top = r8_top %>% mutate(
  drug_bug2 = factor(drug_bug, levels=c("Acinetobacter spp.-Gentamicin",
                                        "Acinetobacter spp.-Imipenem",
                                        "Acinetobacter spp.-Meropenem",
                                        "Acinetobacter spp.-Amikacin",
                                        #"Acinetobacter spp.-Minocycline",
                                        #"E. coli-Co-trimoxazole",
                                        "E. coli-Cefepime",
                                        "E. coli-Ceftazidime",
                                        "E. coli-Cefotaxime",
                                        "E. coli-Ceftriaxone",
                                        "E. coli-Levofloxacin", 
                                        "E. coli-Ciprofloxacin",
                                        "K. pneumoniae-Cefepime",
                                        "K. pneumoniae-Ceftazidime",
                                        "K. pneumoniae-Ceftriaxone",
                                        "K. pneumoniae-Cefotaxime", 
                                        "K. pneumoniae-Levofloxacin",
                                        "K. pneumoniae-Ciprofloxacin",
                                        "K. pneumoniae-Ertapenem",
                                        "S. aureus-Methicillin resistance")
                     
  ))
#  colors for 3 Ecoli
blues <- brewer.pal(6, "Blues")
greens <- brewer.pal(5,"Greens")
#  colors for Acinetobacter 
yellows <- c("gold")
orange <- c("orange")
#  colors for KPN
reds <- brewer.pal(6, "Reds")
#  colors for MRSA and strep
greys <- c("grey20","grey50")   
# combine all colors
#colors.db <- c(reds[1], blues[1], reds[2], blues[2], blues[3], reds[3], yellows,orange, greys)

colors.dbr = c(greens[1],greens[2],greens[3],greens[4], greens[5],blues[1],blues[2],blues[3],blues[4],blues[5],reds[2],reds[3],reds[4],reds[5],reds[6],"gold" )
colors.dbr %>% scales::show_col(cex_label = .7) 

# IF CRUDE DATA
# value_colors = c("Acinetobacter spp.-Gentamicin" = greens[1],
#                  "Acinetobacter spp.-Imipenem" = greens[2],
#                  "Acinetobacter spp.-Meropenem" = greens[3],
#                  "Acinetobacter spp.-Amikacin" = greens[4],
#                  "E. coli-Co-trimoxazole" = reds[1],
#                  "E. coli-Cefepime" = reds[3],
#                  "E. coli-Ceftazidime" = "orangered",
#                  "E. coli-Cefotaxime" = reds[4],
#                  "E. coli-Ceftriaxone" = reds[5],
#                  "E. coli-Levofloxacin" = reds[6], 
#                  "E. coli-Ciprofloxacin"="orangered4",
#                  "K. pneumoniae-Co-trimoxazole" = blues[1],
#                  "K. pneumoniae-Cefepime" = blues[2],
#                  "K. pneumoniae-Ceftazidime"=blues[3],
#                  "K. pneumoniae-Ceftriaxone" = blues[4],
#                  "K. pneumoniae-Cefotaxime"=blues[5], 
#                  "K. pneumoniae-Levofloxacin" = blues[6],
#                  "K. pneumoniae-Ciprofloxacin" = greys[2],
#                  "K. pneumoniae-Ertapenem" = "darkslateblue",
#                  "S. aureus-Methicillin resistance"="gold")

# IF ESTIMATES DATA
value_colors = c("Acinetobacter spp.-Gentamicin" = greens[1],
                 "Acinetobacter spp.-Imipenem" = greens[2],
                 "Acinetobacter spp.-Meropenem" = greens[3],
                 "Acinetobacter spp.-Amikacin" = greens[4],
                # "Acinetobacter spp.-Minocycline" = greens[5],
                 #"E. coli-Co-trimoxazole" = reds[1],
                 "E. coli-Cefepime" = reds[2],
                 "E. coli-Ceftazidime" = "orangered",
                 "E. coli-Cefotaxime" = reds[4],
                 "E. coli-Ceftriaxone" = reds[5],
                 "E. coli-Levofloxacin" = reds[6], 
                 "E. coli-Ciprofloxacin"="orangered4",
                 "K. pneumoniae-Cefepime" = blues[1],
                 "K. pneumoniae-Ceftazidime"=blues[2],
                 "K. pneumoniae-Ceftriaxone" = blues[3],
                 "K. pneumoniae-Cefotaxime"=blues[4], 
                 "K. pneumoniae-Levofloxacin" = blues[5],
                 "K. pneumoniae-Ciprofloxacin" = blues[6],
                "S. aureus-Methicillin resistance" = "gold",
                 "K. pneumoniae-Ertapenem" = "darkslategrey")
                # "K. pneumoniae-Imipenem"=greys[2])


regions <- unique(r8_top$Region)

# Create ordered dataset for each region with largest frequency on top
ordered_datasets <- map(regions, function(region) {
  r8_top %>%
    filter(Region == region) %>%
    group_by(drug_bug2) %>%
    mutate(last_year_value = ifelse(Year == 2023, Prop_est, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(drug_bug2 = fct_reorder(drug_bug2, last_year_value, .desc = TRUE))  # Order by largest to smallest
})

# r8_top = r8_top %>%
#   mutate(Prop2 = ifelse(Prop==0, 0.0001, Prop))


# Combine datasets into one for plotting
r8_top_ordered <- bind_rows(ordered_datasets)

regions = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",       
            "European Region", "Eastern Mediterranean Region","Western Pacific Region")

# Generate a list of ggplot objects for each region
plots3 <- map(regions, function(region) {
  # Filter and reorder `drug_bug2` within the specific region
  region_data <- r8_top_ordered %>%
    filter(Region == region) %>%
    group_by(drug_bug2) %>%
    mutate(last_year_value = ifelse(Year == 2023, Prop_est, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(drug_bug2 = fct_reorder(drug_bug2, last_year_value, .desc = TRUE))
  
  # Create the plot for the current region
  ggplot(region_data, aes(x = Year, fill = drug_bug2, y = Prop_est * 100)) +
    geom_alluvium(aes(alluvium = drug_bug2), alpha = 1) +
    scale_fill_manual(name = " ", values = value_colors) +
    labs(
      x = "",
      y = if(region == "South-East Asia Region") 'Percentage (%)' else NULL,
      title = if (region == regions[1]) "Percentage of drug-resistant BCIs \nby drug-resistant bacteria" else NULL,  # Title only on the first plot
      subtitle = region
    ) +
    scale_x_continuous(breaks = c(2018, 2020, 2022)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
    theme(
      legend.text = element_text(face = "italic", size = 20),
      plot.background = element_rect(fill = "white", colour = "white"),
      title = element_text(size = 34),
      plot.subtitle = element_text(size = 30, hjust = 0.5),  # Center the subtitle
      axis.text.x = element_text(size = 28),
      axis.text.y = ggtext::element_markdown(size = 28),
      strip.text = element_text(size = 28, face = "bold"),  # Strip text for region names
      panel.grid.major = element_line(linetype = "dotted"),
      panel.grid.minor = element_line(linetype = "dotted"),
      legend.position = "none"
    ) +
    guides(fill = guide_legend(nrow = 10))
})

combined_plot3 <- wrap_plots(plots3, ncol = 1) +
  plot_layout(guides = "collect") 
combined_plot3

# UPDATED FIGURE FOR 2023 ONLY
#-------------------------------------------------------------

plot2023 <- d9 %>%
  filter(Year == 2023) %>%
  mutate(Prop = replace_na(Prop, 0),
         Prop_pct = Prop * 100)

# Global legend order
legend_levels <- plot2023 %>%
  group_by(PathogenName2) %>%
  summarise(tot = sum(Prop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(tot)) %>%
  pull(PathogenName2)

# Per-region stack order
plot2023 <- plot2023 %>%
  group_by(Region) %>%
  mutate(stack_order = -Prop) %>%
  ungroup() %>%
  mutate(
    #PathogenName2 = factor(PathogenName2, levels = legend_levels),
    Region = factor(
      Region,
      levels = c("Western Pacific Region",
                 "Eastern Mediterranean Region",
                 "European Region",
                 "South-East Asia Region",
                 "Region of the Americas",
                 "African Region",
                 "Global")
    )
  )

p4 <- ggplot(plot2023,
             aes(x = Region, y = Prop_pct,
                 fill = PathogenName2, order = stack_order)) +
  coord_flip() +
  geom_col(width = 0.7) +
  scale_fill_manual(name = " ", values = value_colorsP,
                    breaks = legend_levels, drop = FALSE) +
  labs(x = "", y = "Percentage (%)",
       title = "Percentage of BCIs by \nbacterial pathogen") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.text.x = element_text(size = 28, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 28),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "right"
  ) +
  guides(fill = FALSE)

p4

p4_wl <- ggplot(plot2023,
             aes(x = Region, y = Prop_pct,
                 fill = PathogenName2, order = stack_order)) +
  coord_flip() +
  geom_col(width = 0.7) +
  scale_fill_manual(name = " ", values = value_colorsP,
                    breaks = legend_levels, drop = FALSE) +
  labs(x = "", y = "Percentage (%)",
       title = "Percentage of BCIs by \nbacterial pathogen") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.text.x = element_text(size = 28, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 28),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom"
  )  +
  guides(fill = guide_legend(nrow = 10))

p4_wl


# RESISTANCE PATTERNS
# 1) Build one 2023 dataset
plot2023 <- r8_top %>%
  filter(Year == 2023) %>%
  mutate(Prop_pct = Prop_est * 100)

# Global legend order (optional but nice to keep legend stable)
legend_levels <- plot2023 %>%
  group_by(drug_bug2) %>%
  summarise(tot = sum(Prop_est, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(tot)) %>%
  pull(drug_bug2)

# Per-region stack order using the `order` aesthetic 
# Larger Prop_est should sit lower in the stack -> use order = -Prop_est
plot2023 <- plot2023 %>%
  group_by(Region) %>%
  mutate(stack_order = -Prop_est) %>%
  ungroup() %>%
  mutate(#drug_bug2 = factor(drug_bug2, levels = legend_levels),
         Region = factor(Region, levels = c("Western Pacific Region", 
                                            "Eastern Mediterranean Region",
                                            "European Region",             
                                            "South-East Asia Region",
                                            "Region of the Americas",
                                            "African Region",
                                            "Global"
                                            )))

# Single plot: one stacked bar per region (2023 only)
p5 = ggplot(plot2023, aes(x = Region, y = Prop_pct, fill = drug_bug2, order = stack_order)) +
  coord_flip()+
  geom_col(width = 0.7) +
  scale_fill_manual(name = " ", values = value_colors, drop = FALSE) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = "Percentage of drug-resistant BCIs \nby drug-resistant bacteria",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.text.x = element_text(size = 28, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 28),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "right"
  ) +
  guides(fill = FALSE)
p5

p5_wl = ggplot(plot2023, aes(x = Region, y = Prop_pct, fill = drug_bug2, order = stack_order)) +
  coord_flip()+
  geom_col(width = 0.7) +
  scale_fill_manual(name = " ", values = value_colors, drop = FALSE) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = "Percentage of drug-resistant BCIs \nby drug-resistant bacteria",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.text.x = element_text(size = 28, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 28),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom"
  )  +
  guides(fill = guide_legend(nrow = 10))
p5_wl

# FOR REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8a_pathogen_distr_bsi_crude_UPDATE.svg"), 
       plot = combined_plot1, 
       device="svg",
       width = 10, height = 44)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8b_pathogen_distr_bsi_crude_UPDATE.svg"), 
       plot = combined_plot2, 
       device="svg",
       width = 10, height = 44)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8c_pathogen_distr_bsi_estimated_UPDATE.svg"), 
       plot = combined_plot3, 
       device="svg",
       width = 10, height = 44)

# ALTERNATIVE PLOTTING
#--------------------------------------------------------------

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_3.12a_pathogen_distr_bsi_2023.svg"), 
       plot = p4, 
       device="svg",
       width = 15, height = 8)


ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_3.12a_pathogen_distr_bsi_2023_withlegend.svg"), 
       plot = p4_wl, 
       device="svg",
       width = 15, height = 10)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_3.12b_pathogen_comb_bsi_est_2023.svg"), 
       plot = p5, 
       device="svg",
       width = 15, height = 8)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_3.12b_pathogen_comb_bsi_est_2023_withlegend.svg"), 
       plot = p5_wl, 
       device="svg",
       width = 15, height = 10)


# Plot for legend with all combinations
p3 = ggplot(r8_top, aes(x = Year, fill = drug_bug2, y = Prop_est * 100)) +
  geom_alluvium(aes(alluvium = drug_bug2), alpha = 1) +
  scale_fill_manual(name = " ", values = value_colors) +
  labs(
    x = "",
    y = 'Percentage (%)',
    title = "Most frequent resistance-pathogen \ncombinations across all pathogens \nidentifed in BSI isolates",
  ) +
  facet_wrap(.~ Region, ncol=7)+
  scale_x_continuous(breaks = c(2018, 2020, 2022)) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),  # Center the subtitle
    axis.text.x = element_text(size = 28),
    axis.text.y = ggtext::element_markdown(size = 28),
    strip.text = element_text(size = 28, face = "bold"),  # Strip text for region names
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 9))
p3

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8c_pathogen_distr_bsi_FORLEGEND_crude_UPDATE.svg"), 
       plot = p3, 
       device="svg",
       width = 38, height = 10)

write.csv(r8_top, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8c_Table_Pathogen_resistance_crude_UPDATED.csv"))
write.csv(d7, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8ab_Table_Pathogen_distr_crude_UPDATED.csv"))



unique(r8_top$drug_bug)


# TO SHARE WITH COUNTRIES/REGIONS
#----------------------------------------------------
# SHARE WITH REGIONS
d = read.csv(paste0(dirOutputReport,"/Chapter 4/Ch4 summary stats/CTAprevalence_prioritypathogen2023.csv"))
d$combined[d$combined== "BLOOD-Escherichia coli-Third generation cephalosporins"] =  "BLOOD-Escherichia coli-Third-generation cephalosporins"
da = adataAC %>%filter(Year==2023, InterpretableAST>10)%>%select(Iso3, Year, combined, Susceptible, Intermediate,
                                                                 Resistant, InterpretableAST, UninterpretableAST) 

dg = left_join(d%>%filter(Year==2023),da, by=c("Year", "Iso3", "combined"))

dg1 = dg %>% filter(Year==2023&!is.na(amr_rate)) 
dg2 = left_join(dg1, cdata%>%select(Iso3, CountryTerritoryArea))

dg3 = dg2 %>% select(WHORegionName, CountryTerritoryArea, Year, Specimen, PathogenName2, AntibioticName, 
                     Grouping,
                     Susceptible, Intermediate, Resistant, InterpretableAST, UninterpretableAST, amr_rate,
                     w_prev, w_prev_lower, w_prev_upper) %>%
  mutate(
    amr_rate = round(amr_rate*100,1),
    w_prev = round(w_prev*100,1),
    w_prev_lower = round(w_prev_lower*100,1),
    w_prev_upper = round(w_prev_upper*100,1),
    estimated_w_prev_ci = sprintf("%.1f \n(%.1f–%.1f)", w_prev, w_prev_lower, w_prev_upper)) %>%
  select(-c(w_prev, w_prev_upper, w_prev_lower)) %>%
  rename(
    crude_prev = "amr_rate"
  )

dg3$AntibioticName[dg3$AntibioticName%in%c("Third-generation cephalosporins","Methicillin-resistance")] = paste0(dg3$AntibioticName[dg3$AntibioticName%in%c("Third-generation cephalosporins","Methicillin-resistance")],"*")
write.xlsx(dg3, "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING//TO_SHARE_WITH_REGIONS/cta_prev_estimates_prioritypathogens_2023.xlsx")

# REGIONAL PREVALENCES
#--------------------------------------------------
d = read.csv(paste0(dirOutputReport,"/Chapter 4/Ch4 summary stats/Figure_4.1_4.2_4.3_REGIONALprevalence2023.csv"))
d_sdg = read.csv(paste0(dirOutputReport,"/Chapter 4/Ch4 summary stats/Figure_4.2a_REGIONALprevalence2023_SDG.csv"))

d1 = rbind(d,d_sdg)

d2 = d1 %>%
  mutate(
    w_prev = round(regional_weighted_mean,1),
    w_prev_lower = round(Q2.5,1),
    w_prev_upper = round(Q97.5,1),
    estimated_w_prev_ci = sprintf("%.1f \n(%.1f–%.1f)", w_prev, w_prev_lower, w_prev_upper)) %>%
  select(-c(Region, drug_bug)
  )

d2$AntibioticName[d2$AntibioticName%in%c("Third-generation cephalosporins","Methicillin-resistance")] = paste0(d2$AntibioticName[d2$AntibioticName%in%c("Third-generation cephalosporins","Methicillin-resistance")],"*")

write.xlsx(d2, "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING//TO_SHARE_WITH_REGIONS/REGIONALprevalence_allpathogens_2023.xlsx")

# CHECK UK
#------------------------------------------------------------------

prev = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))

d = prev%>%filter(Iso3=="GBR",combined%in%pp)

dc = prev%>%filter(Iso3%in%c("GBR", "NLD", "SWE", "DEU", "DNK", "NOR", "FIN"),combined%in%pp)

antibiotic_colors <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#66c2a5", "#fc8d62", "#a6d854", "#e78ac3", "#ffd92f"
)

comparable_country_colors <- c(
  "GBR" = "#000000",  # Black (UK – reference)
  "NLD" = "#E41A1C",  # Red
  "SWE" = "#377EB8",  # Blue
  "DEU" = "#4DAF4A",  # Green
  "DNK" = "#984EA3",  # Purple
  "NOR" = "#FF7F00",  # Orange
  "FIN" = "#A65628",  # Brown
  "IRL" = "#F781BF"   # Pink
)



p = ggplot(d,aes(Year, y=w_prev*100, colour=combined)) + 
  #geom_ribbon(aes(ymin=w_prev_lower*100, ymax=w_prev_upper*100, fill=AntibioticName, alpha=0.1), linetype=2)+ 
    geom_point() + geom_line() +
 facet_wrap(~ Specimen+PathogenName + AntibioticName, ncol=5)+
  labs(y="Percentage resistant")+
  theme_minimal() +
  ylim(0,100) +
  scale_color_manual(values=antibiotic_colors)+
  theme(strip.text = element_text(size = 10),  # Strip text for region names
        panel.grid.major = element_line(linetype = "dotted"),
        panel.grid.minor = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "white", colour = "white"))+
  guides(colour=F, fill=F, alpha=F)
p

p_com = ggplot(dc,aes(Year, y=w_prev*100, colour=Iso3)) + 
  #geom_ribbon(aes(ymin=w_prev_lower*100, ymax=w_prev_upper*100, fill=AntibioticName, alpha=0.1), linetype=2)+ 
  geom_point(size=4) + geom_line(linewidth=1.2) +
  facet_grid(Specimen+PathogenName + AntibioticName~Iso3)+
  labs(y="Percentage resistant")+
  theme_minimal() +
  ylim(0,100) +
  scale_color_manual(values=comparable_country_colors)+
  theme(strip.text = element_text(size = 10),  # Strip text for region names
        panel.grid.major = element_line(linetype = "dotted"),
        panel.grid.minor = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom")+
  guides(fill=F, alpha=F)
p_com

d_share = d %>% select(Year, Specimen, PathogenName, AntibioticName, w_prev,
                       w_prev_lower, w_prev_upper)
ggsave(filename = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/TO_SHARE_WITH_REGIONS/UK_prevalence_overtime.png", 
       plot = p, 
       device="png",
       width = 12, height = 10)

ggsave(filename = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/TO_SHARE_WITH_REGIONS/Comp_prevalence_overtime.png", 
       plot = p_com, 
       device="png",
       width = 12, height = 20)

# EXECUTIVE SUMMARY FIGURES
#-----------------------------------------------------------------

# FIGURE E1 - Number of CTA reporting >0 isolates with AST

# CREATE DATASET
# Extract distinct country-year-region entries where AMR was reported
reported <- adataAC_crude %>%
  filter(InterpretableAST > 0) %>%
  distinct(Iso3, Year, WHORegionName)

years <- sort(unique(reported$Year))

# Cumulative count of unique countries per region over time
d_region <- map_dfr(years, function(y) {
  reported %>%
    filter(Year <= y) %>%
    distinct(Iso3, WHORegionName) %>%
    group_by(WHORegionName) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Year = y)
})

# Global total (sum across all regions)
d_global <- d_region %>%
  group_by(Year) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(WHORegionName = "Global")

d <- bind_rows(d_region, d_global)

dc = cdata %>%
  group_by(WHORegionName) %>%
  summarise(n_total=length(unique(Iso3)))

#dc$n_total[dc$WHORegionName=="Region of the Americas"] = 35
#dc$n_total[dc$WHORegionName=="European Region"] = 53
#dc$n_total[dc$WHORegionName=="Eastern Mediterranean Region"] = 21
#dc$n_total[dc$WHORegionName=="Western Pacific Region"] = 27

dc_global = cdata %>%
  summarise(n_total=length(unique(Iso3))) %>%
  mutate(WHORegionName = "Global") %>%
  select(WHORegionName, n_total)

#dc_global$n_total = 194 # when excluding territories

dc = rbind(dc,dc_global)

d2 = left_join(d,dc) %>%
  mutate(WHORegionName = factor(WHORegionName, 
                         levels = c("African Region",
                                    "Region of the Americas",
                                    "South-East Asia Region",
                                    "European Region",
                                    "Eastern Mediterranean Region",
                                    "Western Pacific Region",
                                    "Global"))
  )         

d3 <- d2 %>%
  group_by(WHORegionName) %>%
  arrange(Year) %>%
  mutate(diff = n - lag(n)) %>%
  ungroup()

# Create a label column with n/ntotal
# Labels for regions: compute cumulative position for label
reversed_levels <- rev(levels(d3$WHORegionName))

label_data <- d3 %>%
  filter(WHORegionName != "Global") %>%
  group_by(Year) %>%
  arrange(Year, factor(WHORegionName, levels=reversed_levels)) %>%
  mutate(
    ymin = cumsum(n) - n,
    ymax = cumsum(n),
    y_label = ymax,
    label = paste0(n, "/", n_total),
    label2 = n,
    label3 = round(n/n_total*100,1)
  ) %>%
  ungroup()

# Labels for Global: just nudge above line
global_label_data <- d3 %>%
  filter(WHORegionName == "Global") %>%
  mutate(
    y_label = n + 10,
    label = paste0(n, "/", n_total),
    label2 = n,
    label3 = round(n/n_total*100,1)
  )
label_data_all = rbind(label_data%>%select(-c(ymin,ymax)),global_label_data)

facet_colors3["Global"] = "grey40"

# Plot OPTION #1
# USE THIS
p1a = ggplot(d3, aes(x = Year, y = n, color = WHORegionName)) +
  geom_line(size=2) +
  geom_point(size=5, shape=15) + 
  #scale_alpha_manual(values=c(0.6,0.6,0.6,0.6,0.6,0.6,1))+
  scale_color_manual(values = facet_colors3) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
  scale_x_continuous(limits = c(2016,2023), breaks = c(2016, 2017, 2018, 2019, 2020,2021, 2022,2023)) +
  labs(
    x = "",
    y = 'Number of countries',
    title = " ",
    color = "",  
    fill = "Region"
  ) +
  # Label at 2023, nudged to the right
  # geom_text_repel(
  #   data = label_data_all %>% filter(Year == 2023),
  #   aes(x = Year, y = n, label = paste0(label, " (", label3, "%)")),
  #   hjust = 0,
  #   nudge_x = 0.5,         
  #   direction = "y",       
  #   segment.color = "grey80", 
  #   size = 5,
  #   inherit.aes = FALSE
  # ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(nrow = 3),
         alpha=F)
p1a

# OPTION #2
p1b = ggplot(d3 %>% filter(WHORegionName!="Global"), aes(x = Year, y = n, color = WHORegionName)) +
  geom_line(size=2) +
  geom_point(size=3, shape=15) + 
  #scale_alpha_manual(values=c(0.6,0.6,0.6,0.6,0.6,0.6))+
  scale_color_manual(values = facet_colors3) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 50, 10)) +
  scale_x_continuous(limits = c(2016,2024), breaks = c(2016, 2017, 2018, 2019, 2020,2021, 2022,2023)) +
  labs(
    x = "",
    y = 'Number of countries',
    title = " ",
    color = "",  
    fill = "Region"
  ) +
  # Label at 2023, nudged to the right
  geom_text_repel(
    data = label_data_all %>% filter(Year == 2023),
    aes(x = Year, y = n, label = paste0(label, " (", label3, "%)")),
    hjust = 0,
    nudge_x = 0.5,
    direction = "y",
    segment.color = "grey80",
    size = 5,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    #legend.position = c(0.6, 0.92),       # lower right inside the plot
    #legend.justification = c(1, 0),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 3),
         alpha=F)
p1b

p1b_g = ggplot(d3 %>% filter(WHORegionName=="Global"), aes(x = Year, y = n, color = WHORegionName)) +
  geom_line(size=2) +
  geom_point(size=3, shape=15) + 
  #scale_alpha_manual(values=c(0.6,0.6,0.6,0.6,0.6,0.6))+
  scale_color_manual(values = facet_colors3) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
  scale_x_continuous(limits = c(2016,2024), breaks = c(2016, 2017, 2018, 2019, 2020,2021, 2022,2023)) +
  labs(
    x = "",
    y = 'Number of countries',
    title = " ",
    color = "",  
    fill = "Region"
  ) +
  # Label at 2023, nudged to the right
  geom_text_repel(
    data = label_data_all %>% filter(Year == 2023, WHORegionName=="Global"),
    aes(x = Year, y = n, label = paste0(label, " (", label3, "%)")),
    hjust = 0,
    vjust=-0.7,
    nudge_x = 0.5,         #
    direction = "y",
    segment.color = "grey80",
    size = 5,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    plot.subtitle = element_text(size = 30, hjust = 0.5),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    #legend.position = c(0.6, 0.92),       # lower right inside the plot
    #legend.justification = c(1, 0),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 3),
         alpha=F)
p1b_g

combined =p1b_g + p1b + plot_layout(ncol=2)
combined

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_1_glass_participation_together.png"), 
       plot =  p1a, 
       device="png",
       width = 8, height = 8)

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_1_glass_participation_global_sep.png"), 
       plot =  combined, 
       device="png",
       width = 17, height = 8)

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Final/Fig_1_glass_participation_global_sep.svg"), 
       plot =  combined, 
       device="svg",
       width = 17, height = 8)

write.csv(d3, paste0(dirOutputReport, "/Executive_sum/Exc summary stats/Exc_fig_1_glass_participation_global_sep.csv"))

d4 = d3 %>%
  mutate(WHORegionName = factor(WHORegionName, levels =c(
    "Western Pacific Region",
    "Eastern Mediterranean Region",
    "European Region",
    "South-East Asia Region",
    "Region of the Americas",
    "African Region",
    "Global")))

# p1c = ggplot(d4, 
#              aes(x = WHORegionName, y = n, fill = WHORegionName, alpha = factor(Year))) +
#   geom_col(position = position_dodge(width = 0.8), width = 0.7) +
#   
#   # Add labels only for most recent year
#   geom_text(
#     data = label_data_all %>% filter(Year == max(Year)),
#     aes(y=n,label = paste0(label," (", label3, "%)")),
#     position = position_dodge(width = 0.8),
#     vjust = -2,
#     hjust = -0.1,
#     color = "black",
#     size = 5
#   ) +
#   scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 20)) +
#   coord_flip() +
#   scale_fill_manual(values = facet_colors3) +
#   #scale_fill_viridis_d(option = "D", direction = -1) +
#   labs(
#     y = "Number of countries",
#     x = '',
#     fill = ""
#   ) +
#   theme_minimal() +
#   theme(
#     legend.text = element_text(size = 15),
#     plot.background = element_rect(fill = "white", colour = "white"),
#     title = element_text(size = 34),
#     plot.subtitle = element_text(size = 30, hjust = 0.5),
#     axis.title.x = element_text(size = 20),
#     axis.text.y = element_text(size = 20),
#     axis.text.x = ggtext::element_markdown(size = 20),
#     panel.grid.major = element_line(linetype = "dotted"),
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   ) +
#   guides(alpha =F,
#          fill = F)
# p1c


# FIGURE E2 - BARPLOTS OF ALL AMR RESISTANCES
r = read.csv(file.path(dirDataModeloutput, "Model_prevalence_estimates/Data_tables_prevalence/REGION_w_prev_all.csv"))

drug_bug = r$drug_bug
r$combined = gsub("Co trimoxazole","Co-trimoxazole", r$drug_bug)
r$combined = gsub("Methicillin resistance","Methicillin-resistance", r$combined)
r$combined = gsub("Third generation cephalosporins","Third-generation cephalosporins", r$combined)

#r$AntibioticName[r$AntibioticName=="Co"] = "Co-trimoxazole"
#r$AntibioticName[r$AntibioticName=="Third generation cephalosporins"] = "Third-generation cephalosporins"
r1 = r %>%
  mutate(
    drug_bug = combined
  )

r1 = r1 %>% filter(combined!="BLOOD-Escherichia coli-Ampicillin")

a = adataAC %>% select(AntibioticName, Antibiotic, Pathogen, PathogenName) %>%
  distinct() %>% 
  filter(!(AntibioticName == "Third-generation cephalosporins" & Antibiotic !="J01DD")&
           !(AntibioticName == "Co-trimoxazole" & Antibiotic =="J01EE")&
           !(AntibioticName == "Methicillin resistance" & Antibiotic !="J01DC"))

r1 = left_join(r,a)

r1$drug_bug = gsub("Co trimoxazole","Co-trimoxazole", r1$drug_bug)
r1$drug_bug = gsub("Methicillin resistance","Methicillin-resistance", r1$drug_bug)
r1$drug_bug = gsub("Third generation cephalosporins","Third-generation cephalosporins", r1$drug_bug)


#r1$AntibioticName[r1$Antibiotic=="COL"] = "Colistin"
r1$AntibioticName = gsub("Co trimoxazole","Co-trimoxazole", r1$AntibioticName)

r1$AntibioticName = gsub("Methicillin resistance","Methicillin-resistance", r1$AntibioticName)
r1$AntibioticName = gsub("Third generation cephalosporins","Third-generation cephalosporins", r1$AntibioticName)

r2 = r1 %>%
  mutate(Grouping = case_when((AntibioticName =="J01DH" | Antibiotic =="ETP" | Antibiotic =="DOR" | Antibiotic =="IPM" | Antibiotic =="MEM") ~ 'Carbapenems',
                              (Antibiotic =="J01DD" | Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" | Antibiotic =="CFM") ~ '3rd-gen. cephalosporins',
                              (Antibiotic =="FEP") ~ '4th-gen. cephalosporins',
                              (Antibiotic =="J01MA" | Antibiotic =="CIP" | Antibiotic =="LVX") ~ 'Fluoroquinolones',
                              (Antibiotic =="J01FA" | Antibiotic =="AZM") ~ 'Macrolides',
                              #(Antibiotic =="J01DC" | Antibiotic =="OXA"| Antibiotic =="FOX"| AntibioticName == "Methicillin-resistance") ~ NA,
                              (Antibiotic =="J01C" | Antibiotic =="PEN"| Antibiotic =="AMP"| Antibiotic =="OXA") ~ 'Penicillins',
                              (Antibiotic =="J01EE" | Antibiotic =="SXT" | AntibioticName == "Co-trimoxazole" ) ~ 'TMP-sulfa',
                              (Antibiotic =="AMK" | Antibiotic =="GEN"|Antibiotic=="SPT") ~ 'Aminoglycocides',
                              (Antibiotic == "COL") ~ 'Polymyxins',
                              (Antibiotic == "MNO" | Antibiotic == "TGC") ~ 'Tetracyclines',
                              (AntibioticName=="Third-generation cephalosporins"| AntibioticName == "Methicillin-resistance") ~ "SDG"
  ),
  Grouping = factor(Grouping, levels=c("Aminoglycocides", "Carbapenems", "3rd-gen. cephalosporins", "4th-gen. cephalosporins",
                                       "Fluoroquinolones", "Macrolides","Penicillins", "Polymyxins", 
                                       "TMP-sulfa", "Tetracyclines","SDG")),
  AntibioticName = factor(AntibioticName, levels= c("Gentamicin","Amikacin","Spectinomycin","Ertapenem",
                                                    "Imipenem", "Meropenem", "Doripenem", "Ceftriaxone","Cefotaxime","Ceftazidime",
                                                    "Cefixime",  "Cefepime", "Ciprofloxacin","Levofloxacin", "Azithromycin",
                                                    "Oxacillin",   "Penicillin G","Ampicillin", "Colistin",
                                                    "Co-trimoxazole",  "Minocycline" , "Tigecycline", "Methicillin-resistance", "Third-generation cephalosporins")),
  PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                  "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae",
                                                  "Shigella spp.","Neisseria gonorrhoeae"),
                         labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                    "Salmonella spp.", "S. aureus", "S. pneumoniae","Shigella spp.","N. gonorrhoeae"))
  )
unique(r2$AntibioticName[is.na(r2$Grouping)])

# 

table(r2$AntibioticName,r2$Grouping)
table(r2$AntibioticName, useNA="always")
table(r2$AntibioticName, useNA="always")

#View(r2 %>% filter(is.na(AntibioticName)))

region = cdata %>% 
  ungroup() %>% select(WHORegionCode, WHORegionName) %>%
  distinct()

r3 = r2 %>% 
  left_join(region) %>%
  mutate(
    WHORegionName = ifelse(is.na(WHORegionName), "Global", WHORegionName)
  )


r4 = r3 %>%
  dplyr::select(WHORegionName, Year, regional_weighted_mean, Q2.5, Q97.5, Specimen,
                PathogenName, PathogenName2, AntibioticName, Grouping, drug_bug, n_countries, n_isolates) 

r5 = r4 %>%
  mutate(Region = factor(WHORegionName)
  )%>%
  mutate(
    Region= factor(Region,levels = c(
      "Global",
      "African Region",
      "Region of the Americas",
      "South-East Asia Region",
      "European Region",
      "Eastern Mediterranean Region", 
      "Western Pacific Region"
    ))
  )

r5 = r5 %>%
  mutate(
    n_cutoff = ifelse(Region=="Global"& n_countries<10, "No",
                      ifelse(Region!="Global" & n_countries<5, "No", "Yes"))
  )

r6 = r5%>%filter(Year==2023)

# LINK WITH AWARE CLASSIFICATION
aware = read.csv(paste0(dirDataNewO, "/ListOfAntibiotics_Aware.csv"), sep=",")       # Population data
aware$AntibioticName[aware$AntibioticName=="Cefotaxime "] = "Cefotaxime"

unique(r6$AntibioticName)
unique(aware$AntibioticName)

r6 = left_join(r6, aware, by="AntibioticName")
table(r6$Aware_Class, useNA="always")

r6$Aware_Class[r6$AntibioticName=="Methicillin-resistance"] = "WATCH"
r6$Aware_Class[r6$AntibioticName=="Third-generation cephalosporins"] = "WATCH"

unique(r6$drug_bug)
r6 = r6 %>% filter(drug_bug!="URINE-Escherichia coli-Ampicillin") # No longer reported from 2022 onwards

# PLOT
# Define pathogen shapes
pathogen_shapes <- c(
  "Acinetobacter spp." = 15,
  "E. coli" = 16,
  "K. pneumoniae" = 17,
  "Salmonella spp." = 18,
  "S. aureus" = 3,
  "S. pneumoniae" = 4,
  "Shigella spp." =  23,
  "N. gonorrhoeae" = 11
)

# Define specimen colors
specimen_colors <- c(
  "BLOOD" = "brown1",
  "URINE" = 'burlywood1',
  "UROGENITAL" = "cadetblue3",
  "STOOL" = "darkred")

# Define AWARE class colors using CAPITAL letters
aware_colors <- c(
  "ACCESS" = "seagreen",   # Softer green
  "WATCH" =  "#F5C719",   # Muted yellow
  "RESERVE" = "#DF536B"   # Coral red
)

base_pathogen_colors <- c(
    "Acinetobacter spp." = "maroon",
    "E. coli" =  "#a6cee3",
    "K. pneumoniae" = "#1f78b4",
    "Salmonella spp." = "#33a02c",
    "S. aureus" = "#b2df8a",
    "S. pneumoniae" =  "#fb9a99",
    "Shigella spp." =  "peachpuff1",
    "N. gonorrhoeae" = "darkorange2"
  )

# Convert to markdown-style italic names
pathogen_colors <- setNames(base_pathogen_colors, 
                            ifelse(grepl("spp\\.", names(base_pathogen_colors)),
                                   paste0("*", gsub(" spp\\.", "", names(base_pathogen_colors)), "* spp."),
                                   paste0("*", names(base_pathogen_colors), "*")))


# Prepare data
plot_data <- r6 %>%
  filter(WHORegionName == "Global") %>%
  mutate(
    Specimen = factor(Specimen, levels = c("BLOOD", "URINE", "UROGENITAL", "STOOL")),
    PathogenFormatted = ifelse(
      grepl("spp\\.", PathogenName2),
      paste0("*", gsub(" spp\\..*", "", PathogenName2), "* spp."),
      paste0("*", PathogenName2, "*")
    ),
    drug_bug_full = paste0(PathogenFormatted, " - ", AntibioticName),
    drug_bug_combined = paste0(Specimen, "@", PathogenFormatted, " - ", AntibioticName)
  ) %>%
  arrange(Specimen, regional_weighted_mean) %>%
  group_by(Specimen) %>%
  mutate(
    Specimen = factor(Specimen, levels = c("BLOOD", "STOOL", "URINE", "UROGENITAL")),
    Aware_Class = factor(Aware_Class, levels = c("ACCESS", "WATCH", "RESERVE")),
    # Create y-axis factor with correct order per facet
    drug_bug_combined_facet = factor(drug_bug_combined, levels = unique(drug_bug_combined)),
    # Clean label to display (remove Specimen prefix)
    drug_bug_combined_label_facet = sub(".*@", "", as.character(drug_bug_combined))
  )

# Create plot
p2 <- ggplot(plot_data, aes(x = regional_weighted_mean, y = drug_bug_combined_facet)) +
  # Specimen background tile
  geom_tile(aes(x = -12, fill = Specimen), width = 8, height = 1, alpha = 0.5) +
  scale_fill_manual(
    values = specimen_colors,
    name = "Infection type",
    labels = c("BLOOD" = "Bloodstream", "STOOL" = "Gastrointestinal",
               "URINE" = "Urinary tract", "UROGENITAL" = "Urogenital")
  ) +
  ggnewscale::new_scale_fill() +
  
  # Main bar
  geom_col(aes(fill = Aware_Class), width = 0.8, alpha = 0.8) +
  geom_errorbar(
    aes(xmin = Q2.5, xmax = Q97.5),
    width = 0
  ) +
  scale_fill_manual(
    values = aware_colors,
    labels = c("ACCESS" = "Access", "WATCH" = "Watch", "RESERVE" = "Reserve"),
    name = "AWaRe class", na.translate = FALSE
  ) +
  ggnewscale::new_scale_fill() +
  
  # Pathogen tile
  geom_tile(aes(x = -4, fill = PathogenName2), width = 7, height = 1, alpha = 0.5) +
  scale_fill_manual(
    values = base_pathogen_colors,
    labels = names(pathogen_colors),
    name = "Pathogen"
  ) +
  
  labs(x = "Resistance (%)", y = "", fill = "", shape = "Pathogen") +
  
  # Facet per specimen with individual y-axes
  facet_grid(Specimen ~ ., scales = "free_y", space = "free_y") +
  
  # Custom labels per facet
  scale_y_discrete(labels = function(x) {
    label_map <- setNames(plot_data$drug_bug_combined_label_facet, plot_data$drug_bug_combined_facet)
    label_map[x]
  }) +
  
  scale_x_continuous(limits = c(-16, 100), breaks = seq(0, 100, 10)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_markdown(size = 12),
    legend.text = element_markdown(size = 15),
    legend.title = element_text(face = "bold", size = 15),
    legend.position = c(0.95, 0.08),
    legend.justification = c(1, 0),
    axis.text.x = element_markdown(size = 18),
    axis.title.x = element_markdown(size = 18),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    strip.text = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0, "lines"),
    plot.background = element_rect(fill = "white", colour = "white")
  )
p2

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_2_resistance_order_bySpecimen.png"), 
       plot =  p2, 
       device="png",
       width = 15, height =17)

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Final/Fig_2_resistance_order_bySpecimen.svg"), 
       plot =  p2, 
       device="png",
       width = 15, height =17)

write.csv(r6, paste0(dirOutputReport, "/Executive_sum/Exc summary stats/Exc_fig_2_amr_bsi_all_global.csv"))

# NOW BY REGION
#-------------------------------------------------------------------------

region_names <- unique(r6$WHORegionName)
region_plots <- map(region_names, ~plot_amr_by_region_ex(data = r6,
                                                         region_name =.x,
                                                         specimen_colors = specimen_colors,
                                                         aware_colors = aware_colors,
                                                         base_pathogen_colors = base_pathogen_colors))

# View the plot for the first region (e.g., Global)
region_plots[[1]]

combined = region_plots[[1]] + region_plots[[2]] + region_plots[[3]] + region_plots[[4]] + 
  region_plots[[5]] + region_plots[[6]]+ plot_layout(ncol = 3)

combined

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_2_resistance_order_bySpecimen_regions.png"), 
       plot =  combined, 
       device="png",
       width = 40, height =30)

# FIGURE ANNEX 1
p1 <- plot_amr_test(adataAC, "Escherichia coli", "BLOOD", facet_colors3, xaxis="st_BCI_million")
p1


ggsave(filename = paste0(dirOutputReport, "/Annexes/Final/Fig_Annex1_AMR_vs_surveillancecoverage.svg"), 
       plot =  p1, 
       device="svg",
       width = 12, height =10)

# FIGURE E3
#----------------------------------------------
d = read.csv("C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/FINAL 2023 FIGURES TABLES AND RESULTS/Executive_sum/Exc summary stats/median_bsi_all_res_OLGA.csv")
r = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))
uhc = read.csv(paste0(dirDataNewO, "/UHC_Index_data.csv"), sep=",")       # Population data

# GET DATA IN RIGHT FORMAT
d1 <- d %>%
  mutate(Region_Full = recode(Region,
                              "AFR" = "African Region",
                              "AMR" = "Region of the Americas",
                              "SEAR" = "South-East Asia Region",
                              "EUR" = "European Region",
                              "EMR" = "Eastern Mediterranean Region",
                              "WPR" = "Western Pacific Region"
  ))
# Set factor levels to control legend order
d1 <- d1 %>%
  mutate(
    Region_Full = factor(Region_Full, levels = names(facet_colors3)),
    c_UHC = case_when(
      UHC_Index < 40 ~ "Low",
      UHC_Index >= 40 & UHC_Index < 60 ~ "Medium",
      UHC_Index >= 60 & UHC_Index < 80 ~ "High",
      UHC_Index >= 80 ~ "Very high",
      TRUE ~ NA_character_  
    )
  )



d2 = left_join(d1, cdata)


income_colors <- c(
  "Low income" = "#990000",
  "Lower middle income" = "#e97452",
  "Upper middle income" = "#f4c7a1",
  "High income" = "#3182bd"
)

d2$IncomeWorldBankJune2023_label <- case_when(
  d2$IncomeWorldBankJune2023 == "HighIncome" ~ "High income",
  d2$IncomeWorldBankJune2023 == "UpperMiddleIncome" ~ "Upper middle income",
  d2$IncomeWorldBankJune2023 == "LowerMiddleIncome" ~ "Lower middle income",
  d2$IncomeWorldBankJune2023 == "LowIncome" ~ "Low income",
  TRUE ~ NA_character_  # Catch any unmatched cases
)

d2$IncomeWorldBankJune2023_label = factor(d2$IncomeWorldBankJune2023_label, levels=c("Low income",
                                                                                     "Lower middle income",
                                                                                     "Upper middle income",
                                                                                     "High income"))


# NOW CHECK WITH ORIGINAL DATA
r1 = r %>% filter(Year==2023, data_submitted2023=="Yes")

br2 = r1 %>% filter(Specimen=="BLOOD", combined!="BLOOD-Escherichia coli-Third generation cephalosporins") %>%
  group_by(WHORegionCode, Iso3, combined, w_prev, InterpretableAST)

br3 = br2 %>%
  ungroup()%>%
  group_by(WHORegionCode, Iso3) %>%
  summarise(
    crude_m = median(w_prev, na.rm=T)*100,
    weighted_m = weightedMedian(w_prev, w=InterpretableAST, na.rm=T)*100,
    InterpretableAST = sum(InterpretableAST, na.rm=T)
  )

ch = unique(br3$Iso3)
ch2 = unique(d$Iso3)
ch[which(!ch%in%ch2)] # Following are missing "PSE" "XKX" "HKG"

br4 = left_join(br3, uhc)

br5 <- br4 %>%
  mutate(Region_Full = recode(WHORegionCode,
                              "AFR" = "African Region",
                              "AMR" = "Region of the Americas",
                              "SEAR" = "South-East Asia Region",
                              "EUR" = "European Region",
                              "EMR" = "Eastern Mediterranean Region",
                              "WPR" = "Western Pacific Region"),
         Region_Full = factor(Region_Full, levels = names(facet_colors3)),
         c_UHC = case_when(
           UHC_Index < 40 ~ "Low",
           UHC_Index >= 40 & UHC_Index < 60 ~ "Medium",
           UHC_Index >= 60 & UHC_Index < 80 ~ "High",
           UHC_Index >= 80 ~ "Very high",
           is.na(UHC_Index) ~ "Unclassified",
           TRUE ~ NA_character_)
         )

br6 = left_join(br5, cdata)

br6$IncomeWorldBankJune2023_label <- case_when(
  br6$IncomeWorldBankJune2023 == "HighIncome" ~ "High income",
  br6$IncomeWorldBankJune2023 == "UpperMiddleIncome" ~ "Upper middle income",
  br6$IncomeWorldBankJune2023 == "LowerMiddleIncome" ~ "Lower middle income",
  br6$IncomeWorldBankJune2023 == "LowIncome" ~ "Low income",
  TRUE ~ NA_character_  # Catch any unmatched cases
)

br6$IncomeWorldBankJune2023_label = factor(br6$IncomeWorldBankJune2023_label, levels=c("Low income",
                                                                                     "Lower middle income",
                                                                                     "Upper middle income",
                                                                                     "High income"))

c_UHC = data.frame(cbind(cat = c("Medium", "High", "Very high"), cut_off = as.numeric(c(40,60,80))))
c_UHC$cut_off = as.numeric(c_UHC$cut_off)

# Differences crude vs weighted
br6%>% group_by(IncomeWorldBankJune2023_label) %>%
  summarise(crude = median(crude_m),
            weighted = median(weighted_m),
            diff_rel = (weighted-crude)/crude*100,
            diff_abs = weighted-crude)


# Create the plot - OLGA's data 
p3 = ggplot(d2, aes(x = UHC_Index, y = Mymed, color = IncomeWorldBankJune2023_label)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", show.legend = FALSE,size=0.8) +
  geom_point(aes(alpha = 0.9), size = 6, shape=19) +
  scale_color_manual(values = income_colors) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed", show.legend = FALSE) +
  theme_classic() +
  labs(
    #title = "Bloodstream infections",
    #subtitle = "Median country-level resistance across all pathogen-antibiotic combinations",
    x = "Universal Health Coverage Index",
    y = "Median \npercentage resistance (%)",
    color = NULL
  ) +
  scale_x_continuous(limits = c(30, 82), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  #geom_vline(xintercept = c_UHC$cut_off, linetype = "dotted", color = "grey40")+
  #annotate("text", x = 40-1, y = 4, label = "Low \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 60-1, y = 4, label = "Medium \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 80-1, y = 4, label = "High \nUHC", vjust = 1.2, hjust=1, size = 5) +
  # ggrepel::geom_text_repel(
  #   aes(label = Iso3),
  #   size = 3,
  #   max.overlaps = 30,       # Increase if you want more labels shown
  #   force = 1,               # Controls push-back from other labels (higher = more separation)
  #   point.padding = 0.2,     # Space around points
  #   segment.color = "black",# Line color linking text to point
  #   show.legend = FALSE
  # )+
theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = c(0.49, 0.85),       # lower right inside the plot
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         size=F, alpha=F)

p3


# Create the plot - USE THIS FOR REPORT
p3a = ggplot(br6, aes(x = UHC_Index, y = crude_m, color = IncomeWorldBankJune2023_label)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", show.legend = FALSE,size=0.8) +
  geom_point(aes(alpha = 0.9), size = 6, shape=19) +
  scale_color_manual(values = income_colors) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed", show.legend = FALSE) +
  theme_classic() +
  labs(
    #title = "Bloodstream infections",
    #subtitle = "Median country-level resistance across all pathogen-antibiotic combinations",
    x = "Universal Health Coverage Index",
    y = "Median \npercentage resistance (%)",
    color = NULL
  ) +
  scale_x_continuous(limits = c(30, 82), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  #geom_vline(xintercept = c_UHC$cut_off, linetype = "dotted", color = "grey40")+
  #annotate("text", x = 40-1, y = 4, label = "Low \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 60-1, y = 4, label = "Medium \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 80-1, y = 4, label = "High \nUHC", vjust = 1.2, hjust=1, size = 5) +
  # ggrepel::geom_text_repel(
  #   aes(label = Iso3),
  #   size = 3,
  #   max.overlaps = 30,       # Increase if you want more labels shown
  #   force = 1,               # Controls push-back from other labels (higher = more separation)
  #   point.padding = 0.2,     # Space around points
  #   segment.color = "black",# Line color linking text to point
  #   show.legend = FALSE
  # )+
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = c(0.49, 0.85),       # lower right inside the plot
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         size=F, alpha=F)

p3a


# Create the plot
p3a_sizediff = ggplot(br6, aes(x = UHC_Index, y = crude_m, color = IncomeWorldBankJune2023_label)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", show.legend = FALSE,size=0.8) +
  geom_point(aes(alpha = 0.9, size = InterpretableAST), shape=19) +
  scale_color_manual(values = income_colors) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed", show.legend = FALSE) +
  theme_classic() +
  labs(
    #title = "Bloodstream infections",
    #subtitle = "Median country-level resistance across all pathogen-antibiotic combinations",
    x = "Universal Health Coverage Index",
    y = "Median \npercentage resistance (%)",
    color = NULL
  ) +
  scale_x_continuous(limits = c(30, 82), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  #geom_vline(xintercept = c_UHC$cut_off, linetype = "dotted", color = "grey40")+
  #annotate("text", x = 40-1, y = 4, label = "Low \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 60-1, y = 4, label = "Medium \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 80-1, y = 4, label = "High \nUHC", vjust = 1.2, hjust=1, size = 5) +
  # ggrepel::geom_text_repel(
  #   aes(label = Iso3),
  #   size = 3,
  #   max.overlaps = 30,       # Increase if you want more labels shown
  #   force = 1,               # Controls push-back from other labels (higher = more separation)
  #   point.padding = 0.2,     # Space around points
  #   segment.color = "black",# Line color linking text to point
  #   show.legend = FALSE
  # )+
  scale_size_continuous(range = c(3, 14))+
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = c(0.49, 0.85),       # lower right inside the plot
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         size=F, alpha=F)

p3a_sizediff

# YES THE ABOVE TWO PLOTS ARE THE SAME, only SGP seems to miss in Olga's dataset


# NOW WITH WEIGHTED MEDIAN
p3b = ggplot(br6, aes(x = UHC_Index, y = weighted_m, color = IncomeWorldBankJune2023_label)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", show.legend = FALSE,size=0.8) +
  geom_point(aes(alpha = 0.9, size = InterpretableAST), shape=19) +
  scale_color_manual(values = income_colors) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed", show.legend = FALSE) +
  theme_classic() +
  labs(
    #title = "Bloodstream infections",
    #subtitle = "Median country-level resistance across all pathogen-antibiotic combinations",
    x = "Universal Health Coverage Index",
    y = "Median \npercentage resistance (%)",
    color = NULL
  ) +
  scale_size_continuous(range = c(3, 14))+
  scale_x_continuous(limits = c(30, 82), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  #geom_vline(xintercept = c_UHC$cut_off, linetype = "dotted", color = "grey40")+
  #annotate("text", x = 40-1, y = 4, label = "Low \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 60-1, y = 4, label = "Medium \nUHC", vjust = 1.2, hjust=1, size = 5) +
  #annotate("text", x = 80-1, y = 4, label = "High \nUHC", vjust = 1.2, hjust=1, size = 5) +
  # ggrepel::geom_text_repel(
  #   aes(label = Iso3),
  #   size = 3,
  #   max.overlaps = 30,       # Increase if you want more labels shown
  #   force = 1,               # Controls push-back from other labels (higher = more separation)
  #   point.padding = 0.2,     # Space around points
  #   segment.color = "black",# Line color linking text to point
  #   show.legend = FALSE
  # )+
  theme(
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = ggtext::element_markdown(size = 20),
    panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = c(0.49, 0.85),       # lower right inside the plot
    legend.justification = c(1, 0),
    legend.title = element_blank()
  )+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         size=F, alpha=F)

p3b


ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_3_resistance_by_income_scatter.png"), 
       plot = p3a, 
       device="png",
       width = 11, height =8)

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Final/Fig_3_resistance_by_income_scatter.svg"), 
       plot = p3a, 
       device="svg",
       width = 11, height =8)

# With difference size
ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_3_resistance_by_income_scatter_size_dots_bynisolates.png"), 
       plot = p3a_sizediff, 
       device="png",
       width = 11, height =8)

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Final/Fig_3_resistance_by_income_scatter_size_dots_bynisolates.svg"), 
       plot = p3a_sizediff, 
       device="svg",
       width = 11, height =8)

write.csv(br6, paste0(dirOutputReport, "/Executive_sum/Exc summary stats/Exc_fig_3_median_bsi_res_USED.csv"))

          
##########################################################################
greens <- brewer.pal(3, "Greens")     # e.g., "#E5F5E0" "#A1D99B" "#31A354"
reds   <- brewer.pal(4, "Reds")       # e.g., "#FEE5D9" "#FCAE91" "#FB6A4A" "#CB181D"
blues  <- brewer.pal(3, "Blues")      # e.g., "#DEEBF7" "#9ECAE1" "#3182BD"

# Manually defined colors
yellows <- c("gold")
orange  <- c("orange")
greys   <- c("grey20", "grey50")

# colors.db matches specific indices
colors.db <- c(greens[2], reds[3], blues[2], "lightgrey", "lemonchiffon1", "darkslategrey")

# Show the colors (optional)
scales::show_col(colors.db, cex_label = 0.7)

# Your original colors (non-italic names)
base_pathogen_colors <- c(
  "Acinetobacter spp." = colors.db[1],
  "E. coli" = colors.db[2],
  "K. pneumoniae" = colors.db[3],
  "Salmonella spp." = colors.db[4],
  "S. aureus" = colors.db[5],
  "S. pneumoniae" = colors.db[6],
  "Shigella spp." =  "burlywood3",
  "N. gonorrhoeae" = "slateblue1"
)

# Convert to markdown-style italic names
pathogen_colors <- setNames(base_pathogen_colors, 
                            ifelse(grepl("spp\\.", names(base_pathogen_colors)),
                                   paste0("*", gsub(" spp\\.", "", names(base_pathogen_colors)), "* spp."),
                                   paste0("*", names(base_pathogen_colors), "*")))

# Define shapes for specimen
specimen_shapes <- c(
  "BLOOD" = 15,
  "URINE" = 16,
  "UROGENITAL" = 17,
  "STOOL" = 18
)

# Prepare the data
plot_data <- r6 %>%
  filter(WHORegionName == "Global") %>%
  mutate(
    drug_bug_full = fct_reorder(drug_bug, regional_weighted_mean),
    y_numeric = as.numeric(drug_bug_full),
    
    PathogenFormatted = ifelse(
      grepl("spp\\.", PathogenName2),
      paste0("*", gsub(" spp\\..*", "", PathogenName2), "* spp."),
      paste0("*", PathogenName2, "*")
    ),
    
    drug_bug_label = paste0(PathogenFormatted, " - ", AntibioticName)
  )

# Tick label background tiles
tick_boxes <- plot_data %>%
  select(y_numeric, PathogenFormatted) %>%
  distinct()

# Plot
p2 = ggplot() +
  # Tick label color tiles
  geom_tile(data = tick_boxes,
            aes(x = -5, y = y_numeric, width = 9, height = 1, fill = PathogenFormatted),
            inherit.aes = FALSE, alpha = 0.5) +
  
  # Connecting lines
  geom_segment(data = plot_data,
               aes(x = 0, xend = regional_weighted_mean,
                   y = y_numeric, yend = y_numeric),
               color = "gray40", linewidth = 0.4) +
  
  # Points
  geom_point(data = plot_data,
             aes(x = regional_weighted_mean,
                 y = y_numeric,
                 shape = Specimen),
             color = "black", size = 4) +
  
  # Scales
  scale_shape_manual(values = specimen_shapes, labels = c(
    "BLOOD" = "Blood",
    "URINE" = "Urine",
    "UROGENITAL" = "Urogenital",
    "STOOL" = "Gastrointestinal")) +
  scale_fill_manual(values = pathogen_colors) +
  scale_y_continuous(
    breaks = plot_data$y_numeric,
    labels = plot_data$drug_bug_label
  ) +
  scale_x_continuous(limits = c(-10, 100), breaks = seq(0, 100, 20)) +
  
  # Labels
  labs(
    x = "Percentage (%)",
    y = "",
    shape = "",
    fill = ""
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_markdown(size = 12),
    legend.text = element_markdown(size = 15),
    legend.title = element_text(face = "bold", size =20),
    legend.position = c(0.95, 0.05),       # lower right inside the plot
    legend.justification = c(1, 0),
    axis.text.x = ggtext::element_markdown(size = 18),
    axis.title.x = ggtext::element_markdown(size = 18),
    panel.grid.major = element_line(linetype = "dotted", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white")
  )+
  guides(fill = guide_legend(nrow = 4),
         shape = guide_legend(nrow = 1))
p2

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_2_resistance_order.png"), 
       plot =  p2, 
       device="png",
       width = 17, height = 18)

# NOW PLOT PER REGION
plot_per_region <- function(region_name, data, pathogen_colors, specimen_shapes) {
  
  region_data <- data %>%
    filter(WHORegionName == region_name) %>%
    mutate(
      drug_bug_full = fct_reorder(drug_bug, regional_weighted_mean),
      y_numeric = as.numeric(drug_bug_full),
      PathogenFormatted = ifelse(
        grepl("spp\\.", PathogenName2),
        paste0("*", gsub(" spp\\..*", "", PathogenName2), "* spp."),
        paste0("*", PathogenName2, "*")
      ),
      drug_bug_label = paste0(PathogenFormatted, " - ", AntibioticName)
    )
  
  tick_boxes <- region_data %>%
    select(y_numeric, PathogenFormatted) %>%
    distinct()
  
  ggplot() +
    geom_tile(data = tick_boxes,
              aes(x = -5, y = y_numeric, width = 9, height = 1, fill = PathogenFormatted),
              inherit.aes = FALSE, alpha = 0.5) +
    geom_segment(data = region_data,
                 aes(x = 0, xend = regional_weighted_mean,
                     y = y_numeric, yend = y_numeric),
                 color = "gray40", linewidth = 0.4) +
    geom_point(data = region_data,
               aes(x = regional_weighted_mean,
                   y = y_numeric,
                   shape = Specimen),
               color = "black", size = 4) +
    scale_shape_manual(values = specimen_shapes, labels = c(
      "BLOOD" = "Blood",
      "URINE" = "Urine",
      "UROGENITAL" = "Urogenital",
      "STOOL" = "Gastrointestinal")) +
    scale_fill_manual(values = pathogen_colors) +
    scale_y_continuous(
      breaks = region_data$y_numeric,
      labels = region_data$drug_bug_label
    ) +
    scale_x_continuous(limits = c(-10, 100), breaks = seq(0, 100, 20)) +
    labs(
      title = region_name,
      x = "Percentage (%)",
      y = "",
      shape = "",
      fill = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      axis.text.y = ggtext::element_markdown(size = 12),
      legend.text = ggtext::element_markdown(size = 15),
      legend.title = element_text(face = "bold", size = 20),
      # legend.position = c(0.95, 0.05),
      # legend.justification = c(1, 0),
      legend.position = "none",
      axis.text.x = ggtext::element_markdown(size = 18),
      axis.title.x = ggtext::element_markdown(size = 18),
      panel.grid.major = element_line(linetype = "dotted", color = "gray80"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = "white")
    ) +
    guides(fill = guide_legend(nrow = 4),
           shape = guide_legend(nrow = 1))
}

# Apply the function to all regions
region_names <- unique(r6$WHORegionName)
region_plots <- map(region_names, ~plot_per_region(.x, r6, pathogen_colors, specimen_shapes))

# View the plot for the first region (e.g., Global)
region_plots[[2]]

combined = region_plots[[1]] + region_plots[[2]] + region_plots[[3]] + region_plots[[4]] + 
  region_plots[[5]] + region_plots[[6]] + plot_layout(ncol = 3)

combined

ggsave(filename = paste0(dirOutputReport, "/Executive_sum/Exc Figures/Provisional/Fig_2_resistance_order_regions.png"), 
       plot =  combined, 
       device="png",
       width = 32, height =30)

# PER YEAR TOP 10
# f1 <- r7 %>%
#   group_by(Region, Year) %>%
#   arrange(desc(Freq)) %>%
#   slice(1:10) %>%
#   mutate(
#     id = paste0(drug_bug, "-", Region, "-", "Year")
#   )
# 
# p4 = ggplot(f1 %>% filter(Year != 2016), aes(x = Year, fill = drug_bug,color="black", y = Prop * 100)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(. ~ Region, ncol = 7) +
#   #scale_fill_manual(name = " ", values = colors.db) +
#   labs(
#     x = "",
#     y = "Percentage of BCI (%)",
#     title = "Most frequent resistance-pathogen combinations across \nall pathogens identifed in BSI isolates"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.text = element_text(face = "italic", size = 20),
#     plot.background = element_rect(fill = "white", colour = "white"),
#     title = element_text(size = 34),
#     axis.text.x = element_text(size = 28),
#     axis.text.y = ggtext::element_markdown(size = 28),
#     strip.text.y.left = element_text(size = 30, angle = 0, vjust = 1.01, hjust = 1),
#     strip.text = element_text(size = 30),
#     strip.text.y = element_blank(),
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "bottom"
#   )
# p4
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.8c_pathogen_distr_bsi_ISOLATES_RESISTANT_ALLYEARS_barplot.svg"), 
#        plot = p4, 
#        device="svg",
#        width = 32, height = 10)
# 
# write.csv(f1, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8ab_Table_Pathogen_distr_overtime_3years_ALLYEARS_FORCONTEXT.csv"))
# 
# 
# # ONE REGION ALL PATHOGENS
# 
# region_data <- r7 %>%
#   filter(Region == "European Region") #%>%
#   # group_by(drug_bug) %>%
#   # mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
#   # fill(last_year_value, .direction = "updown") %>%
#   # ungroup() %>%
#   # mutate(drug_bug = fct_reorder(drug_bug, last_year_value, .desc = TRUE))
# 
# # Create the plot for the current region
# 
# ggplot(region_data, aes(x = Year, fill = drug_bug, y = Prop * 100)) +
#   geom_alluvium(aes(alluvium = drug_bug), alpha = 1) +
#   #scale_fill_manual(name = " ", values = value_colors) +
#   labs(
#     x = "",
#     y = 'Percentage (%)',
#     title = "Most frequent resistance-pathogen \ncombinations across all pathogens \nidentifed in BSI isolates",
#     subtitle = unique(region_data$Region)
#   ) +
#   scale_x_continuous(breaks = c(2018, 2020, 2022)) +
#   theme_minimal() +
#   scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
#   theme(
#     legend.text = element_text(face = "italic", size = 20),
#     plot.background = element_rect(fill = "white", colour = "white"),
#     title = element_text(size = 34),
#     plot.subtitle = element_text(size = 30, hjust = 0.5),  # Center the subtitle
#     axis.text.x = element_text(size = 28),
#     axis.text.y = ggtext::element_markdown(size = 28),
#     strip.text = element_text(size = 28, face = "bold"),  # Strip text for region names
#     panel.grid.major = element_line(linetype = "dotted"),
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "bottom"
#   ) +
#   guides(fill = guide_legend(nrow = 10))
# 
# 



# ECCMID
#----------------------------------------------------------------------------

# TRENDS FOR ECCMID - Abstract Ch3
#----------------------------------------------------------------------------

# FIRST WRANGLING OF CRUDE DATA (AS ON DASHBOARD)


# f12<-read.csv(paste0(dirDataNewO,"/EI_AMRdtaAC_180724_EV.csv"), header=T)
# 
# 
# ######AMR Fig 12 shows 2017-2021 data onlY, because too few countries provided data in 2016
# 
# 
# f12_1<- f12 %>%
#   filter(Year!="2016")%>% 
#   droplevels()
# 
# ##### AMR fig 12 considers only rows where interpretableAST is equal to or greater than 10
# 
# f12_2<- f12_1 %>%
#   filter(InterpretableAST>=10)%>% 
#   droplevels()
# 
# 
# ####resistance are mostly calculated for individual antibiotics, but in a few cases we must group some of the antibiotics. This is the case of SDGs (third generation
# ##cephalosporins in E. coli and methicillin resistance in S. aureus) and also sulfonamides and trimethoprim that is the same as co-trimoxazole (all pathogens). Note groupings are
# ##different in AMR Fig 12 compared to in AMR 8&9 or AMR Fig 11, specially in the case of methicillin resistance in S. aureus!!!
# 
# ##we must create the following groupings:
# 
# 
# f_rm <- f12_2 %>% 
#   group_by(Year, Specimen, PathogenName, Iso3) %>% 
#   summarise(tgc_ab=sum(Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO"),
#             mt_ab=sum(Antibiotic =="OXA" | Antibiotic =="FOX"))%>% 
#   as.data.frame() 
# 
# 
# f12_3<- f12_2 %>%
#   mutate(AbTargets = case_when(
#     (Antibiotic =="IPM") & (Specimen =="BLOOD") & (Pathogen=="ACISPP") ~ AntibioticName,
#     (Antibiotic =="IPM") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") ~ AntibioticName,
#     (Antibiotic =="CTX") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") ~ AntibioticName,
#     (Antibiotic =="CTX") & (Specimen =="BLOOD") & (Pathogen=="KLEPNE") ~ AntibioticName,
#     (Antibiotic =="IPM") & (Specimen =="BLOOD") & (Pathogen=="KLEPNE") ~ AntibioticName,
#     (Antibiotic =="PEN") & (Specimen =="BLOOD") & (Pathogen=="STRPNE") ~ AntibioticName,
#     (Antibiotic =="CIP") & (Specimen =="BLOOD" | Specimen == "STOOL") & (Pathogen=="SALSPP") ~ AntibioticName,
#     (Antibiotic =="CTX" | Antibiotic =="CAZ" | Antibiotic =="CRO" ) & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") ~ 'Third generation cephalosporins',
#     (Year == "2017" | Year == "2018") & (Antibiotic =="J01DD") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") ~ 'Third generation cephalosporins',
#     (Antibiotic =="OXA"| Antibiotic =="FOX") & (Specimen =="BLOOD") & (Pathogen=="STAAUR") ~ 'Methicillin resistance',
#     (Year == "2017" | Year == "2018") & (Antibiotic =="J01DC") & (Specimen =="BLOOD") & (Pathogen=="STAAUR") ~ 'Methicillin resistance',
#     (Antibiotic =="CIP") & (Specimen =="STOOL") & (Pathogen=="SALSPP") ~ AntibioticName,
#     (Antibiotic =="CIP") & (Specimen =="STOOL") & (Pathogen=="SHISPP") ~ AntibioticName,
#     (Antibiotic =="CRO") & (Specimen =="GENITAL") & (Pathogen=="NEIGON") ~ AntibioticName,
#     (Antibiotic =="CTX") & (Specimen =="URINE") & (Pathogen=="ESCCOL") ~ AntibioticName,
#     (Antibiotic =="IPM") & (Specimen =="URINE") & (Pathogen=="ESCCOL") ~ AntibioticName,
#     #(Antibiotic =="J01EE" | Antibiotic =="SXT") & (Specimen =="URINE") & (Pathogen=="ESCCOL") ~ 'Sulfonamides and trimethoprim',
#     (Antibiotic =="CTX") & (Specimen =="URINE") & (Pathogen=="KLEPNE") ~ AntibioticName,
#     (Antibiotic =="IPM") & (Specimen =="URINE") & (Pathogen=="KLEPNE") ~ AntibioticName)
#   )
# 
# f12_4<-merge(f12_3,f_rm, by=c("Year", "Specimen", "PathogenName", "Iso3"), all.x=TRUE)
# 
# f12_5 <- f12_4 %>%
#   mutate(RemoveRecord = case_when((Antibiotic =="J01DD") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="ESCCOL") & tgc_ab>0 ~  "remove",
#                                   (Antibiotic =="J01DC") & (Year == "2017" | Year == "2018") & (Specimen =="BLOOD") & (Pathogen=="STAAUR") & mt_ab>0 ~  "remove"))
# 
# f12_6 <- f12_5 %>%
#   filter(!is.na(AbTargets))%>% 
#   filter(is.na(RemoveRecord))%>% 
#   droplevels()
# 
# 
# f12_7<-select(f12_6, c(Year, Iso3, Specimen, PathogenName, AbTargets, TotalSpecimenIsolates, InterpretableAST,  Resistant))  
# 
# 
# ##calculate resistance
# 
# 
# f12_8 <- f12_7  %>% 
#   mutate(PercentResistant = ((coalesce(Resistant,0)/(coalesce(InterpretableAST,0))*100)))
# 
# 
# #for a given grouping we want row with max (percentageResistance). However, we do not want to select max(InterpretableAST) because we 
# # instead want the InterpretableAST that was used to calculate max (percentageResistance). So instead of summarise(max) we use slice_max
# 
# f12_8a <-f12_8 %>% 
#   as.data.frame() %>%
#   group_by(Specimen, PathogenName, AbTargets, Iso3, Year) %>% 
#   slice_max(PercentResistant) %>%
#   as.data.frame() 
# 
# 
# ##select only CTAs that have 5 years of data within each drug-bug time series
# 
# 
# f_count <- f12_8a %>% 
#   group_by(Specimen,	PathogenName,	AbTargets, Iso3) %>% 
#   summarise(YearsOfData = n_distinct(Year))%>% 
#   as.data.frame() 
# 
# 
# f12_9<-merge(f12_8a,f_count, by=c("Specimen",	"PathogenName",	"AbTargets", "Iso3"),all.x=TRUE)
# 
# f12_10 <- f12_9 %>%
#   filter(as.numeric(YearsOfData) == 5)%>%
#   droplevels()
# 
# f12_10<-distinct(f12_10)
# 
# ##remove time series with less than five countries########################################
# 
# f_unique <- f12_10 %>% 
#   group_by(Specimen,	PathogenName,	AbTargets) %>% 
#   summarise(CTAsInSeries = n_distinct(Iso3))%>% 
#   as.data.frame() 
# 
# f12_11<-merge(f12_10,f_unique,by=c("Specimen",	"PathogenName",	"AbTargets"),all.x=TRUE)
# 
# f12_12 <- f12_11 %>%
#   filter(CTAsInSeries >= 5)%>%
#   droplevels()
# 
# 
# f12_12<-select(f12_12, - c(YearsOfData, CTAsInSeries))  # this is the yearly data for individual CTAs
# 
# 
# #write.csv(f12_12, "C:\\Users\\tosaso\\OneDrive - World Health Organization\\Desktop\\Working On\\f12_12.csv", row.names=FALSE)    
# 
# 
# ##get the median, IQR, count of CTAs and count of isolates with AST
# 
# f12_13a <- f12_12 %>% 
#   group_by(Specimen, PathogenName, AbTargets, Year) %>% 
#   summarise(min= min(PercentResistant), 
#             Q1 = quantile(PercentResistant, probs = 0.25),
#             median=median(PercentResistant),
#             Q3 = quantile(PercentResistant, probs = 0.75),
#             max = max(PercentResistant))%>% 
#   as.data.frame() 
# 
# f12_13b <- f12_12 %>% 
#   group_by(Specimen, PathogenName, AbTargets) %>% 
#   summarise(CTAs = n_distinct(Iso3), TotalBCIsWithAST=sum(InterpretableAST))%>% 
#   as.data.frame() 
# 
# f12_13<-merge(f12_13a, f12_13b,by=c("Specimen",	"PathogenName",	"AbTargets"),all.x=TRUE)
# 
# f12_13= f12_13 %>% 
#   mutate(drug_bug = paste0(Specimen,"-", PathogenName,"-", AbTargets),
#          drug_bug = case_when(
#            drug_bug =="BLOOD-Staphylococcus aureus-Methicillin resistance" ~ "BLOOD-Staphylococcus aureus-Methicillin-resistance",
#            drug_bug == "BLOOD-Escherichia coli-Third generation cephalosporins" ~ "BLOOD-Escherichia coli-Third-generation cephalosporins",
#            TRUE ~ drug_bug
#          ),
#          AbTargets = case_when(
#            AbTargets =="Methicillin resistance" ~ "Methicillin-resistance",
#            AbTargets == "Third generation cephalosporins" ~ "Third-generation cephalosporins",
#            TRUE ~ AbTargets
#          ),
#          AntibioticName = AbTargets)
# 

# PLOT
#-----------------------------------

# custom_labeller <- labeller(
#   AntibioticName = function(labels) labels,  # Show all AntibioticName labels
#   PathogenName = function(labels) {
#     # Ensure all PathogenName labels are displayed once per unique group
#     unique_labels <- unique(labels)
#     sapply(labels, function(label) {
#       if (label %in% unique_labels) label else ""
#     })
#   }
# )
# p1 <- ggplot() +
#   # Add points for crude data
#   geom_point(
#     data = f12_13 %>% filter(Specimen == "BLOOD" & Year != 2017),
#     aes(x = Year, y = median, group = Year, colour = AntibioticName),
#     size = 4  # Increase point size
#   ) +
#   # Add error bars
#   geom_errorbar(
#     data = f12_13 %>% filter(Specimen == "BLOOD" & Year != 2017),
#     aes(x = Year, ymin = Q1, ymax = Q3, group = Year, colour = AntibioticName),
#     width = 0,  # Narrow error bar width
#     size = 2
#   ) +
#   # Add fitted trend line
#   geom_line(
#     data = r6 %>% filter(Region == "Global", Specimen == "BLOOD"),
#     aes(x = Year, y = median * 100, colour = AntibioticName),
#     size = 1.2
#   ) +
#   # Add ribbon for credible intervals
#   geom_ribbon(
#     data = r6 %>% filter(Region == "Global", Specimen == "BLOOD"),
#     aes(x = Year, ymin = Q2.5 * 100, ymax = Q97.5 * 100, fill = AntibioticName),
#     alpha = 0.3  # Slightly more opaque
#   ) +
#   # Adjust y-axis with a secondary axis
#   scale_y_continuous(
#     breaks = seq(0, 100, by = 10),
#     limits = c(0, 100),
#     expand = c(0, 0)
#   ) +
#   # Adjust x-axis scale
#   scale_x_continuous(
#     breaks = seq(min(f12_13$Year, na.rm = TRUE), max(f12_13$Year, na.rm = TRUE), by = 1)
#   ) +
#   # Facet by Pathogen and Antibiotic
#   facet_grid(
#     PathogenName + AntibioticName ~ Region,
#     scales = "free_y",
#     switch = "y",
#     space = "free",
#     labeller = custom_labeller
#   ) +
#   # Add color palette for AntibioticName
#   scale_colour_brewer(palette = "Set1") +  # Replace with a Brewer palette
#   scale_fill_brewer(palette = "Set1") +    # Same palette for ribbon fills
#   # Customize theme
#   theme_minimal() +
#   labs(
#     x = "Year",
#     y = "",
#     title = "Trends in resistance to selected antibiotics in WHO priority pathogens under GLASS surveillance (2018 – 2022)"
#   ) +
#   theme(
#     plot.background = element_rect(fill = "white", colour = "white"),
#     title = element_text(size = 20),
#     axis.text.x = element_text(size = 14, hjust = 1),
#     axis.text.y = ggtext::element_markdown(size = 14),
#     strip.placement = "outside",
#     strip.text.y = element_text(size=20),
#     strip.background = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#     legend.position = "none"
#   ) 
# 
# p1
# 
# 
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.3_Global_trends.png"), 
#        plot = p1, 
#        width = 15, height = 30)


# FOR ECCMID - Abstract Ch4
#---------------------------------------------------------
# A. bacter + E. coli + Klebs 
# pe1 <- plot_pathogen_antibiotic(
#   df = fig4.1%>%filter(drug_bug%in%ppbsi),
#   pathogens = c("Acinetobacter spp.", "Escherichia coli", "Klebsiella pneumoniae",
#                 "Salmonella spp.", "Streptococcus pneumoniae"),
#   specimen = "BLOOD",
#   custom_labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
#                     "Salmonella spp.", "S. pneumoniae"),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# pe1
# 
# pe2 <- plot_pathogen_antibiotic(
#   df = fig4.1%>%filter(drug_bug%in%pputi),
#   specimen = "URINE",
#   pathogens = c("Escherichia coli", "Klebsiella pneumoniae"),
#   custom_labels = c("E. coli", "K. pneumoniae"),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# pe2
# 
# df = r6
# df$Specimen[which(df$drug_bug=="STOOL-Shigella spp.-Ciprofloxacin")] = "STOOL" 
# df$AntibioticName[which(df$drug_bug=="STOOL-Shigella spp.-Ciprofloxacin")] = "Ciprofloxacin" 
# df$PathogenName[which(df$drug_bug=="STOOL-Shigella spp.-Ciprofloxacin")] = "Shigella spp." 
# df$Grouping[which(df$drug_bug=="STOOL-Shigella spp.-Ciprofloxacin")] = "Fluoroquinolones" 
# 
# pe3 <- plot_pathogen_antibiotic(
#   df = df%>%filter(drug_bug%in%ppgi),
#   specimen = "STOOL",
#   pathogens = c("Salmonella spp.", "Shigella spp."),
#   custom_labels = c("Salmonella spp.", "Shigella spp."),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# pe3
# 
# pe4 <- plot_pathogen_antibiotic(
#   df = fig4.1%>%filter(drug_bug%in%ppuro),
#   specimen = "UROGENITAL",
#   pathogens = c("Neisseria gonorrhoeae"),
#   custom_labels = c("N. gonorrhoeae"),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# pe4
# 
# pe5 <- plot_pathogen_antibiotic(
#   df = r6%>%filter(drug_bug%in%SDG &Year==2022),
#   specimen = "BLOOD",
#   pathogens = c("Escherichia coli", "Staphylococcus aureus"),
#   custom_labels = c("E. coli", "S. aureus"),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# pe5
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_BSI.svg"), 
#        plot = pe1, 
#        device="svg",
#        width = 30, height = 6.5)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_UTI.svg"), 
#        plot = pe2, 
#        device="svg",
#        width = 30, height = 4)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_GI.svg"), 
#        plot = pe3, 
#        width = 30, height = 3.3)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_URO.svg"), 
#        plot = pe4, 
#        width = 30, height = 3)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/ECCMID/Figure_SDG.svg"), 
#        plot = pe5, 
#        width = 30, height = 3.2)





# FOR PRESENTATION
#---------------------------------------------------------------------------------------
# SDG INDIVIDUAL COUNTRY PREVALENCE
# w_prev_results_bsi = readRDS(file.path(dirDataModeloutput, "/Model_prevalence_estimates/predicted_w_prev_weakip_set1_bsi.rds"))
# 
# SDG = which(names(w_prev_results_bsi) %in% c("BLOOD-Staphylococcus aureus-Methicillin-resistance","BLOOD-Escherichia coli-Third-generation cephalosporins"))
# 
# # Define colors for each WHORegionCode
# facet_colors2b <- c(
#   "Global" = "grey",
#   "AFR" = palette5[1],
#   "AMR" = palette5[2],
#   "EMR" = "gold",
#   "EUR" = palette5[4],
#   "SEA" = palette5[5],
#   "WPR" = palette5[6]
# )
# # SDG - E. coli
# w_pred <- w_prev_results_bsi[[SDG[1]]]
# d <- adataAC %>% filter(Year == 2022, combined == names(w_prev_results_bsi)[SDG[2]], InterpretableAST > 10)
# Iso3_2022 <- unique(d$Iso3)
# 
# p1 <- ggplot(w_pred %>% filter(Year == 2022, Iso3 %in% Iso3_2022), 
#              aes(x = reorder(Iso3, w_prev), y = w_prev * 100, col = WHORegionCode)) +
#   #geom_point(aes(y = amr_rate * 100), shape = 18, color = "grey", size = 3) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = w_prev_lower * 100, ymax = w_prev_upper * 100), alpha = 0.4, size = 2, width = 0.5, linetype = 1) +
#   theme_minimal() +
#   scale_shape_manual(name = "", values = c(17, 19)) +
#   scale_color_manual(name = " ", values = c(facet_colors2b)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   labs(
#     title = "",
#     #subtitle = bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli")),
#     subtitle = "Percentage of <i>E. coli</i> resistant to 3rd-gen cephalosporins in bloodstream infections<br>(2022 data)",
#     y = "Percentage resistant (%)",
#     x = ""
#   ) +
#   theme(
#     legend.position = "right",
#     axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
#     axis.text.y = element_text(size = 15),
#     title = element_text(size = 20),
#     legend.box = "horizontal",
#     legend.title = element_text(size = 15),
#     plot.subtitle = element_markdown(size = 20, hjust = 0),
#     legend.text = element_text(size = 15),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted")
#   ) +
#   guides(size = FALSE, color = FALSE)
# p1
# 
# # SDG - MRSA
# w_pred <- w_prev_results_bsi[[SDG[2]]]
# d <- adataAC %>% filter(Year == 2022, combined == names(w_prev_results_bsi)[SDG[2]], InterpretableAST > 10)
# Iso3_2022 <- unique(d$Iso3)
# 
# p2 <- ggplot(w_pred %>% filter(Year == 2022, Iso3 %in% Iso3_2022), 
#              aes(x = reorder(Iso3, w_prev), y = w_prev * 100, col = WHORegionCode)) +
#   #geom_point(aes(y = amr_rate * 100), shape = 18, color = "grey", size = 3) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = w_prev_lower * 100, ymax = w_prev_upper * 100), alpha = 0.4, size = 2, width = 0.5, linetype = 1) +
#   theme_minimal() +
#   scale_shape_manual(name = "", values = c(17, 19)) +
#   scale_color_manual(name = " ", values =  c(facet_colors2b)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   labs(
#     title = "",
#     #subtitle = bquote(atop("Percentage of methicillin-resistant " ~ italic("Staphylococcus aureus") ~ " in bloodstream infections (2022 data)")),
#     subtitle = "Percentage of <i>S. aureus</i> resistant to methicillin in bloodstream infections<br>(2022 data)",
#     y = "Percentage resistant (%)",
#     x = ""
#   ) +
#   theme(
#     legend.position = "right",
#     axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
#     axis.text.y = element_text(size = 15),
#     title = element_text(size = 20),
#     legend.box = "horizontal",
#     legend.title = element_text(size = 15),
#     plot.subtitle = element_markdown(size = 20, hjust = 0),
#     legend.text = element_text(size = 15),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted")
#   ) +
#   guides(size = FALSE, color = FALSE)
# p2
# 
# combined_plot1 = p4.2a+p4.2b+plot_layout(ncol=1)
# combined_plot2 = p1+p2+plot_layout(ncol=1)
# 
# 
# # PRESENTATION
# ggsave(filename = paste0(dirOutput, "/Analyses/Section4.1_Global_AMR_prevalence/G7_SDG/Figure_4.2_SDG_3GC_MRSA.png"), 
#        plot = combined_plot1, 
#        width = 10, height = 12, limitsize=F)
# 
# ggsave(filename = paste0(dirOutput, "/Analyses/Section4.1_Global_AMR_prevalence/G7_SDG/Figure_4.2_SDG_3GC_MRSA_CTA.png"), 
#        plot = combined_plot2, 
#        width = 13, height = 13, limitsize=F)










# CODE NOT USED
#----------------------------------------------------------------------------

# CHAPTER 3
############################################################################

# 3.3 BCI Coverage
#---------------------------------------------------------------------------

# ADD BUBBLES PER REGION
#------------------------------------------------------------------------------------------
# n1 <- adataAC %>%
#   filter(InReport == "Yes" & TotalSpecimenIsolates>0) %>%
#   droplevels() %>%
#   select(c(WHORegionName, Iso3, Year, Specimen, SpecimenIsolateswithAST, TotalPopulation)) %>%
#   group_by(Iso3, Year, Specimen) %>% 
#   summarize_all(max) %>%
#   ungroup() %>%
#   group_by(WHORegionName,Specimen) %>%
#   summarise(BCI2022 = sum(SpecimenIsolateswithAST[Year==2022]),
#             BCITotal = sum(SpecimenIsolateswithAST))
# 
# n1b = adataAC %>%
#   filter(InReport == "Yes" & TotalSpecimenIsolates>0) %>%
#   droplevels() %>%
#   select(c(WHORegionName, Iso3, Year, Specimen, SpecimenIsolateswithAST, TotalPopulation)) %>%
#   group_by(Iso3, Year, Specimen) %>% 
#   summarize_all(max) %>%
#   ungroup() %>%
#   group_by(Specimen) %>%
#   summarise(BCI2022 = sum(SpecimenIsolateswithAST[Year==2022]),
#             BCITotal = sum(SpecimenIsolateswithAST)) %>%
#   mutate(
#     WHORegionName ="Global"
#   )
#   
# n2 = rbind(n1b, n1)
# 
# n2 <- n2 %>% mutate(
#   Region = relevel(factor(WHORegionName), ref="Global",
#                    levels = c(
#                      "Global",
#                      "African Region",
#                      "Region of the Americas",
#                      "South-East Asia Region",
#                      "European Region",
#                      "Eastern Mediterranean Region European Region",
#                      "Western Pacific Region",
#                    ))
# )
# 
# n2b = n2 %>% filter(Region=="Global") %>%
#   group_by(Specimen) %>%
#   summarise(BCI2022Total = sum(BCI2022))
# 
# n3 = left_join(n2,n2b)
# n4 = n3 %>% 
#   mutate(BCI2022prop = BCI2022/BCI2022Total)
# 
# fig3.3b = n4
# 
# # Function to create circle coordinates
# circle <- function(center, radius, group) {
#   th <- seq(0, 2 * pi, length.out = 200)
#   data.frame(
#     group = group,
#     x = center[1] + radius * cos(th),
#     y = center[2] + radius * sin(th)
#   )
# }
# 
# # Function to generate nested circles for each region-specimen combination
# generate_circle_data <- function(region, specimen, BCI2022, BCITotal, scale_factor = 5000) {
#   # Calculate radii
#   total_radius <- sqrt(BCITotal) / scale_factor
#   inner_radius <- sqrt(BCI2022) / scale_factor
#   
#   # Define centers
#   center <- c(0, 0)
#   offset_distance <- total_radius - inner_radius
#   inner_center <- c(center[1], center[2] - offset_distance)
#   
#   # Create outer and inner circles
#   outer_circle <- circle(center, total_radius, "Total")
#   inner_circle <- circle(inner_center, inner_radius, "2022")
#   
#   # Combine circles and add metadata for labels and facetting
#   circle_data <- rbind(outer_circle, inner_circle) %>%
#     mutate(
#       label_x = ifelse(group == "Total", center[1], inner_center[1]),
#       label_y = ifelse(group == "Total", center[2] + total_radius * 0.3, inner_center[2]),
#       label = ifelse(group == "Total", number(BCITotal, big.mark = " "), number(BCI2022, big.mark = " ")),
#       WHORegionName = region,
#       Specimen = specimen
#     )
#   
#   return(circle_data)
# }
# 
# # Sample data for multiple regions and specimens
# data=n4
# 
# data = data %>%
#   group_by(WHORegionName) %>%
#   arrange(desc(BCITotal))
# 
# # Apply the function to each row in the dataset
# all_circle_data <- pmap_dfr(
#   list(data$WHORegionName, data$Specimen, data$BCI2022, data$BCITotal),
#   ~ generate_circle_data(..1, ..2, ..3, ..4)
# )
# 
# # Convert group to a factor to control color mapping
# all_circle_data$group <- factor(all_circle_data$group, levels = c("2022", "Total"))
# 
# # Plot with facets for each region-specimen combination
# 
# # Generate a list of unique regions
# regions <- c(
#   "African Region",
#   "Region of the Americas",
#   "South-East Asia Region",
#   "European Region",
#   "Eastern Mediterranean Region", 
#   "Western Pacific Region"
# )
# 
# # Initialize an empty list to store individual plots
# plot_list <- list()
# 
# # Loop over each region and create a separate plot
# for (i in regions) {
#   region_labels <- c(
#     "African Region" = "African\nRegion",
#     "Region of the Americas" = "Region of the \nAmericas",
#     "South-East Asia Region" = "South-East\nAsia Region",
#     "European Region" = "European\nRegion",
#     "Eastern Mediterranean Region" = "Eastern\nMediterranean \nRegion",
#     "Western Pacific Region" = "Western\nPacific Region"
#   )
#   region_label = region_labels[i]
#   # Filter data for the current region
#   region_data <- all_circle_data %>% filter(WHORegionName == i, Specimen == "BLOOD")
#   
#   # Create the plot for the current region
#   p <- ggplot(region_data) +
#     geom_polygon(aes(x = x, y = y, group = group, fill = WHORegionName, alpha=group), color = "black", alpha = 0.6) +
#     geom_text(
#       data = region_data %>% filter(group == "Total"),  # Filter data for the outer bubble
#       aes(x = label_x, y = label_y, label = label, size = 6, vjust = 0.1),  # Set size for Total group
#       color = "white"
#     ) +
#     scale_size_identity() +  # Use size values directly
#     scale_fill_manual(values = facet_colors3a[i]) +
#     coord_equal() +
#     ggthemes::theme_map() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, size = 18)
#     ) +
#     labs(title = region_label)
#   
#   # Add the plot to the list
#   plot_list[[i]] <- p
# }
# 
# # Combine all plots horizontally using cowplot
# combined_plot <- plot_grid(plotlist = plot_list, ncol = length(plot_list), align = "h")
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3_BCI_bubbles_BSI.svg"), 
#        plot = combined_plot,
#        device="svg",
#        width = 12, height = 14,dpi = 300)
# 
# 
# # UTI
# # Loop over each region and create a separate plot
# for (i in regions) {
#   region_labels <- c(
#     "African Region" = "African\nRegion",
#     "Region of the Americas" = "Region of the \nAmericas",
#     "South-East Asia Region" = "South-East\nAsia Region",
#     "European Region" = "European\nRegion",
#     "Eastern Mediterranean Region" = "Eastern\nMediterranean \nRegion",
#     "Western Pacific Region" = "Western\nPacific Region"
#   )
#   region_label = region_labels[i]
#   # Filter data for the current region
#   region_data <- all_circle_data %>% filter(WHORegionName == i, Specimen == "URINE")
#   
#   # Create the plot for the current region
#   p <- ggplot(region_data) +
#     geom_polygon(aes(x = x, y = y, group = group, fill = WHORegionName, alpha=group), color = "black", alpha = 0.6) +
#     geom_text(
#       data = region_data %>% filter(group == "Total"),  # Filter data for the outer bubble
#       aes(x = label_x, y = label_y, label = label, size = 6, vjust = 0.1),  # Set size for Total group
#       color = "white"
#     ) +
#     scale_size_identity() +  # Use size values directly
#     scale_fill_manual(values = facet_colors3a[i]) +
#     coord_equal() +
#     ggthemes::theme_map() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, size = 18)
#     ) +
#     labs(title = region_label)
#   
#   # Add the plot to the list
#   plot_list[[i]] <- p
# }
# 
# # Combine all plots horizontally using cowplot
# combined_plot <- plot_grid(plotlist = plot_list, ncol = length(plot_list), align = "h")
# print(combined_plot)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3_BCI_bubbles_UTI.svg"), 
#        plot = combined_plot,
#        device="svg",
#        width = 12, height = 14,dpi = 300)
# 
# # Loop over each region and create a separate plot
# for (i in regions) {
#   region_labels <- c(
#     "African Region" = "African\nRegion",
#     "Region of the Americas" = "Region of the \nAmericas",
#     "South-East Asia Region" = "South-East\nAsia Region",
#     "European Region" = "European\nRegion",
#     "Eastern Mediterranean Region" = "Eastern\nMediterranean \nRegion",
#     "Western Pacific Region" = "Western\nPacific Region",
#     "Global" = "Global"
#   )
#   region_label = region_labels[i]
#   # Filter data for the current region
#   region_data <- all_circle_data %>% filter(WHORegionName == i, Specimen == "STOOL")
#   
#   # Create the plot for the current region
#   p <- ggplot(region_data) +
#     geom_polygon(aes(x = x, y = y, group = group, fill = WHORegionName, alpha=group), color = "black", alpha = 0.6) +
#     geom_text(
#       data = region_data %>% filter(group == "Total"),  # Filter data for the outer bubble
#       aes(x = label_x, y = label_y, label = label, size = 6, vjust = 0.1),  # Set size for Total group
#       color = "white"
#     ) +
#     scale_size_identity() +  # Use size values directly
#     scale_fill_manual(values = facet_colors3a[i]) +
#     coord_equal() +
#     ggthemes::theme_map() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, size = 18)
#     ) +
#     labs(title = region_label)
#   
#   # Add the plot to the list
#   plot_list[[i]] <- p
# }
# 
# # Combine all plots horizontally using cowplot
# combined_plot <- plot_grid(plotlist = plot_list, ncol = length(plot_list), align = "h")
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3_BCI_bubbles_GI.svg"), 
#        plot = combined_plot,
#        device="svg",
#        width = 12, height = 14,dpi = 300)
# 
# # UROGENITAL
# # Loop over each region and create a separate plot
# for (i in regions) {
#   region_labels <- c(
#     "African Region" = "African\nRegion",
#     "Region of the Americas" = "Region of the \nAmericas",
#     "South-East Asia Region" = "South-East\nAsia Region",
#     "European Region" = "European\nRegion",
#     "Eastern Mediterranean Region" = "Eastern\nMediterranean \nRegion",
#     "Western Pacific Region" = "Western\nPacific Region"
#   )
#   region_label = region_labels[i]
#   # Filter data for the current region
#   region_data <- all_circle_data %>% filter(WHORegionName == i, Specimen == "UROGENITAL")
#   
#   # Create the plot for the current region
#   p <- ggplot(region_data) +
#     geom_polygon(aes(x = x, y = y, group = group, fill=WHORegionName, alpha = group), color = "black", alpha = 0.6) +
#     geom_text(
#       data = region_data %>% filter(group == "Total"),  # Filter data for the outer bubble
#       aes(x = label_x, y = label_y, label = label, size = 6, vjust = 0.1),  # Set size for Total group
#       color = "white"
#     ) +
#     scale_size_identity() +  # Use size values directly
#     scale_fill_manual(values = facet_colors3a[i]) +
#     coord_equal() +
#     ggthemes::theme_map() +
#     theme(
#       legend.position = "none",
#       plot.title = element_text(hjust = 0.5, size = 18)
#     ) +
#     labs(title = region_label)
#   
#   # Add the plot to the list
#   plot_list[[i]] <- p
# }
# 
# # Combine all plots horizontally using cowplot
# combined_plot <- plot_grid(plotlist = plot_list, ncol = length(plot_list), align = "h")
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.3_BCI_bubbles_URO.svg"), 
#        plot = combined_plot,
#        device="svg",
#        width = 12, height = 14, dpi=300)

# # Bubble Plot for WHO region data
# p1b = create_bubble_chart(fig3.3b, "BLOOD", outer_size = 30, inner_size = 20, colors = facet_colors3)
# p2b = create_bubble_chart(fig3.3b, "URINE", outer_size = 30, inner_size = 20, colors = facet_colors3)
# p3b = create_bubble_chart(fig3.3b, "STOOL", outer_size = 30, inner_size = 20, colors = facet_colors3)
# p4b = create_bubble_chart(fig3.3b, "UROGENITAL", outer_size = 30, inner_size = 20, colors = facet_colors3)
# 
# # For writing
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3_BCIperMillion_REGIONALBLOOD.pdf"), 
#        plot = p1b, 
#        width = 8, height =4)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3_BCIperMillion_REGIONALURINE.pdf"), 
#        plot = p2b, 
#        width = 8, height =4)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3_BCIperMillion_REGIONALSTOOL.pdf"), 
#        plot = p3b, 
#        width = 8, height =4)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3_BCIperMillion_REGIONALGONOR.pdf"), 
#        plot = p4b, 
#        width = 8, height =4)

# For writing
#write.csv(fig3.3b, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.3_BCIperMillion_specimenREGIONAL.csv"))




# Fig 3.4 (Now table 3.1)
####################################################################


# VISUALISE
# tab3.1$combined = paste0(tab3.1$Specimen," - ", tab3.1$PathogenName) 
# 
# 
# dummy = data.frame(WHORegionName = rep(unique(tab3.1$WHORegionName, 7)), 
#                    Specimen =rep(c("BLOOD", "URINE", "STOOL", "UROGENITAL"), 7),
#                    PathogenName = rep(c("","","",""), 7),
#                    TotalIsolates = rep(c(NA,NA,NA,NA), 7),
#                    Period = rep(c("2016-2021"), 7),
#                    CountryCount = rep(c(0,0,0,0), 7),
#                    TotalWithCountries = rep(c(NA,NA,NA,NA), 7),
#                    combined = rep(paste0(c("BLOOD", "URINE", "STOOL", "UROGENITAL"), c("-","-","-","-")), 7))
# 
# dummy2022 = dummy
# dummy2022$Period = "2022"
# 
# dummy = rbind(dummy, dummy2022)

# tab3.1P = rbind(tab3.1, dummy)
# tab3.1P <- tab3.1P %>%
#   mutate(
#     combined = factor(combined, levels = rev(c(
#       "BLOOD-",
#       "BLOOD - Acinetobacter spp.",
#       "BLOOD - Escherichia coli",
#       "BLOOD - Klebsiella pneumoniae",     
#       "BLOOD - Salmonella spp.",
#       "BLOOD - Staphylococcus aureus",
#       "BLOOD - Streptococcus pneumoniae",
#       "BLOOD - Total", 
#       "STOOL-",
#       "STOOL - Salmonella spp.",
#       "STOOL - Shigella spp.",
#       "STOOL - Total",
#       "URINE-",    
#       "URINE - Escherichia coli",          
#       "URINE - Klebsiella pneumoniae",
#       "URINE - Total",
#       "UROGENITAL-",    
#       "UROGENITAL - Neisseria gonorrhoeae",                     
#       "UROGENITAL - Total"
#     ))),
#     TotalIsolates_log10 = ifelse(!is.na(TotalIsolates), log10(as.numeric(TotalIsolates)), 0),
#     TotalIsolates_conv = 
#       case_when(
#         Specimen == "URINE" & PathogenName%in%c("Escherichia coli","Total", "Klebsiella pneumoniae") & Period == "2016-2021" ~ as.numeric(TotalIsolates) / 10,
#         Specimen == "URINE" & PathogenName%in%c("Escherichia coli","Total") & Period == "2022" ~ as.numeric(TotalIsolates) / 10,
#         Specimen == "BLOOD" & PathogenName%in%c("Total") & Period == "2016-2021"~ as.numeric(TotalIsolates) / 10,
#         TRUE~TotalIsolates),
#     Changed_flag = 
#       case_when(
#         Specimen == "URINE" & PathogenName%in%c("Escherichia coli","Total", "Klebsiella pneumoniae") & Period == "2016-2021"&WHORegionName== "Western Pacific Region" ~ "Yes",
#         Specimen == "URINE" & PathogenName%in%c("Escherichia coli","Total") & Period == "2022" &WHORegionName== "Western Pacific Region" ~ "Yes",
#         Specimen == "BLOOD" & PathogenName%in%c("Total") & Period == "2016-2021"  &WHORegionName== "Western Pacific Region"~ "Yes",
#         TRUE~"No")
#   )
# tab3.1 = tab3.1 %>%# tadummyb3.1 = tab3.1 %>%
#   mutate(combined = factor(combined, levels = rev(unique(combined))))


# Number of Countries (Left plot)
# p1 <- ggplot(tab3.1P %>% filter(WHORegionName!="Global"), aes(x = combined, y = CountryCount, fill = WHORegionName, alpha=Period)) +
#   geom_bar(stat = "identity", position = "stack") +
#   geom_text(data = tab3.1P %>% filter(WHORegionName == "Global"),
#             aes(label = ifelse(CountryCount == 0, NA, CountryCount), 
#                 x = combined, 
#                 y = max(CountryCount) + 8),  # Adjust y position for reversed axis
#             size = 3.5,alpha=1) +
#   ggh4x::facet_grid2(Period~.,, scales = "free_y", independent = "y")+
#   #facet_grid(Period~., scales = "fixed") +
#   scale_fill_manual(values=facet_colors3a) +
#   labs(
#     title = "Number of \nCTA",
#     x = "",
#     y = "Number of Countries",
#     fill = "Specimen Type"
#   ) +
#   scale_alpha_manual(
#     values = c("2022" = 1, "2016-2021" = 0.6)  # Define alpha for each Period
#   )+
#   #ylim(0,100) +
#   scale_y_reverse(limits=c(100,0),
#                   breaks=seq(0,75,25)) +
#   theme_minimal() +
#   theme(
#     strip.text = element_blank(),  # Bold facet labels
#     axis.text.y = element_blank(),                      # Remove x-axis labels (bottom side)
#     #axis.text.y.top = element_blank(),                  # Remove x-axis labels (top side for flipped plot)
#     axis.title.y = element_blank(),                     # Remove x-axis title
#     axis.title.x = element_text(size=14),
#     axis.text.x = element_text(size=12),
#     axis.title = element_text(size = 10),
#     plot.title = element_text(size = 12, face = "bold"),  # Align title to the right
#     panel.grid.major = element_blank(),  # Remove major grid lines
#     panel.grid.minor = element_blank(),
#     legend.position = "none"                            # Remove legend
#   )  +
#   coord_flip(clip="off")
# p1
# 
# # Number of Countries (Left plot)
# p2 <- ggplot(tab3.1P %>% filter(WHORegionName != "Global"), aes(x = combined, y = TotalIsolates_conv / 10000, fill = WHORegionName, alpha = Period)) +
#   geom_bar(stat = "identity", position = "stack") +
#   geom_text(data = tab3.1P %>% filter(WHORegionName == "Global"),
#             aes(label = comma(TotalIsolates),  # Format labels with commas
#                 x = combined, 
#                 y = max(TotalIsolates_conv, na.rm = TRUE) / 10000 + 20),
#             vjust = 0, size = 3.5, alpha = 1) +
#   geom_text(
#     data = tab3.1P %>% filter(Changed_flag == "Yes"),  # Filter for rows where values were changed
#     aes(
#       x = combined,
#       y = max(tab3.1P$TotalIsolates_conv, na.rm = TRUE) / 10000 - 10,  # Position stars at the top of bars
#       label = "* x10"
#     ),
#     vjust = 0,  # Adjust vertical position of the star
#     size = 3,      # Size of the star
#     color = "black",
#     alpha = 1      # Alpha for the star
#   ) +
#   #facet_grid(Period ~ ., scales = "fixed", switch = "x") +
#   ggh4x::facet_grid2(Period~., scales = "free_y", independent = "y", switch = "x")+
#   scale_fill_manual(values = facet_colors3a) +
#   scale_y_continuous(
#     labels = scales::comma,  # Format y-axis labels with commas
#     limits = c(0,175),
#     breaks = seq(0, round(max(tab3.1P$TotalIsolates_conv, na.rm = TRUE) / 10000,0), by = 25)  # Adjust breaks to steps of 100,000
#   ) +
#   scale_x_discrete(
#     labels = c("BLOOD-" = expression(bold("Bloodstream")),
#                "BLOOD - Acinetobacter spp." = expression(italic("Acinetobacter spp.")),
#                "BLOOD - Escherichia coli" = expression(italic("E. coli")),
#                "BLOOD - Klebsiella pneumoniae" = expression(italic("K. pneumoniae")),     
#                "BLOOD - Salmonella spp." = expression(italic("Salmonella spp.")),
#                "BLOOD - Staphylococcus aureus" = expression(italic("S. aureus")),
#                "BLOOD - Streptococcus pneumoniae" = expression(italic("S. pneumoniae")),
#                "BLOOD - Total" = expression(bold("Total")),
#                "STOOL-" = expression(bold("Gastrointestinal")),
#                "STOOL - Salmonella spp." = expression(italic("Salmonella spp.")), 
#                "STOOL - Shigella spp." = expression(italic("Shigella spp.")),
#                "STOOL - Total" = expression(bold("Total")),
#                "URINE-" = expression(bold("Urinary tract")),
#                "URINE - Escherichia coli" = expression(italic("E. coli")),         
#                "URINE - Klebsiella pneumoniae" = expression(italic("K. pneumoniae")),
#                "URINE - Total" = expression(bold("Total")),
#                "UROGENITAL-" = expression(bold("Gonorrhoea")),
#                "UROGENITAL - Neisseria gonorrhoeae" = expression(italic("N. gonorrhoeae")),                   
#                "UROGENITAL - Total" = expression(bold("Total")))
#   ) +
#   labs(
#     title = "Number of \nBCIs with AST",
#     x = "",
#     y = "Number of BCIs with AST (x10,000)",
#     fill = "Specimen Type"
#   ) +
#   scale_alpha_manual(
#     values = c("2022" = 1, "2016-2021" = 0.6)  # Define alpha for each Period
#   ) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     axis.text.x = element_text(hjust = 0, size = 12),  # Adjusted x-axis text
#     axis.text.y = element_text(hjust = -0.1, size = 10),
#     axis.title = element_text(size = 10),
#     axis.title.x = element_text(size = 14),
#     legend.position = "none",
#     panel.grid.major = element_blank(),  # Remove major grid lines
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(hjust = 0.9, size = 12, face = "bold")  # Align title to the right
#   ) +
#   coord_flip(clip = "off")
# 
# p2
# 
# # Combine the plots side by side
# combined_plot <- p1 + p2 +
#   plot_layout(ncol = 2, widths = c(1, 1)) # Adjust column width as needed
# 
# # Display the combined plot
# combined_plot
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Fig_3.3b_BCIpermillion_specimen.svg"), 
#        plot = combined_plot, 
#        device="svg",
#        width = 17, height = 10)


# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.4(a-e): Trends of BCIs with AST results reported to GLASS-AMR per one million population for priority pathogens
# --------------------------------------------------------------------------------------------------------------------------------------


# CHECK NUMBER OF BLOOD CULTURES (WITH INTERPRETABLE AST) PER 1000 INPATIENT DAYS
# We know the inpatient days are not well reported. We calculate the below, also to compare with ECDC EARSNET and CEASAR
# https://www.ecdc.europa.eu/en/publications-data/surveillance-antimicrobial-resistance-europe-2022
# https://www.ecdc.europa.eu/en/publications-data/antimicrobial-resistance-surveillance-europe-2023-2021-data

# pdays_rates_specimen <- adataAC %>%
#   filter(TotalSpecimenIsolates > 0 & Year == 2022, Specimen=="BLOOD") %>%   # Filter the data first
#   droplevels() %>%
#   select(c(WHORegionCode,Iso3, Year, Specimen, SpecimenIsolateswithAST, amr_glass_inpatient_day_number, EnrolledAMR)) %>% 
#   group_by(Iso3, Year, Specimen) %>%
#   summarize_all(max) %>%
#   mutate(
#     BCI_perthouspdays = (SpecimenIsolateswithAST / amr_glass_inpatient_day_number) * 1000,
#     Inpdays = amr_glass_inpatient_day_number
#   )
# 
# 
# pdays_rates_specimen_region = pdays_rates_specimen %>%
#   group_by(WHORegionCode) %>%
#   summarise(median = median(BCI_perthouspdays, na.rm=T),
#             q1 = quantile(BCI_perthouspdays, prob=0.25, na.rm=T),
#             q3 = quantile(BCI_perthouspdays, prob = 0.75, na.rm=T),
#             missing = sum(is.na(BCI_perthouspdays)),
#             medianpdays = median())
# pdays_rates_specimen_region
# 
# 
# # Illustration of bad reporting of inpatient days and/or number of isolates. 
# pdays_rates_specimen_reported = pdays_rates_specimen %>% filter(!is.na(BCI_perthouspdays))
# write.csv(pdays_rates_specimen_reported, paste0(dirOutput, "/Descriptive/Section3.2_Surveillance_indicators/bci_per1000inpdays.csv"))


# 4.2 BENCHMARK PLOT FOR SDG
###################################################################

# b1 = d5%>%filter(Grouping=="SDG", Year=="2022")
# c1 = r6%>%filter(Grouping=="SDG")
# 
# # BCI per million
# d1 <- adataAC %>%
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
# d2 <- d1 %>%
#   filter(!is.na(Grouping)) %>%
#   droplevels()
# 
# d3 <- d2 %>% 
#   filter(TotalPathogenIsolates>0) %>%   
#   droplevels() 
# 
# d4 <- d3 %>% dplyr::select(c(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName, Grouping, InterpretableAST, TotalPopulation)) 
# 
# d5 <- d4 %>% 
#   as.data.frame() %>%
#   group_by(WHORegionCode, WHORegionName, Iso3, Year, Specimen, PathogenName, Grouping) %>%
#   summarize_all(max) %>%
#   as.data.frame()
# 
# 
# d6 <- d5 %>%
#   mutate(BCI_permillion = ((coalesce(InterpretableAST,0)/(coalesce(TotalPopulation,0))*1000000)),
#          combined = paste0(Specimen, "-", PathogenName, "-", Grouping),
#          WHORegionCode=relevel(factor(WHORegionCode),ref="EUR")
#   )
# 
# d6 = d6%>% mutate(
#   PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
#                                                   "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae",
#                                                   "Shigella spp.","Neisseria gonorrhoeae"),
#                          labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
#                                     "Salmonella spp.", "S. aureus", "S. pneumoniae","Shigella spp.","N. gonorrhoeae")))
# 
# 
# d6b = d6 %>% filter(Specimen=="BLOOD")
# d6b$combined2 = paste0(d6b$PathogenName2, "-", d6b$Grouping)
# 
# d7 = d6b %>% filter(combined2 %in% c("S. aureus-Methicillin resistance","E. coli-Third generation cephalosporins"),
#                     Year=="2022")
# 
# b2 = left_join(b1,d7, by=c("Iso3","PathogenName", "WHORegionName","Specimen", "Year"))
# b3 = b2 %>% filter(!is.na(amr_rate))
# 
# # LINK WITH HAQI DATA
# b4 = left_join(b3,haqidata %>%select(Iso3, val), by="Iso3")
# 
# # REGIONAL HAQI MEDIAN
# hr = haqidata %>%
#   group_by(WHORegionName) %>%
#   summarise(m_val = median(val))%>%
#   filter(!is.na(WHORegionName))
# 
# # Global 
# hg = haqidata %>%
#   summarise(m_val = median(val))%>%
#   mutate(WHORegionName= "Global") 
# 
# hr = rbind(hr,hg)
# 
# # E. coli SDG
# p1 = ggplot(b4 %>% filter(PathogenName == "Escherichia coli"), aes(x = val, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Escherichia coli",WHORegionName!="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>%filter(WHORegionName!="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   facet_wrap(~WHORegionName, ncol=6) +
#   geom_point(size=3.5) +
#   scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 4,max.overlaps=100) +
#   theme_minimal() +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "HAQI index",
#     y = 'Proportion resistant (%)',
#     title = bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli"))
#   ) +
#   theme(
#     plot.title=element_text(size=24),
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 18),
#     legend.position = "none",
#     strip.text = element_text(size = 24),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#   )
# p1
# 
# 
# # S. aureus SDG
# p2 = ggplot(b4 %>% filter(PathogenName == "Staphylococcus aureus"), aes(x = val, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Staphylococcus aureus", WHORegionName!="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>% filter(WHORegionName!="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3.5) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 4, max.overlaps = 100) +
#   facet_wrap(~WHORegionName, ncol=6) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "HAQI index",
#     y = 'Proportion resistant (%)',
#     title = bquote("Methicillin-resistant " ~ italic("Staphylococcus aureus"))
#   ) +
#   theme(
#     plot.title = element_text(size=24),
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 18),
#     legend.position = "none",
#     strip.text = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted")
#   )
# p2
# 
# combined_plot = p1+p2+plot_layout(ncol=1)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.X_Benchmark_HAQI_PREV.png"), 
#        plot = combined_plot, 
#        device="png",
#        width = 32, height = 12)
# 
# # ALL TOGETHER
# 
# p1a = ggplot(b4 %>% filter(PathogenName == "Escherichia coli"), aes(x = val, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Escherichia coli",WHORegionName=="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>%filter(WHORegionName=="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3) +
#   scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 3,max.overlaps=100) +
#   theme_minimal() +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "HAQI",
#     y = 'Proportion resistant (%)',
#     title = bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli"))
#   ) +
#   theme(
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 14),
#     legend.position = "none",
#     strip.text = element_text(size = 22),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#   )
# p1a
# 
# p2b = ggplot(b4 %>% filter(PathogenName == "Staphylococcus aureus"), aes(x = val, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Staphylococcus aureus", WHORegionName=="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>% filter(WHORegionName=="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 3, max.overlaps = 100) +
#   #facet_wrap(~WHORegionName, ncol=6) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "HAQI",
#     y = 'Proportion resistant (%)',
#     title = bquote("Methicillin-resistant " ~ italic("Staphylococcus aureus"))
#   ) +
#   theme(
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 14),
#     legend.position = "right",
#     strip.text = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted")
#   )
# p2b
# 
# combined_plot = p1a+p2b + plot_layout(ncol=2)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.X_Benchmark_HAQI_PREV_global.png"), 
#        plot = combined_plot, 
#        device="png",
#        width = 16, height = 6)
# 
# 
# # AGAINST TESTING COVERAGE
# # REGIONAL
# #-----------------------------------------------------------------------------------
# 
# p3c = ggplot(b4 %>% filter(PathogenName == "Escherichia coli"), aes(x = BCI_permillion, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Escherichia coli",WHORegionName!="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>%filter(WHORegionName!="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3.5) +
#   facet_wrap(~WHORegionName, ncol=7) + 
#   #scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 4,max.overlaps=100) +
#   theme_minimal() +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "BCI per million",
#     y = 'Proportion resistant (%)',
#     title = bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli"))
#   ) +
#   theme(
#     plot.title=element_text(size=24),
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 18),
#     legend.position = "none",
#     strip.text = element_text(size = 24),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#   )
# p3c
# 
# p3d = ggplot(b4 %>% filter(PathogenName == "Staphylococcus aureus"), aes(x = BCI_permillion, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Staphylococcus aureus", WHORegionName!="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>% filter(WHORegionName!="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 3, max.overlaps = 100) +
#   facet_wrap(~WHORegionName, ncol=6) +
#   theme_minimal() +
#   #scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "BCI per million",
#     y = 'Proportion resistant (%)',
#     title = bquote("Methicillin-resistant " ~ italic("Staphylococcus aureus"))
#   )+
#  theme_minimal() +
#   theme(
#     plot.title=element_text(size=24),
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 18),
#     legend.position = "none",
#     strip.text = element_text(size = 24),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#   )
# p3d
# combined_plot = p3c+p3d + plot_layout(ncol=1)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.X_Benchmark_TESTING_PREV.png"), 
#        plot = combined_plot, 
#        device="png",
#        width = 32, height = 12)
# 
# # GLOBAL
# #-----------------------------------------------------------------------------------
# p3a = ggplot(b4 %>% filter(PathogenName == "Escherichia coli"), aes(x = BCI_permillion, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Escherichia coli",WHORegionName=="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>%filter(WHORegionName=="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3.5) +
#   #scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 4,max.overlaps=100) +
#   theme_minimal() +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "BCI per million",
#     y = 'Proportion resistant (%)',
#     title = bquote("3rd-gen cephalosporins resistant " ~ italic("E. coli"))
#   ) +
#   theme(
#     plot.title=element_text(size=24),
#     axis.text.x = element_text(size = 18),
#     axis.text.y = element_text(size = 18),
#     axis.title.x = element_text(size = 20), # X-axis label
#     axis.title.y = element_text(size=20),
#     legend.text = element_text(size = 18),
#     legend.position = "none",
#     strip.text = element_text(size = 24),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#   )
# p3a
# 
# p3b = ggplot(b4 %>% filter(PathogenName == "Staphylococcus aureus"), aes(x = BCI_permillion, y = w_prev*100, col = WHORegionName)) +
#   geom_hline(data = c1 %>% filter(PathogenName == "Staphylococcus aureus", WHORegionName=="Global"), 
#              aes(yintercept = median*100, col = WHORegionName), linetype = "dashed", size=0.8,alpha=0.4) +
#   geom_vline(data = hr %>% filter(WHORegionName=="Global"), 
#              aes(xintercept = m_val, col = WHORegionName), linetype = 2, size=1, alpha=0.4) +
#   geom_point(size=3) +
#   #geom_text(aes(label = Iso3), vjust = -1.5, hjust=0.5, size = 3) + 
#   geom_text_repel(aes(label = Iso3), size = 3, max.overlaps = 100) +
#   #facet_wrap(~WHORegionName, ncol=6) +
#   theme_minimal() +
#   #scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   #scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80,100)) +
#   scale_colour_manual(values = facet_colors3, name="") +
#   labs(
#     x = "BCI per million",
#     y = 'Proportion resistant (%)',
#     title = bquote("Methicillin-resistant " ~ italic("Staphylococcus aureus"))
#     )+
#       theme_minimal() +
#       theme(
#         plot.title=element_text(size=24),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_text(size = 20), # X-axis label
#         axis.title.y = element_text(size=20),
#         legend.text = element_text(size = 18),
#         legend.position = "none",
#         strip.text = element_text(size = 24),
#         panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#         panel.grid.minor = element_line(linetype = "dotted"),
#       )
# p3b
# combined_plot = p3a+p3b + plot_layout(ncol=2)
# 
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.X_Benchmark_BCIperMillion_PREV_global.png"), 
#        plot = combined_plot, 
#        device="png",
#        width = 16, height = 6)





# 4.2.2 Resistance incidence by number of samples tested
###################################################################



# Check completeness
# num_test = adataAC %>% group_by(Year, Iso3) %>%
#   reframe(
#     num.test.total = sum(NumSampledPatients, rm.na=T),
#   )  %>% group_by(Year) %>%
#   reframe(
#     total.missing.tested = sum(is.na(num.test.total)),
#     total.not.missing.tested = sum(!is.na(num.test.total)),
#     prop.missing = round(total.missing.tested/(total.not.missing.tested+total.missing.tested),2)
#   )
# 
# num_test_byregion = adataAC %>% group_by(Year, Iso3,WHORegionCode) %>%
#   reframe(
#     num.test.total = sum(NumSampledPatients, rm.na=T),
#   )  %>% group_by(Year, WHORegionCode) %>%
#   reframe(
#     total.missing.tested = sum(is.na(num.test.total)),
#     total.not.missing.tested = sum(!is.na(num.test.total)),
#     prop.missing = round(total.missing.tested/(total.not.missing.tested+total.missing.tested),2)
#   )
# 
# View(num_test_byregion)
# 
# p1 = ggplot(num_test_byregion, aes(x = WHORegionCode, y = prop.missing*100, fill = WHORegionCode)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "WHO Region Code", y = "% Countries with missing data", title = "% countries with missing data on number of tested patients - by region") +
#   theme_minimal() +
#   theme(
#     legend.position = "none"
#   ) +
#   facet_wrap(. ~ Year)
# 
# p2 = ggplot(num_test, aes(x = Year, y = prop.missing*100, fill = factor(Year))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Year", y = "% Countries with missing data", title = "% countries with missing data on number of tested patients - Overall") +
#   theme_minimal() + 
#   theme(
#     legend.position = "none"
#   )
# 
# pdf(paste0(dirOutput, "/Descriptive/missing_data_n_tested.pdf"), width = 8, height = 8) # Open a PDF device for saving
# multiplot(p2, p1)
# dev.off()

------------------------------------------------------------
# Decided that number tested is not complete enough
------------------------------------------------------------
  
 
# Resistance map with bubbles for Specimen and Pathogen
#######################################################################


# CHAPTER 5: EAR DATA
###################################################################################

# world_un <- st_read(paste0(dirDataNew,"/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp"))
# head(world_un)
# 
# world_data <- world_un %>%
#   rename(Iso3 = ISO_A3) %>%
#   st_make_valid()
# 
# WHO_region_mapping <- idata_country %>% 
#   select(c(Iso3, WHORegionCode)) %>%
#   rename(WHORegion = WHORegionCode)
# 
# 
# world_data <- world_data %>%
#   left_join(WHO_region_mapping, by="Iso3") %>%
#   filter(!is.na(WHORegion))
# 
# # Dissolve (aggregate) the geometry by WHORegion to ensure entire regions are colored
# who_region_geometry <- world_data %>%
#   group_by(WHORegion) %>%
#   summarise(geometry = st_union(geometry), .groups = 'drop')
# 
# # Summarize the data by WHO Region
# eregion_summary <- edata %>%
#   group_by(WHORegion, Year) %>%
#   summarise(events = n(),
#             pathogen_resistance = paste(unique(paste(PathogenName, Resistance, sep = ": ")), collapse = "\n"),
#             .groups = 'drop')
# 
# # Merge with summary data to maintain information for labels and dots
# eregion_summary <- left_join(eregion_summary, who_region_geometry, by = "WHORegion")
# 
# 
# #Define vertical and horizontal offsets for each WHO region
# region_offsets <- data.frame(
#   WHORegion = c("AMR", "AFR", "EMR", "WPR", "SEA", "EUR"),
#   x_offset = c(-1.5, -1.5, 1.5, 1.5, -1.5, 1.5),
#   y_offset = c(-1.5, -1.5, 1.5, 0.5, -0.5, 1.5)
# )
# 
# # Add offsets to the summary data
# eregion_summary <- eregion_summary %>%
#   left_join(region_offsets, by = "WHORegion") %>%
#   mutate(
#     centroid_coords = st_coordinates(st_centroid(geometry)),
#     label_x = centroid_coords[,1] + x_offset,
#     label_y = centroid_coords[,2] + y_offset
#   )
# 
# # Summarize the data by WHO Region
# eregion_eartype <- edata %>%
#   group_by(WHORegion, Year, `EAR Type`) %>%
#   summarise(events_type = n(),
#             .groups = 'drop')
# 
# 
# 
# # Create the map plot
# map_plot <- ggplot(data = world_data) +
#   geom_sf(aes(fill = WHORegion), color = "white") +  # Color the entire WHO Region
#   scale_fill_manual(values = c(palette5, "grey"), name = "WHO Region") +
#   
#   # Add labels with specific alignment and no overlap
#   geom_label_repel(data = eregion_summary, aes(x = label_x, 
#                                                y = label_y, 
#                                                label = paste0(WHORegion, "\nYear: ", Year, "\nEvents: ", events, "\n", pathogen_resistance),
#                                                fill = WHORegion),
#                    size = 3, box.padding = 0.5, point.padding = 0.5, 
#                    segment.color = 'grey50', segment.size = 0.5, 
#                    nudge_x = 7, nudge_y = 5, max.overlaps = Inf) +
#   
#   # Plot jittered dots for each year at the centroid
#   geom_point(data = eregion_summary, aes(geometry = st_centroid(geometry), size = events),
#              stat = "sf_coordinates", position = position_jitter(width = 5, height = 5)) +
#   
#   labs(title = "AMR Reports by WHO Region",
#        fill = "WHO Region",
#        size = "Number of Events",
#        color = "WHO Region") +
#   
#   theme_minimal() +
#   theme(legend.position = "right",
#         axis.text = element_blank(),
#         axis.title = element_blank())
# 
# # Create the stacked bar plot
# bar_plot <- ggplot(data = eregion_summary, aes(x = factor(Year), y = events, fill = WHORegion)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = c(palette5, "grey"), name = "WHO Region") +
#   labs(x = "Year", y = "Number of Events\nby WHO region", fill = "WHO Region") +
#   theme_minimal() +
#   #guides(fill = guide_legend(nrow = 2)) +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size = 10),
#         panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank()  # Remove minor grid lines
#   )
# 
# # Convert the bar plot to a grob (graphical object)
# bar_grob <- ggplotGrob(bar_plot)
# 
# # Define a new color palette for the EAR event types
# new_palette <- brewer.pal(7, "Set3")
# 
# # Create the bar plot using the new color palette
# bar_type_plot <- ggplot(data = eregion_eartype, aes(x = factor(Year), y = events_type, fill = `EAR Type`)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = new_palette, name = "EAR type") +  # Use the new palette
#   labs(x = "Year", y = "Number of Events\nby type", fill = "WHO Region") +
#   theme_minimal() +
#   guides(fill = guide_legend(nrow = 3)) +
#   theme(legend.position = "bottom",
#         axis.text = element_text(size = 8),
#         axis.title = element_text(size = 10),
#         panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank()  # Remove minor grid lines
#   )
# 
# # Convert the bar plot to a grob (graphical object)
# bar_type_grob <- ggplotGrob(bar_type_plot)
# 
# final_plot <- map_plot +
#   annotation_custom(bar_grob, xmin = -200, xmax = -70, ymin = -110, ymax = -30) +  # Adjust for lower left corner
#   # Add the second bar plot (bar_type_grob) as an inset in the lower right corner
#   annotation_custom(bar_type_grob, xmin = 70, xmax = 200, ymin = -110, ymax = -30)  # Adjust for lower right corner
# 
# # Display the final plot
# print(final_plot)
# 
# 
# pdf(paste0(dirOutput, "/Descriptive/EAR_MAP.pdf"), width = 20, height = 33) # Open a PDF device for saving
# # Arrange the plots in a grid with 4 plots per row
# print(final_plot)
# dev.off()
# 


# 4.3 Time series of priority pathogen prevalence
###########################################################

# FIRST MAKE SURE CODE IS RUN TO CREATE r5 dataset (see Fig4.1 - 4.3 prevalence 2022)!!
# r6 = r5 %>% filter(drug_bug %in%c(ppbsi,pputi,SDG, ppgi, ppuro))
# r6$Region2 = ifelse(r6$Region=="Global", "Global", "Region")
# #r7 = r6 %>% filter(PathogenName =="Klebsiella pneumoniae", AntibioticName=="Imipenem")
# 
# p = ggplot(r6 %>% filter(Region!="Global"), aes(x = Year, y = median*100, group=Region, colour=Region, fill=Region)) +
#   geom_line(size=0.8, alpha=0.2) +
#   #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2,linetype=1) +  # Add fill aesthetic for ribbon
#   #geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +  # Bar plot
#   #geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, position = position_dodge(width = 0.9), size = 0.8, alpha = 0.8) +  # Error bars
#   geom_errorbar(aes(ymin = Q2.5*100, ymax = Q97.5*100), 
#                 width = 0.2, 
#                 position = position_dodge(width = 0.5),  # Align error bars with points
#                 size = 1, alpha = 0.8) +  # Error bars
#   geom_point(size = 2, position = position_dodge(width = 0.5))+
#   facet_wrap(~drug_bug, ncol=3, scales="fixed")+
#   scale_colour_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
#   scale_fill_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
#   theme_minimal() +
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +  
#   labs(
#     x="",
#     y=""
#   )+
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(
#       fill = "white",
#       colour = "white"),
#     axis.text.x = element_text(size = 16),
#     axis.text.y = ggtext::element_markdown(size = 18),
#     title = element_text(size = 20),
#     strip.text = element_text(size = 18)  # Change the facet title size
#   )
# p
# 
# p1 = ggplot(r6 %>% filter(Region=="Global"), aes(x = Year, y = median*100, colour=Region, fill=Region)) +
#   geom_line(size=1) +
#   #geom_point()+
#   geom_ribbon(aes(ymin = Q2.5*100, ymax = Q97.5*100), alpha = 0.2,linetype=1) +  # Add fill aesthetic for ribbon
#   scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +  
#   #geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +  # Bar plot
#   #geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, position = position_dodge(width = 0.9), size = 0.8, alpha = 0.8) +  # Error bars
#   #geom_errorbar(aes(ymin = Q2.5*100, ymax = Q97.5*100, width=0))+
#   #geom_point(size=2)+
#   facet_wrap(~drug_bug, ncol=3, scales="fixed")+
#   scale_colour_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
#   scale_fill_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
#   theme_minimal() +
#   labs(
#     x="",
#     y=""
#   )+
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(
#       fill = "white",
#       colour = "white"),
#     axis.text.x = element_text(size = 16),
#     axis.text.y = ggtext::element_markdown(size = 18),
#     title = element_text(size = 20),
#     strip.text = element_text(size = 18)  # Change the facet title size
#   )
# p1
# 
# combined_plot = p1 + p + plot_layout(ncol = 2)
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.3_REGION_trendsNopredict.png"), 
#        plot = combined_plot, 
#        width = 30, height = 30)


