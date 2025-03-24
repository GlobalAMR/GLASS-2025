#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(readxl, rio, lubridate, zoo, ggplot2, Hmisc, stringr,dplyr,
               magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, grid, purrr, brms, cowplot, ggrepel, grid, wesanderson,
               patchwork, ggh4x, ggalluvial, networkD3, tidyr, forcats, ggtext,
               glue, RColorBrewer, scales, svglite, ggrepel, ggnewscale)

#remotes::install_github("glaziou/whomap")

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT/GLASS_final_curated"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
dirDataRaw = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2024 GLASS REPORT/"

dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables"
dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/2021/"
dirDataModeloutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Model_output/model_output/ALL/all_one_model_weakip_set1"

dirOutputReport = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 REPORT WRITING/FINAL 2022 FIGS AND RESULTS NO STATS"

# Load in functions
source("./0_GLASS_functions.R")
source("./0_multiplot.R")

##############################################################
# LOAD IN DATA
##############################################################

# Population data
pdata = read.csv(paste0(dirDataNewO, "/EI_Popdta_180724_EV.csv"), sep=",")       # Population data
pdataDM = read.csv(paste0(dirDataNew, "/EI_PopdtaDM_030924_EV.csv"), sep=",")       # Population data

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_AST_03092024_EV.csv"), sep=",")   # Country data

# Surveillance indicator data
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_Country_030924_EV.csv"), sep=",")                   # Implementation data
# Including healthcare usage data
udata = read.csv(paste0(dirDataNew, "/EI_Implementation_usage_dta_080724_EV.csv"), sep=",")

sdata = read.csv(paste0(dirDataNewO, "/EI_SurveillanceSites_280824_EV.csv"), sep=",") # Surveillance sites
#idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data
#idata_country = read.csv(paste0(dirDataNew,"/EI_Implementationdta_Country_030924_EV.csv"), sep=",")                   # Implementation data

# HAQI data
#haqidata = read.csv(paste0(dirDataRaw, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")
haqidata = read.csv(paste0(dirDataNew, "/EI_HAQIdta_Country_030924_EV.csv"), sep=",")

# AMR data
#adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_Pop_Country_HAQI_030924_EV.csv"), sep=",")   # Country AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_ANALYSES.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_Country_030924_EV.csv"), sep=",")   # Country AMR data
#adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_Pop_Country_030924_EV.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "./updated_summary_dbc_longformat.csv"), sep=",")

# EAR data
edata = read_excel(paste0(dirDataNew, "/EAR_events_2019_2024_EV.xlsx"))

# rrates 2021
rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
  Q1 = as.numeric(Q1),
  Q3 = as.numeric(Q3),
  median = as.numeric(median)
)

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

# Bed density
bdays = read.csv(paste0(dirDataNewO, "/EI_BedDensitydta_180724_EV.csv"), sep=",")

# Standard population
# See also here https://epirhandbook.com/new_pages/standardization.html
standard_pop_data <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/world_standard_population_by_sex.csv")

###################################################################
# PREAMBLE
###################################################################

# Link bed days with country data to get regions
bdays = left_join(bdays, cdata, by="Iso3")

adataAC = left_join(adataAC, bdays%>%dplyr::select(Iso3, Year, BedDensityPer100000Pop), by=c("Iso3", "Year"))
adataAC = left_join(adataAC, udata%>%dplyr::select(Iso3, Year, amr_glass_inpatient_day_number), by=c("Iso3", "Year"))


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

dbdata$combined[which(!dbdata$combined %in% dbtotal)] # both are in the dataset


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

# 3. Combine All Shapefiles
world_combined <- world_un %>%
  mutate(category = "Recognized Territory") %>%
  bind_rows(world_disp %>% mutate(category = "Disputed Area")) %>%
  bind_rows(world_disp_borders %>% mutate(category = "Disputed Border"))

world_combined$category = ifelse(world_combined$NAME%in%world_disp_land$NAME, "Disputed Land", world_combined$category)

# all_columns <- union(colnames(world_un), colnames(world_disp))
# all_columns <- union(all_columns, colnames(world_disp_borders))
# 
# # Add missing columns to each shapefile
# missing_cols_un <- setdiff(all_columns, colnames(world_un))
# for (col in missing_cols_un) {
#   world_un[[col]] <- NA
# }
# 
# missing_cols_disp <- setdiff(all_columns, colnames(world_disp))
# for (col in missing_cols_disp) {
#   world_disp[[col]] <- NA
# }
# 
# missing_cols_disp_borders <- setdiff(all_columns, colnames(world_disp_borders))
# for (col in missing_cols_disp_borders) {
#   world_disp_borders[[col]] <- NA
# }
# 
# world_combined <- rbind(world_un, world_disp,world_disp_borders)
# 
# # Which disputed areas to colour
# disp = unique(world_disp$NAME)
# disp_b = unique(world_disp_borders$NAME)
# patterns <- c("Lake","Lakes", "Sea")
# 
# # Combine patterns into a single regex
# pattern_regex <- paste(patterns, collapse = "|")
# 
# # Find indices of matches
# excl <- grep(pattern_regex, disp)
# 
# disp_col = disp[-excl]
# 
# world_combined <- world_combined %>%
#   mutate(category = ifelse(NAME %in% unique(world_disp$NAME), "disputed area",
#                            ifelse(NAME %in% unique(world_disp_borders$NAME), "disputed border", "cta")))

#################################################################################################
# PREAMBLE
#################################################################################################
adataAC = adataAC %>% 
  #filter(!Id %in% c("IdKOS2016","IdKOS2017","IdKOS2018","IdKOS2019","IdKOS2020")) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No"),
    amr_rate = Resistant/InterpretableAST, 
    #BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  )


###################################################################
# FIGURES
###################################################################

# 2	National AMR surveillance implementation indicators
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

# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.1: Map of CTAs enrolled in GLASS
# --------------------------------------------------------------------------------------------------------------------------------------

e1 = read.csv(paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.1_GLASS_enrolled_or_with_data.csv"))
names(e1)

e1 = e1 %>%
  rename(Iso3 = iso3)

unique(e1$var)

world <- world_combined %>%
  rename(Iso3 = "ISO_3_CODE")

# Merge the filtered data with the shapefile data
e2 <- world %>%
  left_join(e1, by = c("Iso3")) #%>%

e3 <- left_join(e2%>%select(-c(WHORegionCode)), cdata%>%select(Iso3, WHORegionCode), by="Iso3")

e4 <- e3 %>%
  mutate(var = ifelse(category =="Disputed Land", "Not applicable", var),
         var = ifelse(is.na(var), "Not enrolled in GLASS", var), # This is for all the islands etc
         var = factor(var, levels=c("2022 AMR data", "AMR data before 2022", 
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

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.1_enrolledglass_2022.svg"), 
       plot = p,
       device = svg,
       dpi = 300,
       width = 10, height = 8)


# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.2: Implementation status, quality assurance, and standards of national AMR surveillance systems in 2022, for CTAs reporting to GLASS-AMR
# --------------------------------------------------------------------------------------------------------------------------------------

# Of those with NRLs, 
# (i) what proportion have an EQA programme (and among them, what proportion also have EQA of local labs, if relevant), 
# (ii) and what proportion are following international standards for AST? 
# (iii) what proportion have an NCC. 
# Also, of those with an NSP, (i) what proportion have an NCC, (ii) what proportion have an NTRL

###################################################################### AMR DATA ####################################################################

f1 = read.csv(paste0(dirDataNewO, "/EI_Implementationdta_280824_EV.csv"), header=T)

#f1<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Implementationdta_280824.csv", header=T)
#attach(f1)
#fix(f1)

f2 = read.csv(paste0(dirDataNewO, "/EI_Countrydta_180724_EV.csv"), header=T)

#f2<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Countrydta_180724.csv", header=T)
attach(f2)
#fix(f2)


f2 = f2[order(f2$Iso3),] %>%
  mutate(
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
      TRUE ~ CountryTerritoryArea)
  )

f3<-merge(f1,f2,by="Iso3",all.x=TRUE)
f4 <-  dplyr::select(f3, - c("Year", "CountryTerritoryArea", "IncomeWorldBankJune2022", "EnrolledAMR", "EnrollmentDateAMR", "EnrolledAMC", "EnrollmentDateAMC", "WHOLegalStatus", "Candida", "EGASP",
                      "AttribMort", "Tricycle")) 



f5<- f4 %>%
  filter(AMR_NCC != "Not_enrolled")%>% 
  droplevels()



f7 <- f5 %>%
  mutate(NCC = case_when((AMR_NCC =="Established")  ~ 'Available',
                         (AMR_NCC =="Establishment in progress" | AMR_NCC =="Not established") ~ 'Not available',
                         (AMR_NCC =="Not reported") ~ 'No data'))

f8 <- f7 %>%
  mutate(NRL = case_when((AMR_NRL =="Established")  ~ 'Available',
                         (AMR_NRL =="Not established") ~ 'Not available',
                         (AMR_NRL =="Not reported") ~ 'No data'))


f9 <- f8 %>%
  mutate(EQA_NRL = case_when((EQA_to_NRL =="Provided")  ~ 'Available',
                             (EQA_to_NRL =="Not provided") ~ 'Not available',
                             (EQA_to_NRL =="Not reported") ~ 'No data'))


f10 <- f9 %>%
  mutate(International_AST_Standards = case_when((AMR_AST_standards =="O")  ~ 'Not available',
                                                 (AMR_AST_standards =="CLSI" | AMR_AST_standards =="EUCAST"| AMR_AST_standards =="EUCAST|CLSI") ~ 'Available',
                                                 (AMR_AST_standards =="Not reported") ~ 'No data'))

f11 <- f10 %>%
  mutate(All_GLASS_labs_With_EQA = case_when((AMR_EQA_GLASS_labs =="Provided to all laboratories")  ~ 'Available',
                                             (AMR_EQA_GLASS_labs =="Not provided to all laboratories") ~ 'Not available',
                                             (AMR_EQA_GLASS_labs =="Not reported") ~ 'No data'))

f12 <-  dplyr::select(f11, - c("AMR_NCC", "AMR_NRL", "EQA_to_NRL", "AMR_AST_standards", "AMR_EQA_GLASS_labs")) 



f12b <- f12 %>%
  mutate(Core = case_when((NCC == "Available" & NRL == "Available" & EQA_NRL =="Available" & International_AST_Standards =="Available")  ~ "Available",
                          (NCC =="No data" | NRL == "No data" | EQA_NRL == "No data" | International_AST_Standards == "No data") ~ "No data",
                          TRUE ~ "Not available"))

f13<-f12b%>% 
  as.data.frame() %>%
  group_by(WHORegionName,Core) %>% 
  summarise(total_count=n()) %>%
  as.data.frame() 

f13b<-f12b%>%
   as.data.frame() %>%
   group_by(Core) %>%
   summarise(total_count=n()) %>%
   as.data.frame() %>%
   mutate(WHORegionName = "Global")

f14 = rbind(f13, f13b)

f15 = f14 %>%
  group_by(WHORegionName)%>%
  summarise(n = sum(total_count))

f16 = left_join(f14,f15)

f17 = f16 %>% mutate(
  Region = factor(WHORegionName,
                   levels = c("Global",
                     "Western Pacific Region",
                     "Eastern Mediterranean Region",
                     "European Region",
                     "South-East Asia Region",
                     "Region of the Americas",
                     "African Region"
                   ))
  )

f18<-f12b%>% 
  as.data.frame() %>%
  group_by(WHORegionName,All_GLASS_labs_With_EQA) %>% 
  summarise(total_count=n()) %>%
  as.data.frame() 

f18b = f12b%>% 
   as.data.frame() %>%
   group_by(All_GLASS_labs_With_EQA) %>% 
   summarise(total_count=n()) %>%
   as.data.frame() %>%
   mutate(WHORegionName = "Global")

f19 = rbind(f18, f18b)
f20 = f19 %>%
  group_by(WHORegionName)%>%
  summarise(n = sum(total_count))

f21 = left_join(f19,f20)

f21 = f21 %>% mutate(
  Region = factor(WHORegionName,
                  levels = c("Global",
                             "Western Pacific Region",
                             "Eastern Mediterranean Region",
                             "European Region",
                             "South-East Asia Region",
                             "Region of the Americas",
                             "African Region"
                  ))
)

fig3.2a = f17 %>% mutate(
  variable = "Core surveillance components"
)%>%
  rename(
    Value = Core
  ) %>%
  mutate(
    Value = factor(Value, levels= c("Available", "Not available","No data"))
  )

fig3.2b = f21 %>% mutate(
  variable = "GLASS laboratories all with EQA"
) %>%
  rename(
    Value = All_GLASS_labs_With_EQA
  ) %>%
  mutate(
    Value = factor(Value, levels= c("Available", "Not available","No data"))
  )

fig3.2both = rbind(fig3.2a,fig3.2b)

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

cols = brewer.pal(n = 3, name = 'Dark2')
scales::show_col(cols,cex_label = .7) 

p1 <- ggplot(fig3.2a, aes(x = Region, y = total_count / n * 100, fill = Value)) + 
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
      "Available" = 'lightseagreen',
      "Not available" = "brown1",
      "No data" = "grey77"
    ),
    name = "",
    labels = c(
      "Available" = "All available",
      "Not available" = "Not all available",
      "No data" = "Information is incomplete or not reported"
    )
  ) +
  labs(
    x = "",
    y = 'Percentage of enrolled CTAs (%)',
    title = "Core surveillance components",
    subtitle = ""
  ) +
  # Add total number annotation only to the right-hand side panel
  # geom_text(
  #   data = fig3.2both %>% filter(variable == "GLASS laboratories all with EQA"), # Filter for the right panel
  #   aes(
  #     label = paste0("n=", n), 
  #     y =102  # Position slightly outside the bars
  #   ),
  #   hjust = 0,  # Align to the right of the bar
  #   size = 7
  # ) +
  # annotate("text", x = 6, y = 130, label = "Core surveillance components:
  #  National coordination centre; National reference laboratory \n with participation in EQA; 
  #           Use of international AST standards", size = 4)+
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = c(0,20,40,60,80,100)# Add 20% space on the upper end of the y-axis
  ) +
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
    plot.margin = unit(c(1, 1, 2.7, 1), "cm"),
    legend.position = c(0, 0),              # Move legend to bottom-left corner
    legend.justification = c(0, 2.2)
    )+
  guides(fill = guide_legend(nrow = 3))+
  coord_flip()

p1

p2 <- ggplot(fig3.2b, aes(x = Region, y = total_count / n * 100, fill = Value)) + 
  # geom_bar(
  #   data = dummy_data, # Plot the grey bars first
  #   aes(y = total_count),
  #   stat = "identity",
  #   fill = "grey97",
  #   color = NA
  # ) +
  geom_bar(stat = "identity", position = position_stack(reverse=TRUE)) + # Overlay the actual data
  # geom_text(
  #   aes(label = paste0(total_count)), # Add region and number of countries
  #   position = position_stack(reverse = TRUE, vjust=0.5), # Position text in the middle of the bar
  #   size = 5, # Adjust size of the text
  #   color = "white"
  # ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Available" = 'lightseagreen',
      "Not available" = "brown1",
      "No data" = "grey77"
    ),
    name = "",
    labels = c(
      "Available" = "Provided to all laboratories",
      "Not available" = "Not provided to all laboratories",
      "No data" = "Information is incomplete or not reported"
    )
  ) +
  labs(
    x = "",
    y = 'Percentage of enrolled CTAs (%)',
    title = "External quality assurance of laboratories reporting to GLASS",
    subtitle = ""
  ) +
  # Add total number annotation only to the right-hand side panel
  geom_text(
    data = fig3.2both %>% filter(variable == "GLASS laboratories all with EQA"), # Filter for the right panel
    aes(
      label = paste0("n=", n), 
      y =102  # Position slightly outside the bars
    ),
    hjust = 0,  # Align to the right of the bar
    size = 7
  ) +
  # annotate("text", x = 6, y = 130, label = "Core surveillance components:
  #  National coordination centre; National reference laboratory \n with participation in EQA; 
  #           Use of international AST standards", size = 4)+
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = c(0,20,40,60,80,100)# Add 20% space on the upper end of the y-axis
  ) +
  theme(
    plot.subtitle = element_text(hjust = 0, size=16),
    plot.title = element_text(size = 24),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 22,
                                margin = margin(t = 16), hjust=0.5), # X-axis label
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.title = element_blank(),
    #legend.position = "bottom",
    strip.text.x = element_text(size = 22, hjust = 0),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    plot.margin = unit(c(1, 1, 2.7, 1), "cm"),
    legend.position = c(0, 0),              # Move legend to bottom-left corner
    legend.justification = c(0, 2.2)
  )+
  guides(fill = guide_legend(nrow = 3))+
  coord_flip()

p2
combined_plot <- p1 + p2 + plot_layout(ncol = 2)  # Arrange in 2 columns
combined_plot

# For report
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.2_Implementation_status_2022.svg"), 
       plot = combined_plot,
       device = svg,
       dpi = 300,
       width = 26, height = 10)

# For writing
write.csv(fig3.2a, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.2a_Implementation_status.csv"))
write.csv(fig3.2b, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.2b_Implementation_status.csv"))
write.csv(f12b, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Implementation_status_all_variables.csv"))

# --------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.3(a-d): Map of BCIs with AST results reported to GLASS-AMR per one million population by CTA per infectious syndrome in 2022; 
# a) Bloodstream infections; b) Urinary tract infections; c) Gastrointestinal tract infections; and d) Urogenital infections.
# --------------------------------------------------------------------------------------------------------------------------------------

# Get testing and AMR rates
m1 = adataAC %>% 
  filter(InReport == "Yes" & TotalSpecimenIsolates>0) %>%   # Filter the data first
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

table(m2$Bins8q[m2$Year==2022])

m3<-m2 %>% mutate(var  = Bins8q)

m3$var <- factor(m3$var, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))


breaks = m3 %>%
  filter(!is.na(var)) %>%   # Remove rows where 'var' is NA
  group_by(var) %>%         # Group by 'var'
  summarise(max_BCI_permillion = max(BCI_permillion, na.rm = TRUE))
breaks$max_BCI_permillion = round(breaks$max_BCI_permillion,0)

breaks = breaks[[2]]
labels <- as.character(c(0,round(breaks,1)[1:7]))

fig3.3 = m3
p1 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2022, specimen = "BLOOD", palette = palette_map4, na_color = "white", breaks=breaks, labels=labels)
p2 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2022, specimen = "URINE", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)
p3 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2022, specimen = "STOOL", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)
p4 <- plot_bci_map_chlor(shapefile = world_combined, bci_data = fig3.3, year = 2022, specimen = "UROGENITAL", palette = palette_map4, na_color = "white",breaks=breaks, labels=labels)

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

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3a_BCIperMillion_BSI_2022.svg"), 
       plot = p1,
       device = svg, width = 10, height = 8, dpi = 300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3c_BCIperMillion_UTI_2022.svg"), 
       plot = p2,
       device = svg, width = 10, height = 8, dpi = 300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3b_BCIperMillion_GI_2022.svg"), 
       plot = p3,
       device = svg, width = 10, height = 8, dpi = 300)

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Figure_3.3d_BCIperMillion_URO_2022.svg"), 
       plot = p4,
       device = svg, width = 10, height = 8, dpi = 300)

# For writing
write.csv(fig3.3, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.3_BCIperMillion_specimen.csv"))
#write.csv(world_combined, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/shapefile_disp.csv"))




# 3.3	GLASS-AMR surveillance coverage 
###################################################################

# TABLE 3.1 BCI PER PATHOGEN
#-------------------------------------------------------------------------------------------------
# Get testing by pathogen
m0 <- adataAC %>%
  filter(InReport == "Yes" & TotalSpecimenIsolates > 0) %>%   # Filter valid reports
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


cta_region_2016_2022 = cta_region %>% 
  group_by(WHORegionName, Specimen) %>%
  summarise(n = max(n))%>%
  mutate(Period = "2016-2022")

cta_region_2022 = cta_region %>% filter(Year==2022)%>%
  mutate(Period = "2022") %>%
  select(WHORegionName, Specimen, Period,n)

cta2016_2022 = cta_region_2016_2022 %>%
  group_by(Specimen) %>%
  summarise(
    n = sum(n)
  ) %>%
  mutate(Period = "2016-2022")

cta2022= cta_region_2022 %>%
  group_by(Specimen) %>%
  summarise(
    n = sum(n)
  ) %>%
  mutate(Period = "2022")

# Summarize for 2016–2022
summary_2016_2022 <- m1 %>%
  group_by(WHORegionName, Specimen, PathogenName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2016-2022")

# Summarize for 2022
summary_2022 <- m1 %>%
  filter(Year == 2022) %>%
  group_by(WHORegionName, Specimen, PathogenName) %>%
  summarise(TotalIsolates = sum(TotalPathogenIsolateswithAST, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = "2022")

# Combine Summaries
summary_combined <- bind_rows(summary_2016_2022, summary_2022)

# Compute unique country counts (per Pathogen/Specimen/Period)
country_counts <- m0 %>%
  filter(PathogenIsolateswithAST > 0) %>%  # Only include isolates with AST data
  group_by(WHORegionName, Specimen, PathogenName, Year) %>%
  summarise(CountryCount = n_distinct(Iso3), .groups = "drop") %>%
  mutate(Period = ifelse(Year == 2022, "2022", "2016-2022")) %>%
  group_by(WHORegionName, Specimen, PathogenName, Period) %>%
  summarise(CountryCount = max(CountryCount, na.rm = TRUE), .groups = "drop")

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
summary_with_totals_2022 = summary_with_totals %>%filter(Period == 2022)
summary_with_totals_2016_2022 = summary_with_totals %>%filter(Period != 2022)
TotalwithCountries2016_2022 = summary_with_totals_2016_2022$TotalWithCountries
TotalwithCountries2022 = summary_with_totals_2022$TotalWithCountries

summary_table = cbind(summary_with_totals_2022%>%select(WHORegionName, Specimen,PathogenName),
                      TotalwithCountries2016_2022,TotalwithCountries2022)

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

cta_c = rbind(cta2016_2022,cta2022) # If this gives error, copy paste in the console and it works
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
summary_with_totals_2022_global = final_global_table %>%filter(Period == 2022)
summary_with_totals_2016_2022_global = final_global_table %>%filter(Period != 2022)
TotalwithCountries2016_2022_global = summary_with_totals_2016_2022_global$TotalWithCountries
TotalwithCountries2022_global = summary_with_totals_2022_global$TotalWithCountries

summary_table_global = cbind(summary_with_totals_2022_global%>%select(WHORegionName, Specimen,PathogenName),
                             TotalwithCountries2016_2022_global,TotalwithCountries2022_global) 

summary_table_global = summary_table_global %>%
  rename(
    TotalwithCountries2016_2022 = TotalwithCountries2016_2022_global,
    TotalwithCountries2022 = TotalwithCountries2022_global
  )

# Combine regional and global Summaries
final_table <- bind_rows(summary_table, summary_table_global) %>%
  arrange(Specimen, PathogenName, WHORegionName)

summary_table_wide <- final_table %>%
  pivot_wider(
    names_from = c(WHORegionName),  # Combine region and period for column names
    values_from = c("TotalwithCountries2016_2022", "TotalwithCountries2022"),           # Values come from TotalIsolates
    values_fill = "0 (0)"                        # Fill missing values with 0
  ) %>% dplyr::select(Specimen, PathogenName, 'TotalwithCountries2016_2022_Global','TotalwithCountries2022_Global',
                      'TotalwithCountries2016_2022_African Region','TotalwithCountries2022_African Region',
                      'TotalwithCountries2016_2022_Region of the Americas','TotalwithCountries2022_Region of the Americas',
                      'TotalwithCountries2016_2022_South-East Asia Region','TotalwithCountries2022_South-East Asia Region',
                      'TotalwithCountries2016_2022_European Region','TotalwithCountries2022_European Region',
                      'TotalwithCountries2016_2022_Eastern Mediterranean Region','TotalwithCountries2022_Eastern Mediterranean Region',
                      'TotalwithCountries2016_2022_Western Pacific Region','TotalwithCountries2022_Western Pacific Region')

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
                 "2016-2022", "2022", 
                 "2016-2022", "2022", 
                 "2016-2022", "2022", 
                 "2016-2022", "2022", 
                 "2016-2022", "2022", 
                 "2016-2022", "2022", 
                 "2016-2022", "2022")

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

df$Specimen = c("","", "","","","","","","Bloodstream","","",
                "Gastrointestinal tract","", "","Urinary tract", "", "Gonorrhoea (N. gonorrhoea)")
df = df[df$PathogenName!="Neisseria gonorrhoeae",]
df$PathogenName = c("", "","Acinetobacter spp.","E. coli","K. pneumoniae",   
                    "Salmonella spp.","S. aureus","S. pneumoniae", "Total","Salmonella spp.",         
                    "Shigella spp.","Total","E. coli","K. pneumoniae","Total",                   
                     "")

df = df[c(1,2,9,3,4,5,6,7,8,12,10,11,15,13,14,16),]

# FOR SUMMARY TABLE
#tab3.1 = rbind(summary_with_totals, final_global_table)

# For writing
#write.csv(tab3.1, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.3b_BCIperMillion_specimenbypathogen.csv"))

# For report
write.csv(df, file = paste0(dirOutputReport, "/Chapter 3/Ch3 Tables/Table3.1_BCIperMillion_specimenbypathogen.csv"))


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

# 3.4 CRITICAL INFORMATION GAPS in GLASS-AMR
###################################################################

# Table 3.1: Information gaps in GLASS-AMR that impact the interpretation of AMR surveillance data by type  
#---------------------------------------------------------------------------------------------------------------

c1 = read.csv(file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata.csv"))
c1 = c1 %>% select(!X)

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
    Datatype = factor(Datatype, levels=c("Surveillance_core_components", "Surveillance_coverage", "AMR_covariate_denominator",
                                         "AMR_specimen", "Total_Score")),
    Value_label = case_when(
      Value == 0 ~ "No data",
      Value == 1 ~ "Some data",
      Value == 2 ~ "Good data",
      Value %in% c(3:7) ~ "Low completeness",
      Value %in% c(8:14) ~ "Medium-low completeness",
      Value %in% c(15:21) ~ "Medium-high completeness",
      Value %in% c(22:28) ~ "High completeness",
      is.na(Value) ~ "No data"
    )
    
    # Value = fct_explicit_na(as.factor(Value), na_level = "Missing Data"),
  )
c2$Value = factor(c2$Value, levels=sort(unique(c2$Value)))

c2$Value_label = factor(c2$Value_label, levels = c("Low completeness","No data", "Medium-low completeness","Some data",
                                                                 "Medium-high completeness", "Good data", "High completeness"))

c3 = left_join(c2, cdata %>%select(Iso3, WHORegionName, WHORegionCode, IncomeWorldBankJune2022), by="Iso3")

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
  "ColO_nURINE_URINE" = "UTI >10 with AST",
  "ColP_nSTOOL_STOOL" = "GI >10 with AST",
  "ColQ_nURO_UROGENITAL" = "Urogenital >10 with AST"
)

value_colors <- c(
  "No data" = "white",
  "Some data" = reds[3], 
  "Good data" = blues[3], 
  "Low completeness" = "#e0f3db",
  "Medium-low completeness" = "#a8ddb5",
  "Medium-high completeness" = "mediumseagreen", 
  "High completeness" = "darkgreen", 
  "Not applicable (no 2022 GLASS data)" = "#f0f0f0"
)

# Create the heatmap with top-aligned x-axis and grouped categories
p = ggplot(c3, aes(x = Category, y = fct_rev(fct_reorder(Iso3, Iso3)), fill = as.factor(Value_label))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = value_colors,
    na.value = "grey90",
    labels = legend_labels <- c(
      "No data" = "No data",
      "Some data" = "Limited data", 
      "Good data" = "Data available",
      "Low completeness" = "Completeness: \nLow",
      "Medium-low completeness" = "Completeness: \nMedium-low",
      "Medium-high completeness" = "Completeness: \nMedium-high", 
      "High completeness" = "Completeness: \nHigh", 
      "Not applicable (no 2022 GLASS data)" = "Not applicable for 2022"
    ),
    name = ""
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c(NA, "black", NA, NA, NA,NA, NA)  # Only show legend for 0, 1, 2, and NA values
      )
    )
  )+
  labs(x = "", y = "") +
  facet_grid(WHORegionName ~ Datatype, scales = "free", space = "free",
             labeller = labeller(
               Datatype = as_labeller(c(
                 "Surveillance_core_components" = "Building \nblocks \nsurveillance",
                 "Surveillance_coverage" = "Surveillance \ncoverage data",
                 "AMR_covariate_denominator" = "AMR data \n(covariate and denominator)",
                 "AMR_specimen" = "AMR data \n(specimen)",
                 "Total_Score" = "Total Score"
               )),
               WHORegionName = as_labeller(c(
                 "African Region" = "African Region",
                 "Region of the Americas" = "Region of the \nAmericas",
                 "South-East Asia Region" = "South-East\nAsia Region",
                 "European Region" = "European Region",
                 "Eastern Mediterranean Region" = "Eastern Mediterranean Region",
                 "Western Pacific Region" = "Western\nPacific Region"
               ))
             )
  )+
  #coord_fixed(ratio = 1) + 
  theme_minimal() +
  scale_x_discrete(
    position = "top",                # Move x-axis labels to the top
    labels =  c("colB_nssi" = "Core surveillance\npresent",
                "colC_infs_inp" = "N facilities\ninpatient",
                "colD_util_inp" = "N admissions/\nbed days inpatient",
                "colE_util_outp" = "N consultations\noutpatient",
                "ColF_GLASS_infs_inp" = "N GLASS \nfacilities inpatient",
                "ColG_GLASS_util_inp" = "N GLASS \nadmissions/\nbed days \ninpatient",
                "ColH_GLASS_util_outp" = "N GLASS consultations\noutpatient",
                "ColI_nsamp" = "N tested \nreported",
                "ColK_age" = "Age \nreported",
                "ColL_sex" = "Sex \nreported",
                "ColM_origin" = "Infection origin\nreported",
                "ColN_nBLOOD_BLOOD" = "BSI >10 \nwith AST",
                "ColO_nURINE_URINE" = "UTI >10 \nwith AST",
                "ColP_nSTOOL_STOOL" = "GI >10 \nwith AST",
                "ColQ_nURO_UROGENITAL" = "Urogenital >10 \nwith AST",
                "TotalScore" = "Total Score"
    )
  )+ 
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    axis.title.x = element_blank(),
    axis.text.x.top = element_text(angle = 90, hjust = 0, vjust=0.5),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(face = "bold", size = 10)
  ) #+ guides(fill="none")
p

# FOR REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Fig_3.5_missingdata.svg"), 
       plot = p, 
       device="svg",
       width = 14, height = 20)


# 4	GLOBAL AND REGIONAL RESISTANCE TO ANTIBIOTICS
###################################################################

# Generate a single global estimate of resistance by using weights to account for 
# testing coverage and potential bias in the data from each setting
# Hence also allow for comparison between regions and potentially over time.

# 4.1	Resistance to antibiotics under surveillance in 2022	 
###################################################################

#r = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/REGION_w_prev_allALLYEARSnopredict.csv"))
priorset = "all_one_model_weakip_set1"
sensitivity = "fixed_allyears_nopredict"

# 
r = read.csv(file.path(dirOutput, "Analyses/Section4.1_Global_AMR_prevalence/Data_tables", priorset,sensitivity, "REGION_w_prev_all.csv"))
#r = read.csv(file.path(dirDataModeloutput, "Model_prevalence_estimates/Data_tables_prevalence",sensitivity, "REGION_w_prev_all.csv"))

# drug_bug = r$drug_bug
# r$combined = gsub("Co trimoxazole","Co-trimoxazole", r$drug_bug)
# r$combined = gsub("Methicillin resistance","Methicillin-resistance", r$combined)
# r$combined = gsub("Third generation cephalosporins","Third-generation cephalosporins", r$combined)

r1 = r %>%
  mutate(
    drug_bug = combined
  )

a = adataAC %>% select(AntibioticName, Antibiotic, Pathogen, PathogenName) %>%
  distinct()

r1 = left_join(r,a)
r1$drug_bug = gsub("Co trimoxazole","Co-trimoxazole", r1$drug_bug)
r1$drug_bug = gsub("Methicillin resistance","Methicillin-resistance", r1$drug_bug)
r1$drug_bug = gsub("Third generation cephalosporins","Third-generation cephalosporins", r1$drug_bug)


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
                              (Antibiotic == "COL") ~ 'Polymoxins',
                              (Antibiotic == "MNO" | Antibiotic == "TGC") ~ 'Tetracyclines',
                              (AntibioticName=="Third-generation cephalosporins"| AntibioticName == "Methicillin-resistance") ~ "SDG"
  ),
  Grouping = factor(Grouping, levels=c("Aminoglycocides", "Carbapenems", "3rd-gen. cephalosporins", "4th-gen. cephalosporins",
                                       "Fluoroquinolones", "Macrolides","Penicillins", "Polymoxins", 
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


region = cdata %>% 
  ungroup() %>% select(WHORegionCode, WHORegionName) %>%
  distinct()

r3 = r2 %>% 
  left_join(region) %>%
  mutate(
    WHORegionName = ifelse(is.na(WHORegionName), "Global", WHORegionName)
  )


r4 = r3 %>%
  select(WHORegionName, Year, median, Q2.5, Q97.5, Specimen,
         PathogenName, AntibioticName, Grouping, drug_bug, n, Resistant, InterpretableAST)

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
  n_cutoff = ifelse(Region=="Global"& n<10, "No",
                    ifelse(Region!="Global" & n<4, "No", "Yes"))
  )

r6 = r5%>%filter(Year==2022)

# FOR SOME REASONS NA FOR SEA FOR BLOOD-Escherichia coli-Ampicillin; URINE-Escherichia coli-Ampicillin and STOOL-Shigella spp.-Ciprofloxacin

fig4.1 = r6 %>%filter(!Grouping=="SDG")

# A. bacter + E. coli + Klebs 
p4.1 <- plot_pathogen_antibiotic(
  df = fig4.1,
  pathogens = c("Acinetobacter spp.", "Escherichia coli", "Klebsiella pneumoniae",
                "Salmonella spp.", "Streptococcus pneumoniae"),
  specimen = "BLOOD",
  custom_labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                    "Salmonella spp.", "S. pneumoniae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)

# p4.1b <- plot_pathogen_antibiotic(
#   df = fig4.1,
#   pathogens = c("Salmonella spp.", "Streptococcus pneumoniae"),
#   specimen = "BLOOD",
#   custom_labels = c("Salmonella spp.", "S. pneumoniae"),
#   custom_title = "",
#   palette = facet_colors3  # Custom color palette
# )
# p4.1b

p4.3a <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "URINE",
  pathogens = c("Escherichia coli", "Klebsiella pneumoniae"),
  custom_labels = c("E. coli", "K. pneumoniae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
#p4.3a

p4.3b <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "STOOL",
  pathogens = c("Salmonella spp.", "Shigella spp."),
  custom_labels = c("Salmonella spp.", "Shigella spp."),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
p4.3b

p4.3c <- plot_pathogen_antibiotic(
  df = fig4.1,
  specimen = "UROGENITAL",
  pathogens = c("Neisseria gonorrhoeae"),
  custom_labels = c("N. gonorrhoeae"),
  custom_title = "",
  palette = facet_colors3  # Custom color palette
)
p4.1c


ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity,"/Figure_4.1a_bsi_aekss2022.png"), 
       plot = p4.1, 
       width = 40, height = 15)

# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity, "/Figure_4.1b_bsi_ss2022.png"), 
#        plot = p4.1b, 
#        width = 40, height = 7)

ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity, "/Figure_4.1c_uti2022.png"), 
       plot = p4.3a, 
       width = 40, height = 12)

ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity,"/Figure_4.1d_gi2022.png"), 
       plot = p4.3b, 
       width = 40, height = 7)

ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity,"/Figure_4.1e_gon2022.png"), 
       plot = p4.3c, 
       width = 40, height = 5)

combined_plot <- p4.1a +p4.3a + p4.3b + p4.3c + plot_layout(ncol = 1)  # Arrange in 2 columns
combined_plot

# ALL TOGETHER
ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/",priorset,"/",sensitivity,"/Figure_4.1_all.pdf"), 
       plot = combined_plot, 
       width = 40, height = 50, limitsize=F)

# FOR REPORT 
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.1_bsi.svg"), 
       plot = p4.1, 
       device = "svg",
       width = 30, height = 25, limitsize=F)

# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Figure_4.1b_bsi.png"), 
#        plot = p4.1b, 
#        width = 40, height = 7)

#combined_plot = p4.1b+p4.1b2+p4.1b3+plot_layout(ncol=1)

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

write.csv(fig4.1, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.1_4.2_4.3_REGIONALprevalence2022.csv"))



# FOR ECCMID
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



# 4.1.1f SDG
fig4.2 = r5%>%filter(drug_bug=="BLOOD-Escherichia coli-Third-generation cephalosporins"&Year==2022)

p4.2a <- ggplot() +
  geom_col(data = fig4.2, 
           aes(x = reorder(WHORegionName, median), y = median * 100, fill = WHORegionName), 
           alpha = 0.5) +
  geom_errorbar(data = fig4.2, 
                aes(x = reorder(WHORegionName, median), ymin = Q2.5 * 100, ymax = Q97.5 * 100), 
                width = 0.3) +
  #geom_text(data = fig4.1f, 
  #          aes(x = reorder(WHORegionName, median), y = median * 100 + 5, 
  #              label = paste0("n = ", n)), 
  #          size = 6, hjust = -0.7) +
  theme_minimal() +
  scale_fill_manual(name = " ", values = facet_colors3) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
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
    y = "Percentage resistant (%)",
    x = ""
  )
p4.2a

fig4.2b = r5%>%filter(drug_bug=="BLOOD-Staphylococcus aureus-Methicillin-resistance"&Year==2022)

p4.2b <- ggplot() +
  geom_col(data = fig4.2b, 
           aes(x = reorder(WHORegionName, median), y = median * 100, fill = WHORegionName), 
           alpha = 0.5) +
  geom_errorbar(data = fig4.2b, 
                aes(x = reorder(WHORegionName, median), ymin = Q2.5 * 100, ymax = Q97.5 * 100), 
                width = 0.3) +
  #geom_text(data = fig4.1f, 
  #          aes(x = reorder(WHORegionName, median), y = median * 100 + 5, 
  #              label = paste0("n = ", n)), 
  #          size = 6, hjust = -0.7) +
  theme_minimal() +
  scale_fill_manual(name = " ", values = facet_colors3) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
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
    subtitle = "&nbsp; <i> S. aureus</i> - Methicillin-resistance",
    y = "Percentage resistant (%)",
    x = ""
  )
p4.2b

combined_plot = p4.2a+p4.2b+plot_layout(ncol=2)

# REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.2_SDG.svg"), 
       plot = combined_plot, 
       device = "svg",
       width = 20, height = 5, limitsize=F)

write.csv(fig4.2, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.2a_REGIONALprevalence2022_3GC.csv"))
write.csv(fig4.2b, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Figure_4.2b_REGIONALprevalence2022_MRSA.csv"))






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






# 4.1.2 Country map
#--------------------------------------------------------------------

d = read.csv(file.path(dirDataModeloutput, "/Model_prevalence_estimates/Data_tables_prevalence/CTA_w_prev_all.csv"))

# drug_bug = d$drug_bug
# d$combined = gsub("Co-trimoxazole","Co trimoxazole", d$drug_bug)
# d$combined = gsub("Methicillin-resistance","Methicillin resistance", d$combined)
# d$combined = gsub("Third-generation cephalosporins","Third generation cephalosporins", d$combined)
# 
# d1 <- d %>% 
#   separate(combined, into = c("Specimen", "PathogenName", "AntibioticName"), sep = "-")
# 
# d1$AntibioticName = ifelse(d1$AntibioticName=="Co trimoxazole", "Co-trimoxazole", d1$AntibioticName)

d1 = d %>%
  mutate(
    drug_bug = combined
  )

d1$drug_bug = gsub("Co trimoxazole","Co-trimoxazole", d1$drug_bug)
d1$drug_bug = gsub("Methicillin resistance","Methicillin-resistance", d1$drug_bug)
d1$drug_bug = gsub("Third generation cephalosporins","Third-generation cephalosporins", d1$drug_bug)

a = adataAC %>% select(AntibioticName, Antibiotic, Pathogen, PathogenName) %>%
  distinct()

d2 = left_join(d1,a)

d2$AntibioticName = gsub("Co trimoxazole","Co-trimoxazole", d2$AntibioticName)
d2$AntibioticName = gsub("Methicillin resistance","Methicillin-resistance", d2$AntibioticName)
d2$AntibioticName = gsub("Third generation cephalosporins","Third-generation cephalosporins", d2$AntibioticName)

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
                              (Antibiotic == "COL") ~ 'Polymoxins',
                              (Antibiotic == "MNO" | Antibiotic == "TGC") ~ 'Tetracyclines',
                              (AntibioticName=="Third-generation cephalosporins"| AntibioticName == "Methicillin-resistance") ~ "SDG"
  ),
  Grouping = factor(Grouping, levels=c("Aminoglycocides", "Carbapenems", "3rd-gen. cephalosporins", "4th-gen. cephalosporins",
                                       "Fluoroquinolones", "Macrolides","Penicillins", "Polymoxins", 
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
unique(d3$AntibioticName[is.na(d3$Grouping)])

table(d3$AntibioticName,d3$Grouping)
table(d3$AntibioticName, useNA="always")
table(d3$AntibioticName, useNA="always")


region = cdata %>% 
  ungroup() %>% select(WHORegionCode, WHORegionName) %>%
  distinct()

d4 = d3 %>% 
  left_join(region)


d5 = d4 %>%
  select(WHORegionName, Iso3, Year, w_prev, w_prev_lower, w_prev_upper, amr_rate, drug_bug,Specimen,
         PathogenName,PathogenName2, AntibioticName, Grouping, drug_bug)


alpha = 0.5

# LOAD IN SHAPEFILE
#world_un <- st_read(paste0(dirDataNew,"/who_shapefile/GLOBAL_ADM0.shp"))
# world_un <- st_read(paste0(dirDataNew,"/MapShapeFiles/Detailed_Boundary_ADM0/GLOBAL_ADM0.shp"))
# head(world_un)
# 
# world_disp <- st_read(paste0(dirDataNew,"/MapShapeFiles/Detailed_Boundary_Disputed_Areas/Detailed_Boundary_Disputed_Areas.shp"))
# 
# all_columns <- union(colnames(world_un), colnames(world_disp))
# 
# # Add missing columns to each shapefile
# missing_cols_un <- setdiff(all_columns, colnames(world_un))
# for (col in missing_cols_un) {
#   world_un[[col]] <- NA
# }
# 
# missing_cols_disp <- setdiff(all_columns, colnames(world_disp))
# for (col in missing_cols_disp) {
#   world_disp[[col]] <- NA
# }
# 
# world_combined <- rbind(world_un, world_disp)

#combined_world <- rbind(world_un, world_disp)

# MAKE PLOTS - FOR LAYOUT COMBINED EXAMPLES
#--------------------------------------------------------------------------

fig4.2 = d5 %>% filter(drug_bug%in%c(ppbsi,SDG,pputi,ppgi,ppuro))

unique(fig4.2$drug_bug)

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
               amr_data = fig4.2, 
               year = 2022,
               palette = palette_map2,
               title = "No",
               estimated = "Yes",
               specimen = specimen, 
               pathogen_name = pathogen,
               antibiotic_name = antibiotic, 
               antibiotic_label = antibiotic, 
               na_color = "white")#,
               #show_fill=ifelse(pathogen=="Streptococcus pneumoniae", T,F))
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic_name,
                   na_color = "white")#,
                  # show_fill=ifelse(pathogen=="Escherichia coli", T,F))
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white")#,
                   #show_fill=ifelse(pathogen=="Klebsiella pneumoniae" & antibiotic=="Imipenem", T,F)
  uti_plot_list[[pp]] <- p
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white")#,
                  # show_fill=ifelse(pathogen=="Salmonella spp.", T,F))
  gi_plot_list[[pp]] <- p
}
combined_plot_gi = gi_plot_list[[1]]+gi_plot_list[[2]]+plot_layout(ncol = 2)  # Arrange in 2 columns

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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white")
  ng_plot_list[[pp]] <- p
}
combined_plot_gon = ng_plot_list[[1]]+plot_layout(ncol = 1) 

# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.2a_CTA_bsi_prioritypathogen.png"), 
#        plot = combined_plot_bsi, 
#        width = 20, height = 30)
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.2b_CTA_sdg_prioritypathogen.png"), 
#        plot = combined_plot_sdg, 
#        width = 20, height = 15)
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.2c_CTA_uti_prioritypathogen.png"), 
#        plot = combined_plot_uti, 
#        width = 20, height = 30)
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.2d_CTA_stool_prioritypathogen.png"), 
#        plot = combined_plot_gi, 
#        width = 20, height = 30)
# 
# ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.2d_CTA_gon_prioritypathogen.png"), 
#        plot = combined_plot_gon, 
#        width = 10, height = 30)

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

write.csv(fig4.4, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/CTAprevalence_prioritypathogen2022.csv"))

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

unique(fig4.2$drug_bug)

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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=F)#,
  #show_fill=ifelse(pathogen=="Streptococcus pneumoniae", T,F))
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic_name,
                   na_color = "white",
                   subtitle=F)#,
  # show_fill=ifelse(pathogen=="Escherichia coli", T,F))
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=F)#,
  #show_fill=ifelse(pathogen=="Klebsiella pneumoniae" & antibiotic=="Imipenem", T,F)
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=F)#,
  # show_fill=ifelse(pathogen=="Salmonella spp.", T,F))
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
                   amr_data = fig4.2, 
                   year = 2022,
                   palette = palette_map2,
                   title = "No",
                   estimated = "Yes",
                   specimen = specimen, 
                   pathogen_name = pathogen,
                   antibiotic_name = antibiotic, 
                   antibiotic_label = antibiotic, 
                   na_color = "white",
                   subtitle=F)
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
  device = svg, width = 11, height = 7, dpi = 300
)
}

# UTI
for (plot_name in names(uti_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.6_CTA_", plot_name, ".svg"),
    plot = uti_plot_list_i[[plot_name]],
    device = svg, width = 11, height = 7, dpi = 300
  )
}

# STOOL
for (plot_name in names(gi_plot_list_i)) {
  ggsave(
    filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Figure_4.7_CTA_", plot_name, ".svg"),
    plot = gi_plot_list_i[[plot_name]],
    device = svg, width = 11, height = 7, dpi = 300
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




#  4.3	Time series of resistance to selected antibiotics, 2017-2022
###################################################################

r6 = r5 %>% filter(drug_bug %in%c(ppbsi,pputi,SDG, ppgi, ppuro))
r6$Region2 = ifelse(r6$Region=="Global", "Global", "Region")
#r7 = r6 %>% filter(PathogenName =="Klebsiella pneumoniae", AntibioticName=="Imipenem")

p = ggplot(r6 %>% filter(Region!="Global"), aes(x = Year, y = median*100, group=Region, colour=Region, fill=Region)) +
  geom_line(size=0.8, alpha=0.2) +
  #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2,linetype=1) +  # Add fill aesthetic for ribbon
  #geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +  # Bar plot
  #geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, position = position_dodge(width = 0.9), size = 0.8, alpha = 0.8) +  # Error bars
  geom_errorbar(aes(ymin = Q2.5*100, ymax = Q97.5*100), 
                width = 0.2, 
                position = position_dodge(width = 0.5),  # Align error bars with points
                size = 1, alpha = 0.8) +  # Error bars
  geom_point(size = 2, position = position_dodge(width = 0.5))+
  facet_wrap(~drug_bug, ncol=3, scales="fixed")+
  scale_colour_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
  scale_fill_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +  
  labs(
    x="",
    y=""
  )+
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "white",
      colour = "white"),
    axis.text.x = element_text(size = 16),
    axis.text.y = ggtext::element_markdown(size = 18),
    title = element_text(size = 20),
    strip.text = element_text(size = 18)  # Change the facet title size
  )
p

p1 = ggplot(r6 %>% filter(Region=="Global"), aes(x = Year, y = median*100, colour=Region, fill=Region)) +
  geom_line(size=1) +
  #geom_point()+
  geom_ribbon(aes(ymin = Q2.5*100, ymax = Q97.5*100), alpha = 0.2,linetype=1) +  # Add fill aesthetic for ribbon
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100)) +  
  #geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +  # Bar plot
  #geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, position = position_dodge(width = 0.9), size = 0.8, alpha = 0.8) +  # Error bars
  #geom_errorbar(aes(ymin = Q2.5*100, ymax = Q97.5*100, width=0))+
  #geom_point(size=2)+
  facet_wrap(~drug_bug, ncol=3, scales="fixed")+
  scale_colour_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
  scale_fill_manual(values = facet_colors3a) +  # Manually set fill colors if necessary
  theme_minimal() +
  labs(
    x="",
    y=""
  )+
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "white",
      colour = "white"),
    axis.text.x = element_text(size = 16),
    axis.text.y = ggtext::element_markdown(size = 18),
    title = element_text(size = 20),
    strip.text = element_text(size = 18)  # Change the facet title size
  )
p1

combined_plot = p1 + p + plot_layout(ncol = 2)

ggsave(filename = paste0(dirOutput, "/Report/Chapter4_prevalence/Figure_4.3_REGION_trendsNopredict.png"), 
       plot = combined_plot, 
       width = 30, height = 30)



# 4.2.2 Pathogen distributions
###################################################################

d = adataAC


d1 = d %>% filter(Specimen == "BLOOD" & combined%in%dbdata$combined, TotalPathogenIsolates>0, InReport=="Yes",
                  InterpretableAST>10) %>%
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

# REGIONAL DISTRIBUTIONS IN PATHOGENS
#--------------------------------------------------------------------

# PathogenIsolateswithAST = Within each Iso3, Year, Specimen type, 
# DemographicsOrigin type and pathogen type, corresponds to the total number of 
# isolates of this pathogen type with interpretable AST results for at least one 
# antibiotic. Within the specified grouping, corresponds to the maximum count of "InterpretableAST"

# COUNTRY TOTALS
d2 = d1 %>%
  ungroup() %>% filter(AntibioticName != "Third-generation cephalosporins" & Year!=2016) %>%
  dplyr::select(Region, Iso3, Year, Specimen, PathogenName, TotalPathogenIsolates) %>%
  group_by(Region, Iso3, Year, PathogenName) %>%
  summarise_all(max) %>%
  group_by(Iso3) %>%
  filter(
    2022 %in% Year,  # Ensure the country reported in 2022
    n_distinct(Year) >= 3
  ) %>%
  as.data.frame()

# REGIONAL TOTALS
d3 = d2 %>%
  ungroup() %>%
  group_by(Region, Year, PathogenName) %>%
  summarise(TotalPathogenIsolates = sum(TotalPathogenIsolates))

t1 = d2 %>%
  group_by(Region, Year) %>%
  summarise(TotalSpecimen = sum(TotalPathogenIsolates))

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
  summarise(TotalPathogenIsolates = sum(TotalPathogenIsolates))

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
    mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
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
    mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
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
    mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(PathogenName2 = fct_reorder(PathogenName2, last_year_value, .desc = TRUE))
  
  # Create the plot for the current region
  ggplot(region_data, aes(x = Year, fill = PathogenName2, y = Prop * 100)) +
    geom_alluvium(aes(alluvium = PathogenName2), alpha = 1) +
    scale_fill_manual(name = " ", values = value_colorsP) +
    labs(
      x = "",
      y = if(region == "South-East Asia Region") 'Percentage of BCI (%)' else NULL,
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



# p1 <- ggplot(d9, aes(x = Year, fill = PathogenName2, y = TotalPathogenIsolates)) +
#   geom_alluvium(aes(alluvium = PathogenName2), alpha = 1) +
#   facet_wrap(.~Region,ncol=1, scales="free_y")+
#   scale_fill_manual(name = " ", values = colors.db)+
#   labs(
#     x = "",
#     y = 'Number of BCI isolates',
#     title = "Reported BSI isolates"
#   ) +
#   scale_x_continuous(breaks = c(2018, 2020, 2022)) +
#   scale_y_continuous(labels = label_number(big.mark = " "))+
#   theme_minimal() +
#   theme(
#     legend.text = element_text(face = "italic",size=20),
#     plot.background = element_rect(
#       fill = "white",
#       colour = "white"),
#     title = element_text(size = 34),
#     axis.text.x = element_text(size = 28),
#     axis.text.y = ggtext::element_markdown(size = 28),
#     strip.text.y.left = element_text(size = 30, angle = 0, vjust = 1.01, hjust = 1),  
#     strip.text = element_text(size = 30),   
#     strip.text.y = element_blank(),   
#     strip.placement = "outside",            
#     strip.background = element_blank(),      
#     #panel.grid.major.x = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "bottom"
#   )+
#   # geom_text(
#   #   aes(x = Year, y = PathogenIsolateswithAST+10000, label = NumCountriesReporting),
#   #   vjust = -0.5, hjust = 0.5, size = 5
#   # ) +
#   guides(fill = guide_legend(nrow = 6)) 
# p1
# 
# p1b  <- ggplot(d7%>%filter(Year!=2016), aes(x = Year, fill = PathogenName2, y = Prop*100)) +
#   geom_alluvium(aes(alluvium = PathogenName2), alpha = 1) +
#   facet_wrap(.~ Region, ncol=1)+
#   scale_fill_manual(name = " ", values = colors.db)+
#   labs(
#     x = "",
#     y = 'Percentage (%)',
#     title = "Relative frequency of pathogens \nin BSI isolates"
#   ) +
#   scale_x_continuous(breaks = c(2018, 2020, 2022)) +
#   theme_minimal() +
#   theme(
#     legend.text = element_text(face = "italic",size=20),
#     plot.background = element_rect(
#       fill = "white",
#       colour = "white"),
#     title = element_text(size = 34),
#     axis.text.x = element_text(size = 28),
#     axis.text.y = ggtext::element_markdown(size = 28),
#     strip.text.y.left = element_text(size = 30, angle = 0, vjust = 1.01, hjust = 1),  
#     strip.text = element_text(size = 30),   
#     strip.text.y = element_blank(),   
#     strip.placement = "outside",            
#     strip.background = element_blank(),      
#     #panel.grid.major.x = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "none"
#   )#+
#   #guides(fill = guide_legend(nrow = 1)) 
# 
# p1b

#combined_plot = p1 + p1b + plot_layout(ncol=2)

# # FOR REPORT
# ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.8_pathogen_distr_bsi_TOTALPATHISOLATES.svg"), 
#        plot = combined_plot, 
#        device="svg",
#        width = 30, height = 12)
# 


# FOR DRUG BUG COMBINATIONS
#------------------------------------------------------------------------------------------

d = adataAC


d1 = d %>% filter(Specimen == "BLOOD" & combined%in%dbdata$combined, TotalPathogenIsolates>0, InReport=="Yes",
                  InterpretableAST>10) %>%
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

# COUNTRY TOTALS
r1 <- d1 %>%
  filter(!AntibioticName %in% c("Third-generation cephalosporins", "Minocycline") & Year!=2016) %>%
  ungroup() %>%
  dplyr::select(Region, Iso3, Year, Specimen, PathogenName, AntibioticName, Resistant, InterpretableAST, TotalPathogenIsolates) %>%
  group_by(Region, Iso3, Year, PathogenName, AntibioticName) %>%
  summarise_all(max) %>%
  ungroup() %>%
  group_by(Iso3) %>%
  filter(
    2022 %in% Year,  # Ensure the country reported in 2022
    n_distinct(Year) >= 3
  ) %>%
  as.data.frame()

# REGIONAL TOTALS
r2 = r1 %>%
  mutate(Freq = Resistant/InterpretableAST*TotalPathogenIsolates,
         PathogenName2 = factor(PathogenName, levels = c("Acinetobacter spp.","Escherichia coli","Klebsiella pneumoniae",
                                                         "Salmonella spp.","Staphylococcus aureus","Streptococcus pneumoniae"),
                                labels = c("Acinetobacter spp.", "E. coli", "K. pneumoniae",
                                           "Salmonella spp.", "S. aureus", "S. pneumoniae"))) %>%
  dplyr::select(Region, Iso3, Year, PathogenName, PathogenName2,AntibioticName, Resistant,InterpretableAST, TotalPathogenIsolates, Freq)

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
            .groups = "drop") 

# s1 = r1 %>%
#   group_by(Region, Year, PathogenName) %>%
#   summarise(TotalPathogenIsolates = sum(unique(TotalPathogenIsolates))
#   )


#r4 = left_join(r3,s1, by = c("Region", "Year", "PathogenName"))

l1 = r3 %>%
  group_by(Region, Year) %>%
  summarise(
    TOTALTotalPathogenIsolates = sum(Freq, na.rm=T))

r4 = left_join(r3,l1) 
r5 = r4 %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq),
         Prop = Freq/TOTALTotalPathogenIsolates
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
            .groups = "drop") 


lg1 = rg1 %>%
  group_by(Year) %>%
  summarise(
    TOTALTotalPathogenIsolates = sum(Freq, na.rm=T))

rg2 = left_join(rg1,lg1) 

rg3 = rg2 %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq),
         Prop = Freq/TOTALTotalPathogenIsolates
  )

rg3$Region = "Global"

rg3 = rg3 %>% dplyr::select(Region, Year, PathogenName, AntibioticName, Freq, Prop, TOTALTotalPathogenIsolates)

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

r7_complete <- r7 %>% filter(Year%in%c("2022")) %>%
  group_by(Region) %>%
  arrange(desc(Freq)) %>%
  slice(1:10) %>%
  mutate(
    id = paste0(drug_bug, "-", Region)
  )

r7_top = r7%>%filter(id%in%r7_complete$id)

table(r7_top$Year) # 2016 not complete for all regions/DB leave out, then for 2018 one missing, this is E.coli levofloxacin for the americas. Add with 0
unique(r7_top$drug_bug)
# afro2017abactmino = r7_top %>% filter(Region == "African Region", Year==2019,PathogenName=="Acinetobacter spp.", AntibioticName=="Minocycline") %>%
#   mutate(Year = 2018,
#          Freq = 0,
#          Prop = 0)

 amro2017kpncef = r7_top %>% filter(Region == "Region of the Americas", Year==2019,PathogenName=="Klebsiella pneumoniae", AntibioticName=="Cefotaxime") %>%
   mutate(Year = 2018,
          Freq = 0,
         Prop = 0)

r8_top = rbind(r7_top, amro2017kpncef)
unique(r8_top$drug_bug)
r8_top = r8_top %>% mutate(
  drug_bug2 = factor(drug_bug, levels=c("Acinetobacter spp.-Gentamicin",
                                        "Acinetobacter spp.-Imipenem",
                                        "Acinetobacter spp.-Meropenem",
                                        "Acinetobacter spp.-Amikacin",
                                         "E. coli-Co-trimoxazole",
                                         "E. coli-Ampicillin", 
                                         "E. coli-Cefepime",
                                         "E. coli-Ceftazidime",
                                         "E. coli-Cefotaxime",
                                         "E. coli-Ceftriaxone",
                                         "E. coli-Levofloxacin", 
                                         "E. coli-Ciprofloxacin",
                                         "K. pneumoniae-Co-trimoxazole",
                                         "K. pneumoniae-Cefepime",
                                         "K. pneumoniae-Ceftazidime",
                                         "K. pneumoniae-Ceftriaxone",
                                         "K. pneumoniae-Cefotaxime", 
                                         "K. pneumoniae-Levofloxacin",
                                         "K. pneumoniae-Ciprofloxacin",
                                         "S. aureus-Methicillin-resistance")
    
  ))


#  colors for 3 Ecoli
blues <- brewer.pal(6, "Blues")
greens <- brewer.pal(4,"Greens")
#  colors for Acinetobacter 
yellows <- c("gold")
orange <- c("orange")
#  colors for KPN
reds <- brewer.pal(6, "Reds")
#  colors for MRSA and strep
greys <- c("grey20","grey50")   
# combine all colors
#colors.db <- c(reds[1], blues[1], reds[2], blues[2], blues[3], reds[3], yellows,orange, greys)

colors.dbr = c(greens[1],greens[2],greens[3],greens[4], blues[1],blues[2],blues[3],blues[4],blues[5],reds[2],reds[3],reds[4],reds[5],reds[6],"gold" )
colors.dbr %>% scales::show_col(cex_label = .7) 


value_colors = c("Acinetobacter spp.-Gentamicin" = greens[1],
                 "Acinetobacter spp.-Imipenem" = greens[2],
                 "Acinetobacter spp.-Meropenem" = greens[3],
                 "Acinetobacter spp.-Amikacin" = greens[4],
                 "E. coli-Co-trimoxazole" = reds[1],
                 "E. coli-Ampicillin" = reds[2], 
                 "E. coli-Cefepime" = reds[3],
                 "E. coli-Ceftazidime" = "orangered",
                 "E. coli-Cefotaxime" = reds[4],
                 "E. coli-Ceftriaxone" = reds[5],
                 "E. coli-Levofloxacin" = reds[6], 
                 "E. coli-Ciprofloxacin"="orangered4",
                 "K. pneumoniae-Co-trimoxazole" = blues[1],
                 "K. pneumoniae-Cefepime" = blues[2],
                 "K. pneumoniae-Ceftazidime"=blues[3],
                 "K. pneumoniae-Ceftriaxone" = blues[4],
                 "K. pneumoniae-Cefotaxime"=blues[5], 
                 "K. pneumoniae-Levofloxacin" = blues[6],
                 "K. pneumoniae-Ciprofloxacin" = greys[2],
                 "S. aureus-Methicillin-resistance"="gold")


# Plot 4.8c - Top 5
# p = ggplot(r8_top%>%filter(Year!=2016, Region!="Global"), aes(x = Year, fill = drug_bug2, y = Freq)) +
#   geom_alluvium(aes(alluvium = drug_bug2), alpha = 1) +
#   facet_wrap(.~ Region, ncol=1)+
#   scale_fill_manual(name = " ", values = value_colors)+
#   labs(
#     x = "",
#     y = 'Number of BCI',
#     title = ""
#   ) +
#   scale_x_continuous(breaks = c(2018, 2020, 2022)) +
#   theme_minimal() +
#   scale_y_continuous(labels = label_number(big.mark = " "),
#                      breaks = c(25000, 50000,100000,150000,200000,250000))+
#   theme(
#     legend.text = element_text(face = "italic",size=20),
#     plot.background = element_rect(
#       fill = "white",
#       colour = "white"),
#     title = element_text(size = 20),
#     axis.text.x = element_text(size = 16),
#     axis.text.y = ggtext::element_markdown(size = 18),
#     strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),  
#     strip.text = element_text(size = 20),   
#     strip.text.y = element_blank(),   
#     strip.placement = "outside",            
#     strip.background = element_blank(),      
#     #panel.grid.major.x = element_blank(),
#     panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
#     panel.grid.minor = element_line(linetype = "dotted"),
#     legend.position = "bottom"
#   )+
#   guides(fill = guide_legend(nrow = 3)) 
# p


regions <- unique(r8_top$Region)

# Create ordered dataset for each region with largest frequency on top
ordered_datasets <- map(regions, function(region) {
  r8_top %>%
    filter(Region == region) %>%
    group_by(drug_bug2) %>%
    mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(drug_bug2 = fct_reorder(drug_bug2, last_year_value, .desc = TRUE))  # Order by largest to smallest
})

# r8_top = r8_top %>%
#   mutate(Prop2 = ifelse(Prop==0, 0.0001, Prop))


# Combine datasets into one for plotting
r8_top_ordered <- bind_rows(ordered_datasets)

regions = c("Global", "African Region", "Region of the Americas","South-East Asia Region",       
            "European Region", "Eastern Mediterranean Region","Western Pacific Region")

# Generate a list of ggplot objects for each region
plots3 <- map(regions, function(region) {
  # Filter and reorder `drug_bug2` within the specific region
  region_data <- r8_top_ordered %>%
    filter(Region == region) %>%
    group_by(drug_bug2) %>%
    mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
    fill(last_year_value, .direction = "updown") %>%
    ungroup() %>%
    mutate(drug_bug2 = fct_reorder(drug_bug2, last_year_value, .desc = TRUE))
  
  # Create the plot for the current region
  ggplot(region_data, aes(x = Year, fill = drug_bug2, y = Prop * 100)) +
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


# FOR REPORT
ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8a_pathogen_distr_bsi.svg"), 
       plot = combined_plot1, 
       device="svg",
       width = 10, height = 44)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8b_pathogen_distr_bsi.svg"), 
       plot = combined_plot2, 
       device="svg",
       width = 10, height = 44)

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8c_pathogen_distr_bsi.svg"), 
       plot = combined_plot3, 
       device="svg",
       width = 10, height = 44)

# Plot for legend with all combinations
p3 = ggplot(r8_top, aes(x = Year, fill = drug_bug2, y = Prop * 100)) +
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
  guides(fill = guide_legend(nrow = 10))
p3

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Final/Fig_4.8c_pathogen_distr_bsi_FORLEGEND.svg"), 
       plot = p3, 
       device="svg",
       width = 35, height = 10)

write.csv(r8_top, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8c_Table_Pathogen_resistance.csv"))
write.csv(d7, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8ab_Table_Pathogen_distr.csv"))


# PER YEAR TOP 10
f1 <- r7 %>%
  group_by(Region, Year) %>%
  arrange(desc(Freq)) %>%
  slice(1:10) %>%
  mutate(
    id = paste0(drug_bug, "-", Region, "-", "Year")
  )

p4 = ggplot(f1 %>% filter(Year != 2016), aes(x = Year, fill = drug_bug,color="black", y = Prop * 100)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ Region, ncol = 7) +
  #scale_fill_manual(name = " ", values = colors.db) +
  labs(
    x = "",
    y = "Percentage of BCI (%)",
    title = "Most frequent resistance-pathogen combinations across \nall pathogens identifed in BSI isolates"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic", size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    title = element_text(size = 34),
    axis.text.x = element_text(size = 28),
    axis.text.y = ggtext::element_markdown(size = 28),
    strip.text.y.left = element_text(size = 30, angle = 0, vjust = 1.01, hjust = 1),
    strip.text = element_text(size = 30),
    strip.text.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom"
  )
p4

ggsave(filename = paste0(dirOutputReport, "/Chapter 4/Ch4 Figures/Fig_4.8c_pathogen_distr_bsi_ISOLATES_RESISTANT_ALLYEARS_barplot.svg"), 
       plot = p4, 
       device="svg",
       width = 32, height = 10)

write.csv(f1, file = paste0(dirOutputReport, "/Chapter 4/Ch4 summary stats/Fig_4.8ab_Table_Pathogen_distr_overtime_3years_ALLYEARS_FORCONTEXT.csv"))


# ONE REGION ALL PATHOGENS

region_data <- r7 %>%
  filter(Region == "European Region") #%>%
  # group_by(drug_bug) %>%
  # mutate(last_year_value = ifelse(Year == 2022, Prop, NA)) %>%
  # fill(last_year_value, .direction = "updown") %>%
  # ungroup() %>%
  # mutate(drug_bug = fct_reorder(drug_bug, last_year_value, .desc = TRUE))

# Create the plot for the current region

ggplot(region_data, aes(x = Year, fill = drug_bug, y = Prop * 100)) +
  geom_alluvium(aes(alluvium = drug_bug), alpha = 1) +
  #scale_fill_manual(name = " ", values = value_colors) +
  labs(
    x = "",
    y = 'Percentage (%)',
    title = "Most frequent resistance-pathogen \ncombinations across all pathogens \nidentifed in BSI isolates",
    subtitle = unique(region_data$Region)
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
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 10))





# ECCMID
#----------------------------------------------------------------------------

# TRENDS FOR ECCMID
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
