#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, RColorBrewer,
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, purrr, brms, cowplot, ggrepel, grid)

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

# EAR data
edata = read_excel(paste0(dirDataNew, "/EAR_events_2019_2024_EV.xlsx"))

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


# CHAPTER 5: EAR DATA
###################################################################################

world_un <- st_read(paste0(dirDataNew,"/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp"))
head(world_un)

world_data <- world_un %>%
  rename(Iso3 = ISO_A3) %>%
  st_make_valid()

WHO_region_mapping <- idata_country %>% 
  select(c(Iso3, WHORegionCode)) %>%
  rename(WHORegion = WHORegionCode)


world_data <- world_data %>%
  left_join(WHO_region_mapping, by="Iso3") %>%
  filter(!is.na(WHORegion))

# Dissolve (aggregate) the geometry by WHORegion to ensure entire regions are colored
who_region_geometry <- world_data %>%
  group_by(WHORegion) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Summarize the data by WHO Region
eregion_summary <- edata %>%
  group_by(WHORegion, Year) %>%
  summarise(events = n(),
            pathogen_resistance = paste(unique(paste(PathogenName, Resistance, sep = ": ")), collapse = "\n"),
            .groups = 'drop')

# Merge with summary data to maintain information for labels and dots
eregion_summary <- left_join(eregion_summary, who_region_geometry, by = "WHORegion")


#Define vertical and horizontal offsets for each WHO region
region_offsets <- data.frame(
  WHORegion = c("AMR", "AFR", "EMR", "WPR", "SEA", "EUR"),
  x_offset = c(-1.5, -1.5, 1.5, 1.5, -1.5, 1.5),
  y_offset = c(-1.5, -1.5, 1.5, 0.5, -0.5, 1.5)
)

# Add offsets to the summary data
eregion_summary <- eregion_summary %>%
  left_join(region_offsets, by = "WHORegion") %>%
  mutate(
    centroid_coords = st_coordinates(st_centroid(geometry)),
    label_x = centroid_coords[,1] + x_offset,
    label_y = centroid_coords[,2] + y_offset
  )

# Summarize the data by WHO Region
eregion_eartype <- edata %>%
  group_by(WHORegion, Year, `EAR Type`) %>%
  summarise(events_type = n(),
            .groups = 'drop')



# Create the map plot
map_plot <- ggplot(data = world_data) +
  geom_sf(aes(fill = WHORegion), color = "white") +  # Color the entire WHO Region
  scale_fill_manual(values = c(palette5, "grey"), name = "WHO Region") +
  
  # Add labels with specific alignment and no overlap
  geom_label_repel(data = eregion_summary, aes(x = label_x, 
                                               y = label_y, 
                                               label = paste0(WHORegion, "\nYear: ", Year, "\nEvents: ", events, "\n", pathogen_resistance),
                                               fill = WHORegion),
                   size = 3, box.padding = 0.5, point.padding = 0.5, 
                   segment.color = 'grey50', segment.size = 0.5, 
                   nudge_x = 7, nudge_y = 5, max.overlaps = Inf) +
  
  # Plot jittered dots for each year at the centroid
  geom_point(data = eregion_summary, aes(geometry = st_centroid(geometry), size = events),
             stat = "sf_coordinates", position = position_jitter(width = 5, height = 5)) +
  
  labs(title = "AMR Reports by WHO Region",
       fill = "WHO Region",
       size = "Number of Events",
       color = "WHO Region") +
  
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank())

# Create the stacked bar plot
bar_plot <- ggplot(data = eregion_summary, aes(x = factor(Year), y = events, fill = WHORegion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(palette5, "grey"), name = "WHO Region") +
  labs(x = "Year", y = "Number of Events\nby WHO region", fill = "WHO Region") +
  theme_minimal() +
  #guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()  # Remove minor grid lines
  )

# Convert the bar plot to a grob (graphical object)
bar_grob <- ggplotGrob(bar_plot)

# Define a new color palette for the EAR event types
new_palette <- brewer.pal(7, "Set3")

# Create the bar plot using the new color palette
bar_type_plot <- ggplot(data = eregion_eartype, aes(x = factor(Year), y = events_type, fill = `EAR Type`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = new_palette, name = "EAR type") +  # Use the new palette
  labs(x = "Year", y = "Number of Events\nby type", fill = "WHO Region") +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()  # Remove minor grid lines
  )

# Convert the bar plot to a grob (graphical object)
bar_type_grob <- ggplotGrob(bar_type_plot)

final_plot <- map_plot +
  annotation_custom(bar_grob, xmin = -200, xmax = -70, ymin = -110, ymax = -30) +  # Adjust for lower left corner
  # Add the second bar plot (bar_type_grob) as an inset in the lower right corner
  annotation_custom(bar_type_grob, xmin = 70, xmax = 200, ymin = -110, ymax = -30)  # Adjust for lower right corner

# Display the final plot
print(final_plot)


pdf(paste0(dirOutput, "/Descriptive/EAR_MAP.pdf"), width = 20, height = 33) # Open a PDF device for saving
# Arrange the plots in a grid with 4 plots per row
print(final_plot)
dev.off()

