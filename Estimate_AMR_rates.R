#################################################
# GLASS REPORT - ESTIMATE AMR RATES
#################################################

# Author: Esther van Kleef
# Date created: 5 July 2024
# Date latest update: 31 July 2024

rm(list=ls())
# Load R packages
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, Hmisc, stringr,lme4,reshape2, 
               table1, flextable, magrittr, officer, janitor, sf, gtsummary, leaflet,
               meta, metafor,PHEindicatormethods, RColorBrewer, wesanderson, ggforce)

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
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")           # Population data
pdataDM = read.csv(paste0(dirDataNew, "/EI_PopdtaDM_180724_EV.csv"), sep=",")       # Population data by age and sex

# Country data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")       # Country data

# Surveillance indicator data
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",") 

# HAQI data
haqidata = read.csv(paste0(dirDataNew, "/EI_HAQIdta_080724_EV.csv"), sep=",")

# AMR data
#adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_pop_180724_EV.csv"), sep=",")   # Country AMR data
adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_pop_ABX_adapted_180724_EV.csv"), sep=",")   # Country AMR data
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_180724_EV.csv"), sep=",")   # Country AMR data
adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_pop_ABX_adapted_180724_EV.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read_excel(paste0(dirDataNew, "/updated_summary_dbc_longformat.xlsx"), sheet=1)

# rrates 2021
rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
  Q1 = as.numeric(Q1),
  Q3 = as.numeric(Q3),
  median = as.numeric(median)
)

# Drug bug combinations to include in report
combinations2022 = dbdata %>% filter(Period %in% c("2016-","2016-2022")) %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

###################################################################
# PREAMBLE
###################################################################

# Take out all the lines which specify the origin of the sample (as we don't have denominator population data for those)
# Add InReport to specify if drug-bug combination should be reported on
adataAS = adataNT %>% filter(!is.na(TotalPopulation)) %>%
  mutate(
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No"),
    amr_rate = Resistant/InterpretableAST, 
    # The above rates are based on very small numbers
    # Would be better to calculate prevalence by age globally
    BCI_1000000pop = InterpretableAST/TotalPopulation*1000000,
  )

# For now, calculate standardised rates with incidence rates, still direct method, but
# Assuming the same globally (or if anything, regionally), as we are dealing for many countries with low numbers
rrateAS_region = adataAS %>% group_by(WHORegionCode, Year,AgeCat10,Sex, DemographicsOrigin, PathogenName, AntibioticName, Specimen, 
                                      InReport) %>%
  summarise(
    s_interpretableAST = sum(InterpretableAST),  # number of samples
    s_resistant = sum(Resistant),  # count of resistant samples
    s_amr_rate = s_resistant/s_interpretableAST,  # proportion resistant
    m_amr_rate = median(Resistant)/median(InterpretableAST)
  ) %>%
  ungroup()

rrateAS_global = adataAS %>% group_by(Year, DemographicsOrigin, PathogenName, AntibioticName, Specimen, InReport) %>%
  summarise(
    s_interpretableAST = sum(InterpretableAST),  # number of samples
    s_resistant = sum(Resistant),  # count of resistant samples
    s_amr_rate = s_resistant/s_interpretableAST,  # proportion resistant
    m_amr_rate = median(Resistant)/median(InterpretableAST)
  ) %>%
  ungroup()

# Get testing and AMR rates
rrates <- adataAC %>% filter(Year == 2022) %>%
  group_by(Iso3, Specimen, PathogenName, AntibioticName, InReport) %>%
  reframe(amr_rate = Resistant/InterpretableAST,
          BCI_1000000pop = InterpretableAST/TotalPopulation*1000000)

rrates = left_join(rrates, haqidata)


###################################################################
## DESCRIPTIVES
##################################################################


palette <- wes_palette("Darjeeling1", n = 5)
palette2 <- wes_palette("BottleRocket2", n = 1)
palette3 <- wes_palette("GrandBudapest1", n = 2)[2]
palette4 <- wes_palette("BottleRocket2", n = 2)[2]

palette5 = c(palette3, palette[2],palette2,palette[5], palette[4],palette4)

# Number of isolates with AST By region
####################################################################

p1 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Escherichia coli", 
                               specimen = "BLOOD", in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p1

p1.2 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Escherichia coli", 
                                 specimen = "URINE", in_report = "Yes", xlim_max = 2000, 
                                 ncol_facet = 4, palette = palette5)
p1.2

p2 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Klebsiella pneumoniae", 
                               specimen = "BLOOD",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p2

p2.1 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Klebsiella pneumoniae", 
                               specimen = "URINE",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p2.1

p3 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Salmonella spp.", 
                               specimen = "BLOOD",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p3

p3.1 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Salmonella spp.", 
                               specimen = "STOOL",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p3.1

p4 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Acinetobacter spp.", 
                               specimen = "BLOOD",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 4, palette = palette5)
p4

p5 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Shigella spp.", 
                               specimen = "STOOL",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 3, palette = palette5)
p5

p6 = plot_isolates_db_asRegion(data = rrateAS_region, year = 2022, pathogen = "Neisseria gonorrhoeae", 
                               specimen = "UROGENITAL",in_report = "Yes", xlim_max = 1000, 
                               ncol_facet = 3, palette = palette5)
p6

pdf(paste0(dirOutput, "/Descriptive/AST_region_agesex.pdf"), width = 14, height = 100) # Open a PDF device for saving
# Arrange the plots in a grid with 4 plots per row
multiplot(p1,p2,p1.2,p2.1,p3,p3.1,p4,p5,p6, cols=1)
dev.off()

# So for which <100 isolates of drug-bug AMR by age and sex (to calculate regional AMR specific AMR? 
# E coli BLOOD 
# ---------------------------------------
# Ampicilin (ALL)
# Co-trimoxazole (ALL)
# Doripenem (Almost ALL)
# Levofloxacin (AFRO, SEARO)
# Colistin (ALL)

# K. pneumoniae blood
# ---------------------------------------
# Co-trimoxazole (EURO)
# Doripenem (ALL)
# Levofloxacin (AFRO)

# E coli URINE 
# ---------------------------------------
# Ampicillin (ALL)
# Doripenem (ALMOST ALL)
# Colistin (AFRO, EURO)
# Levofloxacin (AFRO)

# K. pneumoniae URINE
# --------------------------------------
# Colistin (AFRO, WPR, EUR)
# Levofloxacin (AFRO only)

# Salmonella spp BLOOD and STOOL
# --------------------------------------
# (Almost) All drug-bug combinations

# Acinetobacter spp BLOOD
# --------------------------------------

# Shigella spp STOOL
# --------------------------------------
# (Almost) All drug-bug combinations

# Neisseria gonorrhoea UROGENITAL
# --------------------------------------
# (Almost) All drug-bug combinations


# NUMBER OF ISOLATES WITH AST GLOBAL
#################################################################

p1 <- plot_isolates_db_as(data = rrateAS_global, year = 2022, specimen = "BLOOD", in_report = "Yes",
                       exclude_antibiotic = c("Doripenem"), ncol_facet = 3)
p1

p2 <- plot_isolates_db_as(data = rrateAS_global, year = 2022, specimen = "URINE", in_report = "Yes",
                       exclude_antibiotic = c("Doripenem"), ncol_facet = 3)
p2

p3 <- plot_isolates_db_as(data = rrateAS_global, year = 2022, specimen = c("UROGENITAL", "STOOL"), in_report = "Yes",
                       exclude_antibiotic = c("Doripenem"), ncol_facet = 3
)
p3

pdf(paste0(dirOutput, "/Descriptive/AST_global_agesex.pdf"), width = 14, height = 20) # Open a PDF device for saving
# Arrange the plots in a grid with 4 plots per row
multiplot(p1,p2,p3, cols=1)
dev.off()

# So for which <100 isolates of drug-bug AMR by age and sex globally 
# BLOOD 
# ---------------------------------------
# Doripenem (Acinetobacter, E. coli, K pneumoniae)
# Levofloxacin (Salmonella spp.)

# URINE 
# ---------------------------------------
# Doripenem (E. coli)

# UROGENITAL / STOOL 
# ---------------------------------------
# Levofloxacin (Salmonella, Shigella)
# Azithromycin (Shigella spp)


# PLOT AMR rates by rage and sex - region
#################################################################################

# Define colors for each WHORegionCode
facet_colors <- c(
  "AFR" = palette5[1],
  "AMR" = palette5[2],
  "EMR" = palette5[3],
  "EUR" = palette5[4],
  "SEA" = palette5[5],
  "WPR" = palette5[6]
)

# p1 = ggplot(rrateAS_region %>% filter(Year==2022, PathogenName=="Escherichia coli", Specimen=="BLOOD", InReport=="Yes", Sex=="Female",
#                                       !AntibioticName %in% c( "Doripenem", "Ampicillin")), 
#             aes(y=m_amr_rate, x=AgeCat10, group=WHORegionCode, col=WHORegionCode)) +
#   #geom_vline(xintercept = "M_0<04", linetype=1, col="grey", size=1) + 
#   geom_point(size=2) +
#   #geom_line() + 
#   facet_wrap(.~ AntibioticName+WHORegionCode,ncol=6)+
#   theme_minimal() + 
#   #geom_vline(xintercept = 50, col="seagreen", linetype=2) + 
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_color_manual(values = palette5) +
#   labs(
#     title = "AMR prevalence - by region & Female age strata (E. coli - Blood)",
#     y = "AMR prevalence",
#     x = "Age group (10 year bands)")
# 
# p1
# 
# p2 = ggplot(rrateAS_region %>% filter(Year==2022, PathogenName=="Escherichia coli", Specimen=="BLOOD", InReport=="Yes", Sex=="Male",
#                                       !AntibioticName %in% c( "Doripenem", "Ampicillin")), 
#             aes(y=m_amr_rate, x=AgeCat10, group=WHORegionCode, col=WHORegionCode)) +
#   #geom_vline(xintercept = "M_0<04", linetype=1, col="grey", size=1) + 
#   geom_point(size=2) +
#   #geom_line() + 
#   facet_wrap(.~ AntibioticName+WHORegionCode,ncol=6)+
#   theme_minimal() + 
#   #geom_vline(xintercept = 50, col="seagreen", linetype=2) + 
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_color_manual(values = palette5) +
#   labs(
#     title = "AMR prevalence - by region & Male age strata (E. coli - Blood)",
#     y = "AMR prevalence",
#     x = "Age group (10 year bands)")
# 
# p2
# 
# # p1.2 = ggplot(adataAS %>% filter(Year==2022, PathogenName=="Escherichia coli", Specimen=="BLOOD", InReport=="Yes", Sex="Female",
# #                                  !AntibioticName %in% c( "Doripenem", "Ampicillin")), 
# #             aes(y=amr_rate, x=AgeCat10, fill=WHORegionCode)) +
# #   #geom_vline(xintercept = "M_0<04", linetype=1, col="grey", size=1) + 
# #   geom_boxplot(na.rm=T) +
# #   geom_jitter() + 
# #   facet_wrap(.~ AntibioticName+WHORegionCode,ncol=6, scales="free_y")+
# #   theme_minimal() + 
# #   #geom_vline(xintercept = 50, col="seagreen", linetype=2) + 
# #   theme(
# #     axis.text.x = element_text(angle = 90, hjust = 1)) +
# #   scale_fill_manual(values = palette5) +
# #   labs(
# #     title = "AMR prevalence - by region & Female age strata (E. coli - Blood)",
# #     y = "AMR prevalence",
# #     x = "Age group (10 year bands)")
# # 
# # p1.2

p1 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Escherichia coli", specimen = "BLOOD", 
  in_report = "Yes",exclude_antibiotics = c("Doripenem", "Ampicillin"), facet_colors = facet_colors,
  palette = palette5)
p1

p1.2 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Escherichia coli", specimen = "URINE", 
                           in_report = "Yes",exclude_antibiotics = c("Doripenem", "Ampicillin"), facet_colors = facet_colors,
                           palette = palette5)
p1.2

p2 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Klebsiella pneumoniae", specimen = "BLOOD", 
                           in_report = "Yes",exclude_antibiotics = NULL, facet_colors = facet_colors,
                           palette = palette5)
p2

p2.1 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Klebsiella pneumoniae", specimen = "URINE", 
                           in_report = "Yes",exclude_antibiotics = NULL, facet_colors = facet_colors,
                           palette = palette5)
p2.1

p3 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Salmonella spp.", specimen = "BLOOD", 
                             in_report = "Yes",exclude_antibiotics = NULL, facet_colors = facet_colors,
                             palette = palette5)
p3

p4 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Acinetobacter spp.", specimen = "BLOOD", 
                           in_report = "Yes",exclude_antibiotics = NULL, facet_colors = facet_colors,
                           palette = palette5)
p4

p5 <- plot_AMRdb_as_region(data = adataAS, year = 2022, pathogen = "Staphylococcus aureus", specimen = "BLOOD", 
                           in_report = "Yes",exclude_antibiotics = NULL, facet_colors = facet_colors,
                           palette = palette5)
p5


# pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_means_Female.pdf"), width = 14, height = 15) # Open a PDF device for saving
# multiplot(p1, cols=1)
# dev.off()
# 
# pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_means_Male.pdf"), width = 14, height = 30) # Open a PDF device for saving
# multiplot(p2, cols=1)
# dev.off()

# 
# pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_Female.pdf"), width = 14, height = 30) # Open a PDF device for saving
# # Arrange the plots in a grid with 4 plots per row
# multiplot(p1.2, cols=1)
# dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_e.coliBLOOD.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p1, cols=1)
dev.off()


pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_e.coliURINE.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p1.2, cols=1)
dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_kpnBLOOD.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p2, cols=1)
dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_kpnURINE.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p2.1, cols=1)
dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_salmBLOOD.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p3, cols=1)
dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_abacterBLOOD.pdf"), width = 20, height = 30) # Open a PDF device for saving
multiplot(p4, cols=1)
dev.off()

pdf(paste0(dirOutput, "/Descriptive/AMR_Regional_agesex_boxplot_s.aureusBLOOD.pdf"), width = 20, height = 10) # Open a PDF device for saving
multiplot(p5, cols=1)
dev.off()


# Interpretation of boxplot of country-level raw AMR rates 
# ---------------------------------------------------------------------
# Shows that for particularly EMRO and AFRO, there is lots of variation in the AMR rates by age and sex

# AST vs Isolates per 1000000 pop - RAW counts
####################################################################################

p1 = ggplot(rrates %>% filter(PathogenName=="Escherichia coli", InReport=="Yes", 
                              Specimen=="BLOOD"), aes(x=BCI_1000000pop, y=amr_rate,group=AntibioticName)) +
  geom_point(size=2, alpha=0.5) +  
  geom_vline(xintercept=70, linetype=2, col="red", linewidth=0.7) +
  geom_smooth(col="seagreen") +
  facet_wrap(.~ AntibioticName, ncol=4, scales=c("free")) +
  theme_minimal() + 
  labs(
    title = "BCI/100 000 vs AMR Rates (E. coli - Blood specimens)",
    x = "BCI/100 000 population",
    y = "Resistance %")
p1


p1.haqi= ggplot(rrates %>% filter(PathogenName=="Escherichia coli", InReport=="Yes", 
                                  Specimen=="BLOOD"), aes(x=val, y=amr_rate,group=AntibioticName)) +
  geom_point(size=2, alpha=0.5) +  
  geom_vline(xintercept=70, linetype=2, col="red", linewidth=0.7) +
  geom_smooth(col="orange") +
  facet_wrap(.~ AntibioticName, ncol=4, scales="free_y") +
  ylim(0,1) + 
  theme_minimal() +
  labs(
    title = "AMR Rates vs HAQI (E. coli - Blood specimen)",
    x = "HAQI",
    y = "Resistance %") 
p1.haqi

p1.haqi.bci= ggplot(rrates %>% filter(PathogenName=="Escherichia coli",  InReport=="Yes", 
                                      Specimen=="BLOOD"), aes(x=val, y=BCI_1000000pop,group=AntibioticName)) +
  geom_point(size=2, alpha=0.5) +  
  geom_vline(xintercept=70, linetype=2, col="red", linewidth=0.7) +
  geom_hline(yintercept=70, linetype=2, col="red", linewidth=0.7) +
  geom_smooth(col="blue") +
  facet_wrap(.~ AntibioticName, ncol=4, scales="free_y") +
  theme_minimal() +
  labs(
    title = "BCI Rates/100 000 vs HAQI (E. coli - Blood specimen)",
    y = "BCI/100 000",
    x = "HAQI") 
p1.haqi.bci


pdf(paste0(dirOutput, "/Descriptive/BCI_HQI_ecoli_AMRrates.pdf"), width = 10, height = 25) # Open a PDF device for saving
# Arrange the plots in a grid with 4 plots per row
multiplot(p1,p1.haqi.bci, p1.haqi,cols=1)
dev.off()


# GLASS AMR Country data
##############################################################

# Countries that reported
creport = unique(adataAC$Iso3[which(adataAC$SpecimenIsolateswithAST>1)])

# GLASS Surveillance sites
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

im_table1 # So 90 countries report to GLASS among those that have also filled out the implementation survey
table(idata_country$AMR_GLASS_AST,useNA="always") # 92 countries reported to GLASS at least one isolate with AST

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

im_table1_regionAST # So 90 countries report to GLASS among those that have also filled out the implementation survey

write.table(im_table1, paste0(dirOutput,"/Descriptive/Section3.2_Surveillance_indicators/im2024_ASTprovided.csv"), col.names = T, row.names=F, append= F, sep=';')
write.table(im_table1_regionGLASS, paste0(dirOutput,"/Descriptive/Section3.2_Surveillance_indicators/im2024_GLASSenrolled_byregion.csv"), col.names = T, row.names=F, append= F, sep=';')
write.table(im_table1_regionAST, paste0(dirOutput,"/Descriptive/Section3.2_Surveillance_indicators/im2024_ASTprovided_byregion.csv"), col.names = T, row.names=F, append= F, sep=';')

###############################################################
# ESTIMATES BASED ON Modelled resistance rates 
###############################################################

# First trial with just E. coli 
ecolidata = adataAS %>% filter(PathogenName == "Escherichia coli" & InReport=="Yes")
unique(ecolidata$combined) # Count as 27 so all should be there

# Create list of data.frames
data_sets = list()

for(i in 1:length(unique(ecolidata$combined))) {
  d = ecolidata # Change to data to use
  data_subset <- adataAC %>% 
    filter(Specimen == d$Specimen[i],
           PathogenName == d$PathogenName[i],
           AntibioticName == d$AntibioticName[i],
           Year == 2022) %>%
    group_by(Iso3) %>%
    reframe(AMRrate = Resistant/InterpretableAST,
            Specimen = d$Specimen[i],
            PathogenName = d$PathogenName[i],
            AntibioticName = d$AntibioticName[i],
            Resistant = Resistant,
            InterpretableAST = InterpretableAST,
            TotalPopulation = TotalPopulation,
            BCI_pop100000 = InterpretableAST/TotalPopulation*100000,
            Year = 2022)
  print(paste0("Num", i))
  #print(data_subset[1,])
  data_sets[[i]] = data_subset
  #print(c(i,combinations2022$Specimen[i], combinations2022$PathogenName[i],combinations2022$AntibioticName[i]))
  
  #print(head(data_subset))
  
}

# For URINE e. coli doripenem only 10 countries provided isolates
# We need to make a distinction between no resistance found vs not tested, can we?
# Yes, all interpretable ASTs should be ASTs including the 100% susceptible ones

# Fit the model to each dataset
##########################################################

# Define the model formula
formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 | Iso3)) #



# Fit the model using brms
fit <- brm(formula, 
           data = data_sets[[1]], 
           family = binomial(), 
           prior = priors, 
           chains = 6, 
           cores = 6, 
           iter = 2000)

# Then update this fit using new data, so model does not need to recompile for each drug bug
model_fits <- lapply(data_sets, function(x) update(fit, newdata = x,
                                                   chains=6, cores=6))

# Extract summaries from each model fit
model_summaries <- lapply(model_fits, function(x) get_fit_model(model_fit=x))

# Plot the posterior means of the intercepts
#################################################################

# Extract the summary of the first model for plotting
model_estimates = NULL

for(i in 1:length(data_sets)){
  d = ecolidata # Change to type of bug-drug combinations to look at
  model_summaries[[i]]$summary$Iso3 <- sub("r_Iso3\\[(.*),Intercept\\]", "\\1", model_summaries[[i]]$summary$Country)
  model_summaries[[i]]$summary$Specimen <- d$Specimen[i]
  model_summaries[[i]]$summary$PathogenName <- d$PathogenName[i]
  model_summaries[[i]]$summary$AntibioticName <- d$AntibioticName[i]
  
  model_summaries[[i]]$summary <- left_join(model_summaries[[i]]$summary,cdata %>% select(Iso3, WHORegionCode, WHORegionName))
  
  # Create overall and regional values
  total = model_summaries[[i]]$summary %>%
    summarise(
      med50 = quantile(med50, probs = 0.5),
      low25 = quantile(low25, probs = 0.25),
      high75 = quantile(high75, probs = 0.75),
      Specimen = unique(Specimen),
      PathogenName = unique(PathogenName),
      AntibioticName = unique(AntibioticName),
      CTA_total = length(unique(Iso3)),
      Total = "Yes"
    )
  #print(total)
  data_total = d %>% filter(combined == d$combined[i]) %>%
    summarise(
      rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
      rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
      rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
    )
  
  total = cbind(total,data_total)
  
  region = model_summaries[[i]]$summary %>% group_by(WHORegionCode) %>%
    summarise(
      med50 = quantile(med50, probs = 0.5),
      low25 = quantile(low25, probs = 0.5),
      high75 = quantile(high75, probs = 0.5),
      Specimen= unique(Specimen),
      PathogenName = unique(PathogenName),
      AntibioticName = unique(AntibioticName),
      CTA_total = length(unique(Iso3)),
      Total = "No"
    )
  
  data_region = d %>% filter(combined == d$combined[i]) %>% 
    group_by(WHORegionCode) %>%
    summarise(
      rawAMRmed50 = median(Resistant/InterpretableAST, na.rm = T),
      rawAMR25 = quantile(Resistant/InterpretableAST, prob=0.25,na.rm = T),
      rawAMR75 = quantile(Resistant/InterpretableAST, prob=0.75,na.rm = T)
    )
  region = left_join(region, data_region)
  total$WHORegionCode = "1.ALL"
  #total = total[,names(total.region)]
  total.region = rbind(total,region)
  model_summaries[[i]]$summary.overall = total.region
  model_estimates = rbind(model_estimates,total.region)
}

###############################################################
# ESTIMATES BASED ON inverse variance weights
###############################################################
pn = unique(adataAC$PathogenName)

rrates_norm <- NULL

# Loop through each pathogen in pn - normalised
for (p in pn) {
  pathogen <- p
  r <- ivw(adataAC, dbdata, year = c("2021"), pathogen =pathogen, cor = 1000000,
           pop = "yes", normalise_w = "yes")
  rrates_norm <- rbind(rrates_norm, r)  # Append the results to rrates
}

rrates <- NULL
# Loop through each pathogen in pn - not normalised
for (p in pn) {
  pathogen <- p
  r <- ivw(adataAC, dbdata, year = c("2021"), pathogen =pathogen, cor = 1000000,
           pop = "no", normalise_w = "yes")
  rrates <- rbind(rrates, r)  # Append the results to rrates
}

# Convert rrates to a data frame - normalised
rrates_norm <- data.frame(rrates_norm)
rrates_norm$Year = as.character(rrates_norm$Year)
rrates_norm$w_normalised = "yes"
rrates_norm$AbTargets[rrates_norm$AbTargets=="Cefotaxime "] = "Cefotaxime"

# Convert rrates to a data frame - not normalised
rrates <- data.frame(rrates)
rrates$w_normalised = "no"
rrates$Year = as.character(rrates$Year)
rrates$AbTargets[rrates$AbTargets=="Cefotaxime "] = "Cefotaxime"

# Link normalised and not normalised to compare
#rrates_norm_notnorm <- rbind(rrates,rrates_norm)
rrates_norm_notnorm <- full_join(rrates,rrates_norm, by=c("Specimen", "PathogenName", "AbTargets","Year"))
rrates_norm_notnorm$drugbug = c(1:length(rrates_norm_notnorm$Year))
# Change to numeric otherwise no linkage possible if not in same format
rrates2021$Year = as.character(rrates2021$Year)


# Link with 2021 data to check - not normalised
rrates_m <- full_join(rrates, rrates2021) %>% filter(Year =="2021")
#rrates <- merge(rrates2021 %>% select(Specimen, PathogenName, AbTargets), rrates)
# Print the final results
print(rrates_m)
rrates_m$drugbug = c(1:length(rrates_m$Year))

####################################################################
# PLOT ESTIMATES
####################################################################

# Plot not normalised weights vs 2021 median and IQR last year
p1 = ggplot(rrates_m%>%filter(Specimen=="BLOOD"), aes(x = AbTargets, y = p_IVW, group=drugbug)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.1, size=2,width=0.5) +
  geom_point(aes(x=AbTargets, y=median), size = 3, col="red") +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), alpha = 0.1, size=2,width=0.5, col="red") +
  labs(title = "Resistance Rate with 95% Confidence Intervals",
       x = "Antibiotic Target",
       y = "Resistance Rate") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  facet_wrap(.~ PathogenName , scales = "free_y",ncol=2) + 
  ggtitle("Inverse variance weighted mean [[95%CI] (black) vs 75% median [IQR] (red)")

# Plot not normalised weights vs normalised weights
p2 = ggplot(rrates_norm_notnorm%>%filter(Specimen=="BLOOD"), aes(x = AbTargets, y = p_IVW.x, group=drugbug)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower.x, ymax = CI_upper.x), alpha = 0.1, size=2,width=0.5) +
  geom_point(aes(x=AbTargets, y= p_IVW.y), size = 3, col="red") +
  geom_errorbar(aes(ymin = CI_lower.y, ymax = CI_upper.y), alpha = 0.1, size=2,width=0.5, col="red") +
  labs(title = "Resistance Rate with 95% Confidence Intervals",
       x = "Antibiotic Target",
       y = "Resistance Rate") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  facet_wrap(.~ PathogenName , scales = "free_y",ncol=2) + 
  ggtitle("Inverse variance weighted mean [95%CI] - not normalised (black) vs normalised (red)")

# Plot figures
pdf(file = paste0(dirOutput, "./Analyses/pop_ivw_rrates_vs_75med - BLOOD.pdf"), width=10,height=12)
print(p1)
dev.off()

# Plot figures
pdf(file = paste0(dirOutput, "./Analyses/pop_ivw_rrates_norm_vs_notnorm - BLOOD.pdf"), width=10,height=12)
print(p2)
dev.off()

write.csv(rrates_norm,file=paste0(dirOutput, "/Analyses/pop_ivw_rates_normalised.csv"))
write.csv(rrates,file=paste0(dirOutput, "/Analyses/pop_ivw_rates.csv"))
