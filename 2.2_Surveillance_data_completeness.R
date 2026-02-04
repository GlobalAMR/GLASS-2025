#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(readxl, rio, lubridate, zoo, ggplot2, Hmisc, stringr,dplyr,
               magrittr, officer, janitor, sf, gtsummary, leaflet, 
               gridExtra, grid, purrr, brms, cowplot, ggrepel, grid, wesanderson,
               patchwork, ggh4x, ggalluvial, networkD3, tidyr, forcats, ggtext,
               glue, RColorBrewer, scales, svglite, reshape2, forcats)


#remotes::install_github("glaziou/whomap")

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
dirDataRaw = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/"

dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"
dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables/2021/"
dirDataModeloutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Model_output/model_output/ALL/all_one_model_weakip_set1"

dirOutputReport = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 REPORT WRITING/FINAL 2023 FIGURES TABLES AND RESULTS"

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
idata_usage = read.csv(paste0(dirDataNew,"/EI_Implementation_usage_dta_140325_EV.csv"), sep=",")                   # Implementation data

# HAQI data
haqidata = read.csv(paste0(dirDataRaw, "/HAQI_raw/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

# AMR data
#adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_Pop_Country_HAQI_030924_EV.csv"), sep=",")   # Country AMR data
adataAC = read.csv(paste0(dirDataNewO, "/EI_AMRdtaAC_110325_EV.csv"), sep=",")   # Country AMR data; use CRUDE data as wrangled by OLGA, where E.coli and MRSA are not manually changed
adataDM = read.csv(paste0(dirDataNew, "/EI_AMRdtaDM_Country_140325_EV.csv"), sep=",")   # Country AMR data
#adataNT = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_Pop_Country_030924_EV.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "./updated_summary_dbc_longformat.csv"), sep=",")

# EAR data
#edata = read_excel(paste0(dirDataNew, "/EAR_events_2019_2024_EV.xlsx"))

# rrates 2021
# rrates2021 = read_excel(paste0(dirOutputCheck, "/rrates_2021_75percentile.xlsx")) 
# rrates2021 = rrates2021%>% filter(Q1!="NA") %>% mutate(
#   Q1 = as.numeric(Q1),
#   Q3 = as.numeric(Q3),
#   median = as.numeric(median)
# )

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

# Bed density
#bdays = read.csv(paste0(dirDataNewO, "/EI_BedDensitydta_180724_EV.csv"), sep=",")

year = 2023 # CHANGE TO YEAR OF GLASS DATA BEING ANALYSED

# Link population and country data
adataAC = left_join(adataAC, cdata)
adataAC = left_join(adataAC, pdata)


# ANALYSES TO POPULATE TABLE
###################################################################################################

# NATIONAL SURVEILLANCE SYSTEM INDICATORS
#--------------------------------------------------------------------------------------------------

f1 = read.csv(paste0(dirDataNewO, "/EI_Implementationdta_110325_EV.csv"), header=T)

#f1<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Implementationdta_280824.csv", header=T)
#attach(f1)
#fix(f1)

f2 = read.csv(paste0(dirDataNewO, "/EI_Countrydta_110325_EVK.csv"), header=T)

#f2<-read.csv("C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLASS_AMR_WHO\\GLASS_2024_FILES\\FINAL CURATED GLASS 2024 DATA\\EI_Countrydta_180724.csv", header=T)
#attach(f2)
#fix(f2)


f2 = f2[order(f2$Iso3),] %>%
  mutate(
    CountryTerritoryArea = case_when(
      CountryTerritoryArea == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire",
      TRUE ~ CountryTerritoryArea)
  )

f3<-merge(f1,f2,by="Iso3",all.x=TRUE)
f4 <-  select(f3, - c("Year", "CountryTerritoryArea", "IncomeWorldBankJune2023", "EnrolledAMR", "EnrollmentDateAMR", "EnrolledAMC", "EnrollmentDateAMC", "WHOLegalStatus", "Candida", "EGASP",
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

f12 <-  select(f11, - c("AMR_NCC", "AMR_NRL", "EQA_to_NRL", "AMR_AST_standards", "AMR_EQA_GLASS_labs")) 



f12b <- f12 %>%
  mutate(Core = case_when((NCC == "Available" & NRL == "Available" & EQA_NRL =="Available" & International_AST_Standards =="Available")  ~ "Available",
                          (NCC =="No data" | NRL == "No data" | EQA_NRL == "No data" | International_AST_Standards == "No data") ~ "No data",
                          TRUE ~ "Not available"))

colB_nssi = f12b %>%
  mutate(colB_nssi = 
           ifelse(f12b$Core=="Available", 2, 
                  ifelse(f12b$Core=="Not available", 1, 0))) %>%
  select(Iso3, colB_nssi)

# Manually change STP to 1
colB_nssi$colB_nssi[colB_nssi$Iso3=="STP"] = 1

# NATIONAL HEALTH INFRASTRUCTURE SUMMARIES INPATIENT
#----------------------------------------------------------------------------

  # ColC_infs_inp: infrastructure summaries inpatient, i.e. amr_amr_hospital_number + amr_amr_acute_care_number
  # ColD_util_inp: infrastructure summaries inpatient, i.e. amr_amr_inpatient_adm_number + amr_amr_inpatient_day_number
  # ColE_util_outp: infrastructure summaries inpatient, i.e. amr_amr_outpatient_cons_number
  # ColF_GLASS_infs_inp: infrastructure summaries inpatient, i.e. amr_glass_hospital_number + amr_glass_acute_care_number
  # ColG_GLASS_util_inp: infrastructure summaries inpatient, i.e. amr_glass_inpatient_adm_number + amr_glass_inpatient_day_number
  # ColH_GLASS_util_outp: infrastructure summaries inpatient, i.e. amr_glass_outpatient_cons_number
  # ColI_dashb: 

u1 = idata_usage

# Merge with population data to do quality checks
u2 = left_join(u1, pdata%>%filter(Year==year), by="Iso3")

# Now quality indicator for surveillance variables
u3 = u2 %>%
  mutate(
    amr_amr_hospitals_number = ifelse(amr_amr_hospitals_number >= amr_amr_acute_care_number, "not_reliable",amr_amr_hospitals_number),
    amr_amr_acute_care_number = ifelse(amr_amr_acute_care_number <10 |amr_amr_acute_care_number<amr_amr_hospitals_number, "not_reliable", amr_amr_acute_care_number),
    amr_amr_inpatient_adm_number = ifelse(amr_amr_inpatient_adm_number< amr_amr_hospitals_number | amr_amr_inpatient_adm_number<0.01*TotalPopulation,
                                          "not_reliable", amr_amr_inpatient_adm_number),
    amr_amr_inpatient_day_number = ifelse(amr_amr_inpatient_day_number<amr_amr_inpatient_adm_number, "not_reliable", amr_amr_inpatient_day_number),
    #amr_amr_outpatient_cons_number = ifelse(amr_amr_outpatient_cons_number< (2*as.numeric(amr_amr_inpatient_adm_number)), "not_reliable", amr_amr_outpatient_cons_number),
    amr_glass_acute_care_number = ifelse(amr_glass_acute_care_number>amr_amr_acute_care_number, "not_reliable",amr_glass_acute_care_number),
    amr_glass_hospitals_number = ifelse(amr_glass_hospitals_number>amr_amr_hospitals_number | amr_glass_hospitals_number>amr_glass_acute_care_number,
                                        "not_reliable", amr_glass_hospitals_number),
    amr_glass_inpatient_adm_number = ifelse(amr_glass_inpatient_adm_number<amr_glass_hospitals_number|amr_glass_inpatient_adm_number>amr_amr_inpatient_adm_number,
                                            "not_reliable", amr_glass_inpatient_adm_number),
    amr_glass_inpatient_day_number = ifelse(amr_glass_inpatient_day_number<=amr_glass_inpatient_adm_number|amr_glass_inpatient_day_number>amr_amr_inpatient_day_number,
                                            "not_reliable", amr_glass_inpatient_day_number),
    amr_glass_outpatient_cons_number = ifelse(amr_glass_outpatient_cons_number>amr_amr_outpatient_cons_number, "not_reliable", amr_glass_outpatient_cons_number),
    lab_number_data_call = ifelse(lab_number_data_call>amr_amr_acute_care_number, "not_reliable",lab_number_data_call)   
  )


u4 = u3%>%
  mutate(
    colC_infs_inp = case_when(
      !is.na(amr_amr_acute_care_number) & !is.na(amr_amr_hospitals_number) & amr_amr_acute_care_number!="not_reliable"&
        amr_amr_hospitals_number!="not_reliable" ~ 2,
      (is.na(amr_amr_acute_care_number) | amr_amr_acute_care_number=="not_reliable") & (is.na(amr_amr_hospitals_number) | amr_amr_hospitals_number=="not_reliable") ~ 0,
      TRUE ~ 1),
    # ColD_util_inp: infrastructure summaries inpatient, i.e. amr_amr_inpatient_adm_number + amr_amr_inpatient_day_number
    colD_util_inp = case_when(
      !is.na(amr_amr_inpatient_adm_number) & !is.na(amr_amr_inpatient_day_number) & amr_amr_inpatient_adm_number!="not_reliable"&
        amr_amr_inpatient_day_number!="not_reliable" ~ 2,
      (is.na(amr_amr_inpatient_adm_number) |  amr_amr_inpatient_adm_number=="not_reliable") & (is.na(amr_amr_inpatient_day_number) | amr_amr_inpatient_day_number=="not_reliable") ~ 0,
      TRUE ~ 1),
    # ColE_util_outp: infrastructure summaries inpatient, i.e. amr_amr_outpatient_cons_number
    colE_util_outp = case_when(
      !is.na(amr_amr_outpatient_cons_number) & amr_amr_outpatient_cons_number!="not_reliable" ~ 2,
      is.na(amr_amr_outpatient_cons_number) | is.na(amr_amr_outpatient_cons_number=="not_reliable") ~ 0,
      TRUE ~ 0),
    # ColF_GLASS_infs_inp: infrastructure summaries inpatient, i.e. amr_glass_hospital_number + amr_glass_acute_care_number
    ColF_GLASS_infs_inp = case_when(
      !is.na(amr_glass_acute_care_number) & !is.na(amr_glass_hospitals_number) & amr_glass_acute_care_number!="not_reliable"&
        amr_glass_hospitals_number!="not_reliable" ~ 2,
      (is.na(amr_glass_acute_care_number) | amr_glass_acute_care_number=="not_reliable") & (is.na(amr_glass_hospitals_number) | amr_glass_hospitals_number=="not_reliable") ~ 0,
      TRUE ~ 1),
    # ColG_GLASS_util_inp: infrastructure summaries inpatient, i.e. amr_glass_inpatient_adm_number + amr_glass_inpatient_day_number
    ColG_GLASS_util_inp = case_when(
      !is.na(amr_glass_inpatient_adm_number) & !is.na(amr_glass_inpatient_day_number) & amr_glass_inpatient_adm_number!="not_reliable"&
        amr_glass_inpatient_day_number!="not_reliable" ~ 2,
      (is.na(amr_glass_inpatient_adm_number) |amr_glass_inpatient_adm_number=="not_reliable") & (is.na(amr_glass_inpatient_day_number) | amr_glass_inpatient_day_number=="not_reliable") ~ 0,
      TRUE ~ 1),
    # ColH_GLASS_util_outp: infrastructure summaries inpatient, i.e. amr_glass_outpatient_cons_number
    ColH_GLASS_util_outp = case_when(
      !is.na(amr_glass_outpatient_cons_number) & amr_glass_outpatient_cons_number!="not_reliable" ~ 2,
      is.na(amr_glass_outpatient_cons_number) | is.na(amr_glass_outpatient_cons_number=="not_reliable") ~ 0,
      TRUE ~ 0)
    # ColI_dashb: )
  )

colC_H = u4 %>% select(
  Iso3, colC_infs_inp, colD_util_inp, colE_util_outp, ColF_GLASS_infs_inp, ColG_GLASS_util_inp, ColH_GLASS_util_outp
)


# NSAMPLED
#-----------------------------------------------------------------------------------------------------------------------

d = adataAC %>% filter(Year == year)

# ColI_nsampled: If sampled patients < TotalSpecimen isolates, mark as not reliable, although could technically be possible, 
# If patient is tested for multiple antibiotics
# Take max per specimen 

# plot(d$NumSampledPatients, d$TotalSpecimenIsolates, xlim=c(0,500000))
# table(adataAC$NumSampledPatients, useNA="always")

all_countries <- unique(d$Iso3)
all_specimens <- unique(d$Specimen)

# Summarize and then complete for all combinations of countries and specimens
e1 <- d %>%
  group_by(Iso3, Specimen) %>%
  summarise(
    NumSampledPatients = if (all(is.na(NumSampledPatients))) NA else max(NumSampledPatients, na.rm = TRUE),
    TotalSpecimenIsolates = if (all(is.na(TotalSpecimenIsolates))) NA else max(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Ensure all country and specimen combinations are present
  complete(Iso3 = all_countries, Specimen = all_specimens) 

# THE BELOW CONSIDERS THAT IF A COUNTRY DOES NOT REPORT ON A PARTICULAR SPECIMEN, 
# NO NUMBER OF SAMPLES IS TO BE EXPECTED, SO THEN THE nsampled_byspec = 1, so 
# TOTAL SCORE ADDS UP TO 4 EVEN IF A COUNTRY DOES NOT REPORT ON ALL 4.
e2 = e1 %>%
  mutate(
    nsampled_byspec = ifelse(is.na(NumSampledPatients) & !is.na(TotalSpecimenIsolates), 0, 1)
  )

ColI_nsamp = e2 %>%
  group_by(Iso3) %>%
  summarise(nsamp_total = sum(nsampled_byspec, na.rm=T)) %>%
  mutate(ColI_nsamp = case_when(
    nsamp_total==4 ~ 2,
    nsamp_total==0 ~ 0,
    TRUE ~ 1
  )) %>% select(Iso3, ColI_nsamp)

# AGE AND SEX MISSINGNESS
#-----------------------------------------------------------------------------

da = adataAS %>% filter(Year==year)

# AGE
agecat = unique(da$AgeCat10[!da$AgeCat10=="UnkAge"])

f1 = da %>%
  group_by(Iso3, Specimen, AgeCat10) %>%
  summarise(
    TotalSpecimenIsolates = if (all(is.na(TotalSpecimenIsolates))) NA else max(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) 


ColK_age = f1 %>%
  group_by(Iso3, Specimen) %>%
  summarise(
    UnkAge_Iso = ifelse(any(AgeCat10 == "UnkAge"), TotalSpecimenIsolates[AgeCat10 == "UnkAge"], 0),
    Total_Iso = sum(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    F_UnkAge = UnkAge_Iso/Total_Iso,
    UnkAge_spec = case_when(
      F_UnkAge < 0.3 ~ 1,
      F_UnkAge > 0.3 ~ 0)
  ) %>% group_by(Iso3) %>%
  summarise(
    sum_unkAge_spec = sum(UnkAge_spec, na.rm=T)) 
  

n_spec = f1 %>%
  group_by(Iso3) %>%
  summarise(n = length(unique(Specimen))
)

ColK_age = left_join(ColK_age, n_spec) %>%
  mutate(
    ColK_age = case_when(
      sum_unkAge_spec == n ~ 2,
      sum_unkAge_spec =! 0 & sum_unkAge_spec != n ~ 1, # If for less then 2 specimen types >30% missing, then assign 0
      TRUE ~ 0
    ))%>% select(Iso3, ColK_age)

# SEX
g1 = da %>%
  group_by(Iso3, Specimen, Sex) %>%
  summarise(
    TotalSpecimenIsolates = if (all(is.na(TotalSpecimenIsolates))) NA else max(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) 

ColL_sex = g1 %>%
  group_by(Iso3, Specimen) %>%
  summarise(
    UnkSex_Iso = ifelse(any(Sex == "Unknown"), TotalSpecimenIsolates[Sex == "Unknown"], 0),
    Total_Iso = sum(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    F_UnkSex = UnkSex_Iso/Total_Iso,
    UnkSex_spec = case_when(
      F_UnkSex < 0.3 ~ 1,
      F_UnkSex > 0.3 ~ 0)
  ) %>% group_by(Iso3) %>%
  summarise(
    sum_unkSex_spec = sum(UnkSex_spec, na.rm=T)) 


ColL_sex  = left_join(ColL_sex, n_spec) %>%
  mutate(
    ColL_sex  = case_when(
      sum_unkSex_spec == n ~ 2,
      sum_unkSex_spec =! 0 & sum_unkSex_spec != n ~ 1, # If for less then 2 specimen types >30% missing, then assign 0
      TRUE ~ 0
    ))%>% select(Iso3, ColL_sex)

# ORIGIN
dao = adataDM%>%filter(DemographicsOrigin%in%c("CO", "HO", "UnkOrigin") & Year==year)


h1 = dao %>%  group_by(Iso3, Specimen, DemographicsOrigin) %>%
  summarise(
    TotalSpecimenIsolates = if (all(is.na(TotalSpecimenIsolates))) NA else max(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) 

ColM_origin = h1 %>%
  group_by(Iso3, Specimen) %>%
  summarise(
    UnkOrigin_Iso = ifelse(any(DemographicsOrigin == "UnkOrigin"), TotalSpecimenIsolates[DemographicsOrigin == "UnkOrigin"], 0),
    Total_Iso = sum(TotalSpecimenIsolates, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    F_UnkOrigin = UnkOrigin_Iso/Total_Iso,
    UnkOrigin_spec = case_when(
      F_UnkOrigin < 0.3 ~ 1,
      F_UnkOrigin > 0.3 ~ 0)
  ) %>% group_by(Iso3) %>%
  summarise(
    sum_unkOrigin_spec = sum(UnkOrigin_spec, na.rm=T)) 

ColM_origin  = left_join(ColM_origin, n_spec) %>%
  mutate(
    ColM_origin  = case_when(
      sum_unkOrigin_spec == n ~ 2,
      sum_unkOrigin_spec =! 0 & sum_unkOrigin_spec != n ~ 1, # If for less then 2 specimen types >30% missing, then assign 0
      TRUE ~ 0
    ))%>% select(Iso3, ColM_origin)


# ISOLATES REPORTED
#-----------------------------------------------------------------------

d = adataAC %>% filter(!AntibioticName%in%c("Ampicillin")) %>% 
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
                              (AntibioticName == "Methicillin-resistance") ~ "Methicillin-resistance"),
         combined = paste0(Specimen, "-" , PathogenName, "-", Grouping)
  )


ii = d %>% filter(Year==year) %>%
  mutate(reported = ifelse(InterpretableAST>10, 1,0 )) %>%
  group_by(Iso3, WHORegionName, Specimen, combined) %>%
  summarise(
    reported = sum(reported)
  ) %>%
  mutate(
    reported = ifelse(reported>0, 1, 0)
  ) %>%
  group_by(Iso3,WHORegionName, Specimen) %>%
  summarise(
    reported_total = sum(reported)
  )

# Unique number of combinations per specimen type
sp = data.frame(table(d$combined, d$Specimen))

den = sp %>%
  filter(Freq > 0) %>%
  group_by(Var2) %>%
  summarise(UniqueVar1Count = n_distinct(Var1)) %>%
  rename(
    Specimen = Var2,
    Total = UniqueVar1Count 
  )
den

ii2 = left_join(ii, den)
ii2 = ii2 %>%
  mutate(prop_reported = reported_total/Total)

all_combinations <- denall_combinations <- expand.grid(
  Iso3 = unique(d$Iso3),
  Specimen = unique(d$Specimen)
)

# colN_Q_nIso <- ii2 %>%
#   group_by(Iso3, Specimen) %>%
#   mutate(
#     ColN_nBLOOD = ifelse(TotalSpecimenIsolates >10, 2, 1),
#     ColO_nURINE = ifelse(TotalSpecimenIsolates >10, 2, 1),
#     ColP_nSTOOL = ifelse(TotalSpecimenIsolates >10, 2, 1),
#     ColQ_nURO = ifelse(TotalSpecimenIsolates >10, 2, 1)
#   )

colN_Q_nIso <- ii2 %>%
  group_by(Iso3, WHORegionName, Specimen) %>%
  mutate(
    ColN_nBLOOD = case_when(
      prop_reported > 0.50 ~ 2,
      prop_reported > 0 & prop_reported <= 0.50 ~ 1,
      TRUE ~ 0
    ),
    ColO_nURINE = case_when(
      prop_reported > 0.50 ~ 2,
      prop_reported > 0 & prop_reported <= 0.50 ~ 1,
      TRUE ~ 0
    ),
    ColP_nSTOOL = case_when(
      prop_reported > 0.50 ~ 2,
      prop_reported > 0 & prop_reported <= 0.50 ~ 1,
      TRUE ~ 0
    ),
    ColQ_nURO = case_when(
      prop_reported > 0.50 ~ 2,
      prop_reported > 0 & prop_reported <= 0.50 ~ 1,
      TRUE ~ 0
    )
  )

colN_Q_nIso <- all_combinations %>%
  left_join(colN_Q_nIso, by = c("Iso3", "Specimen")) %>%
  mutate(
    ColN_nBLOOD = ifelse(is.na(ColN_nBLOOD)| ColN_nBLOOD==0, 0,  ColN_nBLOOD),
    ColO_nURINE = ifelse(is.na(ColO_nURINE)| ColO_nURINE==0, 0,  ColO_nURINE),
    ColP_nSTOOL = ifelse(is.na(ColP_nSTOOL)| ColP_nSTOOL==0, 0,  ColP_nSTOOL),
    ColQ_nURO = ifelse(is.na(ColQ_nURO)| ColQ_nURO==0, 0,  ColQ_nURO)
  )

colN_Q_nIso_wide <- colN_Q_nIso %>%
  pivot_wider(
    id_cols = Iso3,  # Keep Iso3 as row identifiers
    names_from = Specimen,
    values_from = c(ColN_nBLOOD, ColO_nURINE, ColP_nSTOOL, ColQ_nURO)
  ) %>% select(Iso3, ColN_nBLOOD_BLOOD, ColO_nURINE_URINE, ColP_nSTOOL_STOOL, ColQ_nURO_UROGENITAL)


# COMBINED ALL
#--------------------------------------------------------------------------------------
# ADD WEIGHTHING COMPONENT, I.E. Col c_h = 0,1,2 scores; col I to m = 0,2,4; col N to Q = 0,4,6
# ADD WEIGHTHING COMPONENT, I.E. Col c_h = 0,1,2 scores; col I to m = 0,2,4; col N to Q = 0,4,6
# ColI_nsamp$ColI_nsamp = ColI_nsamp$ColI_nsamp*2
# ColK_age$ColK_age = ColK_age$ColK_age*2 
# ColL_sex$ColL_sex = ColL_sex$ColL_sex*2 
# ColM_origin$ColM_origin = ColM_origin$ColM_origin*2
# colN_Q_nIso_wide[,2:5] = colN_Q_nIso_wide[,2:5]*3


k = left_join(colB_nssi, colC_H, by="Iso3")
k2 = left_join(k, ColI_nsamp)
k3 = left_join(k2, ColK_age)
k4 = left_join(k3, ColL_sex)
k5 = left_join(k4, ColM_origin)
k6 = left_join(k5, colN_Q_nIso_wide)
k6 = k6 %>%
  group_by(Iso3) %>%
  mutate(
    TotalScore = rowSums(across(where(is.numeric)), na.rm = TRUE),
  #  TotalScore = ifelse(TotalScore<3, NA, TotalScore)
  )

length(unique(k6$Iso3)) # 130 CTAs
k7 = left_join(k6, cdata %>% dplyr::select(Iso3, WHORegionName))
k7[,c(2:17)] = sapply(k7[,c(2:17)], function(x) ifelse(is.na(x), 0, x))

write.csv(k7, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata_NEW2.csv"))

c2 <- k6 %>%
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
    Datatype = factor(Datatype, levels=c("Surveillance_core_components", "Surveillance_coverage","AMR_specimen", "AMR_covariate_denominator",
                                         "Total_Score")),
    Value_label = case_when(
      Value == 0 ~ "No data",
      Value == 1 ~ "Some data",
      Value == 2 ~ "Good data",
      Value %in% c(3:6) ~ "Low completeness", # 0-20% of total score of 30
      Value %in% c(7:15) ~ "Medium-low completeness", # 20-50%
      Value %in% c(16:24) ~ "Medium-high completeness",#51-80%
      Value %in% c(25:30) ~ "High completeness", #>80%
      is.na(Value) ~ "No data"
    )
    
    # Value = fct_explicit_na(as.factor(Value), na_level = "Missing Data"),
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
  "ColO_nURINE_URINE" = "UTI >10 with AST",
  "ColP_nSTOOL_STOOL" = "GI >10 with AST",
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
      "Medium-low completeness" = "Medium-low (21-50%)",
      "Medium-high completeness" = "Medium-high (51-80%)",
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
      "ColO_nURINE_URINE" = "Urinary tract",
      "ColP_nSTOOL_STOOL" = "Gastrointestinal", 
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


# CHECK AFTER CHANGE OF SCORE FOR OUTPATIENT
# -------------------------------------------------------------
# f1 = read.csv(file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata.csv"))
# 
# f_old = readxl::read_xlsx(path = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.5_missingdata_definitions.xlsx"),sheet=1)
# names(f_old)
# f_old = f_old %>% select(-c("...1"))
# f1= f1 %>% select(-c("X"))
# 
# f_old[,c(2:17)] = sapply(f_old[,c(2:17)], function(x) as.numeric(x))
# f1[,c(2:17)] = sapply(f1[,c(2:17)], function(x) as.numeric(x))
# 
# 
# table(f1$colE_util_outp)
# table(f1$ColH_GLASS_util_outp)
# 
# table(f_old$colE_util_outp)
# table(f_old$ColH_GLASS_util_outp)
# 
# library(diffdf)
# diffdf(f1, f_old)
