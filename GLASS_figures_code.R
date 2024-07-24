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
pdata = read.csv(paste0(dirDataNew, "/EI_Popdta_180724_EV.csv"), sep=",")       # Population data
cdata = read.csv(paste0(dirDataNew, "/EI_Countrydta_180724_EV.csv"), sep=",")   # Country data
#sdata = read.csv(paste0(dirDataNew, "/EI_SurveillanceSites_180724_EV.csv"), sep=",") # Surveillance sites
idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_080724_EV.csv"), sep=",")                   # Implementation data
idata_old = read.csv(paste0(dirDataOld, "/Final_Curated_Data_GLASS_2023_EV/EI_Implementationdta_071123 EV.csv"), sep=",") # Surveillance sites                   # Implementation data

idata_country = read.csv(paste0(dirDataNew,"/EI_ImplementationCdta_080724_EV.csv"), sep=",")                   # Implementation data
#ihmedata = read.csv(paste0(dirDataNew, "/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv"), sep=",")

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

##############################################################
# PREAMBLE
##############################################################

# Country data
##############################################################
# Remove empty columns
cdata = cdata %>% select(-c(X, X.1,X.2))
cdata = cdata[order(cdata$Iso3),]

# Drug-bug combinations for 2023
#########################################################################
combinations2022 = dbdata %>% filter(Period %in% c("2016-","2016-2022")) %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

# Check if all are present
present = which(!unique(combinations2022$combined) %in% unique(adataAC$combined))
unique(combinations2022$combined[present])



# Link AMR data with country data
adataAC = left_join(adataAC, pdata, by=c("Iso3", "Year"))

# Remove space at the end of antibiotic name so can be found in both datasets
adataAC$AntibioticName <- sub(" $", "", adataAC$AntibioticName)

adataAC = adataAC %>% 
  mutate(
    AntibioticName = ifelse(AntibioticName == "Sulfonamides and trimethoprim", "Co-trimoxazole", AntibioticName),
    combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
    InReport = ifelse(combined %in% unique(combinations2022$combined), "Yes","No")
  )

# Link country data so we can join HAQ data
adataAC = left_join(adataAC, cdata, by=c("Iso3"))
unique(adataAC$CountryTerritoryArea)
#adataAC$CountryTerritoryArea[adataAC$CountryTerritoryArea=="C\xf4te d'Ivoire"] = "Côte d'Ivoire"
#unique(haqdata$location_name)

table(adataAC$InReport)

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


rrates <- adataAC %>% filter(Year == 2021) %>%
  group_by(Iso3, Specimen, PathogenName, AntibioticName) %>%
  reframe(amr_rate = Resistant/InterpretableAST,
            BCI_1000000pop = InterpretableAST/TotalPopulation*1000000)

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

# AST vs Isolates per 1000000 pop
####################################################################################

p1 = ggplot(rrates %>% filter(PathogenName=="Escherichia coli", AntibioticName!="Doripenem"), aes(x=BCI_1000000pop, y=amr_rate, 
                                                                     col=AntibioticName,group=AntibioticName)) +
  geom_point(size=2) +  
  geom_smooth(se=F) +
  facet_wrap(.~ Specimen, ncol=4) +
  xlim(0,1000) +
  ylim(0,1) + 
  labs(
    title = "AMR Rates for E. coli")

p2 = ggplot(rrates %>% filter(PathogenName=="Klebsiella pneumoniae"), aes(x=BCI_1000000pop, y=amr_rate, 
                                                                          col=AntibioticName, group=AntibioticName)) +
  geom_point(size=2) +  
  geom_smooth(se=F) +
  facet_wrap(.~ Specimen, ncol=4) +
  xlim(0,1000) +
  ylim(0,1) + 
  labs(
    title = "AMR Rates for K. pneumoniae")

p3 = ggplot(rrates %>% filter(PathogenName=="Acinetobacter spp."), aes(x=BCI_1000000pop, y=amr_rate, 
                                                                       col=AntibioticName, group=AntibioticName)) +
  geom_point(size=2) +  
  geom_smooth(se=F) +
  facet_wrap(.~ Specimen, ncol=4) +
  xlim(0,250) +
  ylim(0,1) + 
  labs(
    title = "AMR Rates for Acinetobacter spp.")

p4 = ggplot(rrates %>% filter(PathogenName=="Salmonella spp."), aes(x=BCI_1000000pop, y=amr_rate, col=AntibioticName,
                                                                    group=AntibioticName)) +
  geom_point(size=2) +  
  geom_smooth(se=F) +
  facet_wrap(.~ Specimen, ncol=4) +
  xlim(0,250) +
  ylim(0,1) + 
  labs(
    title = "AMR Rates for Salmonella spp.")

multiplot(p1,p2,cols=1)
multiplot(p3,p4,cols=1)

# Modelled resistance rates 
###############################################################

# First trial with just E. coli 
#combinations2022ec_BLOOD = combinations2022 %>% filter(PathogenName%in% c("Escherichia coli")&
#                                                       Specimen=="BLOOD")
ecolidata = adataAC %>% filter(PathogenName == "Escherichia coli" & Year == 2021 & InReport=="Yes")
unique(ecolidata$combined) # Count as 27 so all should be there

# Create list of data.frames
data_sets = list()

for(i in 1:length(unique(ecolidata$combined))) {
  d = ecolidata # Change to data to use
  data_subset <- adataAC %>% 
    filter(Specimen == d$Specimen[i],
           PathogenName == d$PathogenName[i],
           AntibioticName == d$AntibioticName[i],
           Year == 2021) %>%
    group_by(Iso3) %>%
    reframe(AMRrate = Resistant/InterpretableAST,
            Specimen = d$Specimen[i],
           PathogenName = d$PathogenName[i],
           AntibioticName = d$AntibioticName[i],
           Resistant = Resistant,
           InterpretableAST = InterpretableAST,
           TotalPopulation = TotalPopulation,
           BCI_pop100000 = InterpretableAST/TotalPopulation*100000,
           Year = 2021)
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
# First fit model to the first dataset
#fit <- fit_amr_model(data_sets[[1]], formula, priors)

# Define the model formula
formula <- bf(Resistant | trials(InterpretableAST) ~ 1 + (1 | Iso3)) #
#formula2 <- bf(Resistant | trials(InterpretableAST) + weights(TotalPopulation) ~ 1 + (1 | Iso3)) # This is weighting by population size

# Define the priors
priors <- c(
  prior(normal(0, 1), class = "Intercept"),    # Prior for the fixed effects (including intercept)
  prior(cauchy(0, 1), class = "sd")            # Prior for the standard deviation of the random effect
)

priors

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


palette <- brewer.pal(7, "Set1")  # Choose a palette with three colors


# Plot the AMR estimates - per pathogen, antibiotics combined
#################################################################################

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