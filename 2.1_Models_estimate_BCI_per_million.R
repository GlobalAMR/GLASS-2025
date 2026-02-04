######################################################
# BCI over time
######################################################

# Author: Esther van Kleef
# Date last updated: 22 April 2025

#######################################
# GLASS REPORT - FIGURES
#######################################
rm(list=ls())

# Load R packages
pacman::p_load(ggplot2, dplyr, tidyr, brms, data.table, bayesplot,wesanderson, glmmTMB, MASS,patchwork, sjPlot,
               openxlsx)

# Locate directories
dirDataOld = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/GLASS HISTORICAL DATA EV"
dirDataNewO = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated"
dirDataNew = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/GLASS_final_curated/GLASS_final_curated_linked"
dirDataRaw = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/FINAL DATA FOR 2025 GLASS REPORT/"

dirOutput = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2025 ANALYSIS EV/2025 Figures_Tables"
#dirOutputCheck = "C:/Users/esthe/World Health Organization/GLASS Data Visualization - Esther work - GLASS 2024/2024 ANALYSIS EV/2024 Figures_Tables/2021/"
dirOutputModel = "C:/Users/esthe/OneDrive - VanKleefBV/Documenten/Documenten/WHO/WHO_GLASS/2025 ANALYSIS EV/2025 Model_output/model_output_surveillance_coverage/"

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

# AMR data
adataAC_crude = read.csv(paste0(dirDataNewO, "/EI_AMRdtaAC_110325_EV.csv"), sep=",")   # Country AMR data

adataAC = read.csv(paste0(dirDataNew, "/EI_AMRdtaAC_ANALYSES.csv"), sep=",")   # Country AMR data
adataAS = read.csv(paste0(dirDataNew, "/EI_AMRdtaINT_ANALYSES.csv"), sep=",")   # Country AMR data

idata = read.csv(paste0(dirDataNew,"/EI_Implementationdta_Country_140325_EV.csv"), sep=",")                   # Implementation data

# List of drug bug combinations
dbdata = read.csv(paste0(dirDataNew, "./updated_summary_dbc_longformat.csv"), sep=",")

# Drug bug combinations to include in report
combinations2022 = dbdata %>% 
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName))

#################################################################
# COLOURS
#################################################################

palette <- wes_palette("Darjeeling1", n = 5)
palette2 <- wes_palette("BottleRocket2", n = 1)
palette3 <- wes_palette("GrandBudapest1", n = 2)[2]
palette4 <- wes_palette("BottleRocket2", n = 2)[2]
palette5 = c(palette3, palette[2],palette2,palette[5], palette[4],palette4)

palette_map = c(palette2, palette[2],palette3)


facet_colors3 <- c(
  "Global" = "grey",
  "African Region" = palette5[1],
  "Region of the Americas" = palette5[2],
  "South-East Asia Region" = palette5[5],
  "European Region" = palette5[4],
  "Eastern Mediterranean Region" = palette5[3],
  "Western Pacific Region" = palette5[6]
)


##################################################################
# DATA WRANGLING
##################################################################

m1 = adataAC_crude %>% 
  filter(TotalSpecimenIsolates>0) %>%   # Filter the data first
  droplevels() 

m1 = m1 %>%
  mutate(combined = paste0(Specimen,"-", PathogenName,"-", AntibioticName),
         combined = gsub("\\s+", " ", trimws(combined)),
         InReport = ifelse(combined %in% combinations2022$combined, "Yes","No")
  )

unique(m1$combined)
combinations2022$combined[!combinations2022$combined %in% m1$combined]

sort(unique(m1$combined))

m1 = left_join(m1, cdata)
m1 = left_join(m1, pdata)

m2<- m1 %>% 
  as.data.frame() %>%
  group_by(WHORegionCode, Iso3, Year, Specimen) %>% 
  summarize_all(max) %>%
  as.data.frame() %>% 
  dplyr::select(WHORegionName,WHORegionCode, Iso3, Year, Specimen, SpecimenIsolateswithAST, TotalPopulation)


m3<-m2 %>% 
  mutate(BCI_permillion = ((coalesce(SpecimenIsolateswithAST,0)/(coalesce(TotalPopulation,0))*1000000)),
         Year_scaled = scale(Year, center = TRUE, scale = F)[, 1]) # Centers year

# MAKE SUMMARY TABLE (TO COMPARE WITH DASHBOARD AND STORE FOR REFERENCE IN REPORT)
ms1 = m3 %>% 
   group_by(WHORegionCode, Specimen, Year) %>%
   summarise(SpecimenIsolateswithAST = sum(SpecimenIsolateswithAST),
             nCTA = length(unique(Iso3)),
             median = median(BCI_permillion),
             min = min(BCI_permillion),
             Q1 = quantile(BCI_permillion, probs=0.25),
             Q3 = quantile(BCI_permillion, probs=0.75),
             max = max(BCI_permillion))
 
ms1_global =m3 %>% 
   group_by(Specimen, Year) %>%
   summarise(SpecimenIsolateswithAST = sum(SpecimenIsolateswithAST),
             nCTA = length(unique(Iso3)),
             median = median(BCI_permillion),
             min = min(BCI_permillion),
             Q1 = quantile(BCI_permillion, probs=0.25),
             Q3 = quantile(BCI_permillion, probs=0.75),
             max = max(BCI_permillion)) %>%
   mutate(
     WHORegionCode = "Global"
   ) %>% 
   dplyr::select(WHORegionCode, Specimen, Year, SpecimenIsolateswithAST, nCTA, min, Q1, median,Q3,max)
 
ms2 = 
   rbind(ms1, ms1_global)
 
#write.csv(ms2, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_million_specimen_overtime_crude_data.csv"))
wb <- createWorkbook()
addWorksheet(wb, "Crude_summaries_spec")

# Write data to each sheet
writeData(wb, sheet = "Crude_summaries_spec", x = ms2)

saveWorkbook(wb, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), overwrite = TRUE)

 region_labels <- c(
   "African Region" = "African\nRegion",
   "Region of the Americas" = "Region of the \nAmericas",
   "South-East Asia Region" = "South-East\nAsia Region",
   "European Region" = "European\nRegion",
   "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
   "Western Pacific Region" = "Western\nPacific Region"#,
   # "Global" = "Global"
 )
 
 custom_labeller <- labeller(
   WHORegionName = as_labeller(region_labels),
 )

global <- ggplot(data = m3, aes(x = Year, y = BCI_permillion, color = Specimen, group = Specimen)) +
  geom_jitter(aes(size = BCI_permillion), width = 0.2, height = 0.2, alpha = 0.2) +  # Jittered points
  geom_smooth(size = 1.5, se = FALSE, method="loess") +  # Smoothed lines
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1.5, se = T, colour="seagreen")+
  geom_smooth(method = "lm", formula = y ~ x, size = 1.5, se = F, colour="orange")+
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1.5, se = F)+
  facet_wrap( ~ Specimen, scales = "free_y", ncol=6) +
  scale_x_continuous(breaks = c(2018, 2020, 2022)) +
  labs(title = "Bloodstream",
       subtitle = "Regional", 
       x = "",
       y = "BCI per million",
       color = "Region",
       fill = "") +
  #scale_color_manual(values = value_colors, name="") +  # Custom color palette for lines and points
  #scale_fill_manual(values = facet_colors) +   # Custom color palette for ribbons
  theme_minimal() +
  theme(plot.background = element_rect(
    fill = "white",
    colour = "white"),
    title = element_text(size = 20),
    axis.text.x = element_text(size = 14),
    #axis.text.y = element_text(size = 18),  
    axis.text.y = ggtext::element_markdown(size = 18),
    strip.text.y.left = element_text(size = 18, angle = 0, vjust = 1.01, hjust = 1),  
    strip.text = element_text(size = 20),   
    #strip.text = element_blank(),   
    #strip.text.y = element_blank(),   
    strip.placement = "outside",            
    strip.background = element_blank(),      
    panel.grid.major = element_line(linetype = "dotted"),  # Dotted major grid lines
    panel.grid.minor = element_line(linetype = "dotted"),
    legend.position = "bottom",
    legend.text = element_text(size = 14)) #+ 
# guides(fill = guide_legend(nrow = 2), size=FALSE) 
global

# By region
p1 = plot_bci_spec_trend_regional(m3, specimen = "BLOOD")
p2 = plot_bci_spec_trend_regional(m3, specimen = "URINE")
p3 = plot_bci_spec_trend_regional(m3, specimen = "STOOL")
p4 = plot_bci_spec_trend_regional(m3, specimen = "UROGENITAL")

combined_plot = p1+p2+p3+p4 + plot_layout(1)

# By country
pc1 = plot_bci_spec_trend_cta(m3, specimen = "BLOOD")
pc2 = plot_bci_spec_trend_cta(m3, specimen = "URINE")
pc3 = plot_bci_spec_trend_cta(m3, specimen = "STOOL")
pc4 = plot_bci_spec_trend_cta(m3, specimen = "UROGENITAL")

# By country - fitted
pcf1 = plot_bci_spec_trend_cta_fitted(m3, specimen = "BLOOD")
pcf2 = plot_bci_spec_trend_cta_fitted(m3, specimen = "URINE")
pcf3 = plot_bci_spec_trend_cta_fitted(m3, specimen = "STOOL")
pcf4 = plot_bci_spec_trend_cta_fitted(m3, specimen = "UROGENITAL")

combined_plot2 = pc1+pc2+pc3+pc4 + plot_layout(1)
combined_plot3 = pcf1+pcf2+pcf3+pcf4 + plot_layout(1)

ggsave(filename =  paste0(dirOutput, "/Analyses/Section3.3_Surveillance_coverage/Trends/Fig_3.4_bci_overtime_regional_crudedata.png"), 
       plot = combined_plot, 
       device="png",
       width = 18, height = 15)

ggsave(filename = paste0(dirOutput, "/Analyses/Section3.3_Surveillance_coverage/Trends/Fig_3.4_bci_overtime_cta_crudedata.png"), 
       plot = combined_plot2, 
       device="png",
       width = 18, height = 15)

ggsave(filename = paste0(dirOutput, "/Analyses/Section3.3_Surveillance_coverage/Trends/Fig_3.4_bci_overtime_cta_crudedata_linearfitted.png"), 
       plot = combined_plot3, 
       device="png",
       width = 18, height = 15)

ggsave(filename = paste0(dirOutput, "/Analyses/Section3.3_Surveillance_coverage/Trends/Fig_3.4_bci_overtime_global_crudedata.png"), 
       plot = global, 
       device="png",
       width = 18, height = 7)

# The regional and country trends largely suggest that a linear trend is suited for most countries
# Then when fitting the frequentist models, a model including a country specific trend does converge with a centered and scaled trend
# What did converge is region specific trends
f <- glm.nb(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1, 
  data = m3 %>%filter(Specimen=="BLOOD")
)
summary(f)

f1 <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + (1 | Iso3 ), 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD")
)

f2 <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1  + (1 | Iso3 ) + (1 | WHORegionCode), 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD")
)
summary(f2)
tab_model(f1, show.ci = TRUE, show.r2 = TRUE, title = "Model Summary")

tab_model(f2, show.ci = TRUE, show.r2 = TRUE, title = "Model Summary")

AIC(f1,f2) # Additional variance explained by WHORegionCode
anova(f1, f2, test = "LRT")

# We may want to scale year, so fitting is easier
f3a <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + (1 + Year_scaled | Iso3 ) + Year_scaled, 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD")
)

f3 <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + (1 + Year_scaled | Iso3 ) + (1 | WHORegionCode) + Year_scaled, 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD")
)

f4 <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + (1 | Iso3 ) + (1 + Year_scaled | WHORegionCode) + Year_scaled, 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD")
)
summary(f3a)
summary(f3)
summary(f4)
AIC(f3a, f3,f4) # f3 gives best fit

f2b <- glmmTMB(
  SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + (1 | Iso3 ) + (1 | WHORegionCode), 
  family = nbinom2,
  data = m3 %>%filter(Specimen=="BLOOD") 
)
summary(f2b) # Overdispersion for UROGENITAL = 1.8; STOOL = 2.55; URINE = 2.19; BLOOD = 2.37; 

# For the variance around Iso3, this is: BLOOD = var(2.9) sd(1.70); URINE: var(4.56 sd(2.32)); STOOL: var(4.14) sd(2.03); UROGENITAL: var(4.18 sd(2.04))
# For the variance around WHORegionCode, this is: BLOOD = var(1.55) sd(1.23); URINE: var(2.08, sd(1.44)); STOOL: var(1.57, sd(1.25); URO: var(0.74 sd(0.86)))

# The above suggests there is overdispersion, as well as CTA-level variance as well as additional regional variance, however
# model fit without regional variance is better.


###################################################################################
# SPECIFY MODEL
#----------------------------------------------------------------------------------

# Fit with year centered to improve model convergence
model1a_formula <- bf(SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1 + Year_c + (1 + Year_c | Iso3)) 
model2a_formula <- bf(SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1 + Year_c + (1 + Year_c | Iso3) + (1 | WHORegionCode)) 
model3a_formula <- bf(SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1 + Year_c + (1 | Iso3) + (Year_c | WHORegionCode))
model4a_formula <- bf(SpecimenIsolateswithAST ~ offset(log(TotalPopulation)) + 1 + Year_c + (1 | Iso3) + Year_c * WHORegionCode)



# Define weakly informative priors
# Best prior for the shape parameter 
# The neg_binomial_2 distribution in Stan (and BRMS calling stan) is parameterized so that 
# the mean is mu and the variance is mu*(1 + mu/phi). 
# If using the "generic prior for everything" for phi, such as a phi ~ half-N(0,1), 
# then most of the prior mass is on models with a large amount of over-dispersion. 
# This will lead to a prior-data conflict if the data only exhibits a small amount of over-dispersion.
# See here https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations

# I'd expect a larger mean shape estimate around 3 max 10 based on above frequentist models for different specimens, 
# I.e. not a large amount of overdispersion
# summary(f2)

# Define the gamma distribution parameters
# shape <- 2      # Shape parameter of the gamma distribution
# rate <- 0.5     # Rate parameter (inverse of scale)
# 
# # Generate values for the x-axis
# x_values <- seq(0, 50, by = 0.1)  # Adjust range and step for visualization
# 
# # Calculate the density values
# density_values <- dgamma(x_values, shape = shape, rate = rate)
# 
# # Create a data frame for plotting
# prior_data <- data.frame(x = x_values, density = density_values)
# 
# # Plot the prior distribution
# ggplot(prior_data, aes(x = x, y = density)) +
#   geom_line(color = "blue", size = 1.2) +
#   labs(
#     title = "Prior for Shape Parameter (Gamma Distribution)",
#     x = "Shape Parameter (phi)",
#     y = "Density"
#   ) +
#   theme_minimal()
# summary(density_values)

weak_priors <- c(
  prior(normal(0, 2.5), class = "Intercept"), # Weakly informative for variation in intercept
  prior(normal(0, 1), class = "b"),  # Weakly informative prior for fixed effects (Year), assuming a small effect over time
  prior(normal(0, 1), class = "sd", lb=0),  #  half normal for sds, commonly used for random-effects
  prior(gamma(2, 0.5), class = "shape"), # Assuming overdispersion is around 2 to 3 but can be higher 
  prior(lkj(1), class="cor") # weakly informative prior for correlation
)

# weak_priors2 <- c(
#   prior(normal(0, 2.5), class = "Intercept"), # Weakly informative for variation in intercept
#   prior(normal(0, 1), class = "b"),  # Weakly informative prior for fixed effects (Year), assuming a small effect over time
#   prior(normal(0, 2.5), class = "sd", lb=0),  #  half normal for sds, commonly used for random-effects, allow for more variation
#   #prior(inv_gamma(0.4, 0.3), class="shape"), # Recommended as default https://github.com/stan-dev/stan/wiki/prior-choice-recommendations
#   prior(gamma(2, 0.5), class = "shape"), # Assuming overdispersion is around 2 to 3 but can be higher 
#   prior(lkj(1), class="cor") # weakly informative prior for correlation
# )

weak_priors2a <- c(
  prior(normal(0, 2.5), class = "Intercept"), # Weakly informative for variation in intercept
  prior(normal(0, 1), class = "b"),  # Weakly informative prior for fixed effects (Year), assuming a small effect over time
  prior(normal(0, 2.5), class = "sd", lb=0),  #  half normal for sds, commonly used for random-effects, allow for more variation
  #prior(inv_gamma(0.4, 0.3), class="shape") # Recommended as default https://github.com/stan-dev/stan/wiki/prior-choice-recommendations
  prior(gamma(2, 0.5), class = "shape") # Assuming overdispersion is around 2 to 3 but can be higher
)


iter = 4000
warmup = 2000

# Initialize list to store model results
model_results <- list()


# Loop over each drug-bug combination
for (specimen in unique(m3$Specimen)) {
  print(specimen)
  # Filter the dataset for the current drug-bug combination
  data_subset <- m3 %>% filter(Specimen == specimen)
  data_subset$Year_c = scale(data_subset$Year, center=TRUE, scale=FALSE) # Just center 
  
  # Model 1: Random slope for Year at country-level (linear)
  model1 <- brm(
    formula = model1a_formula,
    family = negbinomial(),
    data = data_subset,
    cores = 4,
    chains = 4,
    iter = iter,
    warmup = warmup,
    prior = weak_priors,
    control = list(adapt_delta = 0.95),
    set.seed(123)
  )
  
  # Model 2: Random slope for Year at country-level (linear) + WHORegionCode-level variation
  model2 <- brm(
    formula = model2a_formula,
    family = negbinomial(),
    data = data_subset,
    cores = 4,
    chains = 4,
    iter = iter,
    warmup = warmup,
    prior = weak_priors,
    control = list(adapt_delta = 0.95),
    set.seed(123)
  )
  
  # Model 3: Random slope for Year at country-level and fixed effect for regional level (linear)
  model3 <- brm(
    formula = model3a_formula,
    family = negbinomial(),
    data = data_subset,
    cores = 4,
    chains = 4,
    iter = iter,
    warmup = warmup,
    prior = weak_priors,
    control = list(adapt_delta = 0.95),
    set.seed(123)
  )
  
  # model4 <- brm(
  #   formula = model4_formula,
  #   family = negbinomial(),
  #   data = data_subset,
  #   cores = 4,
  #   chains = 4,
  #   iter = iter,
  #   warmup = warmup,
  #   prior = weak_priors2a,
  #   control = list(adapt_delta = 0.95),
  #   set.seed(123)
  # )
  # Store the models in a results list
  model_results[[specimen]] <- list(model1 = model1, model2 = model2, model3 = model3)
}


# Posterior predictive check
#pp_check(model1, type = "hist")

saveRDS(model_results, paste0(dirOutputModel, "weakip/Specimen/Model_fits_scoverage_specimen.rds"))
model_results = readRDS(paste0(dirOutputModel, "weakip/Specimen/Model_fits_scoverage_specimen.rds"))

# LOO COMPARE
loo_compare(loo(model_results[["BLOOD"]]$model1), loo(model_results[["BLOOD"]]$model2),loo(model_results[["BLOOD"]]$model3))
loo_compare(loo(model_results[["URINE"]]$model1), loo(model_results[["URINE"]]$model2),loo(model_results[["URINE"]]$model3))
loo_compare(loo(model_results[["STOOL"]]$model1), loo(model_results[["STOOL"]]$model2),loo(model_results[["STOOL"]]$model3))
loo_compare(loo(model_results[["UROGENITAL"]]$model1), loo(model_results[["UROGENITAL"]]$model2),loo(model_results[["UROGENITAL"]]$model3))

# ABOVE SHOWS THAT A MODEL WITH RANDOM-INTERCEPT (ISO3) AND YEAR AS RANDOM-EFFECT IS BEST MODEL 
# WITH ADDITIONAL BASELINE VARIATION FOR WHOREGION NOT IMPROVING MODEL FIT

summary(model_results[["BLOOD"]]$model1); n_divergent(model_results[["BLOOD"]]$model1) # 
summary(model_results[["URINE"]]$model1); n_divergent(model_results[["URINE"]]$model1) # 
summary(model_results[["STOOL"]]$model1); n_divergent(model_results[["STOOL"]]$model1) # 
summary(model_results[["UROGENITAL"]]$model1);  n_divergent(model_results[["UROGENITAL"]]$model1) # 


# EXTRACT COUNTRY-LEVEL SLOPES
summarize_regional_and_global_slopes <- function(data, model_results, year_popdata, pdata, cdata, region_var = "WHORegionName", specimen) {
  
  sp = ifelse(specimen=="BSI", "BLOOD",
              ifelse(specimen == "UTI", "URINE",
                     ifelse(specimen == "GI", "STOOL", "UROGENITAL")))
  
  d = data %>% filter(Specimen == sp)
  d$Year_c = scale(d$Year, center=T)
  
  # Extract posterior samples for fixed and random effects
  posterior_samples <- as_draws_df(model_results)
  
  # Fixed effect for Year_c
  fixed_effect_samples <- posterior_samples$b_Year_c
  
  # Random effects for Year_c
  random_effect_columns <- grep("^r_Iso3\\[.*?,Year_c\\]$", colnames(posterior_samples), value = TRUE)
  random_effect_samples <- posterior_samples[, random_effect_columns]
  
  # Combine fixed and random slopes
  posterior_country_slopes <- sweep(as.matrix(random_effect_samples), 1, fixed_effect_samples, FUN = "+")
  colnames(posterior_country_slopes) <- gsub("r_Iso3\\[|,Year_c\\]", "", colnames(posterior_country_slopes))
  
  # Filter population data for specified year
  pdata2022 <- pdata %>%
    filter(Year == year_popdata) %>%
    dplyr::select(Iso3, TotalPopulation) %>%
    left_join(cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  
  # Long format for country-level slopes
  posterior_country_long <- as.data.frame(posterior_country_slopes) %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(cols = -draw, names_to = "Iso3", values_to = "country_slope") %>%
    left_join(pdata2022, by = "Iso3")
  
  # Identify countries with at least 3 years of data
  valid_countries <- d %>%
    group_by(Iso3) %>%
    summarise(n_years = n_distinct(Year)) %>%
    filter(n_years >= 3) %>%
    pull(Iso3)
  
  # Weighted mean slope per region per draw
  posterior_regional_slopes <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw, !!sym(region_var)) %>%
    summarise(
      weighted_mean_slope = sum(country_slope * TotalPopulation, na.rm = TRUE) / sum(TotalPopulation, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarize posterior trends for regions
  regional_trend_summary <- posterior_regional_slopes %>%
    group_by(!!sym(region_var)) %>%
    summarise(
      mean_slope = mean(weighted_mean_slope),
      median_slope = median(weighted_mean_slope),
      Q2.5 = quantile(weighted_mean_slope, 0.025),
      Q10 = quantile(weighted_mean_slope, 0.10),
      Q25 = quantile(weighted_mean_slope, 0.25),
      Q75 = quantile(weighted_mean_slope, 0.75),
      Q90 = quantile(weighted_mean_slope, 0.90),
      Q97.5 = quantile(weighted_mean_slope, 0.975),
      .groups = "drop"
    )
  
  # Global weighted mean slope per draw
  posterior_global_trends <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw) %>%
    summarise(
      global_weighted_mean_slope = sum(country_slope * TotalPopulation, na.rm = TRUE) / sum(TotalPopulation, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarize global trend
  global_trend_summary <- posterior_global_trends %>%
    summarise(
      mean_slope = mean(global_weighted_mean_slope),
      median_slope = median(global_weighted_mean_slope),
      Q2.5 = quantile(global_weighted_mean_slope, 0.025),
      Q10 = quantile(global_weighted_mean_slope, 0.10),
      Q25 = quantile(global_weighted_mean_slope, 0.25),
      Q75 = quantile(global_weighted_mean_slope, 0.75),
      Q90 = quantile(global_weighted_mean_slope, 0.90),
      Q97.5 = quantile(global_weighted_mean_slope, 0.975)
    ) %>%
    mutate(!!region_var := "Global") %>%
    dplyr::select(!!sym(region_var), mean_slope, median_slope, Q2.5, Q10, Q25, Q75, Q90, Q97.5)
  
  # Combine regional and global
  regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)
  regional_trend_summary$specimen <- specimen
  
  return(regional_trend_summary)
}

bsi_slopes =  summarize_regional_and_global_slopes(data=m3,model_results[["BLOOD"]]$model1, year_popdata = 2023, pdata, cdata, specimen="BSI")
uti_slopes =  summarize_regional_and_global_slopes(data=m3,model_results[["URINE"]]$model1, year_popdata = 2023, pdata, cdata,specimen="UTI")
gi_slopes =  summarize_regional_and_global_slopes(data=m3,model_results[["STOOL"]]$model1, year_popdata = 2023, pdata, cdata, specimen="GI")
uro_slopes =  summarize_regional_and_global_slopes(data=m3,model_results[["UROGENITAL"]]$model1, year_popdata = 2023, pdata, cdata, specimen="Gonorrhoea")


# Combine and format
slopes_all <- bind_rows(bsi_slopes, uti_slopes, gi_slopes, uro_slopes) %>%
  mutate(
    WHORegionName = factor(
      WHORegionName,
      levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                 "European Region", "Eastern Mediterranean Region", "Western Pacific Region")
    ),
    mean_slope_p = (exp(mean_slope) - 1) * 100,
    median_slope_p = (exp(median_slope) - 1) * 100,
    Q2.5_p = (exp(Q2.5) - 1) * 100,
    Q10_p = (exp(Q10) - 1) * 100,
    Q25_p = (exp(Q25) - 1) * 100,
    Q75_p = (exp(Q75) - 1) * 100,
    Q90_p = (exp(Q90) - 1) * 100,
    Q97.5_p = (exp(Q97.5) - 1) * 100,
    specimen = factor(
      specimen,
      levels = c("BSI", "UTI", "GI", "Gonorrhoea"),
      labels = c("Bloodstream", "Urinary tract", "Gastrointestinal", "Gonorrhoea")
    )
  )

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
                                  ifelse(ncountry$Specimen == "STOOL", "Gastrointestinal", "Gonorrhoea"))
                           )
#slopes_all$combined = paste0(slopes_all$Specimen, "-", slopes_all$PathogenName, "-", slopes_all$AntibioticName)


slopes_all = left_join(slopes_all, ncountry)

slopes_all$Significant = ifelse(slopes_all$Q2.5_p>=1&slopes_all$n>5|slopes_all$Q97.5_p<=-1&slopes_all$n>5, "Yes", "No")


#df_plot <- slopes_all %>%
#  mutate(variable = fct_reorder(WHORegionName, mean_slope_p))

slopes_all$label_95CrI <- sprintf("%.1f (%.1f–%.1f)", 
                                  slopes_all$mean_slope_p,
                                  slopes_all$Q2.5_p, 
                                  slopes_all$Q97.5_p)
unique(slopes_all$WHORegionName)

slopes_all = slopes_all %>%
  mutate(
    WHORegionName = factor(
      WHORegionName,
      levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                 "European Region", "Eastern Mediterranean Region", "Western Pacific Region")),
    specimen = factor(
      specimen,
      levels = c("Bloodstream", "Urinary tract", "Gastrointestinal", "Gonorrhoea"),
      labels = c("Bloodstream", "Urinary tract", "Gastrointestinal", "Gonorrhoea")
    )
    )

unique(slopes_all$WHORegionName)

p = ggplot(slopes_all, aes(y = WHORegionName)) +
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
  geom_rect(aes(xmin = Q25_p, xmax = Q75_p,
                ymin = as.numeric(WHORegionName) - 0.1,
                ymax = as.numeric(WHORegionName) + 0.1,
                fill = WHORegionName),
            alpha = 0.8) +
  # Posterior mean point
  geom_point(aes(x = median_slope_p), size = 3) +
  # Vertical zero line
  facet_wrap(~ specimen, scales = "free_y") +
  scale_fill_manual(values = facet_colors3) +
  scale_color_manual(values = facet_colors3) +
  labs(
    title = "",
    subtitle = "",
    x = "Weighted annual % change in reported BCI",
    y = "WHO Region"
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
    specimen == "Gastrointestinal" ~ scale_y_discrete(),
    TRUE ~ scale_y_discrete(labels = NULL) # Hide labels for other facets
  )
)
p

# FIGURE 3.4
ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.4_slopechange_regional.svg"), 
       plot = p,
       device = svg,
       dpi = 300,
       bg = "white",
       width = 11, height = 8)  

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/Final/Figure_3.4_slopechange_regional.png"), 
       plot = p,
       device = png,
       dpi = 300,
       bg = "white",
       width = 15, height = 9)  


# For writing
#write.csv(slopes_all, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_slopechange_regional.csv"))

slopes_all_p = slopes_all %>%
  dplyr::select(WHORegionName, n,specimen, mean_slope_p, median_slope_p, Q2.5_p, Q10_p, Q25_p, Q75_p, Q90_p, Q97.5_p, label_95CrI, Significant)

slopes_all_p = slopes_all_p %>%
  rename(
    nCTA_3ydata = "n"
  )
addWorksheet(wb, "Estimated_slope_change_spec")
#addWorksheet(wb, "Sheet3")
#addWorksheet(wb, "Sheet4")

# Write data to each sheet
writeData(wb, sheet = "Estimated_slope_change_spec", x = slopes_all_p)

saveWorkbook(wb, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), overwrite = TRUE)

# ESTIMATE EXPECTED REGIONAL TRENDS
#------------------------------------------------------------------------------------

# ALTERNATIVE WITH Epred function
generate_predictions_summary <- function(data, fit, year_range, pdata, cdata, specimen, specimen_filter, pathogen=T) {
  
  # Filter the dataset for the current drug-bug combination (only when generating predictions for pathogen specific data)
  if(pathogen){
    d <- data %>% filter(combined == drug_bug)
  }else{
    d = data %>% filter(Specimen == specimen_filter)
  }
  
  d$Year_c = scale(d$Year, center=T)
  
  year_year_c = d %>% dplyr::select(Year, Year_c) %>%
    distinct()
  
  # Filter population data for the specified year
  pdata2023 <- pdata %>%
    filter(Year == year_range[2]) %>%
    dplyr::select(Iso3, TotalPopulation)
  
  # Join with region data
  pdata2023 <- left_join(pdata2023, cdata %>% dplyr::select(WHORegionName, WHORegionCode, Iso3), by = "Iso3")
  
  newdata <- expand.grid(
    Iso3 = unique(d$Iso3),
    Year = seq(year_range[1], year_range[2])
  ) %>%
    left_join(pdata %>%dplyr::select(c(Iso3,Year,TotalPopulation)), by = c("Year", "Iso3"))
  
  newdata = left_join(newdata, year_year_c)
  
  # Make sure all needed columns are present
  pred_matrix <- posterior_epred(fit, newdata = newdata)
  
  # pred_matrix: rows = posterior draws, columns = rows of newdata
  # Convert to long format
  posterior_country_long <- pred_matrix %>%
    as.data.frame() %>%
    mutate(draw = 1:nrow(.)) %>%
    pivot_longer(-draw, names_to = "rowid", values_to = "prediction") %>%
    mutate(rowid = as.integer(gsub("V", "", rowid))) %>%
    left_join(newdata %>% mutate(rowid = row_number()), by = "rowid") %>%
    mutate(prediction = prediction/TotalPopulation*1e6)
  
  posterior_country_long = left_join(posterior_country_long,cdata %>% dplyr::select(Iso3, WHORegionName))
  
  # Identify countries with at least 3 years of data
  valid_countries <- posterior_country_long %>%
    group_by(Iso3) %>%
    summarise(n_years = n_distinct(Year)) %>%
    filter(n_years >= 3) %>%
    pull(Iso3)
  
  # Calculate regional predictions - weighted 
  posterior_regional_predictions <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw, WHORegionName, Year) %>%
    summarise(
      weighted_prediction = sum(prediction * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )
  
  # Calculate regional predictions - crude median prediction by region and year
  posterior_regional_median_prediction <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw, WHORegionName, Year) %>%
    summarise(
      crude_prediction = median(prediction),
      .groups = "drop"
    )
  
  # Combine weighted mean and median predictions
  posterior_regional_predictions <- posterior_regional_predictions %>%
    left_join(posterior_regional_median_prediction, by = c("draw", "WHORegionName", "Year"))
  
  
  # Calculate global weighted predictions
  posterior_global_predictions <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw, Year) %>%
    summarise(
      weighted_prediction = sum(prediction * TotalPopulation) / sum(TotalPopulation),
      .groups = "drop"
    )%>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(c(draw, WHORegionName,Year, weighted_prediction))
  
  
  # Global crude median
  posterior_global_median_prediction <- posterior_country_long %>%
    filter(Iso3 %in% valid_countries) %>%
    group_by(draw, Year) %>%
    summarise(
      crude_prediction = median(prediction),
      .groups = "drop"
    ) %>%
    mutate(WHORegionName = "Global") %>%
    dplyr::select(c(draw, WHORegionName,Year, crude_prediction))
  
  # Combine global mean and median
  posterior_global_predictions <- posterior_global_predictions %>%
    left_join(posterior_global_median_prediction, by = c("draw","WHORegionName", "Year"))
  
  # Summarize posterior distributions for regions
  regional_trend_summary <- posterior_regional_predictions %>%
    group_by(WHORegionName, Year) %>%
    summarise(
      mean_weighted_prediction = mean(weighted_prediction),
      median_weighted_prediction = median(weighted_prediction),
      Q2.5_weighted_prediction = quantile(weighted_prediction, 0.025),
      Q97.5_weighted_prediction = quantile(weighted_prediction, 0.975),
      mean_crude_prediction = mean(crude_prediction),
      median_crude_prediction = median(crude_prediction),
      Q2.5_crude_prediction = quantile(crude_prediction, 0.025),
      Q97.5_crude_prediction = quantile(crude_prediction, 0.975),
      .groups = "drop"
    )
  
  # Create a wide version to compute 2023 - 2016 differences per draw
  regional_prediction_diff <- posterior_regional_predictions %>%
    filter(Year %in% c(2016, 2023)) %>%
    pivot_wider(
      names_from = Year,
      values_from = c(weighted_prediction, crude_prediction),
      names_glue = "{.value}_{Year}"
    ) %>%
    mutate(
      # Absolute differences
      diff_weighted = weighted_prediction_2023 - weighted_prediction_2016,
      diff_crude = crude_prediction_2023 - crude_prediction_2016,
      
      # Percentage changes
      pct_change_weighted = 100 * diff_weighted / weighted_prediction_2016,
      pct_change_crude = 100 * diff_crude / crude_prediction_2016
    )
  
  # Summarize across draws for each region
  regional_prediction_diff_summary <- regional_prediction_diff %>%
    group_by(WHORegionName) %>%
    summarise(
      # Absolute differences
      diff_weighted_median = quantile(diff_weighted, 0.5),
      diff_weighted_Q2.5 = quantile(diff_weighted, 0.025),
      diff_weighted_Q97.5 = quantile(diff_weighted, 0.975),
      
      diff_crude_median = quantile(diff_crude, 0.5),
      diff_crude_Q2.5 = quantile(diff_crude, 0.025),
      diff_crude_Q97.5 = quantile(diff_crude, 0.975),
      
      # Percent changes
      pct_change_weighted_median = quantile(pct_change_weighted,0.5),
      pct_change_weighted_Q2.5 = quantile(pct_change_weighted, 0.025),
      pct_change_weighted_Q97.5 = quantile(pct_change_weighted, 0.975),
      
      pct_change_crude_median = quantile(pct_change_crude, 0.5),
      pct_change_crude_Q2.5 = quantile(pct_change_crude, 0.025),
      pct_change_crude_Q97.5 = quantile(pct_change_crude, 0.975),
      
      .groups = "drop"
    )
  
  regional_trend_summary <- regional_trend_summary %>%
    left_join(regional_prediction_diff_summary, by = "WHORegionName")
  
  # Summarize posterior distributions globally
  global_trend_summary <- posterior_global_predictions %>%
    group_by(WHORegionName, Year) %>%
    summarise(
      mean_weighted_prediction = mean(weighted_prediction),
      median_weighted_prediction = median(weighted_prediction),
      Q2.5_weighted_prediction = quantile(weighted_prediction, 0.025),
      Q97.5_weighted_prediction = quantile(weighted_prediction, 0.975),
      mean_crude_prediction = mean(crude_prediction),
      median_crude_prediction = median(crude_prediction),
      Q2.5_crude_prediction = quantile(crude_prediction, 0.025),
      Q97.5_crude_prediction = quantile(crude_prediction, 0.975),
      .groups = "drop"
    ) %>%
    dplyr::select(WHORegionName, Year, mean_weighted_prediction, median_weighted_prediction,Q2.5_weighted_prediction, 
                  Q97.5_weighted_prediction,mean_crude_prediction,median_crude_prediction, Q2.5_crude_prediction,Q97.5_crude_prediction)
  
  
  # Create a wide version to compute 2023 - 2016 differences per draw
  global_prediction_diff <- posterior_global_predictions %>%
    filter(Year %in% c(2016, 2023)) %>%
    pivot_wider(
      names_from = Year,
      values_from = c(weighted_prediction, crude_prediction),
      names_glue = "{.value}_{Year}"
    ) %>%
    mutate(
      # Absolute differences
      diff_weighted = weighted_prediction_2023 - weighted_prediction_2016,
      diff_crude = crude_prediction_2023 - crude_prediction_2016,
      
      # Percentage changes
      pct_change_weighted = 100 * diff_weighted / weighted_prediction_2016,
      pct_change_crude = 100 * diff_crude / crude_prediction_2016
    )
  
  # Summarize across draws for each region
  global_prediction_diff_summary <- global_prediction_diff %>%
    summarise(
      # Absolute differences
      diff_weighted_median = quantile(diff_weighted, 0.5),
      diff_weighted_Q2.5 = quantile(diff_weighted, 0.025),
      diff_weighted_Q97.5 = quantile(diff_weighted, 0.975),
      
      diff_crude_median = quantile(diff_crude, 0.5),
      diff_crude_Q2.5 = quantile(diff_crude, 0.025),
      diff_crude_Q97.5 = quantile(diff_crude, 0.975),
      
      # Percent changes
      pct_change_weighted_median = quantile(pct_change_weighted,0.5),
      pct_change_weighted_Q2.5 = quantile(pct_change_weighted, 0.025),
      pct_change_weighted_Q97.5 = quantile(pct_change_weighted, 0.975),
      
      pct_change_crude_median = quantile(pct_change_crude, 0.5),
      pct_change_crude_Q2.5 = quantile(pct_change_crude, 0.025),
      pct_change_crude_Q97.5 = quantile(pct_change_crude, 0.975),
      
      .groups = "drop"
    ) %>% 
    mutate(WHORegionName="Global")
  
  global_trend_summary <- global_trend_summary %>%
    left_join(global_prediction_diff_summary, by = "WHORegionName")
  
  # Combine regional and global summaries
  regional_trend_summary <- bind_rows(regional_trend_summary, global_trend_summary)
  
  # Ensure consistent factor levels for WHORegionName
  regional_trend_summary <- regional_trend_summary %>%
    mutate(
      WHORegionName = factor(WHORegionName, 
                             levels = c("Global", 
                                        "African Region",
                                        "Region of the Americas",
                                        "South-East Asia Region",
                                        "European Region",
                                        "Eastern Mediterranean Region",
                                        "Western Pacific Region"))
    )
  regional_trend_summary$specimen = specimen
  
  return(regional_trend_summary)
}



# GENERATE PREDICTIONS
bsi_pred =  generate_predictions_summary(data = m3, fit = model_results[["BLOOD"]]$model1, year_range = c(2016,2023), pdata, cdata, specimen="BSI", specimen_filter = "BLOOD", pathogen=F)  
uti_pred =  generate_predictions_summary(data = m3, fit = model_results[["URINE"]]$model1, year_range = c(2016,2023), pdata, cdata,specimen="UTI",specimen_filter = "URINE", pathogen=F)
gi_pred =  generate_predictions_summary(data = m3, fit = model_results[["STOOL"]]$model1, year_range = c(2016,2023), pdata, cdata, specimen="GI", specimen_filter = "STOOL", pathogen=F)
uro_pred =  generate_predictions_summary(data = m3, fit = model_results[["UROGENITAL"]]$model1, year_range = c(2016,2023), pdata, cdata, specimen="Gonorrhoea", specimen_filter = "UROGENITAL", pathogen=F)


# VISUALISE PREDICTED TREND AGAINST DATA
m3 = m3 %>%
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

m4 = rbind(m3,m3_g)

m4_filtered <- m4 %>%
  group_by(Iso3) %>%  # Group by country or unique identifier
  filter(n_distinct(Year[!is.na(SpecimenIsolateswithAST)]) >= 3) %>%  # Filter for at least 3 years of data
  ungroup()

pred = rbind(bsi_pred, uti_pred, gi_pred, uro_pred)

pred = pred %>%
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
      "Global",
      "African Region" = "African\nRegion",
      "Region of the Americas" = "Region of the \nAmericas",
      "South-East Asia Region" = "South-East\nAsia Region",
      "European Region" = "European\nRegion",
      "Eastern Mediterranean Region" = "Eastern\nMediterranean Region",
      "Western Pacific Region" = "Western\nPacific Region")
    )
  )

# Visualize Regional Trends
p = ggplot(pred %>%filter(specimen=="BSI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  geom_jitter(
    data = m4_filtered %>% filter(Specimen == "BLOOD"),  # Add data directly to geom_jitter
    aes(x = Year, y = BCI_permillion, size = BCI_permillion),  # Ensure y is mapped correctly
    width = 0.2, height = 0.2, alpha = 0.5, colour="lightblue"
  ) +
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Bloodstream infections",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p

p2 = ggplot(pred %>%filter(specimen=="UTI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  # geom_jitter(
  #   data = m4_filtered %>% filter(Specimen == "URINE"),  # Add data directly to geom_jitter
  #   aes(x = Year, y = BCI_permillion, size = BCI_permillion),  # Ensure y is mapped correctly
  #   width = 0.2, height = 0.2, alpha = 0.5, colour="lightblue"
  # ) +
   scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Urinary tract",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p2

p3 = ggplot(pred %>%filter(specimen=="GI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  geom_jitter(
    data = m4_filtered %>% filter(Specimen == "STOOL"),  # Add data directly to geom_jitter
    aes(x = Year, y = BCI_permillion, size = BCI_permillion),  # Ensure y is mapped correctly
    width = 0.2, height = 0.2, alpha = 0.5, colour="lightblue"
  ) +
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Gastrointestinal",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p3

p4 = ggplot(pred %>%filter(specimen=="Gonorrhoea"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  geom_jitter(
    data = m4_filtered %>% filter(Specimen == "UROGENITAL"),  # Add data directly to geom_jitter
    aes(x = Year, y = BCI_permillion, size = BCI_permillion),  # Ensure y is mapped correctly
    width = 0.2, height = 0.2, alpha = 0.5, colour="lightblue"
  ) +
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Gonorrhoea",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p4

combined_plot = p+p2+p3+p4 + plot_layout(1)

p5 = ggplot(pred %>%filter(specimen=="BSI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Bloodstream infections",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p5

p6 = ggplot(pred %>%filter(specimen=="UTI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Urinary tract",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p6

p7 = ggplot(pred %>%filter(specimen=="GI"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Gastrointestinal",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p7

p8 = ggplot(pred %>%filter(specimen=="Gonorrhoea"), aes(x = Year, y = median_weighted_prediction, color = region_labels)) +
  geom_line(size = 1) +  # Lines for median predictions
  scale_colour_manual(values = facet_colors3) +  # Manually set fill colors if necessary
  facet_wrap(~region_labels, ncol=7, scale= "free_y") +  # Facet by WHORegionName
  geom_ribbon(
    aes(ymin = Q2.5_weighted_prediction, ymax = Q97.5_weighted_prediction, fill =region_labels),
    alpha = 0.2
  ) +
  geom_line(aes(x = Year, y = median_crude_prediction), size = 1, lty=2) +  # Lines for median predictions
  geom_ribbon(
    aes(ymin = Q2.5_crude_prediction, ymax = Q97.5_crude_prediction), lty=2,
    alpha = 0.2
  ) +
  labs(
    title = "Regional Trends Over Time",
    subtitle = "Gonorrhoea",
    y = "Predicted Counts",
    x = "Year",
    color = "Region",
    fill = "Region",
    size = "BCI per Million"
  ) +
  theme_minimal() +
  guides(
    color = "none",  # Remove color legend
    fill = "none",   # Remove fill legend
    size = "none"    # Remove size legend
  )

p8

combined_plot2 = p5+p6+p7+p8 + plot_layout(1)

ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/For_writing/Figure_3.4_slopechange_regional.png"), 
       plot = combined_plot,
       device = png,
       dpi = 300,
       bg = "white",
       width = 15, height = 15)  

# ggsave(filename = paste0(dirOutputReport, "/Chapter 3/Ch3 Figures/For_writing/Figure_3.4_slopechange_regional_withoutdata.png"), 
#        plot = combined_plot2,
#        device = png,
#        dpi = 300,
#        bg = "white",
#        width = 15, height = 15)  

# Write data to existing excel sheet
pred_abs = pred %>% dplyr::select("WHORegionName","Year","specimen", "mean_weighted_prediction", "median_weighted_prediction",
                                  "Q2.5_weighted_prediction","Q97.5_weighted_prediction","mean_crude_prediction", "median_crude_prediction",
                                  "Q2.5_crude_prediction","Q97.5_crude_prediction")


addWorksheet(wb, "Estimated_absolute_trends")
#addWorksheet(wb, "Estimated_diff_2016_vs_2023")

writeData(wb, sheet = "Estimated_absolute_trends", x = pred_abs)
#writeData(wb, sheet = "Estimated_diff_2016_vs_2023", x = pred_diff)

saveWorkbook(wb, file = paste0(dirOutputReport, "/Chapter 3/Ch3 summary stats/Figure_3.4_BCI_perMillion_crude_and_trends.xlsx"), overwrite = TRUE)

