# GLASS-2025
Code supporting the statistical analyses for the WHO Global Antimicrobial Resistance and Use Surveillance System (GLASS) 2025 report.

## References
- WHO GLASS 2025 report - Summary:
  https://www.who.int/publications/i/item/B09585

- WHO GLASS 2025 report, including methodological description in Annex 1:
  Statistical methods and modelling framework: https://www.who.int/publications/i/item/9789240116337/)
  
- WHO GLASS initiative:  
  https://www.who.int/initiatives/glass

## 2025 GLASS report - Summary 
The Global Antimicrobial Resistance and Use Surveillance System (GLASS) 2025 report provides standardized, model-based estimates of antimicrobial resistance across countries and regions. 

Drawing on over 23 million infections reported by 104 countries, the report estimates resistance to 22 antibiotics in eight priority bacterial pathogens across four major infection types. 

By adjusting for population structure and surveillance coverage, these analyses attempt to offer a more robust global picture of AMR trends (Chapter 3). 

The report also introduces a framework to assess the maturity and completeness of national surveillance systems, supporting global efforts to strengthen AMR monitoring and response (Chapter 2).

## Repository workflow

The repository follows the analytical workflow used in the report.

| Step | File | Description |
|---|---|---|
| 1 | `1_Data_wrangling.R` | Data cleaning, harmonisation, and preparation |
| 2 | `2_Age_standardised_testingrates.R` | Calculation of age-standardised testing rates |
| 3 | `3_Models_estimate_AMR_prevalence.R` | Bayesian models underlying estimation of national and regional percentage resistance by infection type, pathogen and antibiotic combinations (Annex 1, p73, i.e. underlying models for Sections 3.1 - 3.5) |
| 3 | `3_Models_estimate_BCI_per_million.R` | Bayesian modelling of surveillance coverage over in 2023 and over time, includes model comparison (Annex 1, p72, i.e. underlying models for Sections 2.2.2) |
| 3 | `3_missing_data_table.R` | Missing data summaries and diagnostics (Section 2.2.3) |
| 4 | `4_Identify_bestmodel.R` | Selection of best-performing statistical models for AMR estimates (Annex 1, p73)|
| 4 | `4_Model_comparison_modelsseperate.R` | Model comparison for AMR estimates for drug-bug-infection type combinations seperately (Annex 1, p74) |
| 5 | `5_Calculate_modelbased_AMR_rates_all.R` | Estimation of national and regional percentage resistance by infection type, pathogen and antibiotic combinations in 2023 (Section 3.1 - 3.4)  |
| 5 | `5_Calculate_modelbased_AMR_trends.R` | Estimation of national and regional percentage resistance by infection type, pathogen and antibiotic combinations over time (Section 3.5) |




