# GLASS-2025
Code supporting the statistical analyses for the WHO Global Antimicrobial Resistance and Use Surveillance System (GLASS) 2025 report.
Repository: https://github.com/GlobalAMR/GLASS-2025

## 2025 GLASS report - Summary 
The Global Antimicrobial Resistance and Use Surveillance System (GLASS) 2025 report provides standardized, model-based estimates of antimicrobial resistance across countries and regions. Drawing on over 23 million infections reported by 104 countries, the report estimates resistance to 22 antibiotics in eight priority bacterial pathogens across four major infection types. By adjusting for population structure and surveillance coverage, these analyses offer a robust global picture of AMR trends. The report also introduces a framework to assess the maturity and completeness of national surveillance systems, supporting global efforts to strengthen AMR monitoring and response.

## Repository workflow

The repository follows the analytical workflow used in the report.

| Step | File | Description |
|---|---|---|
| 1 | `1_Data_wrangling.R` | Data cleaning, harmonisation, and preparation |
| 2 | `2_Age_standardised_testingrates.R` | Calculation of age-standardised testing rates |
| 3 | `3_Models_estimate_AMR_prevalence.R` | Bayesian modelling of AMR prevalence (Annex 1) |
| 3 | `3_Models_estimate_BCI_per_million.R` | Modelling surveillance coverage (Section 2.2.2) |
| 3 | `3_missing_data_table.R` | Missing data summaries and diagnostics (Section 2.2.3) |
| 4 | `4_Identify_bestmodel.R` | Selection of best-performing statistical models (Annex 1)|
| 4 | `4_Model_comparison_modelsseperate.R` | Model comparison analyses (Annex 1) |
| 5 | `5_Calculate_modelbased_AMR_rates_all.R` | Generation of final model-based AMR estimates (Section 3.1 - 3.4)  |
| 5 | `5_Calculate_modelbased_AMR_trends.R` | Estimation of AMR temporal trends (Section 3.5) |

## References

- WHO GLASS 2025 report:
  https://www.who.int/publications/i/item/B09585

- Annex 1:
  Statistical methods and modelling framework: https://www.who.int/publications/i/item/9789240116337/)
  
- WHO GLASS initiative:  
  https://www.who.int/initiatives/glass


