### Future_burden_AMR
Calculates future burden projections of AMR based on changing demography and varying incidence and resistance prevalence levels by age and gender. 

Requires data from ECDC TESSY to run - please apply for access.

## Scripts

All the analysis can be run from MASTER_SCRIPT.R. From the master script always run the interventions and 0_Setup.R first
To run any later sections the 2_ should also be run. 
Sections 1 and 3 only need to be run the first time, and take a significant amount of time to be run.  

- 0_Setup.R: This script loads required packages and the ECDC TESSY data and formats/filters for use in this analysis
  
- 1_0_run_models.R: runs the bayesian multilevel models for each bug-drug combination. Note this takes a LONG time (days) and only needs to be run once. 
- 2_0_eu_pop_size_changes.R: loads and formats the demographic changes up to 2050
- 2_1_calculate_incidence.R: applies the demographic changes to the incidence rates
- 2_2_reaching_targets.R: calculates the incidence rates for the different intervention scenarios

- 3_1_predict_from_models.R: runs the predictions. Takes some time (hours). The models need to have been saved for this to run, see 1_0_run_models.R

- 4_0_model_validation.R: creates the validation plots to compare 2023 projections to 2023 ECDC data
- 4_1_compare_outputs_samples.R: creates plots and analysis for resistant BSI projections and interventions
- 4_2_age_vs_plain.R: runs the analysis to compare including or not the age/sex specific resistance rates

Data files that need to be get read in: 
 - data/data_cleaned_fortrends.csv: This data is from the ECDC - please apply to TESSY for accesss. It is NOT included in this repo.
 - data/proj_23np.csv and data/proj_19np.csv: demographic projections from EU countries available at https://ec.europa.eu/eurostat/web/population-demography/population-projections/database. These are included in the repo.
 - data/uk_ons_principal.csv and data/uk_ons_oldage.csv: The UK demographic projections from ONS. These are included in the repo.
 - data/country_pathogen_age_incidence_estimates.csv: contains incidence calculations from a previous PLOSmed paper: https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1004301. Included in repo.
 - data/ECDC_Atlas_data_2023.cs:. This is downloaded from https://atlas.ecdc.europa.eu/public/index.aspx/. Included in repo.
 - data/coverage_2023.csv: coverage data extracted from the ECDC report available https://www.ecdc.europa.eu/sites/default/files/documents/antimicrobial-resistance-annual-epidemiological-report-EARS-Net-2023.pdf. This is included in the repo.
 - data/est_pop_cov_final.csv: Coverage for ECDC from previous PLOSmed paper. https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1004301. Included in repo.
 - translate_bugs_drugs.csv: dictionary for bug and drug names
