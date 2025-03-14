########################################################## 
####### MASTER SCRIPT  ###################################
####### Developers: N Waterlow & G Knight ################ 
##########################################################

# ALWAYS RUN interventions and Setup.R first
# And must also run 2_ section for any of the later numbers
# Sections 1 and 3 only need to be run the first time though. 

# Specify your interventions
interventions <- data.frame(
  intervention_name = c("fixed","varying","minus1", "minus5", "minus5_over65", "minus20"),
  incidence_change = c(0,0,-1,-5,-5,-20), 
  intervention_start = c(2019, 2019,2027,2027,2027,2027), 
  age_targetting = c(0,0,0,0,65,0) #targets anyone this age or older
)

# load packages and data and generally setup 
source("0_Setup.R")

#### RUN THE MODELS ####
# NOTE THIS TAKES DAYS
# only needs to be ran once
source("1_0_run_models.R")

#### CALCULATE THE INCIDENCES ####
# population per year estimates
source("2_0_eu_pop_size_changes.R")
# calculate changing incidences
source("2_1_calculate_incidence.R")
#implement the interventions
source("2_2_reaching_targets.R")

#### PREDICT FROM MODELS ####
source("3_0_load_models.R")

#Takes a few hours - only needs to be ran once unless you change the interventions
# But takes over your computer - as it runs in parallel
# you can specify at the top whether to run all models (e.g. for final) or just a couple (e.g. for testing)
# creates the model predictions
source("3_1_predict_from_models.R")

#### ANALYSE OUTPUT ####
#note some of these require 3_0_load_models.R to be run, but if so it's sourced within the script

# validate the model against 2023 data
source("4_0_model_validation.R")

# analyse all the model predictions
#specify whether to run for earlier figures, or whether to run for intervention analysis
#### NOTE: around line 120 there is an option to save / read in an already saved dataframe. 
# currently only reads in, doesn't save. So this needs to be altered to save a new version!
source("4_1_compare_outputs_samples.R")

# look at the difference between using age and plain models
source("4_2_age_vs_plain.R")





