# load in modesl


load_them_all <- F
###### CHECK MODELS #####
# find all the  age file names of models
all_model_files <- list.files(path = "brms_fits/")
age_model_names <- all_model_files[grepl("extra_age_", all_model_files, fixed = TRUE)]
age_models_path <-  paste0("brms_fits/", age_model_names)
# make a list with all the model
if(load_them_all == T){
  age_models <- lapply(age_models_path, readRDS)
}
# use this to check for any issues with models
# xcv <- 11
# age_models[[xcv]]
# age_model_names[[xcv]]
# model 11 has sd(age at 1.02)

# Also check non-age models
# find all the  age file names of models
plain_model_names <- all_model_files[grepl("plain_", all_model_files, fixed = TRUE)]
plain_models_path <-  paste0("brms_fits/", plain_model_names)
# make a list with all the models
if(load_them_all == T){
  plain_models <- lapply(plain_models_path, readRDS)
}
# use this to check for any issues with models
# xcv <- 14
# plain_models[[xcv]]
# plain_model_names[[xcv]]

# All now good.

