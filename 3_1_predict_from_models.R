# This scripts runs creates model predictions for the hiearchial models
# Predictions are for the future, for both plain and age models

library(furrr)
library(future)
library(future.apply)
future::plan(multisession)  # Use multi-core processing

##### CREATE PREDICTIONS #####

specific_models <- c(15,40)

# for each bug_drug combo
for(run_model in c(1:length(plain_model_names))
){
  #just specifics
  #for(run_model in specific_models){
  
  for(ivt in 1:nrow(interventions)){
    
    name_run <- interventions[ivt,"intervention_name"]
    
    #check that the model names for age and plain are the same
    if(str_remove(plain_model_names[run_model], pattern ="plain_") != 
       str_remove(age_model_names[run_model], pattern ="extra_age_")){
      stop("NON MATCHING MODELS!")
    }
    print(paste0("starting model ", run_model, " for intervention ", name_run ))
    # Subset the relevant pathogen.
    if(length(grep(plain_model_names[run_model], pattern = "esccol"))==1){
      prediction_data <-d_modelling[pathogen == "Escherichia coli" & variable == as.character(name_run)] 
      
    } else if(length(grep(plain_model_names[run_model], pattern = "acispp"))==1){
      prediction_data <-d_modelling[pathogen == "Acinetobacter spp"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "staaur"))==1){
      prediction_data <-d_modelling[pathogen == "Staphylococcus aureus"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "strpne"))==1){
      prediction_data <-d_modelling[pathogen == "Streptococcus pneumoniae"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "encfai"))==1){
      prediction_data <-d_modelling[pathogen == "Enterococcus faecalis"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "encfae"))==1){
      prediction_data <-d_modelling[pathogen == "Enterococcus faecium"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "klepne"))==1){
      prediction_data <-d_modelling[pathogen == "Klebsiella pneumoniae"& variable == as.character(name_run)]
      
    } else if(length(grep(plain_model_names[run_model], pattern = "pseaer"))==1){
      prediction_data <-d_modelling[pathogen == "Pseudomonas aeruginosa"& variable == as.character(name_run)]
    }
    
    # Incidence needs to be non-negative integers - so ceiling
    prediction_data[, total := ceiling(value)] 
    prediction_data <- prediction_data[ projection %in% c("BSL", "LMRT")]
    
    ###### run future plain
    temp_model <- readRDS(plain_models_path[run_model])
    # cut into chunks so memory never too big
    chunk_size <- 10000
    num_chunks <- ceiling(nrow(prediction_data) / chunk_size)
    # Before passing the data to future_lapply, make a copy of the data.table
    prediction_data_copy <- copy(prediction_data)
    # run the cprediction on the chunks.
    predictions_list <- future_lapply(1:num_chunks, function(i) {
      chunk <- prediction_data[((i - 1) * chunk_size + 1):(i * chunk_size), ]
      predict(temp_model, newdata = na.omit(chunk), allow_new_levels = TRUE, 
              summary = F, ndraw = 2000) # need to na.omit as chunking adds a load of NA rows on the end which break the model
    }, future.seed=TRUE)
    
    # this outputs: columns match rows from prediction data, with 4000 samples in the rows
    # Combine the results
    predictions_list <- lapply(predictions_list, data.frame)
    predict_future_plain <- do.call(cbind, predictions_list)
    #flip
    predict_future_plain <- data.frame(t(predict_future_plain))
    
    ####### run future age
    temp_model <- readRDS(age_models_path[run_model])
    
    # run the prediction on the chunks.
    predictions_list <- future_lapply(1:num_chunks, function(i) {
      chunk <- prediction_data[((i - 1) * chunk_size + 1):(i * chunk_size), ]
      predict(temp_model, newdata = na.omit(chunk), allow_new_levels = TRUE, 
              summary = F, ndraws = 2000)
    },future.seed=TRUE)
    # this outputs: columns match rows from prediction data, with 4000 samples in the rows
    # Combine the results
    predictions_list <- lapply(predictions_list, data.frame)
    predict_future_age <- do.call(cbind, predictions_list)
    #flip
    predict_future_age <- data.frame(t(predict_future_age))
    
    #Want to save each one of these done as it takes ages
    saveRDS(list(prediction_data, predict_future_plain, predict_future_age),
            file = paste0("predictions/individual_predictions",name_run,"_",plain_model_names[[run_model]], ".RDS"))
    
    print(paste0("Model Complete: ", run_model))
  }
}








