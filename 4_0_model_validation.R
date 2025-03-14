################################################################
####### Compare model burden against ECDC data #################
####### Developers: N Waterlow & G Knight ###################### 
################################################################

# This script validates model predictions against ECDC data (2023) to compare predicted 
# and actual incidence rates of antimicrobial resistance (AMR) for different 
# pathogens and drugs. The validation process includes loading coverage and 
# incidence data, comparing model predictions to real-world data, and calculating 
# log errors and percent differences. The results are visualized using plots, 
# and the script also checks for consistency across different bugs, countries, 
# and model types. The comparison results are saved for further analysis.

# All of this is assuming no policy changes or 'shocks': a projection not a forecast

source("3_0_load_models.R")

#First need data to compare to 
# ECDC ATLAS All amr
validation_data <- data.table(read.csv("data/ECDC_Atlas_data_2023.csv"))
# load in 2023 coverage data extracted from ECDC report
# https://www.ecdc.europa.eu/sites/default/files/documents/antimicrobial-resistance-annual-epidemiological-report-EARS-Net-2023.pdf
coverage_23 <- data.table(read.csv("data/coverage_2023.csv"))

#for storing the comparison data
store_comparison <- data.frame()
# NOTE - WE have more combinations in the original data that are available in the 2023 ATLAS download
# so can't valodate everything! Don't validate combined ones in case other things are included. 
##### Name and drug matching from here: https://www.ecdc.europa.eu/en/publications-data/tessy-metadata-report
bug_drugs_dict <- read.csv("bug_names.csv")


#### organise the incidence for compariosn

# compare all the bug/resistances

for(i in 1:nrow(bug_drugs_dict)){
  
  for(fixed_incidence in c(T, F)){
    
    
    # choose one bug_drug
    
    bug_choice <-bug_drugs_dict[i, "combo_name"]
    model_target <- which(plain_model_names == plain_model_names[grepl(bug_drugs_dict[i, "bug"], plain_model_names) & grepl(bug_drugs_dict[i, "drug"], plain_model_names)])
    
    validation_data_R <- validation_data[Indicator== "R - resistant isolates" & 
                                           Population == bug_choice]
    
    coverage_23[validation_data_R, on = c(Country = "RegionName"), country_short := i.RegionCode]
    
    # load the specified model
    if(fixed_incidence == T){
      target_model <- readRDS(file = paste0("predictions/individual_predictionsfixed_",
                                            plain_model_names[model_target], ".RDS"))
    } else if (fixed_incidence == F){
      target_model <- readRDS(file = paste0("predictions/individual_predictionsvarying_",
                                            plain_model_names[model_target], ".RDS"))
    }
    #take out of the storage list
    prediction_data <- target_model[[1]]
    predictions_plain <- target_model[[2]]
    predictions_age <- target_model[[3]]
    
    # combine data and predictions
    prediction_data_plain <- data.table(cbind(prediction_data, predictions_plain))
    prediction_data_age <- data.table(cbind(prediction_data, predictions_age))
    
    ######## AGE MODEL ######
    #melt and combine accross age and sex
    prediction_data_age_2023 <- prediction_data_age[year_s+2009 == 2023]
    prediction_data_age_2023[, c("year_s", "age_s","age_group", "age_squared_s", "sex", "pathogen", "total", "variable", "value") := NULL]
    prediction_data_age_2023 <- melt(prediction_data_age_2023, id.vars = c("country", "projection"))
    prediction_data_age_2023 <- prediction_data_age_2023[, sum(.SD), by = c("country", "projection", "variable")]
    
    #work out range
    pred_age_2023 <- prediction_data_age_2023[, quantile(V1, probs= c(0.025, 0.5, 0.975), na.rm=T),
                                              by=c("projection", "country")]
    pred_age_2023$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(pred_age_2023)/3)
    pred_age_2023 <- dcast.data.table(pred_age_2023,country + projection~stat, value.var = "V1")
    
    #add in teh actual 2023 data
    colnames(validation_data_R)[which(colnames(validation_data)=="RegionCode")] <- "country"
    pred_age_2023[validation_data_R, on = "country", num := i.NumValue]
    pred_age_2023[coverage_23, on = c(country = "country_short"), cov := i.Coverage]
    
    pred_age_2023[, num := as.numeric(num)]
    pred_age_2023[, cov := as.numeric(cov)]
    pred_age_2023[, act_num := num/(cov/100)]
    
    if(fixed_incidence == T){
      pred_age_2023_T <- pred_age_2023
      pred_age_2023_T$type = "fixed"
    }
    
    if(fixed_incidence == F){
      pred_age_2023_F <- pred_age_2023
      pred_age_2023_F$type = "linear"
    }
    
  }
  combo <- rbind(pred_age_2023_T, pred_age_2023_F)
  
  VALIDATION_LOG <-  ggplot(combo[projection == "BSL"], aes(x=country, y = Q50)) + 
    geom_pointrange(aes(ymin =Q2.5, ymax=Q97.5, colour = type, fill = type), position = position_dodge(width=0.5)) + 
    facet_grid(projection~.) + 
    coord_flip() + 
    scale_y_log10() + 
    geom_point(aes(y = act_num), colour = "black") + 
    labs(title = paste0("2023: ", bug_choice))
  
  VALIDATION <-  ggplot(combo[projection == "BSL"], aes(x=country, y = Q50)) + 
    geom_pointrange(aes(ymin =Q2.5, ymax=Q97.5, colour = type, fill = type), position = position_dodge(width=0.5)) + 
    facet_grid(projection~.) + 
    coord_flip() + 
    geom_point(aes(y = act_num), colour = "black") + 
    scale_y_continuous("Incidence per 100,000") + 
    labs(title = paste0("2023: ", bug_choice))
  
  #check if even or not, so can get two plots per figure
  if(i %% 2 == 0){
    
    ggsave(grid.arrange( VALIDATION_1, VALIDATION, nrow =1), 
           file=paste0("plots_nw/validation_inc_",bug_choice,".pdf"),
           width =9, height = 5)
    
  }   else {
    
    VALIDATION_1 <- VALIDATION
  }
  
  
  combo$bug <- bug_choice
  store_comparison <- rbind(store_comparison, combo)
  
  # ##### COMPARE INCIDENCE #####
  # # validation against total tests
  # validation_data_T <- validation_data[Indicator== "Total tested isolates" & 
  #                                        Population == bug_choice]
  # 
  # 
  # tot_incidence_23[coverage_23, on = c(country = "country_short"), cov := i.Coverage]
  # tot_incidence_23[, estimated := V1/(cov/100)]
  # # Subset the relevant pathogen. 
  # if(length(grep(plain_model_names[model_target], pattern = "esccol"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Escherichia coli"] 
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "acispp"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Acinetobacter spp"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "staaur"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Staphylococcus aureus"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "strpne"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Streptococcus pneumoniae"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "encfai"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Enterococcus faecalis"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "encfae"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Enterococcus faecium"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "klepne"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Klebsiella pneumoniae"]
  #   
  # } else if(length(grep(plain_model_names[model_target], pattern = "pseaer"))==1){
  #   inc_comp <-tot_incidence_23[pathogen == "Pseudomonas aeruginosa"]
  # }
  # 
  # inc_comp[validation_data_T, on =c(country = "RegionCode"), validation := i.NumValue]
  # inc_comp[, validation := as.numeric(validation)]
  # 
  # 
  # inc_comp_m <- melt.data.table(inc_comp, id.vars = c("country", "pathogen", "V1", "cov"))
  # 
  # VALIDATION_INCIDENCE <- ggplot(inc_comp_m, aes(x=country, y = value, colour = variable, group = country)) + 
  #   geom_point() +  
  #   scale_y_log10()+
  #   geom_line(linetype= "dotted", colour = "black") +
  #   coord_flip() + 
  #   scale_color_manual(values = c("black", "red"))+
  #   labs(title = paste0(bug_choice, ": incidence validation"))
  
  
}

#### Explore log error 
store_comparison <- data.table(store_comparison)
store_comparison[, percent_dif := ((act_num-Q50)/act_num)*100]
subset_to_validate <- store_comparison[projection =="BSL" &
                                         type == "linear" &
                                         !is.na(percent_dif), ]

subset_to_validate[, log_error :=log(Q50/act_num)]


LOGERROR <- ggplot(subset_to_validate,
                   aes(x = log_error, fill = country)) + geom_histogram() + 
  geom_vline(xintercept = 0.7, linetype = "dashed") + 
  geom_vline(xintercept = -0.7, linetype = "dashed") + 
  # lims(x = c(0,250)) + 
  labs(x ="Log error")

ggsave(grid.arrange(LOGERROR, nrow =1), file=paste0("plots_nw/logerror.pdf"),
       width =9, height = 5)

# How many can compare? which country antibiotic bacteria can we compare? 
dim(subset_to_validate)
# 1/2 - 2
dim(subset_to_validate %>% filter(log_error < 0.7, log_error > -0.7))[1] / dim(subset_to_validate)[1]
# 1/3 - 3
dim(subset_to_validate %>% filter(log_error < 1.0986, log_error > -1.0986))[1] / dim(subset_to_validate)[1]
# 2/3 within 1/3 - 3 

subset_to_validate %>% filter(percent_dif > -Inf) %>% 
  summarise(mean = mean(percent_dif, na.rm = TRUE),
            med = median(percent_dif, na.rm = TRUE), 
            range = range(percent_dif))

dim(subset_to_validate %>% filter(percent_dif > -50, percent_dif < 50))[1] / dim(subset_to_validate)[1]
# 39% within 50% 
dim(subset_to_validate %>% filter(percent_dif > -100, percent_dif < 100))[1] / dim(subset_to_validate)[1]
# 60% within 100% 

# Some countries better than others
nbug = length(unique(subset_to_validate$bug))
nc = length(unique(subset_to_validate$country))
subset_to_validate %>% group_by(country) %>% filter(log_error < 0.7, log_error > -0.7) %>%
  summarise(perc_bugs_per_c = 100 * n()/nbug) %>%
  arrange(desc(perc_bugs_per_c)) %>%
  print(n=Inf)

0.5*nc

# Some bacteria better than others 
subset_to_validate %>% group_by(bug) %>% filter(log_error < 0.7, log_error > -0.7) %>%
  summarise(perc_bugs_per_c = 100 * n()/nc) %>%
  arrange(desc(perc_bugs_per_c)) %>%
  print(n=Inf)
  
0.5*nbug
filter(perc_bugs_per_c > 50)

