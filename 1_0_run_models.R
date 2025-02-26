# This scripts runs Bayesian hierarchical regression models
# There are two sets of models - one with age and sex and one without
# Takes a VERY LONG TIME! (weeks)


# fit the no age no sex models
for(i in unique(data_use1$combo)){
  
  temp_data <- data_use1[combo ==i]
  
  # Which countries? 
  countries <- unique(temp_data$country)
  # These one need additional control factors to converge
  if (i %in% c("esccolureidopen_R", "esccolpseaer_multi_R", "esccolert_R")){
    
    testmodel <-brm(data = temp_data, 
                    family = binomial,
                    rest | trials(total) ~ 1 + year_s + (1 +year_s| country), 
                    file = paste0("brms_fits/plain_",i),
                    file_refit = getOption("brms.file_refit", "always"),
                    save_pars = save_pars(all = TRUE), 
                    control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15))
    
  } else { 
    # all other ones can use standard fit. 
    testmodel <-brm(data = temp_data, 
                    family = binomial,
                    rest | trials(total) ~ 1 + year_s + (1 +year_s| country), 
                    file = paste0("brms_fits/plain_",i),
                    file_refit = getOption("brms.file_refit", "always"),
                    save_pars = save_pars(all = TRUE))
  }
  
  #model predictions
  testout <- data.table(predict(testmodel))
  
  # summaries for plotting
  temp_data$Estimate <- testout$Estimate
  temp_data$Est.Error <- testout$Est.Error
  temp_data$Q2.5 <- testout$Q2.5
  temp_data$Q97.5 <- testout$Q97.5
  temp_data[, prop_est := Estimate/total]
  temp_data[, prop_Q2.5 := Q2.5/total]
  temp_data[, prop_Q97.5 := Q97.5/total]
  
  #plot the fit
  TEMPPLOT <-  ggplot(temp_data, aes(x = year_s, y = proportion)) + 
    facet_wrap(country~.) + 
    geom_point() + 
    geom_ribbon(aes(ymin = prop_Q2.5, ymax = prop_Q97.5), alpha = 0.5) + 
    geom_line(aes(y = prop_est)) + 
    labs(title = c(paste0(i, " bayes r2 is ", round(bayes_R2(testmodel)[1],digits=3)))) 
  #save
  ggsave(paste0("plots_nw/brms_",sub(" ", "_", i),".pdf"),
         plot = TEMPPLOT, 
         width = 10, height = 5)
  #track progress
  print(paste0("Completed ", i))
  
}


######### with age and sex ######
# this doesn't run any predictions!
for(i in unique(data_use_ages$combo)){
  
  # these are the ones that need special help
  # for(i in c(#"acisppaminogl_R", "encfaiaminopen_R", 
  #   "esccolcarbapen_R",
  #   "esccolert_R", "esccolpseaer_multi_R", "strpnecefIII_strpne_R", 
  #   "encfaevanco_R", "acisppfq_pseudo_R")){
  temp_data <- data_use_ages[combo ==i]
  
  # Which countries? 
  countries <- unique(temp_data$country)
  
  # ones that had divergent transitions so need more control
  if (i %in% c(#"acisppaminogl_R", "encfaiaminopen_R", 
    "esccolcarbapen_R",
    "esccolert_R", "esccolpseaer_multi_R", "strpnecefIII_strpne_R", 
    "encfaevanco_R")){
    
    testmodelage <-brm(data = temp_data, 
                       family = binomial,
                       resn | trials(total) ~ 1 + year_s + age_s + age_squared_s + sex + (1 + age_s + year_s| country), 
                       file = paste0("brms_fits/extra_age_",i),
                       file_refit = getOption("brms.file_refit", "always"),
                       save_pars = save_pars(all = TRUE), 
                       chains =4,
                       control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15))
    
    # ones that didn't get a low Rhat so need to run longer
  } else if (i %in% c("acisppfq_pseudo_R")){  
    
    
    testmodelage <-brm(data = temp_data, 
                       family = binomial,
                       resn | trials(total) ~ 1 + year_s + age_s + age_squared_s + sex + (1 + age_s + year_s| country), 
                       file = paste0("brms_fits/extra_age_",i),
                       file_refit = getOption("brms.file_refit", "always"),
                       save_pars = save_pars(all = TRUE), 
                       chains =4, iter = 4000)
    
    
  } else if (i %in% c("encfaigenta_high")){  
    
    
    testmodelage <-brm(data = temp_data, 
                       family = binomial,
                       resn | trials(total) ~ 1 + year_s + age_s + age_squared_s + sex + (1 + age_s + year_s| country), 
                       file = paste0("brms_fits/extra_age_",i),
                       file_refit = getOption("brms.file_refit", "always"),
                       save_pars = save_pars(all = TRUE), 
                       chains =4, iter = 5000,
                       control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15))
    
    # the rest
  } else {
    testmodelage <-brm(data = temp_data, 
                       family = binomial,
                       resn | trials(total) ~ 1 + year_s + age_s + age_squared_s + sex + (1 + age_s + year_s| country), 
                       file = paste0("brms_fits/extra_age_",i),
                       file_refit = getOption("brms.file_refit", "always"),
                       save_pars = save_pars(all = TRUE), 
                       chains =4)
  }
  
}
