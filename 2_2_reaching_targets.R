# set up the interventions to change the incidence rate. 

# need to run setup.r and 1_2_calculate_incidence.R first


# storage
store_incidence_options <- d_for_model[,c("country", "year_s", "age_s", "age_squared_s", "sex","total_fixed", "total_varying", "pathogen", "projection")]
colnames(store_incidence_options)[which(colnames(store_incidence_options)=="total_fixed")] <- "fixed"
colnames(store_incidence_options)[which(colnames(store_incidence_options)=="total_varying")] <- "varying"



for(ivt in 3:nrow(interventions)){ # for each intervention (ignoring fixed and varying)
# how much to change the incidence changing rate by
 intervention_incidence <- as.numeric(interventions[ivt,"incidence_change"])
 year_to_start_intervention <- as.numeric(interventions[ivt,"intervention_start"])
 name_run <- as.character(interventions[ivt,"intervention_name"])
 age_targetting <- as.numeric(interventions[ivt, "age_targetting"])
 # add on new intervention to storage incidence, so can make nice plots at end
 storage_incidence_across[lowage >= age_targetting, paste0(name_run):= year + intervention_incidence]
 storage_incidence_across[lowage < age_targetting, paste0(name_run):= year]
 
 #how many years before change
 d_for_model[,year := year_s + 2009]
 d_for_model[year <year_to_start_intervention, years_old_rate:= year - 2019] # How many years on rate from 2019
 d_for_model[year >=year_to_start_intervention, years_old_rate:= (year_to_start_intervention - 2019)-1] # -1 because the intervention year is already the new one
 # how many years after change
 d_for_model[year < year_to_start_intervention, years_new_rate:= 0]
 d_for_model[year >= year_to_start_intervention, years_new_rate:= (year - year_to_start_intervention)+1] # +1 because the intervention year is this one
 
 # add in the new rate - split by target ageing!
 d_for_model[age_s>=(age_targetting/100), new_rate := rate_change + intervention_incidence]
 d_for_model[age_s<(age_targetting/100), new_rate := rate_change]
 
 # calculate the rate by year
 d_for_model[, annual_incidence := base_incidence + rate_change*years_old_rate + new_rate*years_new_rate]
 # incidence can't be below 0
 d_for_model[annual_incidence<0, annual_incidence:= 0]

 # calculate the incidence based on demographic changes and the incidence that year
 d_for_model[, total_intervention := (annual_incidence/100000 * value)]

 #move the new incidence across to the storage data.frame
 store_incidence_options[d_for_model, on = c("country", "year_s", "age_s", "age_squared_s","sex", "pathogen", "projection"), paste0(name_run) := total_intervention]

 }
 
store_incidence_options_m <- melt.data.table(store_incidence_options,
                                             id.vars = c("country", "year_s", "age_s", "age_squared_s","sex",
                                                         "pathogen", "projection"))
# Make a plot of the incidence over time
time_summary <- store_incidence_options_m[projection %in% c("BSL", "LMRT"),
                                          sum(value),
                                          by = c("projection", "sex", "year_s", "pathogen", "variable")]
time_summary[, year := year_s + 2009]

INTERVENTION_INCIDENCE <- ggplot(time_summary[projection == "BSL" & year %in% c(2019:2050)],
                                 aes(x = year, y = V1, colour = variable)) + 
   facet_grid(sex~pathogen, scales = "free_y") + 
   geom_line() + 
   labs(y = "BSI incidence", title = "Comparison of interventions")

# Make a plot of the incidence over time
store_incidence_options_m[, age_group := cut((100*age_s), breaks =seq(-1, 120, 15))] # Add age groupings
store_incidence_options_m[age_group== "(-1,14]", age_group := "(0,14]"]
store_incidence_options_m[,age_group := factor(age_group, levels = c(unique(projections_by_path$age_group)))]

age_summary <- store_incidence_options_m[projection %in% c("BSL", "LMRT"), sum(value), by = c("projection", "year_s", "age_group", "variable", 
                                                                                              "pathogen")]
age_summary[, year := year_s +2009]



INTERVENTION_AGE<- ggplot(age_summary[projection == "BSL" & year %in% c(2019:2050)],
                                 aes(x = year, y = V1, colour = variable)) + 
   facet_grid(pathogen~age_group, scales = "free_y") + 
   geom_line() + 
   labs(y = "BSI incidence", title = "Comparison of interventions")



# Now want to predict from this. But this will take time! 
# -> run the 2_1_predict_from_models.R script, with vary_incdencs set to "intervention"
d_modelling <- store_incidence_options_m

colnames(storage_incidence_across)[which(colnames(storage_incidence_across)=="year")] <- "varying"
incidence_changes <- melt.data.table(storage_incidence_across, id.vars= c("lowage", "gender", "pathogen"))

incidence_changes[interventions, on =c(variable = "intervention_name"), intervention_start := i.intervention_start]
incidence_changes[variable == "varying", intervention_start := 2019]
incidence_changes[, label_facet := paste0(variable, ": ", intervention_start)]
incidence_changes[label_facet == "minus5_over65: 2027", label_facet := "minus5,65+: 2027"]
incidence_changes[label_facet == "varying: 2019", label_facet := "linear: 2027"]

incidence_changes[, label_facet := factor(label_facet, levels=c("linear: 2027", "minus1: 2027", "minus5: 2027", "minus5,65+: 2027", "minus20: 2027"))]


INTERVENTION_COMPARISONS <- ggplot(incidence_changes, aes(x = pathogen, y = lowage, fill = value)) + 
   facet_grid(label_facet~gender) + geom_tile() +
   scale_fill_gradient2(low = "gold", high = "darkred", mid = "lightblue") + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
   labs(y = "age group", fill = "Rate of change")

ggsave(grid.arrange(INTERVENTION_INCIDENCE, INTERVENTION_COMPARISONS, layout_matrix = rbind(c(1,1,2))),
       file="plots_nw/intervention_incidence.pdf",
       width =20, height = 10)

saveRDS(INTERVENTION_COMPARISONS, file = "output/intervention_heatmap.RDS")


relative_change <- dcast.data.table(time_summary[projection == "BSL" & year_s %in% c(2022-2009, 2030-2009)], 
                                    projection + sex + pathogen + variable ~ year, value.var = "V1")
relative_change[, change := `2030`- `2022`]
relative_change[, relative_change := change/`2022`]
mins <- relative_change[, min(relative_change), by = c("variable")]
maxs <- relative_change[, max(relative_change), by = c("variable")]

relative_change[variable == unlist(mins[1, "variable"]) & relative_change==unlist(mins[1,"V1"])]
relative_change[variable == unlist(mins[2, "variable"]) & relative_change==unlist(mins[2,"V1"])]

relative_change[variable == unlist(maxs[1, "variable"]) & relative_change==unlist(maxs[1,"V1"])]
relative_change[variable == unlist(maxs[2, "variable"]) & relative_change==unlist(maxs[2,"V1"])]



relative_change_age <- dcast.data.table(age_summary[projection == "BSL" & year_s %in% c(2022-2009, 2030-2009)], 
                                    projection + age_group + pathogen + variable ~ year, value.var = "V1")
relative_change_age[, change := `2030`- `2022`]
relative_change_age[, relative_change := change/`2022`]

relative_change_age[variable == "fixed"]
relative_change_age[variable == "varying"]

# look at relative male vs female in 2030
relative_sex_30 <- dcast.data.table(relative_change, projection + pathogen + variable ~ sex, value.var = "2030")
relative_sex_30[, in_30 := f/m]
rel_sex_combo <- relative_sex_30[variable == "varying"]
# and in 2022
relative_sex_22 <- dcast.data.table(relative_change, projection + pathogen + variable ~ sex, value.var = "2022")
relative_sex_22[, relative := f/m]
rel_sex_combo$in_22 <-relative_sex_22[variable == "varying"]$relative
# compare