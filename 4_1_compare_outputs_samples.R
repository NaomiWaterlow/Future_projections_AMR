######################################################################################
####### Generates the impacts of each interventions on each bug-drug #################
####### Developers: N Waterlow & G Knight ############################################
######################################################################################

# This processes and analyzes model predictions (from posteriors) on the impact of interventions on 
# resistant bloodstream infections (BSIs) caused by bacteria and antibiotics. 
# It loads model outputs, calculates summary statistics, and compares predictions 
# across various interventions and models. 
# The code generates several visualizations including trends in BSIs over time, 
# country-specific projections, and a heatmap showing relative changes in resistance. 
# Results are saved for further analysis, with comparisons to 2019 baseline data. 
# The script also includes options for generating plots to visualize intervention effects.
# Also generates FIGURE 3 and FIGURE 5

source("3_0_load_models.R")

# look at one example
model_target_multi <- c(2,13,33)
specific_subset <- model_target_multi
model_target_multi <- c(1:length(plain_model_names))

agesex_country_combo <- data.frame()
agesex_overall_combo <- data.frame()

########################################################################################################################################
################################# To run them all #####################################################################################
########################################################################################################################################
# To get all results

for(ivt in 1:nrow(interventions)){
  for(model_target in model_target_multi){
    
    # load the specified model
    name_run <- as.character(interventions[ivt,"intervention_name"])
    target_model <- readRDS(file = paste0("predictions/individual_predictions",name_run,"_",
                                          plain_model_names[model_target], ".RDS"))
    
    #take out of the storage list
    prediction_data <- target_model[[1]] # this is the input data used to predict from
    predictions_plain <- target_model[[2]] # this is the predictions from the 'plain' model
    predictions_age <- target_model[[3]] # this is the predictions from the 'age' model
    
    # combine data and predictions
    prediction_data_plain <- data.table(cbind(prediction_data, predictions_plain))
    prediction_data_age <- data.table(cbind(prediction_data, predictions_age))
    
    # For the plain model
    #remove the extra columns for ease
    temp_plain <- data.table(copy(prediction_data_plain))
    
    temp_plain[, c("age_squared_s", "age_s","age_group" ,"sex", "total", "pathogen", "variable", "value") := NULL]
    # sum across the removed variables
    plain_summary <- temp_plain[, lapply(.SD, sum, na.rm=TRUE),
                                by=c("year_s", "country","projection") ]
    # melt
    plain_summary <- melt.data.table(plain_summary, id.vars=c("year_s", "projection", "country"))
    #rename
    colnames(plain_summary)[which(colnames(plain_summary)== "value")] <- "plain"
    
    # For the age model
    #remove the extra columns for ease
    temp_age <- copy(prediction_data_age)
    temp_age[, c("age_squared_s", "age_s","age_group", "sex", "total", "pathogen", "variable", "value") := NULL]
    # sum across the removed variables
    age_summary <- temp_age[, lapply(.SD, sum, na.rm=TRUE),
                            by=c("year_s","projection", "country") ]
    # melt
    age_summary <- melt.data.table(age_summary, id.vars=c("year_s", "projection","country"))
    #rename
    colnames(age_summary)[which(colnames(age_summary)== "value")] <- "agesex"
    
    # merge together the two tables
    both_models <- plain_summary[age_summary, on = c("year_s", "projection", "variable", "country"), 
                                 agesex := i.agesex]
    # calculate difference
    both_models[, difference := agesex - plain]
    
    # for initial plots just look at age model
    # add quantiles and make long format
    agesex_country <- both_models[, quantile(agesex, probs= c(0.025, 0.5, 0.975)),
                                  by=c("year_s", "projection", "country")]
    agesex_country$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(agesex_country)/3)
    agesex_country <- dcast.data.table(agesex_country, year_s + country + projection~stat, value.var = "V1")
    
    
    #look at overall (combine across countries)
    #add quantiles and make long format
    agesex_overall <- both_models[, sum(agesex), by = c("year_s", "projection", "variable")]
    agesex_overall <- agesex_overall[, quantile(V1, probs= c(0.025, 0.5, 0.975)),
                                     by=c("year_s", "projection")]
    agesex_overall$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(agesex_overall)/3)
    agesex_overall <- dcast.data.table(agesex_overall, year_s  + projection~stat, value.var = "V1")
    
    
    TREND_OVERALL<- ggplot(agesex_overall[year_s <= c(2030-2009)], aes(x = year_s, y = Q50)) + 
      geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = projection), alpha = 0.5) + 
      geom_line(aes(colour = projection)) + 
      # geom_hline(yintercept = 0, linetype =2) + 
      scale_fill_manual(values = c("darkgreen", "orange", "purple"))+
      scale_colour_manual(values = c("darkgreen", "orange", "purple"))+
      #  lims(y = c(1,120000000))+
      labs(y = "Resistant BSI. Median and 95% Q", 
           title = paste0(str_remove(str_remove(plain_model_names[model_target], pattern ="plain_"), pattern = ".rds")))
    
    
    TREND_BY_COUNTRY <- ggplot(agesex_country[year_s <= c(2030-2009)], aes(x = year_s, y = Q50)) + 
      geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = projection), alpha = 0.5) + 
      geom_line(aes(colour = projection)) + 
      geom_hline(yintercept = 1, linetype =2) + 
      scale_fill_manual(values = c("darkgreen", "orange", "purple"))+
      scale_colour_manual(values = c("darkgreen", "orange", "purple"))+
      facet_wrap(.~country, scales = "free_y", nrow=3) + 
      labs(y = "Resistant BSI. Median and 95% Q", 
           title = paste0(str_remove(str_remove(plain_model_names[model_target], pattern ="plain_"), pattern = ".rds")))
    
    ggsave(grid.arrange(TREND_OVERALL, TREND_BY_COUNTRY),
           , file=paste0("plots_nw/BSIs_",name_run,"_",plain_model_names[model_target], ".pdf"),
           width =10, height = 7)
    
    
    agesex_overall$bug <- str_remove(str_remove(plain_model_names[model_target], pattern ="plain_"), pattern = ".rds")
    agesex_country$bug <- str_remove(str_remove(plain_model_names[model_target], pattern ="plain_"), pattern = ".rds")
    agesex_overall$type <- name_run
    agesex_country$type <- name_run
    agesex_overall_combo <- rbind(agesex_overall_combo, agesex_overall)
    agesex_country_combo <- rbind(agesex_country_combo, agesex_country)
    
    
    print(paste0(name_run, " for model ", model_target, " complete"))
  }
}

# save all the above as can take time - but only do if doing all names otherwise overwrites
if(length(model_target_multi)>3){
  saveRDS(list(agesex_country_combo, agesex_overall_combo), file = "output/compare_outputs.RDS")
}

########################################################################################################################################
################################# If running later #####################################################################################
########################################################################################################################################
compare_outputs <- readRDS("output/compare_outputs.RDS")
agesex_country_combo <- compare_outputs[[1]]
agesex_overall_combo <- compare_outputs[[2]]

agesex_country_combo[which(type == "varying"), "type"] <- "linear"
agesex_overall_combo[which(type == "varying"), "type"] <- "linear"
agesex_country_combo[,type := factor(type, levels=c("fixed", "linear", "minus1", "minus5", "minus5_over65", "minus20"))]
agesex_overall_combo[,type := factor(type, levels=c("fixed", "linear", "minus1", "minus5", "minus5_over65", "minus20"))]

#but we want to compare to 2019 - so need to load in the actual data. 
# will already be loaded from Setup.R
#subset pathogen
cleaned_string <- gsub("^(plain_)|(.rds$)", "", plain_model_names[model_target_multi])

# subset year
data_2019 <- data_use_ages[year == 2019 & combo %in% cleaned_string]
coverages <- data.table(read.csv("data/est_pop_cov_final.csv"))
coverage_19 <- coverages[year == 2019]
# use this just for quick country name conversion
use_for_country <- data.table(read.csv("data/ECDC_Atlas_data_2023.csv"))
coverage_19[use_for_country, on = c(country ="RegionName"), country_short := i.RegionCode]
rm(use_for_country)

data_2019[coverage_19, on=c(country = "country_short"), coverage := i.est_pop_cov_perc ]
data_2019[, total_R := resn/(coverage/100)]
data_2019 <- data_2019[!is.na(total_R)]
# want to compare against 2019 values, so need to change the 2019 format into this same, overall data. 
# so match on country only. Need to sum the RESISTANT BSIs.
start_points <- data_2019[, sum(total_R), by = c("year", "country", "combo")] #i.e. not by age or sex
## Calculate relative changes
agesex_country_combo[start_points,on = c("country", bug = "combo"), base := i.V1 ]
agesex_country_combo[, rel_Q2.5 := Q2.5/base]
agesex_country_combo[, rel_Q50 := Q50/base]
agesex_country_combo[, rel_Q97.5 := Q97.5/base]
agesex_country_combo <- agesex_country_combo[!is.na(rel_Q50)]
# label the bugs and drugs nicely
bug_names<- data.table(read_csv("translate_drug_bugs.csv"))
bug_names[, label := paste0(bac, drug)]
agesex_country_combo[bug_names, on= .(bug = label),bac := Bacteria]
agesex_country_combo[bug_names, on= .(bug = label),drug := Antibiotic]
# subset relevant rows for heatmap and format for plotting
heatmap_maker <- agesex_country_combo[projection == "BSL" & year_s == (2030-2009) & type == "linear"]
heatmap_maker_ordering <- heatmap_maker[bac  == "Escherichia coli" & drug == "Fluoroquinolones"]
heatmap_maker[, country := factor(country, levels = heatmap_maker_ordering[order(rel_Q50)]$country )]
heatmap_maker_ordering <- heatmap_maker[country  == "DE"]
heatmap_maker[, bug := factor(bug, levels = heatmap_maker_ordering[order(rel_Q50)]$bug )]
# manually change the infinite values to the extremes of the heatmap
heatmap_maker[ base == 0, rel_Q50 := exp(5)]
heatmap_maker[ Q50 == 0, rel_Q50 := exp(-2.5)]
heatmap_maker[ Q50 == 0 & base == 0, rel_Q50 := exp(0)]

heatmap_maker <- heatmap_maker %>%
  mutate(var = case_when(bac == 'Enterococcus faecalis' ~ 'E. faecalis',
                         bac == 'Streptococcus pneumoniae' ~ 'S. pneumoniae',
                         bac == 'Enterococcus faecium' ~ 'E. faecium',
                         bac == 'Klebsiella pneumoniae' ~ 'K. pneumoniae',
                         bac == 'Pseudomonas aeruginosa' ~ 'P. aeruginosa',
                         bac == 'Staphylococcus aureus' ~ 'S. aureus',
                         bac == 'Staphylococcus aureus' ~ 'S. aureus'))

# create the plot
HEATMAP <- ggplot(heatmap_maker, aes(x = country, y = drug, fill = log(rel_Q50))) + 
  geom_tile() + 
  facet_grid(bac~., scales = "free", space = "free")+
  scale_fill_gradient2(low = "darkgreen", high = "orange", mid = "white",
                       na.value = "lightgrey") + 
  labs(fill = "log(relative change)")
#What percentage increase
heatmap_maker[rel_Q50>1, .N ]/heatmap_maker[, .N ]
# How many countries go up or down by bug
what_prop_down <- heatmap_maker[rel_Q50<1, .N, by = "bug" ]
what_prop_up <- heatmap_maker[rel_Q50>1, .N, by = "bug" ]
what_prop_up[what_prop_down, on = "bug", down := i.N]
what_prop_up[,prop_up :=  N/(N+down)]
what_prop_up[is.na(down), prop_up := 1]
what_prop_up[order(prop_up)]
# How many bugs go up and down by country
what_prop_down <- heatmap_maker[rel_Q50<1, .N, by = "country" ]
what_prop_up <- heatmap_maker[rel_Q50>1, .N, by = "country" ]
what_prop_up[what_prop_down, on = "country", down := i.N]
what_prop_up[,prop_up :=  N/(N+down)]
what_prop_up[is.na(down), prop_up := 1]
what_prop_up[order(prop_up)]

# subset the relevant names
specific_names <- cleaned_string[specific_subset]

# What is the difference by 2030
agesex_country_combo[projection == "BSL" & year_s %in% c(2030-2009) & 
                       type == "linear"& 
                       bug %in% specific_names] %>%
  group_by(bug) %>%
  arrange(rel_Q50) %>%
  filter(!rel_Q50== "Inf") %>%
  filter(row_number()==1 | row_number()==n()) %>%
  select(c("Q50","rel_Q50","bug")) %>%
  arrange(bug)


# make a plot
REL_COUNTRIES <- ggplot(agesex_country_combo[projection == "BSL" & year_s <= c(2030-2009) & 
                                               bug %in% specific_names],
                        aes(x = year_s+2009, y = rel_Q50)) + 
  geom_ribbon(aes(ymin = rel_Q2.5, ymax = rel_Q97.5, fill = type), alpha = 0.5) + 
  geom_line(aes(colour = type)) + 
  # geom_hline(yintercept = 0, linetype =2) + 
  #  scale_fill_manual(values = c("darkgreen", "orange"))+
  # scale_colour_manual(values = c("darkgreen", "orange"))+
  facet_grid(interaction(bac, drug, sep = "\n")~country, scales = "free_y") + 
  labs(y = "Resistant BSI vs 2019", title = "B", 
       x = "Year", fill = "Intervention", colour = "Intervention") + 
  geom_hline(yintercept = 1) + 
  geom_hline(yintercept = 0.9, linetype = "dotted")+
  theme(#legend.position="none", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.key.height = unit(1.5, "lines"))

REL_COUNTRIES_VARYING <- ggplot(agesex_country_combo[projection == "BSL" & year_s <= c(2030-2009) & 
                                                       type == "linear"& 
                                                       bug %in% specific_names],
                                aes(x = year_s+2009, y = rel_Q50)) + 
  geom_ribbon(aes(ymin = rel_Q2.5, ymax = rel_Q97.5, fill = interaction(bac, drug, sep = "\n")), alpha = 0.5) + 
  geom_line(aes(colour = interaction(bac, drug, sep = "\n"))) + 
  geom_hline(yintercept = 1, linetype =2) + 
  scale_fill_manual(values = c("darkgreen", "orange", "purple"))+
  scale_colour_manual(values = c("darkgreen", "orange", "purple"))+
  facet_wrap(~country, scales = "free_y", nrow = 3) + 
  lims(y = c(0,NA))+
  labs(y = "Resistant BSI vs 2019", title = "B", 
       x = "Year", fill = "Bacteria", colour = "Bacteria") + 
  theme(#legend.position="none", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.key.height = unit(1.5, "lines"))


# need to make sure the same countries are in each dataset, otherwise not a reasonable comparison!
to_exclude <- agesex_country_combo[is.na(base) & year_s == 13,][,c("country", "bug")]
colnames(data_2019)[which(colnames(data_2019) == "combo")] <- "bug"
subsetted <- data_2019[!to_exclude, on = .(country, bug)]
# that removes those that are in the predictions but not the original data
# but also need to remove the ones that are in the original data and not the predictions
in_data <- unique(data_2019[,c("country", "bug")])
in_agesex_country_combo <- unique(agesex_country_combo[,c("country", "bug")])
missing <- fsetdiff(in_data, in_agesex_country_combo) 
# also want to remove these
subsetted <- subsetted[!missing, on = .(country, bug)]

#calculate 2019 values
start_points <- subsetted[, sum(total_R), by = c("year", "bug")] #i.e. not by age or sex or country
# match across and work out relative
agesex_overall_combo[start_points,on = c("bug"), base := i.V1 ]
agesex_overall_combo[, rel_Q2.5 := Q2.5/base]
agesex_overall_combo[, rel_Q50 := Q50/base]
agesex_overall_combo[, rel_Q97.5 := Q97.5/base]
# label nice
agesex_overall_combo[bug_names, on= .(bug = label),bac := Bacteria]
agesex_overall_combo[bug_names, on= .(bug = label),drug := Antibiotic]

REL_OVERALL <- ggplot(agesex_overall_combo[projection == "BSL" & year_s <= c(2050-2009)& 
                                             bug %in% specific_names], aes(x = year_s+2009, y = rel_Q50)) + 
  geom_ribbon(aes(ymin = rel_Q2.5, ymax = rel_Q97.5, fill = type), alpha = 0.5) + 
  geom_line(aes(colour = type)) + 
  facet_grid(.~interaction(bac, drug, sep = "\n"))+
  # geom_hline(yintercept = 0, linetype =2) + 
  # scale_fill_manual(values = c("darkgreen", "orange"))+
  # scale_colour_manual(values = c("darkgreen", "orange"))+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  geom_vline(xintercept = 2030, linetype = "dotted")+
  #  lims(y = c(1,120000000))+
  labs(y = "Resistant BSI vs 2019",, title = "B",
       x = "Year", colour = "Intervention", fill = "Intervention")+
  theme(legend.key.height = unit(1.5, "lines"), 
        strip.text = element_text(face = "italic"))


REL_OVERALL_VARYING <- ggplot(agesex_overall_combo[projection == "BSL" & year_s <= c(2050-2009) &
                                                     type == "linear"& 
                                                     bug %in% specific_names], aes(x = year_s+2009, y = rel_Q50)) + 
  geom_ribbon(aes(ymin = rel_Q2.5, ymax = rel_Q97.5, fill =interaction(bac, drug, sep = "\n")), alpha = 0.5) + 
  geom_line(aes(colour = interaction(bac, drug, sep = "\n"))) + 
  #  facet_grid(.~bug)+
  # geom_hline(yintercept = 0, linetype =2) + 
  scale_fill_manual(values = c("darkgreen", "orange", "purple"))+
  scale_colour_manual(values = c("darkgreen", "orange", "purple"))+
  #  lims(y = c(1,120000000))+
  labs(y = "Resistant BSI vs 2019",, title = "A",
       x = "Year", colour = "Bacteria\nAntibiotic", fill = "Bacteria") + 
  theme(legend.key.height = unit(1.5, "lines"), 
        strip.text = element_text(face = "italic"))

ggsave(grid.arrange(REL_OVERALL_VARYING,
                    REL_COUNTRIES_VARYING + theme(legend.position = "none"),
                    HEATMAP + labs(title = "C", fill = "relative change (log)"),
                    layout_matrix = rbind(c(1,3), 
                                          c(2,3), 
                                          c(2,3)))
       , file=paste0("plots_nw/BSIs_combo",model_target_multi[1],"_",model_target_multi[2] ,".jpeg"),
       width =20, height = 10)

ggsave(grid.arrange(REL_OVERALL_VARYING,
                    REL_COUNTRIES_VARYING + theme(legend.position = "none"),
                    HEATMAP + labs(title = "C", fill = "relative change (log)"),
                    layout_matrix = rbind(c(1,3), 
                                          c(2,3), 
                                          c(2,3)))
       , file=paste0("plots_nw/FIGURE3.jpeg"),
       width =20, height = 10)



# ggsave(grid.arrange(REL_OVERALL, REL_COUNTRIES, layout_matrix = rbind(c(1), 
#                                                                                       c(2), 
#                                                                                       c(2)))
#        , file=paste0("plots_nw/BSIs_combo_interventions_",model_target_multi[1],"_",model_target_multi[2] ,".pdf"),
#        width =10, height = 7)

INTERVENTION_COMPARISONS <- readRDS(file = "output/intervention_heatmap.RDS")
saveRDS(REL_OVERALL, file = "output/rel_overall_plot.RDS")

ggsave(grid.arrange(REL_OVERALL, INTERVENTION_COMPARISONS + labs(title = "B"), layout_matrix = rbind(c(1,1,2), 
                                                                                                     c(1,1,2), 
                                                                                                     c(1,1,2)))
       , file=paste0("plots_nw/BSIs_combo_interventions_",model_target_multi[1],"_",model_target_multi[2] ,".pdf"),
       width =14, height = 8)

ggsave(grid.arrange(INTERVENTION_COMPARISONS + labs(title = "A"),REL_OVERALL, layout_matrix = rbind(c(1,2,2), 
                                                                                                     c(1,2,2), 
                                                                                                     c(1,2,2)))
       , file=paste0("plots_nw/FIGURE5.jpeg"),
       width =14, height = 8)

ggsave(HEATMAP, file = paste0("plots_nw/HEATMAP_SUPPLEMENT.pdf"), 
       width = 14, height = 10)

#### Extract summaries for text

text_summaries <- agesex_overall_combo[projection == "BSL" & year_s == c(2030-2009), ]
# How many of the bu-drugs get below 90% by intervention type. 
text_summaries[rel_Q50<=0.9, .N, by = "type"]#meet target
text_summaries[rel_Q97.5<=0.9, .N, by = "type"]#meet target
text_summaries[, .N, by = "type"] # total
props <- text_summaries[rel_Q50<=0.9, .N, by = "type"]
props[,percent := (N/38)*100]# percentages

# ##### Create all the intervention plots for the supplement

unique_bug_drugs <- unique(agesex_overall_combo$bug)

for(i in 1:length(unique_bug_drugs)){
  
  
  INTERVENTION <- ggplot(agesex_overall_combo[projection == "BSL" & year_s <= c(2050-2009)& 
                                                bug %in% unique_bug_drugs[i]], aes(x = year_s+2009, y = rel_Q50)) + 
    geom_ribbon(aes(ymin = rel_Q2.5, ymax = rel_Q97.5, fill = type), alpha = 0.5) + 
    geom_line(aes(colour = type)) + 
    facet_grid(.~interaction(bac, drug, sep = "\n"))+
    # geom_hline(yintercept = 0, linetype =2) + 
    # scale_fill_manual(values = c("darkgreen", "orange"))+
    # scale_colour_manual(values = c("darkgreen", "orange"))+
    geom_hline(yintercept = 0.9, linetype = "dashed")+
    geom_vline(xintercept = 2030, linetype = "dotted")+
    #  lims(y = c(1,120000000))+
    labs(y = "Resistant BSI vs 2019",
         x = "Year", colour = "Intervention", fill = "Intervention")+
    theme(legend.key.height = unit(1.5, "lines"), 
          strip.text = element_text(face = "italic"))
  
  ggsave(INTERVENTION, file = paste0("plots_nw/INTERVENTIONS_",unique_bug_drugs[i], ".pdf"), 
         width = 14, height = 10)
  
  
}


# range of relative changes by country
temp <- copy(agesex_country_combo)
temp <- temp[projection == "BSL" & year_s == 21 & bug == "acisppaminogl_R" & type == "linear"]

