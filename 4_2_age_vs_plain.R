# outputs across countries.

# from posteriors
# make some plots from predictions etc. 
counter <- 1  #used for saving the plots for main text
combined_results <- data.frame()
agesex_results <- data.frame()
model_target_multi <- c(2,13,33)

bug_names<- data.table(read_csv("translate_drug_bugs.csv"))
bug_names[, label := paste0(bac, drug)]

# run across bug_drugs
for(i in 
   # model_target_multi
    c(1:length(plain_model_names))
    ){
  
model_target <- i
display_percent <- T

# load the specified model - want the one with varying incidence as the base
target_model <- readRDS(file = paste0("predictions/individual_predictionsvarying_",
                                      plain_model_names[model_target], ".RDS"))
bug = bug_names[label ==str_remove(str_remove(plain_model_names[model_target], ".rds"), "plain_")]$Bacteria
drug = bug_names[label ==str_remove(str_remove(plain_model_names[model_target], ".rds"), "plain_")]$Antibiotic


#take out of the storage list
prediction_data <- target_model[[1]]
predictions_plain <- target_model[[2]]
predictions_age <- target_model[[3]]

# combine data and predictions
prediction_data_plain <- data.table(cbind(prediction_data, predictions_plain))
prediction_data_age <- data.table(cbind(prediction_data, predictions_age))

# want in 2050, the overall difference by country.
latest_plain <- prediction_data_plain[year_s == 2030-2009]
latest_age <- prediction_data_age[year_s == 2030-2009]

# For the plain model
#remove the extra columns for ease
temp_plain <- data.table(copy(latest_plain))
temp_plain[, c("age_squared_s", "total", "pathogen", "year_s",  "variable", "value", "age_group") := NULL]
# sum across the removed variables
plain_summary <- temp_plain[, lapply(.SD, sum, na.rm=TRUE),
                            by=c("age_s" ,"sex","country","projection") ]
# melt
plain_summary <- melt.data.table(plain_summary, id.vars=c("projection", "country", "age_s", "sex"))
#rename
colnames(plain_summary)[which(colnames(plain_summary)== "value")] <- "plain"

# For the age model
#remove the extra columns for ease
temp_age <- copy(latest_age)
temp_age[, c("age_squared_s", "total", "pathogen", "year_s", "variable", "value", "age_group") := NULL]
# sum across the removed variables
age_summary <- temp_age[, lapply(.SD, sum, na.rm=TRUE),
                        by=c("projection", "country", "age_s", "sex") ]
# melt
age_summary <- melt.data.table(age_summary, id.vars=c("age_s","sex", "projection","country"))
#rename
colnames(age_summary)[which(colnames(age_summary)== "value")] <- "agesex"

# merge together the two tables
both_models <- plain_summary[age_summary, on = c("age_s","sex", "projection", "variable", "country"), 
                             agesex := i.agesex]

# Want difference by country first (collapsing age and sex)
country_differences <- both_models[, sum(plain), by = c("projection", "country", "variable")]
colnames(country_differences)[which(colnames(country_differences)=="V1")] <- "plain"
tempo <- both_models[, sum(agesex), by = c("projection", "country", "variable")]
country_differences[tempo, on = c("projection", "country", "variable"), agesex := i.V1]

country_differences[, prop_dif := ((agesex-plain)/agesex)*100]
country_summary_percent <- country_differences[, quantile(prop_dif, probs= c(0.025, 0.5, 0.975), na.rm=T),
                              by=c("projection", "country")]
country_summary_percent$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(country_summary_percent)/3)
country_summary_percent <- dcast.data.table(country_summary_percent,country + projection~stat, value.var = "V1")

sorter <- country_summary_percent[projection == "BSL"]
country_summary_percent[, country := factor(country, levels = c(sorter[order(Q50)]$country))]

if(min(sorter$Q2.5, na.rm= T) < -500){
  lower_lim = -500
} else {lower_lim = min(sorter$Q2.5)*1.1}

if(max(sorter$Q97.5, na.rm = T) > 500){
  upper_lim = 500
} else {upper_lim = max(sorter$Q97.5)*1.1}

ACROSS_COUNTRIES_2050_PERCENT <- ggplot(country_summary_percent[projection =="BSL"], aes(x = country, y = Q50)) + 
  geom_hline(yintercept = 0, linetype = 1, colour = "grey60") + 
  geom_hline(yintercept = c(100,-100), linetype =2, colour = "grey60") + 
  geom_hline(yintercept = c(50,-50), linetype =3, colour = "grey60") + 
  coord_cartesian(y = c(lower_lim, upper_lim)) + 
  geom_pointrange(aes(ymin = Q2.5, y = Q50, ymax=Q97.5)) + 
  labs(x = "Country", y = "Percentage 'wrong' by not using agesex model", 
       title = bquote("B     2030 only: " * italic(.(bug)) * " " * .(drug)))

country_differences[, difference := agesex - plain]
country_summary <- country_differences[, quantile(difference, probs= c(0.025, 0.5, 0.975), na.rm=T),
                                               by=c("projection", "country")]
country_summary$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(country_summary)/3)
country_summary <- dcast.data.table(country_summary,country + projection~stat, value.var = "V1")


ACROSS_COUNTRIES_2050 <- ggplot(country_summary[projection =="BSL"], aes(x = country, y = Q50)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_pointrange(aes(ymin = Q2.5, y = Q50, ymax=Q97.5),  position = position_dodge(width = 1)) +
  labs(x = "Country", y = "BSI R difference", 
       title = paste0("B     2030 only: ", str_remove(str_remove(plain_model_names[model_target], ".rds"), "plain_")))


# Now difference by age/sex (collapsing country)
agesex_differences <- both_models[, sum(plain), by = c("projection", "age_s","sex", "variable")]
colnames(agesex_differences)[which(colnames(agesex_differences)=="V1")] <- "plain"
tempo <- both_models[, sum(agesex), by = c("projection", "age_s","sex", "variable")]
agesex_differences[tempo, on = c("projection", "age_s","sex", "variable"), agesex := i.V1]

agesex_differences[, percent_dig := ((agesex - plain)/agesex)*100]
agesex_summary_percent <- agesex_differences[, quantile(percent_dig, probs= c(0.025, 0.5, 0.975), na.rm=T),
                                               by=c("projection", "age_s", "sex")]
agesex_summary_percent$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(agesex_summary_percent)/3)
agesex_summary_percent <- dcast.data.table(agesex_summary_percent,
                                           age_s+sex + projection~stat, value.var = "V1")

# also do a non percent version

agesex_differences[, difference := (agesex - plain)]
agesex_dif <- agesex_differences[, quantile(difference, probs= c(0.025, 0.5, 0.975), na.rm=T),
                                             by=c("projection", "age_s", "sex")]
agesex_dif$stat <- rep(c("Q2.5", "Q50", "Q97.5"), nrow(agesex_dif)/3)
agesex_dif <- dcast.data.table(agesex_dif,age_s+sex + projection~stat, value.var = "V1")



ACROSS_AGESEX_2050_PERCENT <- ggplot(agesex_summary_percent[projection =="BSL"], aes(x = age_s*100, y = Q50, fill = sex)) +
  geom_hline(yintercept = 0, linetype = 1, colour = "grey60") +
  geom_hline(yintercept = c(-100,100), linetype = 2, colour = "grey60")+
  geom_hline(yintercept = c(-50,50), linetype = 3, colour = "grey60")+
#  coord_cartesian(ylim= c(-200,200))+
  geom_ribbon(aes(ymin = Q2.5, y = Q50, ymax=Q97.5), alpha=0.4) + 
  geom_line(aes(colour = sex))+
  labs(x = "Age", y = "BSI R difference", 
       title = paste0("A     2030 only: ", bug, " ", drug))

ACROSS_AGESEX_2050 <- ggplot(agesex_dif[projection =="BSL"], aes(x = age_s*100, y = Q50, fill = sex)) +
  geom_line(aes(colour = sex))+
  geom_ribbon(aes(ymin = Q2.5, y = Q50, ymax=Q97.5), alpha=0.4) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(x = "Age", y = "BSI R difference", 
       title = paste0("A     2030 only: ", bug, " ", drug))

# optional graphs - not super interesting
  # ggsave(grid.arrange(ACROSS_AGESEX_2050, ACROSS_COUNTRIES_2050_PERCENT),
  #        file=paste0("plots_nw/supplement_2030_summary2_",plain_model_names[model_target], ".pdf"),
  #        width =10, height = 7)
  
  if(i %in% model_target_multi){
   assign(paste0("fish_plot_", counter),ACROSS_AGESEX_2050)
    counter <- counter + 1
   }
  
  # create summary stats for the table
  agesex_dif_BSL <- agesex_dif[projection =="BSL"]
  agesex_dif_BSL_labeled <- agesex_dif_BSL
  agesex_dif_BSL_labeled[, type := str_remove(str_remove(plain_model_names[model_target], ".rds"), "plain_")]
  agesex_dif_BSL_labeled <- agesex_dif_BSL_labeled[,c(1:7)]
   #totals
  # Overall sums
  agesex_results <- rbind(agesex_results, 
                          agesex_dif_BSL_labeled)
  
  overall_sums <- agesex_dif_BSL[, lapply(.SD, function(x) round(sum(x))), .SDcols = c("Q2.5", "Q50", "Q97.5")]

  overall_sums[, sex := "Overall"] # Add a row label for "Overall"
  
  # Sums by sex
  sums_by_sex <- agesex_dif_BSL[, lapply(.SD, function(x) round(sum(x))), by = "sex", .SDcols = c("Q2.5", "Q50", "Q97.5")]
  
  # Combine results
  results_temp <- rbind(overall_sums, sums_by_sex, fill = TRUE)
  results_temp[, label := paste0(Q50, " (", Q2.5," - ", Q97.5, ")")]
  
  agesex_dif_BSL[, lapply(.SD, function(x) round(sum(x))), .SDcols = c("Q2.5", "Q50", "Q97.5")]  
  agesex_dif_BSL[, lapply(.SD, function(x) round(sum(x))), by = "sex", .SDcols = c("Q2.5", "Q50", "Q97.5")]
  agesex_dif_BSL[, age_group := cut(age_s, breaks = seq(0.01, 1.01, by = 0.1), 
                        right = FALSE, 
                        labels = paste(seq(1, 91, by = 10), seq(10, 100, by = 10), sep = "-"))]
  by_age_sex <- agesex_dif_BSL[, lapply(.SD, function(x) round(sum(x))), by = c("age_group","sex"), .SDcols = c("Q2.5", "Q50", "Q97.5")]
  by_age_sex[Q50<0, direction := "(neg)" ]
  by_age_sex[Q50>0, direction := "(pos)" ]
  
female_top_2 <- by_age_sex[sex=="f"]  
female_results <- tail(female_top_2[order(abs(Q50))],2)
female_results[, change_ages := paste0(age_group, direction)]
male_top_2 <- by_age_sex[sex=="m"]  
male_results <- tail(male_top_2[order(abs(Q50))],2)
male_results[, change_ages := paste0(age_group, direction)]


combined_results_temp <- data.frame("bacteria" = str_remove(str_remove(plain_model_names[model_target], ".rds"), "plain_"), 
                               "total diff" = unlist(results_temp[sex=="Overall", "label"]), 
                               "female diff" = unlist(results_temp[sex=="f", "label"]),
                               "male diff" = unlist(results_temp[sex=="m", "label"]),
                               "top ages female" = paste0(female_results[1, "change_ages"], " ", female_results[2, "change_ages"]), 
                               "top ages male" = paste0(male_results[1, "change_ages"], " ", male_results[2, "change_ages"]))
combined_results <- rbind(combined_results, combined_results_temp)
print(i)

}
combined_results <- data.table(combined_results)
combined_results[bug_names, on = c(bacteria = "label"), Bacteria := i.Bacteria]
combined_results[bug_names, on = c(bacteria = "label"), Antibiotic := i.Antibiotic]
combined_results[, bacteria := NULL]
combined_results <- combined_results[,c("Bacteria", "Antibiotic", "total.diff", "female.diff", "male.diff", "top.ages.female", "top.ages.male")]

# Create a flextable
ft <- flextable(combined_results)


# Adjust table width to fit the page
ft <- autofit(ft) 
ft <- width(ft, width = dim(ft)$widths * (10 / sum(dim(ft)$widths)))  # Scale to fit page width


# Create a new Word document in landscape mode
doc <- read_docx() %>% 
  body_add_par("") %>%  # Add an empty paragraph
  body_add_flextable(ft) %>%  # Add the table
  body_end_section_landscape() # Set the section to landscape

# Save the document
print(doc, target = "table_landscape.docx")

bug1 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[1]], ".rds"), "plain_")]$Bacteria
drug1 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[1]], ".rds"), "plain_")]$Antibiotic
bug2 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[2]], ".rds"), "plain_")]$Bacteria
drug2 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[2]], ".rds"), "plain_")]$Antibiotic
bug3 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[3]], ".rds"), "plain_")]$Bacteria
drug3 <- bug_names[label ==str_remove(str_remove(plain_model_names[model_target_multi[3]], ".rds"), "plain_")]$Antibiotic


fish_plot_legend <- get_legend(fish_plot_1)
ggsave(grid.arrange(fish_plot_1 + labs(title = bquote("A     2030 only: " * italic(.(bug1)) * " " * .(drug1))) + theme(legend.position = "none"),
                    fish_plot_2+ labs(title = bquote("B     2030 only: " * italic(.(bug2)) * " " * .(drug2)))+ theme(legend.position = "none"),
                    fish_plot_3+ labs(title = bquote("C     2030 only: " * italic(.(bug3)) * " " * .(drug3)))+ theme(legend.position = "none"), 
                    fish_plot_legend, 
                    layout_matrix = rbind(c(1,1,1,1,1,4),
                                          c(2,2,2,2,2,4), 
                                          c(3,3,3,3,3,4))),
       file=paste0("plots_nw/2030_fishplot_figure", ".pdf"),
       width =10, height = 7)


agesex_results[bug_names, on=c(type = "label"), Bacteria := i.Bacteria ]
agesex_results[bug_names, on=c(type = "label"), Antibiotic := i.Antibiotic ]


FISHPLOT_SUMMARY <- ggplot(agesex_results, aes(x =age_s*100, y = Q50, colour = Bacteria)) + 
  geom_point() + 
  facet_grid(.~sex) + 
  labs(x = "age", y = "BSI R difference")


ggsave(grid.arrange(FISHPLOT_SUMMARY),
       file=paste0("plots_nw/2030_fishplot_summary", ".pdf"),
       width =10, height = 7)

#### extract some stats for text

combined_results <- data.table(combined_results)
combined_results[, total_num := tstrsplit(total.diff, " ")[[1]]] 
combined_results[total_num>0, .N]/combined_results[, .N]
combined_results[, .N]
# positive means we undesteimate in plain model
combined_results[, total_male := tstrsplit(male.diff, " ")[[1]]] 
combined_results[total_male>0, .N]/combined_results[, .N]
combined_results[, total_female := tstrsplit(female.diff, " ")[[1]]] 
combined_results[total_female>0, .N]/combined_results[, .N]

