# CALCULATING INCIDENCE
# Generate Figure 2

##### Calculating incidence #####

# from ecdc previous paper
incidence_est_data_cntrylevel <- data.table(read_csv("data/country_pathogen_age_incidence_estimates.csv")[,-1] %>%
                                              rename(gender = sex) )


# calculate rate over time, by country, age and sex. 
ggplot(incidence_est_data_cntrylevel[year %in% c(2015:2019) & pathogen == "Acinetobacter spp"], aes(x = year, y = incidence, colour = country, linetype = gender)) + 
  facet_grid(pathogen ~ age_group, scales = "free_y") + geom_line() 


#data frame for storing changing incidence in. 
storage_incidence_across <- data.frame()
# for each combo 
for (i in 1:nrow(unique(incidence_est_data_cntrylevel[,c("lowage", "gender", "pathogen")]))){
  
  #subset
  target <- unique(incidence_est_data_cntrylevel[,c("lowage", "gender", "pathogen")])[i,]
  sub_set <- incidence_est_data_cntrylevel[lowage == target$lowage & 
                                             gender == target$gender & 
                                             pathogen == target$pathogen &
                                             year %in% c(2015:2019)]
  
  # fit a quick model
  year_coef <- lm(incidence~year, sub_set)$coefficients["year"]
  #store
  storage_incidence_across <- rbind(storage_incidence_across, c(target, year_coef))
  
}
# data.table it
storage_incidence_across <- data.table(storage_incidence_across)
#plot
INCIDENCE_CHANGING <- ggplot(storage_incidence_across, aes(x = pathogen, y = lowage, fill = year)) + 
  facet_grid(.~gender) + geom_tile() +
  scale_fill_gradient2(low = "gold", high = "darkred", mid = "lightblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(y = "Age group", x = "Pathogen", fill = "Rate of change", title = "Incidence")


##### combine with population ####

#demog_proj is the resulting data needed
demog_proj <- as.data.table(demog_proj)
demog_proj[ age %in% c(0:4), lowage := 0]
demog_proj[ age %in% c(5:9), lowage := 5]
demog_proj[ age %in% c(10:14), lowage := 10]
demog_proj[ age %in% c(15:19), lowage := 15]
demog_proj[ age %in% c(20:24), lowage := 20]
demog_proj[ age %in% c(25:29), lowage := 25]
demog_proj[ age %in% c(30:34), lowage := 30]
demog_proj[ age %in% c(35:39), lowage := 35]
demog_proj[ age %in% c(40:44), lowage := 40]
demog_proj[ age %in% c(45:49), lowage := 45]
demog_proj[ age %in% c(50:54), lowage := 50]
demog_proj[ age %in% c(55:59), lowage := 55]
demog_proj[ age %in% c(60:64), lowage := 60]
demog_proj[ age %in% c(65:69), lowage := 65]
demog_proj[ age %in% c(70:74), lowage := 70]
demog_proj[ age %in% c(75:79), lowage := 75]
demog_proj[ age >=80, lowage := 80]
demog_proj[,gender := tolower(gender)]
# calculate the incidence per country per year per age group and sex. For the two projection types
# need to do this for each bug. 

# replicate for each bug
projections_by_path <- data.table(expand_grid(demog_proj, 
                                      pathogen = unique(incidence_est_data_cntrylevel$pathogen)))
# match labels
colnames(projections_by_path)[which(colnames(projections_by_path)=="reportingcountry")] <- "country"
# incidence at baseline year of 2019
base_incidence <- incidence_est_data_cntrylevel[year == 2019]
mean_rates <- base_incidence[, mean(incidence), by = c("gender", "pathogen", "lowage")]
# match the incidence across
# first means then specifics if they exist
projections_by_path[mean_rates,  on = c("gender", "pathogen", "lowage"), base_incidence := i.V1]
projections_by_path[base_incidence, on = c("country", "gender", "pathogen", "lowage"),
            base_incidence := i.incidence ]

# calculate the time since 2019
projections_by_path[,year := as.numeric(year)]
projections_by_path[, year_change := year - 2019]

# calculate the estimated incidence in the future years
# first add in the rate of change by year
projections_by_path[storage_incidence_across, on = c("gender", "lowage", "pathogen"), rate_change := i.year]
# then calculate the incidence
projections_by_path[, year_incidence := base_incidence + (year_change*rate_change)]
# some of them go negative. Can't have negative incidence so set to 0
projections_by_path[year_incidence < 0, year_incidence:=0]

# calculate the incidence based on demographic changes and the incidence that year
projections_by_path[, total_varying := (year_incidence/100000 * value)]
projections_by_path[, total_fixed:= (base_incidence/100000 * value)]


# Make a plot of the incidence over time
projections_by_path[pathogen == "Acinetobacter spp", path_label := "Acineto."]
projections_by_path[pathogen == "Enterococcus faecium", path_label := "E. faecium"]
projections_by_path[pathogen == "Escherichia coli"  , path_label := "E. coli"]
projections_by_path[pathogen == "Klebsiella pneumoniae", path_label := "K. pneumoniae"]
projections_by_path[pathogen == "Pseudomonas aeruginosa", path_label := "P. aeruginosa"]
projections_by_path[pathogen == "Staphylococcus aureus", path_label := "S. aureus"]
projections_by_path[pathogen == "Streptococcus pneumoniae", path_label := "S. pneumoniae"]
projections_by_path[pathogen == "Enterococcus faecalis", path_label := "E. faecalis"]

time_summary_fixed <- projections_by_path[projection %in% c("BSL", "LMRT"), sum(total_fixed), by = c("projection", "gender", "year", "pathogen", "path_label")]
time_summary_varying <- projections_by_path[projection %in% c("BSL", "LMRT"), sum(total_varying), by = c("projection", "gender", "year", "pathogen", "path_label")]

time_summary_fixed[, year := as.numeric(year)]
time_summary_varying[, year := as.numeric(year)]



TIME_SUMMARY_F <- ggplot(time_summary_fixed[year %in% c(2022:2030)],
                         aes(x=year, y = V1, colour = gender, linetype = projection)) + 
  facet_grid(.~path_label) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "A")+ scale_y_continuous(labels = label_comma())+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.text = element_text(face = "italic"))

TIME_SUMMARY_V <- ggplot(time_summary_varying[year %in% c(2022:2030)],
                         aes(x=year, y = V1, colour = gender, linetype = projection)) + 
  facet_grid(.~path_label) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "A")+ scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.text = element_text(face = "italic"))


time_summary_fixed$type <- "fixed"
time_summary_varying$type <- "linear"

time_combo <- rbind(time_summary_fixed, time_summary_varying)

TIME_SUMMARY_together <- ggplot(time_combo[year %in% c(2022:2030) & projection == "BSL"], 
                         aes(x=year, y = V1, colour = gender, linetype = type)) + 
  facet_grid(.~path_label) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "A", colour = "sex")+ scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.text = element_text(face = "italic"))

projections_by_path[, age_group := cut(age, breaks =seq(-1, 120, 15))] # Add age groupings
projections_by_path[age_group== "(-1,14]", age_group := "(0,14]"]
projections_by_path[,age_group := factor(age_group, levels = c(unique(projections_by_path$age_group)))]

age_summary_fixed <- projections_by_path[projection %in% c("BSL", "LMRT"), sum(total_fixed), by = c("projection", "gender", "year", "age_group")]
age_summary_varying <- projections_by_path[projection %in% c("BSL", "LMRT"), sum(total_varying), by = c("projection", "gender", "year", "age_group")]
age_summary_fixed[, year := as.numeric(year)]
age_summary_varying[, year := as.numeric(year)]

AGE_SUMMARY_F <- ggplot(age_summary_fixed[year %in% c(2022:2030)], aes(x=year, y = V1, colour = age_group, linetype = projection)) + 
  facet_grid(.~gender) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "B", colour = "age group")+ scale_y_continuous(labels = label_comma())
AGE_SUMMARY_V <- ggplot(age_summary_varying[year %in% c(2022:2030)], aes(x=year, y = V1, colour = age_group, linetype = projection)) + 
  facet_grid(.~gender) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "B", colour = "age group")+ scale_y_continuous(labels = label_comma())

age_summary_fixed$type <- "fixed"
age_summary_varying$type <- "linear"

age_combo <- rbind(age_summary_fixed, age_summary_varying)

AGE_SUMMARY_together <- ggplot(age_combo[projection == "BSL"&year %in% c(2022:2030)],
                               aes(x=year, y = V1, colour = age_group,
                                   linetype = type) ) + 
  facet_grid(.~gender) + 
  geom_line() + 
  labs(y = "Projected annual BSI Incidence", 
       title = "B", colour = "age group")+ scale_y_continuous(labels = label_comma())

ggsave(grid.arrange(TIME_SUMMARY_together, AGE_SUMMARY_together),
       file="plots_nw/FIGURE2.pdf", #file="plots_nw/INCIDENCE_comparison.pdf",
       width =10, height = 7)

ggsave(grid.arrange(TIME_SUMMARY_F, AGE_SUMMARY_F), file="plots_nw/INCIDENCE_fixed.pdf",
       width =10, height = 7)


ggsave(grid.arrange(TIME_SUMMARY_V, AGE_SUMMARY_V), file="plots_nw/INCIDENCE_varying.pdf",
       width =10, height = 7)

# match the namings to what is needed for model predictions
projections_by_path[, year_s := as.numeric(year)-2009]
projections_by_path[, age_s := age/100]
projections_by_path[, age_squared_s := age_s*age_s]
colnames(projections_by_path)[which(colnames(projections_by_path)=="gender")] <- "sex"
d_for_model <- projections_by_path[, c("country", "year_s", "age_s", "age_squared_s", "sex", "total_fixed",
                               "total_varying" ,"pathogen", "projection", "rate_change", "base_incidence", "value")] 

ggplot(storage_incidence_across, aes(x = pathogen, y = lowage, fill = year)) + 
  facet_grid(.~gender) + geom_tile() +
  scale_fill_gradient2(low = "gold", high = "darkred", mid = "lightblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

temp_plotter <- demog_proj[,sum(value), by = c("gender", "country", "year", "projection", "lowage")]
temp_plotter[, lowage := factor(lowage)]
ggplot(temp_plotter[gender == "f" & projection =="BSL"], 
       aes(x = year, y = V1, colour = lowage)) + 
  geom_line()+ 
  facet_wrap(.~country, scale = "free_y") + 
  labs(title = "Country-specific population projections", y = "population", colour = "age group")
