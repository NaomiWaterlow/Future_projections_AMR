########################################################## 
####### Explore population size changes across Europe ####
####### Developers: N Waterlow & G Knight ################ 
##########################################################

# This script analyzes Eurostat population projections from 2019 and 2023.  
# It loads and processes the data, extracts key demographic variables,  
# and reshapes it for visualization. The script uses the tidyverse package  
# for data manipulation and applies a color palette.  

# load libraries 
library(tidyverse)
library(RColorBrewer)
library(here)
library(patchwork)
theme_set(theme_bw(base_size = 11))
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

## Load in data from https://ec.europa.eu/eurostat/web/population-demography/population-projections/database
setwd(here())
data19 <- read_csv("data/proj_19np.csv") # old
data23 <- read_csv("data/proj_23np.csv") # updated during project

# key = projection,unit,sex,age,geo\time
d19 <- data19 %>% separate(key, c("projection", "unit","sex","age","geo_time"), sep = ",")
d <- data23 %>% separate(key, c("projection", "sex","age","unit","geo_time"), sep = ",")

# What is in there? 
unique(d$projection) # 6 projections 
unique(d$unit) # person
unique(d$sex) # F M and T (total)
unique(d$age) # Total and 1-99 with GE100 (greater or equal to 100, less than 1)
unique(d$geo_time) # Reporting countries: no UK... Need to check list of data available

# Overview plot
d_overview <- d %>% filter(sex == "T", age == "TOTAL", geo_time == "EU27_2020") %>% 
  dplyr::select(-c("unit")) %>% 
  pivot_longer(`2100`:`2022`, names_to = "year") %>%
  filter(year < 2052)

d19_overview <- d19 %>% filter(sex == "T", age == "TOTAL", geo_time == "EU27_2020") %>% 
  dplyr::select(-c("unit")) %>% 
  pivot_longer(`2100`:`2019`, names_to = "year") %>%
  filter(year < 2052)

d_overview_all <- rbind(d_overview %>% mutate(dataset = 23), d19_overview %>% mutate(dataset = 19))

#### compare projections across Europe visually - only slight change 
# ggplot(d_overview_all, aes(x=year, y = value, group = interaction(dataset,projection))) +
#   geom_line(aes(col = projection, lty = factor(dataset))) +
#   scale_y_continuous("Total population", lim = c(0,max(d_overview$value))) +
#   scale_x_discrete("Year", breaks = c(2019, seq(2030,2050,10))) +
#   scale_color_discrete("Projection") +
#   facet_wrap(~projection)
# ggsave("plots/europe_all_19vs23.pdf")
# 
# ggplot(d_overview, aes(x=year, y = value, group = projection)) + 
#   geom_line(aes(col = projection)) + 
#   scale_y_continuous("Total population", lim = c(0,max(d_overview$value))) + 
#   scale_x_discrete("Year", breaks = c(2019, seq(2030,2050,10))) + 
#   scale_color_discrete("Projection")
# ggsave("plots/europe_all_projections.pdf")

# Overview by age plot
d_overview_age <- d %>% filter(sex == "T",!age %in% c("TOTAL"),  geo_time == "EU27_2020") %>% 
  dplyr::select(-c("unit")) %>% 
  pivot_longer(`2100`:`2022`, names_to = "year") %>%
  filter(year < 2052) %>% mutate(dataset = 23)

d_overview_age19 <- d19 %>% filter(sex == "T",!age %in% c("TOTAL"),  geo_time == "EU27_2020") %>% 
  dplyr::select(-c("unit")) %>% 
  pivot_longer(`2100`:`2019`, names_to = "year") %>%
  filter(year < 2052) %>% mutate(dataset = 19)

d_overview_age_all <- rbind(d_overview_age, d_overview_age19)

# Sort out age
d_overview_age_all$age.n <- sub(".*Y", "", d_overview_age_all$age) 
d_overview_age_all[which(d_overview_age_all$age.n == "_GE100"),"age.n"] <- "100"  
d_overview_age_all[which(d_overview_age_all$age.n == "_LT1"),"age.n"] <- "0" 
d_overview_age_all <- d_overview_age_all %>% filter(!age.n %in% c("_GE100","_GE65","_GE75","_GE80","_LT15","_LT20","15-64","15-74","20-64"))
d_overview_age_all$age.n <- as.numeric(d_overview_age_all$age.n)

# ggplot(d_overview_age_all %>% filter(dataset == 23), 
#        aes(x=year, y = value, group = age.n)) + 
#   geom_line(aes(col = age.n)) + 
#   facet_wrap(~projection) + 
#   scale_y_continuous("Total population") + 
#   scale_x_discrete("Year", breaks = c(2019, seq(2030,2050,10))) + 
#   scale_colour_gradientn("Age", colours = myPalette(100), limits=c(1, 100))
# ggsave("plots/europe_all_age.pdf")
# 
# ggplot(d_overview_age_all %>% filter(dataset == 23) %>% filter(age.n %in% c(5,25,40,60,75,90)), aes(x=year, y = value, group = age.n)) + 
#   geom_line(aes(col = age.n)) + 
#   facet_wrap(~projection) + 
#   scale_y_continuous("Total population") + 
#   scale_x_discrete("Year", breaks = c(2019, seq(2030,2050,10))) + 
#   scale_colour_gradientn("Age", colours = myPalette(100), limits=c(1, 100))
# ggsave("plots/europe_all_age_egs_SUPPFIG.pdf")
# 
# ggplot(d_overview_age_all %>% filter(age.n %in% c(5,25,40,60,75,90)), 
#        aes(x=year, y = value, group = interaction(dataset,age.n))) + 
#   geom_line(aes(col = age.n, lty = factor(dataset))) + 
#   facet_wrap(~projection) + 
#   scale_y_continuous("Total population") + 
#   scale_x_discrete("Year", breaks = c(2019, seq(2030,2050,10))) + 
#   scale_colour_gradientn("Age", colours = myPalette(100), limits=c(1, 100))
# ggsave("plots/europe_all_age_egs_19vs23.pdf")


# Filter for what we need: have three sex classes
d <- d %>% filter(!age %in% c("TOTAL"), 
                  !geo_time %in% c("EU27_2020","EA19", "EA20"),
                  !projection %in% c("NMIGR")) %>% dplyr::select(-c("unit"))

# Sort out age
d$age.n <- sub(".*Y", "", d$age) 
d[which(d$age.n == "_GE100"),"age.n"] <- "100"  
d[which(d$age.n == "_LT1"),"age.n"] <- "0"
d <- d %>% filter(!age.n %in% c("_GE100","_GE65","_GE75","_GE80","_LT15","_LT20","15-64","15-74","20-64"))
d$age.n <- as.numeric(d$age.n)

# Pivot longer to have time downwards and remove age column
d_long <- d %>% select(-c("age")) %>% 
  pivot_longer(`2100`:`2022`, names_to = "year") %>%
  group_by(geo_time) %>% 
  mutate(ini_value = last(value), 
         big_country = ifelse(ini_value > 3e+05, 1, 0)) %>%
  filter(year < 2052)

# ggplot(d_long %>% filter(geo_time == "DE",age.n > 70), aes(x=year, y = value, 
#                                                 group = interaction(geo_time, sex, projection))) + 
#   geom_line(aes(col = sex, linetype = projection)) + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~age.n, scales = "free") + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   geom_vline(xintercept = "2050") + 
#   scale_color_discrete("Country")
# 
# 
# ggplot(d_long %>% filter(age.n == 32), aes(x=year, y = value, 
#                              group = interaction(geo_time, sex, projection))) + 
#   geom_line(aes(col = sex, linetype = projection)) + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~big_country, scales = "free", ncol = 1) + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   geom_vline(xintercept = "2050") + 
#   scale_color_discrete("Country")

# exploring drop in fertilty rate impact: across all countries
# ggplot(d_long %>% filter(age.n == 0, projection == "LFRT"), aes(x=year, y = value, 
#                                            group = interaction(geo_time, sex, projection))) + 
#   geom_line(aes(col = sex, linetype = projection)) + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~big_country, scales = "free", ncol = 1) + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   geom_vline(xintercept = "2050") + 
#   scale_color_discrete("Country")
# 
# 
# ggplot(d_long %>% filter(geo_time == "DE", age.n %in% seq(0,100,10)), 
#        aes(x=year, y = value, group = interaction(age.n, sex, projection))) + 
#   geom_line(aes(col = age.n)) + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~projection, scales = "free") + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   scale_colour_gradientn(colours = myPalette(100), limits=c(1, 100))


#### For model-based version don't want to do this.
# Group by 5yrs
d_long_20yr <- d_long %>% mutate(age_groupn = cut(age.n, breaks = seq(-1,120,5))) %>% 
  group_by(projection, year, age_groupn) %>% summarise(totals = sum(value))
d_long_20yr$age_groupn <- as.character(d_long_20yr$age_groupn)
d_long_20yr <- d_long_20yr %>% mutate(age_group = ifelse(age_groupn %in% c("(79,84]", "(84,89]", "(89,94]", "(94,99]", "(99,104]", "(104,109]", 
                                                            "(109,114]", "(114,119]"),"(80,120]",age_groupn)) %>%
  group_by(age_group, projection, year) %>% summarise(total = sum(totals)) 

d_long_20yr$age_group <- factor(d_long_20yr$age_group, levels = c("(-1,4]", "(4,9]","(9,14]","(14,19]", "(19,24]", 
                                                          "(24,29]", "(29,34]", "(34,39]", "(39,44]",  "(44,49]", 
                                                          "(49,54]", "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", 
                                                          "(80,120]"))


# geu <- ggplot(d_long_20yr, 
#        aes(x=year, y = total, group = interaction(age_group, projection))) + 
#   geom_bar(stat = "identity",aes(fill = age_group), position = "fill") + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~projection, scales = "free") + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   scale_fill_discrete("Age group") + 
#   scale_y_continuous("Proportion of total European population")
# ggsave("plots/pop_size_changes_proportions.pdf")


####### Which one for sensitivity analysis? 
d_long_20yr %>% filter(year == 2050, age_group == "(80,120]")
#LMRT has greatest proportion older individuals at 2050 so use this, tho slightly lower numbers (not the lowest)

## Gender changes 
dg <- d_long %>% filter(!sex == "T", projection == "BSL") %>% 
  group_by(year,sex) %>% summarise(total = sum(value))
#plot
ggplot(dg, aes(x=year, y = total, group = sex)) + geom_line(aes(col = sex)) + 
  scale_y_continuous(lim = c(0,max(dg$total)+10000))
# look at BSL type
dga <- d_long %>% filter(projection == "BSL") %>% 
  group_by(age.n, year,sex) %>% summarise(total = sum(value))
# 5 year bands
d_long_20yr <- dga %>% mutate(age_group = cut(age.n, breaks = seq(-1,120,5))) %>% 
  group_by(sex, year, age_group) %>% summarise(totals = sum(total))

# ggplot(d_long_20yr, 
#        aes(x=year, y = totals, group = interaction(age_group, sex))) + 
#   geom_bar(stat = "identity",aes(fill = age_group), position = "stack") + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~sex, scales = "free") + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   scale_fill_discrete("Age group") + 
#   scale_y_continuous("Proportion of total European population")
# 
# ggplot(d_long_20yr, 
#        aes(x=year, y = totals, group = interaction(age_group, sex))) + 
#   geom_bar(stat = "identity",aes(fill = age_group), position = "fill") + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   facet_wrap(~sex, scales = "free") + 
#   scale_x_discrete("Year",breaks = c(2019, seq(2030, 2100, 10))) + 
#   scale_fill_discrete("Age group") + 
#   scale_y_continuous("Proportion of total European population")

# Percentage change bigger for men than women
d_long_20yr %>% filter(year %in% c(2022,2050), age_group == "(74,79]") %>% 
  pivot_wider(values_from = totals, names_from = year) %>% group_by(sex) %>%
  mutate(perc_change = 100 * c(`2050` - `2022`)/`2022`)


## Store for use in future burden projections
d_long <- rename(d_long,reportingcountry = geo_time)
d_long <- rename(d_long,age = age.n)
d_long <- rename(d_long,gender = sex)
# remove Total
d_long <- d_long %>% filter(gender != "T")
# write.csv(d_long, "data/age_projections.csv")


### UK data

# There is new 2025 data
# use principal projection, and old age structure
uk_data_orig <- read_csv("data/uk_ons_principal.csv")
uk_data <- uk_data_orig %>% pivot_longer(cols = `2022`:`2122`) %>% 
  filter(!is.na(Age), !Age == "Ages", !Age == "all_age") %>% 
  mutate(age_group = recode(Age, "0 - 4" = "(-1,4]",  "5 - 9" = "(4,9]", "10 - 14" = "(9,14]", "15 - 19" = "(14,19]", "20 - 24" = "(19,24]", "25 - 29" = "(24,29]",
                             "30 - 34" = "(29,34]", "35 - 39" = "(34,39]",  "40 - 44" = "(39,44]", "45 - 49" = "(44,49]", "50 - 54" = "(49,54]", "55 - 59" = "(54,59]",
                            "60 - 64" = "(59,64]", "65 - 69" = "(64,69]",
                            "70 - 74" = "(69,74]", "75 - 79" = "(74,79]", "80 - 84" = "(80,120]", "85 - 89" = "(80,120]",
                             "90 - 94" = "(80,120]",  "95 - 99" = "(80,120]","100 - 104" = "(80,120]","105 and over" = "(80,120]")) %>% 
  rename(year = name, gender = Sex, age = Age) 
           
uk_data$year <- as.numeric(uk_data$year)          

uk_data$age_group <- factor(uk_data$age_group, levels = c("(-1,4]", "(4,9]","(9,14]","(14,19]", "(19,24]", 
                                                          "(24,29]", "(29,34]", "(34,39]", "(39,44]",  "(44,49]", 
                                                          "(49,54]", "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", 
                                                          "(80,120]"))

# need to add up the population in the top age group!
uk_data <- data.table(uk_data)
uk_data <- uk_data[, sum(value), by = c("gender", "year", "age_group")]
write.csv(uk_data, "data/uk_data_projections_principal.csv")

# use principcal projection, and old age structure
uk_data_orig <- read_csv("data/uk_ons_oldage.csv")
uk_data <- uk_data_orig %>% pivot_longer(cols = `2022`:`2122`) %>% 
  filter(!is.na(Age), !Age == "Ages", !Age == "all_age") %>% 
  mutate(age_group = recode(Age, "0 - 4" = "(-1,4]",  "5 - 9" = "(4,9]", "10 - 14" = "(9,14]", "15 - 19" = "(14,19]", "20 - 24" = "(19,24]", "25 - 29" = "(24,29]",
                            "30 - 34" = "(29,34]", "35 - 39" = "(34,39]",  "40 - 44" = "(39,44]", "45 - 49" = "(44,49]", "50 - 54" = "(49,54]", "55 - 59" = "(54,59]",
                            "60 - 64" = "(59,64]", "65 - 69" = "(64,69]",
                            "70 - 74" = "(69,74]", "75 - 79" = "(74,79]", "80 - 84" = "(80,120]", "85 - 89" = "(80,120]",
                            "90 - 94" = "(80,120]",  "95 - 99" = "(80,120]","100 - 104" = "(80,120]","105 and over" = "(80,120]")) %>% 
  rename(year = name, gender = Sex, age = Age) 

uk_data$year <- as.numeric(uk_data$year)          

uk_data$age_group <- factor(uk_data$age_group, levels = c("(-1,4]", "(4,9]","(9,14]","(14,19]", "(19,24]", 
                                                          "(24,29]", "(29,34]", "(34,39]", "(39,44]",  "(44,49]", 
                                                          "(49,54]", "(54,59]", "(59,64]", "(64,69]", "(69,74]", "(74,79]", 
                                                          "(80,120]"))

# need to add up the population in the top age group!
uk_data <- data.table(uk_data)
uk_data <- uk_data[, sum(value), by = c("gender", "year", "age_group")]
write.csv(uk_data, "data/uk_data_projections_oldage.csv")


# guk <- ggplot(uk_data %>% filter(year < 2051), 
#        aes(x=year, y = value, group = interaction(age_group))) + 
#   geom_bar(stat = "identity",aes(fill = age_group), position = "fill") + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   scale_x_continuous("Year") + 
#   scale_fill_discrete("Age group") + 
#   scale_y_continuous("Proportion of total UK population")
# ggsave("plots/pop_size_changes_proportions_UK.pdf")
# 
# geu + guk + plot_layout(guides = "collect")
# ggsave("plots/pop_size_changes_proportions_SUPPFIG.png")

#### GROUP TOGETHER FOR FUTURE BURDEN CALCULATIONS
################  Demographic data  ################

# For modelling don't want age groups. 
# So assume in the UK that the 5 year age bands are split equally
# also the projections only go up to 100, which I think is okay as long as we note it.
# Also, Remember we are EXCLUDING 0-year olds. What does this mean for how we display our results?

### Projections - start 
# EU
demog_proj_eu <- read_csv("data/age_projections.csv")[,-1] %>% 
  filter(projection %in% c("BSL","LMRT"), year < 2051) %>% # use baseline 
  rename(country = reportingcountry) %>% # rename
 # mutate(age_group = cut(age, breaks = seq(-1,120,5))) %>% # Add age groupings 
  filter(age > 0) %>% # remove (-5,0] age group
  select(gender, country, age, year, value, projection)
#demog_proj_eu$age_group <- as.character(demog_proj_eu$age_group)
#demog_proj_eu[which(demog_proj_eu$age > 79),"age_group"] <- "(80,120]"
demog_proj_eu$gender <- tolower(demog_proj_eu$gender)
min(demog_proj_eu$year) # 2022
max(demog_proj_eu$year) # 2050 

# UK
uk_data <- read_csv("data/uk_data_projections_principal.csv")[,-1] %>% mutate(country = "UK") %>%
  filter(year < 2051 & gender != "t") 
uk_data <- uk_data %>% mutate(projection = "BSL")
uk_data <- uk_data %>% filter(year > 2021)
# need the age column - so splitting equally across age
uk_data <- data.table(uk_data)
uk_data[, age_low := as.numeric(sub("^\\(([-+]?[0-9]+),.*$", "\\1", age_group))]
uk_data[, age_high := as.numeric(sub(".*,(-?\\d+)]", "\\1", age_group))]

# manually switch 80-120 to 20, as assuming split between 80-100
uk_data[age_low == 80, age_high :=100]
# how many years in age group
uk_data[, divisor := age_high - age_low]
# Need the lower age to not be assigned
uk_data[, age_low := age_low+1]

uk_data <- uk_data[, .(
  gender = gender,
  year = year,
  value = V1,
  age_group = age_group,
  country = country,
  projection = projection,
  age_low = age_low,
  age_high = age_high,
  divisor = divisor,
  age = seq(age_low[1], age_high[1])  # Generate sequence for each row
), by = .I]

uk_data[, value := (value/divisor) ]
uk_data <- uk_data[age !=0]


#match demog_proj data.frame from other countries
uk_data_temp <- uk_data[, c("gender", "country", "age", "year", "value", "projection")]



# UK
uk_data <- read_csv("data/uk_data_projections_oldage.csv")[,-1] %>% mutate(country = "UK") %>%
  filter(year < 2051 & gender != "t") 
uk_data <- uk_data %>% mutate(projection = "BSL")
uk_data <- uk_data %>% filter(year > 2021)
# need the age column - so splitting equally across age
uk_data <- data.table(uk_data)
uk_data[, age_low := as.numeric(sub("^\\(([-+]?[0-9]+),.*$", "\\1", age_group))]
uk_data[, age_high := as.numeric(sub(".*,(-?\\d+)]", "\\1", age_group))]

# manually switch 80-120 to 20, as assuming split between 80-100
uk_data[age_low == 80, age_high :=100]
# how many years in age group
uk_data[, divisor := age_high - age_low]
# Need the lower age to not be assigned
uk_data[, age_low := age_low+1]

uk_data <- uk_data[, .(
  gender = gender,
  year = year,
  value = V1,
  age_group = age_group,
  country = country,
  projection = projection,
  age_low = age_low,
  age_high = age_high,
  divisor = divisor,
  age = seq(age_low[1], age_high[1])  # Generate sequence for each row
), by = .I]

uk_data[, value := (value/divisor) ]
uk_data <- uk_data[age !=0]


#match demog_proj data.frame from other countries
uk_data <- uk_data[, c("gender", "country", "age", "year", "value", "projection")]

uk_data1 <- uk_data %>% mutate(projection = "LMRT") # ONLY one scenario for UK predictions so replicate and use this for sensitivity 
# Join for total
demog_proj <- rbind(demog_proj_eu, uk_data_temp, uk_data1)
demog_proj <- demog_proj %>% filter(gender != "T")
demog_proj <- data.table(demog_proj)
demog_proj[gender == "Females", gender := "f"]
demog_proj[gender == "Males", gender := "m"]
demog_proj[gender == "females", gender := "f"]
demog_proj[gender == "males", gender := "m"]

write.csv(demog_proj, "output/demog_proj_all.csv")

# ## Summarise to give country totals
# data_popsize_cntry_totals <- demog_proj %>% filter(gender == "t") %>% 
#   group_by(country, year, projection) %>% dplyr::summarise(total = sum(value)) %>% 
#   select(country, total, year,projection) %>% filter(year > 2018) 
# 
# ggplot(data_popsize_cntry_totals, aes(x=year, y = total, group = interaction(projection,country))) +
#   geom_line(aes(col = country, lty = projection)) 
# 
# write.csv(data_popsize_cntry_totals,"output/data_popsize_cntry_totals.csv")
# 
# Filter for year 2030 and sum across all countries
filtered_data<- demog_proj[year %in% c(2030,2050)]
filtered_data <- filtered_data[,.(value = sum(value)), by = .(age, gender,year, projection)]

# Adjust values: Male = Negative, Female = Positive
filtered_data[gender == "m", value :=  -value]

# Create population pyramid
POP_PYRAMID <- ggplot(filtered_data, aes(x = age, y = value, fill = projection)) +
  geom_col(position = "identity", alpha = 0.5) +  # Overlapping bars with transparency
  coord_flip() +  # Flip axes for pyramid layout
  scale_y_continuous(labels = abs) +  # Ensure positive labels on both sides
  labs(
    title = "Population Pyramid for 2030 and 2050",
    x = "Age",
    y = "Population",
    fill = "Projection"
  ) +
  theme_minimal() +
  facet_wrap(year~.)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_text(size = 10))  # Improve readabil

ggsave(filename = "plots_nw/pop_pyramid.pdf",POP_PYRAMID, width =20, height = 10)

