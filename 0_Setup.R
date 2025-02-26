# SETUP
library(plyr)
library(dplyr)
library(tidyverse)
library(broom)
library(patchwork)
library(data.table)
library(brms)
library(boot)
library(gridExtra)
library(scales)
library(flextable)
library(officer)
library(cowplot)
theme_set(theme_bw())


# LOAD DATA
### This is data from the ECDC - please apply to TESSY for access
amr_data_long_orig <- read_csv("data/data_cleaned_fortrends.csv") 
### REMOVE age 0 and rename
data_use_ages <- amr_data_long_orig %>% filter(year > 2009, year < 2020) %>%
  dplyr::rename(sex = gender) %>% 
  # mutate(age_group = cut(age, breaks = seq(-1,120,5))) %>% # Add age groupings
  filter(age != 0)  # remove age 0 as per previous analysis

# work out percentage resistance by age group
data_use_ages <- data_use_ages %>%
  dplyr::group_by(year, country, sex, combo, age) %>% 
  dplyr::summarise(susn = sum(sus), resn = sum(res), proportion = resn / (susn + resn))

# format age table for use in models
data_use_ages <- data.table(data_use_ages)
data_use_ages[,year_s:= year-2009]# make better format for model fitting
data_use_ages[, total := susn + resn]
data_use_ages[, age_s := age/100]
data_use_ages[, age_squared_s:= age_s*age_s]
data_use_ages <- data_use_ages[!grepl("multi", combo)]

# Group over ages and sex
data_use1 <- amr_data_long_orig %>% filter(year > 2009, year < 2020) %>%
  filter(age > 0) %>% # remove (-5,0] age group
  dplyr::group_by(combo, pathogen, country, year) %>% 
  dplyr::summarise(sust = sum(sus), rest = sum(res), total = sust + rest, 
                   proportion = rest / total) %>%
  mutate(comboctry = paste0(combo, country)) 

# format non-age table for use in models
data_use1 <- data.table(data_use1)
data_use1[,year_s:= year-2009]# make better format for model fitting
data_use1 <- data_use1[!grepl("multi", combo)]

# Check all folders exist
dir.create(file.path(here(), "output"))
dir.create(file.path(here(), "brms_fits"))
dir.create(file.path(here(), "plots_nw"))



