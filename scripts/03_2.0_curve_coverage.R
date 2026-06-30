#### ============================================================
#### Script info
#### ============================================================
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.
#### ============================================================

#### 1. load packages and data ####
library(dplyr)
library(tidyverse)
library(here)
library(ggforce)

#read in the data
curves <- read.csv(here('processed-data', 'FishTherm.csv')) %>%
  select(-(X))

#### 02. normalize all of the datasets so can work with scaled values for sorting ####
data_scaled <- curves %>%
  select(curve_ID, test_temp, response_value, Trait.Group, response_unit) %>%
  group_by(curve_ID, test_temp) %>%
  mutate(mean_response = mean(response_value, na.rm = TRUE)) %>%  # mean at each temp, handles ind response curves
  ungroup() %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = mean_response / max(mean_response, na.rm = TRUE)) %>%  # scale within curve
  ungroup() %>%
  distinct(curve_ID, test_temp, Trait.Group, mean_response, response_scaled, response_unit)

#### 03. add columns for datasets that are left bounded, right bounded, and reach an optimum ####

#optimum: curves that have a max response sandwiched by responses that are less on both sides ...ie go up and come down

# The values rise before the peak.
# The values fall after the peak.
# The peak is not at the edges - ie the first point.

optimum_check <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>% #order data by temp
  summarize(
    peak_pos = which.max(response_scaled), #finds position of max response/opt
    has_optimum = peak_pos > 1 & peak_pos < n() & #peak is not the first or last point
      all(diff(response_scaled[1:peak_pos]) >= 0) &  #response values rise up to peak
      all(diff(response_scaled[peak_pos:n()]) <= 0)  #respose values fall below peak
  )
data_scaled <- left_join(data_scaled, optimum_check, by = "curve_ID")

optimum_curves <- optimum_check %>%
  filter(has_optimum == TRUE)

optimum_check <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  summarize(peak_pos = which.max(response_scaled),
            prop_up = mean(diff(response_scaled[1:peak_pos]) >= 0), # proportion of response changes before the peak that are increasing, ie is mostly increasing?
            prop_down = mean(diff(response_scaled[peak_pos:n()]) <= 0),
            has_optimum = peak_pos > 1 & peak_pos < n() & prop_up >= 0.75 & prop_down >= 0.75)

optimum_curves2 <- optimum_check %>%
  filter(has_optimum == TRUE)

##make lists and compare
optimum_curves1_list <- c(optimum_curves$curve_ID)
optimum_curves2_list <- c(optimum_curves2$curve_ID)

# In optimum_curves1 but not optimum_curves2
setdiff(optimum_curves1_list, optimum_curves2_list) #all curves in 1 are in 2 check

# In optimum_curves2 but not optimum_curves1
setdiff(optimum_curves2_list, optimum_curves1_list) #23
difference <- setdiff(optimum_curves2_list, optimum_curves1_list)

#in optimum_2 curves but not in opt_list from first script  ##interesting, 6 curves that should be in opt list, wonder where they were?
setdiff(optimum_curves2_list, topt_list_01) 
difference <- setdiff(optimum_curves2_list, topt_list_01)

setdiff(topt_list_01, optimum_curves2_list) 
difference <- setdiff(topt_list_01, optimum_curves2_list)

#### 04. vis testing station ####
responses <- data_scaled %>%
  select(curve_ID, Trait.Group, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(Trait.Group, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
ggplot() +
  geom_point(data = data_scaled %>%
               filter(curve_ID %in% difference),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))

####05. Handling datasets without an optimum ####
non_opt <- data_scaled %>%
  filter(has_optimum == FALSE)
non_opt_list <- unique(non_opt$curve_ID)

# Compute left and right bounds
non_opt <- non_opt %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.10, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.10, "yes", "no")
  ) %>%
  ungroup()

#CTMIN only datasets
ctmin <- non_opt %>%
  filter(left_bound == "yes") #criteria 

ggplot() +
  geom_point(data = non_opt %>%
               filter(left_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
