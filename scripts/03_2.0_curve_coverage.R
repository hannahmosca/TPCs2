#### Script info ####
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.

### flagging that i want to replace catagory names in this script with those that match the figure ###

#### 1. load packages and data ####
library(dplyr)
library(tidyverse)
library(here)
library(ggforce)

#read in the data
curves <- read.csv(here('processed-data', 'FishTherm.csv')) %>%
  select(-(X))

#### 2. normalize all of the datasets so can work with scaled values for sorting ####
data_scaled <- curves %>%
  select(curve_ID, test_temp, response_value, Trait.Group, response_unit) %>%
  group_by(curve_ID, test_temp) %>%
  mutate(mean_response = mean(response_value, na.rm = TRUE)) %>%  # mean at each temp, handles ind response curves
  ungroup() %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = mean_response / max(mean_response, na.rm = TRUE)) %>%  # scale within curve
  ungroup() %>%
  distinct(curve_ID, test_temp, Trait.Group, mean_response, response_scaled, response_unit)

#### 3. Catagorize datasets that have coverage to estimate topt ####  
#optimum: curves that have a max response sandwiched by responses that are less on both sides ...ie go up and come down

# The values rise before the peak.
# The values fall after the peak.
# The peak is not at the edges - ie the first point.

optimum_check <- data_scaled %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  summarize(peak_pos = which.max(response_scaled),
            prop_up = mean(diff(response_scaled[1:peak_pos]) >= 0), # proportion of response changes before the peak that are increasing, ie is mostly increasing?
            prop_down = mean(diff(response_scaled[peak_pos:n()]) <= 0),
            has_optimum = peak_pos > 1 & peak_pos < n() & prop_up >= 0.50 & prop_down >= 0.50)

optimum_curves2 <- optimum_check %>%
  filter(has_optimum == TRUE)
optimum_curves2_list <- c(optimum_curves2$curve_ID)

# #in optimum_2 curves but not in opt_list from first script
# setdiff(optimum_curves2_list, topt_list_01) 
# difference1 <- setdiff(optimum_curves2_list, topt_list_01)
# 
# # see notes app
# #in topt list from first script but not in optimum_2
# setdiff(topt_list_01, optimum_curves2_list) # 4
# difference2 <- setdiff(topt_list_01, optimum_curves2_list)
# #446 is should be in the just increasing kind
# #437, 440, 455 should be in optima estimatable // topt list

# vis check #
responses <- data_scaled %>%
  select(curve_ID, Trait.Group, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(Trait.Group, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
ggplot() +
  geom_point(data = data_scaled %>%
               filter(curve_ID %in% difference2),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))

## manual/visual reassignment for topt ##

# had to manually reassign 29 tpcs in this part #
manual_reass_list_opt_add <- c(437,440,455) #have opt
manual_reass_list_opt_remove <- c(446, 12, 19, 32, 74, 77, 90, 111, 136, 188, 209, 245, 263, 293, 296, 297, 319, 342, 357, 358, 360, 378, 384, 411, 422, 454, 60) #no opt

#updating list
optimum_curves_updatedlist <- c(optimum_curves2_list, manual_reass_list_opt_add)
optimum_curves_updatedlist <- optimum_curves_updatedlist[!optimum_curves_updatedlist %in% manual_reass_list_opt_remove]
data_scaled <- data_scaled %>%
  mutate(has_optimum = ifelse(curve_ID %in% optimum_curves_updatedlist, TRUE, FALSE))

#### 4 Further categorize datasets with enough coverage to estimate topt ####

### 4.1 Bounded datasets with coverage to estimate topt ###

#Made the closeness to 0 further for this criteria because overall more data coverage ie more confidence in estimating parameters

opt <- data_scaled %>%
  filter(has_optimum == TRUE) %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.25, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.25, "yes", "no")
  ) %>%
  ungroup()
### 4.2 Left bounded/ ctim with topt
full_rise_with_opt <- opt %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "no")

full_rise_with_opt_list <- unique(full_rise_with_opt$curve_ID)

##vis
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "yes") %>%
               filter(right_bound == "no"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 6,
                      labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.3 right bounded/ ctmax with topt
full_decline_with_opt <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "yes")
full_decline_with_opt_list <- unique(full_decline_with_opt$curve_ID)

##vis
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "no") %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 2,
                      labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.4 full tpc
full_curve <- opt %>%
  filter(right_bound == "yes") %>%
  filter(left_bound == "yes")
full_curve_list <- unique(full_curve$curve_ID) #20

##vis
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "yes") %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 3,
                      labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.5. optimum only ###
optimum_only <- opt %>%
  filter(right_bound == "no") %>%
  filter(left_bound == "no")
optimum_only_list <- unique(optimum_only$curve_ID)

#vis
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "no") %>%
               filter(right_bound == "no"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 5, nrow = 5, page = 6,
                      labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

#### 5 Categorize datasets that do not have coverage to estimate topt ####
non_opt <- data_scaled %>%
  filter(has_optimum == FALSE)
non_opt_list <- unique(non_opt$curve_ID)

### 5.1 categorize datasets that are bounded by a response close to 0, ie coverage to estimate ctmin or ctmax ###

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

## CTMIN / left bound only datasets ##
partial_rise_with_min <- non_opt %>%
  filter(left_bound == "yes")

#vis check
ggplot() +
  geom_point(data = non_opt %>%
               filter(left_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 2,
                      labeller = labeller(curve_ID = curve_labels))
partial_rise_with_min_list <- c(unique(partial_rise_with_min$curve_ID)) #no manual reassignment 

## CTMAX / right bound only datasets
partial_decline_with_max <- non_opt %>%
  filter(right_bound == "yes")

#vis check
ggplot() +
  geom_point(data = non_opt %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
partial_decline_with_max_list <- c(unique(partial_decline_with_max$curve_ID)) #no manual reassignment

### 5.2 categorize datasets that are not bounded by a response close to 0, ie not enough coverage to estimate ctmin or ctmax, but potentially eh ###
unbounded_NO <- non_opt %>% #unbounded, no optima
  filter(left_bound == "no") %>%
  filter(right_bound == "no")

unbounded_NO_list <- c(unique(unbounded_NO$curve_ID))

unbounded_curve_direction <- data_scaled %>%
  group_by(curve_ID) %>%
  filter(curve_ID %in% unbounded_NO_list) %>%
  mutate(slope = lm(response_scaled ~ test_temp)$coefficients[2],
         direction = case_when(slope > 0 ~ "increasing",
                               slope < 0 ~ "decreasing",
                               TRUE ~ "flat"))
## unbounded, increasing ##
increasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "increasing")
partial_rise <- c(unique(increasing_unbounded$curve_ID)) #150

#vis check
ggplot() +
  geom_point(data = increasing_unbounded,
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 9 ,
                      labeller = labeller(curve_ID = curve_labels))

#155, 344, 353, 354 -- move to flat, ie probably irregular (haven't done this yet!)
manual_reass_list_remove_PR <- c(155, 344, 353, 354)
partial_rise_list <- partial_rise[!partial_rise %in% manual_reass_list_remove_PR]

## unbounded, decreasing ##
decreasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "decreasing")
partial_decline_list <- c(unique(decreasing_unbounded$curve_ID)) #44
#vis check
ggplot() +
  geom_point(data = decreasing_unbounded,
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 3,
                      labeller = labeller(curve_ID = curve_labels))
#197, 209, 356, 358, 378 - -- move to flat, ie probably irregular 
manual_reass_list_remove_PD <- c(197, 209, 356, 358, 378)
partial_decline_list <- partial_decline_list[!partial_decline_list %in% manual_reass_list_remove_PD]

## flat 
flat_unbounded <- unbounded_curve_direction %>%
  filter(direction == "flat") #63 - survival curve with 100% across all temps, move to irregular 
ggplot() +
  geom_point(data = flat_unbounded,
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
irregular <- unique(flat_unbounded$curve_ID)

irregular_list <- c(irregular, manual_reass_list_remove_PD, manual_reass_list_remove_PR)

#view 384?


#### putting it together ####
# now i have these vectors that hold all of the curves sorted
all <- c(full_curve_list, optimum_only_list, full_rise_with_opt_list, full_decline_with_opt_list, partial_rise_with_min_list, partial_decline_with_max_list, partial_rise_list, partial_decline_list, irregular_list)

length(unique(all))
curve_ids <- c(unique(responses$curve_ID))
setdiff(curve_ids, all)
setdiff(all, curve_ids)


distinct_curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(dataset_type = case_when(
    curve_ID %in% full_curve_list ~ "full_curve",
    curve_ID %in% optimum_only_list ~ "topt",
    curve_ID %in% full_rise_with_opt_list ~ "left_bound_withopt",
    curve_ID %in% full_decline_with_opt_list ~ "right_bound_withopt",
    curve_ID %in% partial_rise_with_min_list ~ "left_bound",
    curve_ID %in% partial_decline_with_max_list ~ "right_bound",
    curve_ID %in% partial_rise_list ~ "unbounded_increasing",
    curve_ID %in% partial_decline_list ~ "unbounded_decreasing",
    curve_ID %in% irregular_list ~ "irregular",
    TRUE ~ NA_character_
  ))

