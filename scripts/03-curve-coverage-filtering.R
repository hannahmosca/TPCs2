#### Script info ####
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full_curve, optimum_only, full_rise_with_opt, full_decline_with_opt, partial_rise_with_min, partial_decline_with_max, partial_rise, partial_decline) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.

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
# ggplot() +
#   geom_point(data = opt %>%
#                filter(left_bound == "yes") %>%
#                filter(right_bound == "no"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 6,
#                       labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.3 right bounded/ ctmax with topt
full_decline_with_opt <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "yes")
full_decline_with_opt_list <- unique(full_decline_with_opt$curve_ID)

##vis
# ggplot() +
#   geom_point(data = opt %>%
#                filter(left_bound == "no") %>%
#                filter(right_bound == "yes"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 2,
#                       labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.4 full tpc
full_curve <- opt %>%
  filter(right_bound == "yes") %>%
  filter(left_bound == "yes")
full_curve_list <- unique(full_curve$curve_ID) #20

##vis
# ggplot() +
#   geom_point(data = opt %>%
#                filter(left_bound == "yes") %>%
#                filter(right_bound == "yes"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 3,
#                       labeller = labeller(curve_ID = curve_labels))
#no manual reassignment

### 4.5. optimum only ###
optimum_only <- opt %>%
  filter(right_bound == "no") %>%
  filter(left_bound == "no")
optimum_only_list <- unique(optimum_only$curve_ID)

#vis
# ggplot() +
#   geom_point(data = opt %>%
#                filter(left_bound == "no") %>%
#                filter(right_bound == "no"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 5, nrow = 5, page = 6,
#                       labeller = labeller(curve_ID = curve_labels))
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
    left_bound  = ifelse(first_response <= 0.25, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.25, "yes", "no")
  ) %>%
  ungroup()

## CTMIN / left bound only datasets ##
partial_rise_with_min <- non_opt %>%
  filter(left_bound == "yes")

#vis check
# ggplot() +
#   geom_point(data = non_opt %>%
#                filter(left_bound == "yes"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 6, nrow = 6, page = 2,
#                       labeller = labeller(curve_ID = curve_labels))
#no manual reassignment
partial_rise_with_min_list <- c(unique(partial_rise_with_min$curve_ID)) 

## CTMAX / right bound only datasets
partial_decline_with_max <- non_opt %>%
  filter(right_bound == "yes")

#vis check
# ggplot() +
#   geom_point(data = non_opt %>%
#                filter(right_bound == "yes"),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
#                       labeller = labeller(curve_ID = curve_labels))
#no manual reassignment
partial_decline_with_max_list <- c(unique(partial_decline_with_max$curve_ID)) 

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

#vis check
# ggplot() +
#   geom_point(data = increasing_unbounded,
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 6, nrow = 6, page = 1,
#                       labeller = labeller(curve_ID = curve_labels))
partial_rise_list <- c(unique(increasing_unbounded$curve_ID))  #120

## unbounded, decreasing ##
decreasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "decreasing")

#vis check
# ggplot() +
#   geom_point(data = decreasing_unbounded,
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 6, nrow = 6, page = 2,
#                       labeller = labeller(curve_ID = curve_labels))
partial_decline_list <- c(unique(decreasing_unbounded$curve_ID)) #39

## flat 
flat_unbounded <- unbounded_curve_direction %>%
  filter(direction == "flat") #63 - survival curve with 100% across all temps, move to irregular 

irregular_list <- c(unique(flat_unbounded$curve_ID))



#### putting it together ####
# now i have these vectors that hold all of the curves sorted
all <- c(full_curve_list, optimum_only_list, full_rise_with_opt_list, full_decline_with_opt_list, partial_rise_with_min_list, partial_decline_with_max_list, partial_rise_list, partial_decline_list, irregular_list)

length(unique(all))
curve_ids <- c(unique(curves$curve_ID))
setdiff(curve_ids, all)
setdiff(all, curve_ids)

distinct_curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(dataset_type = case_when(
    curve_ID %in% full_curve_list ~ "full_curve",
    curve_ID %in% optimum_only_list ~ "optimum_only",
    curve_ID %in% full_rise_with_opt_list ~ "full_rise_with_opt",
    curve_ID %in% full_decline_with_opt_list ~ "full_decline_with_opt",
    curve_ID %in% partial_rise_with_min_list ~ "partial_rise_with_min",
    curve_ID %in% partial_decline_with_max_list ~ "partial_decline_with_max",
    curve_ID %in% partial_rise_list ~ "partial_rise",
    curve_ID %in% partial_decline_list ~ "partial_decline",
    curve_ID %in% irregular_list ~ "irregular",
    TRUE ~ NA_character_
  ))

dataset_types <- distinct_curves %>%
  group_by(curve_ID) %>%
  select(curve_ID, dataset_type) %>% 
  distinct() %>%
  mutate(topt_TF = dataset_type %in% c("full_curve","optimum_only","full_rise_with_opt","full_decline_with_opt"),
  thermal_min_TF = dataset_type %in% c("full_curve","full_rise_with_opt", "partial_rise_with_min"),
  thermal_max_TF = dataset_type %in% c("full_curve","full_decline_with_opt", "partial_decline_with_max"),
  tolerance_breadth_TF = dataset_type %in% c("full_curve"),
  increasing_side_TF = dataset_type %in% c("optimum_only", "full_rise_with_opt", "partial_rise_with_min", "full_curve", "partial_rise"),
  decreasing_side_TF = dataset_type %in% c("optimum_only", "full_decline_with_opt", "partial_decline_with_max", "full_curve", "partial_decline"))

#### output ####
curves <- curves %>%
  left_join(dataset_types, join_by(curve_ID))

write.csv(curves, file = here('processed-data', "fishtherm_curve_coverage_sorted_updated10_5.csv"))


