#### ============================================================
#### Script info
#### ============================================================
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.
#### ============================================================

#### 1. load packages ####
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
               filter(has_optimum == TRUE),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
                      labeller = labeller(curve_ID = curve_labels))



opt_list <- unique(optimum_curves$curve_ID) #checked these, all fit criteria

# these are ones i got from ctmin and ctmax and unbounded_NO
adding_to_topt <- c(18, 81, 212, 213, 216, 218, 250, 289, 300, 311, 359, 433, 448, 69, 168, 169, 215, 20, 54, 53, 56, 64, 67, 73, 77, 148, 192, 200, 207, 208, 211,223, 217, 228, 247, 251, 252, 278, 294, 325, 341, 345, 423, 425, 427, 249, 436, 437, 438, 439, 440, 444, 455)

opt_list <- c(opt_list, adding_to_topt)
opt_list <- unique(opt_list)

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
  geom_point(data = data_scaled %>%
               filter(curve_ID == "125"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 1,
                      labeller = labeller(curve_ID = curve_labels))

###after this check, adding 18, 81, 212, 213, 216, 218, 250, 289, 300, 311, 359, 433, 448  to topt dataframe list

###72 is really confusing, going to a confusing curve ID list

ctmin_only <- unique(ctmin$curve_ID) ##checking ctmin only
#FINAL CTMIN ONLY LIST#
ctmin_only_list <- ctmin_only[!ctmin_only %in% c(18, 81, 212, 213, 216, 218, 250, 289, 300, 311, 359, 433, 448, 72)] 
adding_to_ctmin <- c(22)
ctmin_only_list <- c(ctmin_only_list, adding_to_ctmin)

#CTMAX only datasets
####testing station####
ggplot() +
  geom_point(data = non_opt %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
##irregular: 29 
#move to topt: 69, 168, 169, 212, 215, 448
ctmax <- non_opt %>%
  filter(right_bound == "yes")
ctmax_only <- unique(ctmax$curve_ID) ##checking ctmin only
#FINAL CTMAX ONLY LIST#
ctmax_only_list <- ctmax_only[!ctmax_only %in% c(69, 168, 169, 212, 215, 448, 29)] #ones i am removing from ctmax

#niether CTMIN or CTMAX
ggplot() +
  geom_point(data = non_opt %>%
               filter(left_bound == "no") %>%
               filter(right_bound == "no"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
#irregular: 3, 4, 12, 19, 31, 32, 33, 63, 155, 178, 197, 209, 319, 320, 322, 324, 332, 334, 340, 353, 354, 355, 356, 357, 358, 361, 364, 376, 378, 384
#move to opt: 20, 54, 53, 56, 64, 67, 73, 77, 148, 192, 200, 207, 208, 211,223, 217, 228, 247, 249, 251, 252, 278, 294, 325, 341, 345, 423, 425, 427, 249, 436, 437, 438, 439, 440, 444, 455
#move to ctmin: 22
unbounded_NO <- non_opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "no")
unbounded_NO <- unique(unbounded_NO$curve_ID) 
unbounded_NO_list <- unbounded_NO[!unbounded_NO %in% c(20, 54, 53, 56, 64, 67, 73, 77, 148, 192, 200, 207, 208, 211,223, 217, 228, 247, 249, 251, 252, 278, 294, 325, 341, 345, 423, 425, 427, 249, 436, 437, 438, 439, 440, 444, 455, 22, 3, 4, 12, 19, 31, 32, 33, 63, 155, 178, 197, 209, 319, 320, 322, 324, 332, 334, 340, 353, 354, 355, 356, 357, 358, 361, 364, 376, 378, 384)]

confusing_datasets <- c(8, 29, 72, 3, 4, 12, 19, 31, 32, 33, 63, 155, 178, 197, 209, 319, 320, 322, 324, 332, 334, 340, 353, 354, 355, 356, 357, 358, 361, 364, 376, 378, 384, 223, 169, 168, 77, 263, 111, 136, 189)


#### 06. WORKING WITH OPT DATASETS ####
## first sort by boundedness ## i made the closeness to 0 further for this ones....
opt <- data_scaled %>%
  filter(curve_ID %in% opt_list) %>%
  group_by(curve_ID) %>%
  arrange(test_temp) %>%
  mutate(
    first_temp = first(test_temp),
    first_response = first(response_scaled),
    left_bound  = ifelse(first_response <= 0.20, "yes", "no"),
    last_temp = last(test_temp),
    last_response = last(response_scaled),
    right_bound = ifelse(last_response <= 0.20, "yes", "no")
  ) %>%
  ungroup()

##first bounded ones##
#ctmin with topt datasets
##testing
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "yes") %>%
               filter(right_bound == "no"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 1,
                      labeller = labeller(curve_ID = curve_labels))

#move to full curve: 16, 73, 218, 256, 403, 404, 434, 435, 441
#move to confusing: 223
ctmin_topt <- opt %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "no")
ctmin_topt_list <- unique(ctmin_topt$curve_ID)
ctmin_topt_list <- ctmin_topt_list[!ctmin_topt_list %in% c(16, 73, 218, 256, 403, 404, 434, 435, 441,223)]

#ctmax with topt datasets
##testing
ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "no") %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 1,
                      labeller = labeller(curve_ID = curve_labels))
#moving to irregular: 169, 168
#moving to full: 215, 255
ctmax_topt <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "yes")
ctmax_topt_list <- unique(ctmax_topt$curve_ID)
ctmax_topt_list <- ctmax_topt_list[!ctmax_topt_list %in% c(215, 255, 169, 168)]



full <- c(325, 16, 73, 218, 256, 403, 404, 434, 435, 441, 215, 255) #curves i think are full that i got from ctmax opt and ctmin opt

#ctmin+ctmax+topt full curves#

ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "yes") %>%
               filter(right_bound == "yes"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 2,
                      labeller = labeller(curve_ID = curve_labels))
#irregular: 210?
#move 278 to just topt
breadth <- opt %>%
  filter(left_bound == "yes") %>%
  filter(right_bound == "yes")
breadth_list <- unique(breadth$curve_ID)
breadth_list <- c(breadth_list, full)
breadth_list <- breadth_list[!breadth_list %in% c(8, 278)]

breadth_list <- unique(breadth_list)

### unbounded data ##

ggplot() +
  geom_point(data = opt %>%
               filter(left_bound == "no") %>%
               filter(right_bound == "no"),
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 17,
                      labeller = labeller(curve_ID = curve_labels))

#moving 77, 263 to irregular
#move 245 to inc unbounded
#move 325 to full
#unbounded curves
topt_only <- opt %>%
  filter(left_bound == "no") %>%
  filter(right_bound == "no")
topt_only <- unique(topt_only$curve_ID)
add_to_topt_only <- c(278)
topt_only <- c(add_to_topt_only, topt_only)
topt_only <- topt_only[!topt_only %in% c(325, 245, 77, 263, 257, 346)]


#### 07 WORKING with unbounded no opt ####

library(dplyr)

unbounded_curve_direction <- data_scaled %>%
  group_by(curve_ID) %>%
  filter(curve_ID %in% unbounded_NO_list) %>%
  mutate(
    slope = lm(response_scaled ~ test_temp)$coefficients[2],  
    direction = case_when(
      slope > 0 ~ "increasing",
      slope < 0 ~ "decreasing",
      TRUE ~ "flat"
    )
  )
increasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "increasing")

ggplot() +
  geom_point(data = unbounded_curve_direction %>%
               filter(direction == "increasing"), 
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 15,
                      labeller = labeller(curve_ID = curve_labels))
#move to irregular: 111, 136, 189,
inc_unbounded_NO_list <- unique(increasing_unbounded$curve_ID)
add_to_inc <- c(245, 257, 346) 
inc_unbounded_NO_list <- c(inc_unbounded_NO_list, add_to_inc)
inc_unbounded_NO_list <- inc_unbounded_NO_list[!inc_unbounded_NO_list %in% c(111, 136, 189, 48)]


##decreasing unbounded

ggplot() +
  geom_point(data = unbounded_curve_direction %>%
               filter(direction == "decreasing"), 
             aes(x = test_temp, y = response_scaled)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 4,
                      labeller = labeller(curve_ID = curve_labels))
decreasing_unbounded <- unbounded_curve_direction %>%
  filter(direction == "decreasing")
dec_unbounded_NO_list <- unique(decreasing_unbounded$curve_ID)
add_to_dec <- c(48)
dec_unbounded_NO_list <- c(dec_unbounded_NO_list, add_to_dec)


# now i have these vectors that hold all of the curves sorted
all <- c(topt_only, ctmax_only_list, ctmin_only_list, inc_unbounded_NO_list, dec_unbounded_NO_list, confusing_datasets, ctmin_topt_list, ctmax_topt_list, breadth_list)
length(unique(all))

# want to put curveID 446 from unbounded IN to ctmin_topt_list 
# remove 446 from inc_unbounded_NO_list
inc_unbounded_NO_list <- inc_unbounded_NO_list[inc_unbounded_NO_list != 446]

# add 446 to ctmin_topt_list (avoid duplicates just in case)
ctmin_topt_list <- unique(c(ctmin_topt_list, 446))

## 
distinct_curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(dataset_type = case_when(
    curve_ID %in% topt_only ~ "topt",
    curve_ID %in% ctmax_only_list ~ "right_bound",
    curve_ID %in% ctmin_only_list ~ "left_bound",
    curve_ID %in% inc_unbounded_NO_list ~ "unbounded_increasing",
    curve_ID %in% dec_unbounded_NO_list ~ "unbounded_decreasing",
    curve_ID %in% confusing_datasets ~ "irregular",
    curve_ID %in% ctmin_topt_list ~ "left_bound_withopt",
    curve_ID %in% ctmax_topt_list ~ "right_bound_withopt",
    curve_ID %in% breadth_list ~ "full_curve",
    TRUE ~ NA_character_
  ))


dataset_types <- distinct_curves %>%
  group_by(curve_ID) %>%
  select(curve_ID, dataset_type) %>%
  distinct() %>%
  mutate(
    topt_TF = case_when(curve_ID %in% c(topt_only, ctmin_topt_list, ctmax_topt_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    thermal_min_TF = case_when(curve_ID %in% c(ctmin_topt_list, ctmin_only_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    thermal_max_TF = case_when(curve_ID %in% c(ctmax_topt_list, ctmax_only_list, breadth_list) ~ TRUE, TRUE ~ FALSE),
    breadth_TF = case_when(curve_ID %in% breadth_list ~ TRUE, TRUE ~ FALSE),
    increasing_side_TF = case_when(curve_ID %in% c(ctmin_topt_list, ctmin_only_list, breadth_list, inc_unbounded_NO_list) ~ TRUE, TRUE ~ FALSE),
    decreasing_side_TF = case_when(curve_ID %in% c(ctmax_topt_list, ctmax_only_list, breadth_list, dec_unbounded_NO_list) ~ TRUE, TRUE ~ FALSE)) %>%
  ungroup()



#### 08. OUTPUT ####
curves <- curves %>%
  left_join(dataset_types, join_by(curve_ID))
write.csv(curves, file = here('processed-data', "fishtherm_curve_coverage_sorted.csv"))


#### 09. visualization ####
curves <- curves %>%
  select(n_unique_temps, curve_ID, study_ID, species_ID, given_trait_name, Trait.Group, Trait.motivation, organization, curve_type, land_or_sea, abs_latitude, habitat_water, dataset_type, topt_TF, thermal_min_TF, thermal_max_TF, increasing_side_TF, decreasing_side_TF) %>%
  distinct() %>%
  mutate(n_unique_temps_capped = ifelse(n_unique_temps >= 7, "7+", n_unique_temps))

## curve coverage figures ##

## want to order it by how much is most seen
curves <- curves %>%
  mutate(dataset_type = factor(
    dataset_type, levels = c("right_bound","right_bound_withopt", "left_bound", "full_curve","unbounded_decreasing", "left_bound_withopt", "irregular","unbounded_increasing", "topt"))) %>%
  mutate(n_unique_temps_capped = factor(n_unique_temps_capped,
                                        levels = c("7+", "6", "5", "4")))


a <- ggplot(data = curves, aes(x = dataset_type, fill = (n_unique_temps_capped))) +
  geom_bar(position = "stack", colour = "black",linewidth = 0.3) +
  scale_fill_manual(values = c("4"= "#CDEDF6",
                               "5" = "#208AAE",
                               "6" ="#70161E",
                               "7+" = "#0D2149")) +
  xlab(NULL) +
  ylab("Number of datasets") +
  scale_y_continuous(expand = expansion(mult = 0.00),
                     breaks = seq(0,150,25)) +
  scale_x_discrete(
    labels = c(
      "full_curve" = "Full curve",
      "left_bound_withopt" = "T-min + T-opt",
      "right_bound_withopt" = "T-max + T-opt", 
      "topt" = "T-opt only",
      "left_bound" = "T-min only",
      "right_bound" = "T-max only",
      "unbounded_increasing" = "Unbound, inc",
      "unbounded_decreasing" = "Unbound, dec",
      "irregular" = "Irregular")) +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

a
ggsave("curve_coverage_by_temperature_resolution.pdf", plot = a, path = here("figures"), width = 6, height = 4)

