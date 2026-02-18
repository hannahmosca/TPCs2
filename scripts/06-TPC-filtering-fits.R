#### script for filtering models and datasets for analysis ####
#### load packages and data ####
####have not done this script yet, left off with the fitted valid data that i have not figured out how to filter yet ###

####01: script set up ####
#clean env and load packages
rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)

#load data, but filter out the curves i needed to refit 
curves <- readRDS(here('processed-data', 'wild_tpcs_data_coverage_sorted13_2_2026.RDS'))
model_preds <- readRDS(here('processed-data', 'all_model_predictions_19_1_26.RDS'))
params <- readRDS(here('processed-data', 'all_model_params_19_1_26.RDS'))
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations_19_1_26.RDS'))


length(unique(model_evaluations$curve_ID)) #457

#### 02 restrain working models to those that predict within reasonable range ####
#within 1sd of min and max
curves_sd <- curves %>%
  group_by(curve_ID) %>%
  mutate(sd_response = sd(response_value, na.rm = TRUE),
         min_1sd = min(response_value, na.rm = TRUE) - sd_response,
         max_1sd = max(response_value, na.rm. = TRUE) + sd_response,
         min_temp = min(test_temp, na.rm = TRUE),
         max_temp = max(test_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(curve_ID = as.numeric(curve_ID))
#Attach bounds to fitted data ###
model_preds_with_bounds <- model_preds %>%
  left_join(
    curves_sd %>% distinct(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
    by = "curve_ID"
  )
#filter valid models within 1 SD of raw data and get valid models/preds ###
valid_models <- model_preds_with_bounds %>%
  group_by(curve_ID, model) %>%
  summarise(valid = all(.fitted >= min_1sd & .fitted <= max_1sd), .groups = "drop") %>%
  filter(valid) %>%
  select(-valid) %>%
  filter(model != "ratkowsky") 

###also want a filter that is if the model predicts ctmin or ctmax to be more than 5 degrees on the x away from the min temp tested and max temp tested
# both for full curve ones
#for ones with ctmin  - just ctmin needs to be within 5
#for ones with a ctmax - jsut ctmax needs to be within 5
valid_models <- valid_models %>%
  left_join(
    params %>%
      distinct(curve_ID, model, ctmin, ctmax),  # assuming these columns exist
    by = c("curve_ID", "model")
  ) %>%
  left_join(
    curves_sd %>% distinct(curve_ID, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
    by = "curve_ID"
  ) %>%
  filter(
    # Keep models where:
    (
      # if only thermal_min == TRUE
      thermal_min_TF == TRUE & thermal_max_TF != TRUE & ctmin >= (min_temp - 5)
    ) |
      (
        # if only thermal_max == TRUE
        thermal_max_TF == TRUE & thermal_min_TF != TRUE & ctmax <= (max_temp + 5)
      ) |
      (
        # if both are TRUE
        thermal_min_TF == TRUE & thermal_max_TF == TRUE &
          ctmin >= (min_temp - 5) & ctmax <= (max_temp + 5)
      ) |
      (
        # if neither are TRUE, keep everything (don’t filter)
        thermal_min_TF != TRUE & thermal_max_TF != TRUE
      )
  )

### we lost some curveIDs
length(unique(valid_models$curve_ID)) # go from #457 to #456 datasets when i filter out the SD
not_curves <- curves %>%
  select(curve_ID) %>%
  filter(!(curve_ID %in% valid_models$curve_ID)) %>%
  distinct() # lost = 63 and 278 (63 is irregular)
not_curves_list <- unique(not_curves$curve_ID)
#check these#
ggplot() +
  geom_point(data = curves %>% 
               filter(curve_ID %in% not_curves_list),
             aes(x = test_temp, y = response_value)) +
  geom_line(data = model_preds %>%
              filter(curve_ID %in% not_curves_list),
            aes(x = test_temp, y = .fitted, colour = model)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")






valid_preds <- model_preds %>%
  semi_join(valid_models, by = c("curve_ID", "model"))

valid_model_evaluations <- model_evaluations %>%
  inner_join(valid_models, by = c("curve_ID", "model"))

valid_params <- params %>%
  inner_join(valid_models %>% select(model, curve_ID), by = c("curve_ID", "model"))

#make some space
rm(model_preds)
rm(model_preds_with_bounds)
rm(model_evaluations)
rm(params)

#### get top 2 models for each dataset? ####
top_models <- valid_model_evaluations %>%
  group_by(curve_ID) %>%
  arrange(AIC, .by_group = TRUE) %>%  
  slice_head(n = 2) %>%          
  ungroup()

top_model <- top_models %>%
  group_by(curve_ID) %>%
  arrange(AIC, .by_group = TRUE) %>%  
  slice_head(n = 1) %>%          
  ungroup()
second_top_model <- top_models %>%
  group_by(curve_ID) %>%
  arrange(-AIC, .by_group = TRUE) %>%  
  slice_head(n = 1) %>%          
  ungroup()
top_model_preds <- valid_preds %>%
  inner_join(top_model %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
second_top_model_preds <- valid_preds %>%
  inner_join(second_top_model %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
top_params <- valid_params %>%
  inner_join(top_models %>% select(curve_ID, model), by = c("curve_ID", "model")) %>%
  left_join(curves %>% select(curve_ID, dataset_type), join_by(curve_ID)) %>%
  distinct()
best_param <- top_params %>%
  inner_join(top_model %>% select(curve_ID, model), by = c("curve_ID", "model"))

## top model preds with their params
top_preds <- top_model_preds %>%
  left_join(best_param, join_by(curve_ID, model))

#### save the top preds/moels ####
saveRDS(top_preds, here("processed-data", "top_model_predictions18_12_2025.RDS"))
##breadth##
breadth_curves <- curves %>%
  filter(dataset_type == "topt") %>%
  left_join(best_param %>% select(curve_ID, topt, y_value_topt), by = "curve_ID") %>%
  group_by(curve_ID) %>%
  mutate(
    thresh_80 = 0.8 * y_value_topt,
    below_topt = test_temp < topt,
    above_topt = test_temp > topt,
    has_below = any(response_value[below_topt] < unique(thresh_80), na.rm = TRUE),
    has_above = any(response_value[above_topt] < unique(thresh_80), na.rm = TRUE),
    usable_for_breadth = has_below & has_above
  ) %>%
  ungroup() %>%
  filter(usable_for_breadth)
breadth_topt <- unique(breadth_curves$curve_ID) #69 of the topt curves can be used for topt

###adding cols to curves ###
curves <- curves %>%
  mutate(
    thermal_tolerance_TF = dataset_type == "full_curve",
    thermal_safety_margin_TF = dataset_type %in% c("full_curve", "right_bound_withopt"),
    breadth_TF = curve_ID %in% breadth_topt | dataset_type == "full_curve"
  )
## add these cols to the other dfs
top_model_preds <- top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
second_top_model_preds <- second_top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
top_params <- top_params %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
best_param <- best_param %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()





responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()

#### TOPT ####
library(ggforce)
ggplot() +
  geom_point(data = curves %>%
               filter(curve_ID %in% act_eng),
             aes(x = test_temp, y = response_value)) +
   geom_point(data = best_param %>%
                filter(curve_ID %in% act_eng),
             aes(x = topt, y = y_value_topt, color = model)) +
   geom_point(data = best_param %>%
                filter(curve_ID %in% act_eng),
             aes(x = ctmin, y = y_value_ctmin, color = model)) +
   geom_point(data = best_param %>%
                filter(curve_ID %in% act_eng),
             aes(x = ctmax, y = y_value_ctmax, color = model)) +
  geom_line(data = top_model_preds %>%
              filter(curve_ID %in% act_eng),
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 20,
                      labeller = labeller(curve_ID = curve_labels)) +
  scale_color_manual(
    values = c(
      "johnsonlewin" = "slateblue", 
      "lactin2" = "#4DAF4A",  
      "oneill"= "magenta", 
      "ratkowsky" = "yellow",  
      "rezende" = "#A65628",  
      "spain" = "royalblue3",  
      "thomas" = "#999999",  
      "weibull" = "black"  ,
      "hinshelwood" = "aquamarine",
      "briere" = "lightblue", 
      "gaussian" = "maroon",
      "quadratic" = "green"
    )
  ) +
  theme_minimal() +
  labs(x = "Test Temperature", y = "Response", color = "Model")



data_types_his <- best_param %>%
  select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, thermal_safety_margin_TF)  %>%
  left_join(curves %>% select(curve_ID, n_unique_temps)) %>%
  distinct()
data_types_his <- data_types_his %>%
  mutate(n_unique_temps = case_when(
    n_unique_temps >= 7 ~ "7+",
    TRUE ~ as.character(n_unique_temps)  # keep the original value otherwise
  ))
data_types_his <- data_types_his %>%
  mutate(none_TF = !if_any(thermal_min_TF:thermal_safety_margin_TF, ~ .x))

data_types_long <- data_types_his %>%
  pivot_longer(
    cols = c(thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF,
             thermal_tolerance_TF, thermal_safety_margin_TF, none_TF),
    names_to = "parameter",
    values_to = "has_param"
  )
param_counts_summary <- data_types_long %>%
  filter(has_param == TRUE) %>%
  count(parameter, n_unique_temps)
#### exploring activation energy ####
act_eng <- top_params %>%
  filter(dataset_type == "unbounded_increasing") %>%
  filter(!(is.na(e)))
###maybe look at the mean and variation around the mean acros curves? 
ggplot(act_eng, aes(x = e)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of parameter e", x = "e", y = "Count")
length(unique(act_eng$curve_ID)) #128 unbinc + additional from other categories

mean <- mean(act_eng$e)








### start here, sorted data by what param curve covers ###

ggplot(param_counts_summary, aes(x = reorder(parameter, -n), y = n, fill = as.factor(n_unique_temps))) +
  geom_col(position = "stack") +
  labs(
    x = "Parameter",
    y = "Datasets",
    fill = "Unique Temps",
    title = "Count of fitted datasets with Each Thermal Parameter"
  ) +
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "topt_TF" = "Topt",
      "none_TF" = "No curve params",
      "thermal_max_TF" = "Tmax",
      "breadth_TF" = "Thermal Breadth",
      "thermal_min_TF" = "Tmin",
      "thermal_safety_margin_TF" = "Thermal Safety Margin",
      "thermal_tolerance_TF" = "Thermal Tolerance"
    )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### parameter analysis #### don't run this again
params_with_curve_info <- best_param %>%
  left_join(curves %>% select(curve_ID, study_ID, habitat_water, habitat, abs_latitude, latitude, longitude, response_type, response_unit, given_trait_name, Trait.Group, Trait.motivation, land_or_sea, treatment_1_group), join_by(curve_ID)) %>%
  distinct()

saveRDS(params_with_curve_info, file = here("processed-data", "sorted_datasets_withparams.RDS"))
