# title: selecting-valid-models-and-deriving-params.R
# Description:
#   Filters candidate thermal performance curve (TPC) models to retain
#   biologically reasonable fits and selects the top-ranked model(s) per curve. Outputs a dataset of tpc parameters

rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)
rm(list=ls())
#### 01 load data ####
curves <- read.csv(here('processed-data', 'fishtherm_curve_coverage_sorted_updated10_5.csv')) %>%
  select(-(X)) %>%
  select(-(X.1))
model_preds <- readRDS(here('processed-data', 'all_model_predictions.RDS')) %>%
  filter(curve_ID != 29) ##### flagging that these were fit before duplicate curve was found on 2026-07-27, so must remove curveID #29 from future data ####
params <- readRDS(here('processed-data', 'all_model_params.RDS')) %>%
  filter(curve_ID != 29)
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations.RDS')) %>%
  filter(curve_ID !=29)


length(unique(model_preds$curve_ID)) #456

#### 03 filter out irregular datasets ####
irregular <- curves %>%
  filter(dataset_type == "irregular")
irregular_list <- c(unique(irregular$curve_ID)) #10 datasets

model_preds_1 <- model_preds %>%
  filter(!(curve_ID %in% irregular_list)) #446 now
params_1 <- params %>%
  filter(!(curve_ID %in% irregular_list)) #446 now
model_evaluations_1 <- model_evaluations %>%
  filter(!(curve_ID %in% irregular_list)) #446 now

#make some space
rm(model_preds)
rm(model_evaluations)
rm(params)

#### 02 restrain working models to those that predict within reasonable range ####

#filter valid models within 1 SD of raw data and get valid models/preds ###

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
model_preds_with_bounds <- model_preds_1 %>%
  left_join(
    curves_sd %>% distinct(curve_ID, response_value, test_temp, sd_response, max_1sd, min_1sd, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
    by = "curve_ID"
  )
# filter out models where any predictions are outside of 1 sd of data points,and also ratkowsky for consistently poor fits
valid_models <- model_preds_with_bounds %>% 
  group_by(curve_ID, model) %>%
  summarise(valid = all(.fitted >= min_1sd & .fitted <= max_1sd), .groups = "drop") %>%
  filter(valid) %>%
  select(-valid) %>%
  filter(model != "ratkowsky") #consistently poor fit/weird shape

length(unique(valid_models$curve_ID)) #446 

#### 03 filter out models that predict tmin or tmax to be more than 5 degrees on the x away from the min temp tested and max temp tested

# both tmin and tmax for full curve ones
#for ones with tmin  -  ctmin needs to be within 5
#for ones with a tmax -  ctmax needs to be within 5

valid_models <- valid_models %>%
  left_join(params_1 %>% distinct(curve_ID, model, ctmin, ctmax), by = c("curve_ID", "model")) %>%
  left_join(curves_sd %>% distinct(curve_ID, min_temp, max_temp, dataset_type, thermal_min_TF, thermal_max_TF),
            by = "curve_ID") %>%
  #count the number of models fitted to each curve
  #check whether predicted ctmin and ctmax are within 5deg of min and max test temp
  group_by(curve_ID) %>% mutate(n_models = n_distinct(model), 
                                min_five_ok = !is.na(ctmin) &
                                  ctmin >= (min_temp - 5),
                                max_five_ok = !is.na(ctmax) &
                                  ctmax <= (max_temp + 5),
  #determine whether each model passes the appropriate endpoint criterion based on the dataset type
    five_flag = case_when(dataset_type == "full_curve" ~ min_five_ok & max_five_ok, # full curves must have both endpoints within 5 deg
                          dataset_type %in% c("full_rise_with_opt", "partial_rise_with_min") ~ min_five_ok,  # datasets with a thermal minimum only need ctmin within 5deg
                          dataset_type %in% c("full_decline_with_opt","partial_decline_with_max") ~ max_five_ok, #datasets with a thermal maximum only need ctmax within 5deg
                          TRUE ~ TRUE),  # datasets w/ applicable thermal endpoint
    
    # determine whether at least one model passes the appropriate endpoint criterion for each dataset
    any_five_ok = any(five_flag, na.rm = TRUE)) %>%
  
  # if only one model fits, always keep 
  # if multiple models fit:
  #   - keep only models passing the relevant 5deg criterion if at least one model passes
  filter(n_models == 1 |!any_five_ok |five_flag) %>%
  ungroup()

length(unique(valid_models$curve_ID)) # #446

valid_preds <- model_preds_1 %>%
  semi_join(valid_models, by = c("curve_ID", "model"))

valid_model_evaluations <- model_evaluations_1 %>%
  inner_join(valid_models, by = c("curve_ID", "model"))

valid_params <- params_1 %>%
  inner_join(valid_models %>% select(model, curve_ID, min_five_ok, max_five_ok), by = c("curve_ID", "model")) %>%
  mutate(ctmin = if_else(min_five_ok == FALSE, NA_real_, ctmin),
         ctmax = if_else(max_five_ok == FALSE, NA_real_, ctmax))

#somehow here i want to put NA when the param isn't valid based on the flags i made

#check these#
ggplot() +
  geom_point(data = curves,
             aes(x = test_temp, y = response_value)) +
  geom_line(data = valid_preds,
            aes(x = test_temp, y = .fitted, colour = model)) +
  geom_vline(data = valid_params,
             aes(xintercept = ctmin)) +
  geom_vline(data = valid_params,
             aes(xintercept = ctmax)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 6, nrow = 6, page = 1) +
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

#make some space
rm(model_preds_1)
rm(model_preds_with_bounds)
rm(model_evaluations_1)
rm(params_1)

#### 03. Get top 2 models for each dataset ####
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
  left_join(best_param, join_by(curve_ID, model)) %>%
  select(-(dataset_type.y)) %>%
  rename(dataset_type = dataset_type.x)

## save the top preds/moels #
saveRDS(top_preds, here("processed-data", "top_model_predictions_10_6.RDS")) 

#### 04. filter curves that have enough data for upper and lower breadth/pejus temps ####
breadth_curves <- curves %>%
  filter(dataset_type %in% c("optimum_only", "full_rise_with_opt", "full_decline_with_opt", "full_curve")) %>%
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
breadth_topt <- unique(breadth_curves$curve_ID) 
breadth_topt_list <- c(breadth_topt)

ggplot() +
  geom_point(data = curves %>% 
               filter(curve_ID %in% breadth_topt_list),
             aes(x = test_temp, y = response_value)) +
  geom_line(data = top_model_preds %>%
              filter(curve_ID %in% breadth_topt_list),
            aes(x = test_temp, y = .fitted, colour = model)) +
  geom_vline(data = top_params %>%
               filter(curve_ID %in% breadth_topt_list),
             aes(xintercept = topt)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 2) +
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



## adding cols to curves ###
curves <- curves %>%
  mutate(
    thermal_tolerance_TF = dataset_type == "full_curve",
    breadth_TF = curve_ID %in% breadth_topt_list)


## add these cols to the other dfs
top_model_preds <- top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
second_top_model_preds <- second_top_model_preds %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
top_params <- top_params %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()
best_param <- best_param %>%
  left_join(curves %>% select(curve_ID, thermal_min_TF, thermal_max_TF, breadth_TF, topt_TF, thermal_tolerance_TF, increasing_side_TF, decreasing_side_TF), join_by(curve_ID)) %>%
  distinct()



#### 05. give NA to params that are not valid..####
#ie the dataset doesnt meet the criteria to have that parameter and the model predicted by default ####

params_with_curve_info <- best_param %>%
  left_join(curves %>% select(curve_ID, study_ID, habitat_water, habitat, abs_latitude, latitude, longitude, response_unit, given_trait_name, Trait.Group, Trait.motivation, land_or_sea, treatment_1_group), join_by(curve_ID)) %>%
  distinct()

#could do this with dataset type
params_with_curve_info <- params_with_curve_info %>%
  mutate(topt = ifelse(topt_TF == FALSE, NA, topt)) %>%
  mutate(ctmin = ifelse(thermal_min_TF == FALSE, NA, ctmin)) %>%
  mutate(ctmax = ifelse(thermal_max_TF == FALSE, NA, ctmax)) %>%
  mutate(thermal_tolerance = ifelse(thermal_tolerance_TF == FALSE, NA, thermal_tolerance)) %>%
  mutate(breadth = ifelse(breadth_TF == FALSE, NA, breadth)) %>%
  mutate(y_value_topt = ifelse(topt_TF == FALSE, NA, y_value_topt)) %>%
  mutate(y_value_ctmin = ifelse(thermal_min_TF == FALSE, NA, y_value_ctmin)) %>%
  mutate(y_value_ctmax = ifelse(thermal_max_TF == FALSE, NA, y_value_ctmax)) %>%
  select(-(c(rmax, e, eh, q10, thermal_safety_margin, skewness)))
  
saveRDS(params_with_curve_info, file = here("processed-data", "tpcs_with_fitted_params_10_6.RDS"))
