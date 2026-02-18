#### script info #### 
#title: TPC-fitting.R
#author: Hannah Mosca
#this script is for fitting TPCs with rtpc package and filtering the data so we know what part of the curve data for, if we can fit all of the datasets with curves, etc.

#### 1. loading and installing packages and data ####
install.packages('rTPC')
# load packages
library(rTPC)
library(nls.multstart)
install.packages("nls.multstart")
library(broom)
library(tidyverse)
library(dplyr)
library(here)
#load the data
d <- readRDS(here("processed-data","wild-tpcsupdated.RdS")) # this was made in script 01

# classifying datasets by how much data is in them//how many test temps
# TP datasets with 5+ temperatures, calling them 'high res'
high_res_ds <- d %>%
  group_by(curve_ID) %>%
  filter(n_unique_temps >= 5) %>%
  ungroup()
length(unique(high_res_ds$curve_ID)) #237

## TP datasets with 4 temperatures, calling them 'low res'
low_res_ds <- d %>%
  anti_join(high_res_ds, by = "curve_ID") 
length(unique(low_res_ds$curve_ID)) #220

#### 2. Fitting high res curves with all 4 parameter models in rtpc
curve_IDs <- unique(high_res_ds$curve_ID)
#### bierre2_1999 ####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'briere2_1999')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ briere2_1999(temp = test_temp, tmin, tmax, a, b),
      data = curve_data,
      iter = c(4,4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax, ctmin)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])

  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_briere2_1999_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_briere2_1999_highres <- bind_rows(resids_list, .id = "list_id")
all_params_briere2_1999_highres <- bind_rows(params_list, .id = "list_id")
all_preds_briere2_1999_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_briere2_1999_highres <- bind_rows(param_points_list, .id = "list_id")

#### hinshelwood_1947####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'hinshelwood_1947')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ hinshelwood_1947(temp = test_temp, a, e, b, eh),
      data = curve_data,
      iter = c(5, 5, 5, 5),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
  
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_hinshelwood_1947_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_hinshelwood_1947_highres <- bind_rows(resids_list, .id = "list_id")
all_params_hinshelwood_1947_highres <- bind_rows(params_list, .id = "list_id")
all_preds_hinshelwood_1947_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_hinshelwood_1947_highres <- bind_rows(param_points_list, .id = "list_id")

#### johnson_lewin_1946####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'johnsonlewin_1946')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ johnsonlewin_1946(temp = test_temp, r0, e, eh, topt),
      data = curve_data,
      iter = c(5, 5, 5, 5),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #74
# combine results
all_fits_johnsonlewin_1946_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_johnsonlewin_1946_highres <- bind_rows(resids_list, .id = "list_id")
all_params_johnsonlewin_1946_highres <- bind_rows(params_list, .id = "list_id")
all_preds_johnsonlewin_1946_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_johnsonlewin_1946_highres <- bind_rows(param_points_list, .id = "list_id")

#### 6. lactin2_1995####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ lactin2_1995(temp = test_temp, a, b, tmax, delta_t),
      data = curve_data,
      iter = c(3, 3, 3, 3),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_lactin2_1995_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_lactin2_1995_highres <- bind_rows(resids_list, .id = "list_id")
all_params_lactin2_1995_highres <- bind_rows(params_list, .id = "list_id")
all_preds_lactin2_1995_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_lactin2_1995_highres <- bind_rows(param_points_list, .id = "list_id")


#### 9. oneil_1972####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'oneill_1972')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ oneill_1972(temp = test_temp, rmax, ctmax, topt, q10),
      data = curve_data,
      iter = c(4, 4, 4, 4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #5
# combine results
all_fits_oneill_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_oneill_1972_highres <- bind_rows(resids_list, .id = "list_id")
all_params_oneill_1972_highres <- bind_rows(params_list, .id = "list_id")
all_preds_oneill_1972_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_oneill_1972_highres <- bind_rows(param_points_list, .id = "list_id")

#### ratkowsky_1983####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'ratkowsky_1983')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ ratkowsky_1983(temp = test_temp, tmin, tmax, a, b),
      data = curve_data,
      iter = c(4, 4, 4, 4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
  # model summary (glance)
  fit_stats <- broom::glance(fit) %>%
    mutate(curve_ID = curve_ids[i])
  fits_tidy_list[[i]] <- fit_stats
  
  resids_list[[i]] <- augment(fit) %>%
    mutate(curve_ID = curve_ids[i])
  
 } else {
  failed_fits <- c(failed_fits, curve_ids[i])
  params_list[[i]] <- NULL
  preds_list[[i]] <- NULL
  param_points_list[[i]] <- NULL
  fits_list[[i]] <- NULL
  fits_tidy_list[[i]] <- NULL
  resids_list[[i]] <- NULL
 }

cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_ratkowsky_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_ratkowsky_1983_highres <- bind_rows(resids_list, .id = "list_id")
all_params_ratkowsky_1983_highres <- bind_rows(params_list, .id = "list_id")
all_preds_ratkowsky_1983_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_ratkowsky_1983_highres <- bind_rows(param_points_list, .id = "list_id")

#### rezende_2019 ####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'rezende_2019')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ rezende_2019(temp = test_temp, q10, a, b, c),
      data = curve_data,
      iter = c(4, 4, 4, 4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #13
# combine results
all_fits_rezende_2019_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_rezende_2019_highres <- bind_rows(resids_list, .id = "list_id")
all_params_rezende_2019_highres <- bind_rows(params_list, .id = "list_id")
all_preds_rezende_2019_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_rezende_2019_highres <- bind_rows(param_points_list, .id = "list_id")

#### Spain_1982####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 1
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'spain_1982')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ spain_1982(temp = test_temp, a, b, c, r0),
      data = curve_data,
      iter = c(3, 3, 3, 3),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_spain_1982_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_spain_1982_highres <- bind_rows(resids_list, .id = "list_id")
all_params_spain_1982_highres <- bind_rows(params_list, .id = "list_id")
all_preds_spain_1982_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_spain_1982_highres <- bind_rows(param_points_list, .id = "list_id")

#### thomas_2012####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 1
  start_upper <- sv + 2
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'thomas_2012')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ thomas_2012(temp = test_temp, a, b, c, topt),
      data = curve_data,
      iter = c(4, 4, 4, 4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_thomas_2012_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_thomas_2012_highres <- bind_rows(resids_list, .id = "list_id")
all_params_thomas_2012_highres <- bind_rows(params_list, .id = "list_id")
all_preds_thomas_2012_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_thomas_2012_highres <- bind_rows(param_points_list, .id = "list_id")

#### Weibull_1995####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- high_res_ds %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'weibull_1995')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ weibull_1995(temp = test_temp, a, topt, b, c),
      data = curve_data,
      iter = c(4, 4, 4, 4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #2
# combine results
all_fits_weibull_1995_highres <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_weibull_1995_highres <- bind_rows(resids_list, .id = "list_id")
all_params_weibull_1995_highres <- bind_rows(params_list, .id = "list_id")
all_preds_weibull_1995_highres <- bind_rows(preds_list, .id = "list_id")
all_param_points_weibull_1995_highres <- bind_rows(param_points_list, .id = "list_id")

#### fitting low rest curves with 3 parameter models in rtpc ####
#### 3 parameter models for datasets with 4 points ####
#any with 4+ points will be attempted with as many models on rtpc that fit 4 points 
# only 3 models have 3 parameters -- gaussian_1989, and quadratic_2008
curve_IDs <- unique(low_res_ds$curve_ID)
curve_ids <- curve_IDs
#### gaussian_1984 ####
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- d %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'gaussian_1987')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ gaussian_1987(temp = test_temp, rmax, topt, a),
      data = curve_data,
      iter = c(4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_gaussian_1987_all <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_gaussian_1987_highres <- bind_rows(resids_list, .id = "list_id")
all_params_gaussian_1987_all <- bind_rows(params_list, .id = "list_id")
all_preds_gaussian_1987_all <- bind_rows(preds_list, .id = "list_id")
all_param_points_gaussian_1987_all <- bind_rows(param_points_list, .id = "list_id")

#### fit with quadratic_2008####
curve_ids <- curve_IDs
# empty containers for fitting loop
fits_list <- vector("list", length(curve_ids))
names(fits_list) <- curve_ids
params_list <- list()
preds_list <- list()
param_points_list <- list()
failed_fits <- c()
fits_tidy_list <- list()
resids_list <- list()
# loop over each curve
for (i in seq_along(curve_ids)) {
  curve_data <- d %>% filter(curve_ID == curve_ids[i])
  
  # get start values and bounds
  sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(sv)) sv <- sv[1, ]
  
  start_lower <- sv - 10
  start_upper <- sv + 10
  
  lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(lower)) lower <- lower[1, ]
  
  upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'quadratic_2008')
  if (is.matrix(upper)) upper <- upper[1, ]
  
  # fit model
  fit <- try(
    nls_multstart(
      response_value ~ quadratic_2008(temp = test_temp, a, b, c),
      data = curve_data,
      iter = c(4,4,4),
      start_lower = start_lower,
      start_upper = start_upper,
      lower = lower,
      upper = upper,
      supp_errors = 'Y',
      convergence_count = FALSE
    ),
    silent = TRUE
  )
  
  fits_list[[i]] <- fit
  
  if (!inherits(fit, "try-error")) {
    #  parameters
    model_params <- calc_params(fit) %>%
      mutate(curve_ID = curve_ids[i]) %>%
      mutate_all(round, 2)
    params_list[[i]] <- model_params
    
    # predictions
    new_data <- data.frame(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))
    preds <- augment(fit, newdata = new_data) %>%
      mutate(curve_ID = curve_ids[i])
    preds_list[[i]] <- preds
    
    # parameter points (topt, ctmax)
    param_points <- model_params %>%
      select(topt, ctmax, ctmin) %>%
      pivot_longer(cols = everything(), names_to = "label", values_to = "test_temp") %>%
      mutate(
        y_value = predict(fit, newdata = data.frame(test_temp = test_temp)),
        curve_ID = curve_ids[i]
      )
    param_points_list[[i]] <- param_points
    
    # model summary (glance)
    fit_stats <- broom::glance(fit) %>%
      mutate(curve_ID = curve_ids[i])
    fits_tidy_list[[i]] <- fit_stats
    
    resids_list[[i]] <- augment(fit) %>%
      mutate(curve_ID = curve_ids[i])
    
  } else {
    failed_fits <- c(failed_fits, curve_ids[i])
    params_list[[i]] <- NULL
    preds_list[[i]] <- NULL
    param_points_list[[i]] <- NULL
    fits_list[[i]] <- NULL
    fits_tidy_list[[i]] <- NULL
    resids_list[[i]] <- NULL
  }
  
  cat("Finished curve_ID:", curve_ids[i], "\n")
}
print(length(failed_fits))  #0
# combine results
all_fits_quadratic_2008_all <- bind_rows(fits_tidy_list, .id = "list_id")
all_resids_quadratic_2008_highres <- bind_rows(resids_list, .id = "list_id")
all_params_quadratic_2008_all <- bind_rows(params_list, .id = "list_id")
all_preds_quadratic_2008_all <- bind_rows(preds_list, .id = "list_id")
all_param_points_quadratic_2008_all <- bind_rows(param_points_list, .id = "list_id")






#### 16. join all predicted values ####
###here we got rid of lrf model because the fit was really weird###
# for the high res
four_param_preds <- list(
  "spain" = all_preds_spain_1982_highres,
  "weibull" = all_preds_weibull_1995_highres,
  "thomas" = all_preds_thomas_2012_highres,
  "rezende" = all_preds_rezende_2019_highres,
  "ratkowsky" = all_preds_ratkowsky_1983_highres,
  "oneill" = all_preds_oneill_1972_highres,
  "lactin2" = all_preds_lactin2_1995_highres,
  "johnsonlewin" = all_preds_johnsonlewin_1946_highres,
  "hinshelwood" = all_preds_hinshelwood_1947_highres,
  "briere" = all_preds_briere2_1999_highres
)

# for the low res
three_params_preds <- list(
  "gaussian" = all_preds_gaussian_1987_all,
  "quadratic" = all_preds_quadratic_2008_all
)
params_list <- list(
  "spain" = all_params_spain_1982_highres,
  "weibull" = all_params_weibull_1995_highres,
  "thomas" = all_params_thomas_2012_highres,
  "rezende" = all_params_rezende_2019_highres,
  "ratkowsky" = all_params_ratkowsky_1983_highres,
  "oneill" = all_params_oneill_1972_highres,
  "lactin2" = all_params_lactin2_1995_highres,
  "johnsonlewin" = all_params_johnsonlewin_1946_highres,
  "hinshelwood" = all_params_hinshelwood_1947_highres,
  "briere" = all_params_briere2_1999_highres,
  "gaussian" = all_params_gaussian_1987_all,
  "quadratic" = all_params_quadratic_2008_all
)
params_points_list <- list(
  "spain" = all_param_points_spain_1982_highres,
  "weibull" = all_param_points_weibull_1995_highres,
  "thomas" = all_param_points_thomas_2012_highres,
  "rezende" = all_param_points_rezende_2019_highres,
  "ratkowsky" = all_param_points_ratkowsky_1983_highres,
  "oneill" = all_param_points_oneill_1972_highres,
  "lactin2" = all_param_points_lactin2_1995_highres,
  "johnsonlewin" = all_param_points_johnsonlewin_1946_highres,
  "hinshelwood" = all_param_points_hinshelwood_1947_highres,
  "briere" = all_param_points_briere2_1999_highres,
  "gaussian" = all_param_points_gaussian_1987_all,
  "quadratic" = all_param_points_quadratic_2008_all
)
fits_list_all <- list(
  "spain" = all_fits_spain_1982_highres,
  "weibull" = all_fits_weibull_1995_highres,
  "thomas" = all_fits_thomas_2012_highres,
  "rezende" = all_fits_rezende_2019_highres,
  "ratkowsky" = all_fits_ratkowsky_highres,
  "oneill" = all_fits_oneill_highres,
  "lactin2" = all_fits_lactin2_1995_highres,
  "johnsonlewin" = all_fits_johnsonlewin_1946_highres,
  "hinshelwood" = all_fits_hinshelwood_1947_highres,
  "briere" = all_fits_briere2_1999_highres,
  "gaussian" = all_fits_gaussian_1987_all,
  "quadratic" = all_fits_quadratic_2008_all
)
resids_list_all <- list(
  "spain" = all_resids_spain_1982_highres,
  "weibull" = all_resids_weibull_1995_highres,
  "thomas" = all_resids_thomas_2012_highres,
  "rezende" = all_resids_rezende_2019_highres,
  "ratkowsky" = all_resids_ratkowsky_1983_highres,
  "oneill" = all_resids_oneill_1972_highres,
  "lactin2" = all_resids_lactin2_1995_highres,
  "johnsonlewin" = all_resids_johnsonlewin_1946_highres,
  "hinshelwood" = all_resids_hinshelwood_1947_highres,
  "briere" = all_resids_briere2_1999_highres,
  "gaussian" = all_resids_gaussian_1987_highres,
  "quadratic" = all_resids_quadratic_2008_highres
)
# Add a model column and bind all rows
all_preds_long_four <- imap_dfr(four_param_preds, ~ .x %>% mutate(model = .y))
all_preds_long_three <- imap_dfr(three_params_preds, ~ .x %>% mutate(model = .y))
all_preds <- rbind(all_preds_long_four, all_preds_long_three)
all_params <- imap_dfr(params_list, ~ .x %>% mutate(model = .y))
all_fits <- imap_dfr(fits_list_all, ~ .x %>% mutate(model = .y))
all_resisds <- imap_dfr(resids_list_all, ~ .x %>% mutate(model = .y))

all_param_points <- imap_dfr(params_points_list, ~ .x %>% mutate(model = .y)) %>%
  pivot_wider(
    id_cols = c(list_id, curve_ID, model),  
    names_from = label,                    
    values_from = c(test_temp, y_value)     
  )
all_paramaters <- left_join(all_params, all_param_points, join_by(curve_ID, model)) %>%
  select(-(c(test_temp_topt, test_temp_ctmax, test_temp_ctmin, list_id.y, list_id.x))) %>%
  select(curve_ID, model, everything())

saveRDS(all_fits, file = here('processed-data', "model_fit_evaluations_17_10_25.RDS"))
saveRDS(all_preds, file = here('processed-data', "all_model_predictions_17_10_25.RDS"))
saveRDS(all_paramaters, file = here('processed-data', "all_model_params_17_10_25.RDS"))




