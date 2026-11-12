## simulate TPCs and check how Topt/Tmax/Tmin/Ea change depending on curve coverage category
library(tidyverse)

# ============================================================
# write function to simulate thermal performance curves
# ============================================================

simulate_tpc <- function(
    temp = seq(-5, 50, by = 0.5),
    shape = "unimodal",
    Topt = 25,
    Pmax = 1,
    breadth = 10,
    n = 1
) {
  
  # ----------------------------------------------------------
  # Generate expected performance
  # ----------------------------------------------------------
  
  if (shape == "unimodal") {
    
    # Gaussian curve
    performance <- Pmax * exp(
      -((temp - Topt)^2) / (2 * breadth^2)
    )
  } else if (shape == "left_skewed") {
    
    # Sharp decline at high temperatures
    performance <- Pmax * exp(
      -((temp - Topt)^2) /
        (2 * breadth^2 * ifelse(temp < Topt, 2, 0.5))
    )
  } else {
    stop("Error: shape unspecified")
  }
  
  ## save as dataframe 
  result = data.frame(response_value = performance, test_temp = temp)
  
  return(result)
}

# ========================
# simulate some curve data 
# =======================#
set.seed(123)

## try one:
Topt = 30
data <- simulate_tpc(
  temp = seq(-10, 50, by = 0.5),
  Topt = Topt,
  Pmax = 1,
  breadth = 8,
  shape = "left_skewed"
)


## sample n_temps points 500 times 
subsample = c()
n_samp = 500
n_temps = 4:6
for(i in 1:n_samp) {
  for(n in n_temps) {
    if(i <= n_samp/5) {
      ## sample full range of temps
      temp = data[sample(1:nrow(data), n),]
    }
    else if(i >= n_samp/5 & i <= n_samp/5*2) {
      ## sample range of test temps before optimum
      d = data[which(data$test_temp <= Topt),]
      temp = d[sample(1:nrow(d), n),]
    }
    else if(i >= n_samp/5*2 & i <= n_samp/5*3){
      ## sample full range of test temps after optimum
      d = data[which(data$test_temp >= Topt),]
      temp = d[sample(1:nrow(d), n),]
    }
    else if(i >= n_samp/5*3 & i <= n_samp/5*4) {
      ## sample range of temps before and slightly after optimum
      d = data[which(data$test_temp <= Topt + 5),]
      temp = d[sample(1:nrow(d), n),]
    }
    else {
      ## sample range of temps after and slightly before optimum
      d = data[which(data$test_temp >= Topt - 5),]
      temp = d[sample(1:nrow(d), n),]
    }
    
    temp$curve_ID = paste0("1_", i, "_", n)
    temp$n_unique_temps = n
    
    ## add sampling error to each point
    ## normal distribution with mean of 0 and SD of 0.1
    temp$response_value = temp$response_value + rnorm(n, mean = 0, sd = 0.1)
    
    subsample = rbind(subsample, temp)
  }
}

data$curve_ID = "1"
data$n_unique_temps = nrow(data)
d = rbind(data, subsample) 

d %>% 
  filter(curve_ID %in% unique(.$curve_ID)[1:50]) %>%
  ggplot(aes(x = test_temp, y = response_value)) +
  geom_point() +
  facet_wrap(~curve_ID)

d %>% 
  filter(curve_ID %in% unique(.$curve_ID)[1:50]) %>%
  ggplot(aes(x = test_temp, y = response_value, colour = curve_ID)) +
  geom_point() 

write.csv(d, "processed-data/simulated-curves.csv", row.names = F)

# ============================================================
# classify datasets based on curve coverage 
# ============================================================
# from: 03_v2_curve_coverage.R

#### Script info ####
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.

### flagging that i want to replace catagory names in this script with those that match the figure ###

#### 1. load packages and data ####
library(dplyr)
library(tidyverse)
library(here)
library(ggforce)

#### 2. normalize all of the datasets so can work with scaled values for sorting ####
data_scaled <- d %>%
  select(curve_ID, test_temp, response_value) %>%
  group_by(curve_ID, test_temp) %>%
  mutate(mean_response = mean(response_value, na.rm = TRUE)) %>%  # mean at each temp, handles ind response curves
  ungroup() %>%
  group_by(curve_ID) %>%
  mutate(response_scaled = mean_response / max(mean_response, na.rm = TRUE)) %>%  # scale within curve
  ungroup() %>%
  distinct(curve_ID, test_temp, mean_response, response_scaled)

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

# # vis check #
# responses <- data_scaled %>%
#   select(curve_ID, Trait.Group, response_unit) %>%
#   distinct()
# curve_labels <- responses %>%
#   mutate(label = paste0(Trait.Group, " (", curve_ID, ")")) %>%
#   select(curve_ID, label) %>%
#   deframe()
# # ggplot() +
#   geom_point(data = data_scaled %>%
#                filter(curve_ID %in% difference2),
#              aes(x = test_temp, y = response_scaled)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
#                       labeller = labeller(curve_ID = curve_labels))

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
curve_ids <- c(unique(d$curve_ID))
setdiff(curve_ids, all)
setdiff(all, curve_ids)

distinct_curves <- d %>%
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
d <- d %>%
  left_join(dataset_types, join_by(curve_ID))

write.csv(d, file = here('processed-data', "fishtherm_curve_coverage_sorted_simulated.csv"))


#### 09. visualization ####
curves <- d

curves <- curves %>%
  select(n_unique_temps, curve_ID, dataset_type, topt_TF, thermal_min_TF, thermal_max_TF, increasing_side_TF, decreasing_side_TF) %>%
  distinct() %>%
  mutate(n_unique_temps_capped = ifelse(n_unique_temps >= 7, "7+", n_unique_temps))

## curve coverage figures ##

## want to order it by how much is most seen
curves <- curves %>%
  mutate(dataset_type = factor(
    dataset_type, levels = c("partial_decline_with_max","full_decline_with_opt", "partial_rise_with_min", "full_curve","partial_decline", "full_rise_with_opt", "irregular","partial_rise", "optimum_only"))) %>%
  mutate(n_unique_temps_capped = factor(n_unique_temps_capped,
                                        levels = c("7+", "6", "5", "4")))

counts <- curves %>%
  count(dataset_type)

a <- ggplot(data = curves, aes(x = dataset_type, fill = (n_unique_temps_capped))) +
  geom_bar(position = "stack", colour = "black",linewidth = 0.3) +
  scale_fill_manual(values = c("4"= "#CDEDF6","5" = "#208AAE","6" ="#70161E","7+" = "#0D2149")) +
  xlab(NULL) +
  ylab("Number of datasets") +
  scale_y_continuous(expand = expansion(mult = 0.00),
                     breaks = seq(0,150,25)) +
  scale_x_discrete(labels = c("full_curve" = "Full curve", "left_bound_withopt" = "T-min + T-opt","right_bound_withopt" = "T-max + T-opt", "topt" = "T-opt only", "left_bound" = "T-min only","right_bound" = "T-max only","unbounded_increasing" = "Unbound, inc", "unbounded_decreasing" = "Unbound, dec","irregular" = "Irregular")) +coord_flip() +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

a

# ============================================================
# fit TPC models and estimate parameters
# ============================================================
## from script: 02-TPCrtpc-fitting.R
#### 1. loading and installing packages and data ####
# load packages
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(dplyr)
library(here)

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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 

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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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
      mutate_all(round, 2) %>%
      mutate(curve_ID = curve_ids[i]) 
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

saveRDS(all_fits, file = here('processed-data', "model_fit_evaluations_simulations.RDS"))
saveRDS(all_preds, file = here('processed-data', "all_model_predictions_simulations.RDS"))
saveRDS(all_paramaters, file = here('processed-data', "all_model_params_simulations.RDS"))


# ==========================================
# select valid models and derive parameters
# ==========================================
# from: selecting-valid-models-and-deriving-params.R
library(here)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)

#### 01 load data ####
curves <- read.csv(here('processed-data', 'fishtherm_curve_coverage_sorted_simulated.csv'))
model_preds <- readRDS(here('processed-data', 'all_model_predictions_simulated.RDS'))
params <- readRDS(here('processed-data', 'all_model_params_simulated.RDS'))
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations_simulated.RDS'))


length(unique(model_preds$curve_ID)) #456

#### 03 filter out irregular datasets ####
irregular <- curves %>%
  filter(dataset_type == "irregular")
irregular_list <- c(unique(irregular$curve_ID)) #1 dataset

model_preds_1 <- model_preds %>%
  filter(!(curve_ID %in% irregular_list)) 
params_1 <- params %>%
  filter(!(curve_ID %in% irregular_list)) 
model_evaluations_1 <- model_evaluations %>%
  filter(!(curve_ID %in% irregular_list)) 

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
  ungroup()

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

length(unique(valid_models$curve_ID)) #455

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

length(unique(valid_models$curve_ID)) # #455

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
# ggplot() +
#   geom_point(data = curves,
#              aes(x = test_temp, y = response_value)) +
#   geom_line(data = valid_preds,
#             aes(x = test_temp, y = .fitted, colour = model)) +
#   geom_vline(data = valid_params,
#              aes(xintercept = ctmin)) +
#   geom_vline(data = valid_params,
#              aes(xintercept = ctmax)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 6, nrow = 6, page = 1) +
#   scale_color_manual(
#     values = c(
#       "johnsonlewin" = "slateblue", 
#       "lactin2" = "#4DAF4A",  
#       "oneill"= "magenta", 
#       "ratkowsky" = "yellow",  
#       "rezende" = "#A65628",  
#       "spain" = "royalblue3",  
#       "thomas" = "#999999",  
#       "weibull" = "black"  ,
#       "hinshelwood" = "aquamarine",
#       "briere" = "lightblue", 
#       "gaussian" = "maroon",
#       "quadratic" = "green"
#     )
#   ) +
#   theme_minimal() +
#   labs(x = "Test Temperature", y = "Response", color = "Model")

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
saveRDS(top_preds, here("processed-data", "top_model_predictions_simulated.RDS")) 

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

# ggplot() +
#   geom_point(data = curves %>% 
#                filter(curve_ID %in% breadth_topt_list),
#              aes(x = test_temp, y = response_value)) +
#   geom_line(data = top_model_preds %>%
#               filter(curve_ID %in% breadth_topt_list),
#             aes(x = test_temp, y = .fitted, colour = model)) +
#   geom_vline(data = top_params %>%
#                filter(curve_ID %in% breadth_topt_list),
#              aes(xintercept = topt)) +
#   facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 2) +
#   scale_color_manual(
#     values = c(
#       "johnsonlewin" = "slateblue", 
#       "lactin2" = "#4DAF4A",  
#       "oneill"= "magenta", 
#       "ratkowsky" = "yellow",  
#       "rezende" = "#A65628",  
#       "spain" = "royalblue3",  
#       "thomas" = "#999999",  
#       "weibull" = "black"  ,
#       "hinshelwood" = "aquamarine",
#       "briere" = "lightblue", 
#       "gaussian" = "maroon",
#       "quadratic" = "green"
#     )
#   ) +
#   theme_minimal() +
#   labs(x = "Test Temperature", y = "Response", color = "Model")



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
  distinct()

#could do this with dataset type - need to fix this im getting rid of data by replacing it with ctmin ?

params_with_curve_info <- params_with_curve_info %>%
  mutate(topt = ifelse(topt_TF == FALSE, NA, topt)) %>%
  mutate(ctmin = ifelse(thermal_min_TF == FALSE, NA, ctmin)) %>%
  mutate(ctmax = ifelse(thermal_max_TF == FALSE, NA, ctmax)) %>%
  mutate(thermal_tolerance = ifelse(thermal_tolerance_TF == FALSE, NA, thermal_tolerance)) %>%
  mutate(thermal_tolerance = ifelse(thermal_tolerance_TF == TRUE & (min_five_ok == FALSE | max_five_ok == FALSE), NA, thermal_tolerance)) %>%
  mutate(breadth = ifelse(breadth_TF == FALSE, NA, breadth)) %>%
  mutate(y_value_topt = ifelse(topt_TF == FALSE, NA, y_value_topt)) %>%
  mutate(y_value_ctmin = ifelse(thermal_min_TF == FALSE, NA, y_value_ctmin)) %>%
  mutate(y_value_ctmax = ifelse(thermal_max_TF == FALSE, NA, y_value_ctmax)) %>%
  select(-(c(rmax, e, eh, q10, thermal_safety_margin, skewness)))

saveRDS(params_with_curve_info, file = here("processed-data", "tpcs_with_fitted_params_simulated.RDS"))

# ========================================
# estimate activation energy
# ========================================
# from: fit-arrhenius-activation-energy.R
library(rTPC)

#### 01. load data ####
parameters <- readRDS(here("processed-data", "tpcs_with_fitted_params_simulated.RDS"))

#filter out irregular and decreasing to get datasets possibly eligible for act. eng
ee <- parameters %>%
  filter(dataset_type %in% c("full_curve", "full_rise_with_opt", "partial_rise", "optimum_only", 
                             "partial_rise_with_min"))


Ea_curves <- unique(ee$curve_ID)

curves <- d
sub_curves <- curves %>%
  filter(curve_ID %in% Ea_curves)
sub_curves <- sub_curves %>%
  select(curve_ID, test_temp, response_value) %>%
  distinct() %>%
  left_join(ee %>% select(curve_ID, topt_TF, topt))


library(dplyr)
library(purrr)

arrhenius_fits <- sub_curves %>%
  group_by(curve_ID) %>%
  group_split() %>%
  map_df(function(df){
    
    topt_val <- unique(df$topt)[1]
    
    # if Topt exists, use only the rising portion (T <= Topt).
    # if Topt is NA, use all observed temperatures.
    if (!is.na(topt_val)) {
      df <- df %>%
        filter(test_temp <= topt_val)
    }
    
    df <- df %>%
      mutate(K = ifelse(test_temp < 150,
                        test_temp + 273.15,
                        test_temp))
    if(nrow(df) < 4){
      return(tibble(
        curve_ID = unique(df$curve_ID),
        e_arr = NA_real_,
        lnc = NA_real_,
        AIC = NA_real_,
        RSS = NA_real_,
        n = nrow(df),
        converged = FALSE
      ))
    }
    
    fit <- try(
      nls(response_value ~ lnc * exp(e_arr/8.62e-05 * (1/median(K) - 1/K)),
          data = df,
          start = c(lnc = median(df$response_value, na.rm = TRUE),
                    e_arr = 1),
          algorithm = "port",
          lower = c(lnc = 0, e_arr = 0),
          upper = c(lnc = Inf, e_arr = 5)),
      silent = TRUE
    )
    
    if(inherits(fit, "try-error")){
      return(tibble(
        curve_ID = unique(df$curve_ID),
        e_arr = NA_real_,
        lnc = NA_real_,
        AIC = NA_real_,
        RSS = NA_real_,
        n = nrow(df),
        converged = FALSE
      ))
    }
    
    rss <- sum(residuals(fit)^2)
    tss <- sum((df$response_value - mean(df$response_value))^2)
    r2 <- 1 - rss/tss
    
    s <- summary(fit)$parameters
    e_est <- s["e_arr", "Estimate"]
    se_e  <- s["e_arr", "Std. Error"]
    tval  <- s["e_arr", "t value"]
    pval  <- s["e_arr", "Pr(>|t|)"]
    
    tibble(
      curve_ID = unique(df$curve_ID),
      e_arr = unname(coef(fit)["e_arr"]),
      lnc = unname(coef(fit)["lnc"]),
      AIC = AIC(fit),
      R2 = r2,
      pval = pval,
      n = nrow(df),
      converged = TRUE,
      temp_range = max(df$test_temp) - min(df$test_temp)
    )
  })

sub_curves <- sub_curves %>%
  left_join(arrhenius_fits, by = "curve_ID")
sub_curves_2 <- sub_curves %>%
  select(curve_ID, topt_TF, topt, e_arr, lnc, AIC, R2, pval, n, converged, temp_range) %>%
  distinct()
sub_curves_2 <- sub_curves_2 %>%
  filter(converged == TRUE)

sub_curves_3 <- sub_curves_2 %>% #235 (ask nikki ab this, going all the way down to 157 after filting for that pval)
  filter(R2 > 0.5)

mean(sub_curves_3$e_arr)

sub_curves_4 <- sub_curves_3 %>% #157
  filter(pval < 0.05)

mean(sub_curves_4$e_arr)

parameters1 <- parameters %>%
  left_join(sub_curves_4 %>% select(curve_ID, e_arr, n, R2, pval, temp_range), join_by(curve_ID)) %>%
  rename(R2_earr = R2) %>%
  rename(pval_earr = pval) %>%
  rename(n_earr = n) %>%
  select(curve_ID, model, topt, ctmin, ctmax, thermal_tolerance, breadth, y_value_topt, y_value_ctmax, y_value_ctmin, e_arr, R2_earr, pval_earr, everything()) %>%
  rename(tmin = ctmin) %>%
  rename(tmax = ctmax) %>%
  rename(y_value_tmin = y_value_ctmin) %>%
  rename(y_value_tmax = y_value_ctmax)

saveRDS(parameters1, here("processed-data", "tpcs_with_fitted_params_with_act_eng_simulated.RDS"))


# ========================================
# analyze variation in curve parameters 
# ========================================
library(cowplot)
library(ggridges)
theme_set(theme_bw())

## rename for ease of use
params = parameters1

## right now, all curve IDs are derived from single curve 
## so can directly compare parameters

## which models were best fit?
params %>%
  ggplot(aes(x = model)) +
  geom_bar() +
  coord_flip()
## mostly quadratic was top fit, but there is some variation
params$model[which(params$curve_ID == "1")] ## full data was fit with weibull 

## gather parameters into a single column
colnames(params)
params_long = params %>% 
  select(-y_value_topt, -y_value_tmin,-y_value_tmax) %>%
  gather(key = 'parameter', value = 'parameter_value', c(topt, tmin, tmax, thermal_tolerance, breadth, e_arr))

params_long <- params_long %>%
  mutate(n_temps = str_split_fixed(curve_ID, "_", 3)[,3]) %>%
  mutate(n_temps = ifelse(n_temps == "", NA, n_temps)) %>%
  mutate(curve_coverage = case_when(dataset_type == "full_curve" ~ "Full curve",
                                    dataset_type == "optimum_only" ~ "Optimum-only",
                                    dataset_type == "full_rise_with_opt" ~ "Full-rise-with-optimum",
                                    dataset_type == "full_decline_with_opt" ~ "Full-decline-with-optimum",
                                    dataset_type == "partial_rise_with_min" ~ "Partial-rise-with-min",
                                    dataset_type == "partial_decline_with_max" ~ "Partial-decline-with-max",
                                    dataset_type == "partial_rise" ~ "Partial-rise",
                                    dataset_type == "partial_decline" ~ "Partial-decline",
                                    dataset_type == "irregular" ~ "Irregular response",
                                    TRUE ~ NA)) %>%
  mutate(curve_coverage = ifelse(curve_ID == "1", NA, curve_coverage)) %>%
  mutate(expected = ifelse(curve_ID == "1", parameter_value, NA)) %>%
  mutate(curve_coverage = factor(curve_coverage, ordered = T, 
                                 levels = rev(c("Full curve", "Optimum-only", "Full-rise-with-optimum",
                                            "Full-decline-with-optimum", "Partial-rise-with-min",
                                            "Partial-decline-with-max", "Partial-rise", "Partial-decline",
                                            "Irregular response"))))

## plot params versus curve type
params_long %>%
  ggplot(aes(x = parameter, y = parameter_value, colour = dataset_type)) +
  geom_point() +
  facet_grid(n_temps~dataset_type) 

plot_data = params_long %>%
  group_by(parameter) %>%
  fill(expected, .direction = "downup") %>%
  ungroup() %>%
  mutate(parameter = case_when(parameter == "tmin" ~ "Tmin",
                               parameter == "topt" ~ "Topt",
                               parameter == "tmax" ~ "Tmax",
                               parameter == "e_arr" ~ "Ea",
                               parameter == "thermal_tolerance" ~ "Tolerance breadth",
                               parameter == "breadth" ~ "Performance breadth",
                               TRUE ~ NA)) %>%
  mutate(parameter = factor(.$parameter, ordered = T, levels = c("Tmin", "Topt", "Tmax", "Ea", 
                                                                 "Performance breadth", "Tolerance breadth"))) %>%
  mutate(y = interaction(curve_coverage, parameter)) %>%
  filter(!is.na(parameter_value)) %>%
  mutate(y = droplevels(y)) %>%
  filter(curve_ID != "1") 

df = data.frame(y = c("na1", "na2", "na3", "na4", "na5"),
                parameter_value = c(NA,NA,NA,NA, NA))

plot_data <- full_join(plot_data, df) %>%
  mutate(y = factor(y, ordered = T, 
                    levels = c("Partial-rise-with-min.Tmin", "Full-rise-with-optimum.Tmin",
                               "Full curve.Tmin" , "na1", "Full-decline-with-optimum.Topt","Full-rise-with-optimum.Topt",     
                               "Optimum-only.Topt", "Full curve.Topt", "na2", "Partial-decline-with-max.Tmax",  
                               "Full-decline-with-optimum.Tmax", "Full curve.Tmax" , "na3", "Partial-rise.Ea",                 
                               "Partial-rise-with-min.Ea", "Full-rise-with-optimum.Ea", "Optimum-only.Ea",               
                               "Full curve.Ea" , "na4", "Full-decline-with-optimum.Performance breadth", 
                               "Full-rise-with-optimum.Performance breadth", 
                               "Optimum-only.Performance breadth", "Full curve.Performance breadth",  "na5",
                               "Full curve.Tolerance breadth")))

## make separate panel for Ea since it is on a different scale 
pal = c(
  "#1B4332",
  "#2D6A4F",
  "#40916C",
  "#74C69D",
  "#B7B7A4",
  "#DDA15E",
  "#BC6C25"
)
pal2 <- c(
  "#023047",
  "#6BB7C9",
  "#9B5DE5"
)
pal3 <- c(
  "#3D405B",
  "#6D597A",
  "#B56576",
  "#E07A5F",
  "#F2CC8F",
  "#D4A373",
  "#A98467",
  "#6B705C",
  "#588157",
  "#344E41"
)
## make plot just to get a legend 
legend_plot = plot_data %>%
  mutate(curve_coverage = factor(curve_coverage, ordered = T, 
                                 levels = c("Full curve", "Optimum-only", "Full-rise-with-optimum",
                                            "Full-decline-with-optimum", "Partial-rise-with-min",
                                            "Partial-decline-with-max", "Partial-rise", "Partial-decline",
                                            "Irregular response"))) %>% 
  ggplot(aes(x = parameter_value, y = y, fill = curve_coverage)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.5,
                      linewidth = 0.1) +
  labs(y = "", x = "Parameter estimate (°C)", fill = "Curve coverage category") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_discrete(breaks = c("Full-rise-with-optimum.Tmin", "Optimum-only.Topt",
                              "Full-decline-with-optimum.Tmax", "Full-rise-with-optimum.Ea",
                              "Optimum-only.Performance breadth", "Full curve.Tolerance breadth"),
                   labels = c("Tmin", "Topt", "Tmax", "Ea", "Performance breadth", "Tolerance breadth")) +
  scale_fill_manual(values = pal[c(1,2,3,4,5,6,7)]) 
legend_plot
legend = get_legend(legend_plot)

plot_degC = plot_data %>%
  filter(parameter != "Ea") %>%
  mutate(curve_coverage = factor(curve_coverage, ordered = T, 
                                 levels = c("Full curve", "Optimum-only", "Full-rise-with-optimum",
                                            "Full-decline-with-optimum", "Partial-rise-with-min",
                                            "Partial-decline-with-max", "Partial-rise", "Partial-decline",
                                            "Irregular response"))) %>%
  ggplot(aes(x = parameter_value, y = y, fill = curve_coverage)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.5,
                      linewidth = 0.1) +
  labs(y = "", x = "Parameter estimate (°C)", fill = "Curve coverage category") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data %>% filter(parameter == "Tmin"),
               aes(x = expected, y = 0.5, yend = 3.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Topt"),
               aes(x = expected, y = 3.5, yend = 7.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Tmax"),
             aes(x = expected, y = 7.5, yend = 10.5)) +
  # geom_segment(data = plot_data %>% filter(parameter == "Ea"),
  #              aes(x = expected, y = 11, yend = 18)) +
  geom_segment(data = plot_data %>% filter(parameter == "Tolerance breadth"),
               aes(x = expected, y = 14.5, yend = 15.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Performance breadth"),
               aes(x = expected, y = 10.5, yend = 14.5)) +
  scale_y_discrete(breaks = c("Full-rise-with-optimum.Tmin", "Optimum-only.Topt",
                              "Full-decline-with-optimum.Tmax", "Full-rise-with-optimum.Ea",
                               "Optimum-only.Performance breadth", "Full curve.Tolerance breadth"),
                   labels = c("Tmin", "Topt", "Tmax", "Ea", "Performance breadth", "Tolerance breadth")) +
  scale_fill_manual(values = pal[c(1,2,3,4,5,6)]) + 
  theme(legend.position = "none")

plot_degC

plot_Ea = plot_data %>%
  filter(parameter == "Ea") %>%
  mutate(curve_coverage = factor(curve_coverage, ordered = T, 
                                 levels = c("Full curve", "Optimum-only", "Full-rise-with-optimum",
                                            "Full-decline-with-optimum", "Partial-rise-with-min",
                                            "Partial-decline-with-max", "Partial-rise", "Partial-decline",
                                            "Irregular response"))) %>%
  ggplot(aes(x = parameter_value, y = y, fill = curve_coverage)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.5,
                      linewidth = 0.1) +
  labs(y = "", x = "Parameter estimate (eV)", fill = "Curve coverage category") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data %>% filter(parameter == "Ea"),
               aes(x = expected, y = 0.5, yend = 5.5)) +
  scale_y_discrete(breaks = c("Full-rise-with-optimum.Ea"),
                   labels = c("Ea")) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = pal[c(1,2,3,5,7)])

plot_Ea

plots = plot_grid(plot_degC, plot_Ea,
          nrow = 2, 
          align = "v", 
          rel_widths = c(1,1),
          rel_heights = c(14, 5)) + 
  draw_label("Parameter", x = 0.02, y = 0.5, angle = 90, vjust = 1, size = 11)

panela = plot_grid(plots, legend, rel_widths = c(0.2, 0.1), ncol = 2)


## now make panel b: plot estimates by number of test temperatures 
plot_ntemp_degC = plot_data %>%
  mutate(n_temps = factor(n_temps, ordered = T, levels = c("6", "5", "4"))) %>%
  filter(parameter != "Ea") %>%
  mutate(y = interaction(n_temps, parameter)) %>%
  mutate(n_temps = factor(n_temps, ordered = T, levels = c("4", "5", "6"))) %>%
  ggplot(aes(x = parameter_value, y = y, fill = n_temps)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.5,
                      linewidth = 0.1) +
  labs(y = "", x = "Parameter estimate (°C)", fill = "Number\nof test\ntemperatures") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data %>% filter(parameter == "Tmin"),
               aes(x = expected, y = 0.5, yend = 3.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Topt"),
               aes(x = expected, y = 3.5, yend = 6.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Tmax"),
               aes(x = expected, y = 6.5, yend = 9.5)) +
  # geom_segment(data = plot_data %>% filter(parameter == "Ea"),
  #              aes(x = expected, y = 11, yend = 18)) +
  geom_segment(data = plot_data %>% filter(parameter == "Tolerance breadth"),
               aes(x = expected, y = 12.5, yend = 15.5)) +
  geom_segment(data = plot_data %>% filter(parameter == "Performance breadth"),
               aes(x = expected, y = 9.5, yend = 12.5)) +
  scale_y_discrete(breaks = c("5.Tmin", "5.Topt", "5.Tmax", "5.Performance breadth", "5.Tolerance breadth"),
                   labels = c("Tmin", "Topt", "Tmax", "Performance breadth", "Tolerance breadth")) +
  scale_fill_manual(values = pal2)

legend_ntemp = get_legend(plot_ntemp_degC)
plot_ntemp_degC = plot_ntemp_degC + theme(legend.position = "none")

plot_ntemp_Ea = plot_data %>%
  mutate(n_temps = factor(n_earr, ordered = T, levels = c("6", "5", "4"))) %>%
  filter(parameter == "Ea") %>%
  mutate(y = interaction(n_temps, parameter)) %>%
  mutate(n_temps = factor(n_temps, ordered = T, levels = c("4", "5", "6"))) %>%
  ggplot(aes(x = parameter_value, y = y, fill = n_temps)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|',
                      point_size = 1,
                      point_alpha = 0.5,
                      linewidth = 0.1) +
  labs(y = "", x = "Parameter estimate (eV)", fill =  "Number\nof test\ntemperatures") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data %>% filter(parameter == "Ea"),
               aes(x = expected, y = 0.5, yend = 3.5)) +
  scale_y_discrete(breaks = c("5.Ea"),
                   labels = c("Ea")) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = pal2)

plot_ntemp_Ea

plots = plot_grid(plot_ntemp_degC, plot_ntemp_Ea,
                  nrow = 2, 
                  align = "v", 
                  rel_widths = c(1,1),
                  rel_heights = c(14, 5)) + 
  draw_label("Parameter", x = 0.02, y = 0.5, angle = 90, vjust = 1, size = 11)

panelb = plot_grid(plots, legend_ntemp, rel_widths = c(0.2, 0.1), ncol = 2)

## now make panel c: colour by model family 
plot_data2 = plot_data %>%
  mutate(y = interaction(model, parameter)) %>% 
  filter(!is.na(parameter_value)) %>%
  mutate(y = droplevels(y)) 

df = data.frame(y = c("na1", "na2", "na3", "na4"),
                parameter_value = c(NA,NA,NA,NA))

plot_data2 <- full_join(plot_data2, df) %>%
  filter(!is.na(model)) %>% 
  mutate(y = factor(y, ordered = T, 
                    levels = c("briere.Tmin", "gaussian.Tmin", "hinshelwood.Tmin", "johnsonlewin.Tmin", "lactin2.Tmin", "oneill.Tmin", "quadratic.Tmin","rezende.Tmin",                    
                               "spain.Tmin", "thomas.Tmin","weibull.Tmin", "na1",
                               "briere.Topt","gaussian.Topt","hinshelwood.Topt",                
                               "johnsonlewin.Topt","lactin2.Topt", "oneill.Topt",                    
                               "quadratic.Topt","rezende.Topt","spain.Topt",                      
                               "thomas.Topt","weibull.Topt","na2",
                               "briere.Tmax", "gaussian.Tmax","johnsonlewin.Tmax",              
                               "lactin2.Tmax","oneill.Tmax","quadratic.Tmax",                  
                               "rezende.Tmax","spain.Tmax","thomas.Tmax",                     
                               "weibull.Tmax","na3", 
                               "briere.Ea","gaussian.Ea","lactin2.Ea",                      
                               "oneill.Ea","quadratic.Ea","rezende.Ea",                      
                               "spain.Ea","thomas.Ea","weibull.Ea",   
                               "johnsonlewin.Ea", "hinshelwood.Ea",
                               "briere.Performance breadth","gaussian.Performance breadth","johnsonlewin.Performance breadth",
                               "lactin2.Performance breadth","oneill.Performance breadth","quadratic.Performance breadth",   
                               "rezende.Performance breadth","thomas.Performance breadth","weibull.Performance breadth", 
                               "na4",
                               "briere.Tolerance breadth","gaussian.Tolerance breadth","johnsonlewin.Tolerance breadth",
                               "lactin2.Tolerance breadth",       
                               "oneill.Tolerance breadth","quadratic.Tolerance breadth","rezende.Tolerance breadth",
                               "thomas.Tolerance breadth",        
                               "weibull.Tolerance breadth"))) %>%
  group_by(model, parameter) %>%
  filter(n() > 3) %>%
  ungroup() %>%
  mutate(model = factor(model, ordered = T, 
                        levels = rev(c("briere", "gaussian", "johnsonlewin", "lactin2",
                                       "oneill", "quadratic", "rezende", "spain", "thomas", "weibull"))))

pal2 = hcl(h = seq(15, 315, length.out = 11), c = 60, l = 65)

plot_fam_degC = plot_data2 %>%
  filter(parameter != "Ea") %>%
  ggplot(aes(x = parameter_value, y = y, fill = model)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      linewidth = 0.1) +
  geom_point(shape = '|', size = 1, alpha = 0.5) +
  labs(y = "", x = "Parameter estimate (°C)", fill = "Model", shape = "Model") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data2 %>% filter(parameter == "Tmin"),
               aes(x = expected, y = 0.5, yend = 9.5)) +
  geom_segment(data = plot_data2 %>% filter(parameter == "Topt"),
               aes(x = expected, y = 9.5, yend = 19.5)) +
  geom_segment(data = plot_data2 %>% filter(parameter == "Tmax"),
               aes(x = expected, y = 19.5, yend = 29.5)) +
  geom_segment(data = plot_data2 %>% filter(parameter == "Performance breadth"),
               aes(x = expected, y = 29.5, yend = 38.5)) +
  geom_segment(data = plot_data2 %>% filter(parameter == "Tolerance breadth"),
               aes(x = expected, y = 38.5, yend = 45.5)) +
  scale_y_discrete(breaks = c("quadratic.Tmin", "quadratic.Topt", "quadratic.Tmax",
                              "quadratic.Performance breadth",
                              "quadratic.Tolerance breadth"),
                   labels = c("Tmin", "Topt", "Tmax", "Performance breadth", "Tolerance breadth")) +
  scale_fill_manual(values = pal2,
                    labels = rev(c("Briere2, 1999", "Gaussian, 1987", "Johnsonlewin, 1946",
                               "Lactin2, 1995","Oneill, 1972","Quadratic, 2008","Rezende, 2019",
                               "Spain, 1982","Thomas, 2012","Weibull, 1995"))) 

legend_fam = get_legend(plot_fam_degC)
plot_fam_degC = plot_fam_degC + theme(legend.position = "none")

plots = plot_grid(plot_fam_degC,
                  nrow = 1, 
                  align = "v", 
                  rel_widths = c(1),
                  rel_heights = c(14)) + 
  draw_label("Parameter", x = 0.02, y = 0.5, angle = 90, vjust = 1, size = 11)

panelc = plot_grid(plots, legend_fam, rel_widths = c(0.2, 0.1), ncol = 2)

## save panels a, b, c
panela
panelb
panelc

ggsave(panela, path = "figures", filename = "sensitivity_panela.pdf",
       width = 6, height = 7)
ggsave(panelb, path = "figures", filename = "sensitivity_panelb.pdf",
       width = 6, height = 7)
ggsave(panelc, path = "figures", filename = "sensitivity_panelc.pdf",
       width = 6, height = 7)


## breadth of temps sampled versus activation energy 

breadth_vs_ea = plot_data %>%
  mutate(temp_range = temp_range/43.17*100) %>% ## express width as % of thermal tolerance breadth
  mutate(temp_range_bin = cut(
    temp_range, 
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    include.lowest = TRUE
  )) %>% ## bin
  filter(temp_range_bin != "(10,20]") %>%
  filter(parameter == "Ea") %>%
  mutate(y = temp_range_bin) %>%
  ggplot(aes(x = parameter_value, y = y)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.5,
                      rel_min_height = 0.01,
                      linewidth = 0.1) +
  geom_point(shape = '|', size = 1, alpha = 0.5) +
  labs(y = "% of tolerance breadth sampled", x = "Activation energy estimate (eV)") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  geom_segment(data = plot_data %>% filter(parameter == "Ea"),
               aes(x = expected, y = 0.5, yend = 8.5)) +
  scale_y_discrete(labels = c("20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")) + 
  theme(legend.position = "none") 

ggsave(breadth_vs_ea, path = "figures", filename = "sensitivity_paneld.pdf",
       width = 2.75, height = 5.4)
