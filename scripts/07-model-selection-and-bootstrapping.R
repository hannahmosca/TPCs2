### filtering models and choosing the best ones? ### 
#### subset of breadth models where we are trying model averaging ####
rm(list = ls())
filtered_models_breadth <- readRDS(here('processed-data', 'full_curve_filtered_model_params_and_preds.RDS'))
curves <- readRDS(here('processed-data', 'wild-tpcs.RdS'))
model_evaluations <- readRDS(here('processed-data', 'model_fit_evaluations_01_10_25.RDS'))

filtered_models_all <- filtered_models_breadth %>%
  left_join(model_evaluations, join_by(curve_ID, model))
#add/calculate AIcc
filtered_models_all <- filtered_models_all %>% 
  group_by(curve_ID, model) %>%
  mutate(
    k = nobs - df.residual,  # number of estimated parameters
    AICc = AIC + (2 * k * (k + 1)) / (nobs - k - 1)
  ) %>%
  ungroup()
filtered_models_all <- filtered_models_all %>%
  group_by(curve_ID) %>%
  mutate(best_model = ifelse(AIC == min(AIC, na.rm = TRUE), "yes", "no"))

best_models_breadth <- filtered_models_all %>%
  filter(best_model == "yes") %>%
  select(-(test_temp)) %>%
  select(-(.fitted)) %>%
  distinct()

curve_info <- curves %>%
  select(curve_ID, curve_type, response_type, latitude, longitude, habitat, habitat_water) %>%
  distinct()

best_models_breadth <- best_models_breadth %>%
  left_join(curve_info, join_by(curve_ID))

ggplot(data = best_models_breadth, aes(x = habitat_water, y = breadth)) +
  geom_violin() +
  labs(title = "Violin Plot of breadth by habitat",
       x = "realm",
       y = "breadth")

responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
ggplot() +
  geom_point(data = curves %>%
               filter(curve_ID %in% curve_IDs),
             aes(x = test_temp, y = response_value)) +
  geom_line(data = filtered_models_all %>%
              filter(best_model == "no"), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = .2) +
  geom_line(data = filtered_models_all %>%
              filter(best_model == "yes"), 
            aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  geom_point(data = filtered_models_all %>%
              filter(best_model == "yes"), 
            aes(x = ctmin, y = y_value_ctmin, color = model)) +
  geom_point(data = filtered_models_all %>%
               filter(best_model == "yes"), 
             aes(x = ctmax, y = y_value_ctmax, color = model)) +
  geom_point(data = filtered_models_all %>%
               filter(best_model == "yes"), 
             aes(x = topt, y = y_value_topt, color = model)) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 4, nrow = 4, page = 1,
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

#### try to bootstrap pipeline from start ####
# # load packages
library(boot)
install.packages("car")
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)
 
library(nls.multstart)
library(nlstools)
library(broom)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# filter curve
curve_data <- filter(curves, curve_ID == 311)

# get start values and bounds
sv <- get_start_vals(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
if (is.matrix(sv)) sv <- sv[1, ]
start_lower <- sv - 10
start_upper <- sv + 10
lower <- get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
if (is.matrix(lower)) lower <- lower[1, ]
upper <- get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995')
if (is.matrix(upper)) upper <- upper[1, ]

# fit model using nls_multstart
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

# create new data for predictions
new_data <- tibble(test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), 0.5))

# generate predictions
d_preds <- augment(fit, newdata = new_data)

# plot raw data + predictions
ggplot() +
  geom_line(aes(test_temp, .fitted), data = d_preds, col = 'blue') +
  geom_point(aes(test_temp, response_value), data = curve_data, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = '311: Standard-growth-rate-weight')

# refit using nlsLM for bootstrapping
fit_nlsLM2 <- nlsLM(
  response_value ~ lactin2_1995(temp = test_temp, a, b, tmax, delta_t),
  data = curve_data,
  start = coef(fit),
  lower = get_lower_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995'),
  upper = get_upper_lims(curve_data$test_temp, curve_data$response_value, model_name = 'lactin2_1995'),
  control = nls.lm.control(maxiter = 500),
  weights = rep(1, nrow(curve_data))
)

# bootstrap using residual resampling
boot3 <- Boot(fit_nlsLM2, method = 'residual')

# keep iter as row number first
boot3_df <- as.data.frame(boot3$t) %>%
  drop_na() %>%
  mutate(iter = row_number())  # now each row is one bootstrap iteration

# create predictions for each iteration
boot3_preds <- boot3_df %>%
  rowwise() %>%
  mutate(
    preds = list(
      tibble(
        test_temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), length.out = 100),
        pred = lactin2_1995(
          temp = seq(min(curve_data$test_temp), max(curve_data$test_temp), length.out = 100),
          a = a,
          b = b,
          tmax = tmax,
          delta_t = delta_t
        )
      )
    )
  ) %>%
  select(iter, preds) %>%
  unnest(preds)

# calculate bootstrapped confidence intervals
boot3_conf_preds <- boot3_preds %>%
  group_by(test_temp) %>%
  summarise(
    conf_lower = quantile(pred, 0.025),
    conf_upper = quantile(pred, 0.975),
    .groups = "drop"
  )

# plot bootstrapped CIs + original fit
ggplot() +
  geom_line(aes(test_temp, .fitted), data = d_preds, col = 'blue') +
  geom_ribbon(aes(test_temp, ymin = conf_lower, ymax = conf_upper), data = boot3_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(test_temp, response_value), data = curve_data, size = 2) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rate with bootstrapped confidence intervals')

# optional: plot all bootstrap predictions for visualizing uncertainty
ggplot() +
  geom_line(aes(test_temp, pred, group = iter), data = boot3_preds, col = 'blue', alpha = 0.01) +
  geom_line(aes(test_temp, .fitted), data = d_preds, col = 'red') +
  geom_point(aes(test_temp, response_value), data = curve_data) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Bootstrapped predictions')

#get params
extra_params <- calc_params(fit_nlsLM2) %>%
pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM2, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM2)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)


ggplot(ci_extra_params, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Calculation of confidence intervals for extra parameters',
       subtitle = 'For the 311 growth TPC; using case resampling')
