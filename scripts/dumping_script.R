### this script is for dumping things when organizing other scripts ###




fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))

ggplot(df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = q_low, ymax = q_high), fill = "lightgreen", alpha = .6, size = 1.2) +
  geom_line(aes (y = temp_mean), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "Latitude", y = "Water Temperature (°C)", title = "qlim=20") +
  theme_classic()


ggplot(temp_monthly_long, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = .5) +
  labs(
    x = "Experienced Temperatures",
    y = "Frequency") +
  theme_classic()

ggplot(temp_monthly_thresholded_long, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = .5) +
  labs(
    x = "Experienced Temperatures",
    y = "Frequency") +
  theme_classic()

df_coarse <- df %>%
  mutate(lat_bin = cut(y, breaks = seq(floor(min(y)),
                                       ceiling(max(y)),
                                       by = 1))) %>% 
  group_by(lat_bin) %>%
  summarise(
    latitude = mean(y, na.rm = TRUE),  
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_min = min(temp_min, na.rm = TRUE),
    temp_max = max(temp_max, na.rm = TRUE),
    q_low = quantile(q_low, probs = 0.025, na.rm = TRUE),
    q_high = quantile(q_high, probs = 0.975, na.rm = TRUE),
    temp_median = median(temp_median, na.rm = TRUE)
  )



###for now i am being conservative on what ctmin. and maxes we have--so if want to be more lenient can refilter the topt/irreg categories for thme later

#####DATA VIS CHECKING #####
responses <- curves %>%
  select(curve_ID, response_type, response_unit) %>%
  distinct()
curve_labels <- responses %>%
  mutate(label = paste0(response_type, " (", curve_ID, ")")) %>%
  select(curve_ID, label) %>%
  deframe()
library(ggforce)

ggplot() +
  geom_point(data = distinct_curves %>%
               filter(dataset_type == "topt"),
             aes(x = test_temp, y = response_value)) +
  # geom_point(data = params %>%
  #              filter(curve_ID %in% opt_list),
  #            aes(x = topt, y = y_value_topt, color = model)) +
  # geom_point(data = top_3_models %>%
  #              filter(curve_ID %in% opt_list),
  #            aes(x = CTmin, y = y_value_ctmin, color = model)) +
  # geom_point(data = top_3_models %>%
  #              filter(curve_ID %in% opt_list),
  #            aes(x = CTmax, y = y_value_ctmax, color = model)) +
  # geom_line(data = predictions %>%
  #             filter(curve_ID %in% opt_list) %>%
  #             filter(best_mod == "yes"), 
  #           aes(x = test_temp, y = .fitted, color = model), linewidth = 1) +
  # geom_line(data = predictions %>%
  #             filter(curve_ID %in% opt_list),
  #           aes(x = test_temp, y = .fitted, color = model), linewidth = .2) +
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
