### lower and upper breadth bounds ###
predictions <- readRDS(here("processed-data", "top_model_predictions.RDS"))

trial_run <- predictions %>%
  filter(curve_ID == 225) %>%
  filter(model == "quadratic")

rmax <- unique(trial_run$rmax)
level <- 0.8
threshold <- level *rmax

above_thresh <- subset(trial_run, .fitted>=threshold)
tmin_breadth <- min(above_thresh$test_temp)
tmax_breadth <- max(above_thresh$test_temp)

breadth <- tmax_breadth - tmin_breadth

tmin_breadth
tmax_breadth
breadth

  #### now try a loop through all of them ####
  level <- 0.8
  breadth_summary <- predictions %>%
    group_by(curve_ID, model) %>%
    summarise(
      rmax = unique(rmax), 
      threshold = level * rmax, 
      tmin_breadth = min(test_temp[.fitted >= threshold], na.rm = TRUE),
      tmax_breadth = max(test_temp[.fitted >= threshold], na.rm = TRUE),
      breadth = tmax_breadth - tmin_breadth,
      .groups = "drop"
    )
  breadth_summary <- breadth_summary %>%
    left_join(predictions %>% select(dataset_type.x, curve_ID, model, topt, breadth), join_by(curve_ID, model)) %>%
    distinct() 
  breadth_summary <- breadth_summary %>%
    rename(modelcalc_breadth = breadth.y) %>%
    rename(my_breadth = breadth.x) %>%
    rename(dataset_type = dataset_type.x)


  #### get other param data ####
  fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
  fitted_datasets <- fitted_datasets %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
  fitted_datasets <- fitted_datasets %>%
    left_join(breadth_summary %>% dplyr::select(curve_ID, model, tmin_breadth, tmax_breadth, my_breadth), join_by(curve_ID, model))
  #### enviornmental temperature data ####
  
  #freshwater all
  all_freshwater_rast <- rast((here("processed-data", "freshwater_summarized_masked.nc"))) #average masked across months from 1982-2025 %>%
  names_temp <- c("mean", "sd", "min", "max", "q_low", "q_high")
  names(all_freshwater_rast) <- names_temp
  all_freshwater <- as.data.frame(all_freshwater_rast, xy = TRUE, na.rm = TRUE)
  all_freshwater <- all_freshwater %>%
    rename(longitude = x) %>%
    rename(latitude = y)
  #marine all
  all_marine_rast <- rast((here("processed-data", "sst_monthly_summarized.nc"))) 
  names(all_marine_rast) <- names_temp
  all_marine <- as.data.frame(all_marine_rast, xy = TRUE, na.rm = TRUE)
  all_marine <- all_marine %>%
    rename(longitude = x) %>%
    rename(latitude = y)
  
  line_data_freshwater <- fitted_datasets %>%
    filter(land_or_sea == "freshwater", thermal_tolerance_TF == TRUE) %>%
    select(curve_ID, latitude, ctmin, ctmax)
  

  ggplot(freshwater_coarse, aes(x = latitude)) +
    geom_ribbon(aes(ymin = low_q, ymax = high_q), fill = "lightgreen", alpha = 0.6, linewidth = 1.2) +
    geom_line(aes(y = mean_temp), color = "darkgreen", linewidth = 2) +
    geom_linerange(data = line_data,
                   aes(x = latitude, ymin = tmin_breadth, ymax = tmax_breadth),
                   color = "blue", linewidth = 1, alpha = .3) +
    geom_point(data = fitted_datasets %>% filter(land_or_sea == "freshwater", breadth_TF == TRUE),
               aes(x = latitude, y = topt), color = "red", alpha = 0.4) +
    labs(x = "latitude", y = "water temperature") +
    theme_classic()
  

    freshwater_coarse <- all_freshwater %>%
    mutate(lat_bin = cut(latitude, breaks = seq(floor(min(latitude)),
                                                ceiling(max(latitude)),
                                                by = .1))) %>%
    group_by(lat_bin) %>%
    summarise(
      latitude = mean(latitude, na.rm = TRUE),
      mean_temp = mean(mean, na.rm = TRUE),
      max_temp = max(max, na.rm = TRUE),
      min_temp = min(min, na.rm = TRUE),
      low_q = quantile(q_low, probs = 0.025, na.rm = TRUE),
      high_q = quantile(q_high, probs = 0.975, na.rm = TRUE))
  
    
    ## marine
    
    line_data_marine <- fitted_datasets %>%
      filter(land_or_sea == "marine", thermal_tolerance_TF == TRUE) %>%
      select(curve_ID, latitude, ctmin, ctmax)
    
    
    ggplot(marine_coarse, aes(x = latitude)) +
      geom_ribbon(aes(ymin = low_q, ymax = high_q), fill = "lightblue1", alpha = 0.6, linewidth = 1.2) +
      geom_line(aes(y = mean_temp), color = "darkblue", linewidth = 2) +
      geom_linerange(data = line_data,
                     aes(x = latitude, ymin = tmin_breadth, ymax = tmax_breadth),
                     color = "darkgreen", linewidth = 1, alpha = .3) +
      geom_point(data = fitted_datasets %>% filter(land_or_sea == "marine", breadth_TF == TRUE),
                 aes(x = latitude, y = topt), color = "red", alpha = 0.4) +
      labs(x = "latitude", y = "water temperature") +
      theme_classic()
    
    
    marine_coarse <- all_marine %>%
      mutate(lat_bin = cut(latitude, breaks = seq(floor(min(latitude)),
                                                  ceiling(max(latitude)),
                                                  by = .1))) %>%
      group_by(lat_bin) %>%
      summarise(
        latitude = mean(latitude, na.rm = TRUE),
        mean_temp = mean(mean, na.rm = TRUE),
        max_temp = max(max, na.rm = TRUE),
        min_temp = min(min, na.rm = TRUE),
        low_q = quantile(q_low, probs = 0.025, na.rm = TRUE),
        high_q = quantile(q_high, probs = 0.975, na.rm = TRUE))
    
    
    #### point data ####
    freshwater_points <- readRDS(here("processed-data", "freshwater_temperatures_my_points.RDS"))
    marine_points <- readRDS(here("processed-data", "sst_temperatures_my_points.RDS"))
    
    
    
   marine <- ggplot(fits_with_temps %>% filter(thermal_tolerance_TF == TRUE) %>% filter(enviornment == "marine"), aes(x = latitude)) +
      geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), fill = "lightblue1", alpha = 0.6, linewidth = 1.2) +
      geom_line(aes(y = mean), color = "darkblue", linewidth = 2) +
      geom_linerange(data = line_data_marine,
                     aes(x = latitude, ymin = tmin_breadth, ymax = tmax_breadth),
                     color = "darkgreen", linewidth = 1, alpha = .3) +
      geom_point(data = fitted_datasets %>% filter(land_or_sea == "marine", breadth_TF == TRUE),
                 aes(x = latitude, y = topt), color = "red", alpha = 0.4) +
      labs(x = "latitude", y = "water temperature") +
      theme_classic()
    
   freshwater <- ggplot(fits_with_temps %>% filter(thermal_tolerance_TF == TRUE) %>% filter(enviornment == "freshwater"), aes(x = latitude)) +
      geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), fill = "lightgreen", alpha = 0.6, linewidth = 1.2) +
      geom_line(aes(y = mean), color = "darkgreen", linewidth = 2) +
      geom_linerange(data = line_data_freshwater,
                     aes(x = latitude, ymin = tmin_breadth, ymax = tmax_breadth),
                     color = "blue", linewidth = 1, alpha = .3) +
      geom_point(data = fitted_datasets %>% filter(land_or_sea == "freshwater", breadth_TF == TRUE),
                 aes(x = latitude, y = topt), color = "red", alpha = 0.4) +
      labs(x = "latitude", y = "water temperature") +
      theme_classic()
library(patchwork)  

marine+freshwater



