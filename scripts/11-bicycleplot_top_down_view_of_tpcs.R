#### bicycle plot ####
library(here)
library(tidyverse)
library(ggplot2)
rm(list = ls())
# load data
predictions <- readRDS(here("processed-data", "top_model_predictions.RDS"))
params <- readRDS(here("processed-data", "tpcs_with_fitted_params_with_act_eng.RDS"))
curves <- read.csv(here("processed-data", "FishTherm.csv"))

params <- params %>%
  left_join(curves %>% select(curve_ID, species_ID, organization), join_by(curve_ID)) %>%
  distinct()

### lower and upper breadth bounds ###
breadth80 <- params %>%
  filter(breadth_TF == TRUE)
breadth80_list <- unique(breadth80$curve_ID)


  level <- 0.8
  breadth_summary80 <- predictions %>%
    filter(curve_ID %in% breadth80_list) %>%
    group_by(curve_ID, model) %>%
    summarise(
      rmax = unique(rmax), 
      threshold = level * rmax, 
      tmin_breadth80 = min(test_temp[round(.fitted, digits = 3) >= threshold], na.rm = TRUE),
      tmax_breadth80 = max(test_temp[round(.fitted, digits = 3) >= threshold], na.rm = TRUE),
      breadth80 = tmax_breadth80 - tmin_breadth80,
      .groups = "drop"
    )
  

  curve_params_with_breadth_adds <- params %>%
    left_join(breadth_summary80 %>% select("curve_ID", "tmin_breadth80", "tmax_breadth80", "breadth80"),
    by = "curve_ID")
  
  ## make min test temp and max test temp 
  curves <- curves %>%
    group_by(curve_ID) %>%
    mutate(min_test_temp = min(test_temp),
           max_test_temp = max(test_temp)) %>%
    ungroup
  curve_range <- curves %>%
    select(curve_ID, min_test_temp, max_test_temp) %>%
    distinct()
  curve_params_with_breadth_adds <- curve_params_with_breadth_adds %>%
    left_join(curve_range) %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial",
                                "freshwater",
                                land_or_sea))

 curve_params_with_breadth_adds[sapply(curve_params_with_breadth_adds, is.infinite)] <- NA
 
 curve_params_with_breadth_adds_avg <- curve_params_with_breadth_adds %>%
   group_by(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp) %>% #151
   mutate(
     averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
     averaged_tmin_breadth80 = if (any(breadth_TF)) mean(tmin_breadth80[breadth_TF], na.rm = TRUE) else NA_real_,
     averaged_tmax_breadth80 = if (any(breadth_TF)) mean(tmax_breadth80[breadth_TF], na.rm = TRUE) else NA_real_,
     averaged_thermal_min = if (any(thermal_min_TF)) mean(tmin[thermal_min_TF], na.rm = TRUE) else NA_real_,
     averaged_thermal_max = if (any(thermal_max_TF)) mean(tmax[thermal_max_TF], na.rm = TRUE) else NA_real_
   ) %>%
   mutate(min_min_test_temp = min(min_test_temp, na.rm = T),
          max_max_test_temp = max(max_test_temp, na.rm = T)) %>%
   mutate(new_dataset_type = ifelse(is.na(averaged_topt) & is.na(averaged_tmin_breadth80) &
                                      is.na(averaged_tmax_breadth80) & is.na(averaged_thermal_min)
                                    & is.na(averaged_thermal_max),
                                    NA, 
                                    "fine"),
          new_dataset_type = ifelse(is.na(new_dataset_type), paste0(unique(dataset_type), collapse = ", "), NA)) %>%
   ungroup()
 
 curve_params_with_breadth_adds_avg %>%
   # filter(!is.na(new_dataset_type)) %>%
   select(dataset_type, given_trait_name, species_ID, study_ID, latitude, Trait.Group, averaged_topt, 
          averaged_tmin_breadth80, averaged_tmax_breadth80, averaged_thermal_min, 
          averaged_thermal_max, min_min_test_temp, max_max_test_temp, land_or_sea,
          abs_latitude, everything()) 
 
 
 new_bike <- curve_params_with_breadth_adds_avg %>%
   select(new_dataset_type, species_ID, study_ID, latitude, Trait.Group, averaged_topt, 
          averaged_tmin_breadth80, averaged_tmax_breadth80, averaged_thermal_min, 
          averaged_thermal_max, min_min_test_temp, max_max_test_temp, land_or_sea,
          abs_latitude) %>%
   distinct() 
 
 new_bike_rank <- new_bike %>%
   group_by(land_or_sea) %>%
   arrange(abs_latitude) %>%
   mutate(lat_rank = row_number()) %>%
   ungroup() %>%
   mutate(land_or_sea = ifelse(land_or_sea == "freshwater", "Freshwater", land_or_sea),
          land_or_sea = ifelse(land_or_sea == "oceanic", "Marine", land_or_sea))
 
 # normalbike
 normalbike <- ggplot(data = new_bike_rank, aes(x = lat_rank)) +
   geom_segment(data = new_bike_rank %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "blue", alpha = .35, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .35, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1) +
   geom_segment(data = new_bike_rank %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "unbounded_increasing, unbounded_decreasing, irregular", "unbounded_increasing, irregular", "irregular, unbounded_increasing")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "grey", alpha = .7, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .35, linewidth = 1) +
   geom_point(data = new_bike_rank %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = lat_rank, y = averaged_topt), color = "black", alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = lat_rank, y = averaged_thermal_max), color = "red", alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = lat_rank, y = averaged_thermal_min), color = "blue", alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = lat_rank, y = averaged_tmin_breadth80), 
              color = "blue", shape = 2, alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = lat_rank, y = averaged_tmax_breadth80), 
              color = "red", shape = 2, alpha = .6, size = 1) +
   labs(x = "Ranked by Absolute Latitude", y = "Temperature", size = 12) +
   theme_classic() +
   facet_wrap(~ land_or_sea)  
 
 normalbike # faceteted 
 ggsave("bicycle_plot.png", plot = normalbike, path = here("figures"), width = 10, height = 5)
 
 ## full bike, not faceted but split and white space minimized
 new_bike_rank_freshwater <- new_bike_rank %>%
   filter(land_or_sea == "Freshwater")
 ctmin_sub_freshwater <- new_bike_rank_freshwater %>%
   filter(averaged_thermal_min<min_min_test_temp)
 ctmax_sub_freshwater <- new_bike_rank_freshwater %>%
   filter(averaged_thermal_max>max_max_test_temp)
 freswhater_bike <- ggplot(data = new_bike_rank_freshwater, aes(x = lat_rank)) +
   geom_segment(data = new_bike_rank_freshwater %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "blue", alpha = .35, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .35, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing, irregular", "unbounded_increasing, irregular", "irregular, unbounded_increasing")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "grey", alpha = .7, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .35, linewidth = 1.2) +
   geom_segment(data = new_bike_rank_freshwater %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .35, linewidth = 1.2) +
   geom_segment(data = ctmin_sub_freshwater,
                aes(x = lat_rank, y = averaged_thermal_min, yend = min_min_test_temp),
                color = "blue",alpha =.4,linetype="dashed", linewidth = .7) +
   geom_segment(data = ctmax_sub_freshwater,
                aes(x = lat_rank, y = averaged_thermal_max, yend = max_max_test_temp),
                color = "red",alpha =.4,linetype="dashed", linewidth = .7) +
   geom_point(data = new_bike_rank_freshwater %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = lat_rank, y = averaged_topt), color = "black", alpha = .6, size = 1.5) +
   geom_point(data = new_bike_rank_freshwater %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = lat_rank, y = averaged_thermal_max), color = "red", alpha = .6, size = 1.5) +
   geom_point(data = new_bike_rank_freshwater %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = lat_rank, y = averaged_thermal_min), color = "blue", alpha = .6, size = 1.5) +
   geom_point(data = new_bike_rank_freshwater %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = lat_rank, y = averaged_tmin_breadth80), 
              color = "blue", shape = 2, alpha = .6, size = 1.8) +
   geom_point(data = new_bike_rank_freshwater %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = lat_rank, y = averaged_tmax_breadth80), 
              color = "red", shape = 2, alpha = .6, size = 1.8) +
   scale_x_continuous(expand = expansion(mult = 0.01)) +
   scale_y_continuous(expand = expansion(mult = 0.015),
                      breaks = scales::pretty_breaks(n = 8)) +
   labs(x = NULL, y = NULL) +
   theme_classic() +
   theme(text = element_text(size = 16))
freswhater_bike
 
ggsave("freshwater_bike.png", plot = freswhater_bike, path = here("figures"), width = 6, height = 3)

##marine bike 
new_bike_rank_marine <- new_bike_rank %>%
  filter(land_or_sea == "Marine")
ctmin_sub_marine <- new_bike_rank_marine %>%
  filter(averaged_thermal_min<min_min_test_temp)
ctmax_sub_marine <- new_bike_rank_marine %>%
  filter(averaged_thermal_max>max_max_test_temp)
marine_bike <- ggplot(data = new_bike_rank_marine, aes(x = lat_rank)) +
  geom_segment(data = new_bike_rank_marine %>% #segment from min to topt (blue) 
                 filter(!is.na(averaged_topt)),
               aes(y = min_min_test_temp, yend = averaged_topt),
               color = "blue", alpha = .35, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>% #segment from topt to max (red) 
                 filter(!is.na(averaged_topt)),
               aes(y = averaged_topt, yend = max_max_test_temp),
               color = "red", alpha = .35, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>% #floating 'curves' that are cold edge only (blue)
                 filter(new_dataset_type == "unbounded_increasing"),
               aes(y = min_min_test_temp, yend = max_max_test_temp),
               color = "blue", alpha = .35, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>% #floating 'curves' that are warm edge only (red)
                 filter(new_dataset_type == "unbounded_decreasing"),
               aes(y = min_min_test_temp, yend = max_max_test_temp),
               color = "red", alpha = .35, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>%  #floating 'curves' that are not informative (grey)
                 filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "irregular, unbounded_increasing", "unbounded_increasing, irregular")),
               aes(y = min_min_test_temp, yend = max_max_test_temp),
               color = "grey", alpha = .7, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                 filter(is.na(averaged_topt)) %>%
                 filter(!is.na(averaged_thermal_max)),
               aes(y = min_min_test_temp, yend = max_max_test_temp),
               color = "red", alpha = .35, linewidth = 1.2) +
  geom_segment(data = new_bike_rank_marine %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                 filter(is.na(averaged_topt)) %>%
                 filter(!is.na(averaged_thermal_min)),
               aes(y = min_min_test_temp, yend = max_max_test_temp),
               color = "blue", alpha = .35, linewidth = 1.2) +
  geom_segment(data = ctmin_sub_marine,
               aes(x = lat_rank, y = averaged_thermal_min, yend = min_min_test_temp),
               color = "blue",alpha =.4,linetype="dashed", linewidth = .7) +
  geom_segment(data = ctmax_sub_marine,
               aes(x = lat_rank, y = averaged_thermal_max, yend = max_max_test_temp),
               color = "red",alpha =.4,linetype="dashed", linewidth = .7) +
  geom_point(data = new_bike_rank_marine %>% #topt (black)
               filter(!is.na(averaged_topt)),
             aes(x = lat_rank, y = averaged_topt), color = "black", alpha = .6, size = 1.5) +
  geom_point(data = new_bike_rank_marine %>% #thermal max (red)
               filter(!is.na(averaged_thermal_max)),
             aes(x = lat_rank, y = averaged_thermal_max), color = "red", alpha = .6, size = 1.5) +
  geom_point(data = new_bike_rank_marine %>% #thermal min (blue)
               filter(!is.na(averaged_thermal_min)),
             aes(x = lat_rank, y = averaged_thermal_min), color = "blue", alpha = .6, size = 1.5) +
  geom_point(data = new_bike_rank_marine %>%  #breadth min (blue)
               filter(!is.na(averaged_tmin_breadth80)),
             aes(x = lat_rank, y = averaged_tmin_breadth80), 
             color = "blue", shape = 2, alpha = .6, size = 1.8) +
  geom_point(data = new_bike_rank_marine %>% #breadth max (red)
               filter(!is.na(averaged_tmax_breadth80)),
             aes(x = lat_rank, y = averaged_tmax_breadth80), 
             color = "red", shape = 2, alpha = .6, size = 1.8) +
  scale_x_continuous(expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = 0.015),
                     breaks = scales::pretty_breaks(n = 8)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(text = element_text(size = 16))
marine_bike

ggsave("marine_bike.png", plot = marine_bike, path = here("figures"), width = 6, height = 3)



 extremes <- ggplot(data = new_bike_rank, aes(x = abs_latitude)) +
   geom_segment(data = new_bike_rank %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "lightgrey", alpha = .2, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "irregular, unbounded_increasing", "unbounded_increasing, irregular")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_point(data = new_bike_rank %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = abs_latitude, y = averaged_topt), color = "lightgrey", alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = abs_latitude, y = averaged_thermal_max), color = "red", alpha = .65, size = 2.5) +
   geom_point(data = new_bike_rank %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = abs_latitude, y = averaged_thermal_min), color = "blue", alpha = .65, size = 2.5) +
   geom_point(data = new_bike_rank %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = abs_latitude, y = averaged_tmin_breadth80), 
              color = "lightgrey", shape = 2, alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = abs_latitude, y = averaged_tmax_breadth80), 
              color = "lightgrey", shape = 2, alpha = .3, size = 1) +
   scale_x_continuous(expand = expansion(mult = 0.01)) +
   scale_y_continuous(expand = expansion(mult = 0.015),
                      breaks = scales::pretty_breaks(n = 8)) +
   labs(x = NULL, y = NULL) +
   theme_classic() +
   theme(text = element_text(size = 18)) +
   facet_wrap(~ land_or_sea)  
 
 extremes
 
 
 less_extremes <- ggplot(data = new_bike_rank, aes(x = abs_latitude)) +
   geom_segment(data = new_bike_rank %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .4, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "irregular, unbounded_increasing", "unbounded_increasing, irregular")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_point(data = new_bike_rank %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = abs_latitude, y = averaged_topt), color = "lightgrey", alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = abs_latitude, y = averaged_thermal_max), color = "lightgrey", alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = abs_latitude, y = averaged_thermal_min), color = "lightgrey", alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = abs_latitude, y = averaged_tmin_breadth80), 
              color = "blue", shape = 2, alpha = .65, size = 3) +
   geom_point(data = new_bike_rank %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = abs_latitude, y = averaged_tmax_breadth80), 
              color = "red", shape = 2, alpha = .65, size = 3) +
   scale_x_continuous(expand = expansion(mult = 0.01)) +
   scale_y_continuous(expand = expansion(mult = 0.015),
                      breaks = scales::pretty_breaks(n = 8)) +
   labs(x = NULL, y = NULL) +
   theme_classic() +
   theme(text = element_text(size = 18)) +
   facet_wrap(~ land_or_sea)  
 
 
 less_extremes
 
 

 cold_slopes <- ggplot(data = new_bike_rank, aes(x = abs_latitude)) +
   geom_segment(data = new_bike_rank %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "blue", alpha = .4, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "irregular, unbounded_increasing", "unbounded_increasing, irregular")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .2, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .4, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "blue", alpha = .3, linewidth = 1) +
   geom_point(data = new_bike_rank %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = abs_latitude, y = averaged_topt), color = "lightgrey", alpha = .2, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = abs_latitude, y = averaged_thermal_max), color = "lightgrey", alpha = .2, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = abs_latitude, y = averaged_thermal_min), color = "lightgrey", alpha = .2, size = 1) +
   geom_point(data = new_bike_rank %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = abs_latitude, y = averaged_tmin_breadth80), 
              color = "grey", shape = 2, alpha = .2, size = 1) +
   geom_point(data = new_bike_rank %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = abs_latitude, y = averaged_tmax_breadth80), 
              color = "grey", shape = 2, alpha = .2, size = 1) +
   scale_x_continuous(expand = expansion(mult = 0.01)) +
   scale_y_continuous(expand = expansion(mult = 0.015),
                      breaks = scales::pretty_breaks(n = 8)) +
   labs(x = NULL, y = NULL) +
   theme_classic() +
   theme(text = element_text(size = 18)) +
   facet_wrap(~ land_or_sea)  
 
 
 cold_slopes
 
 
 warm_slopes <- ggplot(data = new_bike_rank, aes(x = abs_latitude)) +
   geom_segment(data = new_bike_rank %>% #segment from min to topt (blue) 
                  filter(!is.na(averaged_topt)),
                aes(y = min_min_test_temp, yend = averaged_topt),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are cold edge only (blue)
                  filter(new_dataset_type == "unbounded_increasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>%  #floating 'curves' that are not informative (grey)
                  filter(new_dataset_type %in% c("irregular", "unbounded_increasing, unbounded_decreasing", "irregular, unbounded_increasing", "unbounded_increasing, irregular")),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal min (bounded increasing) (blue)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_min)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "lightgrey", alpha = .3, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #floating 'curves' that are warm edge only (red)
                  filter(new_dataset_type == "unbounded_decreasing"),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .4, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #segment from topt to max (red) 
                  filter(!is.na(averaged_topt)),
                aes(y = averaged_topt, yend = max_max_test_temp),
                color = "red", alpha = .4, linewidth = 1) +
   geom_segment(data = new_bike_rank %>% #no opt but bounded by thermal max (bounded decreasing) (red)
                  filter(is.na(averaged_topt)) %>%
                  filter(!is.na(averaged_thermal_max)),
                aes(y = min_min_test_temp, yend = max_max_test_temp),
                color = "red", alpha = .4, linewidth = 1) +
   geom_point(data = new_bike_rank %>% #topt (black)
                filter(!is.na(averaged_topt)),
              aes(x = abs_latitude, y = averaged_topt), color = "lightgrey", alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal max (red)
                filter(!is.na(averaged_thermal_max)),
              aes(x = abs_latitude, y = averaged_thermal_max), color = "lightgrey", alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #thermal min (blue)
                filter(!is.na(averaged_thermal_min)),
              aes(x = abs_latitude, y = averaged_thermal_min), color = "lightgrey", alpha = .6, size = 1) +
   geom_point(data = new_bike_rank %>%  #breadth min (blue)
                filter(!is.na(averaged_tmin_breadth80)),
              aes(x = abs_latitude, y = averaged_tmin_breadth80), 
              color = "grey", shape = 2, alpha = .3, size = 1) +
   geom_point(data = new_bike_rank %>% #breadth max (red)
                filter(!is.na(averaged_tmax_breadth80)),
              aes(x = abs_latitude, y = averaged_tmax_breadth80), 
              color = "grey", shape = 2, alpha = .3, size = 1) +
   scale_x_continuous(expand = expansion(mult = 0.01)) +
   scale_y_continuous(expand = expansion(mult = 0.015),
                      breaks = scales::pretty_breaks(n = 8)) +
   labs(x = NULL, y = NULL) +
   theme_classic() +
   theme(text = element_text(size = 18)) +
   facet_wrap(~ land_or_sea)    
 
warm_slopes

library(patchwork)
piecewise <- cold_slopes + extremes + warm_slopes + less_extremes
piecewise

ggsave("warm_slope.pdf", plot = warm_slopes, path = here("figures"), width = 5, height = 3)
ggsave("cold_slope.pdf", plot = cold_slopes, path = here("figures"), width = 5, height = 3)
ggsave("extremes.pdf", plot = extremes, path = here("figures"), width = 5, height = 3)
ggsave("less_extremes.pdf", plot = less_extremes, path = here("figures"), width = 5, height = 3)


ggsave("piecebike.pdf", plot = piecewise, path = here("figures"), width = 11.5, height = 7)




