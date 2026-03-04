#### bicycle plot ####
library(here)
library(tidyverse)
library(ggplot2)
rm(list = ls())
# load data
predictions <- readRDS(here("processed-data", "top_model_predictions.RDS"))
curve_params <- readRDS(here("processed-data", "tpcs_with_fitted_params.RDS"))
curves <- readRDS(here("processed-data", "wild-tpcs-clean.RdS"))

curve_params <- curve_params %>%
  left_join(curves %>% select(curve_ID, organization), join_by(curve_ID)) %>%
  distinct()

### lower and upper breadth bounds ###
breadth80 <- curve_params %>%
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
  

  curve_params_with_breadth_adds <- curve_params %>%
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
   left_join(curves %>% select(curve_ID, species_ID), join_by(curve_ID)) %>%
   distinct() %>% 
   group_by(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp) %>% #151
   mutate(
     averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
     averaged_tmin_breadth80 = if (any(breadth_TF)) mean(tmin_breadth80[breadth_TF], na.rm = TRUE) else NA_real_,
     averaged_tmax_breadth80 = if (any(breadth_TF)) mean(tmax_breadth80[breadth_TF], na.rm = TRUE) else NA_real_,
     averaged_thermal_min = if (any(thermal_min_TF)) mean(ctmin[thermal_min_TF], na.rm = TRUE) else NA_real_,
     averaged_thermal_max = if (any(thermal_max_TF)) mean(ctmax[thermal_max_TF], na.rm = TRUE) else NA_real_
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



tmax_before_test_temp <- new_bike %>%
  filter(max_max_test_temp > averaged_thermal_max)


vis <- predictions %>%
  left_join(curves %>% select(curve_ID, study_ID, species_ID, Trait.Group), join_by(curve_ID)) %>%
  distinct() %>%
  select(curve_ID, test_temp, .fitted, species_ID) %>%
  left_join(curve_params) %>%
  distinct() 



#make groups based on collapse groups

vis <- vis %>%
  ungroup() %>%
  mutate(group = dense_rank(paste(study_ID, species_ID, latitude, Trait.Group)))

vis <- vis %>%
  left_join(new_bike_rank %>% select(averaged_topt, averaged_tmin_breadth80, averaged_tmax_breadth80, averaged_thermal_min, averaged_thermal_max, lat_rank, study_ID, species_ID, latitude, Trait.Group), join_by(study_ID, species_ID, latitude, Trait.Group))

vis_curves <- unique(vis$curve_ID)

vis_raw_data <- curves %>%
  filter(curve_ID %in% vis_curves)

vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(max_response = max(abs(response_value), na.rm = TRUE))
vis <- vis %>%
  left_join(vis_raw_data %>% select(curve_ID, max_response), join_by(curve_ID)) %>%
  distinct()
#normalize 
## add response value
vis <- vis %>%
  group_by(curve_ID) %>%
  mutate(.fitted_norm = .fitted / max_response) %>%
  ungroup()

vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(response_norm = response_value / max(abs(response_value), na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(vis %>% select(curve_ID, group), join_by(curve_ID)) %>%
  distinct()


#add thermalcolor
vis <- vis %>%
  group_by(curve_ID) %>%
  mutate(
    thermal_side = case_when(
      test_temp < averaged_topt ~ "cold",
      test_temp >=  averaged_topt ~ "warm"
    )
  ) %>%
  ungroup


vis_raw_data <- vis_raw_data %>%
  left_join(new_bike_rank %>% select(lat_rank, study_ID, species_ID, latitude, Trait.Group), join_by(study_ID, species_ID, latitude, Trait.Group)) %>%
  distinct()
  
  
facet_labels <- vis %>%
  select(group, study_ID, species_ID, lat_rank, Trait.Group) %>%
  distinct() %>%
  mutate(
    facet_label = paste0(
      "Study: ", study_ID,
      "Species: ", species_ID,
      "Trait: ", Trait.Group,
      "latitude: ", lat_rank
    )
  )
facet_labeller <- setNames(
  facet_labels$facet_label,
  facet_labels$group
)
vis_raw_data_fresh <- vis_raw_data %>%
  filter(land_or_sea == "terrestrial") %>%
  select(lat_rank, group, everything())
vis_fresh <- vis %>%
  filter(land_or_sea == "terrestrial") %>%
  select(lat_rank, group, everything())
vis_lines_fresh <- vis %>%
  filter(land_or_sea == "terrestrial") %>%
  select(lat_rank, group, everything())


facet_labels <- vis_fresh %>%
  select(group, study_ID, species_ID, lat_rank, Trait.Group) %>%
  distinct() %>%
  mutate(
    facet_label = paste0(
      "Study: ", study_ID,
      "Species: ", species_ID,
      "Trait: ", Trait.Group,
      "latitude: ", lat_rank
    )
  )
facet_labeller <- setNames(
  facet_labels$facet_label,
  facet_labels$group
)

vis_raw_data_marine <- vis_raw_data %>%
  filter(land_or_sea == "oceanic") %>%
  select(lat_rank, group, everything())
vis_marine <- vis %>%
  filter(land_or_sea == "oceanic") %>%
  select(lat_rank, group, everything())
vis_lines_marine <- vis %>%
  filter(land_or_sea == "oceanic") %>%
  select(lat_rank, group, everything())


facet_labels <- vis_marine %>%
  select(group, study_ID, species_ID, lat_rank, Trait.Group) %>%
  distinct() %>%
  mutate(
    facet_label = paste0(
      "Study: ", study_ID,
      "Species: ", species_ID,
      "Trait: ", Trait.Group,
      "latitude: ", lat_rank
    )
  )
facet_labeller <- setNames(
  facet_labels$facet_label,
  facet_labels$group
)

library(ggforce)
ggplot() +
  geom_point(
    data = vis_raw_data_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(x = test_temp, y = response_norm),
    color = "black",
    alpha = 0.4
  ) +
  geom_line(
    data = vis_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(
      x = test_temp,
      y = .fitted_norm,
      group = curve_ID,
      color = thermal_side),
    alpha = 0.4,
    linewidth = 1
  ) +
  geom_vline(
    data = vis_lines_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(xintercept = averaged_topt),
    color = "black",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    data = vis_lines_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(xintercept = averaged_thermal_min),
    color = "blue",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_lines_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(xintercept = averaged_thermal_max),
    color = "red",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_lines_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(xintercept = averaged_tmin_breadth80),
    color = "grey",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_lines_marine %>%
      filter(between(lat_rank, 50, 60)),
    aes(xintercept = averaged_tmax_breadth80),
    color = "grey",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  facet_wrap_paginate(~group, scales = "free", ncol = 3, nrow = 3, page = 2,
                      labeller = labeller(group = facet_labeller)
  ) +
  scale_color_manual(
    values = c(
      cold = "blue",
      warm = "red"
    ),
    guide = "none"   
  ) + theme(
    panel.grid = element_blank()
  )



vis_cold <- vis %>%
  filter(test_temp <= averaged_topt)

vis_warm <- vis %>%
  filter(test_temp >= averaged_topt)



example <- ggplot() +
  geom_vline(
    data = vis  %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_topt),
    color = "black",
    alpha = .6,
    linewidth = 1
  ) +
  geom_vline(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_thermal_min),
    color = "blue",
    alpha = 1,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_thermal_max),
    color = "red",
    alpha = 1,
    linewidth = 0.5
  ) +
  geom_point(
    data = vis %>% 
      filter(dataset_type == "full_curve"),
    aes(x = ctmin, y = y_value_ctmin),
    color = "blue",
    alpha = 1,
    size = 3
  ) +
  geom_point(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(x = ctmax, y = y_value_ctmax),
    color = "red",
    alpha = 1,
    size = 3
  ) +
  geom_vline(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_thermal_max),
    color = "red",
    alpha = 1,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_tmin_breadth80),
    color = "blue",
    alpha = 0.8,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_vline(
    data = vis  %>% 
      filter(dataset_type == "full_curve"),
    aes(xintercept = averaged_tmax_breadth80),
    color = "red",
    alpha = 0.8,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_point(
    data = vis_raw_data %>% 
      filter(dataset_type == "full_curve"),
    aes(x = test_temp, y = response_norm),
    color = "black",
    alpha = 0.7, 
    size = 3
  ) + geom_line(
    data = vis_cold %>%
      filter(curve_ID == 256),
    aes(
      x = test_temp,
      y = .fitted_norm,
      group = interaction(group, curve_ID)
    ),
    color = "blue",
    alpha = 0.4,
    linewidth = 2.5
  ) +
  geom_line(
    data = vis_warm  %>%
      filter(dataset_type == "full_curve"),
    aes(
      x = test_temp,
      y = .fitted_norm,
      group = interaction(group, curve_ID)
    ),
    color = "red",
    alpha = 0.4,
    linewidth = 2.5
  )+
  scale_color_manual(
    values = c(
      cold = "blue",
      warm = "red"),
    guide = "none") + 
  scale_x_continuous(expand = expansion(mult = 0.05),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(expand = expansion(mult = 0.2),
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(
    x = "T (°C)",
    y = "Performance",
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16))

example

ggsave("curve_256.pdf", plot = example, path = here("figures"), width = 4.5, height = 3)



try

SP9
2_0115
43.481843


SP129
2_0108
-36.26874


SP64
2_0025
42.60000

SP33
1_0033
45.07740


SP12
2_0075
47.60769

SP59
2_0017
48.13500


###pulling a full curve (single dataset) for figure 2

vis <- predictions %>%
  left_join(curves %>% select(curve_ID, study_ID, species_ID, Trait.Group), join_by(curve_ID)) %>%
  distinct() %>%
  select(curve_ID, test_temp, .fitted, species_ID) %>%
  left_join(curve_params) %>%
  distinct() 


vis_curves <- unique(vis$curve_ID)

vis_raw_data <- curves %>%
  filter(curve_ID %in% vis_curves) %>%
  left_join(vis %>% select(curve_ID, dataset_type)) %>%
  distinct()


#normalize 
vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(max_response = max(abs(response_value), na.rm = TRUE)) %>%
  distinct()
vis <- vis %>%
  left_join(vis_raw_data %>% select(curve_ID, max_response), join_by(curve_ID)) %>%
  distinct()

vis <- vis %>%
  group_by(curve_ID) %>%
  mutate(.fitted_norm = .fitted / max_response) %>%
  ungroup()

vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(response_norm = response_value / max(response_value, na.rm = TRUE)) %>%
  ungroup()

vis <- vis %>%
  arrange(curve_ID, .fitted_norm)

vis_lines <- vis %>%
  arrange(group, curve_ID, test_temp)

#add thermalcolor
vis <- vis %>%
  mutate(
    thermal_side = case_when(
      test_temp < topt ~ "cold",
      test_temp >= topt ~ "warm"
    )
  )


facet_labels <- vis %>%
  select(study_ID, species_ID, abs_latitude, Trait.Group, curve_ID, given_trait_name) %>%
  distinct() %>%
  mutate(
    facet_label = paste0(
      "Study: ", study_ID, "\n",
      "Species: ", species_ID,
      "Trait: ", given_trait_name,
      "curve: ", curve_ID
    )
  )
facet_labeller <- setNames(
  facet_labels$facet_label,
  facet_labels$curve_ID
)
library(ggforce)
ggplot() +
  geom_point(
    data = vis_raw_data %>%
      filter(dataset_type == "full_curve"), 
    aes(x = test_temp, y = response_norm),
    color = "black",
    alpha = 0.4
  ) +
  geom_line(
    data = vis %>%
      filter(dataset_type == "full_curve"), 
    aes(
      x = test_temp,
      y = .fitted_norm,
      color = thermal_side
    ),
    alpha = 0.4,
    linewidth = 1
  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = topt),
    color = "black",
    alpha = 0.8, 
    linewidth = 1) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = ctmin),
    color = "blue",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = ctmax),
    color = "red4",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = tmin_breadth80),
    color = "lightblue",
    alpha = 0.8,
    linewidth = 0.5  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(dataset_type == "full_curve"),
    aes(xintercept = tmax_breadth80),
    color = "red1",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  facet_wrap_paginate(~curve_ID, scales = "free", ncol = 3, nrow = 3, page = 3,
                      labeller = labeller(curve_ID = facet_labeller)
  ) +
  scale_color_manual(
    values = c(
      cold = "blue",
      warm = "red"
    ),
    guide = "none"   
  ) + theme(
    panel.grid = element_blank()
  )




#### singling out 
b <- ggplot() +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(curve_ID == 406),
    aes(xintercept = topt),
    color = "black",
    alpha = 0.8,
    linewidth = 1) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(curve_ID == 406),
    aes(xintercept = ctmin),
    color = "blue",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(curve_ID == 406),
    aes(xintercept = ctmax),
    color = "red",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(curve_ID == 406),
    aes(xintercept = tmin_breadth80),
    color = "blue",
    alpha = 0.8,
    linewidth = 1  ) +
  geom_vline(
    data = curve_params_with_breadth_adds %>%
      filter(curve_ID == 406),
    aes(xintercept = tmax_breadth80),
    color = "red",
    alpha = 0.8,
    linewidth = 1) +
  geom_line(
    data = vis %>%
      filter(curve_ID == 406),
    aes(
      x = test_temp,
      y = .fitted_norm,
      color = thermal_side
    ),
    alpha = .85,
    linewidth = 3
  ) +
  geom_point(
    data = vis_raw_data %>%
      filter(curve_ID == 406),
    aes(x = test_temp, y = response_norm),
    color = "black",
    alpha = 0.5,
    size= 4
  ) +
  scale_color_manual(
    values = c(
      cold = "blue",
      warm = "red"
    ),
    guide = "none"   
  ) + 
  theme_bw() +
  labs( x = "T (°C)", y = "Performance", size = 15) +
  scale_x_continuous(expand = expansion(mult = 0.1),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(expand = expansion(mult = 0.05),
                     breaks = scales::pretty_breaks(n = 5)) +
  theme( panel.grid = element_blank())
b
ggsave("curve_406.pdf", plot = b, path = here("figures"), width = 4.5, height = 3)





### looking at tmax after last temp tested curves ###
tmax_before_test_temp <- new_bike %>%
  filter(max_max_test_temp > averaged_thermal_max) %>%
  rename(min_test_temp = min_min_test_temp) %>%
  rename(max_test_temp = max_max_test_temp)


vis <- predictions %>%
  left_join(curves %>% select(curve_ID, study_ID, species_ID, Trait.Group, min_test_temp, max_test_temp), join_by(curve_ID)) %>%
  distinct() %>%
  select(curve_ID, test_temp, .fitted, species_ID, min_test_temp, max_test_temp) %>%
  left_join(curve_params) %>%
  distinct() 

#make groups based on collapse groups

vis <- vis %>%
  ungroup() %>%
  mutate(group = dense_rank(paste(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp)))

##only the curves im concerned about
vis_sub <- tmax_before_test_temp %>%
  left_join(vis %>% select(curve_ID, group, test_temp, .fitted, study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp), join_by(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp))

vis_curves <- unique(vis_sub$curve_ID)

vis_raw_data <- curves %>%
  filter(curve_ID %in% vis_curves)

vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(max_response = max(abs(response_value), na.rm = TRUE))
vis_sub <- vis_sub %>%
  left_join(vis_raw_data %>% select(curve_ID, max_response), join_by(curve_ID)) %>%
  distinct()
#normalize 
## add response value
vis_sub <- vis_sub %>%
  group_by(curve_ID) %>%
  mutate(.fitted_norm = .fitted /max_response) %>%
  ungroup()

vis_raw_data <- vis_raw_data %>%
  group_by(curve_ID) %>%
  mutate(response_norm = response_value / max(abs(response_value), na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(vis %>% select(curve_ID, group), join_by(curve_ID)) %>%
  distinct()

vis_raw_data <- vis_raw_data %>%
  select(response_norm, response_value, everything())
#add thermalcolor
vis_sub <- vis_sub %>%
  group_by(curve_ID) %>%
  mutate(
    thermal_side = case_when(
      test_temp < averaged_topt ~ "cold",
      test_temp >=  averaged_topt ~ "warm"
    )
  ) %>%
  ungroup


vis_raw_data <- vis_raw_data %>%
  left_join(tmax_before_test_temp %>% select(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp), join_by(study_ID, species_ID, latitude, Trait.Group, min_test_temp, max_test_temp)) %>%
  distinct()


facet_labels <- vis_sub %>%
  select(group, study_ID, species_ID, Trait.Group) %>%
  distinct() %>%
  mutate(
    facet_label = paste0(
      "Study: ", study_ID,
      "Species: ", species_ID,
      "Trait: ", Trait.Group
    )
  )
facet_labeller <- setNames(
  facet_labels$facet_label,
  facet_labels$group
)




library(ggforce)
ggplot() +
  geom_point(
    data = vis_raw_data,
    aes(x = test_temp, y = response_norm),
    color = "black",
    alpha = 0.4
  ) +
  geom_line(
    data = vis_sub,
    aes(
      x = test_temp,
      y = .fitted_norm,
      group = curve_ID,
      color = thermal_side),
    alpha = 0.4,
    linewidth = 1
  ) +
  geom_vline(
    data = vis_sub,
    aes(xintercept = averaged_topt),
    color = "black",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    data = vis_sub,
    aes(xintercept = averaged_thermal_min),
    color = "blue",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_sub,
    aes(xintercept = averaged_thermal_max),
    color = "red",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_sub,
    aes(xintercept = averaged_tmin_breadth80),
    color = "grey",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  geom_vline(
    data = vis_sub,
    aes(xintercept = averaged_tmax_breadth80),
    color = "grey",
    alpha = 0.8,
    linewidth = 0.5
  ) +
  facet_wrap_paginate(~group, scales = "free", ncol = 3, nrow = 3, page = 2,
                      labeller = labeller(group = facet_labeller)
  ) +
  scale_color_manual(
    values = c(
      cold = "blue",
      warm = "red"
    ),
    guide = "none"   
  ) + theme(
    panel.grid = element_blank()
  )


