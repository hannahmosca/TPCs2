### this script is for testing hypoths ###
  #packages
  install.packages("lmerTest")
  library(lme4)
  library(lmerTest)
  library(performance)
  library(car)
  library(here)
  library(dplyr)
  library(tidyverse)
  library(terra)
  rm(list = ls())
  
  #### 01. load required data ####
  fitted_datasets <- readRDS(here('processed-data', 'tpcs_with_fitted_params_with_act_eng.RDS'))
  fitted_datasets <- fitted_datasets %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
  curves <- read.csv(here('processed-data', 'FishTherm.csv'))
  ##point data
  freshwater_points <- readRDS(here("processed-data", "my_points_freshwater_summary.RDS"))
  marine_points <- readRDS(here("processed-data", "my_points_sst_summary.RDS")) %>%
    rename(q_low = q2.5) %>%
    rename(q_high = q97.5)
  
  
  
  #### 02. combine all temperature data ####

freshwater_points <- freshwater_points %>%
    mutate(enviornment = "Freshwater") 
  
marine_points <- marine_points %>%
    mutate(enviornment = "Marine")
  
point_data_all <- rbind(freshwater_points, marine_points) %>%
  select(latitude, longitude, everything())

fits_with_temps <- fitted_datasets %>%
  left_join(point_data_all, join_by(latitude, longitude)) %>%
  select(-(land_or_sea))
  

####03. Averaging paramaters by 'group'//accounting for pseudorep. ####
collapsed_params <- fits_with_temps %>%
  left_join(curves %>% select(curve_ID, species_ID)) %>%
  distinct() %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(
    averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
    averaged_pbreadth = if (any(breadth_TF)) mean(breadth[breadth_TF], na.rm = TRUE) else NA_real_,
    averaged_tbreadth = if (any(thermal_tolerance_TF)) mean(thermal_tolerance[thermal_tolerance_TF], na.rm = TRUE) else NA_real_,
    averaged_e = if (any(!is.na(e_arr))) mean(e_arr, na.rm = TRUE) else NA_real_)  %>%
    ungroup()

length(unique(collapsed_params$averaged_topt)) #132

collapsed_params_unique <- collapsed_params %>%
  select(study_ID, Trait.Group, species_ID, averaged_e, averaged_topt, averaged_pbreadth, averaged_tbreadth, abs_latitude, latitude, mean, sd, enviornment, q_high) %>%
  distinct()


####04. linear model with Topt and Latitude ####
ggplot(data = collapsed_params_unique %>% filter(!is.na(averaged_topt)),
       aes(x = abs_latitude, y = averaged_topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


lat_avtopt_model <- lmer(averaged_topt ~ abs_latitude * enviornment + (1 | study_ID), 
                         data = collapsed_params_unique %>% filter(!is.na(averaged_topt)))

summary(lat_avtopt_model)

#plot fitted model 
## want to make sure only predicting on range of data
lat_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_topt)) %>%
  group_by(enviornment) %>%
  summarise(
    min_abs_lat = min(abs_latitude),
    max_abs_lat = max(abs_latitude))
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Freshwater"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Freshwater"],
                     length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Marine"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Marine"],
                     length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(lat_avtopt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(lat_avtopt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


avtopt_latitude <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_topt)), aes(x = abs_latitude, y = averaged_topt, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015,0.015))) +
  theme_classic(base_size = 16)
avtopt_latitude

ggsave("topt_latitude_lme.pdf", plot = avtopt_latitude, path = here("figures"), width = 6, height = 4)



#### 05. linear model of performance breadth with latitude ####
ggplot(data = collapsed_params_unique %>% filter(!is.na(averaged_pbreadth)),
       aes(x = abs_latitude, y = averaged_pbreadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


lat_avpbreadth_model <- lmer(averaged_pbreadth ~ abs_latitude * enviornment + (1 | study_ID), 
                         data = collapsed_params_unique %>% filter(!is.na(averaged_pbreadth)))

summary(lat_avpbreadth_model)
# more of a correlation in freshwater but not really signif

#plot fitted model 
## want to make sure only predicting on range of data
lat_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_pbreadth)) %>%
  group_by(enviornment) %>%
  summarise(
    min_abs_lat = min(abs_latitude),
    max_abs_lat = max(abs_latitude))
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Freshwater"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Freshwater"],
                     length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Marine"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Marine"],
                     length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(lat_avpbreadth_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(lat_avpbreadth_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


avpbreadth_latitude <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_pbreadth)), aes(x = abs_latitude, y = averaged_pbreadth, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Performance Breadth") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16)
avpbreadth_latitude


#### 06. linear model of tolerance breadth with latitude ####
ggplot(data = collapsed_params_unique %>% filter(!is.na(averaged_tbreadth)),
       aes(x = abs_latitude, y = averaged_tbreadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


lat_avtbreadth_model <- lmer(averaged_tbreadth ~ abs_latitude * enviornment + (1 | study_ID), 
                             data = collapsed_params_unique %>% filter(!is.na(averaged_tbreadth)))

summary(lat_avtbreadth_model)
# not signif

#plot fitted model 
## want to make sure only predicting on range of data
lat_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_tbreadth)) %>%
  group_by(enviornment) %>%
  summarise(
    min_abs_lat = min(abs_latitude),
    max_abs_lat = max(abs_latitude))
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Freshwater"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Freshwater"],
                     length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_lat[lat_range$enviornment=="Marine"],
                     lat_range$max_abs_lat[lat_range$enviornment=="Marine"],
                     length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(lat_avtbreadth_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(lat_avtbreadth_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


avtbreadth_latitude <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_tbreadth)), aes(x = abs_latitude, y = averaged_tbreadth, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Tolerance Breadth") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme(legend.position = "none")
avtbreadth_latitude

library(patchwork)

breadths_and_lat <- avpbreadth_latitude + avtbreadth_latitude

ggsave("breadths_and_lat_lme.pdf", plot = breadths_and_lat, path = here("figures"), width = 8, height = 4)


#### 07. linear model with performance breadth and tolerance breadth

ggplot(data = collapsed_params_unique %>% filter(!is.na(averaged_tbreadth)),
       aes(x = averaged_pbreadth, y = averaged_tbreadth, fill = "darkgrey")) +
  geom_point(alpha = 0.7) +
  theme_classic() +
  theme(legend.position = "none")



tolerance_and_breadth_model <- lmer(averaged_tbreadth ~ averaged_pbreadth + (1 | study_ID), 
                                    data = collapsed_params_unique %>%
                                      filter(!is.na(averaged_tbreadth)))

summary(tolerance_and_breadth_model)


#plot fitted model 
## want to make sure only predicting on range of data
pbreadth_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_tbreadth)) %>%
  summarise(
    min_pbreadth = min(averaged_pbreadth),
    max_pbreadth = max(averaged_pbreadth))
pbreadth_range
pred_grid <- data.frame(
  averaged_pbreadth = seq(pbreadth_range$min_pbreadth,
                pbreadth_range$max_pbreadth,
                length.out = 200))

pred_grid$pred <- predict(tolerance_and_breadth_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(tolerance_and_breadth_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

tolerance_and_breadth_graph <- ggplot(data = pred_grid, aes(x = averaged_pbreadth)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_tbreadth)),
             aes(x = averaged_pbreadth, y = averaged_tbreadth),
             colour = "darkgrey", size = 2, alpha = 0.65) +
  geom_line(aes(y = pred),
            color = "black", alpha = .8) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "black", alpha = 0.20) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  labs(x = "Performance Breadth", y = "Tolerance Breadth") +
  theme_classic(base_size = 16)

tolerance_and_breadth_graph

ggsave("tolerance_breadth_with_performance_breadth.png", plot = tolerance_and_breadth_graph, path = here("figures"), width = 4, height = 4.2)


#### 08. thermal optima and environmental temperature ####

## avg topts and meanenv. temp ##
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_topt)),
       aes(x = mean, y = averaged_topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


mean_avtopt_model <- lmer(averaged_topt ~ mean * enviornment + (1 | study_ID), 
                          data = collapsed_params_unique %>%
                            filter(!is.na(averaged_topt)))

summary(mean_avtopt_model)

## want to make sure only predicting on range of data
temp_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_topt)) %>%
  group_by(enviornment) %>%
  summarise(
    min_mean_temp = min(mean),
    max_max_temp = max(mean))
temp_range
fresh_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="Freshwater"],
             temp_range$max_max_temp[temp_range$enviornment=="Freshwater"],
             length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="Marine"],
             temp_range$max_max_temp[temp_range$enviornment=="Marine"],
             length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(mean_avtopt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(mean_avtopt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_mean_avtemp <- ggplot(data = pred_grid, aes(x = mean)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_topt)), aes(x = mean, y = averaged_topt, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Mean Habitat Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

topt_mean_avtemp
ggsave("topt_meantemp_lme.pdf", plot = topt_mean_avtemp, path = here("figures"), width = 4, height = 4)


### topt with extreme temperature ##
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_topt)),
       aes(x = q_high, y = averaged_topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


extreme_avtopt_model <- lmer(averaged_topt ~ q_high * enviornment + (1 | study_ID), 
                          data = collapsed_params_unique %>%
                            filter(!is.na(averaged_topt)))

summary(extreme_avtopt_model)

## want to make sure only predicting on range of data
temp_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_topt)) %>%
  group_by(enviornment) %>%
  summarise(
    min_mean_temp = min(q_high),
    max_max_temp = max(q_high))
temp_range
fresh_grid <- data.frame(
  q_high = seq(temp_range$min_mean_temp[temp_range$enviornment=="Freshwater"],
             temp_range$max_max_temp[temp_range$enviornment=="Freshwater"],
             length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  q_high = seq(temp_range$min_mean_temp[temp_range$enviornment=="Marine"],
             temp_range$max_max_temp[temp_range$enviornment=="Marine"],
             length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(extreme_avtopt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(extreme_avtopt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_extreme_temp <- ggplot(data = pred_grid, aes(x = q_high)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_topt)), aes(x = q_high, y = averaged_topt, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Upper Habitat Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015,0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "right")

topt_extreme_temp
ggsave("topt_extremetemp_lme.png", plot = topt_extreme_temp, path = here("figures"), width = 5, height = 4)

#### 09. performance breadth and variability ####
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_pbreadth)),
       aes(x = sd, y = averaged_pbreadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


sd_pbreadth_model <- lmer(averaged_pbreadth ~ sd * enviornment + (1 | study_ID), 
                             data = collapsed_params_unique %>%
                               filter(!is.na(averaged_pbreadth)))

summary(sd_pbreadth_model)

## want to make sure only predicting on range of data
temp_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_pbreadth)) %>%
  group_by(enviornment) %>%
  summarise(
    min_mean_temp = min(sd),
    max_max_temp = max(sd))
temp_range
fresh_grid <- data.frame(
  sd = seq(temp_range$min_mean_temp[temp_range$enviornment=="Freshwater"],
               temp_range$max_max_temp[temp_range$enviornment=="Freshwater"],
               length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  sd = seq(temp_range$min_mean_temp[temp_range$enviornment=="Marine"],
               temp_range$max_max_temp[temp_range$enviornment=="Marine"],
               length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(sd_pbreadth_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(sd_pbreadth_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


sd_pbreadth <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = collapsed_params_unique %>% filter(!is.na(averaged_pbreadth)), aes(x = sd, y = averaged_pbreadth, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Habitat Temp. Variation (SD)", y = "Performance Breadth") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

sd_pbreadth
ggsave("breadth_sd_lme.pdf", plot = sd_pbreadth, path = here("figures"), width = 4, height = 4)


#### 10. thermal safety margin ####
#want to look at sd and at latitude

TSM <- collapsed_params_unique %>%
  filter(!is.na(averaged_topt)) %>%
  mutate(TSM = averaged_topt - mean)

## abs latitude and TSM
ggplot(data = TSM,
       aes(x = abs_latitude, y = TSM, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


lat_TSM_model <- lmer(TSM ~ abs_latitude * enviornment + (1 | study_ID), 
                          data = TSM)

summary(lat_TSM_model)

## want to make sure only predicting on range of data
lat_range <- TSM %>%
  group_by(enviornment) %>%
  summarise(
    min_lat = min(abs_latitude),
    max_lat = max(abs_latitude))
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$enviornment=="Freshwater"],
                     lat_range$max_lat[lat_range$enviornment=="Freshwater"],
           length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$enviornment=="Marine"],
                     lat_range$max_lat[lat_range$enviornment=="Marine"],
           length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(lat_TSM_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(lat_TSM_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


TSM_lat <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = TSM, aes(x = abs_latitude, y = TSM, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Thermal Safety Margin") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

TSM_lat

ggsave("TSM_abslat_lme.pdf", plot = TSM_lat, path = here("figures"), width = 4, height = 4)

## TSM and environmental variability ##

## sd and TSM
ggplot(data = TSM,
       aes(x = sd, y = TSM, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


SD_TSM_model <- lmer(TSM ~ sd * enviornment + (1 | study_ID), 
                      data = TSM)

summary(SD_TSM_model)

## want to make sure only predicting on range of data
sd_range <- TSM %>%
  group_by(enviornment) %>%
  summarise(
    min_sd = min(sd),
    max_sd = max(sd))
sd_range
fresh_grid <- data.frame(
  sd = seq(sd_range$min_sd[sd_range$enviornment=="Freshwater"],
           sd_range$max_sd[sd_range$enviornment=="Freshwater"],
                     length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  sd = seq(sd_range$min_sd[sd_range$enviornment=="Marine"],
           sd_range$max_sd[sd_range$enviornment=="Marine"],
                     length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(SD_TSM_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(SD_TSM_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


TSM_SD <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = TSM, aes(x = sd, y = TSM, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Habitat Temp. Variation (SD)", y = "Thermal Safety Margin") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.016, 0.016))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
TSM_SD
ggsave("TSM_sd_lme.pdf", plot = TSM_SD, path = here("figures"), width = 4, height = 4)


#### 11. model summary ####
install.packages("sjPlot")
library(sjPlot)
library(webshot)

###models that are in main-text paper ###
tab_model(lat_avtopt_model, mean_avtopt_model, lat_TSM_model, SD_TSM_model, sd_pbreadth_model, show.stat = TRUE, show.se = TRUE, file = "linear_model_sum_update.html")

webshot("linear_model_sum_update.html", "linear_model_sum_update.pdf")


tab_model(lat_TSM_model, show.stat = TRUE, show.se = TRUE, file = "latitude_TSM_model.html")

tab_model(extreme_avtopt_model, show.stat = TRUE, show.se = TRUE, file = "extreme_topt_model.html")
webshot("extreme_topt_model.html", "extreme_topt_model.pdf")




#### 12. activation energy and environment ####


#### does activation energy decrease with latitude?

ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_e)),
       aes(x = abs_latitude, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_eng_lat_model <- lmer(averaged_e ~ abs_latitude * enviornment + (1 | study_ID), 
                     data = collapsed_params_unique %>% 
                       filter(!is.na(averaged_e)))

summary(act_eng_lat_model)

## want to make sure only predicting on range of data
lat_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_e)) %>%
  group_by(enviornment) %>%
  summarise(
    min_abs_latitude = min(abs_latitude),
    max_abs_latitude = max(abs_latitude))
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_latitude[lat_range$enviornment=="Freshwater"],
                     lat_range$max_abs_latitude[lat_range$enviornment=="Freshwater"],
           length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_abs_latitude[lat_range$enviornment=="Marine"],
                     lat_range$max_abs_latitude[lat_range$enviornment=="Marine"],
           length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(act_eng_lat_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(act_eng_lat_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


act_lat <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_e)), aes(x = abs_latitude, y = averaged_e, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Activation Energy (Ev)") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme(legend.position = "right")

act_lat
ggsave("activation_eng_and_latitude.png", plot = act_lat, path = here("figures"), width = 7, height = 4)

#### does activation energy decrease with temp?

ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_e)),
       aes(x = mean, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_eng_temp_model <- lmer(averaged_e ~ mean * enviornment + (1 | study_ID), 
                          data = collapsed_params_unique %>%
                            filter(!is.na(averaged_e)))

summary(act_eng_temp_model)

## want to make sure only predicting on range of data
temp_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_e)) %>%
  group_by(enviornment) %>%
  summarise(
    min_temp = min(mean),
    max_temp = max(mean))
temp_range
fresh_grid <- data.frame(
  mean = seq(temp_range$min_temp[temp_range$enviornment=="Freshwater"],
             temp_range$max_temp[temp_range$enviornment=="Freshwater"],
                     length.out = 200),
  enviornment = "Freshwater")
marine_grid <- data.frame(
  mean = seq(temp_range$min_temp[temp_range$enviornment=="Marine"],
             temp_range$max_temp[temp_range$enviornment=="Marine"],
                     length.out = 200),
  enviornment = "Marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(act_eng_temp_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(act_eng_temp_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


act_temp <- ggplot(data = pred_grid, aes(x = mean)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_e)), aes(x = mean, y = averaged_e, color = enviornment), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Average Enviornmental Temperature", y = "Activation Energy (Ev)") +
  scale_color_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

act_temp
act_temp + act_lat

#### 13. activation energy and performance breadth ####

ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_pbreadth)) %>%
                  filter(!is.na(averaged_e)),
       aes(x = averaged_pbreadth, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_bread_model <- lmer(averaged_e ~ averaged_pbreadth  + (1 | study_ID), 
                           data = collapsed_params_unique %>% 
                          filter(!is.na(averaged_pbreadth)) %>%
                          filter(!is.na(averaged_e)))

summary(act_bread_model)

## want to make sure only predicting on range of data
breadth_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_pbreadth)) %>%
  filter(!is.na(averaged_e)) %>%
  summarise(
    min_pbreadth = min(averaged_pbreadth),
    max_pbreadth = max(averaged_pbreadth))
breadth_range
pred_grid <- data.frame(
  averaged_pbreadth = seq(breadth_range$min_pbreadth,
                          breadth_range$max_pbreadth),
             length.out = 200)
pred_grid$pred <- predict(act_bread_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(act_bread_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


act_breadth <- ggplot(data = pred_grid, aes(x = averaged_pbreadth)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_pbreadth)) %>%
               filter(!is.na(averaged_e)), aes(x = averaged_pbreadth, y = averaged_e), color = "black", size = 2.5, alpha = .6) +
  geom_line(aes(y = pred), color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgrey", alpha = .3) +
  labs(x = "Performance Breadth", y = "Activation Energy (Ev)") +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
act_breadth

ggplot(data = collapsed_params_unique %>% 
         filter(!is.na(averaged_tbreadth)) %>%
         filter(!is.na(averaged_e)),
       aes(x = averaged_tbreadth, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_tbread_model <- lmer(averaged_e ~ averaged_tbreadth  + (1 | study_ID), 
                         data = collapsed_params_unique %>% 
                           filter(!is.na(averaged_tbreadth)) %>%
                           filter(!is.na(averaged_e)))

summary(act_tbread_model)

## want to make sure only predicting on range of data
breadth_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_tbreadth)) %>%
  filter(!is.na(averaged_e)) %>%
  summarise(
    min_tbreadth = min(averaged_tbreadth),
    max_tbreadth = max(averaged_tbreadth))
breadth_range
pred_grid <- data.frame(
  averaged_tbreadth = seq(breadth_range$min_tbreadth,
                          breadth_range$max_tbreadth),
  length.out = 200)
pred_grid$pred <- predict(act_tbread_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(act_tbread_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


act_tbreadth <- ggplot(data = pred_grid, aes(x = averaged_tbreadth)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_tbreadth)) %>%
               filter(!is.na(averaged_e)), aes(x = averaged_tbreadth, y = averaged_e), color = "black", size = 2.5, alpha = .6) +
  geom_line(aes(y = pred), color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.3) +
  labs(x = "Tolerance Breadth", y = "Activation Energy (Ev)") +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.015,0.015))) +
  scale_y_continuous(expand = expansion(mult = c(0.015, 0.015))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

Ae_breadth <- act_tbreadth + act_breadth
Ae_breadth
ggsave("Ae_breadth_lin_mod.pdf", plot = Ae_breadth, path = here("figures"), width = 7, height = 4)

tab_model(act_bread_model, act_tbread_model, show.stat = TRUE, show.se = TRUE, file = "Ae_breadth_model_sums.html")

library(webshot)
webshot("Ae_breadth_model_sums.html", "Ae_breadth_model_results.pdf")


#### 14. tolerance breadth and topt --hotterisbetter!####
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_tbreadth)) %>%
         filter(!is.na(averaged_topt)),
       aes(x = averaged_topt, y = averaged_tbreadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()



tol_breadth_topt <- lmer(averaged_topt ~ averaged_tbreadth  + (1 | study_ID), 
                        data = collapsed_params_unique %>% 
                          filter(!is.na(averaged_tbreadth)))

summary(tol_breadth_topt)

## want to make sure only predicting on range of data
tol_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_tbreadth)) %>%
  summarise(
    min_t = min(averaged_tbreadth),
    max_t = max(averaged_tbreadth))

pred_grid <- data.frame(
  averaged_tbreadth = seq(tol_range$min_t,
                      tol_range$max_t),
  length.out = 200)
pred_grid$pred <- predict(tol_breadth_topt, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(tol_breadth_topt, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


t_breadth_topt <- ggplot(data = pred_grid, aes(x = averaged_tbreadth)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_tbreadth)), 
             aes(x = averaged_tbreadth, y = averaged_topt), colour = "darkgrey", size = 2, alpha = .65) +
  geom_line(aes(y = pred), color = "black", alpha = .8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.20) +
  labs(x = "Tolerance Breadth", y = "Thermal Optima") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
  

t_breadth_topt


#### 15. topt and perf breadth ####
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_pbreadth)),
       aes(x = averaged_pbreadth, y = averaged_topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()



per_breadth_topt <- lmer(averaged_topt ~ averaged_pbreadth  + (1 | study_ID), 
                         data = collapsed_params_unique %>% 
                           filter(!is.na(averaged_pbreadth)))

summary(per_breadth_topt)

## want to make sure only predicting on range of data
p_range <- collapsed_params_unique %>%
  filter(!is.na(averaged_pbreadth)) %>%
  summarise(
    min_p = min(averaged_pbreadth),
    max_p = max(averaged_pbreadth))

pred_grid <- data.frame(
  averaged_pbreadth = seq(p_range$min_p,
                      p_range$max_p),
  length.out = 200)
pred_grid$pred <- predict(per_breadth_topt, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(per_breadth_topt, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


p_breadth_topt <- ggplot(data = pred_grid, aes(x = averaged_pbreadth)) +
  geom_point(data = collapsed_params_unique %>%
               filter(!is.na(averaged_pbreadth)), aes(x = averaged_pbreadth, y = averaged_topt), color = "darkgrey", size = 2, alpha = .65) +
  geom_line(aes(y = pred), color = "black", alpha = .8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Thermal Optima") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
library(patchwork)
topt_breadth <- t_breadth_topt + p_breadth_topt

topt_breadth


#### 16. residual topt and breadth ####
topts <- collapsed_params_unique %>%
  filter(!is.na(averaged_topt))
topts$resid_topt_lat = residuals(lat_avtopt_model)
## performance breadth ##
ggplot(data = topts %>%
         filter(!is.na(averaged_pbreadth)),
       aes(x = averaged_pbreadth, y = resid_topt_lat, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  theme_classic()

resid_topt_perbreadth <- lmer(resid_topt_lat ~ averaged_pbreadth  + (1 | study_ID), 
                         data = topts %>% 
                           filter(!is.na(averaged_pbreadth)))

summary(resid_topt_perbreadth)

## want to make sure only predicting on range of data
performance_range <- topts %>%
  filter(!is.na(averaged_pbreadth)) %>%
  summarise(
    min_p = min(averaged_pbreadth),
    max_p = max(averaged_pbreadth))

pred_grid <- data.frame(
  averaged_pbreadth = seq(performance_range$min_p,
                          performance_range$max_p),
  length.out = 200)
pred_grid$pred <- predict(resid_topt_perbreadth, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(resid_topt_perbreadth, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


resid_topt_perbreadth_graph <- ggplot(data = pred_grid, aes(x = averaged_pbreadth)) +
  geom_point(data = topts %>%
               filter(!is.na(averaged_pbreadth)), aes(x = averaged_pbreadth, y = resid_topt_lat), color = "darkgrey", size = 2, alpha = .65) +
  geom_line(aes(y = pred), color = "black", alpha = .8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Topt-adjusted for latitude") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

resid_topt_perbreadth_graph


## tolerance breadth ##
ggplot(data = topts %>%
         filter(!is.na(averaged_tbreadth)),
       aes(x = averaged_tbreadth, y = resid_topt_lat, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "#1F78B4", "Freshwater" = "#33A02C")
  ) +
  theme_classic()





resid_topt_tol <- lmer(resid_topt_lat ~ averaged_tbreadth  + (1 | study_ID), 
                              data = topts %>% 
                                filter(!is.na(averaged_tbreadth)))

summary(resid_topt_tol)

## want to make sure only predicting on range of data
tolerance_range <- topts %>%
  filter(!is.na(averaged_tbreadth)) %>%
  summarise(
    min_t = min(averaged_tbreadth),
    max_t = max(averaged_tbreadth))

pred_grid <- data.frame(
  averaged_tbreadth = seq(tolerance_range$min_t,
                          tolerance_range$max_t),
  length.out = 200)
pred_grid$pred <- predict(resid_topt_tol, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(resid_topt_tol, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


resid_topt_tolbreadth_graph <- ggplot(data = pred_grid, aes(x = averaged_tbreadth)) +
  geom_point(data = topts %>%
               filter(!is.na(averaged_tbreadth)), aes(x = averaged_tbreadth, y = resid_topt_lat), color = "darkgrey", size = 2, alpha = .65) +
  geom_line(aes(y = pred), color = "black", alpha = .8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.20) +
  labs(x = "Tolerance Breadth", y = "Topt-adjusted for latitude") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

adj_topt_breadth <- resid_topt_tolbreadth_graph + resid_topt_perbreadth_graph
adj_topt_breadth

ggsave("adjusted_topt_breadth_lin_mod.png", plot = adj_topt_breadth, path = here("figures"), width = 7, height = 4)

tab_model(tol_breadth_topt, resid_topt_tol, per_breadth_topt, resid_topt_perbreadth, show.stat = TRUE, show.se = TRUE)


