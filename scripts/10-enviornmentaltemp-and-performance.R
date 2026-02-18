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
  #load data
  fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
  fitted_datasets <- fitted_datasets %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
  curves <- readRDS(here('processed-data', 'wild-tpcsupdated.Rds'))
  
  # ##all temperatures
  # #freshwater all
  # all_freshwater_rast <- rast((here("processed-data", "freshwater_summarized_masked.nc"))) #average masked across months from 1982-2025 %>%
  # names_temp <- c("mean", "sd", "min", "max", "q_low", "q_high")
  # names(all_freshwater_rast) <- names_temp
  # all_freshwater <- as.data.frame(all_freshwater_rast, xy = TRUE, na.rm = TRUE)
  # all_freshwater <- all_freshwater %>%
  #   rename(longitude = x) %>%
  #   rename(latitude = y)
  # #marine all
  # all_marine_rast <- rast((here("processed-data", "sst_monthly_summarized.nc"))) 
  # names(all_marine_rast) <- names_temp
  # all_marine <- as.data.frame(all_marine_rast, xy = TRUE, na.rm = TRUE)
  # all_marine <- all_marine %>%
  #   rename(longitude = x) %>%
  #   rename(latitude = y)
  
  ##point data
  freshwater_points <- readRDS(here("processed-data", "freshwater_temperatures_my_points_17_12_2025.RDS"))
  marine_points <- readRDS(here("processed-data", "marine_sst_summary_mypoints17_12_2025.RDS"))

  ##### combine all temperature data ####
  # all_freshwater <- all_freshwater %>%
  #   mutate(enviornment = "freshwater")
  freshwater_points <- freshwater_points %>%
    mutate(enviornment = "Freshwater") 
  # all_marine <- all_marine %>%
  #  mutate(enviornment = "marine")
  marine_points <- marine_points %>%
    mutate(enviornment = "Marine") %>%
    select(-(median))
  
point_data_all <- rbind(freshwater_points, marine_points) %>%
  select(latitude, longitude, everything())

fits_with_temps <- fitted_datasets %>%
  left_join(point_data_all, join_by(latitude, longitude))
  

#### Averaging paramaters by 'group' ####
collapsed_params <- fits_with_temps %>%
  left_join(curves %>% select(curve_ID, species_ID)) %>%
  distinct() %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(
    averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
    averaged_pbreadth = if (any(breadth_TF)) mean(breadth[breadth_TF], na.rm = TRUE) else NA_real_,
    averaged_tbreadth = if (any(thermal_tolerance_TF)) mean(thermal_tolerance[thermal_tolerance_TF], na.rm = TRUE) else NA_real_) %>%
  ungroup()

length(unique(collapsed_params$averaged_topt)) #132

collapsed_params_unique <- collapsed_params %>%
  select(study_ID, Trait.Group, species_ID, averaged_topt, averaged_pbreadth, averaged_tbreadth, abs_latitude, latitude, mean, sd, enviornment, q_high) %>%
  distinct()

# average_topts <- average_topts %>%
#   select(study_ID, Trait.Group, species_ID, averaged_topt, abs_latitude, latitude, mean, sd, enviornment, q_high) %>%
#   distinct() #136
# 
# 
# average_breadths <- fits_with_temps %>%
#   left_join(curves %>% select(curve_ID, species_ID)) %>%
#   distinct() %>%
#   filter(breadth_TF == TRUE) %>%
#   group_by(study_ID, species_ID, latitude, Trait.Group) %>% ## could also try cohort 
#   mutate(averaged_breadth = mean(breadth)) %>%
#   ungroup()
# 
# average_breadths <- average_breadths %>%
#   select(study_ID, Trait.Group, species_ID, averaged_breadth, abs_latitude, latitude, mean, sd, enviornment, q_high) %>%
#   distinct() #136


#rename so all capitals
collapsed_params_unique <- collapsed_params_unique %>%
  mutate(Trait.Group = case_when(
      Trait.Group == "metabolism"      ~ "Metabolism",
      Trait.Group == "reproduction"    ~ "Reproduction",
      Trait.Group == "somatic growth"  ~ "Somatic Growth",
      Trait.Group == "survival"        ~ "Survival",
      TRUE                             ~ Trait.Group
    )
  )


#### linear model with Topt and Latitude ####
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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16)
avtopt_latitude

ggsave("topt_latitude_lm.pdf", plot = avtopt_latitude, path = here("figures"), width = 6, height = 4)



#### linear model of performance breadth with latitude ####
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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16)
avpbreadth_latitude


#### linear model of tolerance breadth with latitude ####
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

summary(lat_avpbreadth_model)
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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(legend.position = "none")
avtbreadth_latitude

library(patchwork)

breadths_and_lat <- avpbreadth_latitude + avtbreadth_latitude

ggsave("breadths_and_lat_lm.pdf", plot = breadths_and_lat, path = here("figures"), width = 8, height = 4)


#### linear model with performance breadth and tolerance breadth

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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(x = "Performance Breadth", y = "Tolerance Breadth") +
  theme_classic(base_size = 16)

tolerance_and_breadth_graph

ggsave("tolerance_breadth_with_performance_breadth.png", plot = tolerance_and_breadth_graph, path = here("figures"), width = 4, height = 4.2)


#### thermal optima and enviornmental mean temperature ####

## avg topts and meanenv. temp
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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

topt_mean_avtemp
ggsave("topt_meantemp_lm.pdf", plot = topt_mean_avtemp, path = here("figures"), width = 4, height = 4)


#### topt with extreme temperature ####
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
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

topt_extreme_temp
ggsave("topt_extremetemp_lm.pdf", plot = topt_extreme_temp, path = here("figures"), width = 4, height = 4)

## breadth and sd
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
  scale_x_continuous(expand = expansion(mult = c(0.014,0.014))) +
  scale_y_continuous(expand = expansion(mult = c(0.014, 0.014))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

sd_pbreadth
ggsave("breadth_sdlm.pdf", plot = sd_pbreadth, path = here("figures"), width = 4, height = 4)





#### thermal safety margin ####
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
  scale_x_continuous(expand = expansion(mult = c(0.014,0.014))) +
  scale_y_continuous(expand = expansion(mult = c(0.014, 0.014))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

TSM_lat

ggsave("TSM_abslat_linear.pdf", plot = TSM_lat, path = here("figures"), width = 4, height = 4)

#### TSM and environmental variability ####

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
  scale_x_continuous(expand = expansion(mult = c(0.014,0.014))) +
  scale_y_continuous(expand = expansion(mult = c(0.014, 0.014))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
TSM_SD
ggsave("TSM_sd_linear.pdf", plot = TSM_SD, path = here("figures"), width = 4, height = 4)




#### model summary ####
install.packages("sjPlot")
library(sjPlot)
library(webshot)

###models that are in paper ###
tab_model(lat_avtopt_model, mean_avtopt_model, extreme_avtopt_model, sd_pbreadth_model, SD_TSM_model, show.stat = TRUE, show.se = TRUE, file = "linear_model_sum_update.html")
webshot("linear_model_sum_update.html", "linear_model_sum_update.pdf")
tab_model(lat_TSM_model, show.stat = TRUE, show.se = TRUE, file = "latitude_TSM_model.html")

#### activation energy and environment ####

activation_eng <- readRDS(here("processed-data", "activation_energy_subset.RDS"))

activation_eng <- activation_eng %>%
  filter(e<12) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(averaged_e = mean(e)) %>%
  ungroup()

##join with env temp ##
activation_eng <- activation_eng %>%
  left_join(point_data_all, join_by(latitude, longitude))

#### does activation energy decrease with latitude?

ggplot(data = activation_eng,
       aes(x = abs_latitude, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_eng_lat_model <- lmer(averaged_e ~ abs_latitude * enviornment + (1 | study_ID), 
                     data = activation_eng)

summary(act_eng_lat_model)

## want to make sure only predicting on range of data
lat_range <- activation_eng %>%
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
  geom_point(data = activation_eng, aes(x = abs_latitude, y = averaged_e, color = enviornment), size = 2, alpha = .65) +
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
  theme(legend.position = "none")

act_lat

act_lat + act_temp


#### does activation energy decrease with temp?

ggplot(data = activation_eng,
       aes(x = mean, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_eng_temp_model <- lmer(averaged_e ~ mean * enviornment + (1 | study_ID), 
                          data = activation_eng)

summary(act_eng_temp_model)

## want to make sure only predicting on range of data
temp_range <- activation_eng %>%
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
  geom_point(data = activation_eng, aes(x = mean, y = averaged_e, color = enviornment), size = 2, alpha = .65) +
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

#### activation energy and performance breadth ####
activation_eng <- activation_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(
    averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
    averaged_pbreadth = if (any(breadth_TF)) mean(breadth[breadth_TF], na.rm = TRUE) else NA_real_,
    averaged_tbreadth = if (any(thermal_tolerance_TF)) mean(thermal_tolerance[thermal_tolerance_TF], na.rm = TRUE) else NA_real_) %>%
  ungroup()


ggplot(data = activation_eng %>%
         filter(!is.na(averaged_pbreadth)),
       aes(x = averaged_pbreadth, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_bread_model <- lmer(averaged_e ~ averaged_pbreadth  + (1 | study_ID), 
                           data = activation_eng %>% 
                          filter(!is.na(averaged_pbreadth)))

summary(act_bread_model)

## want to make sure only predicting on range of data
breadth_range <- activation_eng %>%
  filter(!is.na(averaged_pbreadth)) %>%
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
  geom_point(data = activation_eng %>%
               filter(!is.na(averaged_pbreadth)), aes(x = averaged_pbreadth, y = averaged_e, color = "darkgrey"), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = "red")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "red"), alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Activation Energy (Ev)") +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
act_breadth

ggplot(data = activation_eng %>%
         filter(!is.na(averaged_tbreadth)),
       aes(x = averaged_tbreadth, y = averaged_e, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("Marine" = "blue", "Freshwater" = "lightgreen")
  ) +
  theme_classic()


act_tbread_model <- lmer(averaged_e ~ averaged_tbreadth  + (1 | study_ID), 
                        data = activation_eng %>% 
                          filter(!is.na(averaged_tbreadth)))

summary(act_tbread_model)

## want to make sure only predicting on range of data
breadth_range <- activation_eng %>%
  filter(!is.na(averaged_tbreadth)) %>%
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
  geom_point(data = activation_eng %>%
               filter(!is.na(averaged_tbreadth)), aes(x = averaged_tbreadth, y = averaged_e, color = "darkgrey"), size = 2, alpha = .65) +
  geom_line(aes(y = pred, color = "red")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "red"), alpha = 0.20) +
  labs(x = "Tolerance Breadth", y = "Activation Energy (Ev)") +
  theme_classic(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
Ae_breadth
Ae_breadth <- act_tbreadth + act_breadth
ggsave("Ae_breadth_lin_mod.pdf", plot = Ae_breadth, path = here("figures"), width = 7, height = 4)

tab_model(act_bread_model, act_tbread_model, show.stat = TRUE, show.se = TRUE, file = "Ae_breadth_model_sums.html")

library(webshot)
webshot("Ae_breadth_model_sums.html", "Ae_breadth_model_sums.pdf")


#### tolerance breadth and topt --hotterisbetter!####
ggplot(data = collapsed_params_unique %>%
         filter(!is.na(averaged_tbreadth)),
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
ggsave("topt_breadth_lin_mod.png", plot = topt_breadth, path = here("figures"), width = 7, height = 4)


#### residual topt and breadth ####
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

ggsave("adjusted_topt_breadth_lin_mod.png", plot = adj_topt_breadth, path = here("figures"), width = 7, height = 4)

tab_model(tol_breadth_topt, resid_topt_tol, per_breadth_topt, resid_topt_perbreadth, show.stat = TRUE, show.se = TRUE)


#### breadth and higher levels of organization #### (not enough power )
collapsed_params_organization <- fits_with_temps %>%
  left_join(curves %>% select(curve_ID, species_ID, organization)) %>%
  mutate(organization = ifelse(Trait.Group == "Reproduction", "population", organization)) %>%
  distinct() %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, organization) %>%
  mutate(
    averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
    averaged_pbreadth = if (any(breadth_TF)) mean(breadth[breadth_TF], na.rm = TRUE) else NA_real_,
    averaged_tbreadth = if (any(thermal_tolerance_TF)) mean(thermal_tolerance[thermal_tolerance_TF], na.rm = TRUE) else NA_real_) %>%
  ungroup()


length(unique(collapsed_params_organization$averaged_pbreadth)) #74

collapsed_params_organization <- collapsed_params_organization %>%
  select(study_ID, Trait.Group, organization, species_ID, averaged_topt, averaged_pbreadth, averaged_tbreadth, abs_latitude, latitude, mean, sd, enviornment, q_high) %>%
  distinct()

ggplot(collapsed_params_organization %>% filter(!is.na(averaged_pbreadth)), aes(x = organization, y = averaged_pbreadth)) +
  geom_boxplot() +
  geom_point()

ggplot(collapsed_params_organization %>% filter(!is.na(averaged_tbreadth)), aes(x = organization, y = averaged_tbreadth)) +
  geom_boxplot() +
  geom_point()








#### dumping ####

topt_lat <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = land_or_sea)) +
  geom_point(alpha = 0.7) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_lat
topt_lat_realm <- lmer(topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE,
                                 !is.na(abs_latitude)))
#218
#89 studies

plot(residuals(topt_lat_realm))
qqnorm(resid(topt_lat_realm))
qqline(resid(topt_lat_realm))
hist(resid(topt_lat_realm))
summary(topt_lat_realm)
## predict 
library(ggeffects)
library(ggplot2)
## want to make sure only predicting on range of data
lat_range <- fits_with_temps %>%
  filter(topt_TF == TRUE, !is.na(abs_latitude)) %>%
  group_by(land_or_sea) %>%
  summarise(
    min_lat = min(abs_latitude),
    max_lat = max(abs_latitude)
  )
lat_range
fresh_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$land_or_sea=="freshwater"],
                     lat_range$max_lat[lat_range$land_or_sea=="freshwater"],
                     length.out = 200),
  land_or_sea = "freshwater"
)

marine_grid <- data.frame(
  abs_latitude = seq(lat_range$min_lat[lat_range$land_or_sea=="marine"],
                     lat_range$max_lat[lat_range$land_or_sea=="marine"],
                     length.out = 200),
  land_or_sea = "marine"
)

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(topt_lat_realm, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(topt_lat_realm, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_lat <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = abs_latitude, y = topt, color = land_or_sea), alpha = .6) +
  geom_line(aes(y = pred, color = land_or_sea)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = land_or_sea), alpha = 0.20) +
  labs(x = "Absolute Latitude", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_lat
ggsave("topt_lat_regression2.jpeg", plot = topt_lat, path = here("figures"), width = 5, height = 4)



#topt and enviornmental temp 
topt_mean_tm <- ggplot(data = fits_with_temps %>% 
         filter(topt_TF == TRUE),
       aes(x = mean, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Average Water Temperature",
    y = "Thermal Optima") +
  theme_classic()

topt_mean_tm
mean_topt_model <- lmer(topt ~ mean * enviornment + (1 | study_ID), 
                       data = fits_with_temps %>%
                         filter(topt_TF == TRUE))


plot(residuals(mean_topt_model))
qqnorm(resid(mean_topt_model))
qqline(resid(mean_topt_model))
summary(mean_topt_model)
hist(resid(mean_topt_model))
r2(mean_topt_model)
Anova(mean_topt_model) #significant


## want to make sure only predicting on range of data
temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_mean_temp = min(mean),
    max_max_temp = max(mean))
  temp_range
fresh_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="freshwater"],
                  temp_range$max_max_temp[temp_range$enviornment=="freshwater"],
                     length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  mean = seq(temp_range$min_mean_temp[temp_range$enviornment=="marine"],
                     temp_range$max_max_temp[temp_range$enviornment=="marine"],
                     length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(mean_topt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(mean_topt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_meantemp <- ggplot(data = pred_grid, aes(x = mean)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = mean, y = topt, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Average Water Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
topt_meantemp


ggsave("topt_mean_tmp_regression.pdf", plot = topt_meantemp, path = here("figures"), width = 5, height = 4)



## topt and extremes #
ggplot(data = fits_with_temps %>% 
                         filter(topt_TF == TRUE),
                       aes(x = q_high, y = topt, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "Extreme Water Temperature",
    y = "Thermal Optima") +
  theme_classic()


extremes_topt_model <- lmer(topt ~ q_high * enviornment + (1 | study_ID), 
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE))


plot(residuals(extremes_topt_model))
qqnorm(resid(extremes_topt_model))
qqline(resid(extremes_topt_model))
summary(extremes_topt_model)
hist(resid(extremes_topt_model))

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_upper_temp = min(q_high),
    max_upper_temp = max(q_high))
temp_range
fresh_grid <- data.frame(
  q_high = seq(temp_range$min_upper_temp[temp_range$enviornment=="freshwater"],
             temp_range$max_upper_temp[temp_range$enviornment=="freshwater"],
             length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  q_high = seq(temp_range$min_upper_temp[temp_range$enviornment=="marine"],
             temp_range$max_upper_temp[temp_range$enviornment=="marine"],
             length.out = 200),
  enviornment = "marine")
pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(extremes_topt_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(extremes_topt_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit
pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


topt_extremetemp <- ggplot(data = pred_grid, aes(x = q_high)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = q_high, y = topt, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Extreme Water Temperature", y = "Thermal Optima") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic() +
  theme(
    legend.position = "none")
topt_extremetemp

topt_meantemp


library(patchwork)
plot <- topt_meantemp + topt_extremetemp
plot
ggsave("topt_envtemp2.jpeg", plot = plot, path = here("figures"), width = 10, height = 5)


##does how close your topt is to your env temp depend on how variable your en???? ### 
fits_with_temps <- fits_with_temps %>%
  mutate(diff_max = q_high - topt) %>% 
  mutate(diff_mean = topt - mean) %>%
  mutate(diff_max2 = topt-q_high)
#topt and enviornmental temp
dif <- fits_with_temps %>%
  pivot_longer(
    cols = c(diff_mean, diff_max),
    names_to = "diff_type",
    values_to = "diff_value"
  )
dif_top_en_his <- ggplot(dif %>% 
                           filter(topt_TF == TRUE) %>%
                           mutate(diff_type = factor(diff_type, 
                                                     levels = c("diff_mean", "diff_max"), 
                                                     labels = c("Mean", "Extreme")))) +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = enviornment)) +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(x = "Environmental Temperature", y = "Etemp - Topt") +
  theme_classic()

dif_top_en_his
ggsave("dif_his.pdf", plot = dif_top_en_his, path = here("figures"), width = 5, height = 4)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#topt is further above mean temp in more variable environments in marine systems

## mean diff (mean - topt)
var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = (topt-mean), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_mean_reg

var_dif_mean_model <- lmer(diff_mean ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))

plot(residuals(var_dif_mean_model))
qqnorm(resid(var_dif_mean_model))
qqline(resid(var_dif_mean_model))
hist(resid(var_dif_mean_model))
summary(var_dif_mean_model) #marine sig?

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_sd_temp = min(sd),
    max_sd_temp = max(sd))
temp_range
fresh_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="freshwater"],
             temp_range$max_sd_temp[temp_range$enviornment=="freshwater"],
             length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="marine"],
             temp_range$max_sd_temp[temp_range$enviornment=="marine"],
             length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(var_dif_mean_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(var_dif_mean_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


var_dif_mean_reg <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = sd, y = diff_mean, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "SD water temp", y = "topt - mean") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
var_dif_mean_reg



## extreme diff (q97.5 - topt)
var_dif_extreme_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = (diff_max2), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_extreme_reg

var_dif_extreme_model <- lmer(diff_max ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))

plot(residuals(var_dif_extreme_model))
qqnorm(resid(var_dif_extreme_model))
qqline(resid(var_dif_extreme_model))
hist(resid(var_dif_extreme_model))
summary(var_dif_extreme_model) #both sig

temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_sd_temp = min(sd),
    max_sd_temp = max(sd))
temp_range
fresh_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="freshwater"],
           temp_range$max_sd_temp[temp_range$enviornment=="freshwater"],
           length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  sd = seq(temp_range$min_sd_temp[temp_range$enviornment=="marine"],
           temp_range$max_sd_temp[temp_range$enviornment=="marine"],
           length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(var_dif_extreme_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(var_dif_extreme_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


var_dif_extreme_reg <- ggplot(data = pred_grid, aes(x = sd)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = sd, y = diff_max, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "SD water temp", y = "Experienced thermal extreme - thermal optimum") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
var_dif_extreme_reg

both_plot <- var_dif_mean_reg + var_dif_extreme_reg
both_plot
ggsave("topt_diff_envtemp.jpeg", plot = both_plot, path = here("figures"), width = 10, height = 5)




### performance breadth and tolerance breadth ###
tolerance <- ggplot(data = fits_with_temps %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = sd, y = thermal_tolerance, color = enviornment)) +
  geom_point(alpha = 0.7)  +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal breadth") +
  theme_classic()
tolerance

breadth <- lmer(breadth ~ sd*enviornment + (1 | study_ID),
                              data = fits_with_temps %>%
                                filter(breadth_TF == TRUE))
summary(breadth)
ggsave("tolerance_var.pdf", plot = tolerance, path = here("figures"), width = 5, height = 4)

#performance breadth should increase with var
breadth <- ggplot(data = fits_with_temps %>% 
         filter(breadth_TF == TRUE),
       aes(x = sd, y = breadth, color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
breadth
ggsave("breadth_var.pdf", plot = breadth, path = here("figures"), width = 5, height = 4)

#species in more variable enviornments should have larger performance breadths

mean_and_var_temp <- ggplot(data = fits_with_temps,
       aes(x = mean, y = sd, color = enviornment)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Average Temperature",
    y = "Variability (temp sd)") + 
  scale_color_manual(
  name = "Environment",
  values = c("marine" = "blue", "freshwater" = "lightgreen")
) +
  theme_classic()
mean_and_var_temp
ggsave("temp+mean+var.pdf", plot = mean_and_var_temp, path = here("figures"), width = 5, height = 4)

#show where breadths are in temperature space
library(nlme)
mean_breadth_model <- lme(breadth ~ temp_mean,
                     data = freshwater_temps %>%
                       filter(breadth_TF == TRUE),
                     random = ~ 1|study_ID)
plot(residuals(mean_breadth_model))
qqnorm(resid(mean_breadth_model))
qqline(resid(mean_breadth_model))
hist(resid(mean_breadth_model))
summary(mean_breadth_model)

var_breadth_model <- lme(breadth ~ sd*enviornment,
                          data = fits_with_temps %>%
                            filter(breadth_TF == TRUE),
                           random = ~ 1|study_ID)

summary(var_breadth_model)
plot(residuals(var_breadth_model))
qqnorm(resid(var_breadth_model))
qqline(resid(var_breadth_model))
hist(resid(var_breadth_model))
summary(var_breadth_model)
#tolerance breadth should increase with thermal variability




## deutsch warming tolerance - the difference between ctmax and mean env. temp
fits_with_temps <- fits_with_temps %>%
  mutate(warming_tolerance = ctmax - mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - mean)

##does how close your topt is to your env temp depend on latitude???? ### 
fits_with_temps <- fits_with_temps %>%
  mutate(diff_max = q97.5 - topt) %>% 
  mutate(diff_mean = topt - mean) 

###topt should be closer to mean water temp in the tropics (ie mag should increase with abs. latitude), where temps are higher (out of the tropics hyp)
thermal_saf <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = thermal_safety_margin_duetsch, color = enviornment)) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  scale_color_manual(
  name = "Environment",
  values = c("marine" = "blue", "freshwater" = "lightgreen")) +
  theme_classic()

thermal_saf

model <- lme((thermal_safety_margin_duetsch) ~ abs_latitude*enviornment,
                        data = fits_with_temps %>%
                          filter(topt_TF == TRUE),
                        random = ~ 1|study_ID)
summary(model)

diff_extreme <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = topt-q97.5, color = enviornment)) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
diff_extreme
ggsave("diff_extreme.pdf", plot = diff_extreme, path = here("figures"), width = 5, height = 4)


##okay so how close topt is to envir. temp decreases with latitude in freshwater fish

# warming tolerance - if topt is closer to env mean in tropics, warming tolerance should increase with lat
warming_tol <- ggplot(data = fits_with_temps %>%
                         filter(thermal_max_TF == TRUE),
                       aes(x = abs_latitude, y = warming_tolerance, color = enviornment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "warming tolerance (ctmax - temp_mean)") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
warming_tol

TSM <- ggplot(data = fits_with_temps %>%
                        filter(topt_TF == TRUE),
                      aes(x = abs_latitude, y = thermal_safety_margin_duetsch, color = enviornment)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(
    x = "abs latitude",
    y = "TSM (topt - temp_mean)") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()
TSM

TSM <- lmer(thermal_safety_margin_duetsch ~ abs_latitude*enviornment + (1 | study_ID),
            data = fits_with_temps %>%
              filter(topt_TF == TRUE))
summary(TSM)


temp_range <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(enviornment) %>%
  summarise(
    min_lat_temp = min(abs_latitude),
    max_lat_temp = max(abs_latitude))
temp_range
fresh_grid <- data.frame(
  abs_latitude = seq(temp_range$min_lat_temp[temp_range$enviornment=="freshwater"],
           temp_range$max_lat_temp[temp_range$enviornment=="freshwater"],
           length.out = 200),
  enviornment = "freshwater")
marine_grid <- data.frame(
  abs_latitude = seq(temp_range$min_lat_temp[temp_range$enviornment=="marine"],
           temp_range$max_lat_temp[temp_range$enviornment=="marine"],
           length.out = 200),
  enviornment = "marine")

pred_grid <- bind_rows(fresh_grid, marine_grid)
pred_grid$pred <- predict(TSM, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(TSM, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se


TSM_plot <- ggplot(data = pred_grid, aes(x = abs_latitude)) +
  geom_point(data = fits_with_temps %>% filter(topt_TF == TRUE), aes(x = abs_latitude, y = thermal_safety_margin_duetsch, color = enviornment), alpha = .6) +
  geom_line(aes(y = pred, color = enviornment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = enviornment), alpha = 0.20) +
  labs(x = "Latude", y = "Thermal Safety Margin (topt-mean)") +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  scale_fill_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
TSM_plot





ggsave("TSM_plot.pdf", plot = TSM_plot, path = here("figures"), width = 5, height = 4)



#topt and enviornmental temp
dif <- fits_with_temps %>%
  pivot_longer(
    cols = c(diff_mean, diff_max),
    names_to = "diff_type",
    values_to = "diff_value"
  )
dif_top_en_his <- ggplot(dif %>% 
                           filter(topt_TF == TRUE) %>%
                           mutate(diff_type = factor(diff_type, 
                                                     levels = c("diff_mean", "diff_max"), 
                                                     labels = c("Mean", "Extreme")))) +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = enviornment)) +
  scale_color_manual(
    name = "Realm",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  labs(x = "Environmental Temperature", y = "Etemp - Topt") +
  theme_classic()

dif_top_en_his
ggsave("dif_his.pdf", plot = dif_top_en_his, path = here("figures"), width = 5, height = 4)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
#topt is further above mean temp in more variable environments in marine systems

## mean diff (mean - topt)
var_dif_mean_reg <- ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE),
       aes(x = sd, y = (mean-topt), color = enviornment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_mean_reg

var_dif_mean_model <- lmer(diff_mean ~ sd*enviornment + (1 | study_ID),
             data = fits_with_temps %>%
               filter(topt_TF == TRUE))

plot(residuals(var_dif_mean_model))
qqnorm(resid(var_dif_mean_model))
qqline(resid(var_dif_mean_model))
hist(resid(var_dif_mean_model))
summary(var_dif_mean_model) #marine sig?






var_dif_max_reg <- ggplot(data = fits_with_temps %>%
                             filter(topt_TF == TRUE),
                           aes(x = sd, y = diff_max, color = enviornment)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()

var_dif_max_reg


var_dif_max_model <- lmer((topt-q97.5) ~ sd*enviornment + (1 | study_ID),
                           data = fits_with_temps %>%
                             filter(topt_TF == TRUE))






plot(residuals(var_dif_max_model))
qqnorm(resid(var_dif_max_model))
qqline(resid(var_dif_max_model))
hist(resid(var_dif_max_model))
summary(var_dif_mean_model)






### thermal extremes with latitude ###
ggplot(data = fits_with_temps) +
  geom_point(aes(x = abs_latitude, y = q_low, color = enviornment)) +
  geom_point(aes(x = abs_latitude, y = q_high, color = enviornment), shape = 2)+
  labs(
    x = "abs latitude",
    y = "extremes") +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()


## what about response type
# response types # 
res <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(response_type_group) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#top groups are swimming, metabolism, and growth
ggplot(data = fits_with_temps %>%
         filter(topt_TF == TRUE) %>%
         filter(response_type_group %in% c("swimming", "metabolism", "growth")) %>%
         filter(!(is.na(abs_latitude))),
       aes(x = abs_latitude, y = topt, color = response_type_group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = stats::lm) +
  labs(x = "Absolute Latitude", y = "Thermal Optimum", title = "Scatter of Topt and latitude with response type")

resp_topt_lat <- lmer(topt ~ abs_latitude * response_type_group + (1 | study_ID),
                      data = fits_with_temps %>%
                        filter(response_type_group %in% c("swimming", "metabolism", "growth")) %>%
                        filter(topt_TF == TRUE,
                               !is.na(abs_latitude)))
plot(residuals(resp_topt_lat))
qqnorm(resid(resp_topt_lat))
qqline(resid(resp_topt_lat))
hist(resid(resp_topt_lat))
summary(resp_topt_lat)
r2(resp_topt_lat)
Anova(resp_topt_lat) 

# looking at acclim temp #
#curve type information
curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(same_acclim_temp = case_when(
    all(is.na(acclim_temp)) ~ NA,
    all(acclim_temp == acclim_temp[1], na.rm = TRUE) ~ TRUE,
    TRUE ~ FALSE),
    curve_acclim_temp = if_else(same_acclim_temp == TRUE, acclim_temp[1], NA)) %>%
  ungroup()
curves <- curves %>%
  mutate(curve_acclim_temp = ifelse(str_detect(curve_acclim_temp, "-"), # split by "-" and compute row-wise mean
                                    rowMeans(do.call(rbind, str_split(curve_acclim_temp, "-", simplify = FALSE)) %>% apply(2, as.numeric)),
                                    as.numeric(curve_acclim_temp)))     
fitted_datasets <- fitted_datasets %>%
  left_join(curves %>% select(curve_type, curve_ID, same_acclim_temp, curve_acclim_temp), join_by(curve_ID)) %>%
  distinct()

ggplot(data = fitted_datasets %>% 
         filter(topt_TF == TRUE) %>%
         filter(same_acclim_temp == TRUE),
       aes(x = curve_acclim_temp, y = topt)) +
  geom_point(alpha = 0.7) +
  labs(x = "aclim", y = "Thermal Optimum",
       title = "Scatter of Topt and aclim")

aclim_lm <- lm(topt ~ curve_acclim_temp, data = fitted_datasets %>% 
                 filter(topt_TF == TRUE) %>%
                 filter(same_acclim_temp == TRUE))

summary(aclim_lm)



fits_with_temps <- fits_with_temps %>%
  left_join(breadth_summary %>% dplyr::select(curve_ID, model, tmin_breadth, tmax_breadth, my_breadth), join_by(curve_ID, model))

ggplot() +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE) %>%
               filter(enviornment == "marine"), 
             aes(x = abs_latitude, y = topt), color = "red") +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE) %>%
               filter(enviornment == "freshwater"), 
             aes(x = abs_latitude, y = topt), color = "red") +
  geom_point(data = fits_with_temps %>% 
               filter(breadth_TF == TRUE),
             aes(x = abs_latitude, y = mean), color = "black") +
  geom_linerange(data = fits_with_temps %>% 
                   filter(breadth_TF == TRUE) %>%
                   filter(enviornment == "marine"), 
                 aes(x = abs_latitude, ymin = tmin_breadth, ymax = tmax_breadth), color = "darkblue", linewidth = 1, alpha = .3) +
  geom_linerange(data = fits_with_temps %>% 
                   filter(breadth_TF == TRUE) %>%
                   filter(enviornment == "freshwater"), 
                 aes(x = abs_latitude, ymin = tmin_breadth, ymax = tmax_breadth), color = "darkgreen", linewidth = 1, alpha = .3)




