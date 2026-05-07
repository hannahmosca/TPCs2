#### this is a script for running sensitivity analyses requested from reviewers ####

#### Fit GAM to relationship between Topt and latitude and test to see if it is better/worse than linear model ####

#packages
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(here)
library(dplyr)
library(tidyverse)
rm(list = ls())

# load required data #
fitted_datasets <- readRDS(here('processed-data', 'tpcs_with_fitted_params_with_act_eng.RDS'))
fitted_datasets <- fitted_datasets %>%
  mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
curves <- read.csv(here('processed-data', 'FishTherm.csv'))
##point data
freshwater_points <- readRDS(here("processed-data", "my_points_freshwater_summary.RDS"))
marine_points <- readRDS(here("processed-data", "my_points_sst_summary.RDS")) %>%
  rename(q_low = q2.5) %>%
  rename(q_high = q97.5)

# combine all temperature data 
freshwater_points <- freshwater_points %>%
  mutate(enviornment = "Freshwater") 

marine_points <- marine_points %>%
  mutate(enviornment = "Marine")

point_data_all <- rbind(freshwater_points, marine_points) %>%
  select(latitude, longitude, everything())

fits_with_temps <- fitted_datasets %>%
  left_join(point_data_all, join_by(latitude, longitude)) %>%
  select(-(land_or_sea))


# averaging paramaters by 'group'//accounting for pseudorep
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

## first will show fitted linear model ##

## linear model with Topt and Latitude ##
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
plot(lat_avtopt_model)
qqnorm(residuals(lat_avtopt_model))
qqline(residuals(lat_avtopt_model))
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

## now i am fitting a GAM to topt and latitude ##
install.packages("mgcv")
library(mgcv)
gam_data <-  collapsed_params_unique %>%
  filter(
    !is.na(averaged_topt),
    !is.na(abs_latitude),
    !is.na(enviornment),
    !is.na(study_ID))
gam_data$enviornment <- as.factor(gam_data$enviornment)
gam_data$study_ID <- as.factor(gam_data$study_ID)
gam_data <- droplevels(gam_data)
lat_avtopt_gam <- gam(averaged_topt ~ s(abs_latitude, by = enviornment) + enviornment + s(study_ID, bs = "re"), data = gam_data, method = "REML")
summary(lat_avtopt_gam)
plot(lat_avtopt_gam, pages = 1)

AIC(lat_avtopt_model, lat_avtopt_gam)

