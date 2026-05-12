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

#loading data
params <- readRDS(here('processed-data', 'tpcs_with_fitted_params_with_act_eng.RDS'))
curves <- read.csv(here('processed-data', 'FishTherm.csv'))
params <- params %>%
  mutate(environment = ifelse(land_or_sea == "terrestrial", "freshwater", "marine")) %>%
  select(-(land_or_sea))
params <- params %>%
  left_join(curves %>% select(species_ID, curve_ID, organization), join_by(curve_ID)) %>%
  distinct() 
params <- params %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("Metabolism", "Energy Aquisition", "Somatic Growth", "Locomotion", "Reproduction", "Survival"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "autonomic", "positive"))) %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population"))) %>%
  mutate(environment = factor(environment, levels = c("marine", "freshwater"))) %>%
  mutate(study_ID = as.factor(study_ID))
# load temperature data in #
freshwater_points <- readRDS(here("processed-data", "my_points_freshwater_summary.RDS"))
marine_points <- readRDS(here("processed-data", "my_points_sst_summary.RDS")) %>%
  rename(q_low = q2.5) %>%
  rename(q_high = q97.5)
freshwater_points <- freshwater_points %>%
  mutate(environment = "freshwater") 
marine_points <- marine_points %>%
  mutate(environment = "marine")
point_data_all <- rbind(freshwater_points, marine_points) %>%
  select(latitude, longitude, everything())
fits_with_temps <- params %>%
  left_join(point_data_all, join_by(latitude, longitude, environment))

# averaging topt by trait group only
average_topts_TG <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>% 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TG <- average_topts_TG %>%
  mutate(environment = factor(environment, levels = c("marine", "freshwater")))
average_topts_TG <- average_topts_TG %>%
  select(study_ID, Trait.Group, species_ID, averaged_topt, abs_latitude, latitude, environment, mean, sd) %>%
  distinct() #134



## first will show fitted linear model ##

## linear model with Topt and Latitude ##
ggplot(data = average_topts_TG,
       aes(x = abs_latitude, y = averaged_topt, color = environment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()


lat_avtopt_model <- lmer(averaged_topt ~ abs_latitude * environment + (1 | study_ID), 
                         data = average_topts_TG)

summary(lat_avtopt_model)
plot(lat_avtopt_model)
qqnorm(residuals(lat_avtopt_model))
qqline(residuals(lat_avtopt_model))

## now i am fitting a GAM to topt and latitude ##
install.packages("mgcv")
library(mgcv)

lat_avtopt_gam <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TG, method = "REML")

summary(lat_avtopt_gam)
plot(lat_avtopt_gam, pages = 1)

AIC(lat_avtopt_model, lat_avtopt_gam)

#### sensitivity//new analysis 2-- residuals from gam ###
average_topts_TG$gamresid_topt_lat = residuals(lat_avtopt_gam)


## get residuals for other groupings too ##

## averaging topt by trait group and motivation ##
average_topts_TM <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TM <- average_topts_TM %>%
  mutate(environment = factor(environment, levels = c("marine", "freshwater"))) %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_topt, abs_latitude, latitude, environment, mean, sd) %>%
  distinct() #147


lat_avtopt_TM_gam_model <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TM, method = "REML")

summary(lat_avtopt_TM_gam_model)
average_topts_TM$gamresid_topt_lat = residuals(lat_avtopt_TM_gam_model)

##averaging topt by trait group and motivation and organization## 

average_topts_TO <- fits_with_temps %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation, organization) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TO <- average_topts_TO %>%
  mutate(environment = factor(environment, levels = c("marine", "freshwater"))) %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_topt, abs_latitude, latitude, environment, mean, sd) %>%
  distinct() #149
lat_avtopt_TO_gam_model <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TO, method = "REML")

summary(lat_avtopt_TO_gam_model)
average_topts_TO$gamresid_topt_lat = residuals(lat_avtopt_TO_gam_model)
#### 03. summaries (mean/median/SE/95% CI) for overlay on ridge plots ####

TG_sum <- average_topts_TG %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_topt_r = median(gamresid_topt_lat,na.rm = TRUE),
    se = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )

TM_sum <- average_topts_TM %>%
  group_by(Trait.motivation) %>%
  summarise(
    mean_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_topt_r = median(gamresid_topt_lat,na.rm = TRUE),
    se = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )
TO_sum <- average_topts_TO %>%
  group_by(organization) %>%
  summarise(
    mean_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_topt_r = median(gamresid_topt_lat, na.rm = TRUE),
    se = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )
global_x_limits <- c(-20, 10)

library(ggridges)
trait.groups <- ggplot(average_topts_TG, aes(x = gamresid_topt_lat, y = Trait.Group, fill = Trait.Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.Group, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.Group),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.groups

trait.motivation <- ggplot(average_topts_TM, aes(x = gamresid_topt_lat, y = Trait.motivation,  # reorder on the fly
                                                 fill = Trait.motivation)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.motivation, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TM_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.motivation),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.motivation


trait.organization <- ggplot(average_topts_TO, aes(x = gamresid_topt_lat, y =organization,  # reorder on the fly
                                                   fill = organization)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = organization),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = organization),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 


trait.organization

library(patchwork)
combined_figure <-
  trait.groups +
  trait.motivation +
  trait.organization +
  plot_layout(ncol = 1, guides = "collect") +
  theme(
    axis.title.x = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5)
  )

combined_figure


#### residuals for thermal optima and environmental temperature ####

## avg topts and meanenv. temp ##
ggplot((data = average_topts_TG), aes(x = mean, y = averaged_topt, color = environment)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    name = "Environment",
    values = c("marine" = "blue", "freshwater" = "lightgreen")
  ) +
  theme_classic()


mean_temp_topt_TG_lmer_model <- lmer(averaged_topt ~ mean * environment + (1 | study_ID), 
                          data = average_topts_TG)
average_topts_TG$resid_topt_temp = residuals(mean_temp_topt_TG_lmer_model)

mean_temp_topt_gam_model <- gam(averaged_topt ~ s(mean, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TG, method = "REML")

summary(mean_temp_topt_gam_model)

AIC(mean_temp_topt_TG_lmer_model, mean_temp_topt_gam_model) # linear model of env temp and topt is better than gam for env temp and topt 

## get residuals for linear model of topt with temperature ##
mean_temp_topt_TM_lmer_model <- lmer(averaged_topt ~ mean * environment + (1 | study_ID), 
                                     data = average_topts_TM)
average_topts_TM$resid_topt_temp = residuals(mean_temp_topt_TM_lmer_model)

mean_temp_topt_TO_lmer_model <- lmer(averaged_topt ~ mean * environment + (1 | study_ID), 
                                     data = average_topts_TO)
average_topts_TO$resid_topt_temp = residuals(mean_temp_topt_TO_lmer_model)

#### 03. summaries (mean/median/SE/95% CI) for overlay on ridge plots ####

TG_sum <- average_topts_TG %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_temp_topt_r = mean(resid_topt_temp, na.rm = TRUE),
    median_temp_topt_r = median(resid_topt_temp,na.rm = TRUE),
    se_temp = sd(resid_topt_temp, na.rm = TRUE) / sqrt(n()),
    ci_low_temp = mean_temp_topt_r - 1.96 * se_temp,
    ci_high_temp = mean_temp_topt_r + 1.96 * se_temp,
    mean_lat_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_lat_topt_r = median(gamresid_topt_lat,na.rm = TRUE),
    se_lat = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low_lat = mean_lat_topt_r - 1.96 * se_lat,
    ci_high_lat = mean_lat_topt_r + 1.96 * se_lat,
    .groups = "drop"
  )

TM_sum <- average_topts_TM %>%
  group_by(Trait.motivation) %>%
  summarise(
    mean_temp_topt_r = mean(resid_topt_temp, na.rm = TRUE),
    median_temp_topt_r = median(resid_topt_temp,na.rm = TRUE),
    se_temp = sd(resid_topt_temp, na.rm = TRUE) / sqrt(n()),
    ci_low_temp = mean_temp_topt_r - 1.96 * se_temp,
    ci_high_temp = mean_temp_topt_r + 1.96 * se_temp,
    mean_lat_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_lat_topt_r = median(gamresid_topt_lat,na.rm = TRUE),
    se_lat = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low_lat = mean_lat_topt_r - 1.96 * se_lat,
    ci_high_lat = mean_lat_topt_r + 1.96 * se_lat,
    .groups = "drop"
  )
TO_sum <- average_topts_TO %>%
  group_by(organization) %>%
  summarise(
    mean_temp_topt_r = mean(resid_topt_temp, na.rm = TRUE),
    median_temp_topt_r = median(resid_topt_temp,na.rm = TRUE),
    se_temp = sd(resid_topt_temp, na.rm = TRUE) / sqrt(n()),
    ci_low_temp = mean_temp_topt_r - 1.96 * se_temp,
    ci_high_temp = mean_temp_topt_r + 1.96 * se_temp,
    mean_lat_topt_r = mean(gamresid_topt_lat, na.rm = TRUE),
    median_lat_topt_r = median(gamresid_topt_lat,na.rm = TRUE),
    se_lat = sd(gamresid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low_lat = mean_lat_topt_r - 1.96 * se_lat,
    ci_high_lat = mean_lat_topt_r + 1.96 * se_lat,
    .groups = "drop"
  )

global_x_limits <- c(-20, 20)

library(ggridges)
trait.groups <- ggplot(average_topts_TG, aes(x = resid_topt_temp, y = Trait.Group, fill = Trait.Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.Group, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.Group),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and temperature residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.groups

trait.motivation <- ggplot(average_topts_TM, aes(x = resid_topt_temp, y = Trait.motivation,  # reorder on the fly
                                                 fill = Trait.motivation)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.motivation, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TM_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.motivation),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and temperature residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.motivation


trait.organization <- ggplot(average_topts_TO, aes(x = resid_topt_temp, y =organization,  # reorder on the fly
                                                   fill = organization)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = organization),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = organization),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and temperature residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "blue",
                                "marine" = "green")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 


trait.organization

library(patchwork)
combined_figure <-
  trait.groups +
  trait.motivation +
  trait.organization +
  plot_layout(ncol = 1, guides = "collect") +
  theme(
    axis.title.x = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5)
  )

combined_figure


#### just mean and median for gam lat model and linear temp model ####
global_x_limits <- c(-20, 20)

library(ggridges)
trait.groups <- ggplot(average_topts_TG, aes(y = Trait.Group, fill = Trait.Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low_temp, xmax = ci_high_temp, y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "purple",position = position_nudge(y = -0.15)) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low_lat, xmax = ci_high_lat,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "darkgreen" ,position = position_nudge(y = 0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_temp_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "purple", alpha = .4, position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_lat_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "darkgreen", alpha = .4, position = position_nudge(y = 0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_temp_topt_r, y = Trait.Group),
             shape = 23,size = 1, fill = "purple", alpha = .4,  position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_lat_topt_r, y = Trait.Group),
             shape = 23,size = 1, fill = "darkgreen", alpha = .4,  position = position_nudge(y = 0.15)) +
  labs(x = "Residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.groups

trait.motivation <- ggplot(average_topts_TM, aes(y = Trait.motivation, fill = Trait.motivation)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_errorbarh(data = TM_sum, inherit.aes = FALSE, aes(xmin = ci_low_temp, xmax = ci_high_temp, y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "purple",position = position_nudge(y = -0.15)) +
  geom_errorbarh(data = TM_sum, inherit.aes = FALSE, aes(xmin = ci_low_lat, xmax = ci_high_lat,y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "darkgreen" ,position = position_nudge(y = 0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = mean_temp_topt_r, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "purple", alpha = .4, position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = mean_lat_topt_r, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "darkgreen", alpha = .4, position = position_nudge(y = 0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = median_temp_topt_r, y = Trait.motivation),
             shape = 23,size = 1, fill = "purple", alpha = .4,  position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = median_lat_topt_r, y = Trait.motivation),
             shape = 23,size = 1, fill = "darkgreen", alpha = .4,  position = position_nudge(y = 0.15)) +
  labs(x = "Residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.motivation


trait.organization <- ggplot(average_topts_TO, aes(y = organization, fill = organization)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low_temp, xmax = ci_high_temp, y = organization),
                 height = 0.10,linewidth = 0.4, color = "purple",position = position_nudge(y = -0.15)) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low_lat, xmax = ci_high_lat,y = organization),
                 height = 0.10,linewidth = 0.4, color = "darkgreen" ,position = position_nudge(y = 0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_temp_topt_r, y = organization),
             shape = 21, size = 1.5, fill = "purple", alpha = .4, position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_lat_topt_r, y = organization),
             shape = 21, size = 1.5, fill = "darkgreen", alpha = .4, position = position_nudge(y = 0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_temp_topt_r, y = organization),
             shape = 23,size = 1, fill = "purple", alpha = .4,  position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_lat_topt_r, y = organization),
             shape = 23,size = 1, fill = "darkgreen", alpha = .4,  position = position_nudge(y = 0.15)) +
  labs(x = "Residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.organization


library(patchwork)
combined_figure_lat_and_temp_residuals <-
  trait.groups +
  trait.motivation +
  trait.organization +
  plot_layout(ncol = 1, guides = "collect") +
  theme(
    axis.title.x = element_text(size = 16),
    plot.margin = margin(5, 5, 5, 5)
  )

combined_figure_lat_and_temp_residuals
