# ============================================================
# ## Title: topt_resid_and_performance_type.R
# Description:
# Analyzes fitted topt estimates across response
# contexts in FishTherm. Joins fitted parameters to curve metadata,
# averages topt within study/species groups, summarizes residual topt lat models  by response
# type, motivation, and biological organization, generates ridgeplot
# figures with mean/median and 95% CIs, and tests group differences
# ============================================================
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(here)
library(dplyr)
library(tidyverse)

rm(list=ls())
#### 01. load and join data ####
params <- readRDS(here('processed-data', 'tpcs_with_fitted_params_with_act_eng_10_6.RDS'))
curves <- read.csv(here('processed-data', 'FishTherm.csv'))

params <- params %>%
  left_join(curves %>% select(species_ID, curve_ID, organization), join_by(curve_ID)) %>%
  distinct() 
params <- params %>%
  mutate(environment = ifelse(land_or_sea == "terrestrial", "freshwater", "marine")) %>%
  select(-(land_or_sea))
# set factor levels for consistent ordering in plots/models
params <- params %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("Metabolism", "Energy Aquisition", "Somatic Growth", "Locomotion", "Reproduction", "Survival"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "autonomic", "positive"))) %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population")))

#### 02. collapse within study/species/latitude by grouping variables ####

# averaging topt by trait group only
average_topts_TG <- params %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>% 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TG <- average_topts_TG %>%
  select(study_ID, Trait.Group, species_ID, averaged_topt, abs_latitude, latitude, environment) %>%
  distinct() #140
average_topts_TG$environment <- as.factor(average_topts_TG$environment)
average_topts_TG$study_ID <- as.factor(average_topts_TG$study_ID)
install.packages("mgcv")
library(mgcv)

lat_avtopt_TG_model <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TG, method = "REML")


summary(lat_avtopt_TG_model)
average_topts_TG$resid_topt_lat = residuals(lat_avtopt_TG_model)

## averaging topt by trait group and motivation ##

average_topts_TM <- params %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TM <- average_topts_TM %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_topt, abs_latitude, latitude, environment) %>%
  distinct() #156

average_topts_TM$environment <- as.factor(average_topts_TM$environment)
average_topts_TM$study_ID <- as.factor(average_topts_TM$study_ID)
install.packages("mgcv")
library(mgcv)

lat_avtopt_TM_model <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TM, method = "REML")


summary(lat_avtopt_TM_model)
average_topts_TM$resid_topt_lat = residuals(lat_avtopt_TM_model)

##averaging topt by trait group and motivation and organization## 

average_topts_TO <- params %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation, organization) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()
average_topts_TO <- average_topts_TO %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_topt, abs_latitude, latitude, environment) %>%
  distinct() #158
average_topts_TO$environment <- as.factor(average_topts_TO$environment)
average_topts_TO$study_ID <- as.factor(average_topts_TO$study_ID)
install.packages("mgcv")
library(mgcv)

lat_avtopt_TO_model <- gam(averaged_topt ~ s(abs_latitude, by = environment) + environment + s(study_ID, bs = "re"), data = average_topts_TO, method = "REML")
lat_avtopt_TO_model_lm <- lmer(averaged_topt ~ abs_latitude * environment + (1 | study_ID), 
                               data = average_topts_TO)
summary(lat_avtopt_TO_model)
summary(lat_avtopt_TO_model_lm)
average_topts_TO$resid_topt_lat = residuals(lat_avtopt_TO_model)
average_topts_TO$resid_topt_lat_lme = residuals(lat_avtopt_TO_model_lm)

#### 03. summaries (mean/median/SE/95% CI) for overlay on ridge plots ####


TG_sum <- average_topts_TG %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_topt_r = mean(resid_topt_lat, na.rm = TRUE),
    median_topt_r = median(resid_topt_lat,na.rm = TRUE),
    se = sd(resid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )

TM_sum <- average_topts_TM %>%
  group_by(Trait.motivation) %>%
  summarise(
    mean_topt_r = mean(resid_topt_lat, na.rm = TRUE),
    median_topt_r = median(resid_topt_lat,na.rm = TRUE),
    se = sd(resid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )
TO_sum <- average_topts_TO %>%
  group_by(organization) %>%
  summarise(
    mean_topt_r = mean(resid_topt_lat, na.rm = TRUE),
    median_topt_r = median(resid_topt_lat, na.rm = TRUE),
    se = sd(resid_topt_lat, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_topt_r - 1.96 * se,
    ci_high = mean_topt_r + 1.96 * se,
    .groups = "drop"
  )

global_x_limits <- c(-20, 10)
library(ggridges)
trait.groups <- ggplot(average_topts_TG, aes(x = resid_topt_lat, y = Trait.Group, fill = Trait.Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.Group, color = environment), shape = 73, size = 2.5, alpha = 1) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.Group),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and linear residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  scale_color_manual(values = c("freshwater" = "black",
                                "marine" = "black")) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 

trait.groups

trait.motivation <- ggplot(average_topts_TM, aes(x = resid_topt_lat, y = Trait.motivation,  # reorder on the fly
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
  labs(x = "Topt and linear residuals", y = NULL) +
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

topt_organization_n <- average_topts_TO %>%
  filter(!is.na(resid_topt_lat)) %>%
  group_by(organization) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(TO_sum %>% select(mean_topt_r, median_topt_r, organization), join_by(organization))

trait.organization <- ggplot(average_topts_TO, aes(x = resid_topt_lat, y =organization,  # reorder on the fly
                                                 fill = organization)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization), color = "black", shape = 73, size = 3.5, alpha = 1) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = organization),
             shape = 21, size = 3, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = organization),
             shape = 23,size = 3, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  geom_text(data = topt_organization_n, inherit.aes = FALSE, aes(x = mean_topt_r, y = organization, label = paste0("n = ", n)),
            position = position_nudge(y = -0.28), hjust = 0.5, size = 2.5, color = "black") +
  labs(x = "Topt and linear residuals", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.12, 0.13))) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 
trait.organization

ggsave("topt-and-lat-resid-trait-org.pdf", plot = trait.organization, path = here("figures"), width = 5, height = 4)




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
ggsave("topt-and-lat-resid-combined.pdf", plot = combined_figure, path = here("figures"), width = 5, height = 9)


#### 04. Do the groups differ in topt (ANOVA) ####

## response type ##
library(emmeans)

response_topt_model <- glm(resid_topt_lat ~ Trait.Group, data = average_topts_TG)
summary(response_topt_model)

## pairwise contrast ##
response_topt_model_pairwise <- emmeans(response_topt_model, ~ Trait.Group)
pairwise_contrasts_response_topt_model <- pairs(response_topt_model_pairwise)
summary(pairwise_contrasts_response_topt_model)

## response motivation ##

motivation_topt_model <- glm(resid_topt_lat ~ Trait.motivation, data = average_topts_TM)
summary(motivation_topt_model)

## pairwise contrast ##
motivation_topt_model_pairwise <- emmeans(motivation_topt_model, ~ Trait.motivation)
pairwise_contrasts_motivation_topt_model <- pairs(motivation_topt_model_pairwise)
summary(pairwise_contrasts_motivation_topt_model)

## response organizaiton ##

organization_topt_model <- glm(resid_topt_lat ~ organization, data = average_topts_TO)
organization_topt_model_lme <- glm(resid_topt_lat_lme ~ organization, data = average_topts_TO)

summary(organization_topt_model)
summary(organization_topt_model_lme)

## pairwise contrast ##
organization_topt_model_pairwise <- emmeans(organization_topt_model, ~ organization)
pairwise_contrasts_organization_topt_model <- pairs(organization_topt_model_pairwise)
summary(pairwise_contrasts_organization_topt_model)


library(sjPlot)
library(webshot)

tab_model(response_topt_model,show.se = TRUE, show.stat = TRUE, file = "topt~response_type_models.html")
tab_model(motivation_topt_model,show.se = TRUE, show.stat = TRUE, file = "topt~motivation_type_models.html")
tab_model(organization_topt_model, show.se = TRUE, show.stat = TRUE, file = "topt~organization_type_models.html")
tab_model(organization_topt_model_lme, show.se = TRUE, show.stat = TRUE, file = "topt~organization_type_model_lme.html")

webshot("topt~response_type_models.html", "topt~response_type_models.pdf")
webshot("topt~motivation_type_models.html", "topt~motivation_type_models.pdf")
webshot("topt~organization_type_models.html", "topt~organization_type_models.pdf")
webshot("topt~organization_type_model_lme.html", "topt~organization_type_model_lme.pdf")


# breadth and level of organization
average_breadth_TO <- params %>%
  filter(breadth_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation, organization) %>% ## could also try cohort 
  mutate(averaged_Pbreadth = mean(breadth)) %>%
  ungroup()

BO_sum <- average_breadth_TO %>%
  group_by(organization) %>%
  summarise(
    mean_Pbreadth_r = mean(averaged_Pbreadth, na.rm = TRUE),
    median_Pbreadth_r = median(averaged_Pbreadth, na.rm = TRUE),
    se = sd(averaged_Pbreadth, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_Pbreadth_r - 1.96 * se,
    ci_high = mean_Pbreadth_r + 1.96 * se,
    .groups = "drop"
  )
br_organization_n <- average_breadth_TO %>%
  filter(!is.na(averaged_Pbreadth)) %>%
  group_by(organization) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(BO_sum %>% select(mean_Pbreadth_r, median_Pbreadth_r, organization), join_by(organization))

trait.organization_br <- ggplot(average_breadth_TO, aes(x = averaged_Pbreadth, y =organization,  # reorder on the fly
                                                        fill = organization)) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization), color = "black", shape = 73, size = 3.5, alpha = 1) +
  geom_errorbarh(data = BO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = BO_sum, inherit.aes = FALSE, aes(x = mean_Pbreadth_r, y = organization),
             shape = 21, size = 3, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = BO_sum, inherit.aes = FALSE, aes(x = median_Pbreadth_r, y = organization),
             shape = 23,size = 3, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  geom_text(data = br_organization_n, inherit.aes = FALSE, aes(x = mean_Pbreadth_r, y = organization, label = paste0("n = ", n)),
            position = position_nudge(y = -0.28), hjust = 0.5, size = 2.5, color = "black") +
  labs(x = "Performance Breadth", y = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.12, 0.13))) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 
trait.organization_br
ggsave("Pbreadth-org.pdf", plot = trait.organization_br, path = here("figures"), width = 5, height = 4)

organization_breadth_model <- glm(averaged_Pbreadth ~ organization, data = average_breadth_TO)
summary(organization_breadth_model)


## pairwise contrast ##
organization_breadth_model_pairwise <- emmeans(organization_breadth_model, ~ organization)
pairwise_contrasts_organization_breadth_model <- pairs(organization_breadth_model_pairwise)
summary(pairwise_contrasts_organization_breadth_model)

library(sjPlot)
library(webshot)
tab_model(organization_breadth_model,show.se = TRUE, show.stat = TRUE, file = "Pbreadth~organization_type_models.html")
webshot("Pbreadth~organization_type_models.html", "Pbreadth~organization_type_models.pdf")


tab_model(organization_Ea_model, organization_topt_model,organization_breadth_model, show.se = TRUE, show.stat = TRUE, file = "ea_topt_breadth~organization_type_models.html")
webshot("ea_topt_breadth~organization_type_models.html", "ea_topt_breadth~organization_type_models.pdf")


# tbreadth and level of organization
average_Tbreadth_TO <- params %>%
  filter(thermal_tolerance_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation, organization) %>% ## could also try cohort 
  mutate(averaged_Tbreadth = mean(thermal_tolerance)) %>%
  ungroup()

TBO_sum <- average_Tbreadth_TO %>%
  group_by(organization) %>%
  summarise(
    mean_Tbreadth_r = mean(averaged_Tbreadth, na.rm = TRUE),
    median_Tbreadth_r = median(averaged_Tbreadth, na.rm = TRUE),
    se = sd(averaged_Tbreadth, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_Tbreadth_r - 1.96 * se,
    ci_high = median_Tbreadth_r + 1.96 * se,
    .groups = "drop"
  )

trait.organization_Tbr <- ggplot(average_Tbreadth_TO, aes(x = averaged_Tbreadth, y =organization,  # reorder on the fly
                                                   fill = organization)) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization), color = "black", shape = 73, size = 2, alpha = 1) +
  geom_errorbarh(data = TBO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TBO_sum, inherit.aes = FALSE, aes(x = mean_Tbreadth_r, y = organization),
             shape = 21, size = 2.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TBO_sum, inherit.aes = FALSE, aes(x = median_Tbreadth_r, y = organization),
             shape = 23,size = 2.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Performance Breadth", y = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_discrete(expand = expansion(mult = c(0.08, 0.13))) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) 
trait.organization_Tbr

