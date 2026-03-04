# ============================================================
# ## Title: activation-energy-context-analysis.R
# Description:
# Analyzes fitted activation energy estimates (Ev) across response
# contexts in FishTherm. Joins fitted parameters to curve metadata,
# averages Ev within study/species groups, summarizes Ev by response
# type, motivation, and biological organization, generates ridgeplot
# figures with mean/median and 95% CIs, and tests group differences
# using Gamma GLMs with post-hoc pairwise contrasts.
# ============================================================




#### this is a script for analyzing activation energy by response context ####

#### 01. Load and join data ####
library(here)
library(dplyr)
library(tidyverse)

curve_params <- readRDS(here("processed-data", "tpcs_with_fitted_params_with_act_eng.RDS"))
curves       <- read.csv(here("processed-data", "FishTherm.csv"))

# add organization info to fitted params (by curve_ID)
curve_params <- curve_params %>%
  left_join(curves %>% select(species_ID, curve_ID, organization), by = "curve_ID") %>%
  distinct()

# keep only rows with activation energy estimates
act_eng <- curve_params %>%
  filter(!is.na(e_arr))

# set factor levels for consistent ordering in plots/models

act_eng <- act_eng %>%
  mutate(Trait.Group = factor(Trait.Group,levels = c("Metabolism", "Energy Aquisition", "Somatic Growth", "Locomotion", "Reproduction", "Survival")),
    Trait.motivation = factor(Trait.motivation,levels = c("negative", "voluntary", "autonomic", "positive")),
    organization = factor(organization, levels = c("internal", "individual", "interaction", "population")))

#### 02. collapse within study/species/latitude by grouping variables ####

## response type (Trait.Group)
average_ee_TG <- act_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(averaged_e = mean(e_arr, na.rm = TRUE)) %>%
  ungroup() %>%
  select(study_ID, Trait.Group, species_ID, averaged_e, latitude) %>%
  distinct()  # ~83

#summary stats
mean(average_ee_TG$averaged_e) #.71
se <- sd(average_ee_TG$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG$averaged_e))) #0.05

median(average_ee_TG$averaged_e) #.64

#metabolic responses
average_ee_TG_M <- average_ee_TG %>%
  filter(Trait.Group == "Metabolism")
median(average_ee_TG_M$averaged_e)  #.564
mean(average_ee_TG_M$averaged_e) #.563
se <- sd(average_ee_TG_M$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG_M$averaged_e))) #0.03


## Motivation (Trait.motivation)
average_ee_TM <- act_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.motivation, Trait.Group) %>%
  mutate(averaged_e = mean(e_arr, na.rm = TRUE)) %>%
  ungroup() %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_e, latitude) %>%
  distinct()  # ~94

mean(average_ee_TM$averaged_e) #.69

## Organization
average_ee_TO <- act_eng %>%
  group_by(study_ID, species_ID, latitude, organization, Trait.Group, Trait.motivation) %>%
  mutate(averaged_e = mean(e_arr, na.rm = TRUE)) %>%
  ungroup() %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_e, latitude) %>%
  distinct()  # ~94

#### 03. summaries (mean/median/SE/95% CI) for overlay on ridge plots ####
trait_summary <- average_ee_TG %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_e   = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e, na.rm = TRUE),
    se       = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low   = mean_e - 1.96 * se,
    ci_high  = mean_e + 1.96 * se,
    .groups  = "drop"
  )

motivation_summary <- average_ee_TM %>%
  group_by(Trait.motivation) %>%
  summarise(
    mean_e   = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e, na.rm = TRUE),
    se       = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low   = mean_e - 1.96 * se,
    ci_high  = mean_e + 1.96 * se,
    .groups  = "drop"
  )

organization_summary <- average_ee_TO %>%
  group_by(organization) %>%
  summarise(
    mean_e   = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e, na.rm = TRUE),
    se       = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low   = mean_e - 1.96 * se,
    ci_high  = mean_e + 1.96 * se,
    .groups  = "drop"
  )

#### 04. ridge + point + CI plots ####

library(ggridges)

# limits used across figures for consistent scaling
global_x_limits <- c(-0.1, 3.2)

## response type plot ##
trait.groups <- ggplot(average_ee_TG, aes(x = averaged_e, y = Trait.Group)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3", linewidth = 0.5, alpha = 0.4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = 0.65) +
  geom_point(shape = 73, size = 2.5, alpha = 0.4) +
  geom_errorbarh(data = trait_summary, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = Trait.Group),height = 0.10, linewidth = 0.4, color = "black", position = position_nudge(y = -0.15)) +
  geom_point(data = trait_summary,inherit.aes = FALSE,aes(x = mean_e, y = Trait.Group),shape = 21, size = 1.5, fill = "red", alpha = 0.7, position = position_nudge(y = -0.15)) +
  geom_point(data = trait_summary, inherit.aes = FALSE, aes(x = median_e, y = Trait.Group), shape = 23, size = 1.5, fill = "grey", alpha = 0.7, position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (Ev)", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"), legend.position = "none")
trait.groups
ggsave("activation-en-trait-groups.pdf",plot = trait.groups, path = here("figures"), width = 5, height = 4)

## motivation plot ##
trait.motivation <- ggplot(average_ee_TM, aes(x = averaged_e, y = Trait.motivation)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3",linewidth = 0.5, alpha = 0.4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = 0.5) +
  geom_point(shape = 73, size = 2.5, alpha = 0.4) +
  geom_errorbarh(data = motivation_summary, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = Trait.motivation), height = 0.10, linewidth = 0.4, color = "black", position = position_nudge(y = -0.15)) +
  geom_point(data = motivation_summary, inherit.aes = FALSE, aes(x = mean_e, y = Trait.motivation), shape = 21, size = 1.5, fill = "red", alpha = 0.7, position = position_nudge(y = -0.15)) +
  geom_point(data = motivation_summary,inherit.aes = FALSE, aes(x = median_e, y = Trait.motivation), shape = 23, size = 1.5, fill = "grey", alpha = 0.7, position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (Ev)", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"),legend.position = "none")
trait.motivation
ggsave("activation-en-trait-motivation.pdf", plot = trait.motivation, path = here("figures"),width = 5, height = 4)

## organization plot ##
trait.organization <- ggplot(average_ee_TO, aes(x = averaged_e, y = organization)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3", linewidth = 0.5, alpha = 0.4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = 0.5) +
  geom_point(shape = 73, size = 2.5, alpha = 0.4) +
  geom_errorbarh(data = organization_summary, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization), height = 0.10, linewidth = 0.4, color = "black", position = position_nudge(y = -0.15)) +
  geom_point(data = organization_summary, inherit.aes = FALSE, aes(x = mean_e, y = organization), shape = 21, size = 1.5, fill = "red", alpha = 0.7, position = position_nudge(y = -0.15)) +
  geom_point(data = organization_summary, inherit.aes = FALSE, aes(x = median_e, y = organization), shape = 23, size = 1.5, fill = "grey", alpha = 0.7, position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (Ev)", y = NULL) +
  scale_x_continuous(limits = global_x_limits, expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.13, 0.13))) +
  theme_classic(base_size = 18) +
  theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"), legend.position = "none")
trait.organization
ggsave("activation-en-trait-organization.pdf", plot = trait.organization, path = here("figures"), width = 5, height = 4)

# combine panels into one figure #
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
ggsave("activation-en-combined.pdf", plot = combined_figure, path = here("figures"),width = 5, height = 9)

#### 05. Do the groups differ in mean activation energies (ANOVA) ####

average_ee_TM <- average_ee_TM %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "positive", "autonomic"))) 


## response type ##
library(emmeans)

response_Ea_model <- glm(averaged_e ~ Trait.Group, data = average_ee_TG, family = Gamma(link = "log"))
summary(response_Ea_model)

## pairwise contrast ##
response_Ea_model_pairwise <- emmeans(response_Ea_model, ~ Trait.Group)
pairwise_contrasts_response_Ea_model <- pairs(response_Ea_model_pairwise)
summary(pairwise_contrasts_response_Ea_model)

## response motivation ##

motivation_Ea_model <- glm(averaged_e ~ Trait.motivation, data = average_ee_TM, family = Gamma(link = "log"))
summary(motivation_Ea_model)

## pairwise contrast ##
motivation_Ea_model_pairwise <- emmeans(motivation_Ea_model, ~ Trait.motivation)
pairwise_contrasts_motivation_Ea_model <- pairs(motivation_Ea_model_pairwise)
summary(pairwise_contrasts_motivation_Ea_model)

## response organizaiton ##


organization_Ea_model <- glm(averaged_e ~ organization, data = average_ee_TO, family = Gamma(link = "log"))
summary(organization_Ea_model)

## pairwise contrast ##
organization_Ea_model_pairwise <- emmeans(organization_Ea_model, ~ organization)
pairwise_contrasts_organization_Ea_model <- pairs(organization_Ea_model_pairwise)
summary(pairwise_contrasts_organization_Ea_model)


library(sjPlot)
library(webshot)

tab_model(response_Ea_model,show.se = TRUE, show.stat = TRUE, file = "Ev~response_type_models.html")
tab_model(motivation_Ea_model,show.se = TRUE, show.stat = TRUE, file = "Ev~motivation_type_models.html")
tab_model(organization_Ea_model,show.se = TRUE, show.stat = TRUE, file = "Ev~organization_type_models.html")
webshot("Ev~response_type_models.html", "Ev~response_type_models.pdf")
webshot("Ev~motivation_type_models.html", "Ev~motivation_type_models.pdf")
webshot("Ev~organization_type_models.html", "Ev~organization_type_models.pdf")

