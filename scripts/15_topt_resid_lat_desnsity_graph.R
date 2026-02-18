### topt residuals split by response type ###
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(here)
library(dplyr)
library(tidyverse)

rm(list=ls())
#load data
fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS')) %>%
  select(!(c(Trait.Group, Trait.motivation))) 
fitted_datasets <- fitted_datasets %>%
  mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
curves <- readRDS(here('processed-data', 'wild-tpcsupdated.Rds')) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "survival", "Survival", Trait.Group)) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "reproduction", "Reproduction", Trait.Group)) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "somatic growth", "Somatic Growth", Trait.Group)) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "metabolism", "Metabolism", Trait.Group)) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "Energy aquisition", "Energy Aquisition", Trait.Group))

fitted_datasets <- fitted_datasets %>%
  left_join(curves %>% select(species_ID, curve_ID, Trait.Group, Trait.motivation, organization), join_by(curve_ID)) %>%
  distinct() %>%
  mutate(
    land_or_sea = case_when(
      land_or_sea == "freshwater" ~ "Freshwater",
      land_or_sea == "marine"     ~ "Marine",
      TRUE                        ~ land_or_sea))


fitted_datasets <- fitted_datasets %>%
  mutate(organization = ifelse(Trait.Group == "Reproduction", "population", organization))
#make custum order/all categorizing things factors#
fitted_datasets <- fitted_datasets %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("Metabolism", "Energy Aquisition", "Somatic Growth", "Locomotion", "Reproduction", "Survival"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "autonomic", "positive"))) %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population")))


#### Averaging topt by trait group only ####
average_topts_TG <- fitted_datasets %>%
  left_join(curves %>% select(curve_ID, species_ID)) %>%
  distinct() %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TG <- average_topts_TG %>%
  select(study_ID, Trait.Group, species_ID, averaged_topt, abs_latitude, latitude, land_or_sea) %>%
  distinct() #134


lat_avtopt_TG_model <- lmer(averaged_topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                         data = average_topts_TG)

summary(lat_avtopt_TG_model)
average_topts_TG$resid_topt_lat = residuals(lat_avtopt_TG_model)

#### Averaging topt by trait group and motivation ####

average_topts_TM <- fitted_datasets %>%
  left_join(curves %>% select(curve_ID, species_ID)) %>%
  distinct() %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TM <- average_topts_TM %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_topt, abs_latitude, latitude, land_or_sea) %>%
  distinct() #148


lat_avtopt_TM_model <- lmer(averaged_topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                            data = average_topts_TM)

summary(lat_avtopt_TM_model)
average_topts_TM$resid_topt_lat = residuals(lat_avtopt_TM_model)

#### Averaging topt by trait group and motivation and organization####

average_topts_TO <- fitted_datasets %>%
  left_join(curves %>% select(curve_ID, species_ID)) %>%
  distinct() %>%
  filter(topt_TF == TRUE) %>%
  group_by(study_ID, species_ID, latitude, Trait.Group, Trait.motivation, organization) %>% ## could also try cohort 
  mutate(averaged_topt = mean(topt)) %>%
  ungroup()

average_topts_TO <- average_topts_TO %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_topt, abs_latitude, latitude, land_or_sea) %>%
  distinct() #150


lat_avtopt_TO_model <- lmer(averaged_topt ~ abs_latitude * land_or_sea + (1 | study_ID), 
                            data = average_topts_TO)

summary(lat_avtopt_TO_model)
average_topts_TO$resid_topt_lat = residuals(lat_avtopt_TO_model)


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
trait.groups <- ggplot(average_topts_TG, aes(x = resid_topt_lat, y = Trait.Group,  # reorder on the fly
                                                                                fill = Trait.Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.Group), shape = 73, size = 2.5, alpha = .4) +
  geom_errorbarh(data = TG_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.Group),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TG_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.Group),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
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

trait.motivation <- ggplot(average_topts_TM, aes(x = resid_topt_lat, y = Trait.motivation,  # reorder on the fly
                                                                                       fill = Trait.motivation)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.motivation), shape = 73, size = 2.5, alpha = .4) +
  geom_errorbarh(data = TM_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TM_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = Trait.motivation),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
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


trait.organization <- ggplot(average_topts_TO, aes(x = resid_topt_lat, y =organization,  # reorder on the fly
                                                 fill = organization)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = organization), shape = 73, size = 2.5, alpha = .4) +
  geom_errorbarh(data = TO_sum, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high, y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = mean_topt_r, y = organization),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = TO_sum, inherit.aes = FALSE, aes(x = median_topt_r, y = organization),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Topt and Lat residuals", y = NULL) +
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


#### Do the groups differ in mean activation energies (ANOVA) ####
average_topts_TG <- average_topts_TG %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("Metabolism", "Locomotion", "Energy Aquisition", "Somatic Growth", "Reproduction", "Survival")))
trait_resid_topt_mod <- glm(resid_topt_lat ~ Trait.Group, data = average_topts_TG)
summary(trait_resid_topt_mod)

plot(residuals(trait_resid_topt_mod))
qqnorm(resid(trait_resid_topt_mod))
qqline(resid(trait_resid_topt_mod))
hist(resid(trait_resid_topt_mod))
summary(trait_resid_topt_mod)

average_topts_TM <- average_topts_TM %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "positive", "autonomic"))) 
motivation_resid_topt_mod <- glm(resid_topt_lat ~ Trait.motivation, data = average_topts_TM)
summary(motivation_resid_topt_mod)

average_topts_TO <- average_topts_TO %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population"))) 
organization_resid_topt_mod <- glm(resid_topt_lat ~ organization, data = average_topts_TO)
summary(organization_resid_topt_mod)


install.packages("remotes")
remotes::install_github("rvlenth/emmeans")
install.packages("emmeans")
library(emmeans)
# Using the same example data and model as above

means1 <- emmeans(trait_resid_topt_mod, ~ Trait.Group)
means2 <- emmeans(motivation_resid_topt_mod, ~ Trait.motivation)
means3 <- emmeans(organization_resid_topt_mod, ~ organization)

# Get all pairwise contrasts from the means
pairwise_contrasts1 <- pairs(means1)
summary(pairwise_contrasts1)
pairwise_contrasts2 <- pairs(means2)
summary(pairwise_contrasts2)
pairwise_contrasts3 <- pairs(means3)
summary(pairwise_contrasts3)


library(sjPlot)
library(webshot)
tab_model(trait_resid_topt_mod, motivation_resid_topt_mod, organization_resid_topt_mod, show.stat = TRUE, show.se = TRUE, file = "topt_resid~groups_models.html")
webshot("topt_resid~groups_models.html", "topt_resid~groups_models.pdf")

library(dplyr)
library(emmeans)
library(knitr)

# Extract pairwise contrasts and convert to data frames
df1 <- as.data.frame(summary(pairwise_contrasts1)) %>% mutate(model = "Ev ~ Motivation")
df2 <- as.data.frame(summary(pairwise_contrasts2)) %>% mutate(model = "Ev ~ Response Type")
df3 <- as.data.frame(summary(pairwise_contrasts3)) %>% mutate(model = "Ev ~ Organization")

# Combine all three into one table
combined_contrasts <- bind_rows(df2, df1, df3)

# Select relevant columns and rename nicely
combined_contrasts <- combined_contrasts %>%
  select(model, contrast, estimate, SE, df, t.ratio, p.value)  # adjust column names based on what summary() gives
# For glm contrasts, df and t.ratio may be NA, that's fine

# Display table
kable(combined_contrasts, digits = 3, caption = "Pairwise contrasts for all models")

install.packages("kableExtra")
library(kableExtra)
install.packages("webshot2")
library(webshot2)
combined_contrasts %>%
  kable("pdf", caption = "Pairwise contrasts for all models") %>%
  kable_styling(full_width = FALSE) %>%
  save_kable(here("figures", "pairwise_contrasts_table.pdf"))







