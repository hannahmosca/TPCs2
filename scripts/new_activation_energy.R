#new act eng # 
#### hannah mosca ####
library(here)
library(dplyr)
library(tidyverse)
rm(list=ls())
#### this is a script for analyzing activation energy ####
act_eng <- readRDS(here("processed-data", "new_activation_energy_subset.RDS"))
curve_params <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS")) %>%
  select(!(c(Trait.Group, Trait.motivation))) 
curves <- readRDS(here("processed-data", "wild-tpcsupdated.RdS"))
curve_params <- curve_params %>%
  left_join(curves %>% select(species_ID, curve_ID, Trait.Group, Trait.motivation, organization), join_by(curve_ID)) %>%
  distinct()
curve_params <- curve_params %>%
  mutate(organization = ifelse(Trait.Group == "reproduction", "population", organization))

act_eng <- act_eng %>%
  left_join(curve_params %>% select(curve_ID, Trait.Group, Trait.motivation, organization, thermal_tolerance, breadth, dataset_type, breadth_TF, thermal_tolerance_TF, study_ID, habitat_water, given_trait_name, land_or_sea, species_ID, latitude, abs_latitude), join_by(curve_ID))


act_eng <- act_eng %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("metabolism", "Energy aquisition", "somatic growth", "Locomotion", "reproduction", "survival"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "autonomic", "positive"))) %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population")))


average_ee_TG <- act_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>% ## could also try cohort 
  mutate(averaged_e = mean(e_arr)) %>%
  ungroup()

average_ee_TG <- average_ee_TG %>%
  select(study_ID, Trait.Group, species_ID, averaged_e, latitude) %>%
  distinct() #82

mean(average_ee_TG$averaged_e) #.70
se <- sd(average_ee_TG$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG$averaged_e))) #0.05

median(average_ee_TG$averaged_e) #.64

average_ee_TG_M <- average_ee_TG %>%
  filter(Trait.Group == "metabolism")
median(average_ee_TG_M$averaged_e)  #.564

mean(average_ee_TG_M$averaged_e) #.563
se <- sd(average_ee_TG_M$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG_M$averaged_e))) #0.03


trait_summary <- average_ee_TG %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_e = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e,na.rm = TRUE),
    se = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_e - 1.96 * se,
    ci_high = mean_e + 1.96 * se,
    .groups = "drop"
  )
average_ee_TM <- act_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.motivation, Trait.Group) %>% ## could also try cohort 
  mutate(averaged_e = mean(e_arr)) %>%
  ungroup()

average_ee_TM <- average_ee_TM %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_e, latitude) %>%
  distinct() #93

mean(average_ee_TM$averaged_e) #.68

motivation_summary <- average_ee_TM %>%
  # filter(Trait.Group != "Survival") %>%
  group_by(Trait.motivation) %>%
  summarise(
    mean_e = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e,na.rm = TRUE),
    se = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_e - 1.96 * se,
    ci_high = mean_e + 1.96 * se,
    .groups = "drop"
  )


average_ee_TO <- act_eng %>%
  group_by(study_ID, species_ID, latitude, organization, Trait.Group, Trait.motivation) %>% ## could also try cohort 
  mutate(averaged_e = mean(e_arr)) %>%
  ungroup()

average_ee_TO <- average_ee_TO %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_e, latitude) %>%
  distinct() #93



organization_summary <- average_ee_TO %>%
  # filter(Trait.Group != "Survival") %>%
  group_by(organization) %>%
  summarise(
    mean_e = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e,na.rm = TRUE),
    se = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_e - 1.96 * se,
    ci_high = mean_e + 1.96 * se,
    .groups = "drop"
  )


global_x_limits <- c(-.1, 3.2)
library(ggridges)
trait.groups <- ggplot(average_ee_TG, aes(x = averaged_e, y = Trait.Group,  # reorder on the fly
                                          fill = Trait.Group)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .65) +
  geom_point(aes(y = Trait.Group), shape = 73, size = 2.5, alpha = .4) +
  geom_errorbarh(data = trait_summary %>% filter(Trait.Group != "Survival"), inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.Group),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = trait_summary %>% filter(Trait.Group != "Survival"), inherit.aes = FALSE, aes(x = mean_e, y = Trait.Group),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = trait_summary %>% filter(Trait.Group != "Survival"), inherit.aes = FALSE, aes(x = median_e, y = Trait.Group),
             shape = 23,size = 1.5, fill = "grey", alpha = .7,  position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (e)", y = NULL) +
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
ggsave("activation-en-trait-groups.pdf", plot = trait.groups, path = here("figures"), width = 5, height = 4)

trait.motivation <- ggplot(average_ee_TM, aes(x = averaged_e, y = Trait.motivation,  # reorder on the fly
                                              fill = Trait.motivation)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .5) +
  geom_point(aes(y = Trait.motivation), shape = 73, size = 2.5,  alpha = .4) +
  geom_errorbarh(data = motivation_summary, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = Trait.motivation),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = motivation_summary, inherit.aes = FALSE, aes(x = mean_e, y = Trait.motivation),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = motivation_summary, inherit.aes = FALSE, aes(x = median_e, y = Trait.motivation),
             shape = 23,size = 1.5, fill = "grey", alpha = .7, position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (e)", y = NULL) +
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

ggsave("activation-en-trait-motivation.pdf", plot = trait.motivation, path = here("figures"), width = 5, height = 4)


trait.organization <- ggplot(average_ee_TO,
                             aes(x = averaged_e,
                                 y = organization,  # reorder on the fly
                                 fill = organization)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "red3", linewidth = .5, alpha = .4) +
  geom_density_ridges(alpha = 0.3, fill = "grey40", linewidth = 0, scale = .5) +
  geom_point(aes(y = organization), shape = 73, size = 2.5,  alpha = .4) +
  geom_errorbarh(data = organization_summary, inherit.aes = FALSE, aes(xmin = ci_low, xmax = ci_high,y = organization),
                 height = 0.10,linewidth = 0.4, color = "black",position = position_nudge(y = -0.15)) +
  geom_point(data = organization_summary, inherit.aes = FALSE, aes(x = mean_e, y = organization),
             shape = 21, size = 1.5, fill = "red", alpha = .7, position = position_nudge(y = -0.15)) +
  geom_point(data = organization_summary, inherit.aes = FALSE, aes(x = median_e, y = organization),
             shape = 23,size = 1.5, fill = "grey",  alpha = .7, position = position_nudge(y = -0.15)) +
  labs(x = "Activation energy (e)", y = NULL) +
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
ggsave("activation-en-trait-organization.pdf", plot = trait.organization, path = here("figures"), width = 5, height = 4)

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
ggsave("activation-en-combined.pdf", plot = combined_figure, path = here("figures"), width = 5, height = 9)


###AE and breadth####
act_eng_collapsed <- act_eng %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>%
  mutate(
    averaged_e = mean(e_arr),
    averaged_topt = if (any(topt_TF)) mean(topt[topt_TF], na.rm = TRUE) else NA_real_,
    averaged_pbreadth = if (any(breadth_TF)) mean(breadth[breadth_TF], na.rm = TRUE) else NA_real_,
    averaged_tbreadth = if (any(thermal_tolerance_TF)) mean(thermal_tolerance[thermal_tolerance_TF], na.rm = TRUE) else NA_real_) %>%
  ungroup() %>%
  select(study_ID, species_ID, latitude, Trait.Group, averaged_e, averaged_pbreadth, averaged_tbreadth) %>%
  distinct()

length(unique(act_eng_collapsed$averaged_e)) #82
length(unique(act_eng_collapsed$averaged_pbreadth)) #31
length(unique(act_eng_collapsed$averaged_tbreadth)) #11

thermal_tol_andEa <- ggplot(data = act_eng_collapsed, aes(x = averaged_tbreadth, y = averaged_e)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Thermal Tolerance", y = "Activation Energy (Ev)") +
  theme_classic()
thermal_tol_andEa

thermal_breadthandEa <- ggplot(data = act_eng_collapsed, aes(x = averaged_pbreadth, y = averaged_e)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Breadth", y = "Activation Energy (Ev)") +
  theme_classic()
thermal_breadthandEa


library(patchwork)

thermal_tol_andEa + thermal_breadthandEa


thermal_perf_breadth_andEa_model <- lmer(averaged_e ~ averaged_pbreadth + (1 | study_ID), 
                                         data = act_eng_collapsed %>%
                                           filter(!is.na(averaged_pbreadth)))
thermal_tol_andEa_model <- lm(averaged_e ~ averaged_tbreadth, 
                              data = act_eng_collapsed %>%
                                filter(!is.na(averaged_tbreadth)))
summary(thermal_perf_breadth_andEa_model)



#plot fitted model 
## want to make sure only predicting on range of data
breadth_range <- act_eng_collapsed %>%
  filter(!is.na(averaged_pbreadth)) %>%
  summarise(
    min_breadth = min(averaged_pbreadth),
    max_breadth = max(averaged_pbreadth))

breadth_range
pred_grid <- data.frame(
  averaged_pbreadth = seq(breadth_range$min_breadth,
                breadth_range$max_breadth,
                length.out = 200))
pred_grid$pred <- predict(thermal_perf_breadth_andEa_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(thermal_perf_breadth_andEa_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

thermal_perf_breadth_andEa_graph <- ggplot(data = pred_grid, aes(x = averaged_pbreadth)) +
  geom_point(data = act_eng_collapsed %>% filter(!is.na(averaged_pbreadth)),
             aes(x = averaged_pbreadth, y = averaged_e),
             colour = "black", alpha = 0.75) +
  geom_line(aes(y = pred),
            color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "black", alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Activation Energy") +
  theme_classic()

thermal_perf_breadth_andEa_graph


#### tolerance breadth ####
thermal_tol_andEa_model <- lm(averaged_e ~ averaged_tbreadth, 
                                         data = act_eng_collapsed %>%
                                           filter(!is.na(averaged_tbreadth)))

summary(thermal_tol_andEa_model)


## want to make sure only predicting on range of data
tbreadth_range <- act_eng_collapsed %>%
  filter(!is.na(averaged_tbreadth)) %>%
  summarise(
    min_breadth = min(averaged_tbreadth),
    max_breadth = max(averaged_tbreadth))

tbreadth_range
pred_grid <- data.frame(
  averaged_tbreadth = seq(tbreadth_range$min_breadth,
                          tbreadth_range$max_breadth,
                          length.out = 200))
pred_grid$pred <- predict(thermal_tol_andEa_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(thermal_tol_andEa_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

thermal_tol_andEa_graph <- ggplot(data = pred_grid, aes(x = averaged_tbreadth)) +
  geom_point(data = act_eng_collapsed %>% filter(!is.na(averaged_tbreadth)),
             aes(x = averaged_tbreadth, y = averaged_e),
             colour = "black", alpha = 0.75) +
  geom_line(aes(y = pred),
            color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "black", alpha = 0.20) +
  labs(x = "Tolerance Breadth", y = "Activation Energy") +
  theme_classic()

thermal_tol_andEa_graph

library(webshot)
tab_model(thermal_tol_andEa_model, thermal_perf_breadth_andEa_model, show.stat = TRUE, show.se = TRUE, file = "AE_breadthmodel.html")
webshot("AE_breadthmodel.html", "AE_breadthmodel.pdf")




activation_and_breadth <- thermal_tol_andEa_graph + thermal_perf_breadth_andEa_graph
activation_and_breadth
ggsave("activation_eng_and_breadth_regression.pdf", plot = activation_and_breadth, path = here("figures"), width = 8, height = 4)


#### Do the groups differ in mean activation energies (ANOVA) ####

average_ee_TM <- average_ee_TM %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "positive", "autonomic"))) 
##removing outliers
length(average_ee_TM$averaged_e)

# Calculate Z-scores
z_scores <- scale(average_ee_TM$averaged_e)

# Define a threshold (e.g., 3 standard deviations)
threshold <- 3

# Remove outliers
data_cleaned <- average_ee_TM$averaged_e[abs(z_scores) <= threshold]

# View the cleaned data
print(data_cleaned)


#interquartile range


modTG <- lm(averaged_e ~ Trait.Group, data = average_ee_TG)
summary(modTG)

modTM <- lm(averaged_e ~ Trait.motivation, data = average_ee_TM)
summary(modTM)
install.packages("remotes")
remotes::install_github("rvlenth/emmeans")
install.packages("emmeans")
library(emmeans)
# Using the same example data and model as above
model1 <- glm(averaged_e ~ Trait.motivation, data = average_ee_TM)
motivation_Ea_model <- glm(averaged_e ~ Trait.motivation, data = average_ee_TM, family = Gamma(link = "log"))
summary(model2)

AIC(model1, model2)
means1 <- emmeans(model1, ~ Trait.motivation)
means2 <- emmeans(motivation_Ea_model, ~ Trait.motivation)

# Get all pairwise contrasts from the means
pairwise_contrasts1 <- pairs(means2)
summary(pairwise_contrasts1)

response_Ea_model <- glm(averaged_e ~ Trait.Group, data = average_ee_TG, family = Gamma(link = "log"))
summary(response_Ea_model)
response_Ea_model_pairwise <- emmeans(response_Ea_model, ~ Trait.Group)
pairwise_contrasts2 <- pairs(response_Ea_model_pairwise)
summary(pairwise_contrasts2)

organization_Ea_model <- glm(averaged_e ~ organization, data = average_ee_TO, family = Gamma(link = "log"))
summary(organization_Ea_model)
organization_Ea_model_pairwise <- emmeans(organization_Ea_model, ~ organization)
pairwise_contrasts3 <- pairs(organization_Ea_model_pairwise)
summary(pairwise_contrasts3)

tab_model(response_Ea_model, motivation_Ea_model, organization_Ea_model, show.stat = TRUE, file = "Ev~groups_models.html")
webshot("Ev~groups_models.html", "Ev~groups_models.pdf")

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

