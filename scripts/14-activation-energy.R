#### hannah mosca ####
library(here)
library(dplyr)
library(tidyverse)
rm(list=ls())
#### this is a script for analyzing activation energy ####
curve_params <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS")) %>%
  select(!(c(Trait.Group, Trait.motivation))) 
curves <- readRDS(here("processed-data", "wild-tpcsupdated.RdS"))
curve_params <- curve_params %>%
  left_join(curves %>% select(species_ID, curve_ID, Trait.Group, Trait.motivation, organization), join_by(curve_ID)) %>%
  distinct()
curve_params <- curve_params %>%
  mutate(organization = ifelse(Trait.Group == "Reproduction", "population", organization))

#filter out irregular and decreasing
ee <- curve_params %>%
  filter(dataset_type %in% c("full_curve", "left_bound_withopt", "unbounded_increasing", "topt", 
                             "left_bound")) %>%
  filter(!is.na(e))
### visually filtering out ones that are just topt for the activation energy testing###
act_eng <- unique(ee$curve_ID)
remove <- c(56, 438, 439, 208, 53, 54, 126, 108, 144, 177, 179, 182, 185, 190, 186, 199, 201, 207, 217, 219, 207, 292, 321, 323, 344, 373, 374, 368, 369,377, 417, 429, 431, 436, 438, 461)

subset <- ee %>%
  filter(!(curve_ID %in% remove))

Ea_curves <- unique(subset$curve_ID)


mean(subset$e)

subset <- subset %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("metabolism", "Energy acquisition", "somatic growth", "Locomotion", "reproduction", "survival"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "autonomic", "positive"))) %>%
  mutate(organization = factor(organization, levels = c("internal", "individual", "interaction", "population")))


saveRDS(subset, file = here("processed-data", "activation_energy_subset.RDS"))

average_ee_TG <- subset %>%
  group_by(study_ID, species_ID, latitude, Trait.Group) %>% ## could also try cohort 
  mutate(averaged_e = mean(e)) %>%
  ungroup()

average_ee_TG <- average_ee_TG %>%
  select(study_ID, Trait.Group, species_ID, averaged_e, latitude) %>%
  distinct() #143

mean(average_ee_TG$averaged_e) #.86
median(average_ee_TG$averaged_e) #.66

average_ee_TG_M <- average_ee_TG %>%
  filter(Trait.Group == "metabolism")
median(average_ee_TG_M$averaged_e) 

mean(average_ee_TG_M$averaged_e) #.60
se <- sd(average_ee_TG_M$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG_M$averaged_e)))

median(average_ee_TG_M$averaged_e) #.50

se <- sd(average_ee_TG$averaged_e, na.rm = TRUE) /
  sqrt(sum(!is.na(average_ee_TG$averaged_e)))


trait_summary <- average_ee_TG %>%
  # filter(Trait.Group != "Survival") %>%
  group_by(Trait.Group) %>%
  summarise(
    mean_e = mean(averaged_e, na.rm = TRUE),
    median_e = median(averaged_e,na.rm = TRUE),
    se = sd(averaged_e, na.rm = TRUE) / sqrt(n()),
    ci_low = mean_e - 1.96 * se,
    ci_high = mean_e + 1.96 * se,
    .groups = "drop"
  )
average_ee_TM <- subset %>%
  group_by(study_ID, species_ID, latitude, Trait.motivation, Trait.Group) %>% ## could also try cohort 
  mutate(averaged_e = mean(e)) %>%
  ungroup()

average_ee_TM <- average_ee_TM %>%
  select(study_ID, Trait.Group, Trait.motivation, species_ID, averaged_e, latitude) %>%
  distinct() #163

mean(average_ee_TM$averaged_e) #.83

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


average_ee_TO <- subset %>%
  group_by(study_ID, species_ID, latitude, organization, Trait.Group, Trait.motivation) %>% ## could also try cohort 
  mutate(averaged_e = mean(e)) %>%
  ungroup()

average_ee_TO <- average_ee_TO %>%
  select(study_ID, Trait.Group, Trait.motivation, organization, species_ID, averaged_e, latitude) %>%
  distinct() #168

mean(average_ee_TO$averaged_e) #.85

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


global_x_limits <- c(-.5, 5)
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





metabolic <- ee %>%
  filter(Trait.Group == "metabolism") #.65, 96 obs --> 109obs, .61 avg (grouping with cardiac lowered mean here)
mean(metabolic$e)
swimming <- ee %>%
  filter(Trait.Group == "Locomotion") #.44, 58 obs
mean(swimming$e)
growth <- ee %>%
  filter(Trait.Group == "somatic growth") #1.08, 85 obs
mean(growth$e)
feeding <- ee %>%
  filter(Trait.Group == "Energy aquisition") #0.72, 40 obs
mean(feeding$e)
survival <- ee %>%
  filter(Trait.Group == "survival") #3.34, 5 obs 
mean(survival$e)
reproduction <- ee %>%
  filter(Trait.Group == "reproduction") #1.25, #11 obs 
mean(reproduction$e)


### this should go in the supplement

tolerance_and_breadth <- curve_params %>%
  filter(thermal_tolerance_TF == TRUE) %>%
  filter(breadth_TF == TRUE)

tolerance <- subset %>%
  filter(thermal_tolerance_TF == TRUE)

breadth <- subset %>%
  filter(breadth_TF == TRUE)


thermal_tol_andEa <- ggplot(data = tolerance, aes(x = thermal_tolerance, y = e)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Thermal Tolerance", y = "Activation Energy (Ev)") +
  theme_classic()
thermal_tol_andEa



library(patchwork)

thermal_tol_andEa + thermal_perf_breadth_andEa

ggplot(breadth_and_ea,
       aes(x = thermal_tolerance, y = breadth)) +
  geom_point()

tolerance_and_breadth_graph <- ggplot(data = tolerance_and_breadth, aes(x = breadth, y = thermal_tolerance)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Performance Breadth", y = "Tolerance Breadth") +
  theme_classic()
tolerance_and_breadth


tolerance_and_breadth_model <- lmer(thermal_tolerance ~ breadth + (1 | study_ID), 
                         data = tolerance_and_breadth)

summary(tolerance_and_breadth_model)



#plot fitted model 
## want to make sure only predicting on range of data
breadth_range <- tolerance_and_breadth %>%
  summarise(
    min_breadth = min(breadth),
    max_breadth = max(breadth))
breadth_range
pred_grid <- data.frame(
  breadth = seq(breadth_range$min_breadth,
                     breadth_range$max_breadth,
                     length.out = 200))
pred_grid$pred <- predict(tolerance_and_breadth_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(tolerance_and_breadth_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

tolerance_and_breadth_graph <- ggplot(data = pred_grid, aes(x = breadth)) +
  geom_point(data = tolerance_and_breadth,
             aes(x = breadth, y = thermal_tolerance),
             colour = "black", alpha = 0.75) +
  geom_line(aes(y = pred),
            color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "red", alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Tolerance Breadth") +
  theme_classic()

tolerance_and_breadth_graph




### activation energy and performance breadth 
thermal_perf_breadth_andEa <- ggplot(data = breadth, aes(x = breadth, y = e)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Performance Breadth", y = "Activation Energy (Ev)") +
  theme_classic()
thermal_perf_breadth_andEa

thermal_perf_breadth_andEa_model <- lmer(e ~ breadth + (1 | study_ID), 
                                    data = breadth)

summary(thermal_perf_breadth_andEa_model)



#plot fitted model 
## want to make sure only predicting on range of data
breadth_range <- breadth %>%
  summarise(
    min_breadth = min(breadth),
    max_breadth = max(breadth))
breadth_range
pred_grid <- data.frame(
  breadth = seq(breadth_range$min_breadth,
                breadth_range$max_breadth,
                length.out = 200))
pred_grid$pred <- predict(thermal_perf_breadth_andEa_model, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(thermal_perf_breadth_andEa_model, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

thermal_perf_breadth_andEa_graph <- ggplot(data = pred_grid, aes(x = breadth)) +
  geom_point(data = breadth,
             aes(x = breadth, y = e),
             colour = "black", alpha = 0.75) +
  geom_line(aes(y = pred),
            color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "red", alpha = 0.20) +
  labs(x = "Performance Breadth", y = "Activation Energy") +
  theme_classic()

thermal_perf_breadth_andEa_graph




### activation energy and tolerance breadth 
thermal_tol_andEa <- ggplot(data = tolerance, aes(x = thermal_tolerance, y = e)) +
  geom_point(color = "black", alpha = .75) +
  labs(x = "Tolerance Breadth", y = "Activation Energy (Ev)") +
  theme_classic()
thermal_tol_andEa

thermal_tol_andEamodel <- lm(e ~ thermal_tolerance, data = tolerance)
summary(thermal_tol_andEamodel)



#plot fitted model 
## want to make sure only predicting on range of data
tolerance_range <- tolerance %>%
  summarise(
    min_tol = min(thermal_tolerance),
    max_tol = max(thermal_tolerance))
tolerance_range
pred_grid <- data.frame(
  thermal_tolerance = seq(tolerance_range$min_tol,
                tolerance_range$max_tol,
                length.out = 200))
pred_grid$pred <- predict(thermal_tol_andEamodel, newdata = pred_grid, re.form = NA)
pred_grid$se   <- predict(thermal_tol_andEamodel, newdata = pred_grid, re.form = NA, se.fit = TRUE)$se.fit

pred_grid$lower <- pred_grid$pred - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$pred + 1.96 * pred_grid$se

thermal_tol_andEa_graph <- ggplot(data = pred_grid, aes(x = thermal_tolerance)) +
  geom_point(data = tolerance,
             aes(x = thermal_tolerance, y = e),
             colour = "black", alpha = 0.75) +
  geom_line(aes(y = pred),
            color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "red", alpha = 0.20) +
  labs(x = "Tolerance Breadth", y = "Activation Energy") +
  theme_classic()

thermal_tol_andEa_graph


activation_and_breadth <- thermal_tol_andEa_graph + thermal_perf_breadth_andEa_graph

ggsave("activation_eng_and_breadth_regression.pdf", plot = activation_and_breadth, path = here("figures"), width = 8, height = 4)


#### Do the groups differ in mean activation energies (ANOVA) ####

average_ee_TM <- average_ee_TM %>%
  mutate(Trait.motivation = factor(Trait.motivation, levels = c("negative", "voluntary", "positive", "autonomic"))) 
##removing outliers
length(average_ee_TM$averaged_e)

# Calculate Z-scores
z_scores <- scale(average_ee_TM$averaged_e)

# Define a threshold (e.g., 3 standard deviations)
threshold <- 2

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
model2 <- glm(averaged_e ~ Trait.motivation, data = average_ee_TM, family = Gamma(link = "log"))
summary(model2)
AIC(model1, model2)
means1 <- emmeans(model1, ~ Trait.motivation)
means2 <- emmeans(model2, ~ Trait.motivation)

# Get all pairwise contrasts from the means
pairwise_contrasts <- pairs(means)
summary(pairwise_contrasts)
