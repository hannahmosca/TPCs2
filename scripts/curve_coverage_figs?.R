  #### curve coverage figures ####
  rm(list=ls())
  #### figure for coverage distribution ####
  # load data in # curve params is what im making figure from, but need to join correct trait characteristics from curves 
  
  curve_sorted <- readRDS(here('processed-data', 'wild_tpcs_data_coverage_sorted19_1_2026.RDS')) %>%
    select(!(c(Trait.Group, Trait.motivation))) 
  curve_sorted <- curve_sorted %>%
    mutate(land_or_sea = ifelse(land_or_sea == "terrestrial", "freshwater", "marine"))
  curves <- readRDS(here('processed-data', 'wild-tpcsupdated.Rds')) %>%
    mutate(Trait.Group = ifelse(Trait.Group == "survival", "Survival", Trait.Group)) %>%
    mutate(Trait.Group = ifelse(Trait.Group == "reproduction", "Reproduction", Trait.Group)) %>%
    mutate(Trait.Group = ifelse(Trait.Group == "somatic growth", "Somatic Growth", Trait.Group)) %>%
    mutate(Trait.Group = ifelse(Trait.Group == "metabolism", "Metabolism", Trait.Group))
  curve_sorted <- curve_sorted %>%
    left_join(curves %>% select(curve_ID, Trait.Group, Trait.motivation, organization), join_by(curve_ID)) %>%
    distinct()
  #don't need anymore
  rm(curves)
  ## filter out some things so only working with unique coverage information
  curve_sorted <- curve_sorted %>%
    select(n_unique_temps, curve_ID, study_ID, species_ID, given_trait_name, Trait.Group, Trait.motivation, organization, curve_type, land_or_sea, abs_latitude, habitat_water, dataset_type, topt_TF, thermal_min_TF, thermal_max_TF, increasing_side_TF, decreasing_side_TF) %>%
    distinct() %>%
    mutate(n_unique_temps_capped = ifelse(n_unique_temps >= 7, "7+", n_unique_temps))
  
temps_his <- ggplot(data = curve_sorted, aes(x = n_unique_temps, fill = n_unique_temps_capped)) +
    geom_histogram(binwidth = 1, alpha = 0.6, color = "black") +
    scale_fill_manual(values = c("4" = "#E3D6C6",  
                                 "5" = "#7FA38D", 
                                 "6" = "navy",
                                 "7+" = "#6E6E6E")) +
  scale_y_continuous(breaks = seq(0,220,25)) +
  theme_minimal() + 
    labs(x = "Temperature Treatments", y = "Number of datasets") + 
    theme(
      text = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    labs(fill = "Temperature\nManipulations")
temps_his
  
  
## want to order it by how much is most seen
  curve_sorted <- curve_sorted %>%
    mutate(dataset_type = factor(
      dataset_type,
      levels = c("right_bound","right_bound_withopt", "left_bound", "full_curve","unbounded_decreasing", "left_bound_withopt", "irregular","unbounded_increasing", "topt"))) %>%
    mutate(n_unique_temps_capped = factor(n_unique_temps_capped,
                                   levels = c("7+", "6", "5", "4")))
  

a <- ggplot(data = curve_sorted, aes(x = dataset_type, fill = (n_unique_temps_capped))) +
  geom_bar(position = "stack", colour = "black",linewidth = 0.2) +
  scale_fill_manual(values = c("4"= "#D9D0DE",
                    "5" = "#BC8da0",
                    "6" ="#64525A",
                    "7+" = "#0C1713")) +
  xlab(NULL) +
  ylab("Number of datasets") +
  scale_y_continuous(expand = expansion(mult = 0.00),
                      breaks = seq(0,150,25)) +
  scale_x_discrete(
    labels = c(
      "full_curve" = "Full curve",
      "left_bound_withopt" = "T-min + T-opt",
      "right_bound_withopt" = "T-max + T-opt", 
      "topt" = "T-opt only",
      "left_bound" = "T-min only",
      "right_bound" = "T-max only",
      "unbounded_increasing" = "Unbound, inc",
      "unbounded_decreasing" = "Unbound, dec",
      "irregular" = "Irregular")) +
  coord_flip() +
    theme_classic() +
    theme(
      legend.position = "right",
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

a
ggsave("dataset_type_his_temp.png", plot = a, path = here("figures"), width = 6, height = 4)

library(patchwork)

a <- ggplot(data = coverage_sorted, aes(x = dataset_type, fill = (habitat_water))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("marine" = "navy", "freshwater" = "olivedrab3", "brackish" = "gold2"))+
  xlab("Dataset Coverage") +
  ylab("Count") +
  scale_x_discrete(
    labels = c(
      "full_curve" = "Full curve",
      "left_bound_withopt" = "T-minimum + optimum",
      "right_bound_withopt" = "T-maximum + optimum", 
      "topt" = "T-optimum only",
      "left_bound" = "T-minimum only",
      "right_bound" = "T-maximum only",
      "unbounded_increasing" = "Unbounded, increasing",
      "unbounded_decreasing" = "Unbounded, decreasing",
      "irregular" = "Irregular"
    )) +
  labs(fill = "Realm") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
a
ggsave("dataset_type_his_water.pdf", plot = a, path = here("figures"), width = 7, height = 4)

temps <- curves %>%
  select(curve_ID, study_ID, test_temp) %>%
  group_by(curve_ID, study_ID) %>%
  summarise(
    min_temp = min(test_temp, na.rm = TRUE),
    max_temp = max(test_temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct(study_ID, min_temp, max_temp, .keep_all = TRUE) %>%
  group_by(study_ID) %>%
  mutate(line_num = row_number()) %>%  # give each range within a study a unique line position
  ungroup() %>%
  mutate(study_line = paste(study_ID, line_num, sep = "_"))  # unique ID for plotting

range <- ggplot(temps, aes(y = reorder(study_line, min_temp))) +
  geom_segment(aes(x = min_temp, xend = max_temp, yend = study_line)) +
  geom_point(aes(x = min_temp, y = study_line), color = "blue", size = 2) +
  geom_point(aes(x = max_temp, y = study_line), color = "red", size = 2) +
  labs(x = "Temperature Range Tested", y = "Study") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


range

library(patchwork)
left_column <- temps_his / a + 
  plot_layout(ncol = 1, heights = c(1, 1))  
left_column
right_column <- range + plot_layout(widths = 2)
final_plot <- left_column | right_column  +           
  plot_annotation(tag_levels = "A")
final_plot
ggsave(
  filename = here("figures", "temp_coverage1.png"),  # Corrected filename placement
  plot = final_plot, 
  width = 9, 
  height = 6, 
  dpi = 300, 
  device = "png"
)
#hi
#### figure about curve / parameter coverage ####
