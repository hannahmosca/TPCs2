data <- readRDS(here("processed-data", "tpcs_with_fitted_params_with_act_eng_10_6.RDS"))
curves <- read.csv(here("processed-data", "FishTherm.csv")) %>%
  select(curve_ID, test_temp)

curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(max_test_temp = max(test_temp),
         min_test_temp = min(test_temp)) %>%
  select(curve_ID, min_test_temp, max_test_temp) %>%
  distinct()

plot_data <- data %>%
  select(curve_ID, dataset_type, topt, tmin, tmax, breadth, thermal_tolerance, e_arr) %>%
  pivot_longer(
    cols = c(topt, tmin, tmax, breadth, thermal_tolerance, e_arr),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

n_tpcs <- plot_data %>%
  distinct(curve_ID, dataset_type) %>%
  count(dataset_type, name = "n_tpc")
n_parameter_total <- plot_data %>%
  distinct(curve_ID, parameter) %>%
  count(parameter, name = "n_total")
n_parameter_group <- plot_data %>%
  distinct(curve_ID, dataset_type, parameter) %>%
  count(dataset_type, parameter, name = "n_group")

parameter_summary <- plot_data %>%
  group_by(dataset_type, parameter) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n_distinct(curve_ID),
    .groups = "drop")
parameter_labels <- n_parameter_total %>%
  mutate(label = paste0(parameter, " (n = ", n_total, ")")) %>%
  {setNames(.$label, .$parameter)}

plot_data <- plot_data %>%
  mutate(dataset_type = factor(dataset_type,
                               levels = c("full_curve",
                                          "full_rise_with_opt",
                                          "full_decline_with_opt",
                                          "optimum_only",
                                          "partial_rise_with_min",
                                          "partial_decline_with_max",
                                          "partial_rise",
                                          "partial_decline")))
parameter_labels <- n_parameter_total %>%
  mutate(label = paste0(parameter, " (n = ", n_total, ")")) %>%
  {setNames(.$label, .$parameter)}


ggplot(plot_data, aes(x = dataset_type, y = value, colour = parameter)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.08,
                                             dodge.width = 0.7), alpha = 0.25, size = 1.5) +
  geom_errorbar(data = parameter_summary, aes(y = mean, ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.7), width = 0.12, linewidth = 0.7, show.legend = FALSE) +
  geom_point(
    data = parameter_summary,
    aes(
      y = mean
    ),
    position = position_dodge(width = 0.7),
    size = 3,
    show.legend = FALSE
  ) +
  
  # n parameter estimates within each dataset type
  geom_text(
    data = n_parameter_group %>%
      left_join(
        plot_data %>%
          group_by(dataset_type, parameter) %>%
          summarise(
            y = max(value, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c("dataset_type", "parameter")
      ),
    aes(
      x = dataset_type,
      y = y,
      colour = parameter,
      label = paste0("n = ", n_group)
    ),
    position = position_dodge(width = 0.7),
    vjust = -0.8,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  # n TPCs
  geom_text(
    data = n_tpcs,
    aes(
      x = dataset_type,
      y = -Inf,
      label = paste0("n TPCs = ", n_tpc)
    ),
    inherit.aes = FALSE,
    vjust = -0.5
  ) +
  
  scale_colour_discrete(
    labels = parameter_labels
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  theme_classic() +
  
  theme(
    plot.margin = margin(10, 10, 35, 10)
  ) +
  
  labs(
    x = "Dataset type",
    y = "Parameter value",
    colour = "Parameter"
  )
