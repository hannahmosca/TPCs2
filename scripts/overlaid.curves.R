#### all curves ####
rm(list = ls())
predictions <- readRDS(here("processed-data", "top_model_predictions.RDS")) %>%
  select(-("dataset_type.y")) %>%
  rename(dataset_type = dataset_type.x)
params <- readRDS(here("processed-data", "tpcs_with_fitted_params.RDS"))

curves <- readRDS(here("processed-data", "wild-tpcs-clean.RdS"))
curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(max_response = max(abs(response_value), na.rm = TRUE))

predictions <- predictions %>%
  left_join(curves %>% select(curve_ID, max_response, latitude, abs_latitude), join_by(curve_ID)) %>%
  distinct() %>%
  left_join(params %>% select(curve_ID, topt_TF, topt)) %>%
  distinct()




predictions_norm_all <- predictions %>%
  filter(dataset_type %in% c("topt", "full_curve", "left_bound_withopt", "right_bound_withopt", "unbounded_increasing", "unbounded_decreasing", "left_bound", "right_bound")) %>%
  group_by(curve_ID) %>%
  mutate(norm_response = (.fitted - min(.fitted, na.rm = TRUE)) /
           (max(.fitted, na.rm = TRUE) - min(.fitted, na.rm = TRUE))) %>%
  ungroup()

all <-ggplot(predictions_norm_all, aes(x = test_temp, y = norm_response, group = curve_ID)) +
  geom_line(aes(color = abs_latitude), linewidth = .7, alpha = 0.6) +
  scale_color_gradientn(colors = c("#d7191c", "#FDE725", "turquoise3")) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0.1,0.1))) +
  theme_classic() +
  labs(color = "Absolute latitude",
       x = "Temperature (°C)",
       y = "Normalized fitted response")
all


all <- ggplot(predictions_norm_all, aes(test_temp, norm_response, group = curve_ID)) +
  geom_line(aes(color = abs_latitude),
            linewidth = 0.5, alpha = 0.4, lineend = "round") +
  scale_color_gradientn(colors = c("#d7191c", "#FDE725", "turquoise3")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 1.10), clip = "off") +
  theme_classic() +
  theme(plot.margin = margin(5.5, 12, 5.5, 5.5)) +
  labs(color = "Absolute latitude",
       x = "Temperature (°C)",
       y = "Normalized fitted response")
all
peaks <- predictions_norm_all %>%
  group_by(curve_ID) %>%
  filter(dataset_type %in% c("topt", "full_curve", "left_bound_withopt", "right_bound_withopt")) %>%
  slice_max(norm_response, n = 1, with_ties = FALSE) %>%
  ungroup()

all <- ggplot(predictions_norm_all, aes(test_temp, norm_response, group = curve_ID)) +
  geom_line(aes(color = abs_latitude),
            linewidth = 0.35, alpha = 0.18, lineend = "round") +
  geom_point(data = predictions_norm_all %>% filter(topt_TF == TRUE),
             aes(x = topt, y = 1, color = abs_latitude),
             size = 0.6, alpha = 0.9) +
  scale_color_gradientn(colors = c("#d7191c", "#FDE725", "turquoise3")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 1.05)) +
  theme_classic() +
  labs(color = "Absolute latitude",
       x = "Temperature (°C)",
       y = "Normalized fitted response")
all
ggsave("curve_vis_all.pdf", plot = all, path = here("figures"), width = 10, height = 3)

predictions_norm_less <- predictions %>%
  filter(dataset_type %in% c("full_curve", "right_bound_withopt", "left_bound_withopt", "topt")) %>%
  group_by(curve_ID) %>%
  mutate(norm_response = (.fitted - min(.fitted, na.rm = TRUE)) /
           (max(.fitted, na.rm = TRUE) - min(.fitted, na.rm = TRUE))) %>%
  ungroup()

less <- ggplot(predictions_norm_less, aes(x = test_temp, y = norm_response, group = curve_ID)) +
  geom_line(aes(color = abs_latitude), linewidth = .5, alpha = 0.6) +
  scale_color_gradientn(colors = c("#d7191c", "#FDE725", "turquoise3")) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.015))) +
  theme_classic() +
  labs(color = "Absolute latitude",
       x = "Temperature (°C)",
       y = "Normalized fitted response")
less
ggsave("curve_vis_less.pdf", plot = less, path = here("figures"), width = 7, height = 3)

predictions_norm_less_less <- predictions %>%
  filter(dataset_type %in% c("topt")) %>%
  group_by(curve_ID) %>%
  mutate(norm_response = (.fitted - min(.fitted, na.rm = TRUE)) /
           (max(.fitted, na.rm = TRUE) - min(.fitted, na.rm = TRUE))) %>%
  ungroup()
less_less <- ggplot(predictions_norm_less_less, aes(x = test_temp, y = norm_response, group = curve_ID)) +
  geom_line(aes(color = abs_latitude), linewidth = .65, alpha = 0.6) +
  scale_color_gradientn(colors = c("#d7191c", "#FDE725", "turquoise3")) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  theme_classic() +
  labs(color = "Absolute latitude",
       x = "Temperature (°C)",
       y = "Normalized fitted response")
less_less
ggsave("curve_vis_less_less.pdf", plot = less_less, path = here("figures"), width = 7, height = 3)



