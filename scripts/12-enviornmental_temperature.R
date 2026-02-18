#### this script is to inspect at the different enviornmental temperatures across both realms ####

## libraries 
library(here)
library(dplyr)
library(tidyverse)
library(matrixStats)

# data
marine_all <- readRDS(here("processed-data", "marine_sst_all_temp.RDS")) 
marine_points <- readRDS(here("processed-data", "marine_sst_raw_temp.RDS"))    
freshwater_all_raw <- readRDS(here("processed-data", "freshwater_all_df_no_threshold.RDS")) 
freshwater_all_thresholded <- readRDS(here("processed-data", "freshwater_all_df_threshold.RDS")) 
freshwater_points <-readRDS(here("processed-data", "freshwater_temperatures_my_points.RDS")) 

#### adjusting columns ####
## adjusting marine_all columns

# need to compute sum stats for marine_alll
temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(marine_all), value = TRUE)
temp_matrix <- as.matrix(marine_all[, temp_cols])
marine_all$temp_mean   <- rowMeans(temp_matrix, na.rm = TRUE)
marine_all$temp_sd    <- rowSds(temp_matrix, na.rm = TRUE)
marine_all$temp_median <- rowMedians(temp_matrix, na.rm = TRUE)
marine_all$temp_min   <- rowMins(temp_matrix, na.rm = TRUE)
marine_all$temp_max   <- rowMaxs(temp_matrix, na.rm = TRUE)
marine_all$temp_range <- marine_all$temp_max - marine_all$temp_min
marine_all$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
marine_all$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)

marine_all <- marine_all %>%
  rename(longitude = x) %>%
  rename(latitude = y) %>%
  mutate(enviornment = "marine")

#adjusting columns for marine_points
marine_points <- marine_points %>%
  rename(
    temp_mean = sst_mean,
    temp_sd = sst_sd,
    temp_median = sst_median,
    temp_min = sst_min,
    temp_max = sst_max,
    temp_range = sst_range
  ) %>%
  rowwise() %>%
  mutate(
    q_low = quantile(c_across(`1982-01-01`:`2025-09-01`), probs = 0.025, na.rm = TRUE),
    q_high = quantile(c_across(`1982-01-01`:`2025-09-01`), probs = 0.975, na.rm = TRUE),
    environment = "marine"
  ) %>%
  ungroup()

# adjusting columns for freshwater_all_raw
freshwater_all_raw <- freshwater_all_raw %>%
  rename(q_high = h_low) %>%
  mutate(enviornment = "freshwater")
#adjust for freshwater_all_thresholded
freshwater_all_thresholded <- freshwater_all_thresholded %>%
  select(-(h_low)) %>%
  mutate(enviornment = "freshwater")
#adjust for freshwater_points
freshwater_points <- freshwater_points %>%
  rename(q_low = temp_q_low) %>%
  rename(q_high = temp_q_high) %>%
  mutate(enviornment = "freshwater")

#### marine temperature distribution, need to pivot long####
marine_points_long <- marine_points %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  ) %>%
  mutate(date = as.Date(date))

ggplot(marine_points_long, aes(x = temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white", alpha = 0.7) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures (marine)"
  ) +
  theme_classic()

##subset of marine all lat/long
marine_subset_long <- marine_all %>%
  filter(latitude == 28.375) %>%
  filter(longitude == 125.625) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  ) %>%
  mutate(date = as.Date(date))

ggplot(marine_subset_long, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = temp_mean), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = temp_median), color = "orange", linewidth = 1) +
  geom_vline(aes(xintercept = q_low), color = "black", linewidth = 1) +
  geom_vline(aes(xintercept = q_high), color = "black", linewidth = 1) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures \n at 28.375°, 125.625° monthly averages from 1982-2025"
  ) +
  theme_classic()


##subset of freshwater all lat/long for raw data
##rounding
freshwater_all_raw <- freshwater_all_raw %>%
  mutate(
    latitude = round(latitude, 5),
    longitude = round(longitude, 5)
  )

freshwater_subset <- freshwater_all_raw %>% 
  filter(latitude == 33.58333) %>%
  filter(longitude == -89.25000) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  ) %>%
  mutate(date = as.Date(date))

ggplot(freshwater_subset, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = temp_mean), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = temp_median), color = "orange", linewidth = 1) +
  geom_vline(aes(xintercept = q_low), color = "black", linewidth = 1) +
  geom_vline(aes(xintercept = q_high), color = "black", linewidth = 1) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures \n at 22.25, 100.58333° monthly averages from 1982-2025"
  ) +
  theme_classic()

##look at my points

freshwater_points_long <- freshwater_points %>% 
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  )

### all point locations
ggplot(freshwater_points_long, aes(x = temperature)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "white", alpha = 0.7) +
  # geom_vline(aes(xintercept = temp_mean), color = "red", linewidth = 1) +
  # geom_vline(aes(xintercept = temp_median), color = "orange", linewidth = 1) +
  # geom_vline(aes(xintercept = q_low), color = "black", linewidth = 1) +
  # geom_vline(aes(xintercept = q_high), color = "black", linewidth = 1) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures over all point locations monthly averages from 1982-2025"
  ) +
  theme_classic()

##individual my points
freshwater_subset <- freshwater_points %>% 
  filter(latitude == 41.725964) %>%
  filter(longitude == -72.658727) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  )

ggplot(freshwater_subset, aes(x = temperature)) +
  geom_histogram(binwidth = .5, fill = "lightgreen", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = temp_mean), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = temp_median), color = "orange", linewidth = 1) +
  geom_vline(aes(xintercept = q_low), color = "black", linewidth = 1) +
  geom_vline(aes(xintercept = q_high), color = "black", linewidth = 1) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures \n at specific locs monthly averages from 1982-2025"
  ) +
  theme_classic()











ggplot(monthly_fresh_df) + 
  geom_linerange(aes(ymin = q_low,
                     ymax = h_low, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = temp_mean)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()

ggplot(monthly_fresh_thresholded_df) +
  geom_linerange(aes(ymin = q_low,
                     ymax = q_high, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = temp_mean)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()

monthly_fresh_thresholded_df_coarse <- monthly_fresh_thresholded_df %>%
  mutate(lat_bin = cut(latitude, breaks = seq(floor(min(latitude)),
                                              ceiling(max(latitude)),
                                              by = 1))) %>%
  group_by(lat_bin) %>%
  summarise(
    latitude = mean(latitude, na.rm = TRUE),
    mean_temp = mean(temp_mean, na.rm = TRUE),
    median_temp = median(temp_median, na.rm = TRUE),
    max_temp = max(temp_max, na.rm = TRUE),
    min_temp = min(temp_min, na.rm = TRUE),
    low_q = quantile(q_low, probs = 0.025, na.rm = TRUE),
    high_q = quantile(q_high, probs = 0.975, na.rm = TRUE))

ggplot(monthly_fresh_thresholded_df_coarse) +
  geom_linerange(aes(ymin = low_q,
                     ymax = high_q, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
  geom_point(aes(x = latitude, y = mean_temp)) +
  labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
  theme_classic()
fitted_datasets <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS"))
ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = median_temp, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = median_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "latitude", y = "water temperature") +
  theme_classic()
ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = mean_temp, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = mean_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "latitude", y = "water temperature") +
  theme_classic()

freshwater_to_save <- monthly_fresh_df %>%
  select(latitude, longitude, temp_mean, temp_sd, temp_median, temp_min, temp_max, temp_range)




