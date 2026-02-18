rm(list=ls())
library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages("matrixStats")
library(matrixStats)
marine_sst_points <- readRDS(here("processed-data", "marine_sst_raw_temp.RDS"))
freshwater_temp_points <- readRDS(here("processed-data", "freshwater_temp_data.RDS"))
marine_sst_points <- marine_sst_points %>%
  mutate(abs_lat = abs(latitude))

### all marine temp ###

marine_sst_points <- readRDS(here("processed-data", "marine_sst_raw_temp.RDS"))

marine_sst_all_temps <- readRDS(here("processed-data", "marine_sst_all_temp.RDS"))
marine_sst_all_temps <- marine_sst_all_temps %>%
  rename(longitude = x) %>%
  rename(latitude = y)

temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(marine_sst_all_temps), value = TRUE)
temp_matrix <- as.matrix(marine_sst_all_temps[, temp_cols])

install.packages("matrixStats")
library(matrixStats)
# Select numeric temperature columns
temp_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(marine_sst_all_temps), value = TRUE)
temp_matrix <- as.matrix(marine_sst_all_temps[, temp_cols])


marine_sst_all_temps$sst_mean   <- rowMeans(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_sd     <- rowSds(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_median <- rowMedians(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_min    <- rowMins(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_max    <- rowMaxs(temp_matrix, na.rm = TRUE)
marine_sst_all_temps$sst_range  <- marine_sst_all_temps$sst_max - marine_sst_all_temps$sst_min
marine_sst_all_temps$q_low <- rowQuantiles(temp_matrix, probs = 0.025, na.rm = TRUE)
marine_sst_all_temps$q_high <- rowQuantiles(temp_matrix, probs = 0.975, na.rm = TRUE)

##my point data
ggplot(marine_sst_points) +
  geom_linerange(aes(ymin = sst_mean,
                  ymax = sst_max, x = latitude), color = "lightblue", alpha = 0.6, linewidth = 1.2) +
  geom_point(aes(x = latitude, y = sst_mean)) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean and Max SST by Latitude (1982-2025") +
  theme_classic(base_size = 14)

ggplot(freshwater_temp_points, aes(x = latitude)) +
  geom_linerange(aes(ymin = temp_mean, ymax = temp_max), color = "lightgreen", alpha = 0.6, linewidth = 1.2) +
  geom_point(aes(y = temp_mean), color = "darkgreen", size = 2) +
  labs(
    x = "Latitude",
    y = "Water Temperature (°C)",
    title = "Mean and Max water temperature by Latitude (1982–2025)"
  ) +
  theme_classic(base_size = 14)

### all temp data  ### 
#make more coarse
marine_sst_coarse <- marine_sst_all_temps %>%
  mutate(lat_bin = cut(latitude, breaks = seq(floor(min(latitude)),
                                              ceiling(max(latitude)),
                                              by = 1))) %>% 
  group_by(lat_bin) %>%
  summarise(
    latitude = mean(latitude, na.rm = TRUE),  
    sst_mean = mean(sst_mean, na.rm = TRUE),
    sst_min = min(sst_min, na.rm = TRUE),
    sst_max = max(sst_max, na.rm = TRUE),
    sst_q_low = quantile(q_low, probs = 0.025, na.rm = TRUE),
    sst_q_high = quantile(q_high, probs = 0.975, na.rm = TRUE),
    sst_median = median(sst_median, na.rm = TRUE)
  )
marine_sst_coarse <- marine_sst_coarse %>%
  filter(!is.na(sst_mean) & !is.na(sst_min) & !is.na(sst_max))
ggplot(marine_sst_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = sst_min, ymax = sst_max), fill = "lightblue2", alpha = 0.6, linewidth = 1.2) +
  geom_line(aes(y = sst_mean), color = "navy", size = 2) +
  labs(
    x = "Latitude",
    y = "Sea Surface Temperature (°C)",
    title = "Mean and Range of SST by Latitude (1982–2025, 1° bins)"
  ) +
  theme_classic(base_size = 14)
## sst with topt vis## to show jenn
fitted_datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))
marine <- ggplot(marine_sst_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = sst_q_low, ymax = sst_q_high), fill = "lightblue2", alpha = 0.6, linewidth = 1.2) +
  geom_line(aes(y = sst_mean), color = "navy", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "oceanic") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(
    x = "Latitude",
    y = "Sea Surface Temperature (°C)",
    title = "Mean and Quantiles (1982–2025, 1° bins) with Topt points"
  ) +
  theme_classic()

fresh <- ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = low_q, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = mean_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "Latitude", y = "Water Temperature (°C)") +
  theme_classic()
library(patchwork)
marine + fresh

#### dif between mean and upper quantile ####
freshwater_temp_points <- freshwater_temp_points %>%
  mutate(range_above_mean = temp_max - temp_mean)

marine_sst_points <- marine_sst_points %>%
  mutate(range_above_mean = sst_max - sst_mean)



# Create a shared dataframe with matching column names
freshwater_temp_points_clean <- freshwater_thresholded %>%
  select(temp_median, temp_min, temp_max, temp_mean, q_low, h_low, temp_range) %>%
  rename(q_high = h_low) %>%
  mutate(environment = "Freshwater")

marine_sst_all_clean <- marine_sst_all_temps %>%
  rename(temp_min = sst_min,
         temp_max = sst_max,
         temp_mean = sst_mean,
         temp_median = sst_median,
         temp_range = sst_range) %>%
  select(temp_median, temp_min, temp_max, temp_mean, q_low, q_high, temp_range) %>%
  mutate(environment = "Marine")

# Combine them
combined_temp_points <- bind_rows(freshwater_temp_points_clean, marine_sst_all_clean)



ggplot(combined_temp_points, aes(x = environment, y = q_high - temp_mean, fill = environment)) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.4) +
  scale_fill_manual(values = c("Marine" = "royalblue", "Freshwater" = "darkgreen")) +
  labs(
    x = "Enviornment",
    y = "Q-high - Mean Temp (°C)") +
  theme_classic()

     


marine_sst_points <- marine_sst_points %>%
  mutate(abs_lat = abs(latitude))

ggplot(marine_sst_all_temps, aes(x = latitude, y = sst_mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = sst_mean - sst_sd,
                  ymax = sst_mean + sst_sd),
              alpha = 0.1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean SST by Latitude with SD shading") +
  theme_minimal()

ggplot(marine_sst_points, aes(x = latitude, y = sst_mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = sst_mean - sst_sd,
                  ymax = sst_mean + sst_sd),
              alpha = 0.1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean SST by Latitude with ±1 SD shading") +
  theme_minimal()


marine_sst_long <- marine_sst_points %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),  # all monthly date columns
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date)
  )
sst_lat_summary <- marine_sst_long %>%
  group_by(latitude) %>%
  summarise(
    mean_temp = mean(sst, na.rm = TRUE),
    sd_temp   = sd(sst, na.rm = TRUE),
    q_low     = quantile(sst, 0.025, na.rm = TRUE),
    q_high    = quantile(sst, 0.975, na.rm = TRUE),
    min_temp  = min(sst, na.rm = TRUE),
    max_temp  = max(sst, na.rm = TRUE)
  )




ggplot(marine_sst_all_temps, aes(x = latitude)) +
  geom_linerange(aes(ymin = sst_min, ymax = sst_max), color = "lightblue", alpha = 0.6, linewidth = 1.2) +
  geom_point(aes(y = sst_mean), color = "navy", size = 2) +
  labs(
    x = "Latitude",
    y = "Sea Surface Temperature (°C)",
    title = "Mean and Range of SST by Latitude (1982–2025)"
  ) +
  theme_classic(base_size = 14)

p <- ggplot(marine_sst_all_temps, aes(x = latitude)) +
  geom_ribbon(aes(ymin = q_low, ymax = q_high), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = sst_mean), color = "navy", linewidth = 1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean and Range of SST by Latitude (1982–2025)") +
  theme_classic(base_size = 14)
p
ggplot(marine_sst_all_temps, aes(x = latitude)) +
  geom_ribbon(aes(ymin = sst_min, ymax = sst_max), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = sst_mean), color = "navy", linewidth = 1) +
  labs(x = "Latitude", y = "Sea Surface Temperature (°C)",
       title = "Mean and Range of SST by Latitude (1982–2025)") +
  theme_classic(base_size = 14)

#### freshwater ####
freshwater_unthresholded <- readRDS(here("processed-data", "freshwater_all_df_no_threshold.RDS"))
freshwater_thresholded <- readRDS(here("processed-data", "freshwater_all_df_threshold.RDS"))
fitted_datasets <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS"))

#####unthresholded frequency distribution####
## pivot longer
freshwater_subset <- freshwater_unthresholded %>%
  filter(
    latitude >= 0 & latitude <= 20,
    longitude >= 0 & longitude <= 20
  ) %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "temperature"
  ) %>%
  mutate(date = as.Date(date))

ggplot(freshwater_subset, aes(x = temperature)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "white", alpha = 0.7) +
  labs(
    x = "Experienced Temperature (°C)",
    y = "Frequency",
    title = "Frequency Distribution of Experienced Temperatures (Freshwater)"
  ) +
  theme_minimal(base_size = 14)




# ggplot(freshwater_thresholded) +
#   geom_linerange(aes(ymin = q_low,
#                      ymax = q_high, x = latitude), color = "lightgreen", alpha = .6, linewidth = 1) +
#   geom_point(aes(x = latitude, y = temp_mean)) +
#   labs(x = "Latitude", y = "Water Temperature (1982-2025 month averages)") +
#   theme_classic()

monthly_fresh_thresholded_df_coarse <- freshwater_thresholded %>%
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

ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = mean_temp, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = mean_temp), color = "darkgreen", size = 2) +
  geom_point(data = fitted_datasets %>%
               filter(land_or_sea == "terrestrial") %>%
               filter(topt_TF == TRUE), aes(x = latitude, y = topt), color = "black", alpha = .4) +
  labs(x = "latitude", y = "water temperature") +
  theme_classic()
ggplot(monthly_fresh_thresholded_df_coarse, aes(x = latitude)) +
  geom_ribbon(aes(ymin = low_q, ymax = high_q), fill = "lightgreen", alpha = .6, linewidth = 1.2) +
  geom_line(aes (y = mean_temp), color = "darkgreen", size = 2) +
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



