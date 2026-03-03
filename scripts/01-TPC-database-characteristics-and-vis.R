#### ============================================================
#### Script info
#### ============================================================
# Title: TPC-data-characteristics.R
# Description:
# Summarize FishTherm dataset characteristics
#### ============================================================
#### 1. load packages ####
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)
library(maps)

#### 2. load TPCs and species taxonomy ####
curves <- read_csv(here("processed-data","FishTherm.csv")) %>%
  select(-(...1))
taxa <- read_csv(here("processed-data", "taxonomy.csv"))

#how many curves?
length(unique(curves$curve_ID)) #457
#how many different studies?
length(unique(curves$study_ID)) #118
#how many unique species?
length(unique(taxa$species_ID)) #107

#### 3. info about contributing data by study ####
# how many datasets from each study, helps us better understand how to group and minimize pseudorepl.
curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1) %>%
  select(curve_ID, curve_type, species_ID, latitude, longitude, given_trait_name, Trait.Group, Trait.motivation, response_sample_type, life_stage_tested, treatment_1_type, study_ID, habitat, habitat_water, land_or_sea, n_unique_temps) %>%
  ungroup %>%
  group_by(study_ID) %>%
  mutate(datasets_per_study = n()) %>%
  ungroup

curvesperstudy <- curves_unique %>%
  group_by(study_ID) %>%
  slice(1)

mean_datasets_per_study <- mean(curvesperstudy$datasets_per_study, na.rm = TRUE) #3.87
median_datasets_per_study <- median(curvesperstudy$datasets_per_study, na.rm = TRUE) #2
max <- max(curvesperstudy$datasets_per_study, na.rm = TRUE) #20

#### 4. dataset characteristics ####

# temperature treatments # 

#median # of test temps?
median(curves_unique$n_unique_temps) #5
mean(curves_unique$n_unique_temps) #5.22
# % above 4, % above 5
# percentage above 4
mean(curves_unique$n_unique_temps > 4) * 100 #51.9
mean(curves_unique$n_unique_temps == 4) *100 #48.1 %
# percentage above 5
mean(curves_unique$n_unique_temps > 5) * 100 #26.5

# habitat distribution
habitats <- curves_unique %>%
  group_by(latitude, longitude) %>%
  slice(1) %>%
  ungroup

#get count of distinct habitats from collection locos 
curves_unique %>% 
  count(habitat)
curves_unique %>% 
  count(habitat_water)

# brackish         18
# freshwater      185
# marine          254


## treatments and life stages #

curves_unique %>%
  count(Trait.Group)

 # Energy Aquisition    67
 # Locomotion           89
 # Metabolism          141
 # Reproduction         20
 # Somatic Growth      122
 # Survival             18

curves_unique %>% 
  count(life_stage_tested)

# 1 adult               104
# 2 embryo               23
# 3 fry                   3
# 4 juvenile            214
# 5 larvae               13
# 6 NA                  100

curves_unique %>% 
  count(treatment_1_type)
# Acclimation = 43
# CO₂/pH = 15
# Oxygen = 6
# Photoperiod = 6
# Ration = 51
# Salinity = 45
# Size = 32
# resting or swimming = 6
# NA = 253

## acute or batch acclimated
curves_unique %>% 
  count(curve_type)
#134 acute, 323 batch aclim

# response measured
curves_unique %>%
  count(response_sample_type)

#40 indv, 414 mean, 3 median
  

#### 5. Data visualization ####
  # This section generates:
  #   1) Stacked barplot: datasets by Trait.Group, filled by Trait.motivation
  #   2) Barplots: datasets represented by taxonomic family (and order)
  #   3) Histogram: number of unique test temperatures per curve
  #   4) Segment plot: min–max test temperature range per study
  #   5) Map: collection locations (unique lat/long)

# stacked barplot: datasets per response group, colored by motivation
# set factor order 
curves_unique <- curves_unique %>%
  mutate(Trait.Group = factor(Trait.Group, levels = c("Survival", "Reproduction", "Energy Aquisition","Locomotion", "Somatic Growth", "Metabolism")),
    Trait.motivation = factor(Trait.motivation, levels = c("autonomic", "voluntary", "positive", "negative")))

# colors
motivation_cols <- c(
  "autonomic"  = "#1B4965",
  "voluntary"  = "#77966D",
  "positive"   = "#BDC667",
  "negative"   = "red3")

p_trait_motivation <- ggplot(curves_unique, aes(x = Trait.Group, fill = Trait.motivation)) +
  geom_bar(position = "stack", colour = "black", linewidth = 0.4) +
  scale_fill_manual(values = motivation_cols, labels = c("Autonomic", "Voluntary", "Positive", "Negative")) +
  labs(x = NULL, y = "Number of datasets", fill = "Trait motivation") +
  scale_y_continuous(expand = expansion(mult = 0), breaks = seq(0, 150, 25)) +
  scale_x_discrete(expand = expansion(mult = 0)) +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_trait_motivation
ggsave("curves_by_trait_group_and_motivation.pdf", p_trait_motivation, path = here("figures"), width = 6, height = 4.5)

# Taxonomic representation: datasets by family ( and order)

# join taxonomy once, then reuse for family/order summaries
curves_taxa <- curves_unique %>%
  select(curve_ID, study_ID, species_ID, Trait.Group, habitat) %>%
  left_join(taxa, by = "species_ID")

# by family
family_counts <- curves_taxa %>%
  count(family, name = "datasets_per_family") %>%
  arrange(desc(datasets_per_family))

p_family <- ggplot(family_counts, aes(x = reorder(family, datasets_per_family), y = datasets_per_family)) +
  geom_col(fill = "black", color = "lightgrey", alpha = 0.9, width = 0.75) +
  coord_flip() +
  labs(x = "Family", y = "Datasets") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10),
    axis.text.y = element_text(size = 7)
  ) +
  scale_y_continuous(expand = c(0, 0))

p_family

ggsave("tax_family_dataset_counts.pdf", p_family, path = here("figures"), width = 5, height = 6)

# by order
order_counts <- curves_taxa %>%
  count(order, name = "datasets_per_order") %>%
  arrange(desc(datasets_per_order))

p_order <- ggplot(order_counts, aes(x = reorder(order, datasets_per_order), y = datasets_per_order)) +
  geom_col(fill = "black", color = "lightgrey", alpha = 0.9, width = 0.9) +
  coord_flip() +
  labs(x = "Order", y = "Datasets") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10),
    axis.text.y = element_text(size = 7)
  ) +
  scale_y_continuous(expand = c(0, 0))

p_order

# Histogram of number of test temperatures per curve

p_test_temps <- ggplot(curves_unique, aes(x = n_unique_temps)) +
  geom_histogram(binwidth = 1, fill = "black", color = "lightgrey", alpha = 0.9) +
  theme_minimal() +
  labs(x = "Number of unique test temperatures", y = "Number of datasets") +
  theme(
    text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_test_temps
ggsave("histogram_test_temps.pdf", p_test_temps, path = here("figures"), width = 5, height = 4)


# temperature ranges tested: min–max test temp per curve, grouped by study
temp_ranges <- curves %>%
  select(curve_ID, study_ID, test_temp) %>%
  group_by(curve_ID, study_ID) %>%
  summarise(min_temp = min(test_temp, na.rm = TRUE), max_temp = max(test_temp, na.rm = TRUE),
            .groups = "drop") %>%
  distinct(study_ID, min_temp, max_temp, .keep_all = TRUE) %>%
  group_by(study_ID) %>%
  mutate(line_num = row_number()) %>%
  ungroup() %>%
  mutate(study_line = paste(study_ID, line_num, sep = "_"))

p_temp_range <- ggplot(temp_ranges, aes(y = reorder(study_line, min_temp))) +
  geom_segment(aes(x = min_temp, xend = max_temp, yend = study_line)) +
  geom_point(aes(x = min_temp), color = "blue", size = 2) +
  geom_point(aes(x = max_temp), color = "red", size = 2) +
  labs(x = "Test temperature range (°C)", y = "Study") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p_temp_range
ggsave("thermal_range_tested.pdf", p_temp_range, path = here("figures"), width = 5, height = 8)

# collection locations map (unique sampling locations)

world_map <- map_data("world")
# 1 point per unique lat/long 
collection_points <- curves_unique %>%
  distinct(latitude, longitude, land_or_sea)

p_collection_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "grey67") +
  geom_point(data = collection_points, aes(x = longitude, y = latitude, fill = land_or_sea), color = "black", size = 3, alpha = 0.55, shape = 21, stroke = 0.3, position = position_jitter(width = 0.1, height = 0.1)) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.line = element_blank(),
    text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("oceanic" = "#1F78B4", "terrestrial" = "#33A02C"))

p_collection_map
ggsave("collection_locations_map.pdf", p_collection_map, path = here("figures"), width = 9.5, height = 5)
