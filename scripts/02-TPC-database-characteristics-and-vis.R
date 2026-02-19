#### script info #### 
#title: TPC-data-characteristics.R
#author: Hannah Mosca

#### 1. load packages ####
library(ggpattern)
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)
library(maps)

#### 2. load TPCs and species taxonomy ####
curves <- readRDS(here("processed-data","wild-tpcs-clean.RdS")) 
taxa <- read_csv(here("processed-data", "taxonomy.csv"))

#### 3. info about contributing data by study ####
#how many curves?
length(unique(curves$curve_ID)) #457
#how many different studies?
length(unique(curves$study_ID)) #118
#how many unique species?
length(unique(taxa$species_ID)) #107

# how many datasets from each study, helps us better understand how to group and minimize pseudorepl.

curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1) %>%
  select(curve_ID, curve_type, species_ID, latitude, longitude, given_trait_name, Trait.Group, Trait.motivation, response_curve_type, life_stage_tested, treatment_1_type, study_ID, habitat, habitat_water, land_or_sea, n_unique_temps) %>%
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
  count(response_curve_type)

#40 indv, 414 mean, 3 median
  

#### 5. Data vis ####

## barplot of how many datasets in each response group, colored by response motivation ## 

# order it by how much is most seen
curves_unique <- curves_unique %>%
  mutate(Trait.Group = factor(
    Trait.Group,
    levels = c("Survival","Reproduction", "Energy Aquisition","Locomotion", "Somatic Growth", "Metabolism"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation,
                                        levels = c("autonomic", "voluntary", "positive", "negative")))

a <- ggplot(data = curves_unique, aes(x = Trait.Group, fill = (Trait.motivation))) +
  geom_bar(position = "stack", colour = "black",linewidth = .4) +
  scale_fill_manual(values = c( "autonomic" = "#1B4965","negative"  = "red3","voluntary"  = "#77966D","positive" = "#BDC667"), labels = c("Autonomic", "Voluntary","Positive","Negative")) +
  xlab(NULL) +
  ylab("Number of datasets") +
  labs(fill = "Trait Motivation") +
  scale_y_continuous(expand = expansion(mult = 0.00),breaks = seq(0,150,25)) +
  scale_x_discrete(expand = expansion(mult = 0)) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "right", axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
a
ggsave("dataset_tpe_his_temp.pdf", plot = a, path = here("figures"), width = 6, height = 4.5)

## barplot of represented species in database ## 

#by family
curves_unique_with_taxa <- curves_unique %>%
  select(curve_ID, study_ID, species_ID, Trait.Group, habitat) %>%
  left_join(taxa, join_by(species_ID)) %>%
  group_by(family) %>%
  mutate(datasets_per_family = n()) %>%
  select(datasets_per_family, family) %>%
  distinct()
unique(curves_unique_with_taxa$family)

family <- ggplot(data = curves_unique_with_taxa,aes(x = reorder(family, datasets_per_family, FUN = function(x) -x), y = datasets_per_family)) + 
  geom_bar(stat = "identity", fill = "darkslategrey", color = "lightgrey", alpha = 0.9, width = .9) +
  coord_flip() +
  xlab("Family") + 
  ylab("Datasets") +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 10),axis.text.y = element_text(size = 7)) +
  scale_y_continuous(expand = c(0, 0))
family

#by order 
curves_unique_with_taxa_order <- curves_unique %>%
  select(curve_ID, study_ID, species_ID, Trait.Group, habitat) %>%
  left_join(taxa, join_by(species_ID)) %>%
  group_by(order) %>%
  mutate(datasets_per_order = n()) %>%
  select(datasets_per_order, order) %>%
  distinct()
unique(curves_unique_with_taxa_order$order)

order <- ggplot(data = curves_unique_with_taxa_order,aes(x = reorder(order, datasets_per_order, FUN = function(x) -x), y = datasets_per_order)) + 
  geom_bar(stat = "identity", fill = "darkslategrey", color = "lightgrey", alpha = 0.9, width = .9) +
  coord_flip() +
  xlab("Order") + 
  ylab("Datasets") +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 10),axis.text.y = element_text(size = 7)) +
  scale_y_continuous(expand = c(0, 0))
order

ggsave("histogram_order.pdf", plot = family, path = here("figures"), width = 5, height = 6)


## histogram of # of test temperatures ##
a <- ggplot(curves_unique, aes(x = n_unique_temps)) +
  geom_histogram(binwidth=1, fill="black", color="lightgrey", alpha=0.9) +
  theme_minimal() + 
  labs(x = "Number of Temperatures", y = "Number of datasets") + 
  theme(
    text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
a
ggsave("histogram_test_temps.pdf", plot = a, path = here("figures"), width = 5, height = 4)


## visualize the temperature ranges tested ##

temps <- curves %>%
  select(curve_ID, study_ID, test_temp, latitude, longitude) %>%
  group_by(curve_ID) %>%
  mutate(temp_range = (max(test_temp) - min(test_temp))) %>%
  mutate(min_temp = min(test_temp)) %>%
  mutate(max_temp = max(test_temp)) %>%
  slice(1) %>%
  group_by(study_ID, temp_range) %>%
  mutate(count = n())

temps <- curves %>%
  select(curve_ID, study_ID, test_temp) %>%
  group_by(curve_ID, study_ID) %>%
  summarise(min_temp = min(test_temp, na.rm = TRUE), max_temp = max(test_temp, na.rm = TRUE),.groups = "drop") %>%
  distinct(study_ID, min_temp, max_temp, .keep_all = TRUE) %>%
  group_by(study_ID) %>%
  mutate(line_num = row_number()) %>%  # give each range within a study a unique line position
  ungroup() %>%
  mutate(study_line = paste(study_ID, line_num, sep = "_"))  # unique ID for plotting

range <- ggplot(temps, aes(y = reorder(study_line, min_temp))) +
  geom_segment(aes(x = min_temp, xend = max_temp, yend = study_line)) +
  geom_point(aes(x = min_temp, y = study_line), color = "blue", size = 2) +
  geom_point(aes(x = max_temp, y = study_line), color = "red", size = 2) +
  labs(x = "Temperature Range", y = "Study") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
range
ggsave("thermal_range_tested.pdf", plot = range, path = here("figures"), width = 5, height = 8)

## visualize collection locations ##
world_map <- map_data("world")
map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "grey67") +
  geom_point(data = curves_unique %>%
               group_by(latitude, longitude) %>%
               slice(1), 
             aes(x = longitude, y = latitude, fill = land_or_sea), color = "black", size = 3, alpha = .55, shape = 21, stroke = 0.3, position = position_jitter(width = 0.1, height = 0.1)) + 
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank(), text = element_text(size = 16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),plot.background = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = c("oceanic" = "#1F78B4","terrestrial" = "#33A02C"))
map
ggsave("collection_locations_map.pdf", plot = map, path = here("figures"), width = 9.5, height = 5)
