#### script info #### 
#title: TPC-data-characteristics.R
#author: Hannah Mosca
rm(list=ls())
#### 1. load packages ####
library(ggpattern)
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

#### 2. load TPCs and species taxonomy ####
curves <- readRDS(here("processed-data","wild-tpcsupdated.RdS")) %>%
  mutate(Trait.Group = ifelse(Trait.Group == "Cardiac", "metabolism", Trait.Group)) %>%
  mutate(Trait.motivation = ifelse(Trait.motivation == "", "autonomic", Trait.motivation)) %>%
  mutate(Trait.motivation = ifelse(Trait.motivation == "**check exp", "autonomic", Trait.motivation)) %>%
  mutate(Trait.Group = case_when(
  Trait.Group == "metabolism"      ~ "Metabolism",
  Trait.Group == "reproduction"    ~ "Reproduction",
  Trait.Group == "somatic growth"  ~ "Somatic Growth",
  Trait.Group == "survival"        ~ "Survival",
  Trait.Group == "Energy aquisition"~ "Energy Acquisition",
  TRUE                             ~ Trait.Group
))

saveRDS(curves, here("processed-data","wild-tpcsupdated.RdS"))
rm(list=ls())


curves <- readRDS(here("processed-data","wild-tpcsupdated.RdS"))
unique(curves$Trait.motivation) #check
unique(curves$Trait.Group)

taxa <- read_csv(here("processed-data", "taxonomy_16_12_2025.csv"))
#how many curves?
length(unique(curves$curve_ID)) #457
#how many different studies?
length(unique(curves$study_ID)) #118
#how many unique species?
length(unique(taxa$species_ID)) #107
##so i can do a breakdown of the datsets##
length(unique(curves$latitude))
length(unique(curves$longitude))
curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1) %>%
  group_by(study_ID) %>%
  mutate(datasets_per_study = n()) %>%
  ungroup

curves_unique <- curves %>%
  group_by(curve_ID) %>%
  slice(1) %>%
  select(curve_ID, curve_type, species_ID, latitude, longitude, given_trait_name, Trait.Group, Trait.motivation, response_curve_type, life_stage_tested, treatment_1_type, study_ID, habitat, habitat_water, land_or_sea, n_unique_temps) %>%
  ungroup
curves_unique1 <- curves_unique %>%
  group_by(study_ID) %>%
  mutate(datasets_per_study = n()) %>%
  ungroup
curvesperstudy <- curves_unique1 %>%
  group_by(study_ID) %>%
  slice(1)
mean_datasets_per_study <- mean(curvesperstudy$datasets_per_study, na.rm = TRUE) #3.87
median_datasets_per_study <- median(curvesperstudy$datasets_per_study, na.rm = TRUE) #2
max <- max(curvesperstudy$datasets_per_study, na.rm = TRUE) #20

## what about habitat dis ##
habitats <- curves_unique %>%
  group_by(latitude, longitude) %>%
  slice(1) %>%
  ungroup

#get count of unique 
curves_unique %>% 
  count(habitat)
curves_unique %>% 
  count(habitat_water)

brackish <- curves_unique %>%
  filter(habitat_water == "brackish") %>%
  select(habitat, habitat_water) %>%
  distinct()


sea <-curves_unique %>%
  filter(land_or_sea == "oceanic")
length(unique(sea$study_ID))
land <-curves_unique %>%
  filter(land_or_sea == "terrestrial")
length(unique(land$study_ID))

#### with regard to treatments and life stages ####

respones <- curves_unique %>%
  count(Trait.Group)

life_stages <- curves_unique %>% 
  count(life_stage_tested)

# 1 adult               104
# 2 embryo               23
# 3 fry                   3
# 4 juvenile            214
# 5 larvae               13
# 6 NA                  100
treatments <- curves_unique %>% 
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
curve_type <- curves_unique %>% #134 acute, 323 batch aclim
  count(curve_type)
curve_type_res <- curves_unique %>% #40 indv, 414 mean, 3 median
  count(response_curve_type)
  
#### figure 2: MAP ####
#### this script is for trying to make a map that includes both marine and freshwater data ####
library(terra)
library(here)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(viridis)
sst_monthly <- rast((here("processed-data", "sst_monthly_summarized.nc")))
freshwater_monthly <- rast((here("processed-data", "freshwater_summarized_masked.nc")))
coastline <- ne_coastline(returnclass = "sf", scale = 110)

names(sst_monthly)
names(freshwater_monthly)
##need to rename monthly values

names_new <- c("mean", "sd", "min", "max", "q2.5", "q97.5")
names(sst_monthly) <- names_new
names(freshwater_monthly) <- names_new

res(sst_monthly)
res(freshwater_monthly)
ext(sst_monthly)
ext(freshwater_monthly)

#resample
target_res <- res(freshwater_monthly)

ext(sst_monthly) <- ext(freshwater_monthly)

sst_fine <- resample(sst_monthly, freshwater_monthly, method = "bilinear")
res(sst_fine)
plot(sst_fine[[1]])
plot(sst_monthly[[1]])
ncell(sst_monthly)
ncell(sst_fine)


df_fresh <- as.data.frame(freshwater_monthly, xy = TRUE, na.rm = TRUE)

df_marine <- as.data.frame(sst_fine, xy = TRUE, na.rm = TRUE)

## load required datasets
datasets <- readRDS(here('processed-data', 'sorted_datasets_withparams.RDS'))



#get lat/long
unique_lat_long <- datasets %>%
  select(latitude, longitude, land_or_sea) %>%
  distinct()
fresh_points <- unique_lat_long %>%
  filter(land_or_sea == "terrestrial")
marine_points <- unique_lat_long %>%
  filter(land_or_sea == "oceanic")
#check where points fall, some estuaries to be dealt with
fresh_vec <- vect(fresh_points, geom = c("longitude", "latitude"), crs = crs(sst_monthly))
marine_vec <- vect(marine_points, geom = c("longitude", "latitude"), crs = crs(sst_monthly))

library(tidyterra)
map <- ggplot() + 
  geom_tile(data = df_fresh, aes(x = x, y = y, fill = mean)) +
  geom_tile(data = df_marine, aes(x = x, y = y, fill = mean)) +
  geom_spatvector(data = fresh_vec,
                  color = "white", fill = "#33A02C", size = 1.5, alpha = .8, shape = 21, stroke = 0.3) +
  geom_spatvector(data = marine_vec,
                  color = "white", fill = "lightblue", size = 1.5, alpha = .8, shape = 21, stroke = 0.3) +
  scale_x_continuous(
    breaks = c(-100, 0, 100),
    labels = c("-100", "0", "100")
  ) +
  scale_y_continuous(
    breaks = c(-50, 0, 50),
    labels = c("-50", "0", "50")
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),  
    axis.line = element_blank(),  
    text = element_text(size = 10)
  ) +
  labs(fill = "Temperature")
map

## save map
ggsave("2all_water_temp_points_map(fig2a).png", plot = map, path = here("figures"), width = 8, height = 5)


#### resposne ####
install.packages("rlang")
install.packages("ggpattern")
library(rlang)
library(ggpattern)

curves_unique <- curves_unique %>%
  mutate(Trait.motivation = factor(Trait.motivation, 
                                   levels = c("autonomic", "voluntary", "positive","negative")))



library(ggplot2)
library(ggpattern)
library(forcats)




# want to order it by how much is most seen
curves_unique <- curves_unique %>%
  mutate(Trait.Group = factor(
    Trait.Group,
    levels = c("Survival","Reproduction", "Energy Acquisition","Locomotion", "Somatic Growth", "Metabolism"))) %>%
  mutate(Trait.motivation = factor(Trait.motivation,
                                        levels = c("autonomic", "voluntary", "positive", "negative")))


a <- ggplot(data = curves_unique, aes(x = Trait.Group, fill = (Trait.motivation))) +
  geom_bar(position = "stack", colour = "black",linewidth = .4) +
  scale_fill_manual(
    values = c(
      "autonomic" = "#1B4965",
      "negative"  = "#56282D",
      "voluntary"  = "#77966D",
      "positive" = "#BDC667"
    ),
    labels = c(
      "Autonomic",
      "Voluntary",
      "Positive",
      "Negative"
    )) +
  xlab(NULL) +
  ylab("Number of datasets") +
  labs(fill = "Trait Motivation") +
  scale_y_continuous(expand = expansion(mult = 0.00),
                     breaks = seq(0,150,25)) +
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

a
ggsave("dataset_tpe_his_temp.pdf", plot = a, path = here("figures"), width = 6, height = 4.5)


















ggplot(curves_unique, aes(Trait.Group, count)) +
  geom_col_pattern(aes(pattern=Trait.motivation, pattern_type=Trait.motivation),
                   colour='black', pattern_key_scale_factor=0.5,
                   pattern_spacing=0.7, pattern_frequency=0.7, pattern_units='cm') +
  theme_bw() +
  labs(title = "Use 'stripe' and 'wave' patterns") +
  theme(legend.key.size = unit(1.5, 'cm')) +
  scale_pattern_manual(values=c('stripe', 'crosshatch', 'wave', 'wave')) +
  scale_pattern_type_manual(values=c(NA, NA, 'triangle', 'sine'))

## species
curves_unique_with_taxa <- curves_unique %>%
  dplyr::select(curve_ID, study_ID, species_ID, response_type, habitat) %>%
  left_join(taxa, join_by(species_ID)) %>%
  group_by(family) %>%
  mutate(datasets_per_family = n()) %>%
  dplyr::select(datasets_per_family, family) %>%
  distinct()
unique(curves_unique_with_taxa$family)
family <- ggplot(
  data = curves_unique_with_taxa,
  aes(
    x = reorder(family, datasets_per_family, FUN = function(x) -x),
    y = datasets_per_family
  )
) + geom_bar(stat = "identity", fill = "darkslategrey", color = "lightgrey", alpha = 0.9, width = .9) +
  coord_flip() +
  xlab("Family") + 
  ylab("Datasets") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),
    text = element_text(size = 10),
    axis.text.y = element_text(size = 7)) +
  scale_y_continuous(expand = c(0, 0))
family
ggsave("histogram_families.pdf", plot = family, path = here("figures"), width = 5, height = 6)

#### combine family, response, and map ####
library(patchwork)
final_plot <- (map / (family + response + plot_layout(widths = c(1.3, 1.3)))) + 
  plot_layout(heights = c(1.2, 1.2)) +
  plot_annotation(tag_levels = "A",
                  theme = theme())
final_plot
ggsave(
  filename = here("figures", "extracted_summary.png"),  # Corrected filename placement
  plot = final_plot, 
  width = 9, 
  height = 10, 
  dpi = 300, 
  device = "png"
)



### histogram of # of test temperatures 
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

#median # of test temps?
median(curves_unique$n_unique_temps) #5
mean(curves_unique$n_unique_temps) #5.22
# % above 4, % above 5
# percentage above 4
mean(curves_unique$n_unique_temps > 4) * 100 #51.9
mean(curves_unique$n_unique_temps == 4) *100 #48.1 %
# percentage above 5
mean(curves_unique$n_unique_temps > 5) * 100 #26.5

## some sort of way to visualize the temperature ranges tested ##

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
  labs(x = "Temperature Range", y = "Study") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


range
ggsave("thermal_range_tested.pdf", plot = range, path = here("figures"), width = 5, height = 8)


#### dumping ####
install.packages("maps")
library(maps)
world_map <- map_data("world")
#### latitude and longitude #### want to make the dots sized by how many datasets in each study
## also could be cool to plot it by the median temperature tested
map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "grey67") +
  geom_point(data = curves_unique %>%
               group_by(latitude, longitude) %>%
               slice(1),
             aes(x = longitude, y = latitude, fill = land_or_sea), color = "black", size = 3, alpha = .55, shape = 21, stroke = 0.3,
             position = position_jitter(width = 0.1, height = 0.1)) + 
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),  
    axis.line = element_blank(),  
    text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),  
    plot.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none") +
  scale_fill_manual(values = c("oceanic" = "#1F78B4",   
                                "terrestrial" = "#33A02C"))
map
ggsave("map_no_env_temp_fig2.pdf", plot = map, path = here("figures"), width = 9.5, height = 5)
