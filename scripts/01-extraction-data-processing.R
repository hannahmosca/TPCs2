#### ============================================================
#### Script info
#### ============================================================
# Title: extraction-data-processing.R
# Author: Hannah Mosca
# Description:
#   Cleans and processes raw thermal performance data extracted
#   from the literature. Filters for wild fish only, standardizes
#   variables, categorizes trait types, generates unique curve IDs,
#   and outputs cleaned datasets ready for TPC fitting.
#### ============================================================

rm(list=ls())
#### 1. load packages ####
library(tidyverse)
library(here)
library(stringr)
library(dplyr)

#### 2. load most up to date extraction datasheet ####
filename <- "extracted_data.csv"
raw_data <- read.csv(here("raw-data", filename))

#### 3. initial data cleaning ####

## clean rows, NAs, etc. ##
data <- raw_data %>%
  filter(if_any(everything(), ~ . != "")) %>% #remove empty rows
  filter(origin == "wild") %>%  #keep only wild-origin data 
  mutate(
    across(where(is.character), ~ na_if(.x, "n/a")),
    across(where(is.character), ~ na_if(.x, ""))
  )
## keep rows with at least one valid response metric
data <- data %>%
  filter(if_any(
    c(response_mean, response_ind, response_mode,
      response_median, min_response, max_response),
    ~ !is.na(.)
  ))
rm(raw_data) #save some space in env

## clean curve_type entries ## 
data <- data %>%
  mutate(
    curve_type = case_when(
      curve_type %in% c("accute-exposure", "acute", "acute-exposure") ~ "acute-change",
      curve_type %in% c("batch-aclim", "batch-acclimated") ~ "batch-acclim",
      TRUE ~ curve_type))
         
## tidy raw response_type synonyms ##
data <- data %>%
  mutate(
    response_type = case_when(
      str_detect(response_type, "weight-sgr|SGR-weight") ~ "specific-growth-rate-mass",
      response_type == "max-metabolism" ~ "maximum-metabolic-rate",
      response_type %in% c("maxium-heart-rate", "maxium heart-rate", "maximum heart-rate") ~ "maximum-heart-rate",
      response_type == "whole-oxygen-embryo-consumption" ~ "whole-embyro-oxygen-consumption",
      response_type == "hatching-rate" ~ "hatch-rate",
      response_type == "feeding rate" ~ "feeding-rate",
      TRUE ~ response_type))

#### 4. categorize responses into response/trait groups ####
## remove non-performance traits
data <- data %>%
  filter(!response_type %in% c(
    "total-length", "standard-length", "CTmax", "CTmin",
    "distance-moved", "critical-oxygen-concentration",
    "total-time-following-females", "total-food-consumed"
  ))

#### 6. generate curve IDs for mean response curves ####

#filtering out mean response tpcs
mean_tpcs <- data %>%
  filter(!is.na(response_mean)) %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group, collection_site) %>%
  dplyr::select(study_ID, species_ID, cohort_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group,
                acclim_temp, test_temp, response_ind, response_mean, response_median, min_response, max_response, everything()) %>%
  mutate(curve_ID = ifelse(length(unique(cohort_ID)) == n(),
                           as.character(cur_group_id()),
                           paste(cur_group_id(), cohort_ID, sep = "_"))) %>%
  mutate(response_curve_type = "mean") %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  dplyr::select(-curve_ID) %>%
  rename(curve_ID = id) %>%
  dplyr::select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  mutate(across(c(response_mean, test_temp), as.numeric))

length(unique(mean_tpcs$curve_ID)) # 415


#### 7. generate curve IDs for individual response curves ####
ind_tpcs <- data %>%
  filter(response_ind != "n/a")%>%
  filter(!is.na(response_ind))
#get where curve_id left off in means
start_id <- max(mean_tpcs$curve_ID, na.rm = TRUE) + 1
#assign individual curve_IDs starting from the next available number
ind_tpcs <- ind_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group, collection_site) %>%
  dplyr::select(study_ID, species_ID, cohort_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group,
                acclim_temp, test_temp, response_ind, response_mean, response_median, min_response, max_response, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id - 1)) %>%
  mutate(response_curve_type = "individual") %>%
  dplyr::select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(response_mean, test_temp), as.numeric))

length(unique(ind_tpcs$curve_ID)) #40 


#### 8. generate curve IDs for other sample response curves ####
#filter datasets that report a median value
median_tpcs <- data %>%
  filter(!is.na(response_median)) %>%
  filter(!(response_median == "")) %>%
  filter(is.na(response_mean)) %>%
  filter(is.na(response_ind))
start_id2 <- max(ind_tpcs$curve_ID, na.rm = TRUE) + 1 #new start_ID
# curve_IDs for median tpcs
median_tpcs <- median_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group, collection_site) %>%
  dplyr::select(study_ID, species_ID, cohort_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group,
                acclim_temp, test_temp, response_ind, response_mean, response_median, min_response, max_response, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id2 - 1)) %>%
  mutate(response_curve_type = "median") %>%
  dplyr::select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(response_median, test_temp), as.numeric))
length(unique(median_tpcs$curve_ID)) #3

#filter curves that report min and max value
min_max_tpcs <- data %>%
  filter(!is.na(max_response)) %>%
  filter(!is.na(min_response)) %>%
  filter(is.na(response_mean)) %>%
  filter(!(min_response == "")) %>%
  filter(!(max_response == "")) %>%
  filter(is.na(response_ind))
start_id3 <- max(median_tpcs$curve_ID, na.rm = TRUE) + 1 #new start_id
#curve_IDs for min and max tpcs
min_max_tpcs <- min_max_tpcs %>%
  group_by(study_ID, species_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group, collection_site) %>%
  dplyr::select(study_ID, species_ID, cohort_ID, curve_type, response_type, response_unit, sex, treatment_1_group, treatment_2_group,
                acclim_temp, test_temp, response_ind, response_mean, response_median, min_response, max_response, everything()) %>%
  mutate(curve_ID = as.numeric(cur_group_id() + start_id3 - 1)) %>%
  mutate(response_curve_type = "min-max") %>%
  dplyr::select(cohort_ID, curve_ID, response_curve_type, everything()) %>%
  ungroup() %>%
  group_by(curve_ID) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  mutate(across(c(min_response, max_response, test_temp), as.numeric))

length(unique(min_max_tpcs$curve_ID)) #1

####9. combine dataframes back together and save ####
curves <- rbind(mean_tpcs, ind_tpcs, median_tpcs) #not adding min/max response dataset 

### make sure all datasets have >3 temps 1 deg. apart
curves <- curves %>%
  group_by(curve_ID) %>%
  mutate(
    # sort unique temps for each curve
    sorted_temps = list(sort(unique(test_temp))),
    
    # count how many temps are at least 1 deg apart
    n_unique_temps = map_int(sorted_temps, function(temps) {
      distinct <- temps[1]
      for (t in temps[-1]) {
        if (min(abs(t - distinct)) >= 1) {
          distinct <- c(distinct, t)
        }
      }
      length(distinct)
    })
  ) %>%
  ungroup() %>%
  select(-sorted_temps) %>%
  select(n_unique_temps, curve_ID, test_temp, everything()) %>%
  filter(n_unique_temps > 3)
length(unique(curves$curve_ID)) #457 

#make a response_value category 
#make sure all are numeric
curves <- curves %>%
  mutate(response_ind = as.numeric(response_ind)) %>%
  mutate(response_median = as.numeric(response_median)) %>%
  mutate(response_mean = as.numeric(response_mean))

#make response_value column
curves <- curves %>%
  mutate(response_value = case_when(
    response_curve_type == "mean" ~ response_mean,
    response_curve_type == "individual" ~ response_ind,
    response_curve_type == "median" ~ response_median
  )) %>%
  select(curve_ID, study_ID, species_ID, curve_type, response_type, test_temp, response_value, response_curve_type, everything())

## un-log transform responses that are logged
##need to check these curves to make sure i dont need to refit.
curves <- curves %>%
  mutate(response_value = ifelse(response_type %in% c("log-SMR", "log-active-metabolic-rate", "log-metabolic-scope"),
                                 10^response_mean, response_value))
#transform the natural logged one
curves <- curves %>%
  mutate(response_value = ifelse(response_type == "ln-whole-oxygen-embryo-consumption",
                                 exp(response_mean), response_value))

curves <- curves %>%
  mutate(response_value = ifelse(response_type == "development-rate",
                                 1/response_ind, response_value)) %>%
  mutate(response_unit = ifelse(response_type == "development-rate", "1/days", response_unit))

curves <- curves %>%
  select(curve_ID, response_type, response_value, response_mean, response_ind, response_unit, everything())


response_new_names <- read.csv(here("raw-data", "raw_response_categories.csv"))
response_ontology <- read.csv(here("raw-data", "raw_response_ontology.csv")) %>%
  rename(given_trait_name = Trait.Name) %>%
  filter(if_any(everything(), ~ . != "")) %>%
  select(Trait.Group, given_trait_name, Trait.Unit, Trait.Definition, Trait.motivation, organization)

curves <- curves %>%
  left_join(response_new_names %>% select(new.name, curve_ID, response_type),join_by(curve_ID, response_type))

curves <- curves %>%
  select(curve_ID, response_type, new.name, everything()) %>%
  rename(given_trait_name = new.name)

curves_new <- curves %>%
  left_join(response_ontology %>% select(Trait.Group, given_trait_name, Trait.motivation, organization), join_by(given_trait_name)) %>%
  select(curve_ID, response_type, given_trait_name, Trait.Group, Trait.motivation, organization, everything())

curves_new <- curves_new %>%
 mutate(Trait.Group = case_when(
    Trait.Group == "metabolism"      ~ "Metabolism",
    Trait.Group == "reproduction"    ~ "Reproduction",
    Trait.Group == "somatic growth"  ~ "Somatic Growth",
    Trait.Group == "survival"        ~ "Survival",
    Trait.Group == "Energy aquisition"~ "Energy Aquisition",
    TRUE                             ~ Trait.Group
  ))
####handle survival and mortality curves in this script #### 
curves_new <- curves_new %>%
  mutate(
    # convert to survival only for mortality curves
    response_value = if_else(
      response_type %in% c("percent-mortality", "mortality"),
      100 - response_value,   
      response_value          
    ),
    response_type = if_else(
      response_type %in% c("percent-mortality", "mortality"),
      "percent-survival",
      response_type
    ))

####handle predation time curves #### 

curves_new <- curves_new %>%
  mutate(
    response_value = if_else(
      response_type %in% c("handling-time", "capture-manuever-time"),
      1/response_value,   
      response_value          
    ))
#updating units

curves_new <- curves_new %>%
  mutate(
    response_unit = if_else(
      response_type == "handling-time", "(s/prey)^1",   
      response_unit          
    ))
curves_new <- curves_new %>%
  mutate(
    response_unit = if_else(
      response_type == "capture-manuever-time", "1/s",   
      response_unit          
    ))


#### 5. clean and classify habitats ####
# tidying habitat information
curves_new <- curves_new %>%
  mutate(habitat = if_else(habitat == "coral reef", "reef", habitat)) %>%
  mutate(habitat = if_else(habitat == "Marine", "marine", habitat)) %>%
  mutate(habitat = if_else(habitat == "mixed", "brackish", habitat)) %>%
  mutate(habitat = if_else(habitat == "n/a", NA, habitat)) %>%
  mutate(habitat = if_else(habitat == "", NA, habitat)) %>%
  mutate(habitat = if_else(habitat == "sea", "marine", habitat)) %>%
  mutate(habitat = if_else(habitat == "ocean", "marine", habitat)) %>%
  mutate(habitat_water = case_when(
    habitat %in% c("sound", "marine rockpools", "bay", "marine", "coastal","marine estuary", "intertidal salt marshes", "gulf", "fjord", "reef", "intertidal", "harbour", "marine shelf") ~ "marine",
    habitat %in% c("river", "lake", "swamp", "creek", "pond", "stream", "streams", "reservoir", "freshwater cove") ~ "freshwater",
    habitat %in% c("wetlands", "lagoon", "estuary", "mangrove creek", "brackish") ~ "brackish",
    TRUE ~ NA  # for the NA ones, should get this information from species later on 
  ))

curves_new <- curves_new %>%
  mutate(land_or_sea = case_when(
    habitat %in% c("ocean", "sound", "marine rockpools", "bay", "sea","marine", "coastal","marine estuary", "intertidal salt marshes","gulf", "fjord", "reef", "intertidal", "harbour", "marine shelf","coastal", "estuary") ~ "oceanic",
    habitat %in% c("river", "lake", "swamp", "creek", "pond", "stream", "streams", "wetlands", "lagoon", "mangrove creek", "reservoir", "freshwater cove") ~ "terrestrial",
    TRUE ~ NA
  ))

####10. make sure lat/long is numeric ####
curves_new <- curves_new %>%
  mutate(across(c(response_value, latitude, longitude), as.numeric)) %>%
  mutate(abs_latitude = abs(latitude))

length(unique(curves_new$study_ID)) #118
length(unique(curves_new$species_ID)) #107

#### cleaning up other characteristics ####
#life stage tested
curves_new <- curves_new %>%
  mutate(life_stage_tested = ifelse(life_stage_tested == "juvenile ", "juvenile", life_stage_tested)) %>%
  mutate(life_stage_tested = ifelse(life_stage_tested %in% c("immature", "fingerling", "yearling"), "juvenile", life_stage_tested)) %>%
  mutate(life_stage_tested = ifelse(life_stage_tested == "mature", "adult", life_stage_tested)) %>%
  mutate(life_stage_tested = ifelse(life_stage_tested %in% c("embyro", "egg"), "embryo", life_stage_tested)) %>%
  mutate(life_stage_tested = ifelse(life_stage_tested == "larval", "larvae", life_stage_tested))
#life stage manipulated
curves_new <- curves_new %>%
  mutate(life_stage_manip = ifelse(life_stage_manip %in% c("immature", "fingerling", "yearling"), "juvenile", life_stage_tested)) %>%
  mutate(life_stage_manip = ifelse(life_stage_manip == "mature", "adult", life_stage_tested)) %>%
  mutate(life_stage_manip = ifelse(life_stage_manip == "egg", "embryo", life_stage_tested)) %>%
  mutate(life_stage_manip = ifelse(life_stage_manip == "larval", "larvae", life_stage_tested))
#treatments
curves_new <- curves_new %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("PCO2", "co2"), "CO₂/pH", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("acclimation", "acclimation-temp", "acclimation_to_seasonal_conditions", "incubation_temp"), "Acclimation", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("ration", "prey-density", "ration offered ", "ration offered", "food ration", "food-type", "food", "week-of-refeeding-after-3-weeks-starvation", "satiation"), "Ration", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("size", "size class", "body-size", "age", "initial-mean-weight"), "Size", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("salinity", "conductivity"), "Salinity", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type == "oxygen-level", "Oxygen", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type %in% c("lumination", "time-of-day"), "Photoperiod", treatment_1_type)) %>%
  mutate(treatment_1_type = ifelse(treatment_1_type == "oxygen-level", "Oxygen", treatment_1_type))

curves_new <- curves_new %>%
  mutate(treatment_2_type = ifelse(treatment_2_type == "mass", "Size", treatment_2_type)) %>%
  mutate(treatment_2_type = ifelse(treatment_2_type == "satiation", "Ration", treatment_2_type))


length(unique(curves_new$curve_ID)) #457 unique curve_IDs

check <- curves_new %>%
  select(curve_ID, Trait.motivation, Trait.Group, given_trait_name, response_type, response_unit) %>%
  distinct()


saveRDS(curves_new, file = here("processed-data", "wild-tpcs-clean.RdS"))

