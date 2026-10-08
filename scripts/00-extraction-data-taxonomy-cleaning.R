#### script info #### 
# title: TPC-taxonomy.R
# author: Hannah Mosca
# description: This script loads species data, filters it to match the TPC dataset, and gets taxonomic classifications using ITIS.
rm(list=ls())
#### 1. load packages ####
library(tidyverse)
library(here)
library(dplyr)
library(stringr)

# installing taxize from GitHub 
install.packages("remotes")
remotes::install_github("ropensci/bold")
remotes::install_github("ropensci/taxize")
library(taxize)

#### 2. load most up-to-date extracted species_ID sheet ####
filename <- "data_extraction_species_ID_16_12_2025.csv"
# load species data and remove empty entries
species <- read.csv(here("raw-data", filename)) %>%
  filter(species != "")
##filter species to only those we have temp datasets on
curves <- readRDS(here("processed-data", "wild-tpcs-clean.RdS"))
species_IDs <- unique(curves$species_ID)
species_filtered <- species %>%
  filter(species_ID %in% species_IDs)


# create a new column with full species name
species_filtered <- species_filtered %>%
  mutate(species_name = paste(genus, species))
species_filtered <- species_filtered %>%
  mutate(species_name = case_when(
    species_name == "Austrolebias wolterstorff" ~ "Megalebias wolterstorffi",
    species_name == "gambusia holbrooki" ~ "Gambusia affinis",
    species_name == "Channa striatus" ~ "Channa striata",
    species_name == "Salvelinus  alpinus" ~ "Salvelinus alpinus",
    species_name == "Onychostoma barbatulum" ~ "Onychostoma barbatula",
    species_name == "Zoarces vivparus" ~ "Zoarces viviparus",
    species_name == "Zoramia leptacantha" ~ "Zoramia leptacanthus",
    species_name == "Centropristis  striata" ~ "Centropristis striata",
    species_name == "Chromis  atripectoralis" ~ "Chromis atripectoralis",
    species_name == "Fundulus heteroclitus" ~ "Fundulus heteroclitus heteroclitus",
    species_name == "fundulus heteroclitus" ~ "Fundulus heteroclitus macrolepidotus",
    TRUE ~ species_name   
  )) %>%
  mutate(species_name = if_else(species_ID == "SP53", "Oncorhynchus clarkii pleuriticus", species_name),
         species_name = if_else(species_ID == "SP79", "Oncorhynchus clarkii stomias", species_name))

# extract unique species names
species_name <- unique(species_filtered$species_name) #one that is duplicated, fundilitis heterolitus, but one of them is a sub-species

# get taxonomic classification for each species using ITIS
df1 <- classification(species_name, db = 'itis')
#101 found, 6 not found, 107 total

# make and rotate dataframe 
taxonomy <- map_dfr(.x = df1, ~ data.frame(.x), .id = 'species_name') %>%
  pivot_wider(id_cols = species_name, names_from = rank, values_from = c(name, id)) %>%
  rename_with(~ str_replace(.x, 'name_', '')) %>%
  rename_with(~ str_replace(.x, 'id_', 'wormsid_')) %>%
  janitor::clean_names()
taxonomy1 <- taxonomy %>%
  left_join(species_filtered %>% select(species_ID, species_name, subspecies), join_by(species_name)) %>%
  select(species_ID, kingdom, phylum, superclass, class, superorder, order, suborder, family, subfamily, genus, wormsid_genus, wormsid_genus, species_name, wormsid_species, subspecies.y) %>%
  rename(subspecies = subspecies.y)
write.csv(taxonomy1, here('processed-data', 'taxonomy1.csv')) 
##added 6 missing species info/ sub species info  manually