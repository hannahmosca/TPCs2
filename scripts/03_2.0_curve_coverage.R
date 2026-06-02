#### ============================================================
#### Script info
#### ============================================================
# Title: curve_coverage_filtering.R
# Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.
#### ============================================================

#### 1. load packages ####
library(dplyr)
library(tidyverse)
library(here)
library(ggforce)

#read in the data
curves <- read.csv(here('processed-data', 'FishTherm.csv')) %>%
  select(-(X))