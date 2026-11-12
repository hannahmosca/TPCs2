# FishTPC

## Introduction
This repository contains the data and code used to produce the analyses and figures presented in "Thermal performance metrics of fish vary across thermal environments and levels of biological organization." The study introduces FishTherm, a compilation database of thermal performance curves for wild fishes, and examines how key thermal parameters—including thermal optima, performance breadth, tolerance breadth, and activation energy—vary across latitude, habitat, response type, and levels of biological organization.
All scripts are organized to allow the analyses in the manuscript to be reproduced. Running the scripts in order should recreate the processed datasets, model outputs, and figures presented in the paper.

## Data 
FishTPC Data for public use
 - [FishTherm](https://github.com/hannahmosca/TPCs2/blob/main/processed-data/FishTPC.csv)
 
A minimum dataset needed to reproduce all main analyses and large files that cannot be hosted on Github can be downloaded from these links here.
  
#### SST data files used in analyses can be downloaded here:
 - [SST and SST Anomaly, NOAA Global Coral Bleaching Monitoring](https://catalog.data.gov/dataset/sst-and-sst-anomaly-noaa-global-coral-bleaching-monitoring-5km-v-3-1-monthly-1985-present-lon03) - SST 5km, V.3.1, Monthly, 1985-Present, Lon0360

#### Freshwater Temperature data files can be downloaded here: 
 - [Futurestreams dataset](https://public.yoda.uu.nl/geo/UU01/T7TVTQ.html) 
 - Files used: "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc", "waterTemp_weekAvg_output_E2O_hist_1986-01-07_to_1995-12-30.nc", "waterTemp_weekAvg_output_E2O_hist_1996-01-07_to_2005-12-30.nc", "waterTemp_weekAvg_output_hadgem_rcp4p5_2006-01-07_to_2019-12-30.nc", "waterTemp_weekAvg_output_hadgem_rcp4p5_2020-01-07_to_2029-12-30.nc"


## Code
The following are all of the R scripts contained in this repository and a short description of what each accomplishes:
#### 00-extraction-data-processing.R
 - Optional script: cleans and processes raw thermal performance data extracted from the literature. Filters for wild fish only, standardizes variables, categorizes trait types, generates unique curve IDs, and outputs cleaned database called FishTherm. All scripts after use FishTherm.csv, therefore this is script is optional to run but here for transparency
#### 00-prep-fishtpc.R
 - Optional script: cleans Fishtherm output to make FishTPC dataset for public use
#### 00-extraction-data-taxonomy-cleaning.R
 - Optional script: cleans and processes raw species identificaiton and taxonomy data. Outputs clean file called Taxonomy.csv for use
#### 01-TPC-database-characteristics.R
 - Summarize FishTherm dataset characteristic
#### 02-TPCrtpc-fitting.R 
 - Fits all datasets with all three and four parameter models in the [rtpc package](https://github.com/padpadpadpad/rTPC)
#### 03_curve_coverage_filtering.R
 - Classifies each FishTherm curve by curve coverage (e.g., full curve, T-min only, T-max only, T-opt only, bounded-with-optimum, unbounded) using scaled responses and simple shape/boundedness rules, along with visualizingm then writes curve-type labels and coverage flags back to a processed dataset.
#### 04-selecting-valid-models-and-deriving-params.
 -  Filters candidate thermal performance curve (TPC) models to retain biologically reasonable fits and selects the top-ranked model(s) per curve. Outputs a dataset of tpc parameters
#### 05-fit-arrhenius-activation-energy.R
 -  Estimates Arrhenius activation energy (Ea) for FishTherm curves by fitting an Arrhenius relationship to the increasing-temperature portion of each dataset (temperatures ≤ fitted topt)
#### 06-plotting-params-and-curve-types.R
 -  Makes plot in figure 1 panal B
#### 07-extracting-SST.R
 - Builds a monthly sea-surface temperature (SST) raster time series from the NOAA monthly mean NetCDF (sst.mon.mean.nc), subsets the record to match the FishTherm study window (1982-01 to 2025-09), replaces fill values with NA, and writes a processed NetCDF. Then computes long-term SST summary rasters (mean, sd, min, max, 2.5% and 97.5% quantiles) across all months in the window, maps sampling locations, and extracts summary statistics at unique marine study coordinates.
#### 08-extracting-future-streams-temp.R
 -  Processes weekly freshwater stream temperature rasters (FutureStreams) into monthly mean temperatures for the FishTherm study window (1982-01 to 2025-09). Computes long-term freshwater summary rasters (mean, sd, min, max, 2.5% and 97.5% quantiles) across all months in the window, maps sampling locations, and extracts summary statistics at unique freshwater study coordinates.
#### 09-enviornmentaltemp-and-performance.R
- hypothesis-testing / model-fitting script that joins fitted TPC parameters to environmental temperature summaries, collapses curves to reduce pseudoreplication, then runs a set of mixed-effects regressions on thermal performance parameters 
#### 010-activation-energy-and-performance-type.R
 - Analyzes fitted activation energy estimates (Ev) across response contexts in FishTherm
#### 11-topt_resid_and_performance_type.R
 - Analyzes residual variation in topt across response contexts in FishTherm
#### 12-bicycleplot_top_down_view_of_tpcs.R
 - Produces top down views of tpcs. Figure 1 panal B and C

