# FishTherm

## Introduction
This repository contains the data and code used to produce the analyses and figures presented in "Thermal performance in fishes varies systematically across latitude, habitat, and biological organization." The study introduces FishTherm, a compilation of thermal performance curves for wild fishes, and examines how key thermal parameters—including thermal optima, performance breadth, tolerance breadth, and activation energy—vary across latitude, habitat, response type, and levels of biological organization.
All scripts are organized to allow the analyses and figures in the manuscript to be reproduced. Running the scripts in order should recreate the processed datasets, model outputs, and figures presented in the paper.

## Data 
FishTherm Database
 - [FishTherm](https://github.com/hannahmosca/TPCs2/blob/main/processed-data/FishTherm.csv)
 
A minimum dataset needed to reproduce all main analyses and large files that cannot be hosted on Github can be downloaded from these links here.
  
SST data files used in analyses can be downloaded here:
 - [NOAA OI SST V2 High Resolution Dataset](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html) - Mean Daily Sea Surface Temperature
Freshwater Temperature data files can be downloaded here: 
 - [Futurestreams dataset](https://public.yoda.uu.nl/geo/UU01/T7TVTQ.html) Files used: "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc", "waterTemp_weekAvg_output_E2O_hist_1986-01-07_to_1995-12-30.nc", "waterTemp_weekAvg_output_E2O_hist_1996-01-07_to_2005-12-30.nc", "waterTemp_weekAvg_output_hadgem_rcp4p5_2006-01-07_to_2019-12-30.nc", "waterTemp_weekAvg_output_hadgem_rcp4p5_2020-01-07_to_2029-12-30.nc"


## Code
The following are all of the R scripts contained in this repository and a short description of what each accomplishes:
#### 00-extraction-data-processing.R 
 - Cleans and processes raw thermal performance data extracted from the literature. Filters for wild fish only, standardizes variables, categorizes trait types, generates unique curve IDs, and outputs cleaned database of tpcs ready to be fit
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
#### [07_quantifying-niche-filling.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/07_quantifying-niche-filling.R)
- measures potential thermal niche filling in thermal space
#### [08_quantifying-range-filling.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/08_quantifying-range-filling.R)
- measures potential thermal range filling in geographic space
#### [09_model-selection_niche.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/09_model-selection_niche.R)
- fits models to warm and cool niche filling and plots predictions
#### [10_model-selection_range.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/10_model-selection_range.R)
- fits models to range filling and asymmetry in underfilling and plots predictions
#### [11_sensitivity_model-selection_niche.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/11_sensitivity_model-selection_niche.R)
- checks sensitivity of warm and cool niche filling results to behaviour and acclimatisation
#### [12_sensitivity_model-selection_range.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/12_sensitivity_model-selection_range.R)
- checks sensitivity of range filling and asymmetry in underfilling results to acclimatisation
#### [13_sensitivity_model-selection_niche_no-dormancy.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/13_sensitivity_model-selection_niche_no-dormancy.R)
- checks sensitivity of warma and cool niche filling results to exclusion of dormant species 
#### [14_sensitivity_model-selection_range_no-dormancy.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/14_sensitivity_model-selection_range_no-dormancy.R)
- checks sensitivity of range filling and asymmetry in underfilling results to exclusion of dormant species 
#### [15_sensitivity_range-source.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/15_sensitivity_range-source.R)
- checks sensitivity of warm and cool niche filling results to realized range source 
#### [16_niche-filling-figures.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/16_niche-filling-figures.R)
- creates warm and cool niche filling figures
#### [17_range-filling-figures.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/17_range-filling-figures.R)
- creates range filling figures
#### [18_sensitivity_NicheMapR.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/18_sensitivity_NicheMapR.R)
- tests sensitivity of results on land to parameters used in operative temperature models
#### [19_phylo-gls-sensitivity-analysis.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/19_phylo-gls-sensitivity-analysis.R)
- tests sensitivity of results to choice of method used to control for evolutionary relatedness
#### [20_create-minimum-dataset.R](https://github.com/nicole-a-moore/living-up-to-thermal-potentials/blob/main/R/20_create-minimum-dataset.R)
- creates a minimum dataset needed to reproduce main analyses
  
## Ownership
This reposity is owned by Nikki A. Moore and Jennifer M. Sunday. All data, scripts, and figures may be used by others with proper acknowledgement.