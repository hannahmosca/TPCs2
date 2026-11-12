## transforms FishTPC database from version for our analysis to version for public use
## - renames columns
## - drops unnecessary columns
## - joins relational database 
## - creates metadata file
library(tidyverse)

## read in clean version of database 
fishtherm = read.csv("processed-data/FishTPC/FishTherm.csv")

## replace all instances of " n/a" and "nn/a"
fishtherm[fishtherm == "nn/a"] <- NA
fishtherm[fishtherm == " n/a"] <- NA

## get rid of rowname column 
fishtherm = select(fishtherm, -X)

## ------------------------
## clean mass/length columns
## ------------------------
## clean up body lengths by gathering measurement types all into one column 
## body_length
## body_length_statistic (options: "mean", "individual measurement", "min-max", "mean (min-max)"
## body_length_unit
## body length_error
## body_length_error_units
## body_length_metric

## make sure no curves have both mean AND individual body length measures 
have_both = which(!is.na(fishtherm$mean_body_length) & !is.na(fishtherm$individual_body_length))

#view(fishtherm[have_both,])
## these are calculated from individual measurements - just keep individual measurements 

fishtherm$mean_body_length[have_both] = NA
fishtherm$mean_body_length_error[have_both] = NA
fishtherm$mean_body_length_error_units[have_both] = NA

## see if any have mean AND min/max body length measures  
have_both_min = which(!is.na(fishtherm$mean_body_length) & !is.na(fishtherm$min_body_length))
have_both_max = which(!is.na(fishtherm$mean_body_length) & !is.na(fishtherm$max_body_length))
## yes

test_therm = fishtherm %>% 
  mutate(body_length_statistic = case_when(!is.na(mean_body_length) 
                                           & !is.na(min_body_length) 
                                           & !is.na(max_body_length) ~ "mean (min-max)",
                                           is.na(mean_body_length) 
                                           & !is.na(min_body_length) 
                                           & !is.na(max_body_length) ~ "min-max",
                                           !is.na(mean_body_length) ~ "mean",
                                           is.na(mean_body_length) & !is.na(individual_body_length) ~ "individual measurement", 
                                           TRUE ~ NA),
         body_length = case_when(!is.na(mean_body_length) 
                                   & !is.na(min_body_length) 
                                   & !is.na(max_body_length) ~ 
                                     paste0(mean_body_length, " (", min_body_length, "-", max_body_length, ")"),
                                   is.na(mean_body_length) 
                                   & !is.na(min_body_length) 
                                   & !is.na(max_body_length) ~ paste0(min_body_length, "-", max_body_length),
                                   !is.na(mean_body_length) ~ paste0(mean_body_length),
                                   is.na(mean_body_length) & !is.na(individual_body_length) ~ paste0(individual_body_length), 
                                   TRUE ~ NA),
         body_length_error = case_when(!is.na(mean_body_length) ~ mean_body_length_error,
                                       TRUE ~ NA),
         body_length_error_unit = case_when(!is.na(mean_body_length_error_units) ~ mean_body_length_error_units,
                                       TRUE ~ NA),
         body_length_metric = metric_body_length,
         body_length_unit = body_length_units) %>%
  select(-mean_body_length, 
         -individual_body_length,
         -mean_body_length_error, 
         -mean_body_length_error_units, 
         -min_body_length, 
         -max_body_length, 
         -metric_body_length,
         -body_length_units)

## supplement with initial mean body length when no other measurement is available 
test_therm = test_therm %>% 
  mutate(body_length_statistic = case_when(is.na(body_length) & 
                                             !is.na(initial_mean_body_length) ~ "initial mean", 
                                           TRUE ~ body_length_statistic),
         body_length_unit = case_when(is.na(body_length) & 
                                             !is.na(initial_mean_body_length) ~ initial_mean_body_length_unit, 
                                           TRUE ~ body_length_unit),
         body_length_error = case_when(is.na(body_length) & 
                                        !is.na(initial_mean_body_length) ~ as.character(initial_mean_body_length_error), 
                                      TRUE ~ as.character(body_length_error)),
         body_length_error_unit = case_when(is.na(body_length) & 
                                         !is.na(initial_mean_body_length) ~ initial_mean_body_length_error_unit, 
                                       TRUE ~ body_length_error_unit),
         body_length = case_when(is.na(body_length) & 
                                   !is.na(initial_mean_body_length) ~ as.character(initial_mean_body_length), 
                                 TRUE ~ body_length)) %>%
  select(-initial_mean_body_length, 
         -initial_mean_body_length_unit,
         -initial_mean_body_length_error, 
         -initial_mean_body_length_error_unit) 


## clean units and metric columns 
unique(test_therm$body_length_metric)
test_therm = test_therm %>%
  mutate(body_length_metric = case_when(body_length_metric %in% c("standard-length","SL") ~ "standard length", 
                                        body_length_metric %in% c("total_length","TL", "total length", 
                                                                  "total-length", "total length ") ~ "total length", 
                                        body_length_metric %in% c("FL", "fork-length") ~ "fork length",
                                        body_length_metric %in% c("body-length") ~ NA,
                                        TRUE ~ body_length_metric))
unique(test_therm$body_length_units)
unique(test_therm$body_length_error_unit)
test_therm = test_therm %>%
  mutate(body_length_error_unit = case_when(body_length_error_unit %in% c("SEM", "sem", "SE") ~ "SE", 
                                        TRUE ~ body_length_error_unit))

## do the same but for mass
## body_mass
## body_mass_statistic (options: "mean", "individual measurement", "min-max", "mean (min-max)"
## body_mass_unit
## body_mass_error
## body_mass_error_units
## body_mass_metric

## make sure no curves have both mean AND individual body mass measures 
have_both = which(!is.na(fishtherm$mean_body_mass) & !is.na(fishtherm$individual_body_mass))

#view(fishtherm[have_both,])
## these are calculated from individual measurements - just keep individual measurements 

fishtherm$mean_body_mass[have_both] = NA
fishtherm$mean_body_mass_error[have_both] = NA
fishtherm$mean_body_mass_error_units[have_both] = NA

## see if any have mean AND min/max body length measures  
have_both_min = which(!is.na(fishtherm$mean_body_mass) & !is.na(fishtherm$body_mass_min))
have_both_max = which(!is.na(fishtherm$mean_body_mass) & !is.na(fishtherm$body_mass_max))
## yes

have_one = which(!is.na(fishtherm$body_mass_max) & is.na(fishtherm$body_mass_min))
have_one = which(is.na(fishtherm$body_mass_max) & !is.na(fishtherm$body_mass_min))

unique(fishtherm$mean_body_mass_upper_error)
unique(fishtherm$mean_body_mass_lower_error)
## empty - get rid of these

test_therm = test_therm %>% 
  mutate(body_mass_statistic = case_when(!is.na(mean_body_mass) 
                                           & !is.na(body_mass_min) 
                                           & !is.na(body_mass_max) ~ "mean (min-max)",
                                           is.na(mean_body_mass) 
                                           & !is.na(body_mass_min) 
                                           & !is.na(body_mass_max) ~ "min-max",
                                         
                                           !is.na(mean_body_mass) ~ "mean",
                                           !is.na(individual_body_mass) ~ "individual measurement", 
                                           TRUE ~ NA),
         body_mass = case_when(!is.na(mean_body_mass) 
                                 & !is.na(body_mass_min) 
                                 & !is.na(body_mass_max) ~ 
                                   paste0(mean_body_mass, " (", body_mass_min, "-", body_mass_max, ")"),
                                 is.na(mean_body_mass) 
                                 & !is.na(body_mass_min) 
                                 & !is.na(body_mass_max) ~ paste0(body_mass_min, "-", body_mass_max),
                                 !is.na(mean_body_mass) ~ paste0(mean_body_mass),
                                 !is.na(individual_body_mass) ~ paste0(individual_body_mass), 
                                 TRUE ~ NA),
         body_mass_error = case_when(!is.na(mean_body_mass_error) ~ mean_body_mass_error,
                                     TRUE ~ NA),
         body_mass_error_unit = case_when(!is.na(mean_body_mass_error_units) ~ mean_body_mass_error_units,
                                            TRUE ~ NA),
         body_mass_metric = body_mass_type,
         body_mass_unit = body_mass_units) %>%
  select(-individual_body_mass,
         -mean_body_mass, 
         -body_mass_units, 
         -mean_body_mass_error, 
         -mean_body_mass_upper_error, 
         -mean_body_mass_lower_error,
         -mean_body_mass_error_units, 
         -body_mass_min, 
         -body_mass_max, 
         -body_mass_type) 

## supplement with initial mean body length when no other measurement is available 
test_therm = test_therm %>% 
  mutate(body_mass_statistic = case_when(is.na(body_mass) & 
                                             !is.na(initial_mean_body_mass) ~ "initial mean", 
                                           TRUE ~ body_mass_statistic),
         body_mass_unit = case_when(is.na(body_mass) & 
                                        !is.na(initial_mean_body_mass) ~ initial_mean_body_mass_unit, 
                                      TRUE ~ body_mass_unit),
         body_mass_error = case_when(is.na(body_mass) & 
                                         !is.na(initial_mean_body_mass) ~ as.character(initial_mean_body_mass_error), 
                                       TRUE ~ as.character(body_mass_error)),
         body_mass_error_unit = case_when(is.na(body_mass) & 
                                              !is.na(initial_mean_body_mass) ~ initial_mean_body_mass_error_unit, 
                                            TRUE ~ body_mass_error_unit),
         body_mass = case_when(is.na(body_mass) & 
                                   !is.na(initial_mean_body_mass) ~ as.character(initial_mean_body_mass), 
                                 TRUE ~ body_mass)) %>%
  select(-initial_mean_body_mass, 
         -initial_mean_body_mass_unit,
         -initial_mean_body_mass_error, 
         -initial_mean_body_mass_error_unit) 

colnames(fishtherm)
colnames(test_therm)

unique(fishtherm$habitat)

## clean mean_age_unit and mean_age_error_unit
test_therm <- test_therm %>%
  mutate(mean_age_unit = case_when(mean_age_unit %in% c("y", "year", "years") ~ "years",
                                   mean_age_unit %in% c("days") ~ "days",
                                   mean_age_unit %in% c("months") ~ "months",
                                   mean_age_unit %in% c("DAH", "dph") ~ "days after hatch",
                                   TRUE ~ mean_age_unit),
         mean_age_unit = ifelse(mean_age_unit == " n/a", NA, mean_age_unit))


## clean
# response_error_unit
unique(test_therm$response_error_unit)
test_therm = test_therm %>%
  mutate(response_error_unit = case_when(response_error_unit %in% c("SEM", "s.e.m.", "SE") ~ "SEM",
                                   TRUE ~ response_error_unit))

## collapse acclimation time
which(!is.na(test_therm$acclim_time_total_days) & !is.na(test_therm$acclim_time_median))

test_therm = test_therm %>% 
  ## first, clean column
  mutate(acclim_time_max_days = ifelse(acclim_time_max_days == "nn/a", NA, acclim_time_max_days)) %>%
  mutate(acclim_time_statistic = case_when(!is.na(acclim_time_total_days) & 
                                        !is.na(acclim_time_min_days) & 
                                        !is.na(acclim_time_max_days) ~ "mean (min-max)",
                                        is.na(acclim_time_total_days) & !is.na(acclim_time_median) & 
                                          !is.na(acclim_time_min_days) & 
                                          !is.na(acclim_time_max_days) ~ "median (min-max)",
                                        !is.na(acclim_time_total_days) & 
                                          !is.na(acclim_time_max_days) ~ "mean (max)",
                                        !is.na(acclim_time_total_days) & 
                                          !is.na(acclim_time_min_days) ~ "mean (min)",
                                        is.na(acclim_time_total_days) & 
                                          !is.na(acclim_time_min_days) & 
                                          !is.na(acclim_time_max_days) ~ "min-max",
                                        is.na(acclim_time_min_days) & 
                                          !is.na(acclim_time_max_days) ~ "max",
                                        !is.na(acclim_time_min_days) & 
                                          is.na(acclim_time_max_days) ~ "min",
                                        !is.na(acclim_time_total_days) & 
                                          (is.na(acclim_time_min_days) | 
                                          is.na(acclim_time_max_days))~ "exact value",
                                      TRUE ~  NA),
         acclim_time = case_when(!is.na(acclim_time_total_days) & 
                                        !is.na(acclim_time_min_days) & 
                                        !is.na(acclim_time_max_days) ~ paste0(acclim_time_total_days, " (",
                                                                              acclim_time_min_days, "-",
                                                                              acclim_time_max_days, ")"),
                                      is.na(acclim_time_total_days) & !is.na(acclim_time_median) & 
                                        !is.na(acclim_time_min_days) & 
                                        !is.na(acclim_time_max_days) ~ paste0(acclim_time_median, " (",
                                                                              acclim_time_min_days, "-",
                                                                              acclim_time_max_days, ")"),
                                      !is.na(acclim_time_total_days) & 
                                        !is.na(acclim_time_max_days) ~ paste0(acclim_time_total_days, " (",
                                                                              acclim_time_max_days, ")"),
                                      !is.na(acclim_time_total_days) & 
                                        !is.na(acclim_time_min_days) ~ paste0(acclim_time_total_days, " (",
                                                                              acclim_time_min_days, ")"),
                                      is.na(acclim_time_total_days) & 
                                        !is.na(acclim_time_min_days) & 
                                        !is.na(acclim_time_max_days) ~ paste0(acclim_time_min_days, "-",
                                                                              acclim_time_max_days),
                                      is.na(acclim_time_min_days) & 
                                        !is.na(acclim_time_max_days) ~ paste0(acclim_time_max_days),
                                      !is.na(acclim_time_min_days) & 
                                        is.na(acclim_time_max_days) ~ paste0(acclim_time_min_days),
                                      !is.na(acclim_time_total_days) & 
                                        (is.na(acclim_time_min_days) | 
                                           is.na(acclim_time_max_days)) ~ paste0(acclim_time_total_days),
                                      TRUE ~  NA)) %>%
  select(-acclim_time_total_days,
         -acclim_time_min_days,
         -acclim_time_max_days, 
         -acclim_time_median)

## clean
unique(test_therm$post_acclim_exposure_duration_unit)
test_therm = test_therm %>% 
  mutate(post_acclim_exposure_duration_unit = case_when(post_acclim_exposure_duration_unit %in% 
                                                          c("d", "days") ~ "days",
                                                        post_acclim_exposure_duration_unit %in% 
                                                          c("h", "hours") ~ "hours",
                                                        TRUE ~ post_acclim_exposure_duration_unit)) %>%
  mutate(duration_of_study_days = str_replace_all(duration_of_study_days, ".00", "")) %>%
  rename("study_duration" = duration_of_study_days)

## clean treatment data 
unique(test_therm$treatment_1_type)
unique(test_therm$treatment_1_group)
unique(test_therm$treatment_1_group_error_units)
unique(test_therm$treatment_2_type)
unique(test_therm$treatment_2_group)
unique(test_therm$treatment_2_group_units)

## get rid of empty error columns 
unique(test_therm$treatment_2_group_error)
unique(test_therm$treatment_2_group_error_units)
unique(test_therm$treatment_1_group_units)
unique(test_therm$treatment_1_group_error)

test_therm = test_therm %>%
  mutate(treatment_1_type = case_when(treatment_1_type == "Acclimation" ~ "acclimation", 
                                      treatment_1_type == "Photoperiod" ~ "photoperiod",
                                      treatment_1_type == "Salinity" ~ "salinity",
                                      treatment_1_type == "Size" ~ "size",
                                      treatment_1_type == "Ration" ~ "ration",
                                      treatment_1_type == "Oxygen" ~ "oxygen",
                                      TRUE ~ treatment_1_type),
         treatment_2_type = case_when(treatment_2_type == "Size" ~ "size", 
                                      treatment_2_type == "Ration" ~ "ration",
                                      TRUE ~ treatment_2_type),
         treatment_1_group = tolower(treatment_1_group),
         treatment_1_group_units = ifelse(treatment_1_group_units == "days-after-hatching", "days after hatch", 
                                          treatment_1_group_units),
         ) %>%
  select(-treatment_1_group_error, -treatment_1_group_error_units, -treatment_2_group_error, -treatment_2_group_error_units)

## remove columns 
test_therm <- test_therm %>%
  select(-temp_units, ## (all are degrees C)
         -abs_latitude, ## can be calculated from lat
         -response_mode, ## we don't know what this is
         -mean_age_error,
         -mean_age_error_unit, ## empty 
         
         ## things that were just for us:
         -notes_while_extracting,
         -fig_file_name,
         -data_type,
         -animal_maintenance_notes,
         -experiment_design_notes)

## clean field or lab
unique(test_therm$field_or_lab)
test_therm <- test_therm %>%
  mutate(field_or_lab = ifelse(field_or_lab == "creekside", "field",  field_or_lab))
length(which(is.na(test_therm$field_or_lab)))

## clean land_or_sea
unique(test_therm$land_or_sea)
test_therm <- test_therm %>%
  mutate(realm = ifelse(land_or_sea == "oceanic", "marine", 
                        ifelse(land_or_sea == "terrestrial", "freshwater",
                               NA))) %>%
  select(-land_or_sea)
length(which(is.na(test_therm$realm)))

## -----------------------
## rename columns 
## -----------------------
test_therm <- test_therm %>%
  rename("habitat_general" = habitat_water,
         "response" = given_trait_name,
         "response_type" = Trait.Group,
         "response_motivation" = Trait.motivation,
         "level_of_organization" = organization,
         "n_test_temps" = n_unique_temps,
         "response_statistic" = response_sample_type,
         "reported_response_name" = raw_response_type,
         "habitat_specific" = habitat,
         "collection_location" = collection_site,
         "lat_lon_units" = lat_long_units,
         "lat_lon_method" = lat_long_method,
         "age" = mean_age, 
         "age_unit" = mean_age_unit, 
         "response_error_upper" = response_upper_error,
         "response_error_lower" = response_lower_error,
         "response_equation" = equation_used)

colnames(test_therm)

## delete percent survival measurements into a new curve id 
test_therm <- test_therm %>%
  select(-percent_survival, -percent_survival_error, -percent_survival_error_unit)

## clean sample size columns 
# number_of_fish_per_tank, 
# number_of_observations_on_which_the_reported_mean_and_error_is_calculated, 
# number_of_independent_replicates_of_treatment_temperature..this.will.be.1.if.pseudoreplicated.or.every.tank.was.entered.on.a.separate.line.,
# notes_about_sample_size_and_replication, 
# initial_sample_size, 
# final_sample_size, 

test_therm <- test_therm %>%
  rename("n_fish_per_tank" = number_of_fish_per_tank, 
         "n" = number_of_observations_on_which_the_reported_mean_and_error_is_calculated, 
         "n_replicates" = number_of_independent_replicates_of_treatment_temperature..this.will.be.1.if.pseudoreplicated.or.every.tank.was.entered.on.a.separate.line.,
         "n_notes" = notes_about_sample_size_and_replication, 
         "n_initial" = initial_sample_size, 
         "n_final" = final_sample_size)

## add species taxonomy 
taxonomy <- read.csv("processed-data/FishTPC/taxonomy.csv")

## check none are missing
length(which(!taxonomy$species_ID %in% test_therm$species_ID))
length(which(is.na(taxonomy$species_name)))
which(taxonomy$gen_spp != taxonomy$species_name)

colnames(taxonomy)

## get rid of taxonomy columns that are the same for all fish in the database 
taxonomy <- taxonomy %>%
  select(-kingdom, -phylum, -superclass, -superorder, -suborder,
         -subfamily, -wormsid_genus) %>%
  rename("genus_species" = species_name, 
         "worms_id" = wormsid_species)

## left join
test_therm = left_join(test_therm, taxonomy)

## add reference information
refs <- read.csv("raw-data/FishTPC/data_extraction_07_27_2026 - study_ID.csv")
colnames(refs)

## get rid of ones we dont have data from
refs <- refs %>%
  filter(study_ID %in% test_therm$study_ID)

## keep necessary cols only
refs <- refs %>%
  select(study_ID, title, pub_year, journal, doi, citation.APA) %>%
  filter(!study_ID == "")

## make sure all study ids are in there
which(!test_therm$study_ID %in% refs$study_ID)

## find missing dois 
missing_doi = refs %>%
  filter(doi == "" | !str_detect(doi, "doi") & !str_detect(doi, "10.1"))

## manual search results:
new_dois = data.frame(study_ID = c("2_0111", "2_0112", "2_0117", "1_0008",
                                   "1_0022", "2_0050", "2_0064", "2_0069", "2_0032"),
                      doi = c("doi.org/10.1080/02705060.2002.9663908",
                              "doi.org/10.1093/conphys/coaa036",
                              "doi.org/10.1139/f91-290",
                              "10.1371/journal.pone.0225568",
                              "https://www.science.nus.edu.sg/wp-content/uploads/sites/11/2024/05/s19rbz273-284.pdf", 
                              "10.3354/MEPS072213",
                              "10.3354/meps217121",
                              "https://www.researchgate.net/publicatio /285963104_Influence_of_distillery_effluent_on_feeding_energetics_of_freshwater_fish_in_relation_to_water_temperature", 
                              "https://avesis.comu.edu.tr/yayin/02060d10-9431-44a7-8c6f-1f641a823a1f/the-development-of-doctor-fish-garra-rufa-heckel-1843-larvae-and-the-effect-of-tempe-a-tu-on-their-growth"))

missing_doi <- missing_doi %>%
  select(-doi) %>% 
  left_join(., new_dois)

refs <- refs %>%
  filter(!study_ID %in% missing_doi$study_ID) %>%
  rbind(., missing_doi)

## fix some with spaces 
refs$doi[which(refs$doi == "doi.org/10.1016 /j.jembe.2005.01.009")] = "doi.org/10.1016/j.jembe.2005.01.009"
refs$doi[which(refs$doi == "doi.org /10.1093/conphys/coz063 ")] = "doi.org/10.1093/conphys/coz063 "
refs$doi[which(refs$doi == "https://doi.org/10.1590 /S1679-62252013000400012 ")] = "https://doi.org/10.1590/S1679-62252013000400012 "
refs$doi[which(refs$doi == "doi.org/10.1577/1548-8659")] = "doi.org/10.1577/1548-8659(1999)128<0962:TGTROJ>2.0.CO;2"

## get citations in apa format from DOIs manually
write.csv(data.frame(doi = refs$doi), "processed-data/FishTPC/dois.csv", row.names = F)

apa = read.csv("processed-data/FishTPC/dois_with_apa.csv") %>%
  select(doi, APA, found) %>%
  filter(doi != "")

apa = apa %>%
  mutate(new_doi = str_split_fixed(APA, "https://", 2)[,2]) %>% 
  select(-found)

apa <- left_join(apa, refs) %>%
  select(study_ID, new_doi, APA) %>%
  rename("citation_apa" = APA, "doi" = new_doi)

## join to fishthem
test_therm = left_join(test_therm, apa)

## reorder columns 
test_therm2 = test_therm %>%
  select( 
    ## CURVE DATA 
    study_ID,
    curve_ID, 
    curve_type,
    species_ID, 
    genus_species,
    cohort_ID,
    individual_ID,
    response, 
    test_temp, 
    test_temp_error, 
    test_temp_error_unit, 
    n_test_temps,
    response_value, 
    response_unit,
    response_statistic,
    response_error, 
    response_error_upper, 
    response_error_lower, 
    response_error_unit, 
    response_equation, 
    
    ## TAXONOMIC INFORMATION
    class, 
    order, 
    family, 
    genus, 
    subspecies,
    worms_id,
    
    ## RESPONSE DESCRIPTIONS
    reported_response_name,
    response_type,
    response_motivation,
    level_of_organization,
    
    ## BODY SIZE 
    body_length,
    body_length_statistic,
    body_length_unit,
    body_length_error,
    body_length_error_unit,
    body_length_metric,
    body_mass,
    body_mass_statistic,
    body_mass_unit,
    body_mass_error,
    body_mass_error_unit,
    body_mass_metric,
    
    ## AGE AND SEX
    life_stage_manip, ## perhaps change name, perhaps make sure it is NA if it is same as tested 
    life_stage_tested,
    age, 
    age_unit, 
    sex, 
    
    ## SAMPLE SIZE AND SURVIVAL
    n,
    n_replicates,
    n_fish_per_tank,
    n_initial,
    n_final,
    n_notes,

    ## ACCLIMATION
    acclim_temp, 
    acclim_temp_error, 
    acclim_temp_error_unit, 
    acclim_time,
    acclim_time_statistic,
    acclim_time_error, 
    acclim_time_error_unit,
    post_acclim_exposure_duration, 
    post_acclim_exposure_duration_unit, 
    study_duration, 
    
    ## ADDITIONAL TREATMENTS 
    treatment_1_type, 
    treatment_1_group, 
    treatment_1_group_units, 
    treatment_2_type, 
    treatment_2_group, 
    treatment_2_group_units, 
    
    ## HABITAT
    realm,
    habitat_general, 
    habitat_specific,  
    habitat_source,
    
    ## COLLECTION LOCATION
    collection_location,
    latitude, 
    longitude, 
    lat_lon_units, 
    lat_lon_method,
    
    ## EXPERIMENTAL SETUP NOTES
    extraction_method,
    data_source, 
    field_or_lab,
    
    ## study information
    doi,
    citation_apa
    )

## make sure we don't accidentally lose any columns
colnames(test_therm)[which(!colnames(test_therm) %in% colnames(test_therm2))]

test_therm = test_therm2

## do some final variable cleans
test_therm = test_therm %>%
  mutate(habitat_source = case_when(habitat_source == "web" ~ "web search",
                                    habitat_source == "google-maps" ~ "google maps",
                                    habitat_source == "FishBase" ~ "fishbase",
                                    habitat_source == "source" ~ "reported",
                                    TRUE ~ habitat_source),
         lat_lon_method = ifelse(lat_lon_method == "source", "reported", lat_lon_method)) %>%
  mutate(response_type = tolower(response_type)) %>%
  mutate(response_statistic = ifelse(response_statistic == "individual", "individual measurement", response_statistic)) %>%
  mutate(acclim_time = ifelse(acclim_time == "no acclimation", "0", acclim_time)) %>%
  mutate(genus_species = str_to_sentence(genus_species))

## write out
write.csv(test_therm, "processed-data/FishTPC/FishTPC.csv", row.names = F)

## write out metadata template
metadata = data.frame(variable = colnames(test_therm))

write.csv(metadata, "processed-data/FishTPC/FishTPC_metadata_template.csv", row.names = F)


length(colnames(fishtherm)) ## from 107 
length(colnames(test_therm)) ## to 83 columns :-)

## get stats

length(unique(test_therm$curve_ID))

ft %>%
  select(curve_ID, n_test_temps) %>%
  distinct() %>% 
  group_by(n_test_temps) %>%
  tally()

ft %>%
  select(curve_ID, latitude, longitude) %>%
  distinct() %>% 
  group_by(latitude, longitude) %>%
  tally() %>%
  nrow()

ft %>%
  select(curve_ID, habitat_general) %>%
  distinct() %>% 
  group_by(habitat_general) %>%
  tally() 

ft %>%
  select(study_ID, realm) %>%
  distinct() %>% 
  group_by(realm) %>%
  tally() 

ft %>%
  select(curve_ID, realm) %>%
  distinct() %>% 
  group_by(realm) %>%
  tally() 


ft %>%
  select(family) %>%
  distinct() %>%
  nrow()

ft %>%
  select(curve_ID, life_stage_tested) %>%
  distinct() %>% 
  group_by(life_stage_tested) %>%
  tally() 

ft %>%
  select(curve_ID, response_type) %>%
  distinct() %>% 
  group_by(response_type) %>%
  tally() 

ft %>%
  select(curve_ID, curve_type) %>%
  distinct() %>% 
  group_by(curve_type) %>%
  tally() 

ft %>%
  select(curve_ID, treatment_1_type) %>%
  distinct() %>% 
  group_by(treatment_1_type) %>%
  tally() 


ft %>%
  select(curve_ID, response_statistic) %>%
  distinct() %>% 
  group_by(response_statistic) %>%
  tally() 

ft %>%
  select(curve_ID, n_test_temps) %>%
  distinct() %>% 
  mutate(mean_test_temp = mean(n_test_temps),
         med_test_temps = median(n_test_temps), 
         n_more_than_5 = length(which(.$n_test_temps > 5)))

ft %>%
  select(curve_ID, test_temp) %>%
  distinct() %>% 
  group_by(curve_ID) %>%
  mutate(min_test_temp = min(test_temp),
         max_test_temp = max(test_temp), 
         range = max_test_temp - min_test_temp) %>%
  select(range, curve_ID) %>% distinct() %>%
  ungroup() %>%
  mutate(mean = mean(range))
