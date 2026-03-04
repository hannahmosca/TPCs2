### this is a script to get freshwater temperature data from future streams ###
# goal: to get a raster of monthly averages from 1982-2025

### loading/installing required packages ###
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)
library(viridis)
library(tidyterra)
library(rnaturalearth)
#### 01: Merge 10-15 yr raster chunks of weekly temp data ####
#14 year file chunks, historical and present weekly 

  ## 1979 thr 1985
  file1979thr1985 <- "waterTemp_weekAvg_output_E2O_hist_1979-01-07_to_1985-12-30.nc"
  r_temp1979thr1985 <- rast((here("raw-data", "freshwater-temp", file1979thr1985)), subds = "waterTemperature")
  time_values <- time(r_temp1979thr1985)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp1979thr1985) <- layer_names
  names(r_temp1979thr1985)
  layer_1981 <- names(r_temp1979thr1985)[[157]]
  r_temp1982thr1985 <- subset(r_temp1979thr1985, 157:364)
  names(r_temp1982thr1985) #208 weeks: starting from 1982-01-07 to 1985-12-30

  ## 1986 thr 1995
  file1986thr1995 <- "waterTemp_weekAvg_output_E2O_hist_1986-01-07_to_1995-12-30.nc"
  r_temp1986thr1995 <- rast((here("raw-data", "freshwater-temp", file1986thr1995)), subds = "waterTemperature")
  time_values <- time(r_temp1986thr1995)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp1986thr1995) <- layer_names
  names(r_temp1986thr1995) #520 weeks: starting from 1986-01-07 to 1995-12-30

  ## 1996 thr 2005
  file1996thr2005 <- "waterTemp_weekAvg_output_E2O_hist_1996-01-07_to_2005-12-30.nc"
  r_temp1996thr2005 <- rast((here("raw-data", "freshwater-temp", file1996thr2005)), subds = "waterTemperature")
  time_values <- time(r_temp1996thr2005)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp1996thr2005) <- layer_names
  names(r_temp1996thr2005) #520 weeks: starting from 1996-01-07 to 2005-12-30

  ## 2006 thr 2019
  file2006thr2019 <- "waterTemp_weekAvg_output_hadgem_rcp4p5_2006-01-07_to_2019-12-30.nc"
  r_temp2006thr2019 <- rast((here("raw-data", "freshwater-temp", file2006thr2019)), subds = "waterTemperature")
  time_values <- time(r_temp2006thr2019)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp2006thr2019) <- layer_names
  names(r_temp2006thr2019) #728 weeks: starting from 2006-01-07 to 2019-12-30

  ## 2020 thr 2029
  file2020thr2029 <- "waterTemp_weekAvg_output_hadgem_rcp4p5_2020-01-07_to_2029-12-30.nc"
  r_temp2020thr2029 <- rast((here("raw-data", "freshwater-temp", file2020thr2029)), subds = "waterTemperature")
  time_values <- time(r_temp2020thr2029)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp2020thr2029) <- layer_names
  names(r_temp2020thr2029) # want only through sept 2025
  r_temp2020thr2025 <- subset(r_temp2020thr2029, 1:299)
  names(r_temp2020thr2025) #299 weeks: starting from 2020-01-07 to 2025-09-30

## merge rasters 
  freshwater_r_temp <- c(r_temp1982thr1985, r_temp1986thr1995, r_temp1996thr2005, r_temp2006thr2019, r_temp2020thr2025)

#check raster out
  names(freshwater_r_temp)
  crs(freshwater_r_temp) 
  ext(freshwater_r_temp)
  res(freshwater_r_temp)     
  ncell(freshwater_r_temp)    
  nlyr(freshwater_r_temp)    

#make more space
  rm(r_temp1979thr1985)
  rm(r_temp1982thr1985)
  rm(r_temp1986thr1995)
  rm(r_temp1996thr2005)
  rm(r_temp2006thr2019)
  rm(r_temp2020thr2025)
  rm(r_temp2020thr2029)


#### 02: average across weeks to get monthly ####
  ##filter out high values//make them NA
  threshold <- 350 # 76.86°C
  freshwater_r_temp[freshwater_r_temp > 350] <- NA
  
  ##convert to celcius
  freshwater_r_temp_cel <- freshwater_r_temp - 273.15
  
  #naming thing
  dates <- as.Date(names(freshwater_r_temp_cel))  # assuming layer names are dates
  month_group <- format(dates, "%Y-%m")
  
  ##average from weekly to monthly
  r_monthly <- tapp(freshwater_r_temp_cel, month_group, function(x) mean(x, na.rm = TRUE))
  
  ## adjust monthly layer names
  unique_month_group <- unique(month_group)
  month <- as.Date(paste0(unique_month_group, "-01"))
  names(r_monthly) <- month
  
  #make space/check out raster
  rm(freshwater_r_temp)
  names(r_monthly)
  head(r_monthly)
  res(r_monthly)
  plot(r_monthly[[1]])

  
#### 03: save file locally so don't have to do this computation again ####
writeCDF(r_monthly, filename = here("processed-data", "freshwater_monthly.nc"))


#### 04: compute summary stats on raster ####
  ## load in raster, check names/rename if neccessary
  freshwater_monthly <- rast((here("processed-data", "freshwater_monthly.nc")))
  names(freshwater_monthly)
  #need to rename monthly values
  dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
  names(freshwater_monthly) <- dates
  
  
  ## computing summary stats across layers
  mean_raster <- app(freshwater_monthly, mean, na.rm = TRUE)
  sd_raster <- app(freshwater_monthly, sd, na.rm = TRUE)
  median_raster <- app(freshwater_monthly, median, na.rm = TRUE)
  min_raster <- app(freshwater_monthly, min, na.rm = TRUE)
  max_raster <- app(freshwater_monthly, max, na.rm = TRUE)
  quant_raster <- app(
    freshwater_monthly,
    fun = function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(c(NA,NA))
      quantile(x, c(0.025, 0.975))
    }
  )
 freshwater_summary <- c(mean_raster, sd_raster, min_raster, max_raster, quant_raster)
 names(freshwater_summary) <- c("mean", "sd", "min", "max", "q2.5", "q97.5")
 writeCDF(freshwater_summary, filename = here("processed-data", "freshwater_monthly_summarized.nc"))

#### 05: masking discharge ####
  rm(list=ls()) #make room/clean environment
  ##load data
  freshwater_temp <- rast((here("processed-data", "freshwater_monthly_summarized.nc"))) #average across months from 1982-2025
  discharge <- rast(here("raw-data", "discharge_Avg.nc"))
  coastline <- ne_coastline(returnclass = "sf", scale = 110)
  
  ## check alignment
  ext(freshwater_temp)
  ext(discharge)
  res(freshwater_temp)
  res(discharge)
  
  ##crop discharge file
  discharge_aligned <- resample(discharge, freshwater_temp, method = "near")
  
  ##check out files
  nlyr(freshwater_temp) #6 layers
  nlyr(discharge) #1 layer
  names(freshwater_temp)
  names_fresh_temp <- c("mean", "sd", "min", "max", "q_low", "q_high")
  names(freshwater_temp) <- names_fresh_temp
  
  ## make mask
  Qlim <- 5 # this defines the limit for discharge
  discharge_mask <- discharge_aligned >= Qlim
  plot(discharge_mask)
  ## mask freshwater temp with discharge
  freshwater_masked <- mask(freshwater_temp, discharge_mask, maskvalues = FALSE)
  df <- as.data.frame(freshwater_masked, xy = TRUE, na.rm = TRUE)
  
  ### map of freshwater temp means with points overlaid
d <-  ggplot(df) +
    geom_sf(data = coastline, color = "black", fill = NA, linewidth = 0.2, inherit.aes = FALSE) +
    geom_raster(aes(x=x, y=y, fill=mean), interpolate = TRUE) +    
    scale_fill_viridis() +
    geom_spatvector(data = new_my_points,
                    color = "white", fill = "red", size = 1.5, shape = 21, stroke = 0.5) +
    coord_sf(xlim = c(-180, 180),
             ylim = c(-50, 90)) +
    theme_void() +
    theme(legend.position = "bottom")
  d
  ## save map
  ggsave("freshwater_map_wpoints.pdf", plot = d, path = here("figures"), width = 8, height = 5)
  
  #save masked freshwater 
  writeCDF(freshwater_masked, filename = here("processed-data", "freshwater_summarized_masked.nc"))
  

#### 06: extracting mypoint data ####
  ## load required datasets
  datasets <- read.csv(here('processed-data', 'FishTherm.csv'))
 
  
  #get freshwater fish
  freshwater <- datasets %>% 
    filter(land_or_sea == "terrestrial") %>%
    filter(!(is.na(latitude))) %>%
    filter(!(is.na(longitude))) 
  #get lat/long
  unique_lat_long <- freshwater %>%
    select(latitude, longitude) %>%
    distinct()
  
  #check where points fall, some estuaries to be dealt with
  new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(freshwater_temp))
  library(tidyterra)
  ggplot() +
    geom_spatraster(data = freshwater_masked[[1]]) +
    geom_spatvector(data = new_my_points, color = "red")
  
  #extract points from non discharge masked data
  point_means <- terra::extract(freshwater_temp[[1]], new_my_points, method = "simple", search_radius = 30000)
  point_sd <- terra::extract(freshwater_temp[[2]], new_my_points, method = "simple", search_radius = 30000)
  point_min <- terra::extract(freshwater_temp[[3]], new_my_points, method = "simple", search_radius = 30000)
  point_max <- terra::extract(freshwater_temp[[4]], new_my_points, method = "simple", search_radius = 30000)
  point_q_low <- terra::extract(freshwater_temp[[5]], new_my_points, method = "simple", search_radius = 30000)
  point_q_high <- terra::extract(freshwater_temp[[6]], new_my_points, method = "simple", search_radius = 30000)
  
  #combine to get summary stats of mypoints data
  all <- cbind(point_means, point_sd, point_min, point_max, point_q_low, point_q_high) %>% select(mean, sd, min, max, q_low, q_high)
  #add lat and long back
  all$latitude = unique_lat_long$latitude
  all$longitude = unique_lat_long$longitude
  
  ## save my point data
  saveRDS(all, file = here("processed-data", "my_points_freshwater_summary.RDS"))
