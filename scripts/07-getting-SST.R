### this is a script to get marine temperature data ###
# goal: to get a raster of monthly averages from 1982-2025
## loading/installing required packages ###
library(ncdf4)
library(terra)
library(here)
library(dplyr)
library(tidyverse)
library(viridis)
library(tidyterra)
#### 01: subset raster to get 1982-2025
  ## load data
  filename <- "sst.mon.mean.nc" # sst data, 529 monthly means from 
  r_temp = rast((here("raw-data", filename)), subds = "sst")
  
  ##check it out
  r_temp
  nlyr(r_temp) #529
  names(r_temp)[1:10]
  crs(r_temp) #checks crs of raster
  dim(r_temp) # 720 rows, 1440 columns, and 529
  plot(r_temp[[12]]) ## plot a layer
  res(r_temp)
  
  ## rename layers the dates
  time_values <- time(r_temp)
  layer_names <- format(as.Date(time_values), "%Y-%m-%d")
  names(r_temp) <- layer_names
  names(r_temp)

  ## rotate so gets it to be -180 to 180 and -90to 90
  r_temp <- rotate(r_temp)
  plot(r_temp[[12]])
  
  ##subset r_temp so starts where freshwater data does: 1982-01
  r_temp1982_01to2025_09 <- subset(r_temp, 5:529)
  sst_monthly <- r_temp1982_01to2025_09
  names(sst_monthly)
  
  ##need to rename monthly values
  dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
  names(sst_monthly) <- dates
  
  ## remove missing values
  fill_value <- -9.96921e+36
  # replace all fill values with NA
  sst_monthly[sst_monthly == fill_value] <- NA
  
  ##save raster of all monthly temps
  writeCDF(sst_monthly, filename = here("processed-data", "marine_monthly1982_01to2025_09.nc"), overwrite=TRUE)

#### 02: compute summary stats on raster ####
  ## load in raster, check names/rename if neccessary
  sst_monthly <- rast((here("processed-data", "marine_monthly1982_01to2025_09.nc")))
  names(sst_monthly)
  
  ##need to rename monthly values
  dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
  names(sst_monthly) <- dates
  
  ## extract all temporal values for mypoints
  #load point data
  datasets <- read.csv(here('processed-data', 'FishTherm.csv'))
  ##get marine fish
  marine <- datasets %>% 
    filter(land_or_sea == "oceanic") %>%
    filter(!(is.na(latitude))) %>%
    filter(!(is.na(longitude))) 
  
  #get lat/long
  unique_lat_long <- marine %>%
    dplyr::select(latitude, longitude, study_ID) %>%
    distinct()
  
  #check where points fall, 
  new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(sst_monthly))
  ggplot() +
    geom_spatraster(data = sst_monthly[[1]]) +
    geom_spatvector(data = new_my_points, color = "red")
  
  ## extract raw all temporal point values

  mypoints_list <- vector("list", nlyr(sst_monthly))
  
  for (i in 1:nlyr(sst_monthly)) {
    message("Extracting layer ", i, " of ", nlyr(sst_monthly))
    x <- terra::extract(
      sst_monthly[[i]],
      new_my_points,
      method = "simple",
      search_radius = 30000
    )
    mypoints_list[[i]] <- x[,2] 
  }
  
  point_vals <- as.data.frame(do.call(cbind, mypoints_list))
  names(point_vals) <- names(sst_monthly)
  point_vals$latitude <-  unique_lat_long$latitude
  point_vals$longitude <- unique_lat_long$longitude 
  point_vals <- point_vals %>%
    dplyr::select(latitude, longitude, everything())
  
  saveRDS(point_vals, file = here("processed-data", "marine_sst_all_temporal_mypoints.RDS"))
  
  
  ## computing summary stats across layers
  mean_raster <- app(sst_monthly, mean, na.rm = TRUE)
  sd_raster <- app(sst_monthly, sd, na.rm = TRUE)
  min_raster <- app(sst_monthly, min, na.rm = TRUE)
  max_raster <- app(sst_monthly, max, na.rm = TRUE)
  quant_raster <- app(
    sst_monthly,
    fun = function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(c(NA, NA))
      quantile(x, c(0.025, 0.975))
    }
  )
  ##combine them
  sst_summary <- c(
    mean_raster,
    sd_raster,
    min_raster,
    max_raster,
    quant_raster
  )
  ##rename
  names(sst_summary) <- c(
    "mean",
    "sd",
    "min",
    "max",
    "q2.5",
    "q97.5"
  )
  ## save as a cdf
  writeCDF(sst_summary, filename = here("processed-data", "sst_monthly_summarized.nc"))
 
  
#### 04: make map of overlaid point data ####
  df <- as.data.frame(sst_summary, xy = TRUE, na.rm = TRUE)
  
  d <-  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = mean), interpolate = TRUE) +
    scale_fill_viridis() +
    geom_spatvector(data = new_my_points,
                    color = "white", fill = "red", size = 1.5, shape = 21, stroke = 0.5) +
    theme_void() +
    theme(legend.position = "bottom")
  d

  #### 05 extract my point data ####

  #extract points from non discharge masked data
  point_means <- terra::extract(sst_summary[[1]], new_my_points, method = "simple", search_radius = 30000)
  point_sd <- terra::extract(sst_summary[[2]], new_my_points, method = "simple", search_radius = 30000)
  point_min <- terra::extract(sst_summary[[3]], new_my_points, method = "simple", search_radius = 30000)
  point_max <- terra::extract(sst_summary[[4]], new_my_points, method = "simple", search_radius = 30000)
  point_q2.5 <- terra::extract(sst_summary[[5]], new_my_points, method = "simple", search_radius = 30000)
  point_q97.5 <- terra::extract(sst_summary[[6]], new_my_points, method = "simple", search_radius = 30000)
  
  #combine to get summary stats of mypoints data
  all <- cbind(point_means, point_sd, point_min, point_max, point_q2.5, point_q97.5) %>% select(mean, sd, min, max, q2.5, q97.5)
  #add lat and long back
  all$latitude = unique_lat_long$latitude
  all$longitude = unique_lat_long$longitude
  
  
  ## save my point data
  saveRDS(all, file = here("processed-data", "my_points_sst_summary.RDS"))
  
  