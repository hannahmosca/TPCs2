#### this script is for trying to make a map that includes both marine and freshwater data ####
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

#get freshwater fish

#get lat/long
unique_lat_long <- datasets %>%
  dplyr::select(latitude, longitude) %>%
  distinct()

#check where points fall, some estuaries to be dealt with
new_my_points <- vect(unique_lat_long, geom = c("longitude", "latitude"), crs = crs(sst_monthly))
library(tidyterra)
map <- ggplot() +
  geom_raster(data = df_fresh, aes(x = x, y = y, fill = mean), interpolate = TRUE) +
  geom_raster(data = df_marine, aes(x = x, y = y, fill = mean), interpolate = TRUE) +
  scale_fill_viridis() +
  geom_spatvector(data = new_my_points,
                  color = "white", fill = "red", size = 1.2, alpha = .8, shape = 21, stroke = 0.5) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) 
## save map
ggsave("all_water_temp+points_map.pdf", plot = map, path = here("figures"), width = 8, height = 5)


