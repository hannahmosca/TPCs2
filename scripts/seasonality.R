##seasonality##
marine_rast <- rast((here("processed-data", "marine_monthly1982_01to2025_09.nc")))
names(marine_rast)

##need to rename monthly values
dates <- seq(as.Date("1982-01-01"), as.Date("2025-09-01"), by = "month")
names(marine_rast) <- dates

## remove missing values
fill_value <- -9.96921e+36
# replace all fill values with NA
marine_rast[marine_rast == fill_value] <- NA
install.packages("raster")
library(raster)
library(terra)


random_points_data <- as.data.frame(
  terra::spatSample(x = marine_rast, size = 10, method = "random", xy = TRUE, na.rm = TRUE)
)

temp  = as.vector(random_points_data[, -c(1:2)])
temp = ts(temp, start = 1982-01-01, frequency = 12) 
ts.plot(temp, ylab = "temp")
