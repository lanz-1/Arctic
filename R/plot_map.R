library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)



# This function plots a map of modelled LAI trends in latitudes >= 60



plot_map <- function(dgvm) {
  #read data. LAI from one of the models.
  LAI <- metR::ReadNetCDF(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"),
                          vars = "lai") |> as_tibble()
  
  
  #some models have a 'lat' column, others a 'latitude' column. This causes errors. 
  
  #rename potential 'lat' column to 'latitude'
  if ("lat" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(latitude = lat)
  }
  
  #same for 'lon' and 'longitude'
  if ("lon" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(longitude = lon)
  }
  
  
  #now filter latitudes above 60 degrees
  LAI <- LAI |> dplyr::filter(latitude >= 60)
  
  
  #handle the different time format of VISIT-UT. The code is from an AI
  if (!inherits(LAI$time, "POSIXct")) {
    nc <- ncdf4::nc_open(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"))
    time_vals <- nc$dim$time$vals
    ncdf4::nc_close(nc)
    
    origin_year <- 1700
    actual_years <- as.integer(floor(origin_year + time_vals))  # 325 year values
    
    # Map each row's raw time value to the correct converted year
    LAI <- LAI |> dplyr::mutate(
      time = as.POSIXct(
        paste0(actual_years[match(time, time_vals)], "-07-15"),
        tz = "UTC"
      )
    )
  }
  
  
  # Filter data from 1982 to 2021
  LAI <- LAI |> dplyr::filter(
    time >= as.POSIXct("1982-01-01", tz = "UTC"),
    time <= as.POSIXct("2021-12-31", tz = "UTC")
  )
  
  
  # Build multilayer SpatRaster with one layer per year
  years_f <- sort(unique(LAI$time))
  raster_list_f <- lapply(years_f, function(yr) {
    r_df <- LAI |>
      dplyr::filter(time == yr) |>
      dplyr::select(longitude, latitude, lai) |>
      dplyr::mutate(
        longitude = round(longitude, 3),
        latitude  = round(latitude, 3)
      )
    
    # Try direct rasterization first
    r_out <- tryCatch({
      terra::rast(r_df, type = "xyz", crs = "EPSG:4326")
    }, error = function(e) {
      # Capture r_df explicitly into error handler scope
      r_df_local <- r_df
      pts <- terra::vect(r_df_local, geom = c("longitude", "latitude"), crs = "EPSG:4326")
      terra::rasterize(pts, target_grid, field = "lai", fun = mean)
    })
    
    # Resample onto common grid for comparability across models
    terra::resample(r_out, target_grid, method = "bilinear")
  })
  
  
  raster_LAI_f <- terra::rast(raster_list_f)
  names(raster_LAI_f) <- years_f
  
  # Remove ocean surface cells
  raster_LAI_f <- raster_LAI_f |> terra::mask(land)
  
  
  # Create a numeric year vector matching layer order
  year_nums <- as.numeric(names(raster_LAI_f))
  
  # Fit pixel-wise linear trend using terra::app() with lm
  mod_trend <- terra::app(raster_LAI_f, fun = function(x) {
    if (all(is.na(x))) return(NA)
    fit <- lm(x ~ year_nums)
    return(coef(fit)[2])  # return slope
  })
  
  
  names(mod_trend) <- "mod_trend"
  
  
  
  
  
  
  
  
  
  
  # Plot trendline map
  mod_map <- ggplot() +
    geom_spatraster(data = mod_trend) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "darkgreen",
      limits = c(-0.02, 0.04), #set limits to -0.02 and 0.04 in order to have stronger colors.
      midpoint = 0,
      na.value = NA,
      name = "LAI trend\n(per year)") +
    labs(title = "Linear trend in LAI (1982–2021)") +
    theme_grey() +
    theme(
      panel.grid.major = element_line(colour = "gray")) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))
  
  mod_map
}
  
  
  
  