#' Write 3D Array Data to a NetCDF File
#'
#' Saves a 3D array (time, longitude, latitude) to a NetCDF file with appropriate metadata.
#'
#' @param ary_Global array (time, lon, lat),  from unf_2_ary_WG3(), default 5' (4320 * 2160)
#' @param fn_NC str, filename
#' @param num_TimeDim numric, vector of time dimension
#' @param unit_TimeDim str, unit of time dimension, like "year", "day from 1990-01-01"
#' @param name_Data,unit_Data,longname_Data str, data name, longname and unit for nc-file
#' @param dim_Order str 'TimeLonLat' or 'LonLatTime'
#' @param num_Resolut int, resolution in arc minute
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncvar_put ncatt_put nc_close
#' @export
write_nc_global <- function(ary_Global, fn_NC, num_TimeDim, unit_TimeDim, name_Data, unit_Data, longname_Data, dim_Order = "TimeLonLat", num_Resolut = 5) {

  # Define longitude and latitude points
  lon <- seq(-180 + num_Resolut/60/2, 180, by = num_Resolut/60)  # Longitude: points
  lat <- seq(90 - num_Resolut/60/2, -90, by = -num_Resolut/60)  # Latitude: points

  # Define dimensions for netCDF
  dim_time <- ncdim_def(name = "time", units = unit_TimeDim, vals = num_TimeDim)
  dim_lon <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  dim_lat <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)

  # Define the variable based on the dimension order
  var_Data <- switch(
    dim_Order,
    TimeLonLat = ncvar_def(
      name = name_Data,
      units = unit_Data,
      dim = list(dim_time, dim_lon, dim_lat),
      missval = -9999,
      longname = longname_Data
    ),
    LonLatTime = ncvar_def(
      name = name_Data,
      units = unit_Data,
      dim = list(dim_lon, dim_lat, dim_time),
      missval = -9999,
      longname = longname_Data
    ),
    stop("Invalid dim_Order value. Expected 'TimeLonLat' or 'LonLatTime'.")
  )

  # Validate dimensions of ary_Global
  expected_dims <- switch(
    dim_Order,
    TimeLonLat = c(length(num_TimeDim), length(lon), length(lat)),
    LonLatTime = c(length(lon), length(lat), length(num_TimeDim))
  )

  if (!identical(dim(ary_Global), expected_dims)) {
    stop(paste(
      "Dimension mismatch! ary_Global has dimensions", paste(dim(ary_Global), collapse = " x "),
      "but expected dimensions are", paste(expected_dims, collapse = " x ")
    ))
  }

  # Create netCDF file
  nc_ <- nc_create(fn_NC, vars = var_Data)

  # Write data
  ncvar_put(nc_, var_Data, ary_Global)

  # Add global attributes
  ncatt_put(nc_, 0, "institution", "Ruhr University Bochum")
  ncatt_put(nc_, 0, "history", paste("Created", Sys.Date()))

  # Close the file
  nc_close(nc_)
}
