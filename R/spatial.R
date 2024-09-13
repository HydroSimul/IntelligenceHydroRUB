#' Aggregate Raster Data
#'
#' This function aggregates raster data using a specified factor and function,
#' and fills NA values with a specified value.
#'
#' @param rast_Data A raster object containing the data to be aggregated.
#' @param int_Fact An integer specifying the aggregation factor. Default is 10.
#' @param fct_Aggre A function to be used for aggregation. Default is `mean`.
#' @param fill_NA A value to fill in for NA values in the aggregated raster.
#'        Default is 0.
#'
#' @return A raster object of the aggregated data with NA values replaced.
#'
#' @importFrom terra aggregate rast crop extend
#' @export
aggregate_wg3 <- function(rast_Data, int_Fact = 10, fct_Aggre = mean, fill_NA = 0) {
  rast_Global <- rast(rast_Global_WG3)
  rast_Data <- crop(rast_Data, ext(rast_Global)) |> extend(ext(rast_Global))
  rast_Aggre <- aggregate(rast_Data, int_Fact, fct_Aggre)
  rast_Aggre[is.na(rast_Aggre)] <- fill_NA

  rast_Aggre
}

