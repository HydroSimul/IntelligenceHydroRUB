#' Get the basin of one Cell with GCRC Number
#'
#' @param idx_Continent (string) Index of continents from ("eu", "af","as","au","na","sa")
#' @param num_GCRC (int / vector of int) GCRC number/-s
#' @importFrom terra aggregate vect
#' @return (terra::SpatVector) Basin(s) of every WaterGAP3 Cell
#' @export
basin_GCRC <- function(idx_Continent, num_GCRC) {

  vect_Cell <- switch(idx_Continent,
                      eu = vect_Mama_Cell_eu,
                      af = vect_Mama_Cell_af,
                      as = vect_Mama_Cell_as,
                      au = vect_Mama_Cell_au,
                      na = vect_Mama_Cell_na,
                      sa = vect_Mama_Cell_sa,
                      stop('Invalid continent, make sure in ("eu", "af","as","au","na","sa")'))
  int_Inflow_GCRC <- switch(idx_Continent,
                      eu = int_Inflow_GCRC_eu,
                      af = int_Inflow_GCRC_af,
                      as = int_Inflow_GCRC_as,
                      au = int_Inflow_GCRC_au,
                      na = int_Inflow_GCRC_na,
                      sa = int_Inflow_GCRC_sa,
                      stop('Invalid continent, make sure in ("eu", "af","as","au","na","sa")'))

  vect_Cell <- vect(vect_Cell)

  vect_Basin <- vect()

  for (i in num_GCRC) {
    vect_Basin <- rbind(vect_Basin, aggregate(vect_Cell[int_Inflow_GCRC[[i]]]))
  }


  crs(vect_Basin) <- "EPSG:4326"
  vect_Basin$GCRC <- num_GCRC
  vect_Basin$Continent <- idx_Continent
  vect_Basin
}

