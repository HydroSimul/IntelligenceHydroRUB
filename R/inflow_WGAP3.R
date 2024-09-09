#' Get the inflow Cells with GCRC Number
#'
#' @param idx_Continent (string) Index of continents from ("eu", "af","as","au","na","sa")
#' @param num_GCRC (int) GCRC number
#' @return (vector of int) Inflow cells of every this WaterGAP3 Cell
#' @export
inflow_cell_GCRC <- function(idx_Continent, num_GCRC) {

  int_Inflow_GCRC <- switch(idx_Continent,
                            eu = int_Inflow_GCRC_eu,
                            af = int_Inflow_GCRC_af,
                            as = int_Inflow_GCRC_as,
                            au = int_Inflow_GCRC_au,
                            na = int_Inflow_GCRC_na,
                            sa = int_Inflow_GCRC_sa,
                            stop('Invalid continent, make sure in ("eu", "af","as","au","na","sa")'))
  int_Inflow_GCRC[[i]]
}

