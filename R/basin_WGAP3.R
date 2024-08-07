#' Get the basin of one Cell with GCRC Number
#'
#' @param idx_Continent (string) Index of continents from ("eu", "af","as","au","na","sa")
#' @param num_GCRC (int) GCRC number
#' @importFrom terra aggregate
#' @export
basin_GCRC <- function(idx_Continent, num_GCRC) {
  vect_Cell <- lst_Mama_Cell[[idx_Continent]] |> vect()
  lst_Inflow_GCRC <- c(lst_Inflow_GCRC1, lst_Inflow_GCRC2, lst_Inflow_GCRC3)
  aggregate(vect_Cell[lst_Inflow_GCRC[[idx_Continent]][[num_GCRC]]])
}

