#' Aggregate a 3D Array Along Specified Dimensions
#'
#' Aggregates a 3D array by reducing its resolution based on a specified aggregation factor.
#'
#' @param ary_Data 3D (time, x, y) numeric array to be aggregated. The first must be time dimension.
#' @param n_Aggregate An integer specifying the factor by which to aggregate the array along dimensions 2 and 3.
#' @param fct_Aggregate A function to be applied to each aggregated block (default is `mean`).
#' @param ... Additional arguments to be passed to `fct_Aggregate`.
#'
#' @return A 3D numeric array with reduced dimensions after aggregation.
#' @export
aggregate_3d <- function(ary_Data, n_Aggregate, fct_Aggregate = mean, ...) {

  n_Dim <- dim(ary_Data)

  # Ensure the dimensions are divisible by the aggregation factor
  if (n_Dim[2] %% n_Aggregate != 0 || n_Dim[3] %% n_Aggregate != 0) {
    stop("The dimensions of ary_Data must be divisible by n_Aggregate.")
  }

  # Reshape the array to prepare for aggregation
  dim(ary_Data) <- c(n_Dim[1], n_Aggregate, n_Dim[2] / n_Aggregate, n_Aggregate, n_Dim[3] / n_Aggregate)

  # Apply the aggregation function
  apply(ary_Data, c(1, 3, 5), fct_Aggregate, ...)
}
