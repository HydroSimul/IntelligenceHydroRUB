#' Linear Interpolation Between Two Vectors
#'
#' This function performs linear interpolation between two vectors
#' `num_Start` and `num_End` and returns the interpolated results in a matrix.
#'
#' @param num_Start A numeric vector representing the start points.
#' @param num_End A numeric vector representing the end points.
#' @param n_Inter An integer specifying the number of interpolation points.
#'
#' @return A matrix where each row corresponds to an interpolated point
#'         between `num_Start` and `num_End`.
#'
#' @details The function creates `n_Inter` equally spaced interpolations
#'          between the corresponding elements of `num_Start` and `num_End`.
#'
#' @examples
#' num_Start <- c(1, 5, 10)
#' num_End <- c(10, 15, 20)
#' n_Inter <- 5
#' interpolation_matrix <- linear_interpolate(num_Start, num_End, n_Inter)
#'
#' @export
linear_interpolate <- function(num_Start, num_End, n_Inter) {
  # Create a sequence of interpolation factors from 0 to 1
  factors <- seq(0, 1, length.out = n_Inter)

  # Perform interpolation for each factor and store the result in a matrix
  sapply(factors, \(f) (1 - f) * num_Start + f * num_End)

}
