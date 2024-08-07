#' Calculate Water Temperature
#'
#' This function reads air temperature data, applies a transformation, and writes water temperature data.
#' @description
#' \loadmathjax
#'
#' \mjsdeqn{T_{water} = \frac{c_0}{1 + \mathrm{exp}(c_1 * T_{air} + c_2)}}
#'
#' where \mjseqn{T_{air}} and \mjseqn{T_{water}} in °C
#'
#' @param fn_T_Air The file name of the air temperature data.
#' @param fn_T_Water The file name to save the water temperature data.
#' @param num_T_Air_Fix,num_T_Water_Fix (numeric) Parameters to adjust the air temperature (default is 0.01) and water temperature (default is 1).
#' @param num_Param_C0,num_Param_C1,num_Param_C2 (numeric) Parameter for the transformation formula (default is c0 = 32, c1 = -0.132, c2 = 1.997).
#' @export
water_temperature_UNF <- function(fn_T_Air, fn_T_Water,
                           num_T_Air_Fix = 0.01,
                           num_T_Water_Fix = 1,
                           num_Param_C0 = 32, num_Param_C1 = -0.132, num_Param_C2 = 1.997) {

  mat_T_Air <- read_UNF(fn_T_Air)

  mat_T_water <-  num_Param_C0 / (1 + exp(num_Param_C1 * mat_T_Air * num_T_Air_Fix + num_Param_C2))

  write_UNF(mat_T_water * num_T_Water_Fix, fn_T_Water)
}
