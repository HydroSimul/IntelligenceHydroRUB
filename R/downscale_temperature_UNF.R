#' Downscale temperature using UNF data
#'
#' @param idx_Continent (string) Index of continents from ("eu", "af","as","au","na","sa")
#' @param fn_GCRC_Ref (string) File path to GCRC of original temprature data
#' @param fn_Temp_Ref (string) File path to temperature data
#' @param fn_Write (string) File path to write downscaled temperature data
#' @param num_Factor (num) Scaling factor for temperature correction
#' @importFrom dplyr %>% group_by mutate ungroup
#' @export
downscale_temperature_UNF <- function(idx_Continent, fn_GCRC_Ref, fn_Temp_Ref, fn_Write, num_Factor = 100) {

  # Read and process elevation data
  num_Elev_5min <- lst_Elev_5min[[idx_Continent]]
  int_GCRC_05grad <- read_UNF(fn_GCRC_Ref)
  # mean elevation
  df_Elev <- data.frame(GCRC_Map = int_GCRC_05grad, Elev = num_Elev_5min)
  df_Elev <- df_Elev %>%
    group_by(GCRC_Map) %>%
    mutate(Elev_Map = mean(Elev, na.rm = TRUE)) %>%
    ungroup()
  # Temerature factor
  df_Elev$Temp_Fix <- (df_Elev$Elev_Map - df_Elev$Elev) * 0.006 * num_Factor

  # Read temperature reference data
  mat_Temp_Ref <- t(read_UNF(fn_Temp_Ref))

  # Perform temperature correction
  mat_Temp_Korrect <- mat_Temp_Ref[df_Elev$GCRC_Map, ] + df_Elev$Temp_Fix

  # Write downscaled temperature data
  write_UNF(t(mat_Temp_Korrect), fn_Write)
}
