#' Find a specific GCRC location within a continent
#'
#' This function retrieves the information of a specific GCRC location within a given continent.
#'
#' @param str_Continent string, It must be one of "eu" (Europe), "af" (Africa), "as" (Asia), "au" (Australia), "na" (North America), or "sa" (South America).
#' @param id_GCRC integer, ID of the GCRC within the specified continent
#' @import rlang glue
#' @return A data frame containing the information of the specified GCRC location.
#' @export
#' @examples
#' # Example usage:
#' # Assuming lst_GCRC is a list containing information of GCRCs indexed by continent,
#' # and we want to retrieve the information of GCRC with ID 1 in continent "eu" (Europe):
#' # find_gcrc_location("eu", 1)
find_gcrc_location <- function(str_Continent, id_GCRC) {
  # pre test --------
  if(!(str_Continent %in% c("eu", "af","as","au","na","sa"))) abort(glue_col('{blue `str_Continent`} must be one of {green "eu"} (Europe), {green "af"} (Africa), {green "as"} (Asia), {green "au"} (Australia), {green "na"} (North America), or {green "sa"} (South America).'))
  id_Max <- nrow(lst_GCRC[[str_Continent]])
  id_MyMax <- max(id_GCRC)
  if(id_MyMax > id_Max) abort(glue_col('The {blue `id_GCRC`} {red {id_MyMax}}  is bigger than the maximal ID {green {id_Max}} in {blue "{str_Continent}"}.'))

  # main --------
  lst_GCRC[[str_Continent]][id_GCRC,]
}












