get_vect_mama <- function(idx_Continent) {
  switch(idx_Continent,
         eu = vect_Mama_Cell_eu,
         af = vect_Mama_Cell_af,
         as = vect_Mama_Cell_as,
         au = vect_Mama_Cell_au,
         na = vect_Mama_Cell_na,
         sa = vect_Mama_Cell_sa,
         stop('Invalid continent, make sure in ("eu", "af","as","au","na","sa")'))
}


check_n_cell <- function(idx_Continent, n_Cell) {
  # n_Cell_Conti <- c(180721, 371410, 841703, 109084, 461694, 226852, 70412)
  # names(n_Cell_Conti) <- c("eu", "af", "as", "au", "na", "sa", "global_wg2")
  if (n_Cell_Conti[idx_Continent] != n_Cell) {
    stop(paste0("The continent ", idx_Continent, " must have ",
                n_Cell_Conti[idx_Continent], " cells, but the data have ",
                n_Cell, " cells."))
  }

}




check_lst_name <- function(name_Var, name_List) {
  str_Continent <- c("eu", "af", "as", "au", "na", "sa")


  if (!all(str_Continent %in% name_List)) {
    stop(paste0("The data ", name_Var, " must contain ",
                '"eu", "af", "as", "au", "na", "sa" six data.'))
  }

}
