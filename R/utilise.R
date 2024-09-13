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
