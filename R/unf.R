#' Data im- & export in UNF file
#' @name unf
#' @description
#' # UNF Format
#' - UNF0 / UNF8: **numeric** with 4 / 8 Bytes,
#' - UNF1, UNF2 / UNF4: **integer** with 1, 2, / 4 Bytes
#' # Read & Write
#' - Read data from UNF file
#' - Write data to UNF file
#' @param fn_UNF string, name of the UNF file
#' @importFrom stringr str_sub
#' @return vector of numric or integer by read, UNF file by write
#' @export
read_UNF <- function(fn_UNF) {
  info_UNF <- analyse_file_unf(fn_UNF)

  n_FileSize <- file.size(fn_UNF)
  n_DataSize <- n_FileSize / info_UNF$n_Byte

  ary_Read <- readBin(fn_UNF, info_UNF$type_UNF, n_DataSize, info_UNF$n_Byte, endian = "big")
  if(info_UNF$n_Dim1 > 1) dim(ary_Read) <- c(info_UNF$n_Dim1, n_DataSize / info_UNF$n_Dim1)
  ary_Read

}

#' @rdname unf
#' @importFrom RCurl scp
#' @param ip_Host string, the name of the remote host or its IP address
#' @param str_Username string, the name of the user on the remote machine
#' @param str_Password string, a password for accessing the local SSH key
#' @export
read_UNF_scp <- function(fn_UNF, ip_Host, str_Username, str_Password) {
  info_UNF <- analyse_file_unf(fn_UNF)

  raw_SCP <- scp(ip_Host, fn_UNF, password = str_Password, user = str_Username)
  n_FileSize <- length(raw_SCP)
  n_DataSize <- n_FileSize / info_UNF$n_Byte

  ary_Read <- readBin(raw_SCP, info_UNF$type_UNF, n_DataSize, info_UNF$n_Byte, endian = "big")
  if(info_UNF$n_Dim1 > 1) dim(ary_Read) <- c(info_UNF$n_Dim1, n_DataSize / info_UNF$n_Dim1)
  ary_Read

}

#' @rdname unf
#' @param data_Export vector, data, that would exported
#' @export
write_UNF <- function(data_Export, fn_UNF) {
  mark_UNF <- as.integer(str_sub(fn_UNF, -1))
  fct_AsType <- c(as.numeric, as.integer, as.integer, "", as.integer, "", "", "", as.numeric)[[mark_UNF + 1]]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  writeBin(fct_AsType(data_Export), fn_UNF, n_Byte, endian = "big")
}



analyse_file_unf <- function(fn_UNF) {
  mark_UNF <- as.integer(str_sub(fn_UNF, -1))
  type_UNF <- c("numeric", "int", "int", "", "integer", "", "", "", "double")[mark_UNF + 1]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  n_Dim1 <- 1
  str_MatrxDot <- str_sub(fn_UNF, -8, -8)
  str_MatrxDot2 <- str_sub(fn_UNF, -7, -7)
  str_MatrixN <- str_sub(fn_UNF, -7, -6)
  str_MatrixN2 <- str_sub(fn_UNF, -6, -6)
  if_Matrix <- str_MatrxDot == "." & str_MatrixN %in% c("12", "31", "26", "20")
  if_Matrix2 <- str_MatrxDot2 == "." & str_MatrixN2 %in% c("8", "9")
  if(if_Matrix) n_Dim1 <- as.integer(str_MatrixN)
  if(if_Matrix2) n_Dim1 <- as.integer(str_MatrixN2)
  return(list(type_UNF = type_UNF, n_Byte = n_Byte, n_Dim1 = n_Dim1))
}





#' @rdname unf
#' @param num_Data vector (1D), data, that read from UNF-file and order by the GCRC-Number
#' @param idx_Continent string in ("eu", "af", "as", "au", "na", "sa", "global_wg2")
#' @importFrom terra values
#' @export
unf_2_raster <- function(num_Data, idx_Continent) {

  check_n_cell(idx_Continent, length(num_Data))

  rast_Mask <- lst_rast_Mask_WaterGAP3[[idx_Continent]] |> rast()
  values(rast_Mask)[lst_idx_NotNA[[idx_Continent]]] <- num_Data[lst_idx_GCRC[[idx_Continent]]]

  rast_Mask
}


#' @rdname unf
#' @param lst_num_Data list of vector, data, that read from UNF-file and order by the GCRC-Number,
#' the list must named as ("eu", "af", "as", "au", "na", "sa") two or more
#' @importFrom terra merge
#' @importFrom purrr map2 reduce
#' @export
unf_2_raster_merge <- function(lst_num_Data) {
  check_lst_name("lst_num_Data", names(lst_num_Data))
  str_Continent <- names(lst_num_Data)
  map2(lst_num_Data, str_Continent, unf_2_raster) |>
    reduce(merge)
}



#' @rdname unf
#' @param lst_mat_Data list of matrix(time, GCRC), data, that read from UNF-file and order by time the GCRC-Number,
#' the list must named as ("eu", "af", "as", "au", "na", "sa") two or more
#' @param n_Time int, time length
#' @importFrom purrr map
#' @export
unf_2_ary_WG3 <- function(lst_fn_Data, n_Time) {

  check_lst_name("lst_fn_Data", names(lst_fn_Data))

  lst_mat_Data <- map(lst_fn_Data,
                      \(x) map(x, read_UNF) |> unlist())

  for (i in str_Continent) {
    dim(lst_mat_Data[[i]]) <- c(IntelligenceHydroRUB::n_Cell_Conti[i], n_Time)
  }

  lon <- seq(-180 + 5/60/2, 180, by = 5/60)  # Longitude: 4320 points
  lat <- seq(90 - 5/60/2, -90, by = -5/60)  # Latitude: 2160 points

  n_Lon <- length(lon)
  n_Lat <- length(lat)


  ary_Global <- matrix(NA, n_Time, n_Lon*n_Lat)
  str_Continent <- names(lst_mat_Data)

  for (i in str_Continent) {
    ary_Global[, lst_idx_NotNA_Global[[i]]] <- lst_mat_Data[[i]][lst_idx_GCRC_Global[[i]], ] |> t()
  }

  dim(ary_Global) <- c(n_Time, n_Lon, n_Lat)

  ary_Global
}





#' @rdname unf
#' @param rast_Data (terra::SpatRaster) global raster data and the resolution must be 5'
#' @importFrom terra values rast crop
#' @export
raster_2_unf <- function(rast_Data, idx_Continent) {
  rast_GCRC <- rast(lst_rast_Mask_WaterGAP3[[idx_Continent]])
  rast_Crop <- crop(rast_Data, rast_GCRC, mask = TRUE)
  num_Data <- values(rast_Crop)
  num_GCRC <- values(rast_GCRC)
  idx_GCRC <- !is.na(num_GCRC)
  (num_Data[idx_GCRC])[order(num_GCRC[idx_GCRC])]
}

#' @rdname unf
#' @param fct_Extract A function to apply to the extracted values. Default is `mean`.
#' @param fill_NA num, NA to replace
#' @importFrom terra crs project extract
#' @export
extract_unf <- function(rast_Data, idx_Continent, fct_Extract = mean, fill_NA = 0) {
  vect_Mask <- get_vect_mama(idx_Continent) |> vect()

  crs_Rast <- crs(rast_Data)
  crs_Mask <- crs(vect_Mask)
  if(crs_Rast != crs_Mask) {
    vect_Mask <- project(vect_Mask, crs_Rast)
  }

  num_Extract <- extract(rast_Data, vect_Mask, fct_Extract)
  num_Extract[is.na(num_Extract)] <- fill_NA

  num_Extract
}






