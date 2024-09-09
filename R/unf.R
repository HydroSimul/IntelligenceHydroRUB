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
  fct_AsType <- c(as.numeric, as.integer, as.integer, "", as.integer, "", "", "", "", as.numeric)[[mark_UNF + 1]]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  writeBin(fct_AsType(data_Export), fn_UNF, n_Byte, endian = "big")
}



analyse_file_unf <- function(fn_UNF) {
  mark_UNF <- as.integer(str_sub(fn_UNF, -1))
  type_UNF <- c("numeric", "int", "int", "", "integer", "", "", "", "", "double")[mark_UNF + 1]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  n_Dim1 <- 1
  str_MatrxDot <- str_sub(fn_UNF, -8, -8)
  str_MatrxDot2 <- str_sub(fn_UNF, -7, -7)
  str_MatrixN <- str_sub(fn_UNF, -7, -6)
  str_MatrixN2 <- str_sub(fn_UNF, -6, -6)
  if_Matrix <- str_MatrxDot == "." & str_MatrixN %in% c("12", "31", "26")
  if_Matrix2 <- str_MatrxDot2 == "." & str_MatrixN2 %in% c("8", "9")
  if(if_Matrix) n_Dim1 <- as.integer(str_MatrixN)
  if(if_Matrix2) n_Dim1 <- as.integer(str_MatrixN2)
  return(list(type_UNF = type_UNF, n_Byte = n_Byte, n_Dim1 = n_Dim1))
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



#' @rdname unf
#' @param num_Data vector (1D), data, that read from UNF-file and order by the GCRC-Number
#' @param idx_Continent string in ("eu", "af", "as", "au", "na", "sa", "global_wg2")
#' @importFrom terra values
#' @export
unf_2_raster <- function(idx_Continent, num_Data) {

  check_n_cell(idx_Continent, length(num_Data))

  rast_Mask <- lst_rast_Mask_WaterGAP3[[idx_Continent]] |> rast()
  vct_GCRC <- values(rast_Mask)
  idx_NotNA <- which(!is.na(vct_GCRC))
  idx_GCRC <- vct_GCRC[idx_NotNA]
  values(rast_Mask)[idx_NotNA] <- num_Data[idx_GCRC]

  rast_Mask
}


#' @rdname unf
#' @param lst_num_Data list of vector, data, that read from UNF-file and order by the GCRC-Number,
#' the list must named as ("eu", "af", "as", "au", "na", "sa") two or more
#' @importFrom terra merge
#' @importFrom purrr map2 reduce
#' @export
unf_2_raster_merge <- function(lst_num_Data) {
  str_Continent <- names(lst_num_Data)
  map2(str_Continent, lst_num_Data, unf_2_raster) |>
    reduce(merge)
}


