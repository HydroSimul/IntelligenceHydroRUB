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
