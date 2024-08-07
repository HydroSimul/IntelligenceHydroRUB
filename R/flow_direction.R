#' Some tools for flow direction / path / net
#'
#' @description
#' This package provides some tools for handling flow direction, path, and network-related calculations.
#'
#' @section Functions:
#' - `get_inflow_cells`: Get all the cells which will flow into a specified cell (including the cell itself).
#'
#' @export
#' @param num_FlowDir Integer. The cell number of the next cell. The cell number must range from 1 to the length of the cells.
#' If the cell has no outflow, the number should be set to itself.
#' @return A list of integers representing the cells that flow into the specified cell.
#' @examples
#' num_FD <- c(4,5,2,5,9,3,7,8,8)
#' lst_Cells <- get_inflow_cells(num_FD)
get_inflow_cells <- function(num_FlowDir) {

  n_Cell <- length(num_FlowDir)

  lst_Inflow <- vector("list", n_Cell)

  for (i in 1:n_Cell) {
    lst_Inflow[[i]] <- c(lst_Inflow[[i]], i)

    num_Ori <- i
    num_Next <-  num_FlowDir[i]

    while (num_Next != num_Ori) {
      lst_Inflow[[num_Next]] <- c(lst_Inflow[[num_Next]], i)

      num_Ori <- num_Next
      num_Next <- num_FlowDir[num_Next]

    }

  }

  lst_Inflow
}















