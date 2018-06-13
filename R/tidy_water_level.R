#' @import XLConnect
#' @import XLConnectJars
#' @import data.table
#' @importFrom stringr str_detect
NULL
#' Tidy the observed water level data
#' @param x Character. Full name the water level file to be handled.
#' @export
tidy_water_level <- function(x){
  fname <- x
  wb <- XLConnect::loadWorkbook(fname)
  dt <- XLConnect::readWorksheet(wb, sheet = 1, startRow = 6, startCol = 3, endRow = 36, endCol = 14, header = F, colTypes = XLConnect::XLC$DATA_TYPE.STRING)
  data.table::setDT(dt)
  return(dt[, lapply(.SD, efdcr::col_format_water_level)])
}

#' Tidy one column of water level data (Just for batch processing).
#' @param x Character. One column of water level data.
#' @export
col_format_water_level <- function(x){
  id <- data.table::rleid(stringr::str_detect(x, "\\."))
  result.col <- rep(NA, length(id))
  result.col[1] <- x[1]
  int.part <- trunc(as.numeric(x[1]))
  id.idx <- 2
  for (i in 2:length(id)) {
    if (is.na(x[i])) {
      next
    } else{
      if (id[i] == id.idx){
        result.col[i] <- int.part + as.numeric(x[i]) / 100
      } else {
        id.idx <- id[i + 1]
        int.part <- trunc(as.numeric(x[i]))
        result.col[i] <- as.numeric(x[i])
      }
    }
  }
  return(as.numeric(result.col))
}
