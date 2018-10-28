#' @import data.table
#' @import plyr
#' @importFrom lubridate ymd
NULL
#' Convert csv data files into wq formats
#' @param measure.vars vector of measure variables. Can be integer (corresponding measure column numbers)
#' or character (measure column names) vector. If missing, all non-id columns will be assigned to it.
#' @param src.dt A \code{data.table} object. If a \code{data.frame} object is provided, it will be
#' converted to a \code{data.table} object using \link[data.table]{setDT}.
#' @param path Character. A character indicating the path of the \code{.wq} files to be saved.
#' @param start.date Character. A character of suspected date
#' @param end.date Character. A character of suspected date
#' @export

dt_to_wq <- function(measure.vars, src.dt, path, start.date, end.date){
  if(!is.data.table(src.dt)){
    setDT(src.dt)
  }
  if ("Date" %in% colnames(src.dt)){
    src.dt <- src.dt[Date %between% c(lubridate::ymd(start.date),lubridate::ymd(end.date)), .SD, by = Date]
  } else {
    warning("No Date column found! Writing all data.")
  }
  all.char <- all(is.character(measure.vars))
  all.num <- all(is.numeric(measure.vars))
  if(all.char){
    tar.dt <- src.dt[, ..measure.vars]
    plyr::a_ply(measure.vars,
                .margins = 1,
                .fun = write_wq,
                src.dt_ = tar.dt,
                path_ = path)
  }
  if(all.num){
    tar.dt <- src.dt[, ..measure.vars]
    measure.vars.names <- colnames(src.dt)[measure.vars]
    plyr::a_ply(measure.vars.names,
                .margins = 1,
                .fun = write_wq,
                src.dt_ = tar.dt,
                path_ = path)
  }
  if ((!all.char) & (!all.num)){
    stop("Unexpected measure.vars!")
  }
}
NULL
#' Write .wq files
#' @param measure.var_ vector of measure variables.
#' @param src.dt_ A \code{data.table} object.
#' @param path_ Character. A character indicating the path of the \code{.wq} files to be saved.
write_wq <- function(measure.var_, src.dt_, path_){
  dst.file <- file.path(path_, paste0(measure.var_, ".wq"))
  write(paste(NROW(src.dt_[, ..measure.var_]), measure.var_), file = dst.file)
  write.table(src.dt_[, ..measure.var_],
              file = dst.file,
              sep = " ",
              append = T,
              row.names = seq(0, NROW(src.dt_[, ..measure.var_]) - 1),
              col.names = F,
              quote = F)
}

