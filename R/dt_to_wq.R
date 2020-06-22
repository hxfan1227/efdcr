#' @import plyr
#' @importFrom lubridate ymd
#' @importFrom utils write.table
#' @import data.table
NULL
#' Convert csv data files into wq formats
#' @param measure.vars vector of measure variables. Can be integer (corresponding measure column numbers)
#' or character (measure column names) vector. If missing, all non-id columns will be assigned to it.
#' @param src.dt A \code{data.table} object. If a \code{data.frame} object is provided, it will be
#' converted to a \code{data.table} object using \link[data.table]{setDT}.
#' @param path Character. A character indicating the path of the \code{.wq} files to be saved.
#' @param start.date Character. A character of desired date ('2000-01-01')
#' @param end.date Character. A character of desired date ('2000-01-01')
#' @param interval Character. Increment of the sequence. See \link[base]{seq.Date} for details.
#' @export

dt_to_wq <- function(measure.vars, src.dt, path, start.date, end.date, interval = '1 day'){
  Date = NULL
  if(!is.data.table(src.dt)){
    setDT(src.dt)
  }
  date.dt <- data.table(Date = seq.Date(lubridate::ymd(start.date), lubridate::ymd(end.date), by = interval))
  if ("Date" %in% colnames(src.dt)){
    src.dt <- src.dt[Date %between% c(lubridate::ymd(start.date),lubridate::ymd(end.date)), .SD]
  } else {
    warning("No Date column found! Writing all data.")
  }
  src.dt <- merge(date.dt, src.dt, by = 'Date', all = T)
  all.char <- all(is.character(measure.vars))
  all.num <- all(is.numeric(measure.vars))
  if(all.char){
    tar.dt <- src.dt[, measure.vars, with = F]
    plyr::a_ply(measure.vars,
                .margins = 1,
                .fun = write_wq,
                src.dt_ = tar.dt,
                path_ = path)
  }
  if(all.num){
    tar.dt <- src.dt[, measure.vars, with = F]
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
#' @export
write_wq <- function(measure.var_, src.dt_, path_){
  No = NULL
  dst.file <- file.path(path_, paste0(measure.var_, ".wq"))
  src.dt_[, No:= seq(0, NROW(src.dt_) - 1)]
  src.dt_ <- na.omit(src.dt_)
  write(paste(NROW(src.dt_[, measure.var_, with = F]), measure.var_), file = dst.file)
  write.table(src.dt_[, c('No', measure.var_), with = F],
              file = dst.file,
              sep = " ",
              append = T,
              col.names = F,
              row.names = F,
              quote = F,
              na = '')
}

