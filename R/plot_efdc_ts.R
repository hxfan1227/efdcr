#'@import ggplot2
#'@importFrom ggplot2 %+replace%
#'@import data.table
#'@importFrom data.table :=
#'@importFrom readr read_lines 
#'@importFrom lubridate ddays
#'@importFrom stringr str_extract_all str_split_fixed
#'@importFrom plyr rbind.fill
NULL
#' Generate calibration plots for EFDC exported data.
#' @param fname Character. The name of the \code{.dat} file exported by EFDC.
#' @param ts_num Number. The number of the time series to plot.
#' @param draw Logical. If \code{TRUE}, draw the figure.
#' @param obs_col Character. The colour of the observed data.
#' @param mod_col Character. The colour of the modeled data.
#' @param y_lab Character. Label of the Y axis.
#' @param free_scale Logical. \code{TRUE} set free scale when facetting.
#' @param ... Not used.
#' @export
plot_efdc_ts <- function(fname, 
                             ts_num, 
                             draw = T, 
                             obs_col = 'red',
                             mod_col = 'black',
                             y_lab = NULL,
                             free_scale = T,
                             ...){
  file_header <- read_lines(fname, skip = 11, n_max = 1)
  base_date <- as.Date(str_extract_all(file_header, '\\d{4}/\\d{1,2}/\\d{1,2}', simplify = T)[1,1])
  data_list <- list()
  data_to_read <- 0
  for (i in 1:ts_num){
    data_label <- str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 2]
    id_ <- str_split_fixed(data_label, '-', 2)[1, 1]
    type_ <- str_split_fixed(data_label, '-', 2)[1, 2]
    data_length <- as.numeric(str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 1])
    dt <- data.table::fread(fname, skip = 13 + data_to_read + i, header = F, nrows = data_length, col.names = c('RefDate', 'Value'))
    dt[, ':='(ID   = id_, 
              Type = type_)]
    data_list[[i]] <-  dt
    data_to_read <- data_to_read + data_length
  }
  calibration_dt <- data.table::setDT(rbind.fill(data_list))
  calibration_dt <- calibration_dt[, Date := base_date + ddays(RefDate)]
  mod_dt <- calibration_dt[Type != 'Data']
  obs_dt <- calibration_dt[Type == 'Data']
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = obs_dt, ggplot2::aes(x = Date, y = Value, color = 'Observation'), shape = 1) +
    ggplot2::geom_line(data = mod_dt, ggplot2::aes(x = Date, y = Value, color = 'Model'), size = 0.8) +
    ggplot2::facet_wrap(~ID, ncol = 1, scales = ifelse(free_scale, 'free', 'fixed')) +
    ggplot2::scale_x_datetime(date_labels = '%Y-%m') +
    scale_color_manual('', values = c('Observation' = obs_col, 'Model' = mod_col)) +
    ylab(y_lab)
  if (!free_scale) {
    p <- p + ggplot2::coord_cartesian(xlim = c(min(mod_dt$Date), max(mod_dt$Date)), ylim = c(min(calibration_dt$Value), max(calibration_dt$Value)))
  } else {
    p <- p + ggplot2::coord_cartesian(xlim = c(min(mod_dt$Date), max(mod_dt$Date))) 
  }
  if (draw) {
    return(p)
  } else {
    return(calibration_dt)
  }
}

                 