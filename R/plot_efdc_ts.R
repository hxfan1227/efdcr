#'@import ggplot2
#'@importFrom ggplot2 %+replace%
#'@import data.table
#'@import hydroGOF
#'@importFrom data.table :=
#'@importFrom readr read_lines 
#'@importFrom lubridate ddays
#'@importFrom stringr str_extract_all str_split_fixed
#'@importFrom plyr rbind.fill
#'@importFrom hydroGOF gof
NULL
#' Generate calibration plots for EFDC exported data.
#' @param fname Character. The name of the \code{.dat} file exported by EFDC.
#' @param ts_num Numeric. The number of the time series to plot.
#' @param draw Logical. If \code{TRUE}, draw the figure.
#' @param n_col Numeric. The number of the facet column.
#' @param obs_col Character. The colour of the observed data.
#' @param mod_col Character. The colour of the modeled data.
#' @param obs_shape Numeric. Shape of the observed data.
#' @param mod_size Numeric. (Line) Size of the modeled data.
#' @param y_lab Character. Label of the Y axis.
#' @param x_lim Date. Limits for x axis.
#' @param y_lim Numeric. Limits for y axis.
#' @param id_level Character. The level of IDs.
#' @param date_interval Increment of the date sequence.
#' @param ... Not used.
#' @export
plot_efdc_ts <- function(fname, 
                         ts_num, 
                         draw = T, 
                         n_col = 1,
                         obs_col = 'red',
                         mod_col = 'black',
                         obs_shape = 1,
                         mod_size = 0.8,
                         y_lab = NULL,
                         x_lim = NULL, 
                         y_lim = NULL,
                         id_level,
                         date_interval = '1 day',
                         ...){
  file_header <- read_lines(fname, skip = 11, n_max = 1)
  base_date <- as.Date(str_extract_all(file_header, '\\d{4}/\\d{1,2}/\\d{1,2}', simplify = T)[1,1])
  xlim_ <- x_lim
  ylim_ <- y_lim
  data_list <- list()
  data_to_read <- 0
  for (i in 1:ts_num){
    data_label <- str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 2]
    id_ <- str_split_fixed(data_label, '-', 2)[1, 1]
    type_ <- str_split_fixed(data_label, '-', 2)[1, 2]
    if (type_ != 'Data'){
      type_ <- 'Model'
    }
    data_length <- as.numeric(str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 1])
    dt <- data.table::fread(fname, skip = 13 + data_to_read + i, header = F, nrows = data_length, col.names = c('RefDate', 'Value'))
    dt[, ':='(ID   = as.factor(id_), 
              Type = type_)]
    data_list[[i]] <-  dt
    data_to_read <- data_to_read + data_length
  }
  calibration_dt <- data.table::setDT(rbind.fill(data_list))
  calibration_dt <- calibration_dt[, Date := base_date + ddays(RefDate)]
  if(!is.null(x_lim)){
    xlim_ <- lubridate::ymd(x_lim)
    xlim_ <- as.POSIXct(xlim_)
    dates_ <- seq(min(as.Date(xlim_)), max(as.Date(xlim_)), by = date_interval)
  } else {
    dates_ <- seq(min(as.Date(calibration_dt$Date)), max(as.Date(calibration_dt$Date)), by = date_interval)
  }
  if(!missing(id_level)){
    calibration_dt$ID <- factor(calibration_dt$ID, levels = id_level)
  }
  mod_dt <- calibration_dt[Type != 'Data']
  obs_dt <- calibration_dt[Type == 'Data']
  metrics_dt <- data.table::dcast(calibration_dt[as.Date(Date) %in% dates_, date := as.Date(Date)], ID + date ~ Type, value.var = 'Value', fun.aggregate = mean)
  metrics_dt <-  metrics_dt[, .(Metrics = as.numeric(hydroGOF::gof(Model, Data)),
                                Variable = rownames(hydroGOF::gof(Model, Data))), by = .(ID)]
  metrics_dt <- dcast(metrics_dt, ID ~Variable, value.var = 'Metrics')
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = obs_dt, ggplot2::aes(x = Date, y = Value, color = 'Observation'), shape = obs_shape) +
    ggplot2::geom_line(data = mod_dt, ggplot2::aes(x = Date, y = Value, color = 'Model'), size = mod_size) +
    ggplot2::facet_wrap(~ID, ncol = n_col, scales = 'free') +
    ggplot2::scale_x_datetime(date_labels = '%Y-%m') +
    scale_color_manual('', values = c('Observation' = obs_col, 'Model' = mod_col)) +
    ylab(y_lab)
  p <- p + ggplot2::coord_cartesian(xlim = xlim_, ylim = ylim_)
  if (draw) {
    plot(p)
    invisible(list(fig = p, metrics = metrics_dt))
  } else {
    invisible(list(data = calibration_dt, metrics = metrics_dt))
  }
}

