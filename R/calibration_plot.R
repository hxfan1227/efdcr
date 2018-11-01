#'@import ggplot2
#'@import data.table
#'@importFrom data.table :=
#'@importFrom readr read_lines 
#'@importFrom lubridate ddays ymd
#'@importFrom stringr str_extract_all str_split_fixed
#'@importFrom plyr rbind.fill
NULL
#' Generate calibration plots for EFDC exported data.
#' @param fname Character. The name of the \code{.dat} file exported by EFDC.
#' @param ts_num Number. The number of the time series to plot.
#' @param ... Not used.
#' @export

calibration_plot <- function(fname, ts_num, ...){
  water_elevation_file_header <- read_lines(fname, skip = 11, n_max = 1)
  base_date <- ymd(str_extract_all(water_elevation_file_header, '\\d{4}/\\d{1,2}/\\d{1,2}', simplify = T)[1,1])
  data_list <- list()
  data_to_read <- 0
  for (i in 1:ts_num){
    data_label <- str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 2]
    data_length <- as.numeric(str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 1])
    dt <- data.table::fread(fname, skip = 13 + data_to_read + i, header = F, nrows = data_length, col.names = c('RefDate', 'Value'))
    dt[, Type := data_label]
    data_list[[i]] <-  dt
    data_to_read <- data_to_read + data_length
  }
  calibration_dt <- data.table::setDT(rbind.fill(data_list))
  calibration_dt[, Date := base_date + ddays(RefDate)]
  p <- ggplot2::ggplot(calibration_dt, ggplot2::aes(x = Date, y = Value, color = Type)) + ggplot2::geom_line()
  p
}

# library(ggplot2)
# library(data.table)
# library(plyr)
# library(tidyverse)
# library(lubridate)
# fname <- 'F:/TS_Cal004_WaterEl_Kangshan.dat'
# water_elevation_file_header <- read_lines(fname, skip = 11, n_max = 1)
# base_date <- ymd(str_extract_all(water_elevation_file_header, '\\d{4}/\\d{1,2}/\\d{1,2}', simplify = T)[1,1])
# base_year <- year(base_date)
# ts_number <- 2
# data_list <- list()
# data_to_read <- 0
# for (i in 1:ts_number){
#   data_label <- str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 2]
#   data_length <- as.numeric(str_split_fixed(read_lines(fname, skip = 12 + data_to_read + i, n_max = 1), '\t', 2)[1, 1])
#   dt <- fread(fname, skip = 13 + data_to_read + i, header = F, nrows = data_length, col.names = c('RefDate', 'Value'))
#   dt[, Type := data_label]
#   data_list[[i]] <-  dt
#   data_to_read <- data_to_read + data_length
# }
# 
# a <- setDT(rbind.fill(data_list))
# a
# a[, Date := base_date + ddays(RefDate)]
# a
# ggplot(a, aes(x = Date, y = Value, color = Type)) + geom_line()
# b <- a[Date >= ymd('2010-06-01') & Date <= ymd_hms('2013-11-01-23-59-59')]
# nse(sim = b[Type == 'Kangshan-Model', Value], obs = b[Type == 'Kangshan-Data', Value])
