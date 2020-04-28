#' @import data.table
#' @importFrom plyr alply
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract_all
#' @importFrom purrr map_int
NULL
#' Set water quality boundaries (cwqsrs01-21.inp)
#' @param wq_path Character. Path of the water quality files (\code{*.wq}). The \code{.wq} files can
#' be generated using \link[efdcr]{dt_to_wq}.
#' @param cwqsrs_path Character. Path to store the cwqsrsXX.inp files.
#' @export
set_wqbc <- function(wq_path, cwqsrs_path){
  cwqsrs <- file.path(cwqsrs_path, c(paste0('cwqsr', 0, 1:9, '.inp'), paste0('cwqsr', 10:21, '.inp')))
  
  wq_flist <- plyr::alply(wq_abbs, 
                          1, 
                          function(x, path){
                            list.files(path = path, pattern = paste0(paste0('^', x), '.*\\.wq$'), full.names = T)
                          },
                          path = wq_path)
  empty_data <- data.table::fread(wq_flist[[which(purrr::map_int(wq_flist, length) != 0)[1]]][1])
  empty_data[, 2] <- 0
  station_names <- stringr::str_sub(stringr::str_extract_all(wq_flist[[which(map_int(wq_flist, length) != 0)[1]]], '_.*\\.'), 2, -2)
  for (i in 1:21) {
    writeLines(cwqsrs_h[[i]], cwqsrs[i])
    if (length(wq_flist[[i]]) == 0){
      for (j in 1:length(station_names)) {
        L1 <- paste(1, colnames(empty_data)[1], 86400, 0, 1, 0, '!', station_names[j])
        write(L1, cwqsrs[i], append = T)
        write('1.0', cwqsrs[i], append = T)
        fwrite(empty_data, file = cwqsrs[i], col.names = F, append = T, sep = ' ')
      }
      next()
    }
    for (j in 1:length(station_names)) {
      data_ <- fread(wq_flist[[i]][j])
      L1 <- paste(1, colnames(data_)[1], 86400, 0, 1, 0, '!', station_names[j])
      write(L1, cwqsrs[i], append = T)
      write('1.0', cwqsrs[i], append = T)
      fwrite(data_, file = cwqsrs[i], col.names = F, append = T, sep = ' ')
    }
  }
}





