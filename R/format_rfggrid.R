#' Format Delft3D RFGGRID (.grd) file.
#' @param src_grd Character. Full name of the Delft3D RFGGRID (.grd) file to be formatted.
#' @param dst_grd Character. Full name of the formatted Delft3D RFGGRID (.grd) file.
#' @export
format_rfggrid <- function(src_grd, dst_grd)
{
  require(stringr)
  frgfile <- file(src_grd, "r")
  rfg.in <- readLines(frgfile)
  close(frgfile)
  missing.val <- stringr::str_split_fixed(rfg.in[6], " ", 12)[12]
  missing.val.split <- stringr::str_split_fixed(missing.val, "\\+", 2)
  missing.val.pattern <- paste0(missing.val.split[1], "\\+", missing.val.split[2])
  rfg.out <- file(dst_grd, "a")
  for (i in 1:5) writeLines(rfg.in[i], rfg.out)
  for (i in 7:length(rfg.in)) writeLines(do.call(paste, as.list(stringr::str_replace(str_split(rfg.in[i], " ")[[1]], pattern = missing.val.pattern, "0"))), rfg.out)
  close(rfg.out, "rw")
}
