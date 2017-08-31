#' @importFrom stringr str_replace
NULL
#' Format Delft3D RFGGRID (.grd) file.
#' @param src_grd Character. Full name of the Delft3D RFGGRID (.grd) file to be formatted.
#' @param dst_grd Character. Full name of the formatted Delft3D RFGGRID (.grd) file.
#' @param na_pattern Character. The MISSING VALUE in the src_grd.
#' @param na_repalcement Character. A character vector of replacements.
#' @param coord_sys Character. Coordinate system in the grd.
#' @export
#' @examples \dontrun{
#' f <- system.file("extdata/poyang_lake.grd", package="efdcr")
#' format_rfggrid(src_grd = f, dst_grd = tempfile(fileext = ".grd"))
#' }

format_rfggrid <- function(src_grd,
                           dst_grd,
                           na_pattern = "-9.99999000000000024E\\+02",
                           na_replacement = " 0.00000000000000000E+00",
                           coord_sys = "Cartesian")
{

  src_file <- file(src_grd, "r")
  src_char <- readLines(src_file)
  close(src_file)
  if(file.exists(dst_grd)){unlink(dst_grd)}
  dst_file <- file(dst_grd, "a")
  writeLines("* DS-International EFDC_Explorer Generated", dst_file)
  writeLines("* WL | Delft Hydraulics, Delft3D-RGFGRID Version 4.12.02.20; Apr 2005", dst_file)
  writeLines(paste("* File creation date:", format(Sys.time(), "%F %T")), dst_file)
  writeLines("*", dst_file)
  writeLines(paste("Coordinate System =", coord_sys), dst_file)
  writeLines(paste("Missing Value =", na_replacement), dst_file)
  for (i in 7:length(src_char)) writeLines(str_replace_all(src_char[i],
                                                           pattern = na_pattern,
                                                           replacement = na_replacement),
                                           dst_file
                                           )
  close(dst_file, "rw")
}
