#' Convert ESRI shapefile into text format.
#' @param src_shp Character. Full name of the shapfile to be converted.
#' @param dst_txt Character. Full name of the txt file to write.
#' @param cols Numeric. Vector of column numbers to be extracted.
#' @examples \dontrun{
#' shp_to_txt(src_shp = system.file("extdata/pyl_utm50n.shp"),
#' dst_txt = tempfile(fileext = ".txt"))
#' }
shp_to_txt <- function(src_shp, dst_txt, cols = c(5, 1, 2))
{
  shp_file <- raster::shapefile(src_shp)
  shp_df <- broom::tidy(shp_file)[, cols]
  write.table(shp_df, file = dst_txt, row.names = F, col.names = F, quote = F)
}

