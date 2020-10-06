#' Convert ESRI shapefile into text format.
#' @param src_shp Character. Full name of the shapfile to be converted.
#' @param dst_ldb Character. Full name of the txt file to write.
#' @return Either a list of \code{NULL} or error messages.
#' @export

shp_to_ldb <- function(src_shp, dst_ldb)
{

  shp_file <- raster::shapefile(src_shp)
  shp_df <- broom::tidy(shp_file)[, c("id", "long", "lat")]
  shp_df_split <- split(shp_df, factor(.subset2(shp_df, "id"), levels = unique(.subset2(shp_df, "id"))))
  out <- lapply(shp_df_split, write_ldb, ldb_file = dst_ldb)
  return(out)
}

#' Function for write .ldb files.
#' @param x the data.frame to write.
#' @param ldb_file the .ldb file to write with.
#' @return NULL
write_ldb <- function(x, ldb_file)  # Internal function
{
  names(x) <- NULL
  write(paste0("L", unique(x[, 1])), file = ldb_file, append = T)
  write(paste(NROW(x), NCOL(x)), file = ldb_file, append = T)
  write.table(x[, -1], file = ldb_file, append = T, row.names = F, col.names = F)
}


