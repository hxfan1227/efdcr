NULL 
#' Get EFDC grids from lxly or dxly file and convert it to the sf object
#' @import data.table
#' @import stringr
#' @importFrom magrittr `%>%`
#' @importFrom readr read_lines
#' @import glue
#' @import sf
#' @import ncdf4
#' @param flxly Character. Name of the lxly.inp
#' @param fdxdy Character. Name of the dxdy.inp
#' @param fnc Character. Name of the \code{.nc} file.
#' @return an object of class \code{sf}, which is a classed list-column with simple feature geometries 
#' @export
get_efdc_grids_as_sf <- function(flxly, fdxdy, fnc) {
  if (!missing(fnc)){
    cat(glue::glue('Generate grids from {fnc}\n'))
    nc <- ncdf4::nc_open(fnc)
    lon_bnds <- ncdf4::ncvar_get(nc, 'lon_bnds')
    # longitudes of 4 corners of the grid
    lat_bnds <- ncdf4::ncvar_get(nc, 'lat_bnds')
    lon_bnds_melt <- efdcr::melt_nc(lon_bnds, var = 'lon_bnds', var_name = 'lon', nc = nc)
    lat_bnds_melt <- efdcr::melt_nc(lat_bnds, var = 'lat_bnds', var_name = 'lat', nc = nc)
    lon_lat_df <- merge(lon_bnds_melt, lat_bnds_melt, by = .EACHI)
    data.table::setDT(lon_lat_df)
    lon_lat_df <- na.omit(lon_lat_df)
    n_grids <- sum(!is.na(lon_bnds))/4
    lon_lat_df[order(col, row, cnr), id := rep(1:n_grids, each = 4)]
    sf::st_as_sf(lon_lat_df, coords = c('lon', 'lat')) %>%
      group_by(col, row) %>%
      summarise(geometry = sf::st_combine(geometry)) %>%
      sf::st_cast('POLYGON') %>%
      sf::st_set_crs(4326) -> grids
    ZBOT_dt <- ncdf4::ncvar_get(nc, varid = 'ZBOT') %>%
      efdcr::melt_nc(var = 'ZBOT', var_name = 'ELEV', nc = nc, na.rm = T)
    grids <- merge(grids, ZBOT_dt[, .(col, row, ELEV)])
  } else {
    cat(glue::glue('Generate grids from {flxly} and {fdxdy}\n'))
    lxly_colnames <- readr::read_lines(flxly, skip = 3, n_max = 1) %>%
      stringr::str_split(' ', simplify = T) %>%
      `[`(which(. != ''))
    lxly <- data.table::fread(flxly, skip = 4, col.names = lxly_colnames[-1])
    dxdy_colnames <- readr::read_lines(fdxdy, skip = 3, n_max = 1) %>%
      stringr::str_split(' ', simplify = T) %>%
      `[`(which(. != ''))
    dxdy <- data.table::fread(fdxdy, skip = 4, col.names = dxdy_colnames[c(-1, -length(dxdy_colnames))])
    grids_dt <- merge(lxly, dxdy, by = c('I', 'J'))
    grids_dt[, ':='(X1 = X - DX / 2,
                    Y1 = Y + DY / 2,
                    X2 = X + DX / 2,
                    Y2 = Y + DY / 2,
                    X3 = X + DX / 2,
                    Y3 = Y - DY / 2,
                    X4 = X - DX / 2,
                    Y4 = Y - DY / 2)]
    grids_dt[, .(I, J, X1, X2, X3, X4)] %>%
      data.table::melt(id.vars = c('I', 'J'), value.name = 'X', variable.name = 'Ctag') -> X_dt
    grids_dt[, .(I, J, Y1, Y2, Y3, Y4)] %>%
      data.table::melt(id.vars = c('I', 'J'), value.name = 'Y', variable.name = 'Ctag') -> Y_dt
    X_dt[order(I, J), ID := rep(1:NROW(grids_dt), each = 4)]
    Y_dt[order(I, J), ID := rep(1:NROW(grids_dt), each = 4)]
    X_dt[, Ctag := stringr::str_replace_all(Ctag, 'X', 'C')]
    Y_dt[, Ctag := stringr::str_replace_all(Ctag, 'Y', 'C')]
    coords_dt <- merge(X_dt, Y_dt, by = .EACHI)
    sf::st_as_sf(coords_dt, coords = c('X', 'Y')) %>%
      group_by(I, J) %>%
      summarise(geometry = sf::st_combine(geometry)) %>%
      sf::st_cast('POLYGON') %>%
      sf::st_set_crs(32650) -> grids
    grids <- merge(grids, grids_dt[, .(I, J, ELEV)])
  }
  invisible(grids)
}










