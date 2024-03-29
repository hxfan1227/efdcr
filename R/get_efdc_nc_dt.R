#' @import ncdf4
#' @import ggplot2
#' @importFrom tibble as_tibble
#' @importFrom cubelyr as.tbl_cube
#' @importFrom ncdf4 nc_open ncvar_get
#' @import data.table
#' @importFrom data.table melt setDT
#' @importFrom stats na.omit
#' @importFrom purrr map
NULL
#' Export grid information to a \code{data.table} from the \code{.nc} file generated by EE.
#' @param filename Character. The name of the \code{.nc} file generated by EE
#' @param ncarray Array. The 3D array read by \code{\link[ncdf4]{ncvar_get}}
#' @param ... Characters. The names of the \code{.nc} files generated by EE (maximum 2 at once).
#' @param var_name Character. The name of the variable to read.
#' @param var_out Character. The name of the variable in the molten data.
#' @param var Character. The name of the variable to read.
#' @param wet_depth Numeric. The minimum depth of the wet cell.
#' @param remove_dry Logical. Should the dry cell be removed?
#' @param verbose Logical. Should print the processing message?
#' @param with_coord Logical. Should the data merge with the coordinates (for plot)?
#' @param nc NetCDF file connection generated by \code{\link[ncdf4]{nc_open}}.
#' @param na.rm Logical. A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return A \code{data.table} contains grid coordinates and elevation.
#' @export
get_efdc_nc_dt <- function(..., var_name, wet_depth = 0.15, with_coord = T, remove_dry = T, verbose = T){
  fnames_ <- c(...)
  if (length(fnames_) > 2){
    stop('You can only process 2 files at once')
  }
  if (length(fnames_) == 1){
    get_efdc_nc_var(fnames_[1], var_name = var_name, wet_depth = wet_depth, with_coord = with_coord, remove_dry = remove_dry, verbose = verbose)
  } else {
    get_efdc_nc_var(fnames_[1], fnames_[2], var_name = var_name, wet_depth = wet_depth, with_coord = with_coord, remove_dry = remove_dry, verbose = verbose)
  }
}

NULL
#' Melt nc array files
#' @rdname get_efdc_nc_dt
#' @export
melt_nc <- function(ncarray, 
                    var = 'value', 
                    var_out, 
                    nc, 
                    na.rm = F, 
                    na.value = -999.0){
  if (missing(var_out)) {
    var_out = var
  }
  ncarray[which(ncarray == na.value)] <- NA
  ndims <- length(dim(ncarray))
  dim_names <- purrr::map(1:ndims, function(x){1:dim(ncarray)[x]})
  names(dim_names) <- paste0('Var', 1:ndims)
  dimnames(ncarray) <- dim_names
  ncarray %>% cubelyr::as.tbl_cube(met_name = var_out) %>%
    tibble::as_tibble() %>% data.table::setDT() -> ans
  if (na.rm) {
    ans <- na.omit(ans)
  }
  if (!missing(nc)){
    nc_vars <- nc$var
    col_names <- c(purrr::map_chr(nc_vars[[var]][['dim']], ~ .$name), var_out)
    k_name <- ifelse('lyr' %in% col_names, 'lyr', 'kmax')
    if (k_name %in% col_names) {
      if (nc_vars[[var]][['dim']][[which(col_names == k_name)]][['len']] == 1) {
        col_names <- col_names[-1]
        colnames(ans) <- col_names
      } else{
        colnames(ans) <- col_names
      }
    } else {
      colnames(ans) <- col_names
    }
    
  }
  ans
}

NULL
#' List the variable names in the NetCDF file.
#' @rdname get_efdc_nc_dt
#' @export
has_variable <- function(filename){
  nc <- nc_open(filename)
  cat(paste("The file has",nc$nvars,"variables: "), names(nc$var))
}

NULL 
#' Read data from EE generated NetCDF files.
#' @rdname get_efdc_nc_dt
#' @export
get_efdc_nc_var <- function(..., var_name, wet_depth = 0.15, verbose = T,  with_coord = T, remove_dry = T){
  fnames_ <- c(...)
  if (length(fnames_) > 2){
    stop('You can only process 2 files at once')
  }
  nc <- nc_open(fnames_[1])
  if (with_coord){
    coord_df <- get_efdc_grids_as_sf(fnc = fnames_[1])
  }
  # add ZBOT 
  if (verbose) {
    cat('Reading bottom elevation (ZBOT)...\n')
  }
  if ('ZBOT' %in% names(nc$var)) {
    zbot_df <- ncdf4::ncvar_get(nc, 'ZBOT')
    zbot_df_melt <- melt_nc(zbot_df, var = 'ZBOT', na.rm = T, nc = nc)
  } else {
    if ('Bottom' %in% names(nc$var)) {
      zbot_df <- ncdf4::ncvar_get(nc, 'Bottom')
      zbot_df_melt <- melt_nc(zbot_df, var = 'Bottom', var_out = 'ZBOT', na.rm = T, nc = nc)
    }
  }
  # zbot_df_melt <- setDT(reshape2::melt(zbot_df, value.name = 'ZBOT', na.rm = T))
  
  if (verbose) {
    cat('Finish reading bottom elevation (ZBOT)!\n')
  }
  # return ZBOT
  if (var_name == 'ZBOT' | var_name == 'Bottom'){
    if (with_coord){
      base_df <- merge(coord_df, zbot_df_melt, 
                       by = intersect(colnames(coord_df), colnames(zbot_df_melt)))
      setDT(base_df)
      return(base_df)
    } else {
      return(zbot_df_melt)
    }
  }
  # add WSEL to see if the cell is wet or not
  if (verbose) {
    cat('Reading water surface elevation (WSEL)...\n')
  }
  if (length(fnames_) == 1){
    wsel_df <- ncdf4::ncvar_get(nc, 'WSEL')
    # wsel_df_melt <- setDT(reshape2::melt(wsel_df, value.name = 'WSEL'))
    wsel_df_melt <- melt_nc(wsel_df, var = 'WSEL', nc = nc, na.rm = T)
    var_df <- merge(zbot_df_melt, wsel_df_melt, 
                    by = intersect(colnames(zbot_df_melt), colnames(wsel_df_melt)))
    var_df[, WETFLAG := ifelse((WSEL - ZBOT) > wet_depth | !is.na(WSEL), 1, NA)]
    if (verbose) {
      cat('Finish reading water surface elevation (WSEL)\n')
    }
    if (var_name == 'WSEL') {
      if (remove_dry){
        var_df <- na.omit(var_df, cols = 'WETFLAG')
      }
      if (with_coord){
        base_df <- merge(coord_df, var_df, by = intersect(colnames(coord_df), colnames(var_df)), 
                         allow.cartesian = T)
        setDT(base_df)
        return(base_df)
      } else {
        return(var_df)
      }
    }
    # read other data
    if (verbose) {
      cat('Reading variables ', var_name, ' ...\n', sep = '')
    }
    var_df_ <- ncvar_get(nc, var_name)
    # var_df_melt_ <- setDT(reshape2::melt(var_df_, value.name = var_name))
    var_df_melt_ <- melt_nc(var_df_, var = var_name, nc = nc, na.rm = T)
    var_df <- merge(var_df, var_df_melt_, 
                    by = intersect(colnames(var_df), colnames(var_df_melt_)), all = T)
    if (verbose) {
      cat('Finish reading variables ', var_name, ' !\n', sep = '')
    }
    if (remove_dry){
      var_df <- na.omit(var_df, cols = 'WETFLAG')
    }
    if (with_coord){
      base_df <- merge(coord_df, var_df, 
                       by = intersect(colnames(coord_df), colnames(var_df)), 
                       allow.cartesian = T)
      setDT(base_df)
      return(base_df)
    } else {
      return(var_df)
    }
  } else { # comapre 2 files
    wsel_df1 <- ncvar_get(nc_open(fnames_[1]), 'WSEL')
    wsel_df2 <- ncvar_get(nc_open(fnames_[2]), 'WSEL')
    # wsel_df1_melt <- setDT(reshape2::melt(wsel_df1, value.name = 'WSEL'))
    wsel_df1_melt <- melt_nc(wsel_df1, var = 'WSEL', nc = nc_open(fnames_[1]))
    # wsel_df2_melt <- setDT(reshape2::melt(wsel_df2, value.name = 'WSEL'))
    wsel_df2_melt <- melt_nc(wsel_df2, var = 'WSEL', nc = nc_open(fnames_[2]))
    var_df <- merge(zbot_df_melt, wsel_df1_melt, 
                    by = intersect(colnames(zbot_df_melt), colnames(wsel_df1_melt)), 
                    all = T)
    var_df <- merge(var_df, wsel_df2_melt, 
                    by = intersect(colnames(var_df), colnames(wsel_df2_melt)), 
                    all = T)
    if (verbose) {
      cat('Finish reading water surface elevation (WSEL)\n', 'Comparing Models...\n', sep = '')
    }
    var_df_ <- ncvar_get(nc_open(fnames_[2]), var_name) - ncvar_get(nc_open(fnames_[1]), var_name)
    # var_df_melt_ <- setDT(reshape2::melt(var_df_, value.name = 'DIFF'))
    var_df_melt_ <- efdcr::melt_nc(var_df_, var = var_name, var_out = 'DIFF', nc = nc_open(fnames_[1]))
    var_df1_ <- ncvar_get(nc_open(fnames_[1]), var_name)
    var_df2_ <- ncvar_get(nc_open(fnames_[2]), var_name)
    # var_df1_melt_ <- setDT(reshape2::melt(var_df1_, value.name = 'V1'))
    var_df1_melt_ <- melt_nc(var_df1_, var = var_name, var_out = 'V1', nc = nc_open(fnames_[1]))
    # var_df2_melt_ <- setDT(reshape2::melt(var_df2_, value.name = 'V2'))
    var_df2_melt_ <- melt_nc(var_df2_, var = var_name, var_out = 'V2', nc = nc_open(fnames_[2]))
    var_df <- merge(var_df, var_df1_melt_, by = intersect(colnames(var_df1_melt_), colnames(var_df)))
    var_df <- merge(var_df, var_df2_melt_, by = intersect(colnames(var_df2_melt_), colnames(var_df)))
    var_df <- merge(var_df, var_df_melt_, by = intersect(colnames(var_df_melt_), colnames(var_df)))
    if (verbose) {
      cat('Finish!\n')
    }
    if (with_coord){
      base_df <- merge(coord_df, var_df, 
                       by = intersect(colnames(coord_df), colnames(var_df)), 
                       allow.cartesian = T)
      setDT(base_df)
      return(base_df)
    } else {
      return(var_df)
    }
  }
  base_df <- na.omit(base_df, cols = 'WETFLAG')
  if (verbose) {
    cat('Finish!\n')
  }
  return(base_df)
}


NULL
#' Read coordinates from EE generated NetCDF files.
#' Not used.
#' @rdname get_efdc_nc_dt
#' @export
get_efdc_nc_coordinates <- function(nc){
  # cat(paste("The file has",nc$nvars,"variables: "), names(nc$var))
  # stop if provided a unused var_name
  # stopifnot(var_name %in% names(nc$var))
  # read grids:
  # longitude at grid center
  lon <- ncvar_get(nc, 'lon')
  # latitude at grid center
  lat <- ncvar_get(nc, 'lat')
  # longitudes of 4 corners of the grid
  lon_bnds <- ncvar_get(nc, 'lon_bnds')
  # longitudes of 4 corners of the grid
  lat_bnds <- ncvar_get(nc, 'lat_bnds')
  lon_melt <- melt_nc(lon, var = 'lon', var_out = 'clon', nc = nc)
  lat_melt <- melt_nc(lat, var = 'lat', var_out = 'clat', nc = nc)
  lon_bnds_melt <- melt_nc(lon_bnds, var = 'lon_bnds', var_out = 'lon', nc = nc)
  lat_bnds_melt <- melt_nc(lat_bnds, var = 'lat_bnds', var_out = 'lat', nc = nc)
  lon_lat_df <- merge(lon_bnds_melt, lat_bnds_melt, 
                      by = intersect(colnames(lon_bnds_melt), colnames(lat_bnds_melt)))
  setDT(lon_lat_df)
  lon_lat_df <- na.omit(lon_lat_df)
  n_grids <- sum(!is.na(lon))
  lon_lat_df[order(col, row, cnr), id := rep(1:n_grids, each = 4)]
  # merge center corrdinates:
  base_df <- merge(lon_lat_df, lon_melt, 
                   by = intersect(colnames(lon_lat_df), colnames(lon_melt)))
  base_df <- merge(base_df, lat_melt, 
                   by = intersect(colnames(base_df), colnames(lat_melt)))
  invisible(base_df)
}

NULL
#' Plot EFDC nc data
#' @param fname Character. The file name of the \code{.nc} file.
#' @param var_name Character. The variable name to be plotted.
#' @param start_date Character. The start date of the simulation.
#' @param time_unit Character. A string that specifies the type of units that num refers to. 
#' @param by_f function. A function to apply to the date.
#' @export

plot_efdc_nc_var <- function(fname, 
                             var_name = names(nc_open(fname)$var),
                             start_date,
                             time_unit = 'months', 
                             by_f = lubridate::month
                             ) {
  if (missing(start_date)) {
    start_date <- lubridate::today()
  }
  nc <- nc_open(fname)
  var_name = match.arg(var_name)
  grids_dt <- get_efdc_nc_coordinates(nc)
  var_dt <- get_efdc_nc_dt(fname, var_name = var_name, with_coord = F)
  var_dt[, tTime := lubridate::ymd(start_date) + lubridate::duration(time, time_unit)]
  if ('lyr' %in% colnames(var_dt)) {
    var_dt <- var_dt[, lapply(.SD, mean, na.rm = T), .SDcols = var_name, 
                     by = .(col, row, lyr, time = by_f(tTime))]
    value_plt_dt <- merge(var_dt, grids_dt, by = c('col', 'row'), allow.cartesian = T)
    p <- ggplot(value_plt_dt, aes_string(x = 'lon', y = 'lat', group = 'id', fill = var_name)) +
      geom_polygon(color = NA) +
      facet_wrap(lyr ~ time)
  } else {
    var_dt <- var_dt[, lapply(.SD, mean, na.rm = T), .SDcols = var_name, 
                     by = .(col, row, time = by_f(tTime))]
    value_plt_dt <- merge(var_dt, grids_dt, by = c('col', 'row'), allow.cartesian = T)
    p <- ggplot(value_plt_dt, aes_string(x = 'lon', y = 'lat', group = 'id', fill = var_name)) +
      geom_polygon(color = NA) +
      facet_wrap(~ time)
  }
  p
}

utils::globalVariables(c('WETFLAG', 'WSEL', 'ZBOT', 'lyr', 'tTime', 'time'))