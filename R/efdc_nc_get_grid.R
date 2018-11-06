library(ncdf4)
library(data.table)
library(ggplot2)
filename <- 'C:/Users/Frank/Desktop/1.nc'
efdc_nc_get_grid <- function(filename){
  nc <- nc_open(filename)
  cat(paste("The file has",nc$nvars,"variables: "), names(nc$var))
  # read grids:
  # longitude at grid center
  lon <- ncvar_get(nc, 'lon')
  # latitude at grid center
  lat <- ncvar_get(nc, 'lat')
  # longitudes of 4 corners of the grid
  lon_bnds <- ncvar_get(nc, 'lon_bnds')
  # longitudes of 4 corners of the grid
  lat_bnds <- ncvar_get(nc, 'lat_bnds')
  lon_melt <- melt(lon, na.rm = T)
  lat_melt <- melt(lat, na.rm = T)
  lon_bnds_melt <- melt(lon_bnds, value.name = 'lon', na.rm = T)
  lat_bnds_melt <- melt(lat_bnds, value.name = 'lat', na.rm = T)
  lon_lat_df <- merge(lon_bnds_melt, lat_bnds_melt, by = c('Var2', 'Var3', 'Var1'))
  setDT(lon_lat_df)
  n_grids <- sum(!is.na(lon))
  lon_lat_df[, id := rep(1:n_grids, each = 4)]
  ggplot(lon_lat_df, aes(x = lon, y = lat, group = id)) + 
    geom_polygon(fill = NA, color = 'blue') +
    coord_equal()
  # merge center corrdinates:
  lon_melt <- melt(lon, value.name = 'clon')
  lat_melt <- melt(lat, value.name = 'clat')
  base_df <- merge(lon_lat_df, lon_melt, by.x = c('Var2', 'Var3'), by.y = c('Var1', 'Var2'))
  base_df <- merge(base_df, lat_melt, by.x = c('Var2', 'Var3'), by.y = c('Var1', 'Var2'))
  # merge elevation
  elv_var <- ncvar_get(nc, 'ZBOT')
  elv_var_melt <- melt(elv_var, value.name = 'elevation')
  base_df <- merge(base_df, elv_var_melt, by.x = c('Var2', 'Var3'), by.y = c('Var1', 'Var2'))
  return(base_df)
}

nc <- nc_open(filename)
nc
grid_dt <- efdc_nc_get_grid('C:/Users/Frank/Desktop/1.nc')
data <- ncvar_get(nc, 'TEMP')
data_melt <- melt(data, vaue.name = 'temp', na.rm = T)
setDT(data_melt)


data <- data[, , 1]



