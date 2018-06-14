NULL
#' Calculate the area of the polygon with the given vextices
#' @param x Numeric. An array contains the x coordinates of the vextices.
#' @param y Numeric. An array contains the y coordinates of the vextices.
#' @return The area of the polyon.
#' @export
#' @examples \dontrun{
#' x <- c(0, 0, 1, 1)
#' y <- c(1, 0, 0, 1)
#' cal_poly_area(x, y)
#' }
cal_poly_area <- function(x, y){
  polygon.n <- length(x)
  area <- 0
  vec.x <- vec.y <- rep(0, 2)
  vec.x[1] <- x[2] - x[1]
  vec.y[1] <- y[2] - y[1]
  for (i in 3:polygon.n){
    vec.x[2] <- x[i] - x[1]
    vec.y[2] <- y[i] - y[1]
    area <- area + 0.5 * abs(vec.x[1] * vec.y[2] - vec.x[2] * vec.y[1])
    vec.x[1] <- vec.x[2]
    vec.y[1] <- vec.y[2]
  }
  return(area)
}



