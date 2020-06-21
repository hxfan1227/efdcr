#' @import grid
#' @import ggplot2
NULL
#' Geom for plotting velocity vectors.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}.
#' If specified and \code{inherit.aes = TRUE} (the default),
#' it is combined with the default mapping at the top level of the plot.
#' You must supply mapping if there is no plot mapping.
#'
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If \code{NULL}, the default, the data is inherited from the plot data as specified in the call to \code{\link[ggplot2]{ggplot}}.
#'
#' A \code{data.frame}, or other object, will override the plot data.
#' All objects will be fortified to produce a data frame.
#' See \code{\link[ggplot2]{fortify}} for which variables will be created.
#' @param stat Use to override the default connection
#'
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#'
#' @param na.rm If \code{False}, the default, missing values are removed with a warning.
#' If \code{TRUE}, missing values are silently removed.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped.
#' \code{FALSE}never includes,
#' and \code{TRUE} always includes
#'
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
#' This is most useful for helper functions that define both data and aesthetics and shouldn't inherit
#' behaviour from the default plot specification.
#'
#' @param scale Multiplicative scaling factor.
#' 
#' @param arrow_scale Multiplicative scaling factor for arrows
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}.
#'
#' @export
geom_velocity <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, scale = 1, arrow_scale = 0.5, ...) {
  layer(geom = GeomVelocity,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, scale = scale, arrow_scale = arrow_scale, ...)
  )
}
NULL
#' Geom Velocity
GeomVelocity <- ggplot2::ggproto("GeomVelocity", ggplot2::Geom,
                               required_aes = c("x", "y", "u", "v"),
                               default_aes = ggplot2::aes(color = "black", scale = 1, size = 0.2),
                               draw_key = ggplot2::draw_key_polygon,
                               draw_panel = function(data, panel_scales, coord, scale = 1, arrow_scale = 0.5) {
                                 coords <- coord$transform(data, panel_scales)
                                 Mmag <- max(sqrt(coords$u^2 + coords$v^2))
                                 coords$mag0 <- sqrt(coords$u^2 + coords$v^2)
                                 coords$mag <- with(coords, mag0/Mmag*scale)
                                 coords$angle <- with(coords, atan2(v, u))
                                 coords$dx <- with(coords, cos(angle)*mag)*scale
                                 coords$dy <- with(coords, sin(angle)*mag)*scale
                                 coords <- coords[which(coords$mag0 >= 1e-3),] # don't draw if velocity == 0
                                 arrow.len <- grid::convertUnit(unit(min(coords$mag0) * arrow_scale, "npc"), "cm")
                                 xx <- grid::unit.c(unit(coords$x, "npc"),
                                              unit(coords$x, "npc") + unit(coords$dx, "snpc"))
                                 yy <- grid::unit.c(unit(coords$y, "npc"),
                                              unit(coords$y, "npc") + unit(coords$dy, "snpc"))
                                 pol <- grid::polylineGrob(x = xx, y = yy,
                                                           default.units = "npc",
                                                           arrow = grid::arrow(angle = 10, length = arrow.len),
                                                           gp = grid::gpar(col = coords$color, lwd = coords$size),
                                                           id = rep(seq(nrow(coords)), 2))

                                 pol
                               })
