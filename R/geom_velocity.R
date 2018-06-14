geom_velocity <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, scale = 1,...) {
  layer(geom = GeomVelocity,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, scale = scale, ...)
  )
}
GeomVelocity <- ggplot2::ggproto("GeomVelocity", Geom,
                               required_aes = c("x", "y", "u", "v"),
                               default_aes = ggplot2::aes(color = "black", scale = 1),
                               draw_key = draw_key_polygon,
                               draw_panel = function(data, panel_scales, coord, scale = 1) {
                                 coords <- coord$transform(data, panel_scales)
                                 Mmag <- max(sqrt(coords$u^2 + coords$v^2))
                                 coords$mag0 <- sqrt(coords$u^2 + coords$v^2)
                                 coords$mag <- with(coords, mag0/Mmag*coords$scale)
                                 coords$angle <- with(coords, atan2(v, u))
                                 coords$dx <- with(coords, cos(angle)*mag)*scale
                                 coords$dy <- with(coords, sin(angle)*mag)*scale
                                 coords <- coords[which(coords$mag0 != 0),] # don't draw if velocity == 0
                                 xx <- grid::unit.c(unit(coords$x, "npc"),
                                              unit(coords$x, "npc") + unit(coords$dx, "snpc"))
                                 yy <- grid::unit.c(unit(coords$y, "npc"),
                                              unit(coords$y, "npc") + unit(coords$dy, "snpc"))
                                 pol <- grid::polylineGrob(x = xx, y = yy,
                                                           default.units = "npc",
                                                           arrow = grid::arrow(angle = 15, length = unit(0.5, "lines")),
                                                           gp = grid::gpar(col = coords$colour),
                                                           id = rep(seq(nrow(coords)), 2))

                                 pol
                               })
