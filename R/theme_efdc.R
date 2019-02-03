NULL
#' The theme of the model plot
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param base_line_size Base size for line elements.
#' @param base_rect_size Base size for rect elements.
#' @export
theme_efdc <- function(base_size = 11, 
                       base_family = '',
                       base_line_size = base_size/22,
                       base_rect_size = base_size/22){
  ggplot2::theme_bw(base_size = base_size, base_family = base_family, 
                    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(strip.background = element_blank(),
                   axis.text = element_text(color = 'black'),
                   legend.position = 'bottom')
}