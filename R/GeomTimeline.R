#' Geom timeline
#'
#' A custom geom to plot the timeline of earthquake with magnitude and number of death.
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot2 object to visualize the timeline and magnitude of earthquakes from NOAA data.
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline, geom = geomTimeline, mapping = mapping,
    data = data,  position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
#'
#' A helper function for \code{draw_group} geomTimeline
#'
#' @param data data
#' @param panel_scales panel_scales
#' @param coord coord
#'
#' @importFrom grid segmentsGrob
#' @importFrom grid pointsGrob
#' @importFrom grid xaxisGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#'

draw_timeline <- function(data, panel_scales, coord) {
  
  coords <- coord$transform(data, panel_scales)
  
  # segmentsGrob to draw line segment where we will plot our earthquake points
  my_segment_grob <- grid::segmentsGrob(
    x0 = grid::unit(coords$xmin,"native"),
    x1 = grid::unit(coords$xmax,"native"),
    y0 = grid::unit(coords$y,"native"),
    y1 = grid::unit(coords$y,"native"),
    gp = grid::gpar(col = "grey", alpha = 0.30)
  )
  
  # pointsGrob to draw points to represent earthquakes with varying size and alpha
  my_point_grob <- grid::pointsGrob(
    x = grid::unit(coords$x,"native"),
    y = coords$y,
    pch = coords$shape,
    size = grid::unit(coords$size,"mm"),
    gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha = coords$alpha)
  )
  
  # draws an xaxis
  my_axis_grob <- grid::xaxisGrob()
  
  # group our grobs together for output
  grid::gTree(children = grid::gList(my_segment_grob, my_point_grob, my_axis_grob))
}

#' geomTimeline
#'
#' We are using ggproto to create a new class.
#'
#' @export
geomTimeline <- ggplot2::ggproto("geomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 optional_aes = c("y", "xmin","xmax"),
                                 default_aes = ggplot2::aes(y = 0.5, shape = 21, size = 1, colour = "blue", fill = "blue", alpha = 0.5, stroke = 1),
                                 draw_key = ggplot2::draw_key_point,
                                 
                                 draw_group = draw_timeline)
#'
#' StatTimeline
#'
#' @importFrom dplyr filter
#' @export
StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 required_aes = c("x","xmin","xmax"),
                                 
                                 setup_params = function(data, params) {
                                   min <- data$xmin
                                   max <- data$xmax
                                   list(
                                     min = min,
                                     max = max,
                                     na.rm = params$na.rm
                                   )
                                 },
                                 # Adding filter to limit date based on min and max dates and also remove any NA's from size
                                 
                                 compute_group = function(data, scales, min, max) {
                                   data %>% dplyr::filter(data$x > data$xmin & data$x < data$xmax & !is.na(data$size))
                                 }
)