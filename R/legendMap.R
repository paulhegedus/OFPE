#' @title Create a scale bar
#'
#' @description Function from 3wen/legendMap repo for 'adding a North arrow and/or a
#' scale bar to maps done using ggplot2'. All credit for this function goes to
#' Ewen Gallic. All notation and documentation is from Ewen Gallic.
#'
#' This function creates a scale bar. See https://github.com/3wen/legendMap.git
#' for examples.
#' @param lon Longitude of the bottom left point of the first rectangle to draw.
#' @param lat Latitude of the bottom left point of the first rectangle to draw.
#' @param distance_lon Length of each rectangle.
#' @param distance_lat Width of each rectangle.
#' @param distance_legend Distance between rectangles and legend texts.
#' @param dist_units Units of distance "km" (kilometers) (default), "nm"
#' (nautical miles), "mi" (statute miles).
#' @return Return a list whose elements are; 'rectangle': a data.frame containing
#' the coordinates to draw the first rectangle, 'rectangle2': a data.frame containing
#' the coordinates to draw the second rectangle, and 'legend': a data.frame
#' containing the coordinates of the legend texts, and the texts as well.
#' @source https://github.com/3wen/legendMap.git
#' @export
create_scale_bar <-  function(lon,
                              lat,
                              distance_lon,
                              distance_lat,
                              distance_legend,
                              dist_units = "km") {
  # First rectangle
  bottom_right <- maptools::gcDestination(lon = lon,
                                          lat = lat,
                                          bearing = 90,
                                          dist = distance_lon,
                                          dist.units = dist_units,
                                          model = "WGS84")
  topLeft <- maptools::gcDestination(lon = lon,
                                     lat = lat,
                                     bearing = 0,
                                     dist = distance_lat,
                                     dist.units = dist_units,
                                     model = "WGS84")
  rectangle <- cbind(lon = c(lon, lon, bottom_right[1, "long"],
                             bottom_right[1, "long"], lon),
                     lat = c(lat, topLeft[1, "lat"],
                             topLeft[1, "lat"], lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  # Second rectangle t right of the first rectangle
  bottom_right2 <- maptools::gcDestination(lon = lon,
                                           lat = lat,
                                           bearing = 90,
                                           dist = distance_lon * 2,
                                           dist.units = dist_units,
                                           model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1, "long"],
                              bottom_right[1, "long"],
                              bottom_right2[1, "long"],
                              bottom_right2[1, "long"],
                              bottom_right[1,"long"]),
                      lat = c(lat, topLeft[1, "lat"],
                              topLeft[1, "lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  # Now let's deal with the text
  on_top <- maptools::gcDestination(lon = lon,
                                    lat = lat,
                                    bearing = 0,
                                    dist = distance_legend,
                                    dist.units = dist_units,
                                    model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1, "long"] <- bottom_right[1, "long"]
  on_top3[1, "long"] <- bottom_right2[1, "long"]
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend,
                             text = c(0, distance_lon, distance_lon * 2)),
                       stringsAsFactors = FALSE,
                       row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
#' @title Create orientation arrow.
#'
#' @description Function from 3wen/legendMap repo for 'adding a North arrow and/or a
#' scale bar to maps done using ggplot2'. All credit for this function goes to
#' Ewen Gallic. All notation and documentation is from Ewen Gallic.
#'
#' This function creates an orientation arrow (i.e. north arrow).
#' See https://github.com/3wen/legendMap.git for examples.
#' @param scale_bar Result of create_scale_bar()
#' @param length Desired length of the arrow
#' @param distance Distance between legend rectangles and the bottom of the arrow.
#' Default is 1.
#' @param dist_units Units of distance "km" (kilometers) (default), "nm"
#' (nautical miles), "mi" (statute miles).
#' @return Return a list whose elements are; 'res': coordinates to draw an arrow,
#' and 'coords_n': coordinates of the middle of the arrow (where the "N" will be plotted).
#' @source https://github.com/3wen/legendMap.git
#' @export
create_orientation_arrow <- function(scale_bar,
                                     length,
                                     distance = 1,
                                     dist_units = "km") {
  lon <- scale_bar$rectangle2[1, 1]
  lat <- scale_bar$rectangle2[1, 2]
  # Bottom point of the arrow
  beg_point <- maptools::gcDestination(lon = lon,
                                       lat = lat,
                                       bearing = 0,
                                       dist = distance,
                                       dist.units = dist_units,
                                       model = "WGS84")
  lon <- beg_point[1, "long"]
  lat <- beg_point[1, "lat"]
  # Let us create the endpoint
  on_top <- maptools::gcDestination(lon = lon,
                                   lat = lat,
                                   bearing = 0,
                                   dist = length,
                                   dist.units = dist_units,
                                   model = "WGS84")
  left_arrow <- maptools::gcDestination(lon = on_top[1, "long"],
                                        lat = on_top[1, "lat"],
                                        bearing = 225,
                                        dist = length / 5,
                                        dist.units = dist_units,
                                        model = "WGS84")
  right_arrow <- maptools::gcDestination(lon = on_top[1, "long"],
                                         lat = on_top[1, "lat"],
                                         bearing = 135,
                                         dist = length / 5,
                                         dist.units = dist_units,
                                         model = "WGS84")
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1, "long"], yend = on_top[1, "lat"]),
    cbind(x = left_arrow[1, "long"], y = left_arrow[1, "lat"],
          xend = on_top[1, "long"], yend = on_top[1, "lat"]),
    cbind(x = right_arrow[1, "long"], y = right_arrow[1, "lat"],
          xend = on_top[1, "long"], yend = on_top[1, "lat"]))
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1, "lat"]) / 2)
  return(list(res = res, coords_n = coords_n))
}
#' @title Enables drawing of a scale bar on a ggplot object, and orientation arrow (optional)
#'
#' @description Function from 3wen/legendMap repo for 'adding a North arrow and/or a
#' scale bar to maps done using ggplot2'. All credit for this function goes to
#' Ewen Gallic. All notation and documentation is from Ewen Gallic.
#'
#' This function Enables drawing of a scale bar on a ggplot object, and orientation
#' arrow (optional). See https://github.com/3wen/legendMap.git for examples. Utilizes
#' create_scale_bar(), and create_orientation_arrow().
#' @param lon Longitude of the bottom left point of the first rectangle to draw.
#' @param lat Latitude of the bottom left point of the first rectangle to draw.
#' @param distance_lon Length of each rectangle.
#' @param distance_lat Width of each rectangle.
#' @param distance_legend Distance between rectangles and legend texts.
#' @param dist_unit Units of distance "km" (kilometers) (default), "nm"
#' (nautical miles), "mi" (statute miles).
#' @param rec_fill Filling colour of the first rectangle (default to white).
#' @param rec2_fill Filling colour of the second rectangle (default to black).
#' @param rec_colour Colour of the first rectangle (default to black).
#' @param rec2_colour Colour of the second rectangle (default to black).
#' @param legend_colour Legend colour (default to black).
#' @param legend_size Legend size (default to 3).
#' @param orientation Logical, if TRUE (default), adds an orientation arrow to
#' the plot.
#' @param arrow_length Length of the arrow (default to 500 km).
#' @param arrow_distance Distance between the scale bar and the bottom of
#' the arrow (default to 300 km)
#' @param arrow_north_size Size of the "N" letter (default to 6).
#' @return Enables drawing of a scale bar on a ggplot object,
#' and optionally an orientation arrow. Use in ggplot plot code.
#' @source https://github.com/3wen/legendMap.git
#' @export
scale_bar <- function(lon,
                      lat,
                      distance_lon,
                      distance_lat,
                      distance_legend,
                      dist_unit = "km",
                      rec_fill = "white",
                      rec_colour = "black",
                      rec2_fill = "black",
                      rec2_colour = "black",
                      legend_colour = "black",
                      legend_size = 3,
                      orientation = TRUE,
                      arrow_length = 500,
                      arrow_distance = 300,
                      arrow_north_size = 6) {
  the_scale_bar <- OFPE::create_scale_bar(lon = lon,
                                          lat = lat,
                                          distance_lon = distance_lon,
                                          distance_lat = distance_lat,
                                          distance_legend = distance_legend,
                                          dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- ggplot2::geom_polygon(data = the_scale_bar$rectangle,
                                      ggplot2::aes(x = lon, y = lat),
                                      fill = rec_fill,
                                      colour = rec_colour)
  # Second rectangle
  rectangle2 <- ggplot2::geom_polygon(data = the_scale_bar$rectangle2,
                                      ggplot2::aes(x = lon, y = lat),
                                      fill = rec2_fill,
                                      colour = rec2_colour)
  # Legend
  scale_bar_legend <- ggplot2::annotate("text",
                                        label = paste(the_scale_bar$legend[, "text"],
                                                      dist_unit, sep=""),
                                        x = the_scale_bar$legend[, "long"],
                                        y = the_scale_bar$legend[, "lat"],
                                        size = legend_size,
                                        colour = legend_colour)

  res <- list(rectangle1, rectangle2, scale_bar_legend)
  if (orientation) {# Add an arrow pointing North
    coords_arrow <- OFPE::create_orientation_arrow(scale_bar = the_scale_bar,
                                                   length = arrow_length,
                                                   distance = arrow_distance,
                                                   dist_unit = dist_unit)
    arrow <- list(ggplot2::geom_segment(data = coords_arrow$res,
                                        ggplot2::aes(x = x, y = y, xend = xend, yend = yend)),
                  ggplot2::annotate("text",
                                    label = "N",
                                    x = coords_arrow$coords_n[1, "x"],
                                    y = coords_arrow$coords_n[1, "y"],
                                    size = arrow_north_size,
                                    colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}
