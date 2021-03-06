% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legendMap.R
\name{create_scale_bar}
\alias{create_scale_bar}
\title{Create a scale bar}
\source{
https://github.com/3wen/legendMap.git
}
\usage{
create_scale_bar(
  lon,
  lat,
  distance_lon,
  distance_lat,
  distance_legend,
  dist_units = "km"
)
}
\arguments{
\item{lon}{Longitude of the bottom left point of the first rectangle to draw.}

\item{lat}{Latitude of the bottom left point of the first rectangle to draw.}

\item{distance_lon}{Length of each rectangle.}

\item{distance_lat}{Width of each rectangle.}

\item{distance_legend}{Distance between rectangles and legend texts.}

\item{dist_units}{Units of distance "km" (kilometers) (default), "nm"
(nautical miles), "mi" (statute miles).}
}
\value{
Return a list whose elements are; 'rectangle': a data.frame containing
the coordinates to draw the first rectangle, 'rectangle2': a data.frame containing
the coordinates to draw the second rectangle, and 'legend': a data.frame
containing the coordinates of the legend texts, and the texts as well.
}
\description{
Function from 3wen/legendMap repo for 'adding a North arrow and/or a
scale bar to maps done using ggplot2'. All credit for this function goes to
Ewen Gallic. All notation and documentation is from Ewen Gallic.

This function creates a scale bar. See https://github.com/3wen/legendMap.git
for examples.
}
