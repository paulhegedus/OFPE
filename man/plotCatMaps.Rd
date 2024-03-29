% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotFuns.R
\name{plotCatMaps}
\alias{plotCatMaps}
\title{Create a map of a categorical variable of interest from provided data.}
\usage{
plotCatMaps(
  df,
  var_col_name,
  var_label,
  var_main_label,
  fieldname,
  farmername,
  utm_zone
)
}
\arguments{
\item{df}{Data.frame or data.table with data to be mapped. Must include columns
labeled 'x' and 'y' and for the data that is to be mapped.}

\item{var_col_name}{The name of the column of the variable in the
supplied data ('dat'). This can be a vector corresponding with
'var_label' and 'var_main_label'.}

\item{var_label}{The label to be applied to the legend of the map
corresponding to the variable mapped. This can be a vector corresponding
with 'var_col_name' and 'var_main_label'.}

\item{var_main_label}{The main label to apply to the map. This can be a vector
corresponding with 'var_col_name' and 'var_label'.}

\item{fieldname}{Unique field name corresponding to all fields used in the simulation.}

\item{farmername}{Name of the farmer managing the field. Used to identify UTM zone and
for figure labeling.}

\item{utm_zone}{The user must provide the UTM zone of the data.}
}
\value{
If one variable for plotting is passed as an argument, a map of
selected variable is returned, otherwise a list with the map for each
variable the user passed in will be returned.
}
\description{
Function for creating a map with a Google satellite basemap
and the rasterized map of a variable supplied by the user. The user supplies
the column name to search for in the data to map, the legend label, and the
main label. They also must provide the name of the field mapped and the farmer
that manages it. Assumes a categorical variable and thus a discrete scale is applied.
For mapping continuous numeric data see plotMaps().
}
