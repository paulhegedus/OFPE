% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rxFuns.R
\name{getRxGrid}
\alias{getRxGrid}
\title{Return a grid for creating experiments and prescriptions.}
\usage{
getRxGrid(
  db,
  rx_dt,
  farmername,
  fieldname,
  trt_length,
  trt_width,
  unique_fieldname,
  mgmt_scen = "base",
  buffer_width,
  heading = 0
)
}
\arguments{
\item{db}{Connection to an OFPE formatted database.}

\item{rx_dt}{Data frame that contains the coordinates of locations to apply
experimental inputs. For an experiment, these are the centroids of the grid
made to aggregate data for the field.}

\item{farmername}{Provide the farmer name that manages the field}

\item{fieldname}{Provide the name of the field for the experiment or prescription.}

\item{trt_length}{Length, in meters, for which to apply treatments.}

\item{trt_width}{Width, in meters, for which to apply treatments.}

\item{unique_fieldname}{Unique fieldname for the field(s) used for the experiment. This
concatenates multiple fields with an ampersand. Used for labeling.}

\item{mgmt_scen}{If the user is creating a prescription or experimental
prescription, they must provide the management scenario to use for their
prescription. The user can choose from the management options listed in
the SimClass. The options are 'SSOPT': site-specific optimized rates,
'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
input rate in conventional system types, and the farmer selected rate for
organic systems, both of which are already provided. Default to 'base' for new
experiments, override if making a prescription.}

\item{buffer_width}{Width from the edge of the field within which to apply treatments.}

\item{heading}{Numeric, heading in degrees from true north to rotate the
experiment/prescription to. Default is 0 (no rotation). Note that if a
heading is provided, the grid is trimmed based on the buffered boundary but
rotation caused by providing a heading may skew treatment rates so that
they encroach into the cleanup strip.}
}
\value{
NULL, grid data stored in database in 'rxgrids' and 'rxgridtemp'.
}
\description{
Returns a grid of the user specified dimensions combined
with data passed in by the user. The data available for use as an
experiment is clipped to the grid, removing areas between the field edge
and the grid within the field. This is used to apply the experimental
or check rates.
}
