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
  boom_width,
  unique_fieldname,
  mgmt_scen = "base"
)
}
\arguments{
\item{db}{Connection to an OFPE formatted database.}

\item{rx_dt}{Data frame that contains the coordinates of locations to apply
experimental inputs. For an experiment, these are the centroids of the grid
made to aggregate data for the field.}

\item{farmername}{If the user is creating a new experiment, provide or
select the name of the farmer that owns or manages the field(s) that
an experiment is going to be generated for.}

\item{fieldname}{If the user is creating a new experiment, provide or
select the fieldname of the field to use. The field list is from the
available fields in the database for experimentation.}

\item{trt_length}{Length, in meters, for which to apply treatments.}

\item{boom_width}{The width of the sprayer boom or spreader.}

\item{unique_fieldname}{Unique fieldname for the field(s) used for the experiment. This
concatenates multiple fields with an ampersand. Used for labelling.}

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