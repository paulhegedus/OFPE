% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rxFuns.R
\name{makeRx}
\alias{makeRx}
\title{Make experiment or prescription}
\usage{
makeRx(rx_sdt, fld_bound, rx_for_year, conv)
}
\arguments{
\item{rx_sdt}{A 'sf' object containing the experiment or prescription
generated by user inputs.}

\item{fld_bound}{A 'sf' object containing the field boundary(ies) with
a base rate application.}

\item{rx_for_year}{The year that the prescription or experiment
is made for. Added to a column to specify the data.}

\item{conv}{The conversion factor between lbs of the input to the
units of the as-applied input (i.e. lbs N/ac to gal urea/ac).
Should be from RxClass object or is 1 / the conversion factor to
go from lbs of the experimental input to units of the as-applied
input.}
}
\value{
A 'sf' object with the final output for the producer.
}
\description{
This function takes the experiment or prescription 'sf'
object as well as the field boundary 'sf' object. The area of the experiment
or prescription is subtracted from the field boundary, and the two are
combined to create one output with the base rate around the experiment or
prescription rates.

While named 'makeRx', this is the function for creating new experiments
as well.
}
