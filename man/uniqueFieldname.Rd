% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysisFuns.R
\name{uniqueFieldname}
\alias{uniqueFieldname}
\title{Identify a unique fieldname from data.}
\usage{
uniqueFieldname(dat)
}
\arguments{
\item{dat}{data.frame with a column called 'field' to produce a unique fieldname from, or
a vector of fieldnames.}
}
\value{
Character with unique field name.
}
\description{
Returns a character with the fieldname(s) present in the data. Unique name
for the field(s) analyzed. If multiple fields are used they are separated by an ampersand,
otherwise the singular field name is used. This is used for plotting. Can pass in a data.frame
with a column named 'field', or a vector of fieldnames.
}
