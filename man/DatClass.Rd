% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatClass.R
\name{DatClass}
\alias{DatClass}
\title{R6 Class for storing inputs and data for analysis and simulations}
\description{
R6 Class for storing user specified inputs and processing data
for the analysis/simulation and Rx building steps of the OFPE data cycle.
This object includes user selections such as the field and year of data
to export from an OFPE database and the type of data (grid or observed) for analysis
and simulation/prescription generation.

Inputs can be supplied directly to this class during instantiation, however
this is NOT recommended except for advanced users. It is recommended that the
user supplies the database connection and uses the interactive selection
methods to select user inputs.

This class stores inputs from the user and has the methods for for exporting
data from the database and processing the data for analysis, simulation, and
prescription building.
}
\seealso{
\code{\link{DBCon}} for database connection class,
\code{\link{ModClass}} for model fitting class that relies on data in DatClass,
\code{\link{SimClass}} for simulation class that rely on data in DatClass.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dbCon}}{Database connection object connected to an OFPE formatted
database, see DBCon class.}

\item{\code{farmername}}{Name of the farmer that owns the selected field.}

\item{\code{fieldname}}{Name of the field for analysis. Selected from
the 'all_farms.fields' table of an OFPE formatted database.}

\item{\code{respvar}}{Response variable(s) to optimize experimental inputs based
off of. The user can select 'Yield' and/or 'Protein' based on data
availability. User must select at least 'Yield'.}

\item{\code{expvar}}{Experimental variable to optimize, select/input
'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
input that was experimentally varied across the field as part of the
on-farm experimentation.}

\item{\code{sys_type}}{Provide the type of system used in the experiment.
This determines the price used for calculating net-return and for
the net-return of the opposite type. Select from "Conventional" and
"Organic". The net-returns will be calculated with the corresponding
economic data based on this choice, and the 'NRopp' management
scenario (see SimClass$executeSim) will be based on the opposite
(e.g. if you are growing conventional wheat, the management outcome
'NRopp' shows the net-return calculated from organically grown wheat).
In the example, organic prices are calculated from 0 N fertilizer rates,
however with seeding rates it is purely the difference in the price received
used to calculate net-return.}

\item{\code{yldyears}}{The year(s) of interest for the yield response
variables in the selected field. This must be a named list with the
specified field names.}

\item{\code{proyears}}{The year(s) of interest for the protein response
variables in the selected field. This must be a named list with the
specified field names.}

\item{\code{mod_grid}}{Select whether to use gridded or observed data
locations for the analysis step. See the 'AggInputs' class for more
information on the 'GRID' option. The user must have aggregated data
with the specified GRID option prior to this step. (i.e. you will not
have access to data aggregated with the 'Grid' option if you have not
executed the process of aggregation with the 'Grid' option selected. The
same principle applies for the 'Observed' option. It is recommended that
the analysis is performed with 'Observed' data, and for the simulation to
be performed with 'Grid' data.}

\item{\code{sim_grid}}{Select whether to use gridded or observed data
locations for the simulation and subsequent prescription building step.
See the 'AggInputs' class for more information on the 'GRID' option.
The user must have aggregated data with the specified GRID option prior
to this step. (i.e. you will not have access to data aggregated with the
'Grid' option if you have not executed the process of aggregation with
the 'Grid' option selected. The same principle applies for the 'Observed'
option. It is recommended that the analysis is performed with 'Observed'
data, and for the simulation to be performed with 'Grid' data.}

\item{\code{dat_used}}{Option for the length of year to use data in the analysis,
simulation, and prescription building steps. See the 'AggInputs' class
documentation for more information on the 'dat_used' selection.}

\item{\code{center}}{TRUE/FALSE. Option for whether to center explanatory data around
each explanatory variables mean or to use the raw observed explanatory varaible
data. Centering is recommended as it puts variables on similar scales and makes
the model fitting process less error prone.}

\item{\code{split_pct}}{Select the percentage of data to use for the training dataset
in the analysis step. The training dataset is used to fit the model to each
of the crop responses. The difference will be split into a validation dataset
that is used to evaluate the model performance on data it has not 'seen' before.}

\item{\code{clean_rate}}{Select the maximum rate that could be realistically be applied
by the application equipment (sprayer or seeder). This is used for a rudimentary
cleaning of the data that removes observations with as-applied rates above this
user supplied threshold. Rates above this threshold should be able to be classified
as machine measurement errors. For example, based on knowledge of the prescription/
experiment applied and taking into account double applications on turns, a rate
for as-applied nitrogen might be something like 300 - 400 lbs N/acre.}

\item{\code{mod_dat}}{Based off of the user selections such as 'mod_grid', this is a named
list for each response variable ('yld' and/or 'pro'). The data in each of
these lists are processed and then split into training and validation datasets. This
data is used for the model fitting and evaluations steps.}

\item{\code{sim_dat}}{Based off of the user selections such as 'sim_grid', this is a named
list for each year specified in the SimClass 'sim_years' field. The data in each of
these lists are processed and used in the Monte Carlo simulation.}

\item{\code{mod_num_means}}{Named vector of the means for each numerical covariate, including
the experimental variable. This is used for converting centered data back to the
original form. The centering process does not center four numerical variables; the
x and y coordinates, the response variable (yld/pro), and the experimental variable.
This is for the data specified from the analysis data inputs (grid specific).}

\item{\code{sim_num_means}}{Named vector of the means for each numerical covariate, including
the experimental variable. This is used for converting centered data back to the
original form. The centering process does not center three numerical variables; the
x and y coordinates, the response variable (yld/pro) and the experimental variable.
This is for the data specified from the analysis data inputs (grid specific).}

\item{\code{opp_sys_type}}{Opposite of the user selected system type ('sys_type'). This is
used to select the correct price received to calculate 'NRopp' in the Monte Carlo
simulation.}

\item{\code{fieldname_codes}}{Data.frame with a column for the names of the fields selected
by the user and a corresponding code. This is used in the simulation data when being
passed to C++ functions as purely numeric matrices.}

\item{\code{SI}}{Logical, whether to use SI units. If TRUE, yield and experimental data are
converted to kg/ha. If FALSE, the default values from the database are used. These are
bu/ac for yield and lbs/ac for experimental data (nitrogen or seed).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{DatClass$new()}}
\item \href{#method-selectInputs}{\code{DatClass$selectInputs()}}
\item \href{#method-setupDat}{\code{DatClass$setupDat()}}
\item \href{#method-getSimDat}{\code{DatClass$getSimDat()}}
\item \href{#method-clone}{\code{DatClass$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatClass$new(
  dbCon,
  farmername = NULL,
  fieldname = NULL,
  respvar = NULL,
  expvar = NULL,
  sys_type = NULL,
  yldyears = NULL,
  proyears = NULL,
  mod_grid = NULL,
  dat_used = NULL,
  center = NULL,
  split_pct = NULL,
  SI = FALSE,
  clean_rate = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dbCon}}{Database connection object connected to an OFPE formatted
database, see DBCon class.}

\item{\code{farmername}}{Name of the farmer that owns the selected field.}

\item{\code{fieldname}}{Name of the field to for analysis. Selected from
the 'all_farms.fields' table of an OFPE formatted database.}

\item{\code{respvar}}{Response variable(s) to optimize experimental inputs based
off of. The user can select 'Yield' and/or 'Protein' based on data
availability. User must select at least 'Yield'.}

\item{\code{expvar}}{Experimental variable to optimize, select/input
'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
input that was experimentally varied across the field as part of the
on-farm experimentation.}

\item{\code{sys_type}}{Provide the type of system used in the experiment.
This determines the price used for calculating net-return and for
the net-return of the opposite type. Select from "Conventional" and
"Organic". The net-returns will be calculated with the corresponding
economic data based on this choice, and the 'NRopp' management
scenario (see SimClass$executeSim) will be based on the opposite
(e.g. if you are growing conventional wheat, the management outcome
'NRopp' shows the net-return calculated from organically grown wheat).
In the example, organic prices are calculated from 0 N fertilizer rates,
however with seeding rates it is purely the difference in the price received
used to calculate net-return.}

\item{\code{yldyears}}{The year(s) of interest for the yield response
variables in the selected field. This must be a named list with the
specified field names.}

\item{\code{proyears}}{The year(s) of interest for the protein response
variables in the selected field. This must be a named list with the
specified field names.}

\item{\code{mod_grid}}{Select whether to use gridded or observed data
locations for the analysis step. See the 'AggInputs' class for more
information on the 'GRID' option. The user must have aggregated data
with the specified GRID option prior to this step. (i.e. you will not
have access to data aggregated with the 'Grid' option if you have not
executed the process of aggregation with the 'Grid' option selected. The
same principle applies for the 'Observed' option. It is recommended that
the analysis is performed with 'Observed' data, and for the simulation to
be performed with 'Grid' data.}

\item{\code{dat_used}}{Option for the length of year to use data in the analysis,
simulation, and prescription building steps. See the 'AggInputs' class
documentation for more information on the 'dat_used' selection.}

\item{\code{center}}{TRUE/FALSE. Option for whether to center explanatory data around
each explanatory variables mean or to use the raw observed explanatory varaible
data. Centering is recommended as it puts variables on similar scales and makes
the model fitting process less error prone.}

\item{\code{split_pct}}{Select the percentage of data to use for the training dataset
in the analysis step. The training dataset is used to fit the model to each
of the crop responses. The difference will be split into a validation dataset
that is used to evaluate the model performance on data it has not 'seen' before.}

\item{\code{SI}}{Logical, whether to use SI units. If TRUE, yield and experimental data are
converted to kg/ha. If FALSE, the default values from the database are used. These are
bu/ac for yield and lbs/ac for experimental data (nitrogen or seed).}

\item{\code{clean_rate}}{Select the maximum rate that could be realistically be applied
by the application equipment (sprayer or seeder). This is used for a rudimentary
cleaning of the data that removes observations with as-applied rates above this
user supplied threshold. Rates above this threshold should be able to be classified
as machine measurement errors. For example, based on knowledge of the prescription/
experiment applied and taking into account double applications on turns, a rate
for as-applied nitrogen might be something like 300 - 400 lbs N/acre. NOTE: make sure to
specify in the correct units, for example if SI = FALSE specify in lbs/ac, otherwise
in kg/ha.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new 'AggInputs' object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-selectInputs"></a>}}
\if{latex}{\out{\hypertarget{method-selectInputs}{}}}
\subsection{Method \code{selectInputs()}}{
Interactive method for selecting inputs related to the data used in the
analysis, simulation, and subsequent prescription generation steps. The
description below describes the process of interactively selecting the
necessary parameters needed for the automated analysis, simulation, and
prescription building.

The user first selects a farmer for which they want to analyze a field
from, which is used to compile a list of available fields ready for
analysis, indicated by its presence in the farmername_a schema of the
OFPE database.

The user then selects the response variables to optimize on and the
experimental variable to optimize. The user must know what data is
available for the specific field (i.e. if the user select 'Protein' they
must have aggregated protein data for the specified field, or if the
user selects 'As-Applied Seed Rate' seed rates must have been the
experimental variable of interest when aggregating data).

The user then selects the location of aggregated data to use for both
the analysis and simulation/prescription building steps. The user also
needs to select the length of the year for which 'current' year data
was aggregated for (March 30th decision point or the full year).

The user also has the choice of which vegetation index data to use as
covariates, as well as the preferred source for precipitation and
growing degree day data. Finally, the user has the option of whether
to center covariate data or to use the raw observed data for analysis
and simulation and the percent of data to use in the training data for
model fitting. The rest of the data is withheld for validation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatClass$selectInputs()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{None}}{No arguments needed because passed in during class
instantiation.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A 'DatClass' object with complete user selections.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setupDat"></a>}}
\if{latex}{\out{\hypertarget{method-setupDat}{}}}
\subsection{Method \code{setupDat()}}{
This function calls the private methods for data gathering and
processing. The data gather step takes the user selected inputs
for the field, the response variables, and the data types ('mod_grid')
and exports the appropriate data into a a list, called 'mod_dat' with
lists, named  for each response variable ('yld' and/or
'pro') with each data type data from all fields selected.

The processing step goes through each data frame contained in the
nested 'mod_dat' list and trims the data based on
the user selections for the vegetation index and precipitation and
growing degree day sources. If the user selected to center the
covariate data, the values of each variable will be subtracted from
the mean of that variable. In this case, a named
vector of each variable and the mean will be created for reverting
back to observed values.

After this step, the data in 'mod_dat' is split into training and
validation sets based on the percentage of data the user selected
to include in the training dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatClass$setupDat()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{None}}{No arguments needed because passed in during class}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A named list with training and validation data, called
'mod_dat', for each response variable ('yld' and/or 'pro').
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSimDat"></a>}}
\if{latex}{\out{\hypertarget{method-getSimDat}{}}}
\subsection{Method \code{getSimDat()}}{
This function calls the private methods for data gathering and
processing. The gathering process takes the vector of simulation
years and gathers the appropriate 'sat' data from the OFPE database
and then processes the data using the same parameters as for the
data used in the model fitting process.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatClass$getSimDat(sim_years)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sim_years}}{Vector of years available in the database
to gather to simulate management outcomes in.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data.table with the user specified data for the simulation.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
