% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildDB.R
\name{BuildDB}
\alias{BuildDB}
\title{R6 Class for building OFPE database}
\source{
\url{https://trac.osgeo.org/postgis/wiki/UsersWikiCreateFishnet}
}
\description{
R6 class for for loading required extensions and
building the the OFPE database schemas and initial tables.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dbCon}}{Database connection object connected to an OFPE formatted
database, see DBCon class.}

\item{\code{postgis_version}}{Version of PostGIS installed (i.e. 2.5, 3.0).}

\item{\code{farmers}}{Vector of farmer names to use for building initial schemas.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{BuildDB$new()}}
\item \href{#method-buildDatabase}{\code{BuildDB$buildDatabase()}}
\item \href{#method-.loadExtensions}{\code{BuildDB$.loadExtensions()}}
\item \href{#method-.buildSchemas}{\code{BuildDB$.buildSchemas()}}
\item \href{#method-.buildTables}{\code{BuildDB$.buildTables()}}
\item \href{#method-.buildSpatialIndexes}{\code{BuildDB$.buildSpatialIndexes()}}
\item \href{#method-clone}{\code{BuildDB$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a database builder object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$new(dbCon, postgis_version, farmers)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dbCon}}{dbCon Database connection object connected to an OFPE formatted
database, see DBCon class.}

\item{\code{postgis_version}}{Version of PostGIS installed (i.e. 2.5, 3.0).}

\item{\code{farmers}}{Vector of farmer names to use for building initial schemas.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new 'BuildDB' object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-buildDatabase"></a>}}
\if{latex}{\out{\hypertarget{method-buildDatabase}{}}}
\subsection{Method \code{buildDatabase()}}{
Execute database builder functions. Runs, .loadExtensions(), .buildSchemas(),
and .buildTables().
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$buildDatabase()}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{None}}{No arguments needed because they are provided during class
initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.loadExtensions"></a>}}
\if{latex}{\out{\hypertarget{method-.loadExtensions}{}}}
\subsection{Method \code{.loadExtensions()}}{
Loads extensions needed for OFPE database functions such
as PostGIS tools and a function for generating a net across an area of
interest (see source). No arguments needed if provided on class
initialization, otherwise arguments provided take precedence.
ST_CreateFishnet was written by the PostGIS team and built in this
method, see source for reference and credit. The dot indicates that this
function would be private if not for documentations sake.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$.loadExtensions(db = NULL, postgis_version = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{db}}{Connection to a database.}

\item{\code{postgis_version}}{PostGIS version installed.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Enabled database extensions.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.buildSchemas"></a>}}
\if{latex}{\out{\hypertarget{method-.buildSchemas}{}}}
\subsection{Method \code{.buildSchemas()}}{
Builds the skeleton of the database. Two schemas for each farmer supplied
is built; one for holding raw data collected on-farms from equipment, and
another for storing aggregated data files. A general schema call 'all_farms'
is also created to hold data that is not farmer specific or farm-wide data.
No arguments needed if provided on class initialization, otherwise arguments
provided take precedence. The dot indicates that this function would be
private if not for documentations sake. Note that farmer names are converted
to lowercase upon addition to the database if not already.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$.buildSchemas(db = NULL, farmers = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{db}}{Connection to a database.}

\item{\code{farmers}}{Vector of farmer names.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Built database schemas.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.buildTables"></a>}}
\if{latex}{\out{\hypertarget{method-.buildTables}{}}}
\subsection{Method \code{.buildTables()}}{
Builds initial tables in the 'all_farms' schema. None of these tables
are filled with information. These tables will be filled with methods
in the UpdateDB class. The tables created are 'farmers' with an integer
column for a farmer ID and column for the farmer name. The 'farms' table
contains an ID column, and columns for the name of the farm and the ID of
the farmer who owns it. The 'fields' table contains an ID column for
sections of the field, a field ID, the ID of the farm the field falls
within, the farmer's ID, and the name of the field. Geometry columns are
also added to the 'farms' and 'fields' tables. No arguments needed if
provided on class initialization, otherwise arguments provided take
precedence. The dot indicates that this function would be private if not
for documentations sake.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$.buildTables(db = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{db}}{Connection to a database.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Built 'all_farms' tables
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.buildSpatialIndexes"></a>}}
\if{latex}{\out{\hypertarget{method-.buildSpatialIndexes}{}}}
\subsection{Method \code{.buildSpatialIndexes()}}{
Builds spatial indexes on the tabels created in the 'all_farms' schema.
These indexes are used to speed up spatial queries and are a PostGIS tool.
This means that this function must be run after loading the extensions and
creating the tables. No arguments needed if provided on class initialization,
otherwise arguments provided take precedence. The dot indicates that this
function would be private if not for documentations sake.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$.buildSpatialIndexes(db = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{db}}{Connection to a database.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Built spatial indexes on the 'farms' and 'fields' tables.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BuildDB$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
