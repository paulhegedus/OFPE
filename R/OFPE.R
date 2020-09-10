#' OFPE: On-Farm Precision Experiments data management and analysis tools
#'
#' The OFPE package contains tools for supporting the OFPE data framework
#' developed at Montana State University. This package contains S3 and R6
#' objects for executing data management and analysis tasks, including;
#' creating an OFPE formatted spatial database, importing on-farm and
#' remotely sensed data, aggregating data from disparate sources, data
#' analysis and optimization of inputs, simulationg of management outcomes,
#' and generation of prescriptive application maps for optimized inputs.
#'
#' This package relies on a PostgreSQL database named 'OFPE'. The user is
#' assumed to have downloaded PostgreSQL and PostGIS. This process is
#' outlined on the OFPE Website \url{https://paulhegedus.github.io/OFPE-Website/}.
#'
#' The package follows Google's R Style Guide which follows the Tidyverse Style Guide.
#' However, there are a couple distinctions from Google's style guide. Rather
#' than naming functions with BigCamelCase, they are named with smallCamelCase,
#' while R6 classes are named with BigCamelCase and assigned to smallCamelCase
#' objects. Anything besides a class object or function is in snake_case.
#'
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stats sd
#' @useDynLib OFPE, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom maptools gcDestination
#' @importFrom grDevices topo.colors
#' @importFrom methods as
#' @importFrom data.table .SD
#' @importFrom stats na.omit
#' @importFrom stats median
#'
#' @seealso \url{https://sites.google.com/site/ofpeframework/}
#' @seealso \url{https://paulhegedus.github.io/OFPE-Website/}
#' @seealso \url{https://github.com/paulhegedus/OFPEDATA.git}
#' @seealso \url{https://google.github.io/styleguide/Rguide.html}
#' @seealso \url{https://style.tidyverse.org}
#'
#' @section General Functions:
#' DBCon
#'
#' @section Database Creation & Managements:
#' See Vignette 1 for database management and updating examples.
#'
#' BuildDB, ManageDB, ManageFarmers, ManageFarms, ManageFields
#'
#' @section Data Import:
#' See the Vignettes 2A and 2B for examples importing data from farms and
#' Google Earth Engine.
#'
#' The OFPE data import functions and R6 class generators...
#' ImportOF,
#'
#' The GEE data import functions and R6 class generators...
#'
#' @section Data Aggregation:
#' See Vignette 3 for examples aggregating data from disparate sources.
#'
#' The OFPE data aggregation functions and R6 class generators...
#'
#' @section Data Analysis and Simulation:
#' See Vignette 4 for an example on analyzing collected data and simulating
#' management outcomes.
#'
#' The OFPE data analysis and simulation functions and R6 class generators...
#'
#' @section Prescription Generation:
#' See Vignette 5 for an example on creating an optimized site-specific variable
#' rate input prescription.
#'
#' The OFPE prescription creation functions and R6 class generators...
#'
#' @docType package
#' @name OFPE
NULL
