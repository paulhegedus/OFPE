#' @title R6 Class for plotting outputs of the OFPE simulation.
#'
#' @description R6 class for generating maps and figures from observed data
#' in an OFPE database.
#'
#' This class ...
#'
#' Uses aggregated not raw data in the database.
#' can be used to pull data from database
#'
#'
#' is initialized for field(s) and year(s) observation.
#' If multiple years the mean for each location in the dataset is used.
#'
#' can make maps of any variable in aggregated data (uses mean if multiple years)
#' can crate scatterplots between two variables in data
#' can create histograms/pirate plots?/boxplots of any variable in data
#'
#' correlation stuff between yield and protein? map with correlation and overall
#' r & cv for each?
#'
#' @export
ObsOP <- R6::R6Class(
  "ObsOP",
  public = list(
    # initialize with data specs & db connection
    #   - methods run with inputs & so user must know col names to specify as arguments to make fgures
    #   - optional to have out_path upon initialization
    # data gather (dataframe in self$)
    #   - will take mean if multiple years
    #   - can krige protein to yield if available
    # NR calculator

    # map maker (use OFPE::plotMaps)
    # scatterplot maker
    #   - have r2/r in figure
    # histogram/boxplot/pirate plot maker
    #   - include summary stats in output list
    # correlation map maker
    #   - map the relationship between two variables across a field
    #     -> used to identify areas where observations are related
    #   - yld/pro correlations - only possible if pro data available

  ),
  private = list()
)




