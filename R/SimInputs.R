#' @title R6 Class for storing inputs for analysis, simulations, and prescriptions
#'
#' @description R6 class for for storing information needed mainly for the
#' simulation performed post-analysis and pre-prescription building. This
#' class stores inputs such as the model used for analysis and the subsequent
#' simulation, the number of iterations for the simulation, and the weather
#' scenario to simulate. This class also is used for storing inputs related
#' to the model and simulation outputters.
#'
#' Inputs can be supplied directly to this class during instantiation, however
#' this is NOT recommended except for advanced users. It is recommended that the
#' user supplies the database connection and uses the interactive selection
#' methods to select user inputs.
#'
#' This class is used for instantiating the selected model class and associated
#' outputter, activating and executing the 'LikeYear' class to identify the year
#' from the past that is most likely to reflect the upcoming year. This class
#' also is used to instantiate the simulation class.
#' @export
SimInputs <- R6::R6Class(
  "SimInputs",
  public = list(
    # mod -> will have different classes for different models
    # iterations
    # sim_year -> options or other -> year on record or LikeYear
    #



  )
)




