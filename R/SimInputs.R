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
#' This class is used for instantiating the simulation class and associated
#' outputter, activating and executing the 'LikeYear' class to identify the year
#' from the past that is most likely to reflect the upcoming year.
#' @export
SimInputs <- R6::R6Class(
  "SimInputs",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field sPr Provide the number of iterations to run the simulation. Without
    #' user inputs the default will be 100.
    sPr = NULL,
    #' @field opt The user also has the option to select the optimization method.
    #' There are two current options, 'Maximum' or 'Derivative'. Both methods select
    #' the optimum as-applied rate at each location in the field based on calculating
    #' the net-return at each point under a sequence of as-applied rates. Selecting
    #' 'Maximum' selects the optimim rate as the as-applied rate with the highest
    #' calculated net-return. The 'Derivative' approach calculates the first
    #' derivatives (slope) at each as-applied rate at each point and selects the
    #' optimum as the as-applied rate where an increase of one unit of the as-applied
    #' input results does not result in an increase in net-return that exceeds the
    #' cost of one unit of the as-applied input. For example, at one point in the field,
    #' if the net-return is \$100 at 20 lbs N/acre and \$101 at 21 lbs N/acre where N
    #' costs \$1.50 per lbs, the 'Maximum' approach would consider 21 lbs N/acre the
    #' optimum at that point because \$100 < \$101, whereas the 'Derivative' approach
    #' would select 20 lbs N/acre the optimum because the $1 increase between 20 and
    #' 21 lbs N/acre is less than the \$1.50 cost for the 1 unit increase in N.
    opt = NULL,
    #' @field sim_year Select the year(s) for which to simulate net-return outcomes for.
    #' Multiple years can be selected and will be simulated in sequence or one year can
    #' be selected. The user can only select from years that they have aggregated with
    #' the 'Satellite' option in the aggregation step of the OFPE data cycle. However,
    #' when using the interactive selection method of this class, the user also has
    #' the option of performing an analysis of all of the years the user has
    #' aggregated in the database and identifying the wettest, driest, average,
    #' and the predicted year from the past that will most resemble an upcoming
    #' year. See the 'LikeYear' class for more information on this option.
    sim_year = NULL,
    #' @field fcr Input the uniform as-applied rate the farmer would have applied
    #' to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre).
    fcr = NULL,
    #' @field AArateCutoff Select the maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    AArateCutoff = NULL,
    #' @field AAmin Select the minimum as-applied rate to simulate management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    AAmin = NULL,
    #' @field out_path Provide the path to the folder in which to store and
    #' save outputs from the simulation including figures and tables on
    #' management outcomes.
    out_path = NULL,

    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param sPr Provide the number of iterations to run the simulation. Without
    #' user inputs the default will be 100.
    #' @param opt The user also has the option to select the optimization method.
    #' There are two current options, 'Maximum' or 'Derivative'. Both methods select
    #' the optimum as-applied rate at each location in the field based on calculating
    #' the net-return at each point under a sequence of as-applied rates. Selecting
    #' 'Maximum' selects the optimim rate as the as-applied rate with the highest
    #' calculated net-return. The 'Derivative' approach calculates the first
    #' derivatives (slope) at each as-applied rate at each point and selects the
    #' optimum as the as-applied rate where an increase of one unit of the as-applied
    #' input results does not result in an increase in net-return that exceeds the
    #' cost of one unit of the as-applied input. For example, at one point in the field,
    #' if the net-return is \$100 at 20 lbs N/acre and \$101 at 21 lbs N/acre where N
    #' costs \$1.50 per lbs, the 'Maximum' approach would consider 21 lbs N/acre the
    #' optimum at that point because \$100 < \$101, whereas the 'Derivative' approach
    #' would select 20 lbs N/acre the optimum because the $1 increase between 20 and
    #' 21 lbs N/acre is less than the \$1.50 cost for the 1 unit increase in N.
    #' @param sim_year Select the year(s) for which to simulate net-return outcomes for.
    #' Multiple years can be selected and will be simulated in sequence or one year can
    #' be selected. The user can only select from years that they have aggregated with
    #' the 'Satellite' option in the aggregation step of the OFPE data cycle. However,
    #' when using the interactive selection method of this class, the user also has
    #' the option of performing an analysis of all of the years the user has
    #' aggregated in the database and identifying the wettest, driest, average,
    #' and the predicted year from the past that will most resemble an upcoming
    #' year. See the 'LikeYear' class for more information on this option.
    #' @param fcr Input the uniform as-applied rate the farmer would have applied
    #' to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre).
    #' @param AArateCutoff Select the maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    #' @param AAmin Select the minimum as-applied rate to simulate management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    #' @param out_path Provide the path to the folder in which to store and
    #' save outputs from the simulation including figures and tables on
    #' management outcomes.
    #' @return A new 'SimInputs' object.
    initialize = function(dbCon,
                          sPr = NULL,
                          opt = NULL,
                          sim_year = NULL,
                          fcr = NULL,
                          AArateCutoff = NULL,
                          AAmin = NULL,
                          out_path = NULL) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon

      if (!is.null(sPr)) {
        stopifnot(is.numeric(sPr))
        self$sPr <- sPr
      }
      if (!is.null(opt)) {
        stopifnot(is.character(opt))
        self$opt <- opt
      }
      if (!is.null(sim_year)) {
        stopifnot(is.numeric(sim_year))
        self$sim_year <- sim_year
      }
      if (!is.null(fcr)) {
        stopifnot(is.numeric(fcr))
        self$fcr <- fcr
      }
      if (!is.null(AArateCutoff)) {
        stopifnot(is.numeric(AArateCutoff))
        self$AArateCutoff <- AArateCutoff
      }
      if (!is.null(AAmin)) {
        stopifnot(is.numeric(AAmin))
        if (!is.null(AArateCutoff)) {
          stopifnot(self$AArateCutoff > AAmin)
        }
        self$AAmin <- AAmin
      }
      if (!is.null(out_path)) {
        stopifnot(is.character(out_path))
        self$out_path <- out_path
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the analysis and the
    #' subsequent simulations and prescription generation. The description below
    #' describes the process of interactively selecting the necessary parameters
    #' needed for the automated analysis, simulation, and prescription building.
    #'
    #' The user will need to select the functional form of a model to use for
    #' analysis. The user can select 'GAM' for a generalized additive model,
    #' 'Non-Linear Logistic' for a logistic function, or... if more model
    #' options are added, they will need to be added here. They will also
    #' need to provide the number of iterations to run the Monte Carlo
    #' simulation, as well as the optimization method to use.
    #'
    #' There are two current options, 'Maximum' or 'Derivative'. Both methods select
    #' the optimum as-applied rate at each location in the field based on calculating
    #' the net-return at each point under a sequence of as-applied rates. Selecting
    #' 'Maximum' selects the optimim rate as the as-applied rate with the highest
    #' calculated net-return. The 'Derivative' approach calculates the first
    #' derivatives (slope) at each as-applied rate at each point and selects the
    #' optimum as the as-applied rate where an increase of one unit of the as-applied
    #' input results does not result in an increase in net-return that exceeds the
    #' cost of one unit of the as-applied input. For example, at one point in the field,
    #' if the net-return is \$100 at 20 lbs N/acre and \$101 at 21 lbs N/acre where N
    #' costs \$1.50 per lbs, the 'Maximum' approach would consider 21 lbs N/acre the
    #' optimum at that point because \$100 < \$101, whereas the 'Derivative' approach
    #' would select 20 lbs N/acre the optimum because the $1 increase between 20 and
    #' 21 lbs N/acre is less than the \$1.50 cost for the 1 unit increase in N.
    #'
    #' Finally, the user has to select the year or sequence of years for which to
    #' assess management outcomes in under the various economic scenarios of the
    #' Monte Carlo simulation. The user must aggregate 'Satellite' data for each
    #' year that they would like to simulate. See the 'AggInputs' class and
    #' Vignette 3 for an overview of the aggregations step. Alternatively,
    #' the user can select to perform an analysis of all of the years that
    #' they have aggregated 'Satellite' data for to identify the wettest,
    #' driest, and average years from the historic record the user has
    #' generated. It also will identify the year most expected to follow
    #' a specified year based off of the trend in the historic data the user
    #' has gatherd.
    #' @param farmername Provide the name of the farmer who owns the field that is
    #' the target of analysis/simulation/prescription generation.
    #' @param fieldname Provide the name of the field that is
    #' the target of analysis/simulation/prescription generation.
    #' @return A completed 'DatInputs' object.
    selectInputs = function(farmername, fieldname) {
      private$.selectIter()
      private$.selectOpt()
      private$.selectSimYear(self$dbCon$db, farmername, fieldname)
      private$.selectFcr()
      private$.selectAArateCutoff()
      private$.selectAAmin()
      private$.selectOutPath()
    }
  ),
  private = list(
    .selectIter = function() {
      self$sPr <- as.integer(
        readline("Provide the number of iterations to run the simulation: ")
      )
    },
    .selectOpt = function() {
      opt <- as.character(select.list(
        c("Maximum", "Derivative"),
        title = "Select the optimization method to use."
      ))
      self$opt <- ifelse(opt == "Maximum", "max",
                         ifelse(opt == "Derivative", "deriv", NA))
    },
    .selectSimYear = function(db, farmername, fieldname) {
      sim_year <- as.character(select.list(
            c("Select Year", "Run 'LikeYear' Analysis"),
            title = "Select whether to choose a year(s) to simulate based off of 'Satellite' data aggrgated in the user's OFPE database. Otherwise, elect to run the 'LikeYear' analysis to identify and select from the wettest, driest, average, and predicted year to follow a user selected year. "
      ))
      if (sim_year == "Select Year") {
        sat_exist <- as.logical(
          DBI::dbGetQuery(
            db,
            paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'sat')")
          )
        )
        if (sat_exist) {
          sim_years <- as.list(fieldname) %>%
            `names<-`(fieldname)
          for (i in 1:length(sim_years)) {
            sim_years[[i]] <- unique(DBI::dbGetQuery(
              db,
              paste0("SELECT DISTINCT year
                       FROM ", farmername, "_a.sat sat
                       WHERE sat.field = '", sim_years[[i]], "'")
            )$year)
          }
          for (i in 1:length(sim_years)) {
            for (j in 1:length(sim_years)) {
              if (i != j) {
                sim_years[[i]] <-
                  sim_years[[i]][sim_years[[i]] %in% sim_years[[j]]]
              }
            }
          }
          sim_years <- do.call(c, sim_years) %>% unique

          self$sim_year <- as.numeric(select.list(
            sim_years,
            multiple = TRUE,
            title = paste0("Select year(s) to get simulate weather conditions for.")
          ))
          # self$predClim <-
          #   paste0(
          #     "Like",
          #     inputs$predClim
          #   )
        } else {
          stop("No satellite data exists. Return to the aggregation step (Vignette 3) and aggregate 'Satellite' data.")
        }
      }else{

        # TODO
        # do 'LikeYear' analysis and selectYears method
        # self$sim_year <- choice from 'LikeYear' output

      }
    },
    .selectFcr = function() {
      self$fcr <- as.numeric(readline(
        "Input the uniform as-applied rate the farmer would have applied to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre): "
      ))
    },
    .selectAArateCutoff = function() {
      self$AArateCutoff <- as.numeric(readline(
        "Select the maximum as-applied rate to simulate management outcomues to (i.e. 200 lbs N or seed per acre): "
      ))
    },
    .selectAAmin = function() {
      self$AAmin <- as.numeric(readline(
        "Select the minimum as-applied rate to simulate management outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre): "
      ))
      stopifnot(self$AArateCutoff > self$AAmin)
    },
    .selectOutPath = function() {
      self$out_path <- as.character(readline(
        "Provide the path to a folder in which to save simulation outputs (i.e. '~/path/to/folder' or 'C:/path/to/folder'): "
      ))
    }
  )
)




