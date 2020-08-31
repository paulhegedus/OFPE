#' @title R6 Class for storing inputs and executing the OFPE simulation
#'
#' @description R6 class for for storing information needed for the
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
#' This class performs the Monte Carlo simulation to compare management outcomes.
#' The inputs also determine whether to activate and execute methods of the 'LikeYear'
#' class to identify the year from the past that is most likely to reflect the upcoming year.
#' @export
SimClass <- R6::R6Class(
  "SimClass",
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
    #' @field sim_years Select the year(s) for which to simulate net-return outcomes for.
    #' Multiple years can be selected and will be simulated in sequence or one year can
    #' be selected. The user can only select from years that they have aggregated with
    #' the 'Satellite' option in the aggregation step of the OFPE data cycle. However,
    #' when using the interactive selection method of this class, the user also has
    #' the option of performing an analysis of all of the years the user has
    #' aggregated in the database and identifying the wettest, driest, average,
    #' and the predicted year from the past that will most resemble an upcoming
    #' year. See the 'LikeYear' class for more information on this option.
    sim_years = NULL,
    #' @field fs Input the uniform as-applied rate the farmer would have applied
    #' to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre).
    #' fs = Farmer Selected.
    fs = NULL,
    #' @field AArateCutoff Select the maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    AArateCutoff = NULL,
    #' @field AAmin Select the minimum as-applied rate to simulate management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    AAmin = NULL,
    #' @field SAVE Logical, whether to save figures. Autofilled to FALSE if
    #' a user selects NA in the 'out_path' or is NULL. Autofilled to TRUE
    #' otherwise. This will be triggered to FALSE if the user passes FALSE
    #' as the only argument to the 'setupOP' method. The user can also select
    #' to save/not save individual figures.
    SAVE = NULL,
    #' @field out_path Provide the path to the folder in which to store and
    #' save outputs from the simulation including figures and tables on
    #' management outcomes. Type NA to not create any folders. You will not be
    #' able to save any outputs. (Note, even if a path is provided, the user
    #' can pass FALSE as the sole argument to the 'setupOP' method to prevent
    #' the creation of folders. This will automatically prevent any plots to
    #' be saved.)
    out_path = NULL,

    #' @field datClass DatClass R6 object that has been initialized. This
    #' object executes the gathering and processing of data for the simulation.
    datClass = NULL,
    #' @field modClass ModClass R6 object that has been initialized and with
    #' fitted models. The fitted models in this object are used to predict
    #' crop responses at each location in the simulation data for all
    #' experimental rates in the user specified range.
    modClass = NULL,
    #' @field econDat EconDat R6 object that has been initialized and setup
    #' with all economic parameters and data used in the Monte Carlo simulation.
    econDat = NULL,
    #' @field sim_list A list for each user selected simulation year containing
    #' n lists of that year's satellite data for n experimental rates. This
    #' can be thought of as an array with the x dimension representing each
    #' centroid location of the simulation year 'sat' data, and the y dimension
    #' as each covariate value. The z dimension replicates the x and y dimensions
    #' for each experimental rate from 'AAmin' to 'AArateCutoff'. Predicted
    #' yield and/or protein and net-returns are estimated for every data point
    #' in the array (list of lists), and are used to estimate net-returns for
    #' every data point, which are used to identify the optimum experimental
    #' rate at each point to generate a site-specific optimum rate map and
    #' for identifying the full field optimum rate.
    sim_list = NULL,
    #' @field fieldsize The fieldsize, in acres, of the selected fields,
    #' gathered from the OFPE database.
    fieldsize = NULL,
    #' @field sim_out A list named for each user selected simulation year,
    #' each containing a named list containing ... TODO ...
    sim_out = NULL,

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
    #' @param sim_years Select the year(s) for which to simulate net-return outcomes for.
    #' Multiple years can be selected and will be simulated in sequence or one year can
    #' be selected. The user can only select from years that they have aggregated with
    #' the 'Satellite' option in the aggregation step of the OFPE data cycle. However,
    #' when using the interactive selection method of this class, the user also has
    #' the option of performing an analysis of all of the years the user has
    #' aggregated in the database and identifying the wettest, driest, average,
    #' and the predicted year from the past that will most resemble an upcoming
    #' year. See the 'LikeYear' class for more information on this option.
    #' @param fs Input the uniform as-applied rate the farmer would have applied
    #' to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre).
    #' fs = Farmer Selected.
    #' @param AArateCutoff Select the maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    #' @param AAmin Select the minimum as-applied rate to simulate management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    #' @param SAVE Logical, whether to save figures. Autofilled to FALSE if
    #' a user selects NA in the 'out_path' or is NULL. Autofilled to TRUE
    #' otherwise. This will be triggered to FALSE if the user passes FALSE
    #' as the only argument to the 'setupOP' method. The user can also select
    #' to save/not save individual figures.
    #' @param out_path Provide the path to the folder in which to store and
    #' save outputs from the simulation including figures and tables on
    #' management outcomes. Type NA to not create any folders. You will not be
    #' able to save any outputs. (Note, even if a path is provided, the user
    #' can pass FALSE as the sole argument to the 'setupOP' method to prevent
    #' the creation of folders. This will automatically prevent any plots to
    #' be saved.)
    #' @return A new 'SimClass' object.
    initialize = function(dbCon,
                          sPr = NULL,
                          opt = NULL,
                          sim_years = NULL,
                          fs = NULL,
                          AArateCutoff = NULL,
                          AAmin = NULL,
                          SAVE = NULL,
                          out_path = NULL) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon

      if (!is.null(sPr)) {
        stopifnot(is.numeric(sPr))
        self$sPr <- sPr
      }
      if (!is.null(opt)) {
        stopifnot(is.character(opt),
                  any(grepl("Maximum|Derivative", opt)))
        self$opt <- ifelse(opt == "Maximum", "max",
                           ifelse(opt == "Derivative", "deriv", NA))

      }
      if (!is.null(sim_years)) {
        stopifnot(is.numeric(sim_years))
        self$sim_years <- sim_years
      }
      if (!is.null(fs)) {
        stopifnot(is.numeric(fs))
        self$fs <- fs
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
      if (!is.null(SAVE)) {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }
      if (!is.null(out_path)) {
        stopifnot(is.character(out_path))
        self$out_path <- out_path

        if (is.na(self$out_path) | is.null(self$out_path)) {
          self$SAVE <- FALSE
        }
      }
      # TODO
      # do c++ functions need to be sourced here??
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
    #' @return A 'SimClass' object with completed .
    selectInputs = function(farmername, fieldname) {
      private$.selectIter()
      private$.selectOpt()
      private$.selectSimYear(self$dbCon$db, farmername, fieldname)
      private$.selectFcr()
      private$.selectAArateCutoff()
      private$.selectAAmin()
      private$.selectOutPath()
    },
    #' @description
    #' Method used to prepare the class for the simulation. This method takes
    #' a DatClass, ModClass, and EconDat R6 classes that the user has initialized
    #' and/or executed the mehtods for. The DatClass object is used to execute
    #' a method with the user selected simulation year(s) to gather this data
    #' from the database and process in the same way data was processed for
    #' the model fitting. The ModClass' fitted models are used in the simulation
    #' to predict crop responses at each location of the simulation data for the
    #' user specified range of experimental rates. The EconDat object contains
    #' the economic parameters and data used in the Monte Carlo simulation to
    #' compare management outcomes for each simulation year under a range of
    #' economic conditions.
    #' @param datClass DatClass R6 object that has been initialized. This
    #' object executes the gathering and processing of data for the simulation.
    #' @param modClass ModClass R6 object that has been initialized and with
    #' fitted models. The fitted models in this object are used to predict
    #' crop responses at each location in the simulation data for all
    #' experimental rates in the user specified range.
    #' @param econDat EconDat R6 object that has been initialized and setup
    #' with all economic parameters and data used in the Monte Carlo simulation.
    #' @return Processed simulation data in the DatClass.
    setupSim = function(datClass, modClass, econDat) {
      stopifnot(
        any(grepl("DatClass", class(datClass))),
        !is.null(datClass$dbCon),
        !is.null(datClass$fieldname),
        !is.null(datClass$farmername),
        !is.null(datClass$gdd_source),
        !is.null(datClass$prec_source),
        !is.null(datClass$center),
        !is.null(datClass$dat_used),
        any(grepl("ModClass", class(modClass))),
        !is.null(modClass$mod_list),
        # needs not null mod_list, ...
        any(grepl("EconDat", class(econDat))),
        !is.null(econDat$FC),
        !is.null(econDat$ssAC),
        !is.null(econDat$Prc),
        !is.null(econDat$B0pd),
        !is.null(econDat$B1pd),
        !is.null(econDat$B2pd)
      )
      self$datClass <- datClass
      self$modClass <- modClass
      self$econDat <- econDat
      self$datClass$getSimDat(self$sim_years)

      self$fieldsize <- private$.gatherFieldSize()
    },
    #' @description
    #' Method for executing the Monte Carlo simulation to compare management
    #' scenarios for each year the user selected to simulate in. The method
    #' begins by creating a list of lists (essentially an array) for each
    #' simulated year's data for the length of the range of experimental
    #' rates to simulate. A column for the experimental rate and for the
    #' net-returns from each of the management scenarios is added to
    #' each data table.
    #'
    #' Then, the fitted models are used to predict the crop responses
    #' for every location in the simulated data for every experimental
    #' rate. This is repeated for every simulation year.
    #'
    #' The simulation is executed for each year to create a list of outputs
    #' for each user selected simulation year. For the length of the number
    #' of iterations specified by the user, an economic scenario is selected
    #' from the user provided/default economic conditions to select a cost
    #' of the experimental variable and the price received for conventionally
    #' or organically grown crops. Net-return for every point under every
    #' experimental rate is calculated. These net-returns are used to
    #' calculate the total net-return under each management scenario.
    #'
    #' The management scenarios reported include; NRmin: if the minimum rate ('AAmin')
    #' were applied to the entire field (e.g. 0 lbs N/ac), NRfs: if the rate the farmer
    #' would have applied without experimentation was applied (i.e. business as
    #' usual), NRact: the 'actual' net-return in the simulated year if the experiment
    #' in the most recent of the user selected years for analysis was applied (e.g. if
    #' you fit models on observed data from 2016 and 2018, NRact is calculated based on
    #' the as-applied values in 2018), NRssopt: the site-specific profit maximizing
    #' experimental rate, NRffopt: the full-field profit maximizing experimental rate
    #' which is only identifiable with site-specific experimentation but still represents
    #' a uniform application across the field, and finally NRopp: the net-return if the
    #' crop was grown in the opposite system type of the user selected system type (e.g.
    #' if conventionally grown wheat, this is the net-return for organically grown wheat).
    #' If the system type is conventional, NRopp is from the application of 'AAmin' (e.g. 0
    #' lbs N/ac) otherwise if the system is organic, NRopp is from the application of the
    #' farmer selected rate with the conventional price.
    #'
    #' NOTE: Depending on the size/number of the fields, the number of iterations to run
    #' the simulation, and the number of selected simulation years, this may take some time
    #' (i.e. 15 minutes - hours)...
    #' @param None All required arguments supplied upon the initialization
    #' and setup methods.
    #' @return A list of lists ('sim_out') with output from the simulation for each selected
    #' simulation year.
    executeSim = function() {
      ## TODO - TEMP
      temp_rr <- nrow(self$datClass$sim_dat[[1]])
      self$datClass$sim_dat <- lapply(self$datClass$sim_dat,
                                      function(x) x[runif(50, 1, temp_rr), ])
      ## END TEMP

      ## for all sim years rep sim dat for length of EXP rate range & add EXP val
      EXPvec <- seq(self$AAmin, self$AArateCutoff, 1)
      self$sim_list <- lapply(self$datClass$sim_dat,
                              function(x) rep(list(x), length(EXPvec))) %>%
        lapply(private$.setupSimDat, EXPvec)

      ## for each location for each rate for each sim year predict repsonses
      self$sim_list <- lapply(self$sim_list,
                              private$.simResponses,
                              self$datClass$respvar)
      ## do simulation & find opt. etc. for each sim_year save to sim_out list
      self$sim_out <- lapply(self$sim_list,
                             private$.runSim) %>%
        `names<-`(names(self$sim_list))
      for (i in 1:length(self$sim_out)) {
        self$sim_list[[i]] <- self$sim_out[[i]]$sim_list
        self$sim_out[[i]]$sim_list <- NULL
      }
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
      sim_years <- as.character(select.list(
            c("Select Year", "Run 'LikeYear' Analysis"),
            title = "Select whether to choose a year(s) to simulate based off of 'Satellite' data aggrgated in the user's OFPE database. Otherwise, elect to run the 'LikeYear' analysis to identify and select from the wettest, driest, average, and predicted year to follow a user selected year. "
      ))
      if (sim_years == "Select Year") {
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

          self$sim_years <- as.numeric(select.list(
            sim_years,
            multiple = TRUE,
            title = paste0("Select year(s) to get simulate weather conditions for.")
          ))
        } else {
          stop("No satellite data exists. Return to the aggregation step (Vignette 3) and aggregate 'Satellite' data.")
        }
      }else{

        # TODO
        # do 'LikeYear' analysis and selectYears method
        # self$sim_years <- choice from 'LikeYear' output

      }
    },
    .selectFcr = function() {
      self$fs <- as.numeric(readline(
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
      self$SAVE <- as.character(select.list(
        c(TRUE, FALSE),
        title = "Select whether to save output plots from the simulation. "
      ))
      self$out_path <- as.character(readline(
        "Provide the path to a folder in which to save simulation outputs (i.e. '~/path/to/folder' or 'C:/path/to/folder'). Type NA to prevent any folders from being created.: "
      ))
      if (is.na(self$out_path) | is.null(self$out_path)) {
        self$SAVE <- FALSE
      }
    },
    .setupSimDat = function(sub_sim_list, EXPvec) {
      sub_sim_list <- mapply(private$.addCols,
                             sub_sim_list,
                             EXPvec,
                             SIMPLIFY = FALSE)
      return(sub_sim_list)
    },
    .addCols = function(dat, EXPval) {
      dat$EXP <- EXPval
      names(dat)[grep("EXP", names(dat))] <- self$datClass$expvar
      dat$NR <- NA
      dat$NRmin <- NA
      dat$NRopp <- NA
      dat$NRfs <- NA
      return(dat)
    },
    .simResponses = function(sub_sim_list, respvar) {
      for (i in 1:length(respvar)) {
        sub_sim_list <- lapply(sub_sim_list,
                               private$.predResps,
                               self$modClass$mod_list[[i]],
                               respvar[i])
      }
      return(sub_sim_list)
    },
    .predResps = function(dat, mod, respvar) {
      dat$pred <- mod$predResps(dat, mod$mod)
      names(dat)[grep("^pred$", names(dat))] <- paste0("pred_", respvar)
      return(dat)
    },
    .runSim = function(sim_list) {
      Bp.var <- matrix(0, nrow = self$sPr, ncol = 9)
      colnames(Bp.var) <- c("BaseP", "EXP.cost", "NR.ssopt", "NR.min", "NR.fs",
                            "ffopt.EXPrate", "NR.ffopt", "NR.act", "NR.opp")
      NRoptList <- rep(list(NA), self$sPr)
      NRffmaxList <- rep(list(NA), self$sPr)
      rr <- nrow(sim_list[[1]])
      sim_list_names <- names(sim_list[[1]])

      for (bp in 1:self$sPr) {
        rp <- as.integer(runif(1, 1, length(self$econDat$Prc$Year)))
        Bp_col <- grep(self$datClass$sys_type, names(self$econDat$Prc))
        Bp <- as.numeric(self$econDat$Prc[rp, Bp_col])
        CEXP <- as.numeric(self$econDat$Prc[rp, "cost"])
        BpOpp_col <- grep(self$datClass$opp_sys_type, names(self$econDat$Prc))
        BpOpp <- as.numeric(self$econDat$Prc[rp, BpOpp_col])
        sim_list <-
          lapply(sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
          lapply(function(x) apply(x, 2, as.numeric)) %>%
          lapply(OFPE::NRcalcCpp,
                 Bp,
                 self$econDat$B0pd,
                 self$econDat$B1pd,
                 self$econDat$B2pd,
                 CEXP,
                 BpOpp,
                 self$econDat$FC,
                 self$fs,
                 self$econDat$ssAC,
                 ifelse(any(self$datClass$respvar == "pro"), 1, 0),
                 rr,
                 grep(paste0("^", self$datClass$expvar, "$"), sim_list_names) - 1,
                 grep("pred_yld", sim_list_names) - 1,
                 grep("pred_pro", sim_list_names) - 1,
                 grep("^NR$", sim_list_names) - 1,
                 grep("NRmin", sim_list_names) - 1,
                 grep("NRopp", sim_list_names) - 1,
                 grep("NRfs", sim_list_names) - 1,
                 self$AAmin)
        sim_list <- lapply(sim_list, function(x) data.table::as.data.table(x) %>%
                        `names<-`(sim_list_names))
        NRff <- data.frame(EXP.rate = self$AAmin:self$AArateCutoff, NR.ff = NA)
        NRff$NR.ff <- lapply(sim_list, function(x) sum(x$NR, na.rm = TRUE))
        NRff <- apply(NRff, 2, as.numeric) %>% as.data.frame()
        if (self$opt == "max") {
          NRffmax <- subset(NRff,
                            NRff[, "NR.ff"] == max(na.omit(NRff[, "NR.ff"])))
        } else {
          if (self$opt == "deriv") {
            NRff <- NRff %>% `names<-`(NULL) %>% as.matrix()
            NRffmax <-OFPE::derivFFoptCpp(NRff,
                                          nrow(NRff),
                                          self$fieldsize,
                                          CEXP) %>%
              as.data.frame() %>%
              `names<-`(c("EXP.rate", "NR.ff"))
          }
        }
        ffopt_rate <- NRffmax[1, "EXP.rate"]
        # TODO: need to add NR.ffopt & EXP.rate.ffopt
        NRopt <- data.frame(BaseP = rep(Bp, rr),
                            EXP.cost = rep(CEXP, rr),
                            x = sim_list[[1]]$x,
                            y = sim_list[[1]]$y,
                            row = sim_list[[1]]$row,
                            col = sim_list[[1]]$col,
                            field = sim_list[[1]]$field,
                            EXP.rate.ssopt = NA,
                            NR.ssopt = NA,
                            NR.min = sim_list[[1]]$NRmin,
                            NR.opp = ifelse(self$datClass$sys_type == "conv",
                                            sim_list[[1]]$NRopp,
                                            sim_list[[self$fs + 1]]$NRopp),
                            NR.fs = sim_list[[self$fs + 1]]$NRfs,
                            yld.opt = NA,
                            yld.min = sim_list[[1]]$pred_yld,
                            yld.fs = sim_list[[self$fs + 1]]$pred_yld,
                            pro.opt = NA,
                            pro.min = sim_list[[1]]$pred_pro,
                            pro.fs = sim_list[[self$fs + 1]]$pred_pro,
                            NR.ffopt = sim_list[[ffopt_rate + 1]]$NR,
                            EXP.rate.ffopt = ffopt_rate)
        NRopt[,c("EXP.rate.ssopt", "NR.ssopt", "yld.opt", "pro.opt")] <-
          private$.getNRopt(sim_list,
                            self$AArateCutoff,
                            CEXP,
                            self$opt,
                            self$AAmin)
        NRopt <- apply(NRopt, 2, as.numeric)

        ## Fill in Bp.var
        Bp.var[bp, "BaseP"] <- Bp
        Bp.var[bp, "EXP.cost"] <- CEXP
        Bp.var[bp, "NR.ssopt"] <- mean(NRopt[, "NR.ssopt"], na.rm = T)
        Bp.var[bp, "NR.min"] <- mean(NRopt[, "NR.min"], na.rm = T)
        Bp.var[bp, "NR.fs"] <- mean(NRopt[, "NR.fs"], na.rm = T)
        Bp.var[bp, "ffopt.EXPrate"] <- NRffmax[, "EXP.rate"]
        Bp.var[bp, "NR.ffopt"] <- NRffmax[, "NR.ff"] / rr
        Bp.var[bp, "NR.opp"] <- mean(NRopt[, "NR.opp"], na.rm = T)
        NRopt <- private$.calcNRact(NRopt, sim_list[[1]]$year[1], Bp, CEXP)
        Bp.var[bp, "NR.act"] <- mean(NRopt[, "NR.act"], na.rm = T)

        # need to change field back to real words
        NRopt <- data.table::as.data.table(NRopt)
        NRopt$field <-
          self$datClass$fieldname_codes[match(NRopt$field, self$datClass$fieldname_codes$field_code), "field"]
        NRoptList[[bp]] <- NRopt
        NRoptList[[bp]]$sim <- bp
        NRffmaxList[[bp]] <- data.table::as.data.table(NRffmax)
        NRffmaxList[[bp]]$sim <- bp
      }
      out_list <- list(Bp.var = data.table::as.data.table(Bp.var),
                       NRopt = data.table::rbindlist(NRoptList),
                       NRffmax = data.table::rbindlist(NRffmaxList),
                       sim_list = sim_list)
      return(out_list)
    },
    .getNRopt = function(sim_list, AArateCutoff, CEXP, opt, AAmin) {
      NRoptDat <- data.frame(EXP.rate.ssopt = rep(NA, nrow(sim_list[[1]])),
                             NR.ssopt = rep(NA, nrow(sim_list[[1]])))
      # get NR optimum and EXP rate optimum
      NRdf <- matrix(nrow = nrow(sim_list[[1]]),
                     ncol = (length(AAmin:AArateCutoff))) %>%
        as.data.frame()
      names(NRdf) <- AAmin:AArateCutoff
      NRdf[, 1:ncol(NRdf)] <- lapply(sim_list, private$.getNR)

      if (opt == "max") {
        NRoptDat$NR.ssopt <- do.call(pmax, NRdf)
        NRoptDat$EXP.rate.ssopt <-
          colnames(NRdf)[max.col(NRdf, ties.method = "first")]
        NRoptDat <- sapply(NRoptDat, as.numeric) %>% as.data.frame()
      } else {
        if (opt == "deriv") {
          Nrates <- data.frame(Nrates = AAmin:AArateCutoff)
          rr <- nrow(NRdf)
          cc <- ncol(NRdf)
          NRdf <- as.matrix(NRdf)
          NRoptDat <- as.matrix(NRoptDat)
          Nrates <- as.matrix(Nrates)
          colnames(NRdf) <- NULL
          colnames(NRoptDat) <- NULL
          colnames(Nrates) <- NULL
          NRoptDat <- OFPE::derivNRoptCpp(NRdf, NRoptDat, Nrates, rr, cc, CEXP) %>%
            as.data.frame() %>%
            `names<-`(c("EXP.rate.ssopt", "NR.ssopt"))
        }
      }
      NRoptDat$yld.opt <- NA
      NRoptDat$pro.opt <- NA
      loc_correct <- AAmin - 1
      for (i in 1:nrow(NRoptDat)) {
        NRoptDat$yld.opt[i] <-
          sim_list[[NRoptDat$EXP.rate.ssopt[i] - loc_correct]]$pred_yld[i]
        NRoptDat$pro.opt[i] <-
          sim_list[[NRoptDat$EXP.rate.ssopt[i] - loc_correct]]$pred_pro[i]
      }
      return(NRoptDat)
    },
    .getNR = function(dat) {
      NRcol <- dat$NR
      return(NRcol)
    },
    .calcNRact = function(NRopt, year, Bp, CEXP) {
      NRopt <- data.table::as.data.table(NRopt)
      NRopt$cell_id <- paste0(NRopt$row, "_", NRopt$col)

      dat_index <- grep(year, names(self$datClass$sim_dat))
      dat <- self$datClass$sim_dat[[dat_index]]
      dat$cell_id <- paste0(dat$row, "_", dat$col)
      dat$field <-
        self$datClass$fieldname_codes[
          match(dat$field, self$datClass$fieldname_codes$field_code), "field"
        ]
      # use data from yield if possible, else, use protein
      dat_index <- ifelse(any(grepl("yld", names(self$datClass$mod_dat))),
                          "yld",
                          "pro")
      dat_index <- grep(dat_index, names(self$datClass$mod_dat))
      obs_dat <- self$datClass$mod_dat[[dat_index]]
      obs_dat <- data.table::rbindlist(obs_dat)
      year_index <- by(obs_dat$year, obs_dat$field, unique) %>%
        lapply(as.character) %>%
        lapply(as.numeric) %>%
        lapply(max)
      year_index <- data.frame(field = names(year_index),
                               year = do.call(rbind, year_index))
      temp_list <- rep(list(NA), nrow(year_index))
      dat_list <- rep(list(NA), nrow(year_index))
      NRopt_list <- rep(list(NA), nrow(year_index))
      for (i in 1:length(temp_list)) {
        NRopt_list[[i]] <- NRopt[which(NRopt$field ==
                                         self$datClass$fieldname_codes[i, "field_code"]), ]
        dat_list[[i]] <- dat[which(dat$field == year_index[i, "field"]), ]
        temp_list[[i]] <-
          obs_dat[which(obs_dat$year == year_index[i, "year"] &
                          obs_dat$field == year_index[i, "field"]), ]
        exp_index <- grep(paste0("^", self$datClass$expvar, "$"),
                          names(temp_list[[i]]))
        temp_list[[i]] <-
          by(temp_list[[i]][, exp_index],
             temp_list[[i]]$cell_id,
             median,
             na.rm = TRUE)
        temp_list[[i]] <-
          data.frame(cell_id = row.names(temp_list[[i]]),
                     exp = as.numeric(temp_list[[i]]))
        temp_list[[i]]$field <- year_index[i, "field"]
        dat_list[[i]]$exp <-
          temp_list[[i]][match(dat_list[[i]]$cell_id,
                               temp_list[[i]]$cell_id), "exp"]

        names(dat_list[[i]])[grep("exp", names(dat_list[[i]]))] <-
          self$datClass$expvar
        for (j in 1:length(self$datClass$respvar)) {
          dat_list[[i]] <- private$.predResps(dat_list[[i]],
                                    self$modClass$mod_list[[j]],
                                    self$datClass$respvar[j])
        }
        names(dat_list[[i]])[grep(self$datClass$expvar,
                                  names(dat_list[[i]]))] <- "exp"
        dat_list[[i]] <- private$.NRcalc(dat_list[[i]],
                               ifelse(any(self$datClass$respvar == "pro"), 1, 0),
                               Bp,
                               self$econDat$B0pd,
                               self$econDat$B1pd,
                               self$econDat$B2pd,
                               CEXP,
                               self$econDat$FC,
                               self$econDat$ssAC)
        NRopt_list[[i]]$NR.act <- dat_list[[i]][
          match(NRopt_list[[i]]$cell_id, dat_list[[i]]$cell_id), "NR"
        ] - self$econDat$ssAC
        NRopt_list[[i]]$cell_id <- NULL
      }
      NRopt <- data.table::rbindlist(NRopt_list) %>%
        as.matrix() %>%
        apply(2, as.numeric)
      return(NRopt)
    },
    .gatherFieldSize = function() {
      fields <- as.list(self$datClass$fieldname)
      for (i in 1:length(fields)) {
        fields[[i]] <- DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT area
                 FROM all_farms.fields
                 WHERE fieldname = '", fields[[i]], "'")
        )$area %>% sum()
      }
      fieldsize <- do.call(c, fields) %>% sum()
      return(fieldsize)
    },
    .NRcalc = function(dat, predInd, Bp, B0pd, B1pd, B2pd, CEXP, FC, ssAC) {
      if (predInd == 1) {
        P <- Bp + (B0pd + B1pd * dat$pred_pro + B2pd * dat$pred_pro^2);
        dat$NR <- (dat$pred_yld * P) - CEXP * dat$exp - FC;
      } else {
        dat$NR <- (dat$pred_yld * (Bp + B0pd)) - CEXP * dat$exp - FC;
      }
      return(dat)
    }
  )
)




