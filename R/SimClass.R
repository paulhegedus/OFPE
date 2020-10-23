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
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{DatClass}} for the class containing simulation data,
#' \code{\link{ModClass}} for the class containing the fitted models,
#' \code{\link{EconDat}} for the class containing economic data for the simulation,
#' and \code{\link{SimOP}} for the class that save data and figures from the simulation
#' results.
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
    #' @field unique_fieldname Unique field name corresponding to all fields used in
    #' the simulation.
    unique_fieldname = NULL,
    #' @field unique_fxn The functional form of the models used for analysis.
    unique_fxn = NULL,
    #' @field dat_path If SAVE is TRUE, is the path to the 'Outputs/SimData/'
    #' folder, else to the getwd().
    dat_path = NULL,
    #' @field Bp Mean base price of the crop grown in the system type.
    Bp = NULL,
    #' @field CEXP Mean cost of the as-applied experimental input.
    CEXP = NULL,

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
    },
    #' @description
    #' Interactive method for selecting inputs related to the analysis and the
    #' subsequent simulations and prescription generation. The description below
    #' describes the process of interactively selecting the necessary parameters
    #' needed for the automated analysis, simulation, and prescription building.
    #'
    #' The user will need to select the functional form of a model to use for
    #' analysis. The user can select 'GAM' for a generalized additive model,
    #' 'Non-Linear Logistic' for a logistic function, or user generated models.
    #' If more model options are created, they will need to be added here. They will also
    #' need to provide the number of iterations to run the Monte Carlo
    #' simulation, as well as the optimization method to use.
    #'
    #' There are two current options, 'Maximum' or 'Derivative'. Both methods select
    #' the optimum as-applied rate at each location in the field based on calculating
    #' the net-return at each point under a sequence of as-applied rates. Selecting
    #' 'Maximum' selects the optimum rate as the as-applied rate with the highest
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
    #' @return A 'SimClass' object with completed inputs.
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

      self$unique_fieldname <- OFPE::uniqueFieldname(self$datClass$fieldname)
      unique_fxn <- do.call(rbind, self$modClass$fxn)
      self$unique_fxn <- paste0(row.names(unique_fxn), unique_fxn[, 1]) %>%
        paste(collapse = "-")

      if (self$SAVE) {
        private$.setupOP()
        self$dat_path <- paste0(self$out_path, "/Outputs/SimData/")
      } else {
        self$dat_path <- paste0(getwd(), "/")
      }
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
      ## for all sim years rep sim dat for length of EXP rate range & add EXP val
      EXPvec <- seq(self$AAmin, self$AArateCutoff, 1)

      ## run simulation for each year -
      # sim_list = list for each pred year with list of each point for each rate
      # self$sim_list <-rep(list(NA), length(self$sim_years)) %>%
      #   `names<-`(self$sim_years)
      invisible(mapply(private$.yearSim,
                self$datClass$sim_dat,
                self$sim_years,
                MoreArgs = list(EXPvec = EXPvec),
                SIMPLIFY = FALSE))
      tryCatch({
        # Actual NR (both years from mod fitting)
        # Get all of the observed data (trn + val) from the more
        # densely populated respvar.
        private$.plotActNRFun()
      },
      error = function(e) {
        print(paste0("ERROR PLOTTING 'ACTUAL' NR FOR ",
                     self$unique_fieldname, "!!!"))
      })
    },
    #' @description
    #' If the user has selected to not save anything, remove temporary files
    #' that could have been used by the SimOP class.
    #' @param None No arguments needed.
    #' @return Any temporary files deleted.
    cleanUp = function() {
      if (!self$SAVE) {
        file.remove(paste0(self$dat_path,
                           self$unique_fieldname, "_BpVar_",
                           self$unique_fxn, "_",
                           sim_year, "_",
                           self$opt, ".csv"))
        file.remove(paste0(self$dat_path,
                           self$unique_fieldname, "_NRffMaxData_",
                           self$unique_fxn, "_",
                           sim_year, "_",
                           self$opt, ".csv"))
        file.remove(paste0(self$dat_path,
                           self$unique_fieldname, "_NRopt_",
                           self$unique_fxn, "_",
                           sim_year, "_",
                           self$opt, ".csv"))
      }
    },
    #' @description
    #' Plot the average net-return, yield, and protein, of all points vs the
    #' experimental variable for the average price and experimental cost scenario
    #' from the Monte Carlo simulation. Calculates net-return for
    #' every point under every experimental rate in the user selected
    #' range under the average economic conditions from the simulation.
    #'
    #' Figure shows the selected response (net-return or predicted yield or
    #' predicted protein) for every point for every experimental rate. The
    #' mean response across rates is shown as a colored line.
    #' @param sim_list List for every rate simulated over with data.tables
    #' for every point in the simulation year.
    #' @param AAmin Minimum as-applied rate used in simulation of management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    #' @param respvar Response variable(s) used to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param AArateCutoff The maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param Bp The mean base price used to plot net-returns vs as-applied data.
    #' @param CEXP The mean cost of the as-applied experimental input.
    #' @return Data saved in 'Outputs/Predictions'.
    plotEstsVsExp = function(sim_list,
                             AAmin,
                             respvar,
                             expvar,
                             AArateCutoff,
                             fieldname,
                             fxn,
                             sim_year,
                             SAVE,
                             out_path,
                             Bp,
                             CEXP) {
      DNR <- data.table::rbindlist(sim_list)
      nr_plot <- private$.estsVsExpPlot("NR",
                                        DNR,
                                        expvar,
                                        fieldname,
                                        sim_year,
                                        Bp,
                                        CEXP,
                                        AAmin,
                                        AArateCutoff,
                                        fxn)
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/",
                               fieldname, "_NRvs",
                               ifelse(expvar == "aa_n","N","SR"),
                               "_", fxn, "_", sim_year, ".png"),
                        plot = nr_plot, device = "png", scale = 1,
                        width = 7.5, height = 5, units = "in")
      }
      resp_plots <- lapply(respvar,
                           private$.estsVsExpPlot,
                           DNR,
                           expvar,
                           fieldname,
                           sim_year,
                           Bp,
                           CEXP,
                           AAmin,
                           AArateCutoff,
                           fxn)
      if (SAVE) {
        for (i in 1:length(resp_plots)) {
          try({dev.off()}, silent = TRUE)
          ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/",
                                 fieldname, "_", toupper(respvar[i]), "vs",
                                 ifelse(expvar == "aa_n","N","SR"),
                                 "_", fxn, "_", sim_year, ".png"),
                          plot = resp_plots[[i]], device = "png", scale = 1,
                          width = 7.5, height = 5, units = "in")
        }
      }
      resp_plots$nr <- nr_plot
      big_plot <- cowplot::plot_grid(plotlist = resp_plots, nrow = 3, ncol = 1)
      #return(big_plot)
    },
    #' @description
    #' Function for creating a map of the actual net-return observed in the
    #' field for a selected year available in the data the user specified
    #' for fitting the models (i.e. if you used multiple years to fit a model
    #' you can map the net-return for either year).
    #'
    #' This function uses the observed data for the specified year and if
    #' there is multiple response variables, predicts them to the yield
    #' dataset because this is more spatially dense. This uses the model
    #' fit in the analysis step that was used in the simulation. After
    #' both responses are in the same dataset, net-return is calculated
    #' based on the actual as-applied experimental input and mapped across
    #' the field.
    #' @param dat Data.table or data.frame to use for calculating net-return.
    #' Must include all covariates used in the model fitting processes. These
    #' are the locations actual net-return is calculated for.
    #' @param year A single year, present in 'dat', for which to calculate and
    #' map the actual net-return.
    #' @param Bp The base price corresponding to the price for the system
    #' type selected by the user (i.e. conventional or organic).
    #' @param B0pd The intercept for the protein premium/dockage equation.
    #' @param B1pd The coefficient for protein in the protein premium/dockage
    #' equation.
    #' @param B2pd The coefficient for protein squared for the protein
    #' premium/dockage equation.
    #' @param CEXP The cost of the experimental input.
    #' @param FC Fixed costs ($/acre) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    #' @param ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    #' @param respvar Response variable(s) used to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param modClass ModClass R6 object that has been initialized and with
    #' fitted models. The fitted models in this object are used to predict
    #' crop responses at each location in the simulation data for all
    #' experimental rates in the user specified range.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param farmername The name of the farmer that manages the field.
    #' @param fxn The functional form of the models used for analysis.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param db Connection to the OFPE database to identify UTM zone.
    #' @param utm_fieldname Name of the field for identifying the UTM zone.
    #' @return Map of the actual net-return from a given year in 'Outputs/Maps'.
    plotActNR = function(dat,
                         year,
                         Bp,
                         B0pd,
                         B1pd,
                         B2pd,
                         CEXP,
                         FC,
                         ssAC,
                         respvar,
                         expvar,
                         modClass,
                         fieldname,
                         farmername,
                         fxn,
                         SAVE,
                         out_path,
                         db,
                         utm_fieldname) {
      dat <- dat[which(dat$year == year), ]

      # if respvar > 1 and all(respvar ! in names(dat))
      # -> predict other respvar to data
      if (length(respvar > 1)) {
        miss_resp <- respvar[which(!respvar %in% names(dat))]
        # predict for all missing respvars (i.e. protein)
        for (i in 1:length(miss_resp)) {
          miss_index <- grep(miss_resp[i], respvar)
          dat$pred <- modClass$mod_list[[miss_index]]$predResps(
            dat, modClass$mod_list[[miss_index]]$m
          )
          names(dat)[grep("^pred$", names(dat))] <- respvar[miss_index]
        }
      }
      # calculate NR for each point
      names(dat)[grep(expvar, names(dat))] <- "exp"
      if (any(respvar == "pro")) {
        P <- Bp + (B0pd + B1pd * dat$pro + B2pd * dat$pro^2)
        dat$NR <- (dat$yld * P) - CEXP * dat$exp - FC - ssAC
      } else {
        dat$NR <- (dat$yld * (Bp + B0pd)) - CEXP * dat$exp - FC - ssAC
      }
      # pass to plot sim maps
      var_col_name <- "NR"
      var_label <- "Net-return ($/ac)"
      var_main_label <- paste0("Observed net-return in ", year)
      utm_zone <- OFPE::findUTMzone(db, fieldname = utm_fieldname)
      p <- OFPE::plotMaps(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername,
                          utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = paste0(out_path, "Outputs/Maps/",
                        fieldname, "_observed_NR",
                        "_map_", year, "_", fxn, ".png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      #return(p)
    }
  ),
  private = list(
    .setupOP = function() {
      cwd <- paste0(self$out_path, "/Outputs") # outputs working directory
      if (!file.exists(cwd)) {
        dir.create(cwd)
        dir.create(paste0(cwd, "/", "Predictions"))
        dir.create(paste0(cwd, "/", "SimData"))
        dir.create(paste0(cwd, "/", "Maps"))
      } else {
        if (!file.exists(paste0(cwd, "/", "Predictions"))) {
          dir.create(paste0(cwd, "/", "Predictions"))
        }
        if (!file.exists(paste0(cwd, "/", "SimData"))) {
          dir.create(paste0(cwd, "/", "SimData"))
        }
        if (!file.exists(paste0(cwd, "/", "Maps"))) {
          dir.create(paste0(cwd, "/", "Maps"))
        }
      }
    },
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
    .selectSimYear = function(farmername, fieldname) {
      sim_years <- as.character(select.list(
            c("Select Year", "Run 'LikeYear' Analysis"),
            title = "Select whether to choose a year(s) to simulate based off of 'Satellite' data aggrgated in the user's OFPE database. Otherwise, elect to run the 'LikeYear' analysis to identify and select from the wettest, driest, average, and predicted year to follow a user selected year. "
      ))
      if (sim_years == "Select Year") {
        sat_exist <- as.logical(
          DBI::dbGetQuery(
            self$dbCon$db,
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
              self$dbCon$db,
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
    .setupSimDat = function(sim_list, EXPvec) {
      sim_list <- mapply(private$.addCols,
                         sim_list,
                         EXPvec,
                         SIMPLIFY = FALSE)
      return(sim_list)
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
    .simResponses = function(sim_list, respvar) {
      for (i in 1:length(respvar)) {
        sim_list <- lapply(sim_list,
                           private$.predResps,
                           self$modClass$mod_list[[i]],
                           respvar[i])
      }
      return(sim_list)
    },
    .predResps = function(dat, m, respvar) {
      dat$pred <- m$predResps(dat, m$m)
      names(dat)[grep("^pred$", names(dat))] <- paste0("pred_", respvar)
      return(dat)
    },
    .yearSim = function(sim_dat, sim_year, EXPvec) {
      sim_list <- NA
      tryCatch({
        sim_list <-
          ## make list with df's for each N rate
          rep(list(sim_dat), length(EXPvec)) %>%
          private$.setupSimDat(EXPvec) %>%
          ## for each location for each rate for each sim year predict repsonses
          private$.simResponses(self$datClass$respvar)  %>%
          ## do simulation & find opt. etc. for each sim_year save to written files
          ## returns the sim_list for plotEstsVsExp() for each pred year
          private$.runSim(sim_year)
        },
        error = function(e) {
          print(paste0("ERROR SIMULATING ",
                       sim_year, " FOR ",
                       self$unique_fieldname, "!!!"))
      })
      if (self$SAVE) {
        tryCatch({
            self$plotEstsVsExp( # plotEstsVsExp
              sim_list,
              self$AAmin,
              self$datClass$respvar,
              self$datClass$expvar,
              self$AArateCutoff,
              self$unique_fieldname,
              self$unique_fxn,
              sim_year,
              self$SAVE,
              self$out_path,
              self$Bp,
              self$CEXP
            )
          },
          error = function(e) {
            print(paste0("ERROR PLOTTING SIMULATED ",
                         sim_year, " RESPS VS EXP ",
                         " FOR ", self$unique_fieldname, "!!!"))
          })
      }
      return(invisible())
    },
    .runSim = function(sim_list, sim_year) {
      ## keep Bp.var, but maybe do not need NRopt list and NRffmax list
      Bp.var <- matrix(0, nrow = self$sPr, ncol = 10)
      colnames(Bp.var) <- c("BaseP", "EXP.cost", "NR.ssopt", "NR.min", "NR.fs",
                            "ffopt.EXPrate", "NR.ffopt", "NR.act", "NR.opp", "sim")

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
          } # else some other opt method (i.e. not max or deriv)
        }
        ffopt_rate <- NRffmax[1, "EXP.rate"]
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
        Bp.var[bp, "sim"] <- bp
        # fill out rest
        NRopt <- data.table::as.data.table(NRopt)
        NRopt$field <-
          self$datClass$fieldname_codes[match(NRopt$field,
                                              self$datClass$fieldname_codes$field_code),
                                        "field"]
        NRopt$sim <- bp
        NRffmax$sim <- bp
        ## save data
        if (bp == 1) {
          # Bp.var
          if (file.exists(
            paste0(self$dat_path,
                   self$unique_fieldname, "_BpVar_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv")
          )) {
            file.remove(
              paste0(self$dat_path,
                     self$unique_fieldname, "_BpVar_",
                     self$unique_fxn, "_",
                     sim_year, "_",
                     self$opt, ".csv")
            )
          }
          data.table::fwrite(
            as.data.frame(Bp.var)[bp, ],
            paste0(self$dat_path,
                   self$unique_fieldname, "_BpVar_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv"),
            row.names = FALSE
          )
          Bp.var_con <- file(paste0(self$dat_path,
                                    self$unique_fieldname, "_BpVar_",
                                    self$unique_fxn, "_",
                                    sim_year, "_",
                                    self$opt, ".csv"),
                             "at")
          # NRffmax
          if (file.exists(
            paste0(self$dat_path,
                   self$unique_fieldname, "_NRffMaxData_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv")
          )) {
            file.remove(
              paste0(self$dat_path,
                     self$unique_fieldname, "_NRffMaxData_",
                     self$unique_fxn, "_",
                     sim_year, "_",
                     self$opt, ".csv")
            )
          }
          data.table::fwrite(
            NRffmax,
            paste0(self$dat_path,
                   self$unique_fieldname, "_NRffMaxData_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv"),
            row.names = FALSE
          )
          NRffmax_con <- file(paste0(self$dat_path,
                                     self$unique_fieldname, "_NRffMaxData_",
                                     self$unique_fxn, "_",
                                     sim_year, "_",
                                     self$opt, ".csv"),
                              "at")
          # NRopt
          if (file.exists(
            paste0(self$dat_path,
                   self$unique_fieldname, "_NRopt_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv")
          )) {
            file.remove(
              paste0(self$dat_path,
                     self$unique_fieldname, "_NRopt_",
                     self$unique_fxn, "_",
                     sim_year, "_",
                     self$opt, ".csv")
            )
          }
          data.table::fwrite(
            NRopt,
            paste0(self$dat_path,
                   self$unique_fieldname, "_NRopt_",
                   self$unique_fxn, "_",
                   sim_year, "_",
                   self$opt, ".csv"),
            row.names = FALSE
          )
          NRopt_con <- file(paste0(self$dat_path,
                                   self$unique_fieldname, "_NRopt_",
                                   self$unique_fxn, "_",
                                   sim_year, "_",
                                   self$opt, ".csv"),
                            "at")
        } else {
          ## append to tables
          # Bp.var
          write(toString(as.data.frame(Bp.var)[bp, ]),
                file = Bp.var_con,
                append = TRUE,
                sep = ",")
          # NRffmax
          write(toString(as.data.frame(NRffmax)[1, ]),
                file = NRffmax_con,
                append = TRUE,
                sep = ",")
          # NRopt
          for (i in 1:nrow(NRopt)) {
            write(toString(NRopt[i, ]),
                  file = NRopt_con,
                  append = TRUE,
                  sep = ",")
          }
        }
      }
      ## close connections
      close(Bp.var_con)
      close(NRffmax_con)
      close(NRopt_con)
      ## calculate NR with mean prices for plotEstsVsExp()
      sim_list <- private$.meanSimList(sim_list, sim_list_names)
      return(sim_list)
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
          by(temp_list[[i]][, exp_index, with = FALSE][[1]],
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
    },
    .meanSimList = function(sim_list, sim_list_names) {
      Bp_col <- grep(self$datClass$sys_type,
                     names(self$econDat$Prc))
      BpOpp_col <- grep(self$datClass$opp_sys_type,
                        names(self$econDat$Prc))
      cd <- list(
        Bp = mean(
          self$econDat$Prc[, Bp_col],
          na.rm = TRUE
        ),
        CEXP = mean(
          self$econDat$Prc[, "cost"],
          na.rm = TRUE
        ),
        BpOpp = mean(
          self$econDat$Prc[, BpOpp_col],
          na.rm = TRUE
        )
      )
      ## apply NRcalcCpp fxn to dnr with the cd econ scenario
      self$Bp <- cd$Bp
      self$CEXP <- cd$CEXP
      BpOpp <- cd$BpOpp
      rr <- nrow(sim_list[[1]])
      ## Calc NR for every point for every EXP rate (calc NR0 and NRorg for N = 0)
      sim_list <- lapply(sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
        lapply(function(x) apply(x, 2, as.numeric)) %>%
        lapply(OFPE::NRcalcCpp,
               self$Bp,
               self$econDat$B0pd,
               self$econDat$B1pd,
               self$econDat$B2pd,
               self$CEXP,
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
      sim_list <- lapply(sim_list, function(x) data.table::as.data.table(x) %>% `names<-`(sim_list_names))

      return(sim_list)
    },
    .estsVsExpPlot = function(var, # NR, pred_yld, pred_pro
                              DNR,
                              expvar,
                              fieldname,
                              sim_year,
                              Bp,
                              CEXP,
                              AAmin,
                              AArateCutoff,
                              fxn) {
      stopifnot(any(grepl("NR|yld|pro", var)))
      if (grepl("NR", var)) {
        names(DNR)[grep(paste0("^", var, "$"), names(DNR))] <- "var"
      } else {
        names(DNR)[grep(paste0("^pred_", var, "$"), names(DNR))] <- "var"
      }
      xMIN <- AAmin
      xMAX  <- AArateCutoff
      xSTEP <- (AArateCutoff - AAmin) / 10
      step_by <- ifelse(grepl("yld", var), 10, 5)
      yMIN <- DescTools::RoundTo(min(DNR$var, na.rm = T), step_by, floor)
      yMAX <- DescTools::RoundTo(max(DNR$var, na.rm = T), step_by, ceiling)
      ySTEP <- (yMAX - yMIN) / step_by
      names(DNR)[grep(expvar, names(DNR))] <- "exp"
      var_color <- ifelse(grepl("NR", var), "green",
                          ifelse(grepl("yld", var), "red",
                                 "cyan"))
      y_lab <- ifelse(grepl("NR", var), "Estimated Net-Return ($)",
                      ifelse(grepl("yld", var), "Predicted Yield (bu/ac)",
                             "Predicted Grain Protein %"))
      x_lab <- paste0(ifelse(expvar == "aa_n", "Nitrogen",  "Seed"), " (lbs/ac)")
      sub_title <-  paste0(fxn, " : Base Price = $",
                           round(Bp, 2), ", ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Cost = $", round(CEXP, 2))
      var_plot <-
        ggplot2::ggplot(DNR, ggplot2::aes(x = exp, y = var)) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_smooth(color = var_color) +
        ggplot2::labs(y = y_lab,
                      x =x_lab) +
        ggplot2::ggtitle(paste0(fieldname, " ", sim_year),
                         subtitle = sub_title) +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX),
                                    breaks = seq(xMIN, xMAX, xSTEP)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 14))
      if (any(DNR$var < 0, na.rm = T)) {
        var_plot <- var_plot +
          ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2)
      }
      return(var_plot)
    },
    .plotActNRFun = function() {
      index <- lapply(self$datClass$mod_dat, lapply, dim) %>%
        lapply(function(x) do.call(rbind, x)) %>%
        lapply(function(x) sum(x[, 1]))
      index <- do.call(rbind, index)
      index <- which(index[, 1] == max(index[, 1])) %>% as.numeric()
      dat <- rbind(self$datClass$mod_dat[[index]]$trn,
                   self$datClass$mod_dat[[index]]$val)
      years <- unique(dat$year) %>% as.character()
      # use the most recent economic years available
      cd <- aggregate(.~Year, data = self$econDat$Prc, FUN = mean)
      cd <- cd[cd$Year == max(cd$Year), ]
      Bp <- cd[, grep(self$datClass$sys_type, names(cd))]
      CEXP <- cd[, "cost"]
      for (i in 1:length(years)) {
        self$plotActNR(dat,
                       years[i],
                       Bp,
                       self$econDat$B0pd,
                       self$econDat$B1pd,
                       self$econDat$B2pd,
                       CEXP,
                       self$econDat$FC,
                       self$econDat$ssAC,
                       self$datClass$respvar,
                       self$datClass$expvar,
                       self$modClass,
                       self$unique_fieldname,
                       self$datClass$farmername,
                       self$unique_fxn,
                       self$SAVE,
                       self$out_path,
                       self$dbCon$db,
                       self$datClass$fieldname[1])
      }
    }
  )
)




