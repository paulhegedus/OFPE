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
    #' Additionally, the user can select 'Ecological' where rates are optimized based 
    #' on maximization of profit while considering the cost of nitrogen use inefficiency.
    #' Only recommended for N fertilizer trials. At every point and for every N rate the 
    #' NUE is estimated via modeling performed in Hegedus & Ewing, 2022. Based on 
    #' estimated NUE, the cost of inefficiency is calculated and subtracted from net-
    #' return. After net-return is amended, the derivative approach is used to find 
    #' the rate where net-return is maximized and adding more N does not increase 
    #' net-return more than the cost of adding that N. 
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
    #' fs = Farmer Selected. Note: this rate must be in the same units as those 
    #' specified in DatClass. i.e. if DatClass$SI == TRUE than fs should be in kg/ha, 
    #' else in lbs/ac.
    fs = NULL,
    #' @field AArateCutoff The maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    AArateCutoff = NULL,
    #' @field AAmin The minimum as-applied rate to simulate management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    AAmin = NULL,
    #' @field EXPvec Vector of the experimental rates to simulate over. This 
    #' must begin with the AAmin and end with the AArateCutoff. Note: this vector 
    #' must be in the same units as those specified in DatClass. i.e. if DatClass$SI 
    #' == TRUE than EXPvec should be in kg/ha, else in lbs/ac.
    EXPvec = NULL,
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
    #' for each experimental rate from 'AAmin' to 'AArateCutoff' as given by 'EXPvec'. Predicted
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
    #' @field FC The fixed costs of operations besides fertilizer and seed..
    FC = NULL,
    #' @field nue_class The R6 class for fitting and using the NUE model developed 
    #' by Hegedus & Ewing, 2022. Only initialized if the optimization method selected 
    #' is 'Ecological'.
    nue_class = NULL,

    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param sPr Provide the number of iterations to run the simulation. Without
    #' user inputs the default will be 100.
    #' @param opt The user also has the option to select the optimization method.
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
    #' Additionally, the user can select 'Ecological' where rates are optimized based 
    #' on maximization of profit while considering the cost of nitrogen use inefficiency.
    #' Only recommended for N fertilizer trials. At every point and for every N rate the 
    #' NUE is estimated via modeling performed in Hegedus & Ewing, 2022. Based on 
    #' estimated NUE, the cost of inefficiency is calculated and subtracted from net-
    #' return. After net-return is amended, the derivative approach is used to find 
    #' the rate where net-return is maximized and adding more N does not increase 
    #' net-return more than the cost of adding that N. 
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
    #' fs = Farmer Selected. Note: this rate must be in the same units as those 
    #' specified in DatClass. i.e. if DatClass$SI == TRUE than fs should be in kg/ha, 
    #' else in lbs/ac.
    #' @param EXPvec Vector of the experimental rates to simulate over. This 
    #' must begin with the AAmin and end with the AArateCutoff. Note: this vector 
    #' must be in the same units as those specified in DatClass. i.e. if DatClass$SI 
    #' == TRUE than EXPvec should be in kg/ha, else in lbs/ac.
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
                          EXPvec = NULL,
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
                  any(grepl("Maximum|Derivative|Ecological", opt)))
        self$opt <- ifelse(opt == "Maximum", "max",
                           ifelse(opt == "Derivative", "deriv",
                                  ifelse(opt == "Ecological", "ecol", NA)))

      }
      if (!is.null(sim_years)) {
        stopifnot(is.numeric(sim_years))
        self$sim_years <- sim_years
      }
      if (!is.null(fs)) {
        stopifnot(is.numeric(fs))
        self$fs <- fs
      }
      if (!is.null(EXPvec)) {
        stopifnot(is.numeric(EXPvec), 
                  self$fs %in% EXPvec)
        self$AAmin <- EXPvec[1]
        self$AArateCutoff <- EXPvec[length(EXPvec)]
        self$EXPvec <- EXPvec
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
      private$.selectEXPvec()
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
        !is.null(datClass$dat_used),
        any(grepl("ModClass", class(modClass))),
        !is.null(modClass$mod_list),
        # needs not null mod_list, ...
        any(grepl("EconDat", class(econDat))),
        !is.null(econDat$ssAC),
        !is.null(econDat$Prc),
        !is.null(econDat$B0pd),
        !is.null(econDat$B1pd),
        !is.null(econDat$B2pd),
        !is.null(econDat$B3pd)
      )
      self$datClass <- datClass
      self$modClass <- modClass
      self$econDat <- econDat
      if (self$datClass$SI) {
        # convert costs to SI units
        if (!is.null(self$econDat$FC)) {
          # multiply $/ac by (1 ac / 0.404686 ha) for $/ha
          self$econDat$FC <- self$econDat$FC / 0.404686
        }
        # multiply $/ac by (1 ac / 0.404686 ha) for $/ha
        self$econDat$ssAC <- self$econDat$ssAC / 0.404686
        # multiply $/bu by (1 bu / 60 lbs) by (1 lbs / 0.453592 kg) for $/kg
        self$econDat$Prc$org <- self$econDat$Prc$org / (60 * 0.453592)
        self$econDat$Prc$conv <- self$econDat$Prc$conv / (60 * 0.453592)
        # multiply $/lbs by (1 lbs / 0.453592 kg) for $/kg
        self$econDat$Prc$cost <- self$econDat$Prc$cost / 0.453592
        # multiply $/ac by (1 ac / 0.404686 ha) for $/ha
        self$econDat$Prc$FC <- self$econDat$Prc$FC / 0.404686
      }
      
      self$datClass$getSimDat(self$sim_years)
      
      # check degree of NA's from covars in simulated data
      mod_covars <- mapply(
        function(x, y) {stringr::str_extract_all(x, paste(y, collapse = "|"))},
        lapply(self$modClass$mod_list, function(x) x$form),
        self$modClass$covars
      ) %>% 
        do.call(c, .) %>% 
        unique()
      if (any(grepl(paste0("^x$|^x$y|", self$datClass$expvar), mod_covars))) {
        mod_covars <- mod_covars[-grep(paste0("^x$|^x$y|", self$datClass$expvar),
                                       mod_covars)]
      }
      self$datClass$sim_dat <- mapply(private$.checkSimDat,
                                      self$datClass$sim_dat,
                                      self$sim_years,
                                      MoreArgs = list(mod_covars = mod_covars),
                                      SIMPLIFY = FALSE) %>% 
        .[!unlist(lapply(., is.null))]
      
      self$fieldsize <- private$.gatherFieldSize()
      
      if (self$datClass$SI) {
        # 1 ac = 0.404686 ha
        self$fieldsize <- self$fieldsize * 0.404686
      }

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
      
      if (self$opt == "ecol") {
        utm_epsg <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$datClass$fieldname[1])
        self$nue_class <- NUE$new(mod_type = "RF", utm_epsg = utm_epsg) # SVR
        # add WHC class to sim data
        whc_dat <- sf::st_read(self$dbCon$db,
                            query = paste0("SELECT * FROM all_farms.whc 
                                   WHERE field = '", self$datClass$fieldname, "' 
                                           AND method = 'nb';"))
        self$datClass$sim_dat <- lapply(self$datClass$sim_dat,
                                        private$.extractWHC,
                                        whc_dat) %>% 
          lapply(OFPE::removeNAfromCovars,
                 "WHC")
        
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
      # EXPvec <- seq(self$AAmin, self$AArateCutoff, 1)

      ## run simulation for each year -
      invisible(mapply(private$.yearSim,
                self$datClass$sim_dat,
                self$sim_years,
                MoreArgs = list(EXPvec = self$EXPvec),
                SIMPLIFY = FALSE))
      gc()
      # tryCatch({
      #   # Actual NR (both years from mod fitting)
      #   # Get all of the observed data (trn + val) from the more
      #   # densely populated respvar.
      #   private$.plotActNRFun()
      # },
      # error = function(e) {
      #   print(paste0("ERROR PLOTTING 'ACTUAL' NR FOR ",
      #                self$unique_fieldname, "!!!"))
      # })
    },
    #' @description
    #' If the user has selected to not save anything, remove temporary files
    #' that could have been used by the SimOP class.
    #' @param None No arguments needed.
    #' @return Any temporary files deleted.
    cleanUp = function() {
      if (!self$SAVE) {
        for (i in 1:length(self$sim_years)) {
          file.remove(paste0(self$dat_path, self$sim_years[i], "/",
                             self$unique_fieldname, "_BpVar_",
                             self$unique_fxn, "_",
                             sim_year, "_",
                             self$opt, ".csv"))
          file.remove(paste0(self$dat_path, self$sim_years[i], "/",
                             self$unique_fieldname, "_NRffMaxData_",
                             self$unique_fxn, "_",
                             sim_year, "_",
                             self$opt, ".csv"))
          file.remove(paste0(self$dat_path, self$sim_years[i], "/",
                             self$unique_fieldname, "_NRopt_",
                             self$unique_fxn, "_",
                             sim_year, "_",
                             self$opt, ".csv"))
        }
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
    #' @param respvar Response variable(s) used to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
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
    plotEstsVsExp = function(respvar,
                             expvar,
                             fieldname,
                             fxn,
                             sim_year,
                             SAVE,
                             out_path,
                             Bp,
                             CEXP) {
      nr_plot <- private$.estsVsExpPlot("NR",
                                        expvar,
                                        fieldname,
                                        sim_year,
                                        Bp,
                                        CEXP,
                                        self$AAmin,
                                        self$AArateCutoff,
                                        fxn)
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        invisible(suppressWarnings(suppressMessages(
          ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/", sim_year, "/",
                               fieldname, "_NRvs",
                               ifelse(expvar == "aa_n","N","SR"),
                               "_", fxn, "_", sim_year, ".png"),
                        plot = nr_plot, device = "png", scale = 1,
                        width = 7.5, height = 5, units = "in"))))
      }
      resp_plots <- lapply(respvar,
                           private$.estsVsExpPlot,
                           expvar,
                           fieldname,
                           sim_year,
                           Bp,
                           CEXP,
                           self$AAmin,
                           self$AArateCutoff,
                           fxn)
      if (SAVE) {
        for (i in 1:length(resp_plots)) {
          try({dev.off()}, silent = TRUE)
          invisible(suppressWarnings(suppressMessages(
            ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/", sim_year, "/",
                                   fieldname, "_", toupper(respvar[i]), "vs",
                                   ifelse(expvar == "aa_n","N","SR"),
                                   "_", fxn, "_", sim_year, ".png"),
                            plot = resp_plots[[i]], device = "png", scale = 1,
                            width = 7.5, height = 5, units = "in"))))
        }
      }
      resp_plots$nr <- nr_plot
      #big_plot <- cowplot::plot_grid(plotlist = resp_plots, nrow = 3, ncol = 1)
      #return(big_plot)
      
      if (self$opt == "ecol") {
        nue_plot <- private$.estsVsExpPlot("NUE",
                                          expvar,
                                          fieldname,
                                          sim_year,
                                          Bp,
                                          CEXP,
                                          self$AAmin,
                                          self$AArateCutoff,
                                          fxn)
        if (SAVE) {
          try({dev.off()}, silent = TRUE)
          invisible(suppressWarnings(suppressMessages(
            ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/", sim_year, "/",
                                   fieldname, "_NUEvs",
                                   ifelse(expvar == "aa_n","N","SR"),
                                   "_", fxn, "_", sim_year, ".png"),
                            plot = nue_plot, device = "png", scale = 1,
                            width = 7.5, height = 5, units = "in"))))
        }
        resp_plots$nue <- nue_plot
        
        nue_n_nr_plot <- private$.NUEnNRVsExpPlot(expvar,
                                                  fieldname,
                                                  sim_year,
                                                  Bp,
                                                  CEXP,
                                                  self$AAmin,
                                                  self$AArateCutoff,
                                                  fxn)
        if (SAVE) {
          try({dev.off()}, silent = TRUE)
          invisible(suppressWarnings(suppressMessages(
            ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/", sim_year, "/",
                                   fieldname, "_NR&costNUEvs",
                                   ifelse(expvar == "aa_n","N","SR"),
                                   "_", fxn, "_", sim_year, ".png"),
                            plot = nue_n_nr_plot, device = "png", scale = 1,
                            width = 7.5, height = 5, units = "in"))))
        }
        resp_plots$nue_n_nr <- nue_n_nr_plot
      }
      
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
    #' @param B3pd The coefficient for protein cubed for the protein
    #' premium/dockage equation.
    #' @param CEXP The cost of the experimental input.
    #' @param FC Fixed costs ($/area) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    #' @param ssAC The cost ($/area) of using site-specific technology or variable rate
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
                         B3pd, 
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
      if (self$datClass$SI) {
        nr_lab <- "ha"
        yld_lab <- "(kg/ha)"
        exp_lab <- "(kg/ha)"
        bp_denom <- "kg"
        exp_denom <- "kg"
      } else {
        nr_lab <- "ac"
        yld_lab <- "(bu/ac)"
        exp_lab <- "(lbs/ac)"
        bp_denom <- "bu"
        exp_denom <- "lbs"
      }
      
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
          gc()
        }
      }
      # calculate NR for each point
      names(dat)[grep(paste0("^", expvar, "$"), names(dat))] <- "exp"
      if (any(respvar == "pro")) {
        P <- Bp + (B0pd + B1pd * dat$pro + B2pd * dat$pro^2 + B3pd * dat$pro^3)
        dat$NR <- (dat$yld * P) - (CEXP * dat$exp) - FC - ssAC
      } else {
        dat$NR <- (dat$yld * Bp) - (CEXP * dat$exp) - FC - ssAC
      }
      # pass to plot sim maps
      var_col_name <- "NR"
      var_label <- paste0("Net-return ($/", nr_lab, ")")
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
        for (i in 1:length(self$sim_years)) {
          dir.create(paste0(cwd, "/", "Predictions/", self$sim_years[i]))
        }
        dir.create(paste0(cwd, "/", "SimData"))
        for (i in 1:length(self$sim_years)) {
          dir.create(paste0(cwd, "/", "SimData/", self$sim_years[i]))
        }
        dir.create(paste0(cwd, "/", "Maps"))
        for (i in 1:length(self$sim_years)) {
          dir.create(paste0(cwd, "/", "Maps/", self$sim_years[i]))
        }
      } else {
        if (!file.exists(paste0(cwd, "/", "Predictions"))) {
          dir.create(paste0(cwd, "/", "Predictions"))
          for (i in 1:length(self$sim_years)) {
            dir.create(paste0(cwd, "/", "Predictions/", self$sim_years[i]))
          }
        } else {
          for (i in 1:length(self$sim_years)) {
            if (!file.exists(paste0(cwd, "/", "Predictions/", self$sim_years[i]))) {
              dir.create(paste0(cwd, "/", "Predictions/", self$sim_years[i]))
            }
          }
        }
        if (!file.exists(paste0(cwd, "/", "SimData"))) {
          dir.create(paste0(cwd, "/", "SimData"))
          for (i in 1:length(self$sim_years)) {
            dir.create(paste0(cwd, "/", "SimData/", self$sim_years[i]))
          }
        } else {
          for (i in 1:length(self$sim_years)) {
            if (!file.exists(paste0(cwd, "/", "SimData/", self$sim_years[i]))) {
              dir.create(paste0(cwd, "/", "SimData/", self$sim_years[i]))
            }
          }
        }
        if (!file.exists(paste0(cwd, "/", "Maps"))) {
          dir.create(paste0(cwd, "/", "Maps"))
          for (i in 1:length(self$sim_years)) {
            dir.create(paste0(cwd, "/", "Maps/", self$sim_years[i]))
          }
        } else {
          for (i in 1:length(self$sim_years)) {
            if (!file.exists(paste0(cwd, "/", "Maps/", self$sim_years[i]))) {
              dir.create(paste0(cwd, "/", "Maps/", self$sim_years[i]))
            }
          }
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
        c("Maximum", "Derivative", "Ecological"),
        title = "Select the optimization method to use."
      ))
      self$opt <- ifelse(opt == "Maximum", "max",
                         ifelse(opt == "Derivative", "deriv", 
                                ifelse(opt == "Ecological", "ecol", NA)))
    },
    .selectSimYear = function(farmername, fieldname) {
      sim_years <- as.character(select.list(
            c("Select Year", "Run 'LikeYear' Analysis"),
            title = "Select whether to choose a year(s) to simulate based off of 'Satellite' data aggrgated in the user's OFPE database. Otherwise, elect to run the 'LikeYear' analysis to identify and select from the wettest, driest, average, and predicted year to follow a user selected year. "
      ))
      if (sim_years == "Select Year") {
        sat_exist <- DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'sat')")
        ) %>% 
          as.numeric() %>% 
          as.logical()
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
        "Input the uniform as-applied rate the farmer would have applied to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre). Should be in the same units specified in the SI argument of DatClass (e.g. kg/ha or lbs/ac): "
      ))
    },
    .selectEXPvec = function() {
      EXPvec <- readline(
        "Provide a vector of experimental rates to simulate over. Separate with commas. Should be in the same units specified in the SI argument of DatClass (e.g. kg/ha or lbs/ac): "
      )
      EXPvec <- as.numeric(unlist(strsplit(EXPvec, ",")));
      self$AAmin <- EXPvec[1]
      self$AArateCutoff <- EXPvec[length(EXPvec)]
      self$EXPvec <- EXPvec
      stopifnot(self$fs %in% self$EXPvec)
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
    .setupSimDat = function(dat) {
      ## if not both respvar
      if (length(self$datClass$respvar) == 1) {
        if (any(grepl("yld", self$datClass$respvar))) {
          dat$pred_pro <- NA ## TODO: this might need to be arbitrary 0 for matrix in C++
        } else {
          dat$pred_yld <- NA
        }
      }

      ## add NR cols
      dat$NR <- NA
      dat$NRmin <- NA
      dat$NRopp <- NA
      dat$NRfs <- NA
      return(dat)
    },
    .trimSimCols = function(dat) {
      # columns needed for the simulation
      keep_columns <- c("x", "y", "row", "col", "field", "year", "NR", "NRmin",
                        "NRopp", "NRfs", self$datClass$expvar,
                        "pred_yld", "pred_pro", "NUE")
      # get rid of columns not in the keep vector
      bad_columns <- names(dat)[!names(dat) %in% keep_columns]
      for (i in 1:length(bad_columns)) {
        remove_var <- bad_columns[i]
        dat <- dat[, (remove_var):= NULL]
      }
      return(dat)
    },
    .addExpCols = function(dat, EXPval) {
      dat$EXP <- EXPval
      names(dat)[grep("EXP", names(dat))] <- self$datClass$expvar
      return(dat)
    },
    .simResponses = function(respvar) {
      for (i in 1:length(respvar)) {
        self$sim_list <- lapply(self$sim_list,
                           private$.predResps,
                           self$modClass$mod_list[[i]],
                           respvar[i])
        gc()
      }
    },
    .predResps = function(dat, m, respvar) {
      dat$pred <- m$predResps(dat, m$m)
      gc()
      ## if response values are negative or above 1000, 
      ## impute by taking random number from distribution of 
      ## responses at closest exp rate
      sim_exp_rate <- dat[, grep(paste0("^", self$datClass$expvar, "$"), names(dat)), with = FALSE][[1]] %>% 
        unique()
      og_dat <- self$datClass$mod_dat[[grep(paste0("^", respvar, "$"), names(self$datClass$mod_dat))]] %>% 
        data.table::rbindlist() 
      og_exp_col <- grep(paste0("^", self$datClass$expvar, "$"), names(og_dat))
      og_dat[[og_exp_col]] <- og_dat[[og_exp_col]] %>% 
        round()
      og_dat <- og_dat[which(abs(og_dat[[og_exp_col]] - sim_exp_rate) == 
                                  min(abs(og_dat[[og_exp_col]] - sim_exp_rate))), ]
      resp_col <- grep(paste0("^", respvar, "$"), names(og_dat))
      mean_resp <- mean(og_dat[[resp_col]], na.rm = TRUE)
      sd_resp <- sd(og_dat[[resp_col]], na.rm = TRUE)
      sd_resp <- ifelse(is.na(sd_resp), 0.1, sd_resp)
      
      if (respvar == "yld") {
        max_yld <- ifelse(self$datClass$SI, 250 * 67.251, 250)
        bad_dim <- sum(dat$pred < 0 | dat$pred > max_yld)
        dat$pred <- ifelse(dat$pred < 0 | dat$pred > max_yld, 
                           rgamma(bad_dim, 
                                  (mean_resp / sd_resp)^2, 
                                  mean_resp / sd_resp^2),
                           dat$pred)
      }
      if (respvar == "pro") {
        bad_dim <- sum(dat$pred < 0 | dat$pred > 30)
        dat$pred <- ifelse(dat$pred < 0 | dat$pred > 30, 
                           rgamma(bad_dim, 
                                  (mean_resp / sd_resp)^2, 
                                  mean_resp / sd_resp^2),
                           dat$pred)
      }
      
      names(dat)[grep("^pred$", names(dat))] <- paste0("pred_", respvar)
      gc()
      
      if (self$opt == "ecol") {
        names(dat)[grep(paste0("^", self$datClass$expvar, "$"), names(dat))] <- 
          "EXP"
        dat$NUE <- self$nue_class$nuePred(dat) 
        names(dat)[grep("EXP", names(dat))] <- self$datClass$expvar 
      } else {
        dat$NUE <- NA
      }
      return(dat)
    },
    .yearSim = function(sim_dat, sim_year, EXPvec) {
      # add cols for NR & make list with df's for each N rate
      self$sim_list <- private$.setupSimDat(sim_dat) %>%
        list() %>%
        rep(length(EXPvec)) %>% 
        `names<-`(EXPvec)
      ## add exp rates to lists
      self$sim_list <- mapply(private$.addExpCols,
                              self$sim_list,
                              EXPvec,
                              SIMPLIFY = FALSE)
      # for all years, location, & each rate predict repsonses
      private$.simResponses(self$datClass$respvar)
      gc()
      
      # get rid of unneccessary columns for the simulation
      self$sim_list <- lapply(self$sim_list,
                              private$.trimSimCols)
      # do simulation & find opt. etc. for each sim_year save to written files
      # returns the sim_list for plotEstsVsExp() for each pred year
      
      private$.runSim(sim_year)
      gc()
      
      # tryCatch({
      #   
      #   },
      #   error = function(e) {
      #     print(paste0("ERROR SIMULATING ",
      #                  sim_year, " FOR ",
      #                  self$unique_fieldname, "!!!"))
      # })
      if (self$SAVE) {
        tryCatch({
          self$plotEstsVsExp( # plotEstsVsExp
            self$datClass$respvar,
            self$datClass$expvar,
            self$unique_fieldname,
            self$unique_fxn,
            sim_year,
            self$SAVE,
            self$out_path,
            self$Bp,
            self$CEXP
          )},
          error = function(e) {
            print(paste0("ERROR PLOTTING SIMULATED ",
                         sim_year, " RESPS VS EXP ",
                         " FOR ", self$unique_fieldname, "!!!"))
          })
        if (self$opt == "ecol") {
          ## TODO: plot NUE vs EXP
          ## TODO: plot OG NR & cost NUE vs EXP
        }
      }
      return(invisible())
    },
    .runSim = function(sim_year) {
      rr <- nrow(self$sim_list[[1]])
      sim_list_names <- names(self$sim_list[[1]])
      ## make connections and clean files
      private$.setupOutFiles(sim_year)
      ## on.exit(closeAllConnections())
      Bp.var_con <- file(paste0(self$dat_path, sim_year, "/",
                                self$unique_fieldname, "_BpVar_",
                                self$unique_fxn, "_",
                                sim_year, "_",
                                self$opt, ".csv"),
                         "at")
      NRffmax_con <- file(paste0(self$dat_path, sim_year, "/",
                                 self$unique_fieldname, "_NRffMaxData_",
                                 self$unique_fxn, "_",
                                 sim_year, "_",
                                 self$opt, ".csv"),
                          "at")
      NRopt_con <- file(paste0(self$dat_path, sim_year, "/",
                               self$unique_fieldname, "_NRopt_",
                               self$unique_fxn, "_",
                               sim_year, "_",
                               self$opt, ".csv"),
                        "at")
      ## run simulations, saves to files
      invisible(lapply(1:self$sPr,
                       private$.simFunIter,
                       rr,
                       sim_list_names,
                       sim_year,
                       Bp.var_con,
                       NRffmax_con,
                       NRopt_con))
      gc()
      ## close connections
      close(Bp.var_con)
      close(NRffmax_con)
      close(NRopt_con)
      
      ## 1 rep sim with actual economic data from that year
      private$.simActEconConds(rr, sim_list_names, sim_year)

      ## calculate NR with mean prices for plotEstsVsExp()
      private$.meanSimList(sim_list_names)
      gc()
    },
    .setupOutFiles = function(sim_year) {
      # Bp.var
      if (file.exists(
        paste0(self$dat_path, sim_year, "/",
               self$unique_fieldname, "_BpVar_",
               self$unique_fxn, "_",
               sim_year, "_",
               self$opt, ".csv")
      )) {
        file.remove(
          paste0(self$dat_path, sim_year, "/",
                 self$unique_fieldname, "_BpVar_",
                 self$unique_fxn, "_",
                 sim_year, "_",
                 self$opt, ".csv")
        )
      }
      x <- c("BaseP", "EXP.cost", "NR.ssopt", "NR.min", "NR.fs", "ffopt.EXPrate",
             "NR.ffopt", "NR.act", "NR.opp", "median.ssopt.EXPrate", "sim")
      df <- as.data.frame(matrix(vector(), 0, length(x)))
      names(df) <- x
      write.csv(df,
                paste0(self$dat_path, sim_year, "/",
                       self$unique_fieldname, "_BpVar_",
                       self$unique_fxn, "_",
                       sim_year, "_",
                       self$opt, ".csv"),
                row.names = FALSE)

      # NRffmax
      if (file.exists(
        paste0(self$dat_path, sim_year, "/",
               self$unique_fieldname, "_NRffMaxData_",
               self$unique_fxn, "_",
               sim_year, "_",
               self$opt, ".csv")
      )) {
        file.remove(
          paste0(self$dat_path, sim_year, "/",
                 self$unique_fieldname, "_NRffMaxData_",
                 self$unique_fxn, "_",
                 sim_year, "_",
                 self$opt, ".csv")
        )
      }
      x <- c("EXP.rate", "NR.ff", "sim")
      df <- as.data.frame(matrix(vector(), 0, length(x)))
      names(df) <- x
      write.csv(df,
                paste0(self$dat_path, sim_year, "/",
                       self$unique_fieldname, "_NRffMaxData_",
                       self$unique_fxn, "_",
                       sim_year, "_",
                       self$opt, ".csv"),
                row.names = FALSE)
      # NRopt
      if (file.exists(
        paste0(self$dat_path, sim_year, "/",
               self$unique_fieldname, "_NRopt_",
               self$unique_fxn, "_",
               sim_year, "_",
               self$opt, ".csv")
      )) {
        file.remove(
          paste0(self$dat_path, sim_year, "/",
                 self$unique_fieldname, "_NRopt_",
                 self$unique_fxn, "_",
                 sim_year, "_",
                 self$opt, ".csv")
        )
      }
      x <- c("BaseP", "EXP.cost", "x", "y", "row", "col", "field", "EXP.rate.ssopt",
             "NR.ssopt", "NR.min", "NR.opp", "NR.fs", "yld.opt", "yld.min", "yld.fs",
             "pro.opt", "pro.min", "pro.fs", "NR.ffopt", "yld.ffopt", "pro.ffopt",
             "EXP.rate.ffopt", "NUE", "cost.NUE", "NR.act", "yld.act", "pro.act", "sim")
      df <- as.data.frame(matrix(vector(), 0, length(x)))
      names(df) <- x
      write.csv(df,
                paste0(self$dat_path, sim_year, "/",
                       self$unique_fieldname, "_NRopt_",
                       self$unique_fxn, "_",
                       sim_year, "_",
                       self$opt, ".csv"),
                row.names = FALSE)
    },
    .simFunIter = function(bp, rr, sim_list_names, sim_year,
                           Bp.var_con, NRffmax_con, NRopt_con) {
      tryCatch({
        Bp.var <- matrix(0, nrow = 1, ncol = 11)
        colnames(Bp.var) <- c("BaseP", "EXP.cost", "NR.ssopt", "NR.min", "NR.fs", 
                              "ffopt.EXPrate", "NR.ffopt", "NR.act", "NR.opp", "median.ssopt.EXPrate", "sim")
        
        rp <- as.integer(runif(1, 1, length(self$econDat$Prc$Year)))
        Bp_col <- grep(self$datClass$sys_type, names(self$econDat$Prc))
        Bp <- as.numeric(self$econDat$Prc[rp, Bp_col])
        CEXP <- as.numeric(self$econDat$Prc[rp, "cost"])
        FC <- as.numeric(self$econDat$Prc[rp, "FC"])
        BpOpp_col <- grep(self$datClass$opp_sys_type, names(self$econDat$Prc))
        BpOpp <- as.numeric(self$econDat$Prc[rp, BpOpp_col])
        self$sim_list <-
          lapply(self$sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
          lapply(function(x) apply(x, 2, as.numeric)) %>%
          lapply(OFPE::NRcalcCpp,
                 Bp,
                 self$econDat$B0pd,
                 self$econDat$B1pd,
                 self$econDat$B2pd,
                 self$econDat$B3pd,
                 CEXP,
                 BpOpp,
                 FC,
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
        gc()
        self$sim_list <- lapply(self$sim_list, function(x) data.table::as.data.table(x) %>%
                                  `names<-`(sim_list_names)) %>%
          lapply(private$.cleanNRdat)
        
        
        invisible(ifelse(self$datClass$sys_type == "conv",
                         NR.opp <- self$sim_list[[1]]$NRopp,
                         NR.opp <- self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$NRopp))
        NRopt <- data.frame(BaseP = rep(Bp, rr),
                            EXP.cost = rep(CEXP, rr),
                            x = self$sim_list[[1]]$x,
                            y = self$sim_list[[1]]$y,
                            row = self$sim_list[[1]]$row,
                            col = self$sim_list[[1]]$col,
                            field = self$sim_list[[1]]$field,
                            EXP.rate.ssopt = NA,
                            NR.ssopt = NA,
                            NR.min = self$sim_list[[1]]$NRmin,
                            NR.opp = NR.opp,
                            NR.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$NRfs,
                            yld.opt = NA,
                            yld.min = self$sim_list[[1]]$pred_yld,
                            yld.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$pred_yld,
                            pro.opt = NA,
                            pro.min = self$sim_list[[1]]$pred_pro,
                            pro.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$pred_pro,
                            NR.ffopt = NA,
                            yld.ffopt = NA,
                            pro.ffopt = NA,
                            EXP.rate.ffopt = NA,
                            NUE = NA,
                            cost.NUE = NA)
        
        if (self$opt == "ecol") {
          # calculate cost of NUE
          self$sim_list <- mapply(private$.calcNUEcost, 
                                  self$sim_list, 
                                  names(self$sim_list),
                                  MoreArgs = list(CEXP = CEXP),
                                  SIMPLIFY = FALSE) %>% 
            lapply(function(x) {x$NR <- x$NR - x$cost.NUE; return(x)})
        }
        
        NRff <- data.frame(EXP.rate = self$EXPvec, NR.ff = NA)
        div_by <- self$fieldsize / rr
        NRff$NR.ff <- lapply(self$sim_list, function(x) sum(x$NR * div_by, na.rm = TRUE))
        NRff <- apply(NRff, 2, as.numeric) %>% as.data.frame()
        if (self$opt == "max") {
          NRffmax <- subset(NRff,
                            NRff[, "NR.ff"] == max(na.omit(NRff[, "NR.ff"])))
        } 
        if (self$opt == "deriv" | self$opt == "ecol") {
          NRff <- NRff %>% `names<-`(NULL) %>% as.matrix()
          NRffmax <- OFPE::derivFFoptCpp(NRff,
                                         nrow(NRff),
                                         self$fieldsize,
                                         CEXP) %>%
            as.data.frame() %>%
            `names<-`(c("EXP.rate", "NR.ff"))
        } 
        ffopt_rate <- NRffmax[1, "EXP.rate"]
        
        NRopt$NR.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$NR
        NRopt$yld.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$pred_yld
        NRopt$pro.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$pred_pro
        NRopt$EXP.rate.ffopt <- ffopt_rate
        
        NRopt[,c("EXP.rate.ssopt", "NR.ssopt", "yld.opt", "pro.opt", "NUE", "cost.NUE")] <-
          private$.getNRopt(CEXP)
        
        if (self$opt == "ecol") {
          NRopt$NR.ffopt <- NRopt$NR.ffopt + self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$cost.NUE
          NRopt$NR.ssopt <- NRopt$NR.ssopt + NRopt$cost.NUE
          NRffmax[1, "NR.ff"] <- sum((self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$NR + 
            self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$cost.NUE) * div_by,
            na.rm = TRUE)
        } 
        
        gc()
        NRopt <- apply(NRopt, 2, as.numeric)
        ## Fill in Bp.var
        Bp.var[1, "BaseP"] <- Bp
        Bp.var[1, "EXP.cost"] <- CEXP
        Bp.var[1, "NR.ssopt"] <- median(NRopt[, "NR.ssopt"], na.rm = T)
        Bp.var[1, "NR.min"] <- median(NRopt[, "NR.min"], na.rm = T)
        Bp.var[1, "NR.fs"] <- median(NRopt[, "NR.fs"], na.rm = T)
        Bp.var[1, "ffopt.EXPrate"] <- NRffmax[, "EXP.rate"]
        Bp.var[1, "NR.ffopt"] <- median(NRopt[, "NR.ffopt"], na.rm = T) # NRffmax[, "NR.ff"] / self$fieldsize # / rr
        Bp.var[1, "NR.opp"] <- median(NRopt[, "NR.opp"], na.rm = T)
        Bp.var[1, "median.ssopt.EXPrate"] <- median(NRopt[, "EXP.rate.ssopt"], na.rm = T)
        
        if (self$datClass$fieldname %in% unique(self$datClass$mod_dat$yld$trn$field)) {
          NRopt <- private$.calcNRact(NRopt, self$sim_list[[1]]$year[1], Bp, CEXP, FC)
        } else {
          temp <- matrix(NA, nrow = nrow(NRopt), ncol = 3)
          colnames(temp) <- c("NR.act", "yld.act", "pro.act")
          NRopt <- cbind(NRopt, temp)
          rm(temp)
        }
        gc()
        
        Bp.var[1, "NR.act"] <- median(NRopt[, "NR.act"], na.rm = T)
        Bp.var[1, "sim"] <- bp
        # fill out rest
        NRopt <- data.table::as.data.table(NRopt)
        NRopt$field <-
          self$datClass$fieldname_codes[
            match(NRopt$field, self$datClass$fieldname_codes$field_code),
            "field"]
        NRopt$sim <- bp
        NRffmax$sim <- bp
        ## save data & append to tables
        # Bp.var
        write(toString(as.data.frame(Bp.var)[1, ]),
              file = Bp.var_con,
              append = TRUE,
              sep = ",")
        # NRffmax
        write(toString(as.data.frame(NRffmax)[1, ]),
              file = NRffmax_con,
              append = TRUE,
              sep = ",")
        gc()
        # NRopt
        for (i in 1:nrow(NRopt)) {
          write(toString(NRopt[i, ]),
                file = NRopt_con,
                append = TRUE,
                sep = ",")
        }
        gc()
        if (self$opt == "ecol") {
          self$sim_list <- lapply(self$sim_list, function(x) {x$cost.NUE <- NULL; return(x)})
        }
      },
      warning = function(w) {return(print(paste0("warning at ", sim_year, " bp = ", bp)))},
      error = function(e) {return(print(paste0("error at ", sim_year, " bp = ", bp)))})
    },
    .getNRopt = function(CEXP) {
      NRoptDat <- data.frame(EXP.rate.ssopt = rep(NA, nrow(self$sim_list[[1]])),
                             NR.ssopt = rep(NA, nrow(self$sim_list[[1]])))
      # get NR optimum and EXP rate optimum
      NRdf <- matrix(nrow = nrow(self$sim_list[[1]]),
                     ncol = (length(self$EXPvec))) %>%
        as.data.frame()
      names(NRdf) <- self$EXPvec
      NRdf[, 1:ncol(NRdf)] <- lapply(self$sim_list, private$.getNR)

      if (self$opt == "max") {
        NRoptDat$NR.ssopt <- do.call(pmax, NRdf)
        NRoptDat$EXP.rate.ssopt <-
          colnames(NRdf)[max.col(NRdf, ties.method = "first")]
        NRoptDat <- sapply(NRoptDat, as.numeric) %>% as.data.frame()
      } 
      if (self$opt == "deriv" | self$opt == "ecol") {
        Nrates <- data.frame(Nrates = self$EXPvec)
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
      
      NRoptDat$yld.opt <- NA
      NRoptDat$pro.opt <- NA
      NRoptDat$NUE <- NA
      NRoptDat$cost.NUE <- NA
      for (i in 1:nrow(NRoptDat)) {
        NRoptDat$yld.opt[i] <-
          self$sim_list[[grep(paste0("^", NRoptDat$EXP.rate.ssopt[i], "$"), names(self$sim_list))]]$pred_yld[i]
        NRoptDat$pro.opt[i] <- 
          self$sim_list[[grep(paste0("^", NRoptDat$EXP.rate.ssopt[i], "$"), names(self$sim_list))]]$pred_pro[i]
      }
      
      if (self$opt == "ecol") {
        for (i in 1:nrow(NRoptDat)) {
          NRoptDat$NUE[i] <-
            self$sim_list[[grep(paste0("^", NRoptDat$EXP.rate.ssopt[i], "$"), names(self$sim_list))]]$NUE[i]
          NRoptDat$cost.NUE[i] <- 
            self$sim_list[[grep(paste0("^", NRoptDat$EXP.rate.ssopt[i], "$"), names(self$sim_list))]]$cost.NUE[i]
        }
      }
      
      return(NRoptDat)
    },
    .getNR = function(dat) {
      NRcol <- dat$NR
      return(NRcol)
    },
    .calcNRact = function(NRopt, year, Bp, CEXP, FC) {
      ## calculates the NR for the sim year if the experiment from
      ## the most recent year in the observed data were applied in the
      ## sim year with the economic conditions from the simulation. 
      
      ## get sim dat for year
      sim_dat <- as.list(year) %>%
        `names<-`(year)
      sim_dat <- lapply(sim_dat,
                        self$datClass$.__enclos_env__$private$.gatherSatDat,
                        'sat',
                        self$datClass$fieldname,
                        'grid')
      sim_dat <- lapply(sim_dat,
                        self$datClass$.__enclos_env__$private$.processSatDat) %>%
        lapply(data.table::as.data.table) %>%
        invisible()
      sim_num_means <- as.list(self$datClass$respvar) %>%
        `names<-`(self$datClass$respvar)
      sim_num_means <- lapply(sim_dat, self$datClass$.__enclos_env__$private$.findMeans)
      sim_dat <- mapply(self$datClass$.__enclos_env__$private$.centerDat,
                        sim_dat,
                        sim_num_means,
                        SIMPLIFY = FALSE) %>%
        lapply(self$datClass$.__enclos_env__$private$.makeAllSimColsNumeric)
      # clean it up for modeling
      mod_covars <- mapply(
        function(x, y) {stringr::str_extract_all(x, paste(y, collapse = "|"))},
        lapply(self$modClass$mod_list, function(x) x$form),
        self$modClass$covars
      ) %>% 
        do.call(c, .) %>% 
        unique()
      if (any(grepl(paste0("^x$|^x$y|", self$datClass$expvar), mod_covars))) {
        mod_covars <- mod_covars[-grep(paste0("^x$|^x$y|", self$datClass$expvar),
                                       mod_covars)]
      }
      sim_dat <- private$.checkSimDat(sim_dat[[1]], year, mod_covars)
      
      # if (self$opt == "ecol") {
      #   utm_epsg <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$datClass$fieldname[1])
      #   # add WHC class to sim data
      #   whc_dat <- sf::st_read(self$dbCon$db,
      #                          query = paste0("SELECT * FROM all_farms.whc 
      #                              WHERE field = '", self$datClass$fieldname, "' 
      #                                      AND method = 'nb';"))
      #   sim_dat <- private$.extractWHC(sim_dat, whc_dat)
      # }
      
      # make the cell_id column
      sim_dat$cell_id <- paste0(sim_dat$row, "_", sim_dat$col)
      # change field codes to field names (useful if multiple fields)
      sim_dat$field <-
        self$datClass$fieldname_codes[match(
          sim_dat$field, self$datClass$fieldname_codes$field_code
        ), "field"]
      
      ## get mod_dat from recent year
      # will use data from yield if possible, else, use protein.
      # b/c spatial resolution
      sim_dat_index <- ifelse(any(grepl("yld", names(self$datClass$mod_dat))),
                              "^yld$",
                              "^pro$")
      # get yield or protein observed data that was used to fit the model
      sim_dat_index <- grep(sim_dat_index, names(self$datClass$mod_dat))
      # get the observed data
      obs_dat <- self$datClass$mod_dat[[sim_dat_index]]
      # bind training and validation for full dataset
      obs_dat <- data.table::rbindlist(obs_dat)
      obs_dat$year <- as.numeric(as.character(obs_dat$year))
      
      # for all unique field and year combos,
      year_index <- by(obs_dat$year, obs_dat$field, unique) %>%
        ## do some conversions to get it out of the 'by' output format
        lapply(as.character) %>%
        lapply(as.numeric) %>%
        ## only take the most recent year that data was used to fit model
        lapply(max)
      # make the list a data frame with each unique field and the most
      # recent year's data used to fit the model for that field
      year_index <- data.frame(field = names(year_index),
                               year = do.call(rbind, year_index))
      
      temp_dat <- as.list(year_index$field)
      for (i in 1:length(temp_dat)) {
        temp_dat[[i]] <- subset(obs_dat, obs_dat$field == year_index$field[i])
        temp_dat[[i]] <- subset(temp_dat[[i]], temp_dat[[i]]$year == year_index$year[i])
      }
      obs_dat <- data.table::rbindlist(temp_dat)
      rm(temp_dat)
      
      ## extract exp rates from mod_dat to sim dat
      # trim obs_dat to field, cell_id, expvar
      keep_cols <- grep(paste(c("field", "cell_id", paste0("^", self$datClass$expvar, "$")), collapse = "|"),
                        names(obs_dat))
      obs_dat <- obs_dat[, keep_cols, with = FALSE]
      # merge with sim_dat
      sim_dat <- merge(sim_dat, obs_dat, c("field", "cell_id"))
      # add NR cols 
      sim_dat <- private$.setupSimDat(sim_dat)
      
      ## predict responses
      for (i in 1:length(self$datClass$respvar)) {
        sim_dat$pred <- self$modClass$mod_list[[i]]$predResps(
          sim_dat, 
          self$modClass$mod_list[[i]]$m
        )
        ## if response values are negative or above 1000, 
        ## impute by taking random number from distribution of 
        ## responses at closest exp rate
        og_dat <- self$datClass$mod_dat[[grep(paste0("^", self$datClass$respvar[i], "$"), 
                                              names(self$datClass$mod_dat))]] %>% 
          data.table::rbindlist() 
        resp_col <- grep(paste0("^", self$datClass$respvar[i], "$"), names(og_dat))
        mean_resp <- mean(og_dat[[resp_col]], na.rm = TRUE)
        sd_resp <- sd(og_dat[[resp_col]], na.rm = TRUE)
        sd_resp <- ifelse(is.na(sd_resp), 0.1, sd_resp)
        
        if (self$datClass$respvar[i] == "yld") {
          max_yld <- ifelse(self$datClass$SI, 250 * 67.251, 250)
          bad_dim <- sum(sim_dat$pred < 0 | sim_dat$pred > max_yld)
          sim_dat$pred <- ifelse(sim_dat$pred < 0 | sim_dat$pred > max_yld, 
                             rgamma(bad_dim, 
                                    (mean_resp / sd_resp)^2, 
                                    mean_resp / sd_resp^2),
                             sim_dat$pred)
        }
        if (self$datClass$respvar[i] == "pro") {
          bad_dim <- sum(sim_dat$pred < 0 | sim_dat$pred > 30)
          sim_dat$pred <- ifelse(sim_dat$pred < 0 | sim_dat$pred > 30, 
                             rgamma(bad_dim, 
                                    (mean_resp / sd_resp)^2, 
                                    mean_resp / sd_resp^2),
                             sim_dat$pred)
        }
        names(sim_dat)[grep("^pred$", names(sim_dat))] <- 
          paste0("pred_", self$datClass$respvar[i])
      }
      
      # if (self$opt == "ecol") {
      #   names(sim_dat)[grep(paste0("^", self$datClass$expvar, "$"), names(sim_dat))] <- 
      #     "EXP"
      #   sim_dat$NUE <- self$nue_class$nuePred(sim_dat) 
      #   names(sim_dat)[grep("EXP", names(sim_dat))] <- self$datClass$expvar 
      # } else {
      #   sim_dat$NUE <- NA
      # }
      
      # trim cols 
      sim_dat <- private$.trimSimCols(sim_dat)
      
      ## calculate net return
      names(sim_dat)[grep(paste0("^", self$datClass$expvar, "$"),
                          names(sim_dat))] <- "exp"
      sim_dat <- private$.NRcalc(
        sim_dat,
        ifelse(any(self$datClass$respvar == "pro"), 1, 0),
        Bp,
        self$econDat$B0pd,
        self$econDat$B1pd,
        self$econDat$B2pd,
        self$econDat$B3pd,
        CEXP,
        FC,
        self$econDat$ssAC
      )
      
      # if (self$opt == "ecol") {
      #   # calculate cost of NUE
      #   names(sim_dat)[grep(paste0("^", self$datClass$expvar, "$"), names(sim_dat))] <- 
      #     "EXP"
      #   sim_dat$cost.NUE <- (sim_dat$EXP * sim_dat$NUE) * CEXP
      #   sim_dat$NR <- sim_dat$NR - sim_dat$cost.NUE
      # }
      
      ## report yld pro & NR
      NRopt <- as.data.frame(NRopt)
      names_order <- names(as.data.frame(NRopt))
      # make the cell_id column
      NRopt$cell_id <- paste0(NRopt$row, "_", NRopt$col)
      # change field names to field codes 
      sim_dat$field <-
        self$datClass$fieldname_codes[match(
          sim_dat$field, self$datClass$fieldname_codes$field
        ), "field_code"]
      # add back in cell_id to sim_dat & trim cols
      sim_dat$cell_id <- paste0(sim_dat$row, "_", sim_dat$col)
      keep_cols <- grep("field|cell_id|^NR$|pred_", names(sim_dat))
      sim_dat <- sim_dat[, keep_cols, with = FALSE]
      # change NR to NR.act
      resps <- c("yld", "pro")
      for (i in 1:length(resps)) {
        names(sim_dat)[grep(paste0("pred_", resps[i]), names(sim_dat))] <- 
          paste0(resps[i], ".act")
      }
      names(sim_dat)[grep("^NR$", names(sim_dat))] <- "NR.act"
      
      NRopt <- merge(NRopt, sim_dat, by = c("field", "cell_id"))
      NRopt$cell_id <- NULL
      NRopt <- apply(NRopt, 2, as.numeric)
      
      # put back in correct order
      names_order <- c(names_order, "NR.act", "yld.act", "pro.act")
      NRopt <- NRopt[, names_order]
      gc()
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
    .NRcalc = function(dat, predInd, Bp, B0pd, B1pd, B2pd, B3pd, CEXP, FC, ssAC) {
      if (predInd == 1) {
        P <- Bp + (B0pd + B1pd * dat$pred_pro + B2pd * dat$pred_pro^2 + B3pd * dat$pred_pro^3);
        dat$NR <- (dat$pred_yld * P) - (CEXP * dat$exp) - FC;
      } else {
        dat$NR <- (dat$pred_yld * Bp) - (CEXP * dat$exp) - FC;
      }
      return(dat)
    },
    .meanSimList = function(sim_list_names) {
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
        ),
        FC = mean(
          self$econDat$Prc[, "FC"],
          na.rm = TRUE
        )
      )
      ## apply NRcalcCpp fxn to dnr with the cd econ scenario
      self$Bp <- cd$Bp
      self$CEXP <- cd$CEXP
      self$FC <- cd$FC
      BpOpp <- cd$BpOpp
      rr <- nrow(self$sim_list[[1]])
      ## Calc NR for every point for every EXP rate (calc NR0 and NRorg for N = 0)
      self$sim_list <- lapply(self$sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
        lapply(function(x) apply(x, 2, as.numeric)) %>%
        lapply(OFPE::NRcalcCpp,
               self$Bp,
               self$econDat$B0pd,
               self$econDat$B1pd,
               self$econDat$B2pd,
               self$econDat$B3pd,
               self$CEXP,
               BpOpp,
               self$FC,
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
      self$sim_list <- lapply(self$sim_list, function(x) data.table::as.data.table(x) %>%
                                `names<-`(sim_list_names)) %>%
        lapply(private$.cleanNRdat)
      if (self$opt == "ecol") {
        # calculate cost of NUE
        self$sim_list <- mapply(private$.calcNUEcost, 
                                self$sim_list, 
                                names(self$sim_list),
                                MoreArgs = list(CEXP = self$CEXP),
                                SIMPLIFY = FALSE) 
      }
    },
    .estsVsExpPlot = function(var, # NR, pred_yld, pred_pro, NUE
                              expvar,
                              fieldname,
                              sim_year,
                              Bp,
                              CEXP,
                              AAmin,
                              AArateCutoff,
                              fxn) {
      if (self$datClass$SI) {
        nr_lab <- "ha"
        yld_lab <- "(kg/ha)"
        exp_lab <- "(kg/ha)"
        bp_denom <- "kg"
        exp_denom <- "kg"
      } else {
        nr_lab <- "ac"
        yld_lab <- "(bu/ac)"
        exp_lab <- "(lbs/ac)"
        bp_denom <- "bu"
        exp_denom <- "lbs"
      }
      
      DNR <- data.table::rbindlist(self$sim_list)
      DNR <- DNR[runif(nrow(DNR) * 0.25, 1, nrow(DNR)), ]
      stopifnot(any(grepl("NR|yld|pro|NUE", var)))
      if (grepl("NR|NUE", var)) {
        names(DNR)[grep(paste0("^", var, "$"), names(DNR))] <- "var"
      } else {
        names(DNR)[grep(paste0("^pred_", var, "$"), names(DNR))] <- "var"
      }
      xMIN <- AAmin
      xMAX  <- AArateCutoff
      xSTEP <- (AArateCutoff - AAmin) / 10
      step_by <- ifelse(grepl("yld", var), 10,
                        ifelse(grepl("NUE", var), 0.1, 5))
      yMIN <- DescTools::RoundTo(min(DNR$var, na.rm = T), step_by, floor)
      yMAX <- DescTools::RoundTo(max(DNR$var, na.rm = T), step_by, ceiling)
      ySTEP <- ifelse(var != "NUE", (yMAX - yMIN) / step_by, (yMAX - yMIN) * step_by)
      names(DNR)[grep(paste0("^", expvar, "$"), names(DNR))] <- "exp"
      var_color <- ifelse(grepl("NR", var), "green",
                          ifelse(grepl("yld", var), "red",
                                 ifelse(grepl("pro", var), "cyan", "purple")))
      y_lab <- ifelse(grepl("NR", var), paste0("Estimated Net-Return ($/", nr_lab, ")"),
                      ifelse(grepl("yld", var), paste0("Predicted Yield ", yld_lab),
                             ifelse(grepl("pro", var), paste0("Predicted Grain Protein %"), 
                                    "Nitrogen Use Efficiency")))
      x_lab <- paste0(ifelse(expvar == "aa_n", "Nitrogen",  "Seed"), " ", exp_lab)
      sub_title <-  paste0(fxn, " : Base Price = $",
                           round(Bp, 2), "/", bp_denom, ", ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Cost = $", round(CEXP, 2), "/", exp_denom)
      var_plot <-
        ggplot2::ggplot(DNR, ggplot2::aes(x = exp, y = var)) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_smooth(color = var_color, span = 0.001) +
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
    .NUEnNRVsExpPlot = function(expvar,
                                fieldname,
                                sim_year,
                                Bp,
                                CEXP,
                                AAmin,
                                AArateCutoff,
                                fxn) {
      if (self$datClass$SI) {
        nr_lab <- "ha"
        exp_lab <- "(kg/ha)"
        bp_denom <- "kg"
        exp_denom <- "kg"
      } else {
        nr_lab <- "ac"
        exp_lab <- "(lbs/ac)"
        bp_denom <- "bu"
        exp_denom <- "lbs"
      }
      
      DNR <- data.table::rbindlist(self$sim_list)
      DNR <- DNR[runif(nrow(DNR) * 0.25, 1, nrow(DNR)), ]
      stopifnot(any(grepl("NR|cost.NUE", names(DNR))))
      
      xMIN <- AAmin
      xMAX  <- AArateCutoff
      xSTEP <- (AArateCutoff - AAmin) / 10
      
      step_by <- 5
      yMIN <- DescTools::RoundTo(min(DNR$NR, na.rm = T), step_by, floor)
      yMAX <- DescTools::RoundTo(max(DNR$NR, na.rm = T), step_by, ceiling)
      ySTEP <- (yMAX - yMIN) / step_by
      
      coeff <- 10
      nue_step_by <- 5
      nue_yMIN <- yMIN / coeff
      nue_yMAX <- yMAX / coeff
      nue_ySTEP <- (nue_yMAX - nue_yMIN) / nue_step_by
      
      names(DNR)[grep(paste0("^", expvar, "$"), names(DNR))] <- "exp"
      NR_color <- "darkgreen"
      cost.NUE_color <- "purple"
      
      y_lab_nr <- paste0("Estimated Net-Return ($/", nr_lab, ")")
      y_lab_cost.NUE <- paste0("Value of Nitrogen Loss ($/", nr_lab, ")")
      
      x_lab <- paste0(ifelse(expvar == "aa_n", "Nitrogen",  "Seed"), " ", exp_lab)
      sub_title <-  paste0(fxn, " : Base Price = $",
                           round(Bp, 2), "/", bp_denom, ", ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Cost = $", round(CEXP, 2), "/", exp_denom)
      
      var_plot <-
        ggplot2::ggplot(DNR) +
        ggplot2::geom_point(ggplot2::aes(x = exp, y = NR),
                            shape = 1,
                            color = NR_color) +
        ggplot2::geom_smooth(ggplot2::aes(x = exp, y = NR), 
                             color = NR_color, 
                             span = 0.001,
                             formula = y ~ s(x, bs = "cs", k = 5)) +
        ggplot2::geom_point(ggplot2::aes(x = exp, y = cost.NUE * coeff),
                            shape = 2,
                            color = cost.NUE_color) +
        ggplot2::geom_smooth(ggplot2::aes(x = exp, y = cost.NUE * coeff), 
                             color = cost.NUE_color, 
                             span = 0.001,
                             formula = y ~ s(x, bs = "cs", k = 5)) +
        ggplot2::labs(x = x_lab) +
        ggplot2::ggtitle(paste0(fieldname, " ", sim_year),
                         subtitle = sub_title) +
        ggplot2::scale_y_continuous(name = y_lab_nr,
                                    limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP),
                                    sec.axis = ggplot2::sec_axis(~./coeff, 
                                                                 name = y_lab_cost.NUE)) +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX),
                                    breaks = seq(xMIN, xMAX, xSTEP)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 14),
                       axis.title.y = ggplot2::element_text(color = NR_color, size=12),
                       axis.title.y.right = ggplot2::element_text(color = cost.NUE_color, size=12))
      if (any(DNR$NR < 0, na.rm = T) | any(DNR$cost.NUE < 0, na.rm = T)) {
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
      FC <- cd[, "FC"]
      for (i in 1:length(years)) {
        self$plotActNR(dat,
                       years[i],
                       Bp,
                       self$econDat$B0pd,
                       self$econDat$B1pd,
                       self$econDat$B2pd,
                       self$econDat$B3pd,
                       CEXP,
                       FC,
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
    },
    .cleanNRdat = function(dat) {
      NRcols <- grep("NR", names(dat))
      for (j in 1:length(NRcols)) {
        if (!all(is.na(dat[[NRcols[j]]]))) {
          dat[dat[[NRcols[j]]] < -2000 | dat[[NRcols[j]]] > 2000, (names(dat)[NRcols[j]])] <- NA
        }
      }
      return(dat)
    },
    # .imputeDat = function() {
    #   covars <- do.call(c, self$modClass$covars) %>% unique()
    #   if (self$datClass$expvar %in% covars) {
    #     covars <- covars[-grep(self$datClass$expvar, covars)]
    #   }
    #   dat <- data.table::rbindlist(self$datClass$sim_dat)
    #   parm_df <- data.frame(
    #     parms = covars,
    #     bad_parms = FALSE,
    #     means = NA,
    #     sd = NA
    #   )
    #   parm_df <- OFPE::findBadParms(parm_df, dat)
    #   # get number of NA's in data
    #   nas <- sapply(dat[, covars, with = FALSE], summary)
    #   # impute vars with NA
    #   dat <- as.data.frame(dat)
    #   for (i in 1:length(nas)) {
    #     if (any(grepl("NA's", names(nas[[i]])))) {
    #       bad_col <- grep(paste0("^", names(nas)[i], "$"), names(dat))
    #       if (sum(is.na(dat[, bad_col])) != nrow(dat)) {
    #         pred_vars <- parm_df$parms[!parm_df$bad_parms]
    #         if (any(grepl(paste0("^", names(nas)[i], "$"), pred_vars))) {
    #           pred_vars <- pred_vars[-grep(names(nas)[i], pred_vars)]
    #         }
    #         form <- paste(pred_vars, collapse = " + ") %>% 
    #           paste0(names(nas)[i], " ~ ", .)
    #         
    #         m0 <- lm(as.formula(form), data = dat)
    #         preds <- suppressWarnings(predict(m0, dat))
    #         na_rows <- is.na(dat[, bad_col])
    #         dat[na_rows, bad_col] <- preds[na_rows]
    #       } else {
    #         stop(print(paste0(names(nas)[i], " is missing in all simulation datasets.")))
    #       }
    #     }
    #   }
    #   sim_dat <- split(dat, dat$year) %>% 
    #     lapply(data.table::as.data.table)
    #   
    #   return(sim_dat)
    # },
    .checkSimDat = function(sim_dat, sim_year, mod_covars) {
      if (any(unlist(self$modClass$fxn) == "BayesLawrence")) {
        mod_covars <- c("prec_py_g", "claycontent")
      }
      
      ## check that all the covars in the data 
      parm_df <- data.frame(
        parms = mod_covars,
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      parm_df <- OFPE::findBadParms(parm_df, sim_dat)
      if (any(unlist(self$modClass$fxn) == "BayesLawrence")) {
        parm_df$bad_parms <- FALSE
      }
      ##  if any covars used in model are bad, then skip that sim year
      if (any(parm_df$bad_parms)) {
        warning(paste0("Warning: Cannot simulate ", sim_year, ". Missing ",
                       paste(parm_df$parms[parm_df$bad_parms], collapse = ", "), 
                       ". Skipping ", sim_year, ". Choosing another year to simulate is recommended."))
        self$sim_years <- self$sim_years[-grep(sim_year, self$sim_years)]
        self$datClass$sim_dat[[grep(sim_year, names(self$datClass$sim_dat))]] <- NULL
        return(NULL)
      } else {
        ##  else remove NA from covars
        mod_covars <- parm_df$parms[!parm_df$bad_parms]
        sim_dat <- OFPE::removeNAfromCovars(sim_dat, mod_covars)
        
        ## TODO/TEMP - NEED TO MOVE TO AFTER NUE CALCULATED??
        # # columns needed for the simulation
        # keep_columns <- c("x", "y", "row", "col", "field", "year", mod_covars)
        # # get rid of columns not in the keep vector
        # bad_columns <- names(sim_dat)[!names(sim_dat) %in% keep_columns]
        # for (i in 1:length(bad_columns)) {
        #   remove_var <- bad_columns[i]
        #   sim_dat <- sim_dat[, (remove_var):= NULL]
        # }
        return(sim_dat)
      }
    },
    .simActEconConds = function(rr, sim_list_names, sim_year) {
      tryCatch({
        Bp.var <- matrix(0, nrow = 1, ncol = 11)
        colnames(Bp.var) <- c("BaseP", "EXP.cost", "NR.ssopt", "NR.min", "NR.fs", 
                              "ffopt.EXPrate", "NR.ffopt", "NR.act", "NR.opp", "median.ssopt.EXPrate", "sim")
        
        if (any(self$econDat$Prc$Year == sim_year)) {
          rp <- which(self$econDat$Prc$Year == sim_year)
        } else {
          if (sim_year < min(self$econDat$Prc$Year)) {
            rp <- which(self$econDat$Prc$Year == min(self$econDat$Prc$Year))
          }
          if (sim_year > max(self$econDat$Prc$Year)) {
            rp <- which(self$econDat$Prc$Year == max(self$econDat$Prc$Year))
          }
        }
        
        Bp_col <- grep(self$datClass$sys_type,
                       names(self$econDat$Prc))
        BpOpp_col <- grep(self$datClass$opp_sys_type,
                          names(self$econDat$Prc))
        Bp <- mean(
          self$econDat$Prc[rp, Bp_col],
          na.rm = TRUE
        )
        CEXP <- mean(
          self$econDat$Prc[rp, "cost"],
          na.rm = TRUE
        )
        BpOpp <- mean(
          self$econDat$Prc[rp, BpOpp_col],
          na.rm = TRUE
        )
        FC <- mean(
          self$econDat$Prc[rp, "FC"],
          na.rm = TRUE
        )
        
        ## apply NRcalcCpp fxn to dnr with the cd econ scenario
        self$sim_list <-
          lapply(self$sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
          lapply(function(x) apply(x, 2, as.numeric)) %>%
          lapply(OFPE::NRcalcCpp,
                 Bp,
                 self$econDat$B0pd,
                 self$econDat$B1pd,
                 self$econDat$B2pd,
                 self$econDat$B3pd,
                 CEXP,
                 BpOpp,
                 FC,
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
        gc()
        self$sim_list <- lapply(self$sim_list, function(x) data.table::as.data.table(x) %>%
                                  `names<-`(sim_list_names)) %>%
          lapply(private$.cleanNRdat)
        
        invisible(ifelse(self$datClass$sys_type == "conv",
                         NR.opp <- self$sim_list[[1]]$NRopp,
                         NR.opp <- self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$NRopp))
        NRopt <- data.frame(BaseP = rep(Bp, rr),
                            EXP.cost = rep(CEXP, rr),
                            x = self$sim_list[[1]]$x,
                            y = self$sim_list[[1]]$y,
                            row = self$sim_list[[1]]$row,
                            col = self$sim_list[[1]]$col,
                            field = self$sim_list[[1]]$field,
                            EXP.rate.ssopt = NA,
                            NR.ssopt = NA,
                            NR.min = self$sim_list[[1]]$NRmin,
                            NR.opp = NR.opp,
                            NR.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$NRfs,
                            yld.opt = NA,
                            yld.min = self$sim_list[[1]]$pred_yld,
                            yld.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$pred_yld,
                            pro.opt = NA,
                            pro.min = self$sim_list[[1]]$pred_pro,
                            pro.fs = self$sim_list[[grep(paste0("^", self$fs, "$"), names(self$sim_list))]]$pred_pro,
                            NR.ffopt = NA,
                            yld.ffopt = NA,
                            pro.ffopt = NA,
                            EXP.rate.ffopt = NA,
                            NUE = NA,
                            cost.NUE = NA)
        
        if (self$opt == "ecol") {
          # calculate cost of NUE
          self$sim_list <- mapply(private$.calcNUEcost, 
                                  self$sim_list, 
                                  names(self$sim_list),
                                  MoreArgs = list(CEXP = CEXP),
                                  SIMPLIFY = FALSE) %>% 
            lapply(function(x) {x$NR <- x$NR - x$cost.NUE; return(x)})
        }
        
        NRff <- data.frame(EXP.rate = self$EXPvec, NR.ff = NA)
        div_by <- self$fieldsize / rr
        NRff$NR.ff <- lapply(self$sim_list, function(x) sum(x$NR * div_by, na.rm = TRUE))
        NRff <- apply(NRff, 2, as.numeric) %>% as.data.frame()
        if (self$opt == "max") {
          NRffmax <- subset(NRff,
                            NRff[, "NR.ff"] == max(na.omit(NRff[, "NR.ff"])))
        } 
        if (self$opt == "deriv" | self$opt == "ecol") {
          NRff <- NRff %>% `names<-`(NULL) %>% as.matrix()
          NRffmax <- OFPE::derivFFoptCpp(NRff,
                                         nrow(NRff),
                                         self$fieldsize,
                                         CEXP) %>%
            as.data.frame() %>%
            `names<-`(c("EXP.rate", "NR.ff"))
        } 
        ffopt_rate <- NRffmax[1, "EXP.rate"]
        
        NRopt$NR.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$NR
        NRopt$yld.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$pred_yld
        NRopt$pro.ffopt <- self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$pred_pro
        NRopt$EXP.rate.ffopt <- ffopt_rate
        
        NRopt[,c("EXP.rate.ssopt", "NR.ssopt", "yld.opt", "pro.opt", "NUE", "cost.NUE")] <-
          private$.getNRopt(CEXP)
        
        if (self$opt == "ecol") {
          NRopt$NR.ffopt <- NRopt$NR.ffopt + self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$cost.NUE
          NRopt$NR.ssopt <- NRopt$NR.ssopt + NRopt$cost.NUE
          NRffmax[1, "NR.ff"] <- sum((self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$NR + 
                                        self$sim_list[[grep(paste0("^", ffopt_rate, "$"), names(self$sim_list))]]$cost.NUE) * div_by,
                                     na.rm = TRUE)
        } 
        
        gc()
        NRopt <- apply(NRopt, 2, as.numeric)
        ## Fill in Bp.var
        Bp.var[1, "BaseP"] <- Bp
        Bp.var[1, "EXP.cost"] <- CEXP
        Bp.var[1, "NR.ssopt"] <- median(NRopt[, "NR.ssopt"], na.rm = T)
        Bp.var[1, "NR.min"] <- median(NRopt[, "NR.min"], na.rm = T)
        Bp.var[1, "NR.fs"] <- median(NRopt[, "NR.fs"], na.rm = T)
        Bp.var[1, "ffopt.EXPrate"] <- NRffmax[, "EXP.rate"]
        Bp.var[1, "NR.ffopt"] <- median(NRopt[, "NR.ffopt"], na.rm = T) # NRffmax[, "NR.ff"] / self$fieldsize # / rr
        Bp.var[1, "NR.opp"] <- median(NRopt[, "NR.opp"], na.rm = T)
        Bp.var[1, "median.ssopt.EXPrate"] <- median(NRopt[, "EXP.rate.ssopt"], na.rm = T)
        
        if (self$datClass$fieldname %in% unique(self$datClass$mod_dat$yld$trn$field)) {
          NRopt <- private$.calcNRact(NRopt, self$sim_list[[1]]$year[1], Bp, CEXP, FC)
        } else {
          temp <- matrix(NA, nrow = nrow(NRopt), ncol = 3)
          colnames(temp) <- c("NR.act", "yld.act", "pro.act")
          NRopt <- cbind(NRopt, temp)
          rm(temp)
        }
        gc()
        
        Bp.var[1, "NR.act"] <- median(NRopt[, "NR.act"], na.rm = T)
        Bp.var[1, "sim"] <- paste0(sim_year, "EconCondition")
        # fill out rest
        NRopt <- data.table::as.data.table(NRopt)
        NRopt$field <-
          self$datClass$fieldname_codes[
            match(NRopt$field, self$datClass$fieldname_codes$field_code),
            "field"]
        NRopt$sim <- paste0(sim_year, "EconCondition")
        NRffmax$sim <- paste0(sim_year, "EconCondition")
        
        Bp.var <- data.table::as.data.table(Bp.var)
        NRffmax <- data.table::as.data.table(NRffmax)
        ## save data & append to tables
        data.table::fwrite(Bp.var, paste0(self$dat_path, sim_year, "/",
                              self$unique_fieldname, "_BpVar_",
                              self$unique_fxn, "_SimYr",
                              sim_year, "EconCond_",
                              self$opt, ".csv"))
        data.table::fwrite(NRopt, paste0(self$dat_path, sim_year, "/",
                             self$unique_fieldname, "_NRopt_",
                             self$unique_fxn, "_SimYr",
                             sim_year, "EconCond_",
                             self$opt, ".csv"))
        data.table::fwrite(NRffmax, paste0(self$dat_path, sim_year, "/",
                               self$unique_fieldname, "_NRffMaxData_",
                               self$unique_fxn, "_SimYr",
                               sim_year, "EconCond_",
                               self$opt, ".csv"))
        gc()
        if (self$opt == "ecol") {
          self$sim_list <- lapply(self$sim_list, function(x) {x$cost.NUE <- NULL; return(x)})
        }
      },
      warning = function(w) {return(print(paste0("warning with actual economics for ", sim_year)))},
      error = function(e) {return(print(paste0("error with actual economics for ", sim_year)))})
    },
    .extractWHC = function(x, y) {
      utm_zone <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$datClass$fieldname[1])
      x$X <- x$x
      x$Y <- x$y
      x <- sf::st_as_sf(x, coords = c("X", "Y"), crs = utm_zone)
      y <- sf::st_transform(y, utm_zone)
      temp <- nngeo::st_nn(x, y) %>%
        unlist()
      y <- sf::st_drop_geometry(y)
      x$WHC <- y[temp, "whcClass"]
      x <- sf::st_drop_geometry(x)
      return(x)
    },
    .calcNUEcost = function(dat, EXP, CEXP) {
      EXP <- as.numeric(EXP)
      dat$cost.NUE <- (EXP * dat$NUE) * CEXP
      return(dat)
    }
  )
)




