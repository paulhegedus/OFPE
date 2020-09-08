#' @title R6 Class for generating an OFPE prescription or experiment
#'
#' @description R6 class for for creating a prescription or experiment for a
#' field of interest. The user can create a new experiment with inputs randomly
#' applied across the field with no stratification, or select data from the database
#' on which to stratify experimental rates. Alternatively the user can create
#' a prescription or experimental prescription based on the optimized rates from
#' the user selected management scenario for given economic and weather conditions.
#' This process builds off of the simulation R6 class (SimClass), where the
#' Monte Carlo simulation for a year or year(s) the user thinks the upcoming year
#' is going to resemble, with the user specified economic data, is used for
#' identifying the optimum rates for each management strategy at each part.
#'
#' If the user decides to do an experimental prescription, the optimum rates of the
#' user specified management scenario from the simulation is used as the base map
#' for the experimental prescription. Experimental rates can be generated from gaps
#' in the optimum rates to create a distribution of rates across the field to be used
#' for fitting crop response models in upcoming years, or from user specified
#' experimental rates. These are then randomly applied to the field
#' according to user specified proportions for each rate, stratified on the
#' optimum rates.
#'
#' If the user decides to make an OFPE prescription with no experiment, the
#' optimum rates for each point for the user specified management scenario is
#' taken as the prescription. Instead of applying experimental rates, a farmer
#' selected rate representing the rate they would have applied is randomly
#' placed across the field, stratified on the optimum rates. These are used
#' as a check to evaluate the performance of the optimized rates or whether
#' further experimentation is needed.
#'
#' When creating an experimental prescription or a prescription, the user must pass
#' an initialized SimClass. This implies that the user has initialized and set up
#' the required DatClass, ModClass, and EconDat R6 objects. The user has the option
#' of passing a SimClass object that has been executed or not. Regardless, the RxClass
#' will check 1) for simulation output data and 2) execute the simulation for any year(s)
#' that the user specified for generating a prescription for that is not present in the
#' simulation data. This simulation output data is used to generate the experimental
#' prescriptions and the prescriptions.
#'
#' If the user allows, the prescriptions or experiments are saved to the 'Outputs' folder
#' and are saved in the OFPE database in the farmer specific aggregated schema in a table
#' called 'rx' (farmername_a.rx).
#' @export
RxClass <- R6::R6Class(
  "RxClass",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field gen_type What type of output to generate; 'NewExp_NoStrat' for
    #' a new experiment with randomly placed experimental rates with no stratification,
    #' 'NewExp_wStrat' for a new experiment with randomly placed experimental rates
    #' stratified on user specified data, 'ExpRx' for a experimental rates randomly
    #' placed, stratified on the optimum rates from the user selected prescription,
    #' or 'Rx' where the prescription is output to the user, with check rates of a
    #' farmer selected base rate randomly placed, stratified on optimum rates.
    gen_type = NULL,
    #' @field trt_length Length, given in feet and converted to meters, for
    #' which to apply treatments.
    trt_length = NULL,
    #' @field boom_width The width of the sprayer boom or spreader, given as feet
    #' and converted to meters.
    boom_width = NULL,
    #' @field orientation TODO...
    orientation = NULL,
    #' @field fld_prop The proportion of the field to apply experimental
    #' or optimum check rates to.
    fld_prop = NULL,
    #' @field conv The conversion factor between lbs of the input to the
    #' units of the as-applied input (i.e. lbs N/ac to unit input/ac)
    conv = NULL,
    #' @field base_rate The rate to apply between the experimental rates
    #' and the field edge, or as check rates in the prescription selected
    #' option.
    base_rate = NULL,
    #' @field rx_years The year or vector of years to base the prescription
    #' off of. If the user passes in multiple years, the average optimum rate
    #' from the simulations will be used. If the user has passed in a SimClass
    #' object where the simulation has been executed, the RxClass will check the
    #' output data to avoid redundantly running a simulation.
    rx_years = NULL,
    #' @field rx_for_year Provide the year that the prescription or experiment
    #' is made for. Used for labeling outputs.
    rx_for_year = NULL,
    #' @field SAVE Logical, whether to save figures and the prescription.
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise. If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    SAVE = NULL,
    #' @field out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.). If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    out_path = NULL,
    #' @field to_DB Logical, whether to save the prescription or experiment
    #' into the database.
    to_DB = NULL,
    #' @field farmername If the user is creating a new experiment, provide or
    #' select the name of the farmer that owns or manages the field(s) that
    #' an experiment is going to be generated for.
    farmername = NULL,
    #' @field fieldname If the user is creating a new experiment, provide or
    #' select the fieldname of the field to use. The field list is from the
    #' available fields in the database for experimentation.
    fieldname = NULL,
    #' @field exp_rate_length Provide the length of experimental rates to apply.
    #' This applies to new experiments and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of rates for the optimized base map, so take your selections for
    #' the management scenario and number of optimum rates into account.
    exp_rate_length = NULL,
    #' @field exp_rates Provide a vector of experimental rates equal to the
    #' number of experimental rates provided in 'exp_rate_length'. This is
    #' required for all new experiments, however can be left to null for
    #' experimental prescriptions if experimental rates should be generated
    #' based on gaps in optimum rates.
    exp_rates = NULL,
    #' @field exp_rates_prop Provide proportions (0 - 1) for the length
    #' of experimental rates provided in 'exp_rate_length'. This is required
    #' for all new experiments and experimental prescriptions.
    exp_rates_prop = NULL,
    #' @field strat_dat If the user is creating a new experiment with stratification
    #' they must apply the data that they are stratifying on. This provides an
    #' identifier for the data being used for the stratification. These must be codes
    #' used in the OFPE workflow sucha as 'yld', 'pro', 'aa_n', 'aa_sr', etc.
    strat_dat = NULL,
    #' @field strat_dat_year Follows the same structure of 'strat_dat', however contains
    #' the years to use for stratification of each of the data types for each of the fields.
    strat_dat_year = NULL,
    #' @field simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' initialized, however does not need to have the execution method performed.
    #' The RxClass will check the SimClass for simulation output data that matches
    #' the year of the data provided, and execute simulations for any years not
    #' present.
    simClass = NULL,
    #' @field exp_rate_gen Logical, for the experimental prescription provide
    #' TRUE or FALSE for whether to create experimentla rates from gaps in the
    #' optimized rates. These experimental rates are placed between optimum rates
    #' to establish a range of rates for which to improve model predictions in future
    #' years.
    exp_rate_gen = NULL,
    #' @field opt_rate_length Provide the number of oprimum rates to use.
    #' This applies to prescriptions and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of experimental rates, so take your selections for
    #' the management scenario and number of experimental rates into account.
    opt_rate_length = NULL,
    #' @field mgmt_scen If the user is creating a prescription or experimental
    #' prescription, they must provide the management scenario to use for their
    #' prescription. The user can choose from the management options listed in
    #' the SimClass. The options are 'SSOPT': site-specific optimized rates,
    #' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
    #' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
    #' 25 lbs seed/ac), or 'Opp': to create a prescription using the rate used to
    #' calculate the net-return under the opposite management price scenario for
    #' the crop (e.g. if your system is conventional this would be the rate of input
    #' used to calculate net-return if grown organically). This is a rate of 0 lbs
    #' input for N fertilizer inputs and the same as the farmer selected rate for seed
    #' inputs.
    mgmt_scen = NULL,
    #' @field expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,

    #' @field out_gen An output generator R6 class (i.e. ExpGen or RxGen), that produces
    #' the output specified by the user. There are different classes with different
    #' required arguments based on whether the user is setting up a new experiment
    #' or creating a prescription. These are automatically called by this 'SetupGen'
    #' method.
    out_gen = NULL,

    #' @description Initializing this class can be performed independent or
    #' after running the analysis and simulation step of the OFPE data cycle.
    #' If the user is creating a new experiment for a field, they should
    #' initialize the class with the arguments specified in the documentation
    #' for that method. It is recommended that the user uses the interactive
    #' setup method rather than passing in arguments, unless an experienced
    #' user. For new experiments, the user will not require an initialized
    #' SimClass object, however if creating a prescription or experimental
    #' prescription the user will need to pass in an initialized and/or executed
    #' SimClass R6 class, EVEN IF using the interactive methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param gen_type What type of output to generate; 'NewExp_NoStrat' for
    #' a new experiment with randomly placed experimental rates with no stratification,
    #' 'NewExp_wStrat' for a new experiment with randomly placed experimental rates
    #' stratified on user specified data, 'ExpRx' for a experimental rates randomly
    #' placed, stratified on the optimum rates from the user selected prescription,
    #' or 'Rx' where the prescription is output to the user, with check rates of a
    #' farmer selected base rate randomly placed, stratified on optimum rates.
    #' @param trt_length Length, in feet, for which to apply treatments.
    #' @param boom_width The width of the sprayer boom or spreader (feet).
    #' @param orientation TODO...
    #' @param fld_prop The proportion of the field to apply experimental
    #' or optimum check rates to.
    #' @param conv The conversion factor between lbs of the input to the
    #' units of the as-applied input (i.e. lbs N/ac to unit input/ac)
    #' @param base_rate The rate to apply between the experimental rates
    #' and the field edge, or as check rates in the prescription selected
    #' option.
    #' @param rx_years The year or vector of years to base the prescription
    #' off of. If the user passes in multiple years, the average optimum rate
    #' from the simulations will be used. If the user has passed in a SimClass
    #' object where the simulation has been executed, the RxClass will check the
    #' output data to avoid redundantly running a simulation.
    #' @param rx_for_year Provide the year that the prescription or experiment
    #' is made for. Used for labeling outputs.
    #' @param SAVE Logical, whether to save figures and the prescription.
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise. If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    #' @param out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.). If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    #' @param to_DB Logical, whether to save the prescription or experiment
    #' into the database.
    #' @param farmername If the user is creating a new experiment, provide or
    #' select the name of the farmer that owns or manages the field(s) that
    #' an experiment is going to be generated for.
    #' @param fieldname If the user is creating a new experiment, provide or
    #' select the fieldname of the field to use. The field list is from the
    #' available fields in the database for experimentation.
    #' @param exp_rate_length Provide the length of experimental rates to apply.
    #' This applies to new experiments and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of rates for the optimized base map, so take your selections for
    #' the management scenario and number of optimum rates into account.
    #' @param exp_rates Provide a vector of experimental rates equal to the
    #' number of experimental rates provided in 'exp_rate_length'. This is
    #' required for all new experiments, however can be left to null for
    #' experimental prescriptions if experimental rates should be generated
    #' based on gaps in optimum rates.
    #' @param exp_rates_prop Provide proportions (0 - 1) for the length
    #' of experimental rates provided in 'exp_rate_length'. This is required
    #' for all new experiments and experimental prescriptions.
    #' @param strat_dat If the user is creating a new experiment with stratification
    #' they must apply the data that they are stratifying on. This provides an
    #' identifier for the data being used for the stratification. These must be codes
    #' used in the OFPE workflow sucha as 'yld', 'pro', 'aa_n', 'aa_sr', etc. This
    #' must be provided as a list with the fields for the prescription and a character
    #' vector of the stratification data used. You must have data you want to stratify
    #' on aggregated in the database, otherwise more paramters for utilizing the raw
    #' data would be required.
    #' @param strat_dat_year Follows the same structure of 'strat_dat', however contains
    #' the years to use for stratification of each of the data types for each of the fields.
    #' @param simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' initialized, however does not need to have the execution method performed.
    #' The RxClass will check the SimClass for simulation output data that matches
    #' the year of the data provided, and execute simulations for any years not
    #' present.
    #' @param exp_rate_gen Logical, for the experimental prescription provide
    #' TRUE or FALSE for whether to create experimentla rates from gaps in the
    #' optimized rates. These experimental rates are placed between optimum rates
    #' to establish a range of rates for which to improve model predictions in future
    #' years.
    #' @param opt_rate_length Provide the number of oprimum rates to use.
    #' This applies to prescriptions and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of experimental rates, so take your selections for
    #' the management scenario and number of experimental rates into account.
    #' @param mgmt_scen If the user is creating a prescription or experimental
    #' prescription, they must provide the management scenario to use for their
    #' prescription. The user can choose from the management options listed in
    #' the SimClass. The options are 'SSOPT': site-specific optimized rates,
    #' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
    #' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
    #' 25 lbs seed/ac), or 'Opp': to create a prescription using the rate used to
    #' calculate the net-return under the opposite management price scenario for
    #' the crop (e.g. if your system is conventional this would be the rate of input
    #' used to calculate net-return if grown organically). This is a rate of 0 lbs
    #' input for N fertilizer inputs and the same as the farmer selected rate for seed
    #' inputs.
    #' @param expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @return
    initialize = function(dbCon,
                          gen_type = NULL,
                          trt_length = NULL,
                          boom_width = NULL,
                          orientation = NULL,
                          fld_prop = NULL,
                          conv = NULL,
                          base_rate = NULL,
                          rx_years = NULL,
                          rx_for_year = NULL,
                          out_path = NULL,
                          SAVE = NULL,
                          to_DB = NULL,
                          fieldname = NULL,
                          farmername = NULL,
                          exp_rate_length = NULL,
                          exp_rates = NULL,
                          exp_rates_prop = NULL,
                          strat_dat = NULL,
                          strat_dat_year = NULL,
                          simClass = NULL,
                          exp_rate_gen = NULL,
                          opt_rate_length = NULL,
                          mgmt_scen = NULL,
                          expvar = NULL) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon

      if (!is.null(expvar)) {
        stopifnot(is.character(expvar),
                  any(grepl("As-Applied Nitrogen|As-Applied Seed Rate", expvar)))
        self$expvar <- ifelse(expvar == "As-Applied Nitrogen", "aa_n", "aa_sr")
      }
      if (!is.null(gen_type)) {
        stopifnot(is.character(gen_type),
                  any(grepl("NewExp_noStrat|NewExp_wStrat|ExpRx|Exp", gen_type)))
        self$gen_type <- gen_type
      }
      if (!is.null(trt_length)) {
        stopifnot(is.numeric(trt_length),
                  trt_length > 0)
        self$trt_length <- round(trt_length, 0) * 0.3048 # convert to meters
      }
      if (!is.null(boom_width)) {
        stopifnot(is.numeric(boom_width),
                  boom_width > 0)
        self$boom_width <- round(boom_width, 0) * 0.3048 # convert to meters
      }
      if (!is.null(orientation)) {
        stopifnot(is.numeric(orientation))
        self$orientation <- orientation
      }
      if (!is.null(fld_prop)) {
        stopifnot(is.numeric(fld_prop),
                  fld_prop >= 0 & fld_prop <= 1)
        self$fld_prop <- fld_prop
      }
      if (!is.null(conv)) {
        stopifnot(is.numeric(conv))
        self$conv <- 1 / conv
      }
      if (!is.null(base_rate)) {
        stopifnot(is.numeric(base_rate))
        self$base_rate <- round(base_rate, 0)
      }
      if (!is.null(rx_years)) {
        stopifnot(is.numeric(rx_years))
        self$rx_years <- rx_years
      }
      if (!is.null(rx_for_year)) {
        stopifnot(is.numeric(rx_for_year))
        self$rx_for_year <- rx_for_year
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
      if (!is.null(to_DB)) {
        stopifnot(is.logical(to_DB))
        self$to_DB <- to_DB
      }
      if (!is.null(farmername)) {
        stopifnot(is.character(farmername))
        self$farmername <- farmername
      }
      if (!is.null(fieldname)) {
        stopifnot(is.character(fieldname))
        self$fieldname <- fieldname
      }
      if (!is.null(exp_rate_length)) {
        stopifnot(is.numeric(exp_rate_length))
        self$exp_rate_length <- exp_rate_length
      }
      if (!is.null(exp_rates)) {
        stopifnot(is.numeric(exp_rates))
        self$exp_rates <- exp_rates
      }
      if (!is.null(exp_rates_prop)) {
        stopifnot(is.numeric(exp_rates_prop),
                  sum(exp_rates_prop) >= 0.99)
        self$exp_rates_prop <- exp_rates_prop
      }
      if (!is.null(strat_dat)) {
        stopifnot(is.list(strat_dat),
                  length(strat_dat) == length(fieldname))
        for (i in 1:length(strat_dat)) {
          stopifnot(any(grepl("yld|pro|aa_n|aa_sr", strat_dat[[i]])))
        }
        self$strat_dat <- strat_dat
      }
      if (!is.null(strat_dat_year)) {
        stopifnot(is.list(strat_dat_year),
                  length(strat_dat_year == length(strat_dat)))
        self$strat_dat_year <- strat_dat_year
      }
      if (!is.null(simClass)) {
        stopifnot(any(grepl("SimClass", class(modClass))))
        self$simClass <- simClass
        self$expvar <- self$simClass$datClass$expvar
        self$out_path <- self$simClass$out_path
      }
      if (!is.null(exp_rate_gen)) {
        stopifnot(is.logical(exp_rate_gen))
        self$exp_rate_gen <- exp_rate_gen
      }
      if (!is.null(opt_rate_length)) {
        stopifnot(is.numeric(opt_rate_length))
        self$opt_rate_length <- opt_rate_length
      }
      if (!is.null(mgmt_scen)) {
        stopifnot(is.character(mgmt_scen),
                  any(grepl("SSOPT|FFOPT|FS|Min|Opp", mgmt_scen)))
        self$mgmt_scen <- mgmt_scen
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the prescription or
    #' experiment generation process. It is recommended to use this interactive
    #' method rather than passing in inputs as arguments unless an experienced
    #' user. If the user is creating an experimental prescription or prescription,
    #' they must have initialized this class with a SimClass object, even if using
    #' this interactive method for selecting other inputs.
    #'
    #' The user first selects whether to make a prescription or experiment. If
    #' the user specifies a type and has not passed in a required input upon
    #' initialization, an error will be thrown. Based on the user specification
    #' different selections are required for the user to set up the generation
    #' method.
    #'
    #' The user will have to select the treatment length, the width of the application
    #' boom for a sprayer or preader, the orientation to align the treatments relative
    #' to N/S, the base rate to apply to around experiments or for checks in the
    #' prescription, the year(s) the farmer thinks the upcoming year will resemble and
    #' to make the prescription for, and whether to save the prescription or experiment
    #' in the database.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A 'RxClass' object with complete user selections.
    selectInputs = function() {
      private$.selectGenType()
      private$.selectTrtLength()
      private$.selectBoomWidth()
      private$.selectOrientation()
      private$.selectBaseRate()
      private$.selectYearForRx()
      private$.selectToDB()
      private$.selectConv()
      if (is.null(self$simClass)) {
        private$.selectOutPath()
      } else {
        self$out_path <- self$simClass$out_path
      }
      if (grepl("NewExp", self$gen_type)) {
        private$.selectExpvar()
        private$.selectFarmer()
        private$.selectFieldname()
        private$.selectExpRateLength()
        private$.selectExpRates()
        private$.selectExpRatesProp()
        private$.selectFldProp()
        if (grepl("wStrat", self$gen_type)) {
          private$.selectStratDat()
          private$.selectStratYears()
        }
      }
      if (grepl("Rx", self$gen_type)) {
        stopifnot(is.null(self$simClass))
        private$.selectRxYears(self$simClass$datClass$farmername,
                               self$simClass$datClass$fieldname)
        private$.selectOptRateLength()
        private$.selectMgmtScen()
        if (grepl("ExpRx", self$gen_type)) {
          private$.selectFldProp()
          private$.selectExpRateLength()
          private$.selectExpRateGen()
          if (!self$exp_rate_gen) {
            private$.selectExpRates()
          }
          private$.selectExpRatesProp()
        }
        self$expvar <- self$simClass$datClass$expvar
        self$farmername <- self$simClass$datClass$farmername
      }
    },
    #' @description
    #' Method used to setup the output location for the prescription and related
    #' figures.
    #' @param create Logical, whether to create folders for output. If not,
    #' no plots will be saved by default.
    #' @return A folder created in the path for model output figures.
    setupOP = function(create = TRUE) {
      stopifnot(is.logical(create))
      if (!create) {
        self$SAVE <- FALSE
      }
      if (self$SAVE) {
        cwd <- paste0(self$out_path, "/Outputs") # outputs working directory
        if (create) {
          if(!file.exists(cwd)){
            dir.create(cwd)
            dir.create(paste0(cwd,"/","Rx"))
          }else{
            if(!file.exists(paste0(cwd,"/","Rx"))){
              dir.create(paste0(cwd,"/","Rx"))
            }
          }
        }
      }
    },
    #' @description
    #' Method used to setup the experiment or prescription generator based on the
    #' user select 'gen_type'. This initializes the output generator selected by the
    #' user with the arguments needed for that generator.
    #' @param rxClass rxClass R6 class object. This method passes it's own class
    #' instantiation into the generator class.
    #' @return An instantiated output generator class.
    setupGen = function() {
      self$out_gen <- private$.loadGenerator()
    },
    #' @description
    #' Method for calling the execution method of the experiment or prescription
    #' generator. These call the methods in the generator R6 classes.
    #' @param None All parametes supplied upon initialization.
    #' @return A completed experiment or prescription.
    executeOutGen = function() {
      self$out_gen$executeOutput()
    },
    #' @description
    #' Method for saving prescriptions or experiments and related plots. This only
    #' save plots if the user has supplied a folder path to save the
    #' plots to, and if the user does not select SAVE == FALSE. If the user passes in
    #' an argument to SAVE, this replaces any previously selected SAVE option (e.g.
    #' if SAVE was set to TRUE in the class initialization and the user passes FALSE
    #' as an argument here, the class' selection for SAVE is set to FALSE from TRUE).
    #' @param SAVE Whether to save diagnostic plots. If NULL uses the user selected
    #' choice. If not NULL and is logical, argument replaces previously set SAVE
    #' options for the entire class.
    #' @return Shapefile and map saved in 'Outputs' folder and in database.
    saveOutputs = function(SAVE = NULL) {
      browser()

      if (is.null(SAVE)) {
        SAVE <- self$SAVE
      } else {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }
      if (!is.null(self$simClass)) {
        unique_fxn <- do.call(rbind, self$simClass$modClass$fxn)
        unique_fxn <- paste0(row.names(unique_fxn), "-", unique_fxn[, 1]) %>%
          paste(collapse = "--")
        unique_fieldname <- OFPE::uniqueFieldname(self$simClass$datClass$fieldname)
        opt <- self$simClass$opt
      } else {
        unique_fxn <- NA
        unique_fieldname <- OFPE::uniqueFieldname(self$fieldname)
        opt <- NA
      }
      ## TODO
      # plot exp/rx map
      self$plotRxMap(
        self$out_gen$RX,
        paste0("exp",
               ifelse(self$expvar == "aa_n", "N", "Seed"),
               "Rates"),
        "exprate",
        paste0(ifelse(self$expvar == "aa_n", "N", "Seed"),
               " Rate (lbs/ac)"),
        paste0(unique_fieldname, "experimental ",
               ifelse(self$expvar=="aa_n", "N", "Seed"),
               " rates for ", self$rx_for_year),
        unique_fieldname,
        self$rx_for_year,
        unique_fxn,
        opt,
        SAVE,
        self$farmername,
        self$out_path
      )

      # save to outputs folder
      if (grepl("NewExp", self$gen_type)) {
        suppressWarnings(sf::st_write(
            self$out_gen$RX,
            paste0(self$out_path,
                   "/Outputs/Rx/",
                   unique_fieldname, "_Exp_",
                   self$rx_for_year, ".shp"),
            quiet = TRUE,
            delete_dsn = TRUE))
      } else {
        suppressWarnings(sf::st_write(
          self$out_gen$RX,
          paste0(self$out_path,
                 "/Outputs/SimData/",
                 unique_fieldname, "_Rx_",
                 unique_fxn, "_",
                 self$rx_for_year, "_",
                 opt, ".shp"),
          quiet = TRUE,
          delete_dsn = TRUE))
      }
      # save to database
      invisible(self$uploadRxFun(
          self$out_gen$RX,
          self$dbCon$db,
          self$farmername,
          unique_fieldname,
          self$rx_for_year
      ))
    },
    #' @description
    #' This method is for plotting maps of simulation outcomes. These
    #' include the net-returns from the management strategies, predicted
    #' responses, and optimized rates. Is a wrapper to a private plotRxMap
    #' method.
    #' @param dat Data frame with the net-returns and experimental optimums
    #' for every point for every simulation iteration.
    #' @param var The label of the variable to map. Used in figure name.
    #' @param var_col_name The name of the column of the variable in the
    #' supplied data ('dat')
    #' @param var_label The label to be applied to the legend of the map
    #' corresponding to the variable mapped.
    #' @param var_main_label The main label to apply to the map.
    #' @param fieldname Unique field name corresponding to all fields used
    #' in the simulation.
    #' @param rx_for_year Provide the year that the prescription or experiment
    #' is made for. Used for labeling outputs.
    #' @param fxn The functional form of the models used for analysis. Not
    #' applicable for
    #' experiments, only prescriptions using an optimization method.
    #' @param opt The optimization method used in the simulation. Not applicable for
    #' experiments, only prescriptions using an optimization method.
    #' @param SAVE Logical, whether to save figure.
    #' @param farmername The name of the farmer that manages the field.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Maps saved in 'Outputs/Rx/'
    plotRxMap = function(dat,
                         var,
                         var_col_name,
                         var_label,
                         var_main_label,
                         fieldname,
                         rx_for_year,
                         fxn = NULL,
                         opt = NULL,
                         SAVE = TRUE,
                         farmername,
                         out_path) {
      p <- private$.plotRxMap(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername) %>%
        suppressMessages() %>%
        suppressWarnings()
      if (SAVE) {
        if (!is.na(fxn) & !is.na(opt)) {
          ggplot2::ggsave(
            file = paste0(out_path, "/Outputs/Rx/",
                          fieldname, "_", tolower(var),
                          "_RxMap_", fxn, "_", rx_for_year, "_", opt, ".png"),
            plot = p, device = "png",
            width = 7.5, height = 7.5, units = "in"
          )
        } else {
          ggplot2::ggsave(
            file = paste0(out_path, "/Outputs/Rx/",
                          fieldname, "_", tolower(var),
                          "_ExpMap_", rx_for_year, ".png"),
            plot = p, device = "png",
            width = 7.5, height = 7.5, units = "in"
          )
        }
      }
      #return(p)
    },
    #' @description
    #' This method is for uploading the experiment or prescription to the
    #' database.
    #' @param RX Data frame with the net-returns and experimental optimums
    #' for every point for every simulation iteration.
    #' @param db Database connection connected to an OFPE formatted
    #' database.
    #' @param farmername The name of the farmer that manages the field.
    #' @param fieldname Unique field name corresponding to all fields used
    #' in the simulation.
    #' @param rx_year Provide the year that the prescription or experiment
    #' is made for. Used for labeling outputs.
    #' @return Output saved in database
    uploadRxFun = function(RX, db, farmername, fieldname, rx_year) {
      browser()

      RX <- sf::st_transform(RX, "epsg:4326")
      RX <- as(RX, "Spatial")
      tabExist <- as.logical(DBI::dbGetQuery(
        db,
        paste0("SELECT EXISTS (
          SELECT 1
          FROM   information_schema.tables
          WHERE  table_schema = '", farmername, "_a'
          AND    table_name = 'rx')")
      ))
      if (!tabExist) {
        for (j in 1:ncol(RX)) { # convert NA to NaN for db import
          if (anyNA(as.data.frame(RX[, j])[1])) {
            naIndex <- which(is.na(as.data.frame(RX[, j])[1]))
            RX[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
          }
        }
        suppressMessages(rpostgis::pgInsert(db,
                                            c(paste0(farmername, "_a"), "rx"),
                                            RX,
                                            geom = "geometry",
                                            new.id = "gid")) #, return.pgi=TRUE
        # make field and year and orig_file primary keys to prevent overwriting (updates instead)
        invisible(DBI::dbGetQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_a.rx
                 ADD PRIMARY KEY (gid, field, rxyear)"))
        )
        for (j in 1:length(names(RX))) {
          tryCatch({
            DBI::dbSendQuery(db,
                             paste0("ALTER TABLE ", farmername, "_a.rx
                                    ALTER COLUMN ", names(RX)[j], " TYPE TEXT"))
          },
          error = function(e) {print(paste0("warning in column ", names(RX)[j]))})
        }
        # convert polygon table to multiploygon geometry
        # if it is a polygon df
        if (any(grepl("poly", class(sf::st_geometry(sf::st_as_sf(RX))),
                      ignore.case = TRUE))) {
          # if it is NOT a multipolygon df
          if (!any(grepl("multi", class(sf::st_geometry(sf::st_as_sf(RX))),
                         ignore.case = TRUE))) {
            invisible(DBI::dbGetQuery(
              db,
              paste0("ALTER TABLE ", farmername, "_a.rx
              ALTER COLUMN geometry TYPE geometry(MultiPolygon, 4326)
              USING ST_Multi(geometry)")
            ))
          }
        }
        # create spatial index
        tryCatch({
          invisible(DBI::dbSendQuery(
            db,
            paste0("CREATE INDEX rx_geom_idx_new",
                   " ON ", farmername, "_a.rx
                   USING GIST (geometry)")
          ))},
          error = function(e) {
            print(paste0("error creating spatial index for ", farmername, "_a.", dtype))
          }
        )
      } else {
        # get column names from db
        dbCols <- DBI::dbGetQuery(db, paste0("SELECT column_name
                                   FROM information_schema.columns
                                   WHERE table_schema = '", farmername, "_a'
                                   AND table_name = 'rx'"))[, 1]
        dbCols <- dbCols[c(-which(dbCols == "gid"), -which(dbCols == "geometry"))] # take out "gid" & "geometry" column, will be added later
        # get columns from db that are not in file & columns from file that are not in db
        inDB <- noMatch(dbCols, names(RX))
        inDF <- noMatch(names(RX), dbCols)
        # add cols from db to file and cols from file to db
        if (length(inDB) > 0) {
          row.names(RX) <- as.character(seq(1, nrow(RX), 1))
          RX <- sp::spCbind(RX, as.data.frame(matrix(NA, nrow(RX), length(inDB))))
          names(RX) <- c(names(RX), inDB)
        }
        if (length(inDF) > 0) {
          for (j in 1:length(inDF)) {
            DBI::dbGetQuery(db, paste0("ALTER TABLE ", farmername, "_a.rx
                                       ADD COLUMN ", inDF[j], " TEXT"))
          }
        }
        # convert NA to NaN for db import
        for (j in 1:ncol(RX)) {
          if (anyNA(as.data.frame(RX[, j])[1])) {
            naIndex <- which(is.na(as.data.frame(RX[, j])[1]))
            RX[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
          }
        }
        # if geometry is polygon change to multipolygon
        # if it is a polygon df
        if (any(grepl("poly", class(sf::st_geometry(sf::st_as_sf(RX))),
                      ignore.case = TRUE))) {
          # if it is NOT a multipolygon df
          if (!any(grepl("multi", class(sf::st_geometry(sf::st_as_sf(RX))),
                         ignore.case = TRUE))) {
            RX <- sf::st_cast(sf::st_as_sf(RX), "MULTIPOLYGON")
            RX$gid <- 1:nrow(RX)
            invisible(DBI::dbSendQuery(
                db,
                paste0(
                  "DELETE FROM ", farmername, "_a.rx
                  WHERE field = '", fieldname, "'
                  AND rx_year = 'rx';")
            ))
            tryCatch(sf::st_write(RX, db, paste0(farmername, "_a.rx"), append = TRUE),
                     error = function(e) {
                       print(paste0("RX already exists in database"))
                     })
            RX <- as(RX, "Spatial")
          } else {
            suppressMessages(rpostgis::pgInsert(
              db,
              c(paste0(farmername, "_a"), "rx"),
              RX,
              geom = "geometry",
              new.id = "gid",
              upsert.using = c("gid", "field", "rxyear")
            ))
            for (j in 1:length(names(RX))) {
              tryCatch({
                DBI::dbSendQuery(db,
                                 paste0("ALTER TABLE ", farmername, "_a.rx
                                        ALTER COLUMN ", names(RX)[j], " TYPE TEXT"))
              },
              error = function(e) {print(paste0("warning in column ", names(RX)[j]))})
            }
          }
        } else {
          # add file to database (upsert using field, year, orig_file) - gid added here
          suppressMessages(rpostgis::pgInsert(
            db,
            c(paste0(farmername, "_a"), "rx"),
            RX,
            geom = "geometry",
            new.id = "gid",
            upsert.using=c("gid", "fieldname", "rxyear")
          ))
          for (j in 1:length(names(RX))) {
            tryCatch({
              DBI::dbSendQuery(db,
                               paste0("ALTER TABLE ", farmername, "_a.rx
                                      ALTER COLUMN ", names(RX)[j], " TYPE TEXT"))
            },
            error = function(e) {print(paste0("warning in column ", names(RX)[j]))})
          }
        }
      }
      return()
    }
  ),
  private = list(
    .selectExpvar = function() {
      expVar <- as.character(select.list(
        c("As-Applied Nitrogen", "As-Applied Seed Rate"),
        title = "Select experimental variable."))
      self$expvar <- ifelse(expVar == "As-Applied Nitrogen", "aa_n", "aa_sr")
    },
    .selectGenType = function() {
      gen_type <- as.character(select.list(
        c("NewExp_noStrat", "NewExp_wStrat", "ExpRx", "Rx"),
        title = "Select the type of output to create (i.e. experiment and/or prescription."))
      self$gen_type <- gen_type
    },
    .selectTrtLength = function() {
      self$trt_length <- as.numeric(readline(
        "Provide the length of the treatment (in feet) to apply the experimental inputs: "
      ))
      self$trt_length <- round(self$trt_length, 0) * 0.3048 # convert to meters
    },
    .selectBoomWidth = function() {
      self$boom_width <- as.numeric(readline(
        "Provide the length of the boom of the sprayer or width of the spreader (in feet) to determine the width of the treatment plots: "
      ))
      self$boom_width <- round(self$boom_width, 0) * 0.3048 # convert to meters
    },
    .selectOrientation = function() {
      self$orientation <- as.numeric(readline(
        "Provide the orientation of the treatments to N (in degrees). Used to rotate the treatments to align with farmer management: "
      ))
    },
    .selectFldProp = function() {
      self$fld_prop <- as.numeric(readline(
        "Provide the proportion (0 - 1) of the field to use for experimentation: "
      ))
      stopifnot(is.numeric(self$fld_prop),
                self$fld_prop >= 0 & self$fld_prop <= 1)
    },
    .selectOutPath = function() {
      self$SAVE <- as.character(select.list(
        c(TRUE, FALSE),
        title = "Select whether to save the output map and plots from the prescription generation."
      ))
      self$out_path <- as.character(readline(
        "Provide the path to a folder in which to save simulation outputs (i.e. '~/path/to/folder' or 'C:/path/to/folder'). Type NA to prevent any folders from being created.: "
      ))
      if (is.na(self$out_path) | is.null(self$out_path)) {
        self$SAVE <- FALSE
      }
    },
    .selectConv = function() {
      self$conv <- as.numeric(readline(
            "Conversion from the experimental units to the product applied (i.e. 3.55 gal/c urea = 1 lb N/ac, or 0.46 with NPK). Select 1 for no conversion needed (e.g. for seeding rates): "
      ))
      self$conv <- 1 / self$conv
    },
    .selectBaseRate = function() {
      self$base_rate <- as.numeric(readline(
        "Provide the base rate to apply around experimental rates for new experiments, OR the farmer selected check rate for prescriptions: "
      ))
    },
    .selectRxYears = function(farmername, fieldname) {
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
          title = paste0("Select year(s) to generate a prescription for.")
        ))
      } else {
        stop("No satellite data exists. Return to the aggregation step (Vignette 3) and aggregate 'Satellite' data.")
      }
    },
    .selectYearForRx = function() {
      self$rx_for_year <- as.numeric(readline(
        "Provide the year to create the experiment or prescription for: "
      ))
    },
    .selectToDB = function() {
      self$to_DB <- as.logical(select.list(
        c(TRUE, FALSE),
        title = paste0("Select whether to save the experiment or prescription to the database.")
      ))
    },
    .selectFarmer = function() {
      self$farmername <- as.character(select.list(
        unique(DBI::dbGetQuery(self$dbCon$db, "SELECT farmer FROM all_farms.farmers")$farmer),
        multiple = FALSE,
        title = "Select farm to analyze a field in."))
    },
    .selectFieldname = function() {
      farmeridx <- DBI::dbGetQuery(self$dbCon$db,
                                   paste0("SELECT farmeridx FROM
                                   all_farms.farmers
                                          WHERE farmer = '", self$farmername, "'")) %>%
        as.numeric()
      flds <- DBI::dbGetQuery(
        self$dbCon$db,
        paste0("SELECT DISTINCT fieldname
               FROM all_farms.fields
               WHERE farmeridx = '", farmeridx,"';")
      )$fieldname
      self$fieldname <-
        as.character(select.list(
          unique(flds),
          multiple = TRUE,
          title = "Select field(s) to make a prescription for."
        )
      )
    },
    .selectExpRateLength = function() {
      self$exp_rate_length <- as.numeric(readline(
        "Provide the number of experimental rates to apply, you may need to take into account the number of optimum rates to not exceed your equipments capacity for applying rates and the base rate: "
      ))
      self$exp_rate_length <- round(self$exp_rate_length, 0)
    },
    .selectExpRates = function() {
      self$exp_rates <- rep(NA, self$exp_rate_length)
      for (i in 1:self$exp_rate_length) {
        self$exp_rates[i] <- as.numeric(readline(
          paste0("Provide experimental rate ", i, ": ")
        ))
      }
    },
    .selectExpRatesProp = function() {
      self$exp_rates_prop <- rep(NA, self$exp_rate_length)
      for (i in 1:self$exp_rate_length) {
        self$exp_rates_prop[i] <- as.numeric(readline(
          paste0("Provide the proportion of experimental rates to apply experimental rate ", i, ": ")
        ))
      }
      stopifnot(sum(self$exp_rates_prop) >= 0.99)
    },
    .selectStratDat = function() {
      stopifnot(!is.null(self$fieldname))
      strat_dat <- as.list(self$fieldname)
      for (i in 1:length(strat_dat)) {
        self$strat_dat[[i]] <- as.character(select.list(
          c("yld", "pro", "aa_n", "aa_sr"),
          title = paste0("Provide the types of data to stratify on for ", self$fieldname[i], "."),
          multiple = TRUE))
      }
    },
    .selectStratYears = function() {
      self$strat_dat_year <- self$strat_dat
      for (i in 1:length(self$strat_dat)) {
        for (j in 1:length(self$strat_dat[[i]])) {
          if (!grepl("aa_", self$strat_dat[[i]][j])) {
            dat_exist <- as.logical(
              DBI::dbGetQuery(
                self$dbCon$db,
                paste0("SELECT EXISTS (
                SELECT 1
                FROM information_schema.tables
                WHERE table_schema = '", self$farmername, "_a'
                AND table_name = '", self$strat_dat[[i]][j], "')")
              )
            )
            if (dat_exist) {
              unq_years <- unique(DBI::dbGetQuery(
                self$dbCon$db,
                paste0("SELECT DISTINCT year
                       FROM ", self$farmername, "_a.", self$strat_dat[[i]][j]," tab
                       WHERE tab.field = '", self$fieldname[i], "'")
              )$year)
              self$strat_dat_year[[i]][j] <- as.numeric(select.list(
                unq_years,
                multiple = TRUE,
                title = paste0("Select year(s) to use for stratification for the  ", self$strat_dat[[i]][j], " data.")
              ))
            } else {
              stop(paste0("No available", self$strat_dat[[i]][j], " data for ", self$fieldname[i], " to stratify on."))
            }
          } else {
            # check for yld or check for pro
            dat_exist <- as.logical(
              DBI::dbGetQuery(
                self$dbCon$db,
                paste0("SELECT EXISTS (
                SELECT 1
                FROM information_schema.tables
                WHERE table_schema = '", self$farmername, "_a'
                AND table_name = 'yld')")
              )
            )
            if (dat_exist) {
              unq_years <- unique(DBI::dbGetQuery(
                self$dbCon$db,
                paste0("SELECT DISTINCT year
                       FROM ", self$farmername, "_a.yld tab
                       WHERE tab.field = '", self$fieldname[i], "'")
              )$year)
              self$strat_dat_year[[i]][j] <- as.numeric(select.list(
                unq_years,
                multiple = TRUE,
                title = paste0("Select year(s) to use for stratification for the  ", self$strat_dat[[i]][j], " data.")
              ))
            } else {
              dat_exist <- as.logical(
                DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT EXISTS (
                SELECT 1
                FROM information_schema.tables
                WHERE table_schema = '", self$farmername, "_a'
                AND table_name = 'pro')")
                )
              )
              if (dat_exist) {
                unq_years <- unique(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT DISTINCT year
                       FROM ", self$farmername, "_a.pro tab
                       WHERE tab.field = '", self$fieldname[i], "'")
                )$year)
                self$strat_dat_year[[i]][j] <- as.numeric(select.list(
                  unq_years,
                  multiple = TRUE,
                  title = paste0("Select year(s) to use for stratification for the  ", self$strat_dat[[i]][j], " data.")
                ))

              } else {
                stop(paste0("No available", self$strat_dat[[i]][j], " data for ", self$fieldname[i], " to stratify on."))
              }
            }
          }
        }
      }
    },
    .selectExpRateGen = function() {
      self$exp_rate_gen <- as.logical(select.list(
        c(TRUE, FALSE),
        title = paste0("Select whether to create experimental rates in gaps between optimum rates, or to provide your own rates.")
      ))
    },
    .selectOptRateLength = function() {
      self$opt_rate_length <- as.numeric(readline(
        "Provide the number of optimum rates to apply, you may need to take into account the number of experimental rates to not exceed your equipments capacity for applying rates: "
      ))
      self$opt_rate_length <- round(self$opt_rate_length, 0)
    },
    .selectMgmtScen = function() {
      mgmt_scen <- as.character(select.list(
        c("SSOPT", "FFOPT", "FS", "Min", "Opp"),
        title = "Select the management scenario to use for the prescription output. See documentation for acronym descriptions."))
      self$mgmt_scen <- mgmt_scen
    },
    .loadGenerator = function() {
      out_gen <- ifelse(grepl("Rx", self$gen_type), "RxGen", "ExpGen")
      if (grepl("Exp", out_gen)) {
        init_text <- c("dbCon", "trt_length", "boom_width", "orientation", "fld_prop",
                       "conv", "base_rate", "rx_for_year", "out_path", "SAVE", "fieldname",
                       "farmername", "exp_rate_length", "exp_rates", "exp_rates_prop")
        if (grepl("wStrat", self$gen_type)) {
          init_text <- c(init_text, "strat_dat", "strat_dat_year")
        }
      } else {
        init_text <- c("dbCon", "simClass", "mgmt_scnen", "trt_length", "boom_width",
                       "orientation", "conv", "base_rate", "rx_years",
                       "rx_for_year", "out_path", "SAVE", "opt_rate_length")
        if (grepl("Exp", self$gen_type)) {
          init_text <- c(init_text, "fld_prop", "exp_rate_gen", "exp_rate_length",
                         "exp_rates", "exp_rates_prop")
        }
      }
      init_text <- paste0("self$", init_text) %>%
        paste(collapse = ", ")
      return(eval(parse(text = paste0(out_gen, "$new(", init_text, ")"))))
    },
    .plotRxMap = function(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername) {
      color <- rev(colorRamps::matlab.like2(15))
      stopifnot(
        length(var_col_name) == length(var_label),
        length(var_col_name) == length(var_main_label),
        length(var_label) == length(var_main_label),
        is.character(var_col_name),
        is.character(var_label),
        is.character(var_main_label),
        is.character(fieldname),
        length(fieldname) == 1,
        is.character(farmername),
        !is.null(dat$x),
        !is.null(dat$y),
        any(grepl(var_col_name, names(dat)))
      )
      utm_zone <- OFPE::findUTMzone(farmername = farmername)

      df <- as.data.frame(dat)
      sp <- sp::SpatialPoints(coords = df[, c("x", "y")])
      utm <- sf::st_as_sf(sp, remove_coordinates = FALSE)
      utm <- cbind(utm, sp@coords)
      if (is.na(raster::crs(utm))) {
        sf::st_crs(utm) <- utm_zone
      }
      utm <- sf::st_transform(utm, "epsg:4326")
      utm[, 1:2] <- sp::coordinates(as(utm, "Spatial"))
      llc <- sp::coordinates(as(utm, "Spatial")) %>%
        as.data.frame() %>%
        `names<-`(c("x", "y"))
      sp <- sp::SpatialPoints(coords = llc[, c("x", "y")])
      e <- raster::extent(llc[, c("x", "y")])
      map <- ggmap::get_map(location = c(lon = mean(sp::coordinates(as(utm, "Spatial"))[, 1]),
                                         lat = mean(sp::coordinates(as(utm, "Spatial"))[, 2])),
                            zoom = 14, maptype = "satellite", source = "google")
      dat <- sf::st_transform(dat, "epsg:4326")
      # attr(map, "bb")$ll.lat <- sf::st_bbox(dat)["ymin"]
      # attr(map, "bb")$ll.lon <- sf::st_bbox(dat)["xmin"]
      # attr(map, "bb")$ur.lat <- sf::st_bbox(dat)["ymax"]
      # attr(map, "bb")$ur.lon <- sf::st_bbox(dat)["xmax"]

      var_map <-
        ggmap::ggmap(map, extent  =  "panel") +
        ggplot2::coord_sf(crs = sf::st_crs(4326)) +
        ggplot2::geom_sf(data = dat, ggplot2::aes(fill = exprate), inherit.aes = FALSE) +
        ggplot2::scale_fill_gradientn(limits = c(floor(min(dat$exprate)),
                                                 ceiling(max(dat$exprate))),
                                      colours = color,
                                      breaks = seq(as.integer(floor(min(dat$exprate))),
                                                   as.integer(ceiling(max(dat$exprate))),
                                                   by = (ceiling(max(dat$exprate)) -
                                                           floor(min(dat$exprate))) / 5),
                                      name = var_label) +
        ggplot2::scale_x_continuous(limits = c(e@xmin-0.002, e@xmax+0.002),
                                    expand = c(0, 0),
                                    breaks = c(e@xmin-0.002, e@xmax+0.002)) +
        ggplot2::scale_y_continuous(limits = c(e@ymin-0.002, e@ymax+0.002),
                                    expand = c(0, 0)) +
        ggplot2::labs(title = var_main_label, x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank()) +
        OFPE::scale_bar(lon = e@xmin-0.0015,
                        lat = e@ymin-0.0015,
                        distance_lon = 0.2,
                        distance_lat = .01,
                        distance_legend = -.01,
                        dist_unit = "km",
                        orientation = TRUE,
                        arrow_length = .05,
                        arrow_distance = .02) %>%
        suppressMessages()
      return(var_map)
    }
  )
)







