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
#' a set up SimClass. This implies that the user has initialized and set up
#' the required DatClass, ModClass, and EconDat R6 objects. The user must pass a
#' SimClass object that has been executed. This simulation output data is used to
#' generate the experimental prescriptions and the prescriptions.
#'
#' If the user allows, the prescriptions or experiments are saved to the 'Outputs' folder
#' and are saved in the OFPE database in the farmer specific aggregated schema in a table
#' called 'rx' (farmername_a.rx).
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{SimClass}} for the class that contains simulation outputs,
#' \code{\link{ExpGen}} for the class for creating new experiments, and
#' \code{\link{RxGen}} for the class that creates experimental prescriptions
#' or pure prescriptions.
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
    #' @field trt_width The width of the treatment blocks. Should be a multiple of 
    #' the sprayer boom or spreader width, given as feet (converted to meters).
    trt_width = NULL,
    #' @field heading Numeric, heading in degrees from true north to rotate the 
    #' experiment/prescription to. Default is 0 (no rotation). Note that if a 
    #' heading is provided, the grid is trimmed based on the buffered boundary but
    #' rotation caused by providing a heading may skew treatment rates so that 
    #' they encroach into the cleanup strip.
    heading = 0,
    #' @field fld_prop The proportion of the field to apply experimental
    #' rates to (i.e. 0.5 or 1.0), OR, the percent of available cells to
    #' apply check rates to (i.e. 0.05, 0.1).
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
    #' @field SAVE Logical, whether to save figures and the experiment or prescription.
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
    #' @field strat_dat_parms Named list by fieldname that contains a list for each field
    #' containing named slots for 'table', 'grid_size', 'path', 'year', and 'col_name' 
    #' to define the stratification data to use for randomly applying experimental rates.
    #' Set named slots to NA for any fields that you don't want to make stratified
    #' rates on. You can stratify on multiple variables per field, with priority 
    #' given by order. Each of the sublist slots for each field must have the same 
    #' dimensions. Note that more stratification variables increases processing times.
    #' 
    #' The table ('table') indicates the location within the database that the stratification
    #' data is stored in. This can either be from an aggregated table ('yld', 'pro',
    #' or 'sat') or can be from a raw table. Simply specify the table name and the 
    #' schema will be derived from the farmername. For data from an aggregated table, 
    #' the user must also provide the size ('grid_size') of the grid cells used to 
    #' aggregate the data the user desires in the aggregated dataset (i.e. 10, 30 meters). 
    #' This is a numeric variable and if stratifying on raw data, this parameter 
    #' can be left NA. Conversely, if you are stratifying on raw data, an additional 
    #' parameter called 'path' needs to be supplied in a named slot of each field's 
    #' sublist to specify the original filename of the data imported into the database. 
    #' If the desired data is from an aggregated table than enter NA for the 'path'. The year of 
    #' the desired data must also be provided ('year'). This is to specify which data in 
    #' the aggregated table to use. If using raw data, the year is automatically 
    #' derived from the data specified by the filename. Finally, the user must 
    #' supply the column name ('col_name') of the variable to stratify on. 
    #' This must be supplied for both raw and aggregated data.
    strat_dat_parms = NULL,
    #' @field simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' set up. The RxClass will check the SimClass for simulation output data
    #' from which years(s) can be selected from.
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
    #' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
    #' input rate in conventional system types, and the farmer selected rate for
    #' organic systems, both of which are already provided.
    mgmt_scen = NULL,
    #' @field expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,
    #' @field min_app_rate The minimum as-applied rate that the equipment can
    #' apply. This must be in the units of the input applied to the field,
    #' which is not necessarily the same as the experimental input. This is
    #' an equipment restriction specific to the farmer.
    min_app_rate = NULL,
    #' @field buffer_width The width of the buffer from the field edge within which
    #' to place experiments or prescriptions. Provided by the user in feet and
    #'converted to meters internally.
    buffer_width = NULL,
    #' @field min_rate_jumps Optional, supply either 'N/S' or 'E/W' to indicate 
    #' direction in which to minimize rate jumps. This is the predominant direction 
    #' that the experimental input is applied across the field. This function minimizes
    #' the difference in rates between adjacent treatments for easier use on the 
    #' equipment. Note that this will eradicate any randomization/optimization of 
    #' rates and will partially or completely remove stratification. This function makes 
    #' sure that the rates do not vary by more than 2 rate levels in the direction 
    #' specified. Default is NULL, which prevents execution. This will not guarantee
    #' eradication of all rate jumps, but will reduce the amount. 
    min_rate_jumps = NULL,
    
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
    #' user. For new experiments, the user will not require a set up
    #' SimClass object, however if creating a prescription or experimental
    #' prescription the user will need to pass in a set up and/or executed
    #' SimClass R6 class, EVEN IF using the interactive methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' set up and simulations from years that the user expects the upcoming year
    #' will be like must be executed.
    #' @param gen_type What type of output to generate; 'NewExp_NoStrat' for
    #' a new experiment with randomly placed experimental rates with no stratification,
    #' 'NewExp_wStrat' for a new experiment with randomly placed experimental rates
    #' stratified on user specified data, 'ExpRx' for a experimental rates randomly
    #' placed, stratified on the optimum rates from the user selected prescription,
    #' or 'Rx' where the prescription is output to the user, with check rates of a
    #' farmer selected base rate randomly placed, stratified on optimum rates.
    #' @param trt_length Length, in feet, for which to apply treatments.
    #' @param trt_width The width, in feet, for which to apply treatments.
    #' @param heading Numeric, heading in degrees from true north to rotate the 
    #' experiment/prescription to. Default is 0 (no rotation). Note that if a 
    #' heading is provided, the grid is trimmed based on the buffered boundary but
    #' rotation caused by providing a heading may skew treatment rates so that 
    #' they encroach into the cleanup strip.
    #' @param fld_prop The proportion of the field to apply experimental
    #' rates to (i.e. 0.5 or 1.0), OR, the percent of available cells to
    #' apply check rates to (i.e. 0.05, 0.1).
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
    #' @param SAVE Logical, whether to save figures and the experiment or prescription.
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
    #' @param strat_dat_parms Named list by fieldname that contains a list for each field
    #' containing named slots for 'table', 'grid_size', 'path', 'year', and 'col_name' 
    #' to define the stratification data to use for randomly applying experimental rates. 
    #' You can stratify on multiple variables per field, with priority given by order. 
    #' Each of the sublist slots for each field must have the same dimensions. Note
    #' that more stratification variables increases processing times.
    #' 
    #' The table ('table') indicates the location within the database that the stratification
    #' data is stored in. This can either be from an aggregated table ('yld', 'pro',
    #' or 'sat') or can be from a raw table. Simply specify the table name and the 
    #' schema will be derived from the farmername. For data from an aggregated table, 
    #' the user must also provide the size ('grid_size') of the grid cells used to 
    #' aggregate the data the user desires in the aggregated dataset (i.e. 10, 30 meters). 
    #' This is a numeric variable and if stratifying on raw data, this parameter 
    #' can be left NA. Conversely, if you are stratifying on raw data, an additional 
    #' parameter called 'path' needs to be supplied in a named slot of each field's 
    #' sublist to specify the original filename of the data imported into the database. 
    #' If the desired data is from an aggregated table than enter NA for the 'path'. The year of 
    #' the desired data must also be provided ('year'). This is to specify which data in 
    #' the aggregated table to use. If using raw data, the year is automatically 
    #' derived from the data specified by the filename. Finally, the user must 
    #' supply the column name ('col_name') of the variable to stratify on. 
    #' This must be supplied for both raw and aggregated data.
    #' @param exp_rate_gen Logical, for the experimental prescription provide
    #' TRUE or FALSE for whether to create experimental rates from gaps in the
    #' optimized rates. These experimental rates are placed between optimum rates
    #' to establish a range of rates for which to improve model predictions in future
    #' years.
    #' @param opt_rate_length Provide the number of optimum rates to use.
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
    #' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
    #' input rate in conventional system types, and the farmer selected rate for
    #' organic systems, both of which are already provided.
    #' @param expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param min_app_rate The minimum as-applied rate that the equipment can
    #' apply. This must be in the units of the input applied to the field,
    #' which is not necessarily the same as the experimental input. This is
    #' an equipment restriction specific to the farmer.
    #' @param buffer_width The width of the buffer from field edge for
    #' experiments or prescriptions (feet).
    #' @param min_rate_jumps Optional, supply either 'N/S' or 'E/W' to indicate 
    #' direction in which to minimize rate jumps. This is the predominant direction 
    #' that the experimental input is applied across the field. This function minimizes
    #' the difference in rates between adjacent treatments for easier use on the 
    #' equipment. Note that this will eradicate any randomization/optimization of 
    #' rates and will partially or completely remove stratification. This function makes 
    #' sure that the rates do not vary by more than 2 rate levels in the direction 
    #' specified. Default is NULL, which prevents execution. This will not guarantee
    #' eradication of all rate jumps, but will reduce the amount.
    #' @return An initialized RxClass R6 class object.
    initialize = function(dbCon,
                          simClass = NULL,
                          gen_type = NULL,
                          trt_length = NULL,
                          trt_width = NULL,
                          heading = 0,
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
                          strat_dat_parms = NULL,
                          exp_rate_gen = NULL,
                          opt_rate_length = NULL,
                          mgmt_scen = NULL,
                          expvar = NULL,
                          min_app_rate = NULL,
                          buffer_width = 0,
                          min_rate_jumps = NULL) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon

      if (!is.null(expvar)) {
        stopifnot(is.character(expvar),
                  any(grepl("As-Applied Nitrogen|As-Applied Seed Rate", expvar)))
        self$expvar <- ifelse(expvar == "As-Applied Nitrogen", "aa_n", "aa_sr")
      }
      if (!is.null(gen_type)) {
        stopifnot(is.character(gen_type),
                  any(grepl("NewExp_noStrat|NewExp_wStrat|ExpRx|Rx", gen_type)))
        self$gen_type <- gen_type
      }
      if (!is.null(trt_length)) {
        stopifnot(is.numeric(trt_length),
                  trt_length > 0)
        self$trt_length <- round(trt_length, 0) * 0.3048 # convert to meters
      }
      if (!is.null(trt_width)) {
        stopifnot(is.numeric(trt_width),
                  trt_width > 0)
        self$trt_width <- round(trt_width, 0) * 0.3048 # convert to meters
      }
      if (!is.null(buffer_width)) {
        stopifnot(is.numeric(buffer_width),
                  buffer_width > 0)
        self$buffer_width <- round(buffer_width, 0) * 0.3048 # convert to meters
      }
      if (!is.null(heading)) {
        stopifnot(is.numeric(heading))
        self$heading <- heading
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
      if (!is.null(simClass)) {
        stopifnot(any(grepl("SimClass", class(simClass))))
        self$simClass <- simClass
        self$expvar <- self$simClass$datClass$expvar
        self$out_path <- self$simClass$out_path
        self$farmername <- self$simClass$datClass$farmername
        self$fieldname <- self$simClass$datClass$fieldname
      }
      if (!is.null(fieldname) & is.null(self$fieldname)) {
        stopifnot(is.character(fieldname))
        self$fieldname <- fieldname
      }
      if (!is.null(strat_dat_parms)) {
        if (grepl("NewExp_wStrat", self$gen_type)) {
          stopifnot(length(strat_dat_parms) == length(fieldname))
          # for (i in 1:length(strat_dat_parms)) {
            # stopifnot(names(strat_dat_parms[[i]]) == c("table", "path", "year", "col_name"))
            # for (j in 1:length(strat_dat_parms[[i]])) {
            #   stopifnot(is.character(strat_dat_parms[[i]][j]) |
            #               is.numeric(strat_dat_parms[[i]][j]) |
            #               is.na(strat_dat_parms[[i]][j]))
            # }
          # }
          self$strat_dat_parms <- strat_dat_parms
        }
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
                  any(grepl("SSOPT|FFOPT|FS|Min", mgmt_scen)))
        self$mgmt_scen <- mgmt_scen
      }
      if (!is.null(min_app_rate)) {
        stopifnot(is.numeric(min_app_rate))
        self$min_app_rate <- min_app_rate
      }
      if (!is.null(min_rate_jumps)) {
        stopifnot(is.character(min_rate_jumps),
                  any(grepl("N/S|E/W", min_rate_jumps)))
        self$min_rate_jumps <- min_rate_jumps
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the prescription or
    #' experiment generation process. It is recommended to use this interactive
    #' method rather than passing in inputs as arguments unless an experienced
    #' user. If the user is creating an experimental prescription or prescription,
    #' they must have set up this class with a SimClass object, even if using
    #' this interactive method for selecting other inputs.
    #'
    #' The user first selects whether to make a prescription or experiment. If
    #' the user specifies a type and has not passed in a required input upon
    #' initialization, an error will be thrown. Based on the user specification
    #' different selections are required for the user to set up the generation
    #' method.
    #'
    #' The user will have to select the treatment length and width, the heading 
    #' to align the treatments relative to N/S, the base rate to apply to around 
    #' experiments or for checks in the prescription, the year(s) the farmer 
    #' thinks the upcoming year will resemble and to make the prescription for, 
    #' and whether to save the prescription or experiment in the database.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A 'RxClass' object with complete user selections.
    selectInputs = function() {
      private$.selectGenType()
      private$.selectTrtLength()
      private$.selectTrtWidth()
      private$.selectHeading()
      private$.selectBaseRate()
      private$.selectYearForRx()
      private$.selectToDB()
      private$.selectConv()
      if (is.null(self$simClass)) {
        private$.selectOutPath()
      } else {
        self$SAVE <- self$simClass$SAVE
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
        }
      }
      if (grepl("Rx", self$gen_type)) {
        stopifnot(!is.null(self$simClass))
        private$.selectRxYears(self$simClass$datClass$farmername,
                               self$simClass$datClass$fieldname,
                               self$dbCon$db)
        private$.selectOptRateLength()
        private$.selectFldProp()
        private$.selectMgmtScen()
        if (grepl("ExpRx", self$gen_type)) {
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
      private$.selectMinAppRate()
      private$.selectMinRateJumps()
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
      self$out_gen$RX$applrate <- ifelse(self$out_gen$RX$applrate < self$min_app_rate &
                                           self$out_gen$RX$applrate > 0,
                                         self$min_app_rate,
                                         self$out_gen$RX$applrate)
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
    #' @param to_DB Logical, whether to save the prescription or experiment
    #' into the database. If NULL uses the user selected choice. If not NULL
    #' and is logical, argument replaces previously set to_DB options for the
    #' entire class.
    #' @return Shapefile and map saved in 'Outputs' folder and in database.
    saveOutputs = function(SAVE = NULL, to_DB = NULL) {
      if (is.null(SAVE)) {
        SAVE <- self$SAVE
      } else {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }
      if (is.null(to_DB)) {
        to_DB <- self$to_DB
      } else {
        stopifnot(is.logical(to_DB))
        self$to_DB <- to_DB
      }
      self$plotRxMap(SAVE)
      self$plotCellTypeMap(SAVE)
      if (SAVE) {
        suppressWarnings(sf::st_write(
          self$out_gen$RX,
          self$out_gen$out_name,
          quiet = TRUE,
          delete_dsn = TRUE))
      }
      if (to_DB) {
        invisible(self$uploadRxFun(
          self$out_gen$RX,
          self$dbCon$db,
          self$farmername,
          self$out_gen$unique_fieldname,
          self$rx_for_year,
          self$out_gen$size,
          self$out_gen$mgmt_scen
        ))
      }
    },
    #' @description
    #' This method is for plotting maps of simulation outcomes. These
    #' include the net-returns from the management strategies, predicted
    #' responses, and optimized rates. Is a wrapper to a private plotRxMap
    #' method.
    #' @param SAVE Logical, whether to save figure.
    #' @return Maps saved in 'Outputs/Rx/' and/or returned.
    plotRxMap = function(SAVE = TRUE) {
      if (is.null(self$fieldname)) {
        utm_zone <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$simClass$datClass$fieldname[1])
      } else {
        utm_zone <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$fieldname[1])
      }
      p <- private$.plotRxMap(self$out_gen$RX,
                              self$out_gen$var_col_name,
                              self$out_gen$var_label,
                              self$out_gen$var_main_label,
                              self$out_gen$unique_fieldname,
                              self$farmername,
                              utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()

      if (SAVE) {
        ggplot2::ggsave(
          file = self$out_gen$out_map_name,
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      #return(p)
    },
    #' @description
    #' This method is for plotting a map showing the distribution of cell
    #' types. This shows which cells are the optimized or management scenario
    #' rates, which are experimental rates, and where base rates are applied.
    #' @param SAVE Logical, whether to save figure.
    #' @return Maps saved in 'Outputs/Rx/' and/or returned.
    plotCellTypeMap = function(SAVE = TRUE) {
      if (is.null(self$fieldname)) {
        utm_zone <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$simClass$datClass$fieldname[1])
      } else {
        utm_zone <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$fieldname[1])
      }
      p <- private$.plotCellTypeMap(self$out_gen$RX,
                                    self$out_gen$unique_fieldname,
                                    self$farmername,
                                    self$rx_for_year,
                                    self$fieldname,
                                    utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()

      if (SAVE) {
        ggplot2::ggsave(
          file = self$out_gen$cell_out_map_name,
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }

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
    #' @param size The size of the treatment zones, which is the treatment width x
    #' the treatment length.
    #' @param mgmt_scen If the user is creating a prescription or experimental
    #' prescription, they must provide the management scenario to use for their
    #' prescription. The user can choose from the management options listed in
    #' the SimClass. The options are 'SSOPT': site-specific optimized rates,
    #' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
    #' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
    #' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
    #' input rate in conventional system types, and the farmer selected rate for
    #' organic systems, both of which are already provided. This is set to 'exp'
    #' for all new experiments.
    #' @param dtype The data type of the data for upload to the database, default
    #' to 'rx'.
    #' @return Output experiment or prescription saved in database.
    uploadRxFun = function(RX, db, farmername, fieldname, rx_year, size, mgmt_scen, dtype = "rx") {
      RX <- sf::st_transform(RX, "epsg:4326") %>%
        as("Spatial")
      schema <- paste0(farmername, "_a")
      query <- paste0("ALTER TABLE ", schema,".", dtype,
                      " ADD PRIMARY KEY (gid, field, rxyear, size, mgmt_scen)")
      upserts <- c("gid", "field", "rxyear", "size", "mgmt_scen")
      tab_exist <- OFPE::tabExist(db, schema, dtype)
      if (!tab_exist) {
        RX <- OFPE::setNAtoNaN(RX)
        OFPE::importNewDat(db, RX, schema, dtype)
        invisible(DBI::dbGetQuery(db, query))
        OFPE::makeDBcolsText(db, names(RX), schema, dtype)
        OFPE::convPolyToMulti(db, RX, schema, dtype)
        OFPE::makeSpatialIndex(db, "rx_geom_idx_new", schema, dtype)
      } else {
        RX <- OFPE::standardizeColNames(db, RX,schema, dtype)
        RX <- OFPE::setNAtoNaN(RX)
        if (any(grepl("poly", class(sf::st_geometry(sf::st_as_sf(RX))),
                      ignore.case = TRUE))) {
          # if it is NOT a multipolygon df
          if (!any(grepl("multi", class(sf::st_geometry(sf::st_as_sf(RX))),
                         ignore.case = TRUE))) {
            invisible(DBI::dbSendQuery(
              db,
              paste0(
                "DELETE FROM ", schema, ".", dtype, "
                  WHERE field = '", fieldname, "'
                  AND rxyear = '", rx_year,"'
                  AND size = '", size, "'
                  AND mgmt_scen = '", mgmt_scen, "';")
            ))
            OFPE::importMulti(db, RX, schema, dtype)
          } else {
            OFPE::importDat(db, RX, schema, dtype, upserts)
            OFPE::makeDBcolsText(db, names(RX), schema, dtype)
          }
        } else {
          OFPE::importDat(db, RX, schema, dtype, upserts)
          OFPE::makeDBcolsText(db, names(RX), schema, dtype)
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
      if (is.null(self$simClass)) {
        gen_options <- c("NewExp_noStrat", "NewExp_wStrat")
      } else {
        gen_options <- c("ExpRx", "Rx")
      }
      gen_type <- as.character(select.list(
        gen_options,
        title = "Select the type of output to create (i.e. experiment and/or prescription."))
      self$gen_type <- gen_type
    },
    .selectTrtLength = function() {
      self$trt_length <- as.numeric(readline(
        "Provide the length of the treatment (in feet) to apply the experimental inputs: "
      ))
      self$trt_length <- round(self$trt_length, 0) * 0.3048 # convert to meters
    },
    .selectTrtWidth = function() {
      self$trt_width <- as.numeric(readline(
        "Provide the width of the treatment (in feet) to apply the experimental inputs. Should be a multiple of the boom or sprayer width: "
      ))
      self$trt_width <- round(self$trt_width, 0) * 0.3048 # convert to meters
    },
    .selectBuffWidth = function() {
      stopifnot(!is.null(self$trt_width))
      self$buffer_width <- as.numeric(readline(
        "Provide the width of an internal buffer from the field edge to place the prescriptions within (in feet). Prevents experiment from being in cleanup strips. Default is the treatment width: "
      ))
      if (!is.null(self$buffer_width) | !is.na(self$buffer_width)) {
        self$buffer_width <- round(self$buffer_width, 0) * 0.3048 # convert to meters
      } else {
        self$buffer_width <- self$trt_width
      }
    },
    .selectHeading = function() {
      self$heading <- as.numeric(readline(
        "Provide the heading of the treatments to N (in degrees). Used to rotate the treatments to align with farmer management: "
      ))
    },
    .selectFldProp = function() {
      self$fld_prop <- as.numeric(readline(
        "Provide the proportion (0 - 1) of the field to use for experimentation (e.g. 0.8) or to apply check rates to (e.g. 0.1): "
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
    .selectRxYears = function() {
      self$rx_years <- as.numeric(select.list(
        names(self$simClass$sim_out),
        multiple = TRUE,
        title = paste0("Select the year(s) with weather conditions to generate a prescription based on.")
      ))
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
          paste0("Provide experimental rate (low to high) ", i, ": ")
        ))
      }
    },
    .selectExpRatesProp = function() {
      self$exp_rates_prop <- rep(NA, self$exp_rate_length)
      for (i in 1:self$exp_rate_length) {
        self$exp_rates_prop[i] <- as.numeric(readline(
          paste0("Provide the proportion of experimental rates to apply experimental rate (low to high) ", i, ": ")
        ))
      }
      stopifnot(sum(self$exp_rates_prop) >= 0.99)
    },
    .selectMinRateJumps = function() {
      min_rate_jumps <- as.character(select.list(
        c("Yes", "No"),
        title = "Select whether to minimize the jumps in rates between treatments. Will add time to processing."))
      if (min_rate_jumps == "Yes") {
        min_rate_jumps <- as.character(select.list(
          c("N/S", "E/W"),
          title = "Select the general direction of travel across the field. Rates are minimized across treatment columns or rows, respectively."))
        self$min_rate_jumps <- min_rate_jumps
      } 
    },
    .selectStratDat = function() {
      stopifnot(!is.null(self$fieldname))
      self$strat_dat_parms <- rep(list(NA), length(self$fieldname))
      self$strat_dat_parms <- lapply(self$strat_dat_parms, function(x) list(table = NA,
                                                                            grid_size = NA,
                                                                            path = NA, 
                                                                            year = NA, 
                                                                            col_name = NA))
      for (i in 1:length(self$fieldname)) {
        strat_dat <- as.logical(select.list(
          c(TRUE, FALSE),
          multiple = FALSE,
          title = paste0("TRUE/FALSE: Whether to randomly apply experimental rates with stratification in ", self$fieldname[i], ".")
        ))
        if (strat_dat) {
          strat_vars <- as.numeric(readline(prompt = paste0("Provide the number of variables to stratify on in ", self$fieldname[i], ". Submit 0 for no stratification.: ")))
          stopifnot(is.numeric(strat_vars),
                    !is.na(strat_vars))
          if (strat_vars != 0) {
            self$strat_dat_parms[[i]] <- lapply(self$strat_dat_parms[[i]], rep, NA, strat_vars)
            strat_vars <- 1:strat_vars
            
            for (j in 1:length(strat_vars)) {
              ## select table/type of data to gather
              table_selection <- as.character(select.list(
                c("Agg", "Raw"),
                multiple = FALSE,
                title = paste0("Select location of the table with stratification data ", strat_vars[j]," for ", self$fieldname[i], ".")
              ))
              schema <- ifelse(table_selection == "Raw", "_r", "_a")
              schema <- paste0(self$farmername, schema)
              tables <- suppressWarnings(DBI::dbGetQuery(
                self$dbCon$db,
                paste0("SELECT *
                     FROM pg_catalog.pg_tables 
                     WHERE schemaname = '", schema, "';")
              ))$tablename
              self$strat_dat_parms[[i]]$table[j] <- as.character(select.list(
                tables,
                multiple = FALSE,
                title = paste0("Select location of the table with stratification data ", strat_vars[j]," for ", self$fieldname[i], ".")
              ))
              
              ## if raw select original filename, else agg path = NA
              if (table_selection == "Raw") {
                filenames <- suppressWarnings(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT DISTINCT orig_file 
                     FROM ", schema, ".", self$strat_dat_parms[[i]]$table[j], ";")
                )$orig_file)
                self$strat_dat_parms[[i]]$path[j] <- as.character(select.list(
                  filenames,
                  multiple = FALSE,
                  title = paste0("Select the filename used to import the data into the database ('orig_file' column in raw data) for stratification data ", strat_vars[j]," for ", self$fieldname[i], ".")
                ))
              } 
              
              ## if raw get year, else ask user
              if (table_selection == "Raw") {
                self$strat_dat_parms[[i]]$year[j] <- suppressWarnings(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT DISTINCT year 
                     FROM ", schema, ".", self$strat_dat_parms[[i]]$table[j], "
                     WHERE orig_file = '", self$strat_dat_parms[[i]]$path[j], "';")
                )$year)
              } else {
                unq_years <- unique(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT DISTINCT year 
                     FROM ", schema, ".", self$strat_dat_parms[[i]]$table[j]," 
                     WHERE field = '", self$fieldname[i], "'")
                )$year)
                self$strat_dat_parms[[i]]$year[j] <- as.numeric(select.list(
                  unq_years,
                  multiple = FALSE,
                  title = paste0("Select year to use for stratification data ", self$strat_vars[j], " for ", self$fieldname[i], ".")
                ))
                
                unq_grid_size <- suppressWarnings(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT DISTINCT size 
                     FROM ", schema, ".", self$strat_dat_parms[[i]]$table[j], "
                       WHERE field = '", self$fieldname[i], "' 
                       AND year = '", self$strat_dat_parms[[i]]$year[j], "';")
                )$size)
                self$strat_dat_parms[[i]]$grid_size[j] <- as.numeric(select.list(
                  unq_grid_size,
                  multiple = FALSE,
                  title = paste0("Select the size of the grid (in meters) used to clean and aggregate stratification data ", strat_vars[j]," for ", self$fieldname[i], ".")
                ))
              }
              
              ## ask user to select from column names for strat
              if (table_selection == "Raw") {
                temp_tab_cols <-  suppressWarnings(DBI::dbGetQuery(
                  self$dbCon$db,
                  paste0("SELECT *
                       FROM  ", schema, ".", self$strat_dat_parms[[i]]$table[j], "
                       WHERE orig_file = '", self$strat_dat_parms[[i]]$path[j], "';")
                ))
              } else {
                temp_tab_cols <- OFPE::fetchAggStratDat(
                  table_var = self$strat_dat_parms[[i]]$table[j], 
                  strat_var = NULL, 
                  field = self$fieldname[i], 
                  year = self$strat_dat_parms[[i]]$year[j], 
                  farmername = self$farmername,
                  grid_size = self$strat_dat_parms[[i]]$grid_size[j],
                  db = self$dbCon$db
                )
              }
              temp_tab_cols <- private$.removeNAcols(temp_tab_cols)
              temp_tab_cols <- as.data.frame(t(subset(temp_tab_cols,
                                                      temp_tab_cols == FALSE)))
              
              self$strat_dat_parms[[i]]$col_name[j] <- as.character(select.list(
                colnames(temp_tab_cols),
                multiple = FALSE,
                title = paste0("Select column name with stratification data ", self$strat_vars[j], " for ", self$fieldname[i], ".")
              ))
            }
          }
        }
      }
      names(self$strat_dat_parms) <- self$fieldname
    },
    .removeNAcols = function(df) {
      for (i in 1:ncol(df)) {
        if (all(grepl("NaN", df[, i]))) {
          df[, i] <- as.numeric(df[, i])
        }
      }
      df[, grep("geom|geometry", colnames(df))] <- NULL
      df <- sapply(df, function(x) all(is.nan(x)|is.na(x)))
      return(df)
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
        c("SSOPT", "FFOPT", "FS", "Min"),
        title = "Select the management scenario to use for the prescription output. See documentation for acronym descriptions."))
      self$mgmt_scen <- mgmt_scen
    },
    .selectMinAppRate = function() {
      self$min_app_rate <- as.numeric(readline(
        "Provide the minimum as-applied rate (in units of conversion) that the producer's equipment can apply: "
      ))
    },
    .loadGenerator = function() {
      out_gen <- ifelse(grepl("Rx", self$gen_type), "RxGen", "ExpGen")
      if (grepl("Exp", out_gen)) {
        init_text <- c("dbCon", "trt_length", "trt_width", "heading", "fld_prop", 
                       "expvar", "conv", "base_rate", "rx_for_year", "out_path", 
                       "SAVE", "fieldname", "farmername", "exp_rate_length", 
                       "exp_rates", "exp_rates_prop", "buffer_width", "min_rate_jumps")
        if (grepl("wStrat", self$gen_type)) {
          init_text <- c(init_text, "strat_dat_parms")
        } 
      } else {
        init_text <- c("dbCon", "simClass", "mgmt_scen", "trt_length", "trt_width",
                       "heading", "expvar", "conv", "base_rate", "rx_years",
                       "rx_for_year", "out_path", "SAVE", "opt_rate_length", "fld_prop", 
                       "buffer_width", "min_rate_jumps")
        if (grepl("Exp", self$gen_type)) {
          init_text <- c(init_text, "exp_rate_gen", "exp_rate_length",
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
                          farmername,
                          utm_zone) {
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
      # map <- ggmap::get_map(location = c(lon = mean(sp::coordinates(as(utm, "Spatial"))[, 1]),
      #                                    lat = mean(sp::coordinates(as(utm, "Spatial"))[, 2])),
      #                       zoom = 14, maptype = "satellite", source = "google")
      map <- ggmap::get_map(location = c(e@xmin, e@ymin,
                                         e@xmax, e@ymax),
                            maptype = "satellite", source = "google", zoom = 15
      )
      dat <- sf::st_transform(dat, "epsg:4326")
      var_map <-
        ggmap::ggmap(map, extent  =  "panel") +
        ggplot2::coord_sf(crs = sf::st_crs(4326)) +
        # ggplot2::scale_x_continuous(limits = c(e@xmin-0.002, e@xmax+0.002),
        #                             expand = c(0, 0),
        #                             breaks = c(e@xmin-0.002, e@xmax+0.002)) +
        # ggplot2::scale_y_continuous(limits = c(e@ymin-0.002, e@ymax+0.002),
        #                             expand = c(0, 0)) +
        ggplot2::labs(title = var_main_label, x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank()) %>%
        # OFPE::scale_bar(lon = e@xmin-0.0015,
        #                 lat = e@ymin-0.0015,
        #                 distance_lon = 0.2,
        #                 distance_lat = .01,
        #                 distance_legend = -.01,
        #                 dist_unit = "km",
        #                 orientation = TRUE,
        #                 arrow_length = .05,
        #                 arrow_distance = .02) %>%
        suppressMessages()
      if (length(unique(dat$exprate)) > 10) {
        color <- rev(colorRamps::matlab.like2(15))
        var_map <- var_map +
          ggplot2::geom_sf(data = dat, ggplot2::aes(fill = exprate), inherit.aes = FALSE) +
          ggplot2::scale_fill_gradientn(limits = c(floor(min(dat$exprate, na.rm = TRUE)),
                                                   ceiling(max(dat$exprate, na.rm = TRUE))),
                                        colours = color,
                                        breaks = seq(as.integer(floor(min(dat$exprate, na.rm = TRUE))),
                                                     as.integer(ceiling(max(dat$exprate, na.rm = TRUE))),
                                                     by = (ceiling(max(dat$exprate, na.rm = TRUE)) -
                                                             floor(min(dat$exprate, na.rm = TRUE))) / 5),
                                        name = var_label)
      } else {
        MIN <- floor(min(dat$exprate, na.rm = TRUE))
        MAX <- ceiling(max(dat$exprate, na.rm = TRUE))
        STEP <- (MAX - MIN) / 5
        color <- rev(colorRamps::matlab.like2(length(unique(dat$exprate))))
        dat$exprate <- round(dat$exprate)
        dat$exprate_f <- factor(dat$exprate)
        var_map <- var_map +
          ggplot2::geom_sf(data = dat, ggplot2::aes(fill = exprate_f), inherit.aes = FALSE) +
          ggplot2::scale_fill_manual(limits = levels(dat$exprate_f),
                                        values = color,
                                        breaks = levels(dat$exprate_f),
                                        name = var_label)
      }
      return(var_map)
    },
    .plotCellTypeMap = function(dat,
                                unique_fieldname,
                                farmername,
                                rx_for_year,
                                fieldname,
                                utm_zone) {
      stopifnot(any(grepl("cell_type", names(dat))))

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
      # map <- ggmap::get_map(location = c(lon = mean(sp::coordinates(as(utm, "Spatial"))[, 1]),
      #                                    lat = mean(sp::coordinates(as(utm, "Spatial"))[, 2])),
      #                       zoom = 14, maptype = "satellite", source = "google")
      map <- ggmap::get_map(location = c(e@xmin, e@ymin,
                                         e@xmax, e@ymax),
                            maptype = "satellite", source = "google", zoom = 15
      )
      dat <- sf::st_transform(dat, "epsg:4326")
      dat$cell_type <- ifelse(dat$cell_type == "base", "Base",
                              ifelse(dat$cell_type == "exp", "Exp",
                                     ifelse(dat$cell_type == "check", "Check",
                                            dat$cell_type)))
      var_map <-
        ggmap::ggmap(map, extent  =  "panel") +
        ggplot2::coord_sf(crs = sf::st_crs(4326)) +
        ggplot2::geom_sf(data = dat, ggplot2::aes(fill = cell_type), inherit.aes = FALSE) +
        # ggplot2::scale_x_continuous(limits = c(e@xmin-0.002, e@xmax+0.002),
        #                             expand = c(0, 0),
        #                             breaks = c(e@xmin-0.002, e@xmax+0.002)) +
        # ggplot2::scale_y_continuous(limits = c(e@ymin-0.002, e@ymax+0.002),
        #                             expand = c(0, 0)) +
        ggplot2::labs(title = paste0(unique_fieldname, " Rate Type Applied"),
                      subtitle = rx_for_year,
                      x = "",
                      y = "",
                      fill = "Rate Type") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank()) %>%
        # OFPE::scale_bar(lon = e@xmin-0.0015,
        #                 lat = e@ymin-0.0015,
        #                 distance_lon = 0.2,
        #                 distance_lat = .01,
        #                 distance_legend = -.01,
        #                 dist_unit = "km",
        #                 orientation = TRUE,
        #                 arrow_length = .05,
        #                 arrow_distance = .02) %>%
        suppressMessages()
      return(var_map)
    }
  )
)







