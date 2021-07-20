#' @title R6 Class for generating a OFPE prescriptions or experimental prescriptions
#'
#' @description R6 class for for creating a prescription or experimental prescription
#' for a field of interest. The user can create a prescription or experimental
#' prescription based on the optimized rates from the user selected management
#' scenario for given economic and weather conditions. This process builds off of
#' the simulation R6 class (SimClass), where the Monte Carlo simulation for a year
#' or year(s) the user thinks the upcoming year is going to resemble, with the user
#' specified economic data, is used for identifying the optimum rates for each
#' management strategy at each part.
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
#' an set up SimClass. This implies that the user has initialized and set up
#' the required DatClass, ModClass, and EconDat R6 objects. The user must pass a
#' SimClass object that has been executed. This simulation output data is used to select
#' the years that the farmer thinks the upcoming year will resemble and generate
#' the experimental prescriptions and the prescriptions.
#'
#' This class follows the generator interface that includes an initialization method
#' and an 'executeOutput' method.
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{SimClass}} for the class that contains simulation outputs, and
#' \code{\link{ExpGen}} for the alternative class that creates new experiments.
#' @export
RxGen <- R6::R6Class(
  "RxGen",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' set up, however does not need to have the execution method performed.
    #' The RxClass will check the SimClass for simulation output data that matches
    #' the year of the data provided, and execute simulations for any years not
    #' present.
    simClass = NULL,
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
    #' @field trt_length Length, in meters, for which to apply treatments.
    trt_length = NULL,
    #' @field boom_width The width of the sprayer boom or spreader.
    boom_width = NULL,
    #' @field orientation TODO... Not implemented yet.
    orientation = NULL,
    #' @field expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,
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
    #' @field out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.). If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    out_path = NULL,
    #' @field SAVE Logical, whether to save figures and the prescription.
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise. If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    SAVE = NULL,
    #' @field opt_rate_length Provide the number of oprimum rates to use.
    #' This applies to prescriptions and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of experimental rates, so take your selections for
    #' the management scenario and number of experimental rates into account.
    opt_rate_length = NULL,

    #' @field fld_prop The percent of available cells to
    #' apply check rates to (i.e. 0.05, 0.1).
    fld_prop = NULL,
    #' @field exp_rate_gen Logical, for the experimental prescription provide
    #' TRUE or FALSE for whether to create experimentla rates from gaps in the
    #' optimized rates. These experimental rates are placed between optimum rates
    #' to establish a range of rates for which to improve model predictions in future
    #' years.
    exp_rate_gen = NULL,
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

    #' @field unique_fieldname Unique fieldname for the field(s) used for the experiment. This
    #' concatenates multiple fields with an ampersand. Used for labelling.
    unique_fieldname= NULL,
    #' @field rx_dt Data frame that contains the coordinates of locations to apply
    #' experimental inputs. For an experiment, these are the centroids of the grid
    #' made to aggregate data for the field.
    rx_dt = NULL,
    #' @field RX Table containing the geographic locations of the experiment. Also
    #' contains the rates in the experimental and as-applied units. This can be
    #' saved as a shapefile and given to the equipment applying the input.
    RX = NULL,
    #' @field out_name Created parameter for the file name of the prescription data,
    #' used for saving the data to the 'Outputs' folder.
    out_name = NULL,
    #' @field out_map_name Created parameter with the file name for the map of the
    #' prescription. Used for saving the map to the 'Outputs' folder.
    out_map_name = NULL,
    #' @field cell_out_map_name Created parameter with the file name for the map of the
    #' rate type applied to each cell. Used for saving the map to the 'Outputs' folder.
    cell_out_map_name = NULL,
    #' @field var The label of the variable to map. Used in figure labelling for plotting
    #' in RxClass.
    var = NULL,
    #' @field var_col_name The name of the column of the variable in the
    #' supplied data ('dat'). Used in figure labelling for plotting
    #' in RxClass.
    var_col_name = NULL,
    #' @field var_label The label to be applied to the legend of the map
    #' corresponding to the variable mapped. Used in figure labelling for plotting
    #' in RxClass.
    var_label = NULL,
    #' @field var_main_label The main label to apply to the map. Used in figure
    #' labelling for plotting in RxClass.
    var_main_label = NULL,
    #' @field size The size of the treatment zones, which is the treatment length x
    #' the boom width x 2.
    size = NULL,

    #' @description It is recommended to initialize this class through the RxClass,
    #' because it is integrated into the OFPE workflow. This also ensures all of the
    #' inputs are in the correct format and present.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param simClass If the user is creating an experimental prescription or
    #' prescription an R6 class SimClass object must be supplied. This has to be
    #' set up, however does not need to have the execution method performed.
    #' The RxClass will check the SimClass for simulation output data that matches
    #' the year of the data provided, and execute simulations for any years not
    #' present.
    #' @param mgmt_scen If the user is creating a prescription or experimental
    #' prescription, they must provide the management scenario to use for their
    #' prescription. The user can choose from the management options listed in
    #' the SimClass. The options are 'SSOPT': site-specific optimized rates,
    #' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
    #' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
    #' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
    #' input rate in conventional system types, and the farmer selected rate for
    #' organic systems, both of which are already provided.
    #' @param trt_length Length, in meters, for which to apply treatments.
    #' @param boom_width The width of the sprayer boom or spreader.
    #' @param orientation TODO... Not implemented yet.
    #' @param expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
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
    #' @param out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.). If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    #' @param SAVE Logical, whether to save figures and the prescription.
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise. If not applied and a SimClass object has
    #' been supplied, the selected option in that class will be used.
    #' @param opt_rate_length Provide the number of oprimum rates to use.
    #' This applies to prescriptions and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of experimental rates, so take your selections for
    #' the management scenario and number of experimental rates into account.
    #' @param fld_prop The percent of available cells to
    #' apply check rates to (i.e. 0.05, 0.1).
    #' @param exp_rate_gen Logical, for the experimental prescription provide
    #' TRUE or FALSE for whether to create experimentla rates from gaps in the
    #' optimized rates. These experimental rates are placed between optimum rates
    #' to establish a range of rates for which to improve model predictions in future
    #' years. Not needed if pure prescription.
    #' @param exp_rate_length Provide the length of experimental rates to apply.
    #' This applies to new experiments and experimental prescriptions. This
    #' represents the equipment constraints of the farmer. In the case of the
    #' experimental prescription, this number of rates does not include the
    #' number of rates for the optimized base map, so take your selections for
    #' the management scenario and number of optimum rates into account.
    #' Not needed if pure prescription.
    #' @param exp_rates Provide a vector of experimental rates equal to the
    #' number of experimental rates provided in 'exp_rate_length'. This is
    #' required for all new experiments, however can be left to null for
    #' experimental prescriptions if experimental rates should be generated
    #' based on gaps in optimum rates. Not needed if pure prescription.
    #' @param exp_rates_prop Provide proportions (0 - 1) for the length
    #' of experimental rates provided in 'exp_rate_length'. This is required
    #' for all new experiments and experimental prescriptions. Not needed if
    #' pure prescription.
    #' @return An initialized RxGen R6 class object.
    initialize = function(dbCon,
                          simClass,
                          mgmt_scen,
                          trt_length,
                          boom_width,
                          orientation,
                          expvar,
                          conv,
                          base_rate,
                          rx_years,
                          rx_for_year,
                          out_path = NULL,
                          SAVE,
                          opt_rate_length,
                          fld_prop,
                          exp_rate_gen = NULL,
                          exp_rate_length = NULL,
                          exp_rates = NULL,
                          exp_rates_prop = NULL) {
      stopifnot(!is.null(dbCon),
                !is.null(simClass),
                !is.null(mgmt_scen),
                !is.null(trt_length),
                !is.null(boom_width),
                !is.null(orientation),
                !is.null(fld_prop),
                !is.null(conv),
                !is.null(base_rate),
                !is.null(rx_years),
                !is.null(rx_for_year),
                !is.null(SAVE),
                !is.null(out_path),
                is.numeric(trt_length),
                trt_length > 0,
                is.numeric(boom_width),
                boom_width > 0,
                is.numeric(orientation),
                is.numeric(fld_prop),
                fld_prop >= 0 & fld_prop <= 1,
                is.numeric(conv),
                is.numeric(base_rate),
                is.numeric(rx_for_year),
                is.logical(SAVE),
                is.character(out_path),
                is.character(expvar),
                any(grepl("aa_n|aa_sr", expvar)),
                is.numeric(rx_years),
                any(grepl("SimClass", class(simClass))),
                is.numeric(opt_rate_length),
                is.character(mgmt_scen),
                any(grepl("SSOPT|FFOPT|FS|Min", mgmt_scen)))
      self$dbCon <- dbCon
      self$simClass <- simClass
      self$trt_length <- round(trt_length, 0)
      self$boom_width <- round(boom_width, 0)
      self$orientation <- orientation
      self$conv <- conv
      self$base_rate <- round(base_rate, 0)
      self$rx_for_year <- rx_for_year
      self$SAVE <- SAVE
      self$out_path <- out_path
      if (is.na(self$out_path) | is.null(self$out_path)) {
        self$SAVE <- FALSE
      }
      self$expvar <- expvar
      self$rx_years <- rx_years
      self$opt_rate_length <- opt_rate_length
      self$mgmt_scen <- mgmt_scen

      if (!is.null(fld_prop)) {
        stopifnot(is.numeric(fld_prop),
                  fld_prop >= 0 & fld_prop <= 1)
        self$fld_prop <- fld_prop
      }
      if (!is.null(exp_rate_gen)) {
        stopifnot(is.logical(exp_rate_gen))
        self$exp_rate_gen <- exp_rate_gen
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
      self$unique_fieldname <- OFPE::uniqueFieldname(self$simClass$datClass$fieldname)
      private$.makeRxDt()
    },
    #' @description
    #' Method for calling the execution method of the experiment
    #' generator. This randomly applies the experimental rates across
    #' the field. If the user selected stratification data, these are
    #' used for stratification during the random placement.
    #' @param None All parametes supplied upon initialization.
    #' @return A completed experiment table containing the output.
    executeOutput = function() {
      rx_sdt <- OFPE::getRxGrid(self$dbCon$db,
                                self$rx_dt,
                                self$simClass$datClass$farmername,
                                self$simClass$datClass$fieldname,
                                self$trt_length,
                                self$boom_width,
                                self$unique_fieldname,
                                self$mgmt_scen) %>%
        ## TODO - orientation
        OFPE::trimGrid(self$simClass$datClass$fieldname,
                       self$dbCon$db,
                       self$simClass$datClass$farmername) %>%
        private$.binOptRates()
      if (!is.null(self$exp_rate_gen)) {
        if (self$exp_rate_gen) {
          self$exp_rates <- private$.expRateGen(self$exp_rate_length,
                                                rx_sdt$exprate,
                                                self$simClass$AAmin,
                                                self$simClass$AArateCutoff)
        }
      }
      if (!is.null(self$exp_rates) &
          !is.null(self$exp_rates_prop) &
          !is.null(self$fld_prop) &
          !is.null(self$exp_rate_length)) {
        rx_sdt <- OFPE::applyExpRates(rx_sdt,
                                      self$exp_rates,
                                      self$exp_rates_prop,
                                      self$fld_prop,
                                      self$exp_rate_length)
      } else {
        rx_sdt <- private$.applyCheckRates(rx_sdt,
                                           self$base_rate,
                                           self$fld_prop,
                                           self$opt_rate_length)
      }
      fld_bound <- OFPE::makeBaseRate(self$dbCon$db,
                                         self$simClass$datClass$fieldname,
                                         self$unique_fieldname,
                                         self$base_rate,
                                         self$boom_width,
                                         self$trt_length,
                                         self$simClass$datClass$farmername,
                                      self$mgmt_scen)
      self$RX <- OFPE::makeRx(rx_sdt, fld_bound, self$rx_for_year, self$conv)
      private$.makeOutLabels()
    }
  ),
  private = list(
    .makeRxDt = function() {
      stopifnot(is.character(self$mgmt_scen),
                any(grepl("SSOPT|FFOPT|FS|Min", self$mgmt_scen)))
      rx_list <-as.list(
        which(names(self$simClass$sim_out) %in% self$rx_years)
      )
      for (i in 1:length(rx_list)) {
        rx_list[[i]] <- self$simClass$sim_out[[rx_list[[i]]]]$NRopt
        rx_list[[i]]$cell_id <- paste0(rx_list[[i]]$row, "_", rx_list[[i]]$col)
        rx_list[[i]]$cell_id <- factor(rx_list[[i]]$cell_id)
        cols <- grep("field|cell_id", names(rx_list[[i]]))
        non_nums <- rx_list[[i]][, ..cols] %>%
          unique()
        cols <- grep("field", names(rx_list[[i]]))
        rx_list[[i]] <- rx_list[[i]][, -..cols]
        rx_list[[i]] <- rx_list[[i]][, lapply(.SD, mean), by = rx_list[[i]]$cell_id]
        names(rx_list[[i]])[1] <- "cell_id"
        rx_list[[i]]$cell_id <- factor(rx_list[[i]]$cell_id)
        rx_list[[i]] <- merge(rx_list[[i]], non_nums, by = "cell_id")
        cols <- grep("sim", names(rx_list[[i]]))
        rx_list[[i]] <- rx_list[[i]][, -..cols]
      }
      self$rx_dt <- data.table::rbindlist(rx_list)
      cols <- grep("field|cell_id", names(self$rx_dt))
      non_nums <- self$rx_dt[, ..cols] %>%
        unique()
      cols <- grep("field", names(self$rx_dt))
      self$rx_dt <- self$rx_dt[, -..cols]
      self$rx_dt <- self$rx_dt[, lapply(.SD, mean), by = self$rx_dt$cell_id]
      names(self$rx_dt)[1] <- "cell_id"
      self$rx_dt$cell_id <- factor(self$rx_dt$cell_id)
      self$rx_dt <- merge(self$rx_dt, non_nums, by = "cell_id")
      private$.setBaseRate()
      self$rx_dt$cell_type <- self$mgmt_scen
      self$rx_dt$mgmt_scen <- self$mgmt_scen
      cols <- grep("cell_id", names(self$rx_dt))
      self$rx_dt <- self$rx_dt[, -..cols]
    },
    .makeOutLabels = function() {
      unique_fxn <- do.call(rbind, self$simClass$modClass$fxn)
      unique_fxn <- paste0(row.names(unique_fxn), "-", unique_fxn[, 1]) %>%
        paste(collapse = "--")
      opt <- self$simClass$opt
      unique_rx_years <- paste(self$rx_years, collapse = " & ")
      self$out_name <- paste0(self$out_path,
                         "/Outputs/Rx/",
                         self$unique_fieldname, "_",
                         self$mgmt_scen, "_Rx_",
                         unique_fxn, "_",
                         self$rx_for_year, "_",
                         opt, ".shp")
      self$var <- paste0("rx", ifelse(self$expvar == "aa_n", "N", "Seed"), "Rates")
      self$var_col_name <- "exprate"
      self$var_label <- paste0(ifelse(self$expvar == "aa_n", "N", "Seed"), " Rate (lbs/ac)")
      self$var_main_label <- paste0(self$unique_fieldname, " Rx ",
                                    ifelse(self$expvar=="aa_n", "N", "Seed"),
                                    " rates for ", self$rx_for_year,
                                    " for conditions like ", unique_rx_years)
      self$out_map_name <- paste0(self$out_path, "/Outputs/Rx/",
                                  self$unique_fieldname, "_", tolower(self$var),
                                  "_", self$mgmt_scen, "_RxMap_", unique_fxn, "_",
                                  self$rx_for_year, "_", opt, ".png")
      self$cell_out_map_name <- paste0(self$out_path, "/Outputs/Rx/",
                                       self$unique_fieldname, "_", self$mgmt_scen,
                                       "_rateTypeMap_", unique_fxn, "_",
                                       self$rx_for_year, "_", opt, ".png")
      self$size <- paste0(self$trt_length, " x ", self$boom_width * 2)
    },
    .setBaseRate = function() {
      if (self$mgmt_scen == "SSOPT") {
        self$rx_dt$base_rate <- self$rx_dt$EXP.rate.ssopt
      }
      if (self$mgmt_scen == "FFOPT") {
        self$rx_dt$base_rate <- self$rx_dt$EXP.rate.ffopt
      }
      if (self$mgmt_scen == "FS") {
        self$rx_dt$base_rate <- self$simClass$fs
      }
      if (self$mgmt_scen == "Min") {
        self$rx_dt$base_rate <- self$simClass$AAmin
      }
    },
    .binOptRates = function(rx_sdt) {
      stopifnot(!is.null(rx_sdt),
                !is.null(self$opt_rate_length))
      rates <- rx_sdt$exprate %>% na.omit()
      if (sd(rates) != 0) {
        breaks <- seq(min(rates), max(rates), (max(rates) - min(rates)) / self$opt_rate_length)
        tags <- rep(NA, self$opt_rate_length)
        for (i in 1:self$opt_rate_length) {
          tags[i] <- paste0(breaks[i], " - ", breaks[i + 1])
        }
        rx_sdt$bins <- cut(rx_sdt$exprate,
                               breaks = breaks,
                               include.lowest = TRUE,
                               right = FALSE,
                               labels = tags)
        rx_sdt$exprate <- ave(rx_sdt$exprate, rx_sdt$bins)
      } else {
        rx_sdt$bins <- rx_sdt$exprate
      }
      rx_sdt[grep("bins", names(rx_sdt))] <- NULL
      return(rx_sdt)
    },
    .expRateGen = function(exp_rate_length, base_rates, AAmin, AArateCutoff) {
      base_rates <- na.omit(base_rates)
      direction <- ifelse(min(base_rates) - AAmin > AArateCutoff - max(base_rates),
                          "to_min",
                          "to_max")
      if (direction == "to_min") {
        if (exp_rate_length == 1) {
          if (!AAmin %in% base_rates) {
            exp_rates <- AAmin
          } else {
            if (!AArateCutoff %in% base_rates) {
              exp_rates <- AArateCutoff
            } else {
              base_rates <- c(AAmin, unique(base_rates), AArateCutoff) %>% sort()
              rate_diffs <- rep(NA, length(base_rates) - 1)
              for (i in 1:length(rate_diffs)) {
                rate_diffs[i] <- base_rates[i + 1] - base_rates[i]
              }
              exp_rates <- base_rates[which(rate_diffs == max(rate_diffs))] +
                rate_diffs[which(rate_diffs == max(rate_diffs))] / 2
            }
          }
        } else {
          exp_rates <- rep(NA, exp_rate_length)
          if (!AAmin %in% base_rates) {
            exp_rates[1] <- AAmin
          }
          if (any(is.na(exp_rates))) {
            if (!AArateCutoff %in% base_rates) {
              exp_rates[exp_rate_length] <- AArateCutoff
            }
            if (any(is.na(exp_rates))) {
              base_rates <- c(AAmin, unique(base_rates), AArateCutoff) %>% sort()
              rate_diffs <- rep(NA, length(base_rates) - 1)
              for (i in 1:length(rate_diffs)) {
                rate_diffs[i] <- base_rates[i + 1] - base_rates[i]
              }
              miss_rates <- which(is.na(exp_rates))
              for (i in 1:length(miss_rates)) {
                exp_rates[miss_rates[i]] <-
                  base_rates[which(rate_diffs == max(rate_diffs))] +
                  rate_diffs[which(rate_diffs == max(rate_diffs))] / 2
                rate_diffs[which(rate_diffs == max(rate_diffs))] <- 0
              }
            }
          }
        }
      } else {
        if (exp_rate_length == 1) {
          if (!AArateCutoff %in% base_rates) {
            exp_rates <- AArateCutoff
          } else {
            if (!AAmin %in% base_rates) {
              exp_rates <- AAmin
            } else {
              base_rates <- c(AAmin, unique(base_rates), AArateCutoff) %>% sort()
              rate_diffs <- rep(NA, length(base_rates) - 1)
              for (i in 1:length(rate_diffs)) {
                rate_diffs[i] <- base_rates[i + 1] - base_rates[i]
              }
              exp_rates <- base_rates[which(rate_diffs == max(rate_diffs))] +
                rate_diffs[which(rate_diffs == max(rate_diffs))] / 2
            }
          }
        } else {
          exp_rates <- rep(NA, exp_rate_length)
          if (!AArateCutoff %in% base_rates) {
            exp_rates[exp_rate_length] <- AArateCutoff
          }
          if (any(is.na(exp_rates))) {
            if (!AAmin %in% base_rates) {
              exp_rates[1] <- AAmin
            }
            if (any(is.na(exp_rates))) {
              base_rates <- c(AAmin, unique(base_rates), AArateCutoff) %>% sort()
              rate_diffs <- rep(NA, length(base_rates) - 1)
              for (i in 1:length(rate_diffs)) {
                rate_diffs[i] <- base_rates[i + 1] - base_rates[i]
              }
              miss_rates <- which(is.na(exp_rates))
              for (i in 1:length(miss_rates)) {
                exp_rates[miss_rates[i]] <-
                  base_rates[which(rate_diffs == max(rate_diffs))] +
                  rate_diffs[which(rate_diffs == max(rate_diffs))] / 2
                rate_diffs[which(rate_diffs == max(rate_diffs))] <- 0
              }
            }
          }
        }
      }
      return(sort(exp_rates))
    },
    .applyCheckRates = function(rx_sdt, base_rate, fld_prop, opt_rate_length) {
      check_cell_length <- DescTools::RoundTo(nrow(rx_sdt) * fld_prop, 1, floor)
      rx_sdt$exprate_f <- factor(rx_sdt$exprate)
      strat_samps <- splitstackshape::stratified(
        rx_sdt,
        "exprate_f",
        floor(check_cell_length / opt_rate_length)
      )$cell_id
      check_cells <- row.names(rx_sdt[rx_sdt$cell_id %in% strat_samps, ])
      if (length(strat_samps) < check_cell_length) {
        miss_cells <- check_cell_length - length(strat_samps)
        samp_cells <- sample(
          row.names(rx_sdt[!row.names(rx_sdt) %in% check_cells, ]),
          miss_cells
        )
        check_cells <- c(check_cells, samp_cells)
      }
      check_rate_rep <- rep(base_rate, check_cell_length) %>% sample()
      rx_sdt[check_cells, "exprate"] <- check_rate_rep
      rx_sdt[check_cells, "cell_type"] <- "check"
      rx_sdt[grep("exprate_f", names(rx_sdt))] <- NULL
      return(rx_sdt)
    }
  )
)

