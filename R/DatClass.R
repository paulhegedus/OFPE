#' @title R6 Class for storing inputs and data for analysis and simulations
#'
#' @description R6 Class for storing user specified inputs and processing data
#' for the analysis/simulation and Rx building steps of the OFPE data cycle.
#' This object includes user selections such as the field and year of data
#' to export from an OFPE database and the type of data (grid or observed) for analysis
#' and simulation/prescription generation.
#'
#' Inputs can be supplied directly to this class during instantiation, however
#' this is NOT recommended except for advanced users. It is recommended that the
#' user supplies the database connection and uses the interactive selection
#' methods to select user inputs.
#'
#' This class stores inputs from the user and has the methods for for exporting
#' data from the database and processing the data for analysis, simulation, and
#' prescription building.
#' @seealso \code{\link{DBCon}} for database connection class,
#' \code{\link{ModClass}} for model fitting class that relies on data in DatClass,
#' \code{\link{SimClass}} for simulation class that rely on data in DatClass.
#' @export
DatClass <- R6::R6Class(
  "DatClass",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field farmername Name of the farmer that owns the selected field.
    farmername = NULL,
    #' @field fieldname Name of the field for analysis. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    fieldname = NULL,
    #' @field respvar Response variable(s) to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability. User must select at least 'Yield'.
    respvar = NULL,
    #' @field expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,
    #' @field sys_type Provide the type of system used in the experiment.
    #' This determines the price used for calculating net-return and for
    #' the net-return of the opposite type. Select from "Conventional" and
    #' "Organic". The net-returns will be calculated with the corresponding
    #' economic data based on this choice, and the 'NRopp' management
    #' scenario (see SimClass$executeSim) will be based on the opposite
    #' (e.g. if you are growing conventional wheat, the management outcome
    #' 'NRopp' shows the net-return calculated from organically grown wheat).
    #' In the example, organic prices are calculated from 0 N fertilizer rates,
    #' however with seeding rates it is purely the difference in the price received
    #' used to calculate net-return.
    sys_type = NULL,
    #' @field yldyears The year(s) of interest for the yield response
    #' variables in the selected field. This must be a named list with the
    #' specified field names.
    yldyears = NULL,
    #' @field proyears The year(s) of interest for the protein response
    #' variables in the selected field. This must be a named list with the
    #' specified field names.
    proyears = NULL,
    #' @field mod_grid Select whether to use gridded or observed data
    #' locations for the analysis step. See the 'AggInputs' class for more
    #' information on the 'GRID' option. The user must have aggregated data
    #' with the specified GRID option prior to this step. (i.e. you will not
    #' have access to data aggregated with the 'Grid' option if you have not
    #' executed the process of aggregation with the 'Grid' option selected. The
    #' same principle applies for the 'Observed' option. It is recommended that
    #' the analysis is performed with 'Observed' data, and for the simulation to
    #' be performed with 'Grid' data.
    mod_grid = NULL,
    #' @field sim_grid Select whether to use gridded or observed data
    #' locations for the simulation and subsequent prescription building step.
    #' See the 'AggInputs' class for more information on the 'GRID' option.
    #' The user must have aggregated data with the specified GRID option prior
    #' to this step. (i.e. you will not have access to data aggregated with the
    #' 'Grid' option if you have not executed the process of aggregation with
    #' the 'Grid' option selected. The same principle applies for the 'Observed'
    #' option. It is recommended that the analysis is performed with 'Observed'
    #' data, and for the simulation to be performed with 'Grid' data.
    sim_grid = NULL,
    #' @field dat_used Option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection.
    dat_used = NULL,
    #' @field center TRUE/FALSE. Option for whether to center explanatory data around
    #' each explanatory variables mean or to use the raw observed explanatory varaible
    #' data. Centering is recommended as it puts variables on similar scales and makes
    #' the model fitting process less error prone.
    center = NULL,
    #' @field split_pct Select the percentage of data to use for the training dataset
    #' in the analysis step. The training dataset is used to fit the model to each
    #' of the crop responses. The difference will be split into a validation dataset
    #' that is used to evaluate the model performance on data it has not 'seen' before.
    split_pct = NULL,
    #' @field clean_rate Select the maximum rate that could be realistically be applied
    #' by the application equipment (sprayer or seeder). This is used for a rudimentary
    #' cleaning of the data that removes observations with as-applied rates above this
    #' user supplied threshold. Rates above this threshold should be able to be classified
    #' as machine measurement errors. For example, based on knowledge of the prescription/
    #' experiment applied and taking into account double applications on turns, a rate
    #' for as-applied nitrogen might be something like 300 - 400 lbs N/acre.
    clean_rate = NULL,

    #' @field mod_dat Based off of the user selections such as 'mod_grid', this is a named
    #' list for each response variable ('yld' and/or 'pro'). The data in each of
    #' these lists are processed and then split into training and validation datasets. This
    #' data is used for the model fitting and evaluations steps.
    mod_dat = NULL,
    #' @field sim_dat Based off of the user selections such as 'sim_grid', this is a named
    #' list for each year specified in the SimClass 'sim_years' field. The data in each of
    #' these lists are processed and used in the Monte Carlo simulation.
    sim_dat = NULL,
    #' @field mod_num_means Named vector of the means for each numerical covariate, including
    #' the experimental variable. This is used for converting centered data back to the
    #' original form. The centering process does not center four numerical variables; the
    #' x and y coordinates, the response variable (yld/pro), and the experimental variable.
    #' This is for the data specified from the analysis data inputs (grid specific).
    mod_num_means = NULL,
    #' @field sim_num_means Named vector of the means for each numerical covariate, including
    #' the experimental variable. This is used for converting centered data back to the
    #' original form. The centering process does not center three numerical variables; the
    #' x and y coordinates, the response variable (yld/pro) and the experimental variable.
    #' This is for the data specified from the analysis data inputs (grid specific).
    sim_num_means = NULL,
    #' @field opp_sys_type Opposite of the user selected system type ('sys_type'). This is
    #' used to select the correct price received to calculate 'NRopp' in the Monte Carlo
    #' simulation.
    opp_sys_type = NULL,
    #' @field fieldname_codes Data.frame with a column for the names of the fields selected
    #' by the user and a corresponding code. This is used in the simulation data when being
    #' passed to C++ functions as purely numeric matrices.
    fieldname_codes = NULL,

    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param farmername Name of the farmer that owns the selected field.
    #' @param fieldname Name of the field to for analysis. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    #' @param respvar Response variable(s) to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability. User must select at least 'Yield'.
    #' @param expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param sys_type Provide the type of system used in the experiment.
    #' This determines the price used for calculating net-return and for
    #' the net-return of the opposite type. Select from "Conventional" and
    #' "Organic". The net-returns will be calculated with the corresponding
    #' economic data based on this choice, and the 'NRopp' management
    #' scenario (see SimClass$executeSim) will be based on the opposite
    #' (e.g. if you are growing conventional wheat, the management outcome
    #' 'NRopp' shows the net-return calculated from organically grown wheat).
    #' In the example, organic prices are calculated from 0 N fertilizer rates,
    #' however with seeding rates it is purely the difference in the price received
    #' used to calculate net-return.
    #' @param yldyears The year(s) of interest for the yield response
    #' variables in the selected field. This must be a named list with the
    #' specified field names.
    #' @param proyears The year(s) of interest for the protein response
    #' variables in the selected field. This must be a named list with the
    #' specified field names.
    #' @param mod_grid Select whether to use gridded or observed data
    #' locations for the analysis step. See the 'AggInputs' class for more
    #' information on the 'GRID' option. The user must have aggregated data
    #' with the specified GRID option prior to this step. (i.e. you will not
    #' have access to data aggregated with the 'Grid' option if you have not
    #' executed the process of aggregation with the 'Grid' option selected. The
    #' same principle applies for the 'Observed' option. It is recommended that
    #' the analysis is performed with 'Observed' data, and for the simulation to
    #' be performed with 'Grid' data.
    #' @param dat_used Option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection.
    #' @param center TRUE/FALSE. Option for whether to center explanatory data around
    #' each explanatory variables mean or to use the raw observed explanatory varaible
    #' data. Centering is recommended as it puts variables on similar scales and makes
    #' the model fitting process less error prone.
    #' @param split_pct Select the percentage of data to use for the training dataset
    #' in the analysis step. The training dataset is used to fit the model to each
    #' of the crop responses. The difference will be split into a validation dataset
    #' that is used to evaluate the model performance on data it has not 'seen' before.
    #' @param clean_rate Select the maximum rate that could be realistically be applied
    #' by the application equipment (sprayer or seeder). This is used for a rudimentary
    #' cleaning of the data that removes observations with as-applied rates above this
    #' user supplied threshold. Rates above this threshold should be able to be classified
    #' as machine measurement errors. For example, based on knowledge of the prescription/
    #' experiment applied and taking into account double applications on turns, a rate
    #' for as-applied nitrogen might be something like 300 - 400 lbs N/acre.
    #' @return A new 'AggInputs' object.
    initialize = function(dbCon,
                          farmername = NULL,
                          fieldname = NULL,
                          respvar = NULL,
                          expvar = NULL,
                          sys_type = NULL,
                          yldyears = NULL,
                          proyears = NULL,
                          mod_grid = NULL,
                          dat_used = NULL,
                          center = NULL,
                          split_pct = NULL,
                          clean_rate = NULL) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon
      if (!is.null(farmername)) {
        stopifnot(is.character(farmername))
        self$farmername <- farmername
      }
      if (!is.null(fieldname)) {
        stopifnot(is.character(fieldname))
        self$fieldname <- fieldname
      }
      if (!is.null(respvar)) {
        stopifnot(is.character(respvar),
                  any(grepl("Yield|Protein", respvar)))
        self$respvar <- ifelse(respvar == "Yield", "yld", "pro")
        stopifnot(any(grepl("yld", self$respvar)))
      }
      if (!is.null(expvar)) {
        stopifnot(is.character(expvar),
                  any(grepl("As-Applied Nitrogen|As-Applied Seed Rate", expvar)))
        self$expvar <- ifelse(expvar == "As-Applied Nitrogen", "aa_n", "aa_sr")
      }
      if (!is.null(sys_type)) {
        stopifnot(is.character(sys_type),
                  any(grepl("Conventional|Organic", sys_type)))
        self$sys_type <- ifelse(sys_type == "Conventional", "conv", "org")
        self$opp_sys_type <- ifelse(self$sys_type == "conv", "org", "conv")
      }
      if (!is.null(yldyears)) {
        stopifnot(is.list(yldyears),
                  any(grepl(paste(self$fieldname, collapse = "|"),
                            names(yldyears))),
                  length(yldyears) == length(fieldname))
        self$yldyears <- yldyears
      }
      if (!is.null(proyears)) {
        stopifnot(is.list(proyears),
                  any(grepl(paste(self$fieldname, collapse = "|"),
                            names(proyears))),
                  length(proyears) == length(fieldname))
        self$proyears <- proyears
      }
      if (!is.null(mod_grid)) {
        stopifnot(is.character(mod_grid),
                  grepl("Grid|Observed", mod_grid))
        self$mod_grid <- ifelse(mod_grid == "Grid", "grid", "obs")
      }
      if (!is.null(dat_used)) {
        stopifnot(is.character(dat_used))
        self$dat_used <- ifelse(dat_used == "Decision Point",
                                "decision_point",
                                "full_year")
      }
      if (!is.null(center)) {
        stopifnot(is.logical(center))
        self$center <- center
      }
      if (!is.null(self$yldyears) & !is.null(self$proyears)) {
        private$years <- list(yldyears=self$yldyears,
                           proyears=self$proyears)
      } else {
        if (!is.null(self$yldyears)) {
          private$years <- list(yldyears=self$yldyears)
        }
        if (!is.null(self$proyears)) {
          private$years <- list(proyears=self$proyears)
        }
      }
      if (!is.null(split_pct)) {
        stopifnot(is.numeric(split_pct))
        self$split_pct <- split_pct
      }
      if (!is.null(clean_rate)) {
        stopifnot(is.numeric(clean_rate))
        self$clean_rate <- clean_rate
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the data used in the
    #' analysis, simulation, and subsequent prescription generation steps. The
    #' description below describes the process of interactively selecting the
    #' necessary parameters needed for the automated analysis, simulation, and
    #' prescription building.
    #'
    #' The user first selects a farmer for which they want to analyze a field
    #' from, which is used to compile a list of available fields ready for
    #' analysis, indicated by its presence in the farmername_a schema of the
    #' OFPE database.
    #'
    #' The user then selects the response variables to optimize on and the
    #' experimental variable to optimize. The user must know what data is
    #' available for the specific field (i.e. if the user select 'Protein' they
    #' must have aggregated protein data for the specified field, or if the
    #' user selects 'As-Applied Seed Rate' seed rates must have been the
    #' experimental variable of interest when aggregating data).
    #'
    #' The user then selects the location of aggregated data to use for both
    #' the analysis and simulation/prescription building steps. The user also
    #' needs to select the length of the year for which 'current' year data
    #' was aggregated for (March 30th decision point or the full year).
    #'
    #' The user also has the choice of which vegetation index data to use as
    #' covariates, as well as the preferred source for precipitation and
    #' growing degree day data. Finally, the user has the option of whether
    #' to center covariate data or to use the raw observed data for analysis
    #' and simulation and the percent of data to use in the training data for
    #' model fitting. The rest of the data is withheld for validation.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A 'DatClass' object with complete user selections.
    selectInputs = function() {
      private$.selectFarmer(self$dbCon$db)
      private$.selectRespvar()
      private$.selectField(self$dbCon$db)
      private$.selectYears(self$dbCon$db)
      private$.selectExpvar()
      private$.selectSystemType()
      private$.selectAggLocs()
      private$.selectAggLOY()
      private$.selectCenter()
      private$.selectDatSplitPct()
      private$.selectCleanRate()
    },
    #' @description
    #' This function calls the private methods for data gathering and
    #' processing. The data gather step takes the user selected inputs
    #' for the field, the response variables, and the data types ('mod_grid')
    #' and exports the appropriate data into a a list, called 'mod_dat' with
    #' lists, named  for each response variable ('yld' and/or
    #' 'pro') with each data type data from all fields selected.
    #'
    #' The processing step goes through each data frame contained in the
    #' nested 'mod_dat' list and trims the data based on
    #' the user selections for the vegetation index and precipitation and
    #' growing degree day sources. If the user selected to center the
    #' covariate data, the values of each variable will be subtracted from
    #' the mean of that variable. In this case, a named
    #' vector of each variable and the mean will be created for reverting
    #' back to observed values.
    #'
    #' After this step, the data in 'mod_dat' is split into training and
    #' validation sets based on the percentage of data the user selected
    #' to include in the training dataset.
    #' @param None No arguments needed because passed in during class
    #' @return A named list with training and validation data, called
    #' 'mod_dat', for each response variable ('yld' and/or 'pro').
    setupDat = function() {
      self$mod_dat <- private$.fetchDat(self$mod_grid, self$respvar) %>%
        lapply(private$.processDat) %>%
        invisible()
      self$mod_num_means <- as.list(self$respvar) %>%
        `names<-`(self$respvar)
      self$mod_num_means <- lapply(self$mod_dat, private$.findMeans)
      self$mod_dat <- mapply(private$.centerDat,
                             self$mod_dat,
                             self$mod_num_means,
                             SIMPLIFY = FALSE)
      private$.splitDat()
    },
    #' @description
    #' This function calls the private methods for data gathering and
    #' processing. The gathering process takes the vector of simulation
    #' years and gathers the appropriate 'sat' data from the OFPE database
    #' and then processes the data using the same parameters as for the
    #' data used in the model fitting process.
    #' @param sim_years Vector of years available in the database
    #' to gather to simulate management outcomes in.
    #' @return A data.table with the user specified data for the simulation.
    getSimDat = function(sim_years) {
      self$sim_dat <- as.list(sim_years) %>%
        `names<-`(sim_years)
      self$sim_dat <- lapply(self$sim_dat,
                             private$.gatherSatDat,
                             'sat',
                             self$fieldname,
                             'grid')
      self$sim_dat <- lapply(self$sim_dat,
                             private$.processSatDat) %>%
        lapply(data.table::as.data.table) %>%
        invisible()
      self$sim_num_means <- as.list(self$respvar) %>%
        `names<-`(self$respvar)
      self$sim_num_means <- lapply(self$sim_dat, private$.findMeans)
      self$sim_dat <- mapply(private$.centerDat,
                             self$sim_dat,
                             self$sim_num_means,
                             SIMPLIFY = FALSE) %>%
        lapply(private$.makeAllSimColsNumeric)
    }
    # TODO
    # more fields
    # like year dat (if selected)
  ),
  private = list(
    years = NULL,

    .selectFarmer = function(db) {
      self$farmername <- as.character(select.list(
        unique(DBI::dbGetQuery(db, "SELECT farmer FROM all_farms.farmers")$farmer),
        multiple = FALSE,
        title = "Select farm to analyze a field in."))
    },
    .selectRespvar = function() {
      respVar <- as.character(select.list(
        c("Yield", "Protein"),
        multiple = TRUE,
        title = "Select response variable(s) to optimize on. In some cases protein data is not available. However, either yield or protein must be selected."
      ))
      self$respvar <- ifelse(respVar == "Yield", "yld", "pro")
      stopifnot(any(grepl("yld", self$respvar)))
    },
    .selectExpvar = function() {
      expVar <- as.character(select.list(
        c("As-Applied Nitrogen", "As-Applied Seed Rate"),
        title = "Select experimental variable."))
      self$expvar <- ifelse(expVar == "As-Applied Nitrogen", "aa_n", "aa_sr")
    },
    .selectSystemType = function() {
      sys_type <- as.character(select.list(
        c("Conventional", "Organic"),
        title = "Select your system type."))
      self$sys_type <- ifelse(sys_type == "Conventional", "conv", "org")
      self$opp_sys_type <- ifelse(self$sys_type == "conv", "org", "conv")
    },
    .selectField = function(db) {
      flds <- rep(list(NA), length(self$respvar))
      for (i in 1:length(flds)) {
        tabExist <- DBI::dbGetQuery(
          db,
          paste0("SELECT EXISTS (
                 SELECT 1
                 FROM information_schema.tables
                 WHERE table_schema = '", self$farmername, "_a'
                 AND table_name = '", self$respvar[i], "')"
          )) %>% 
          as.numeric() %>% 
          as.logical()
        if (tabExist) {
          flds[[i]] <- DBI::dbGetQuery(
              db,
              paste0("SELECT DISTINCT field FROM ",
                     self$farmername, "_a.", self$respvar[i], ";")
          )$field
        } else {
          self$respvar[i] <- NA
        }
      }
      flds <- unlist(flds) %>%
        na.omit()
      self$respvar <- self$respvar[!is.na(self$respvar)]

      self$fieldname <-
        as.character(select.list(
            unique(flds),
            multiple = TRUE,
            title = "Select field(s) to analyze data for. Multiple can be selected if desired (i.e. sec1east and sec1west)."
          )
        )
    },
    .selectYears = function(db) {
      for (i in 1:length(self$respvar)) {
        years <-  rep(list(NA), length(self$fieldname)) %>%
          `names<-`(self$fieldname)
        for (j in 1:length(years)) {
          years[[j]] <- as.character(select.list(unique(DBI::dbGetQuery(
            db,
            paste0("SELECT DISTINCT year
                   FROM ", self$farmername, "_a.", self$respvar[i], " ", self$respvar[i], "
                   WHERE ", self$respvar[i], ".field = '", self$fieldname[j], "'"))$year),
            multiple = TRUE,
            title = paste0("Select years from ",
                           self$fieldname[j],
                           " to get ", ifelse(self$respvar[i] == "yld", "Yield", "Protein"),
                           " data for to include in analysis.")))
        }
        if (self$respvar[i] == "yld") {
          self$yldyears <- years
        } else {
          self$proyears <- years
        }
      }
      if (!is.null(self$yldyears) & !is.null(self$proyears)) {
        private$years <- list(yldyears=self$yldyears,
                           proyears=self$proyears)
      } else {
        if (!is.null(self$yldyears)) {
          private$years <- list(yldyears=self$yldyears)
        }
        if (!is.null(self$proyears)) {
          private$years <- list(proyears=self$proyears)
        }
      }
    },
    .selectAggLocs = function() {
      gridOrObs <- as.character(select.list(
          c("Grid", "Observed"),
          title = paste0("Select whether to use gridded ('Grid') or observed ('Observed') data locations for the analysis step.")
      ))
      self$mod_grid <- ifelse(gridOrObs == "Grid", "grid", "obs")
    },
    .selectAggLOY = function() {
      data_used <- as.character(select.list(
          c("Decision Point", "Full Year"),
          title = paste0("Select the data constraint for determining the time span for which to gather data.")
      ))
      self$dat_used <- ifelse(data_used == "Decision Point",
                              "decision_point",
                              "full_year")
    },
    .selectCenter = function() {
      self$center <- as.logical(select.list(
        c(TRUE, FALSE),
        title = paste0("Select whether to center explanatory data (TRUE) or to use the measured explanatory data (FALSE).")
      ))
    },
    .selectDatSplitPct = function() {
      self$split_pct <- as.numeric(readline(
        "Provide the percentage of data to use as a training dataset for model fitting. The rest of the data will be witheld for model validation: "
      ))
    },
    .selectCleanRate = function() {
      self$clean_rate <- as.numeric(readline(
        "Provide the threshold for as-applied rates above which are obvious machine measurment errors (e.g. 350 lbs/N/acre): "
      ))
    },

    .fetchDat = function(GRID, respvar) {
      dat <- as.list(respvar) %>%
        `names<-`(respvar)
      dat <- mapply(
        private$.importDBdat,
        dat,
        private$years,
        MoreArgs = list(GRID = GRID),
        SIMPLIFY = FALSE
      )
      return(dat)
    },
    .importDBdat = function(respvar, years, GRID) {
      dat <- rep(list(NA),
                  length(years)) %>%
        `names<-`(names(years))
      #fieldname <- names(dat)
      dat <- mapply(private$.gatherDBdat,
                    years,
                    respvar,
                    self$fieldname,
                    MoreArgs = list(GRID = GRID)) %>%
        data.table::rbindlist()
      return(dat)
    },
    .gatherDBdat = function(years, respvar, fieldname, GRID) {
      dat <- as.list(years) %>%
        `names<-`(years)
      dat <- lapply(dat,
                    private$.getDBdat,
                    respvar,
                    fieldname,
                    GRID)
      return(dat)
    },
    .getDBdat = function(year, respvar, fieldname, GRID) {
      OFPE::removeTempFarmerTables(self$dbCon$db, self$farmername)
      tt <- invisible(
        DBI::dbSendQuery(
          self$dbCon$db,
          paste0(
            "CREATE TABLE ", self$farmername,"_a.temp AS (SELECT *
            FROM ", self$farmername, "_a.", respvar, " ", respvar,"
            WHERE field = '", fieldname, "'
            AND year = '", year, "'
            AND grid = '", GRID, "'
            AND datused = '", self$dat_used,"');"
          )
        )
      )
      DBI::dbClearResult(tt)
      tt <- invisible(
        DBI::dbSendQuery(
          self$dbCon$db,
          paste0(
            "ALTER TABLE ",
            self$farmername, "_a.temp
            DROP COLUMN geometry;"
          )
        )
      )
      DBI::dbClearResult(tt)
      db_dat <- invisible(
        DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT * FROM ", self$farmername, "_a.temp;")
        )
      )
      tt <- invisible(
        DBI::dbSendQuery(
          self$dbCon$db,
          paste0(
            "DROP TABLE ", self$farmername, "_a.temp;"
          )
        )
      )
      DBI::dbClearResult(tt)

      ## TEMP - REMOVE!
      # set.seed(342134)
      # if (respvar == "yld") {
      #   db_dat <- db_dat[runif(nrow(db_dat) * 0.05, 1, nrow(db_dat)), ]
      # } else {
      #   db_dat <- db_dat[runif(nrow(db_dat) * 0.1, 1, nrow(db_dat)), ]
      # }
      ## TEMP - REMOVE!

      return(db_dat)
    },
    .processDat = function(dat) {
      dat <- private$.trimCols(
        dat, c("grid", "size", "datused", "farmer", "prev_year")) %>%
        private$.makeFactors() %>%
        private$.makeOLMmeans() %>%
        private$.cleanDat()
      return(dat)
    },
    .processSatDat = function(dat) {
      dat <- private$.trimCols(
        dat, c("grid", "size", "datused", "farmer", "prev_year")) %>%
        private$.makeFactors() %>%
        private$.makeOLMmeans() %>%
        private$.cleanDat()
      return(dat)
    },
    .trimCols = function(dat, trim_cols) {
      return(dat[, !(names(dat) %in% trim_cols), with = FALSE])
    },
    .makeFactors = function(dat) {
      dat$field <- factor(dat$field)
      dat$year <- factor(dat$year)
      dat$musym <- factor(dat$musym)
      dat$grtgroup <- factor(dat$grtgroup)
      dat$texture0cm <- factor(dat$texture0cm)
      dat$texture10cm <- factor(dat$texture10cm)
      dat$texture30cm <- factor(dat$texture30cm)
      dat$texture60cm <- factor(dat$texture60cm)
      dat$texture100cm <- factor(dat$texture100cm)
      dat$texture200cm <- factor(dat$texture200cm)

      return(dat)
    },
    ## take the mean of each OLM data by depth
    .makeOLMmeans = function(dat) {
      cols <- grep("bulkdensity", names(dat))
      dat$bulkdensity <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("claycontent", names(dat))
      dat$claycontent <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("sandcontent", names(dat))
      dat$sandcontent <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("phw", names(dat))
      dat$phw <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("watercontent", names(dat))
      dat$watercontent <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("carboncontent", names(dat))
      dat$carboncontent <- rowMeans(dat[, cols, with = FALSE], na.rm = TRUE)
      
      cols <- grep("texture", names(dat))
      dat$texture <- apply(dat[, cols, with = FALSE], 1, private$.Mode)
      
      return(dat)
    },
    .Mode = function(x, na.rm = FALSE) {
      if(na.rm){
        x = x[!is.na(x)]
      }
      ux <- unique(x)
      return(ux[which.max(tabulate(match(x, ux)))])
    },
    .cleanDat = function(dat) {
      if (any(grepl("aa_n|aa_sr|yld|pro", names(dat)))) {
        col <- names(dat)[grep("^aa_n$|^aa_sr$", names(dat))]
        for(i in 1:length(col)){
          rows <- which(dat[, names(dat) %in% col[i], with = FALSE] < self$clean_rate)
          dat <- dat[rows, ]
          rows <- which(dat[, names(dat) %in% col[i], with = FALSE] >= 0)
          dat <- dat[rows, ]
          dat <- na.omit(dat, col)
        }
        col <- names(dat)[grep("^yld$|^pro$", names(dat))]
        for(i in 1:length(col)){
          rows <- which(dat[, names(dat) %in% col[i], with = FALSE] > 0)
          dat <- dat[rows, ]
          dat <- na.omit(dat, col)
        }
      }
      return(dat)
    },
    .centerDat = function(dat, num_means) {
      dat_list <- split(dat, dat$year)
      dat_list <- mapply(private$.centerFun,
                         dat_list,
                         num_means,
                         SIMPLIFY = FALSE)
      dat <- data.table::rbindlist(dat_list)
      return(dat)
    },
    .centerFun = function(subdat, num_means) {
      num_names <- names(num_means)
      if (self$center) {
        no_cent_cols <- c("^x$", "^y$", "cell_id", "field", 
                          "size", "grid", "datused", "farmer",
                          "year", "prev_year", "geometry",
                          "grtgroup", "texture0cm", "texture10cm",
                          "texture30cm", "texture60cm",
                          "texture100cm", "texture200cm", "musym",
                          "texture")
        no_cent_col_ids <- grep(paste(no_cent_cols, collapse = "|"), names(subdat))
        dfc <- subdat %>% dplyr::select(-no_cent_col_ids) %>% 
          as.data.frame()
        for (i in 1:ncol(dfc)) {
          if (is.numeric(dfc[, i])) {
            dfc[, i] <- dfc[, i] - mean(dfc[, i], na.rm = T)
          }
        }
        names(dfc) <- paste0(names(dfc), "_cent")
        subdat <- cbind(subdat, dfc)
        rm(dfc) # save space in mem
      }
      return(subdat)
    },
    .findMeans = function(dat) {
      num_names <- names(dat)[sapply(dat, is.numeric)]
      num_names <- num_names[!grepl(paste0("^x$|^y$|^yld$|^pro$"), num_names)]
      num_means <- rep(as.list(NA), length(unique(dat$year))) %>%
        `names<-`(unique(dat$year))
      means <- by(dat[, num_names, with = FALSE], dat$year, sapply, mean, na.rm = TRUE)
      for (i in 1:length(unique(dat$year))) {
        means[[i]][grep(paste0("^", self$expvar, "$"), names(means[[i]]))] <- 0 # don't center exp var
        if (self$center) {
          num_means[[i]] <- means[[i]]
        } else {
          num_means[[i]] <- rep(0, length(num_names)) %>%
            `names<-`(num_names)
        }
      }
      return(num_means)
    },
    .splitDat = function() {
      set.seed(6201994)
      self$mod_dat <- lapply(self$mod_dat, private$.splitDatTrnVal) %>%
        `names<-`(names(self$mod_dat))
    },
    .splitDatTrnVal = function(dat) {
      if (all(is.na(dat$musym))) {
        sub_list <- split(dat, list(dat$field, dat$year))
      } else {
        sub_list <- split(dat, list(dat$field, dat$year, dat$musym))
      }
      split_dat <- lapply(sub_list, private$.dualSplit, self$split_pct)
      out_trn <- sapply(split_dat,'[',"trn") %>%
        data.table::rbindlist()
      out_val <- sapply(split_dat,'[',"val") %>%
        data.table::rbindlist()
      out_dat <- list(trn = out_trn, val = out_val)
      return(out_dat)
    },
    .subDat = function(STRING, dat) {
      YEAR <- substr(STRING,
                     stringr::str_locate(STRING, "20")[1],
                     stringr::str_locate(STRING, "20")[1] + 3)
      FIELD <- gsub(paste0(".", YEAR), "", STRING)
      dat <- subset(dat, dat$field == FIELD & dat$year == YEAR)
      return(dat)
    },
    .dualSplit = function(dat, trnprop) {
      trnprop <- trnprop * 0.01
      valprop <- 1 - trnprop
      sum_sam <- round(nrow(dat) * trnprop) +
        round(nrow(dat) * valprop)
      dif <- nrow(dat) - sum_sam
      if (dif == 0) {
        subL <- split(
          dat,
          sample(c(rep("trn", round(nrow(dat) * trnprop)),
                   rep("val", round(nrow(dat) * valprop))))
        )
      } else {
        subL <- split(
          dat,
          sample(c(rep("trn", round(nrow(dat) * trnprop) + dif),
                   rep("val", round(nrow(dat) * valprop))))
        )
      }
      return(subL)
    },
    .gatherSatDat = function(year, respvar, fieldname, GRID) {
      dat_list <- as.list(fieldname) %>%
        `names<-`(fieldname)
      for (i in 1:length(dat_list)) {
        dat_list[[i]] <- private$.getDBdat(year, respvar, fieldname[i], GRID)
      }
      dat <- data.table::rbindlist(dat_list)
      return(dat)
    },
    .makeAllSimColsNumeric = function(dat) {
      stopifnot(!is.null(dat$field),
                !is.null(dat$cell_id))
      cell_id_split <- stringr::str_split(dat$cell_id, "_", simplify = FALSE) %>%
        lapply(as.numeric)
      cell_id_split <- do.call(rbind, cell_id_split) %>%
        data.table::as.data.table() %>%
        `names<-`(c("row", "col"))
      dat <- cbind(cell_id_split, dat)
      dat$cell_id <- NULL
      self$fieldname_codes <-
        data.frame(field = unique(dat$field),
                   field_code = seq(1, length(unique(dat$field))))
      dat$field <- match(dat$field, self$fieldname_codes$field)
      return(dat)
    }
  )
)







