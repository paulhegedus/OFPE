#' @title R6 Class for storing inputs to the 'DatObject' class
#'
#' @description R6 class for for storing information needed for the
#' 'DatObject' class which is used for the analysis/simulation and Rx
#' building steps of the OFPE data cycle. This object inlcudes user
#' selections such as the field and year of data to export from an
#' OFPE database and the type of data (grid or observed) for analysis
#' and simulation/prescription generation.
#'
#' Inputs can be supplied directly to this class during instantiation, however
#' this is NOT recommended except for advanced users. It is recommended that the
#' user supplies the database connection and uses the interactive selection
#' methods to select user inputs.
#'
#' This class is passed to the 'DatObject' class that executes the methods
#' for exporting data from the database and processing the data for analysis,
#' simulation, and prescription building.
#' @export
DatInputs <- R6::R6Class(
  "DatInputs",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field farmername Name of the farmer that owns the selected field.
    farmername = NULL,
    #' @field fieldname Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    fieldname = NULL,
    #' @field respvar Response variable(s) to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    respvar = NULL,
    #' @field expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,
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
    #' data, and for the simulation tobe performed with 'Grid' data.
    sim_grid = NULL,
    #' @field dat_used Option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection.
    dat_used = NULL,
    #' @field veg_index Option for the vegetation index data to use for analysis,
    #' simulation, and prescription building steps. Select from 'NDVI', 'NDRE',
    #' and 'CIRE'. These are the three vegatation indices downloaded from
    #' Google Earth Engine and used to enrich yield and protein datasets in the
    #' aggregation step.
    veg_index = NULL,
    #' @field prec_source Option for the satellite source of precipitation data
    #' to use for analysis, simulation, and prescription building steps. Select
    #' from 'Daymet' or 'Gridmet'. These are the sources of precipitation data
    #' downloaded from Google Earth Engine and used to enrich yield and protein
    #' datasets in the aggregation step. This selection is for the users preference,
    #' but note that the other source will be used if the selected source is
    #' unavailable.
    prec_source = NULL,
    #' @field gdd_source Option for the satellite source of growing degree day (GDD)
    #' data to use for analysis, simulation, and prescription building steps. Select
    #' from 'Daymet' or 'Gridmet'. These are the sources of GDD data downloaded from
    #' Google Earth Engine and used to enrich yield and protein datasets in the
    #' aggregation step. This selection is for the users preference, but note that
    #' the other source will be used if the selected source is unavailable.
    gdd_source = NULL,
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
    #' by the applicating machine (sprayer or seeder). This is used for a rudimentary
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
    #' list for each response variable ('yld' and/or 'pro'). The data in each of
    #' these lists are processed and used in the Monte Carlo simulation.
    sim_dat = NULL,
    #' @field num_means Named vector of the means for each numerical covariate, including
    #' the experimental variable. This is used for converting centered data back to the
    #' original form. The centering process does not center three numerical variables; the
    #' x and y coordinates, and the response variable (yld/pro).
    num_means = NULL,

    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param farmername Name of the farmer that owns the selected field.
    #' @param fieldname Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    #' @param respvar Response variable to aggregate data for, select/input
    #' 'Yield' or 'Protein'. Multiple options allowed.
    #' @param expvar Experimental variable to aggregate data for, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
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
    #' @param sim_grid Select whether to use gridded or observed data
    #' locations for the simulation and subsequent prescription building step.
    #' See the 'AggInputs' class for more information on the 'GRID' option.
    #' The user must have aggregated data with the specified GRID option prior
    #' to this step. (i.e. you will not have access to data aggregated with the
    #' 'Grid' option if you have not executed the process of aggregation with
    #' the 'Grid' option selected. The same principle applies for the 'Observed'
    #' option. It is recommended that the analysis is performed with 'Observed'
    #' data, and for the simulation tobe performed with 'Grid' data.
    #' @param dat_used Option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection.
    #' @param veg_index Option for the vegetation index data to use for analysis,
    #' simulation, and prescription building steps. Select from 'NDVI', 'NDRE',
    #' and 'CIRE'. These are the three vegatation indices downloaded from
    #' Google Earth Engine and used to enrich yield and protein datasets in the
    #' aggregation step.
    #' @param prec_source Option for the satellite source of precipitation data
    #' to use for analysis, simulation, and prescription building steps. Select
    #' from 'Daymet' or 'Gridmet'. These are the sources of precipitation data
    #' downloaded from Google Earth Engine and used to enrich yield and protein
    #' datasets in the aggregation step. This selection is for the users preference,
    #' but note that the other source will be used if the selected source is
    #' unavailable.
    #' @param gdd_source Option for the satellite source of growing degree day (GDD)
    #' data to use for analysis, simulation, and prescription building steps. Select
    #' from 'Daymet' or 'Gridmet'. These are the sources of GDD data downloaded from
    #' Google Earth Engine and used to enrich yield and protein datasets in the
    #' aggregation step. This selection is for the users preference, but note that
    #' the other source will be used if the selected source is unavailable.
    #' @param center TRUE/FALSE. Option for whether to center explanatory data around
    #' each explanatory variables mean or to use the raw observed explanatory varaible
    #' data. Centering is recommended as it puts variables on similar scales and makes
    #' the model fitting process less error prone.
    #' @param split_pct Select the percentage of data to use for the training dataset
    #' in the analysis step. The training dataset is used to fit the model to each
    #' of the crop responses. The difference will be split into a validation dataset
    #' that is used to evaluate the model performance on data it has not 'seen' before.
    #' @param clean_rate Select the maximum rate that could be realistically be applied
    #' by the applicating machine (sprayer or seeder). This is used for a rudimentary
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
                          yldyears = NULL,
                          proyears = NULL,
                          mod_grid = NULL,
                          sim_grid = NULL,
                          dat_used = NULL,
                          veg_index = NULL,
                          prec_source = NULL,
                          gdd_source = NULL,
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
      }
      if (!is.null(expvar)) {
        stopifnot(is.character(expvar),
                  any(grepl("As-Applied Nitrogen|As-Applied Seed Rate", expvar)))
        self$expvar <- ifelse(expvar == "As-Applied Nitrogen", "aa_n", "aa_sr")
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
      if (!is.null(sim_grid)) {
        stopifnot(is.character(sim_grid),
                  grepl("Grid|Observed", sim_grid))
        self$sim_grid <- ifelse(sim_grid == "Grid", "grid", "obs")
      }
      if (!is.null(dat_used)) {
        stopifnot(is.character(dat_used))
        self$dat_used <- ifelse(dat_used == "Decision Point",
                                "decision_point",
                                "full_year")
      }
      if (!is.null(veg_index)) {
        stopifnot(is.character(veg_index),
                  grepl("NDVI|NDRE|CIRE", veg_index))
        self$veg_index <- ifelse(veg_index == "NDVI", "ndvi",
                                ifelse(veg_index == "NDRE", "ndre", "cire"))
      }
      if (!is.null(prec_source)) {
        stopifnot(is.character(prec_source),
                  grepl("Daymet|Gridmet", prec_source))
        self$prec_source <- ifelse(prec_source == "Daymet", "daymet", "gridmet")
      }
      if (!is.null(gdd_source)) {
        stopifnot(is.character(gdd_source),
                  grepl("Daymet|Gridmet", gdd_source))
        self$gdd_source <- ifelse(gdd_source == "Daymet", "daymet", "gridmet")
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
    #' model fitting. The rest of the data is witheld for validation.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A completed 'DatInputs' object.
    selectInputs = function() {
      private$.selectFarmer(self$dbCon$db)
      private$.selectRespvar()
      private$.selectField(self$dbCon$db)
      private$.selectYears(self$dbCon$db)
      private$.selectExpvar()
      private$.selectAggLocs()
      private$.selectAggLOY()
      private$.selectVegIndex()
      private$.selectPrecSource()
      private$.selectGddSource()
      private$.selectCenter()
      private$.selectDatSplitPct()
      private$.selectCleanRate()
    },
    #' @description
    #' This function calls the private methods for data gathering and
    #' processing. The data gather step takes the user selected inputs
    #' for the field, the response variables, and the data types ('mod_grid'
    #' and 'sim_grid') and exports the appropriate data into a a list,
    #' called 'mod_dat' and 'sim_dat', with lists, named  for each response variable
    #' ('yld' and/or 'pro') with each data type data from all fields
    #' selected.
    #'
    #' The processing step goes through each data frame contained in the
    #' nested 'mod_dat' and 'sim_dat' lists and trims the data based on
    #' the user selections for the vegetation index and precipitation and
    #' growing degree day sources. If the user selected to center the
    #' experimental and covariate data, the values of each variable will
    #' be subtracted from the mean of that variable. In this case, a named
    #' vector of each variable and the mean will be created for reverting
    #' back to observed values.
    #'
    #' After this step, the data in 'mod_dat' is split into training and
    #' validation sets based on the percentage of data the user selected
    #' to include in the training dataset.
    #' @param None No arguments needed because passed in during class
    #' @return A named list with training and validation data, called
    #' 'mod_dat', for each response variable ('yld' and/or 'pro'),
    #' and a named list ('yld' and/or 'pro') for the simulation and prescription
    #' building process, called 'sim_dat'.
    setupDat = function() {
      self$mod_dat <- private$.fetchDat(self$mod_grid) %>%
        lapply(private$.processDat) %>%
        invisible()
      self$sim_dat <- private$.fetchDat(self$sim_grid) %>%
        lapply(private$.processDat) %>%
        invisible()
      private$.splitDat()
    }
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
    },
    .selectExpvar = function() {
      expVar <- as.character(select.list(
        c("As-Applied Nitrogen", "As-Applied Seed Rate"),
        title = "Select experimental variable."))
      self$expvar <- ifelse(expVar == "As-Applied Nitrogen", "aa_n", "aa_sr")
    },
    .selectField = function(db) {
      flds <- rep(list(NA), length(self$respvar))
      for (i in 1:length(flds)) {
        tabExist <- as.logical(DBI::dbGetQuery(
          db,
          paste0("SELECT EXISTS (
                 SELECT 1
                 FROM information_schema.tables
                 WHERE table_schema = '", self$farmername, "_a'
                 AND table_name = '", self$respvar[i], "')"
              )
            )
          )
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
      gridOrObs <- as.character(select.list(
        c("Grid", "Observed"),
        title = paste0("Select whether to use gridded ('Grid') or observed ('Observed') data locations for the simulation and/or Rx building step.")
      ))
      self$sim_grid <- ifelse(gridOrObs == "Grid", "grid", "obs")
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
    .selectVegIndex = function() {
      veg_index <- as.character(select.list(
        c("NDVI", "NDRE", "CIRE"),
        title = paste0("Select the vegetation index to use for analysis and simulation.")
      ))
      self$veg_index <- ifelse(veg_index == "NDVI", "ndvi",
                               ifelse(veg_index == "NDRE", "ndre", "cire"))
    },
    .selectPrecSource = function() {
      prec_source <- as.character(select.list(
        c("Daymet", "Gridmet"),
        title = paste0("Select the source of precipitation data to use for analysis and simulation.")
      ))
      self$prec_source <- ifelse(prec_source == "Daymet", "daymet", "gridmet")
    },
    .selectGddSource = function() {
      gdd_source <- as.character(select.list(
        c("Daymet", "Gridmet"),
        title = paste0("Select the source of growing degree day data to use for analysis and simulation.")
      ))
      self$gdd_source <- ifelse(gdd_source == "Daymet", "daymet", "gridmet")
    },
    .selectCenter = function() {
      self$center <- as.character(select.list(
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

    .fetchDat = function(GRID) {
      dat <- as.list(self$respvar) %>%
        `names<-`(self$respvar)
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
      invisible(
        DBI::dbSendQuery(
          self$dbCon$db,
          paste0(
            "CREATE TABLE ", self$farmername,"_a.temp AS (SELECT *
            FROM ", self$farmername, "_a.", respvar, " ", respvar,"
            WHERE field = '", fieldname, "'
            AND year = '", year, "'
            AND grid = '", GRID, "'
            AND datused = '", self$dat_used,"');

            ALTER TABLE ",
            self$farmername, "_a.temp
            DROP COLUMN geometry;"
          )
        )
      )
      db_dat <- invisible(
        DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT * FROM ", self$farmername, "_a.temp;")
        )
      )
      invisible(
        DBI::dbGetQuery(
          self$dbCon$db,
          paste0(
            "DROP TABLE ", self$farmername, "_a.temp;"
          )
        )
      )
      return(db_dat)
    },
    .processDat = function(dat) {
      dat <- private$.trimCols(
        dat, c("grid", "size", "datused", "farmer", "prev_year")) %>%
        private$.selectDat() %>%
        private$.makeFactors() %>%
        private$.cleanDat() %>%
        private$.centerDat()
      return(dat)
    },
    .trimCols = function(dat, trim_cols) {
      return(dat[,!(names(dat) %in% trim_cols)])
    },
    .selectDat = function(dat) {
      df <- matrix(NA, nrow = nrow(dat), ncol = 7) %>%
        as.data.frame()
      names(df) <- c("prec_cy","prec_py","gdd_cy","gdd_py",
                     "veg_cy","veg_py","veg_2py")
      prec_key <- ifelse(self$prec_source == "daymet", "d", "g")
      alt_prec_key <- ifelse(self$prec_source == "daymet", "g", "d")
      gdd_key <- ifelse(self$gdd_source == "daymet", "d", "g")
      alt_gdd_key <- ifelse(self$gdd_source == "daymet", "g", "d")

      df$prec_cy <- ifelse(!is.na(dat[, grep(paste0("prec_cy_", prec_key),
                                             names(dat))]),
                           dat[, grep(paste0("prec_cy_", prec_key),
                                      names(dat))],
                           dat[, grep(paste0("prec_cy_", alt_prec_key),
                                      names(dat))])
      df$prec_py <- ifelse(!is.na(dat[, grep(paste0("prec_py_", prec_key),
                                             names(dat))]),
                           dat[, grep(paste0("prec_py_", prec_key),
                                      names(dat))],
                           dat[, grep(paste0("prec_py_", alt_prec_key),
                                      names(dat))])
      df$gdd_cy <- ifelse(!is.na(dat[, grep(paste0("gdd_cy_", gdd_key),
                                            names(dat))]),
                          dat[, grep(paste0("gdd_cy_", gdd_key),
                                     names(dat))],
                          dat[, grep(paste0("gdd_cy_", alt_gdd_key),
                                     names(dat))])
      df$gdd_py <- ifelse(!is.na(dat[, grep(paste0("gdd_py_", gdd_key),
                                            names(dat))]),
                          dat[, grep(paste0("gdd_py_", gdd_key),
                                     names(dat))],
                          dat[, grep(paste0("gdd_py_", alt_gdd_key),
                                     names(dat))])
      df$veg_cy <- ifelse(!is.na(dat[, grep(paste0(self$veg_index, "_cy_s"),
                                            names(dat))]),
                          dat[, grep(paste0(self$veg_index, "_cy_s"),
                                     names(dat))],
                          dat[, grep(paste0(self$veg_index, "_cy_l"),
                                     names(dat))])
      df$veg_py <- ifelse(!is.na(dat[, grep(paste0(self$veg_index, "_py_s"),
                                            names(dat))]),
                          dat[, grep(paste0(self$veg_index, "_py_s"),
                                     names(dat))],
                          dat[, grep(paste0(self$veg_index, "_py_l"),
                                     names(dat))])
      df$veg_2py <- ifelse(!is.na(dat[, grep(paste0(self$veg_index, "_2py_s"),
                                             names(dat))]),
                           dat[, grep(paste0(self$veg_index, "_2py_s"),
                                      names(dat))],
                           dat[, grep(paste0(self$veg_index, "_2py_l"),
                                      names(dat))])
      dat <- private$.trimCols(
        dat,
        c("prec_cy_d", "prec_py_d", "gdd_cy_d", "gdd_py_d",
          "prec_cy_g", "prec_py_g", "gdd_cy_g", "gdd_py_g",
          "ndvi_cy_s", "ndvi_py_s", "ndvi_2py_s",
          "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
          "ndre_cy", "ndre_py", "ndre_2py",
          "cire_cy", "cire_py", "cire_2py")
      )
      dat <- cbind(dat, df)
      return(dat)
    },
    .makeFactors = function(dat) {
      dat$field <- factor(dat$field)
      dat$year <- factor(dat$year)
      dat$musym <- factor(dat$musym)
      return(dat)
    },
    .cleanDat = function(dat) {
      if (any(grepl("aa_n|aa_sr|yld|pro", names(dat)))) {
        col <- names(dat)[grep("aa_n|aa_sr", names(dat))]
        for(i in 1:length(col)){
          rows <- which(dat[names(dat) %in% col[i]] < self$clean_rate |
                          is.na(dat[names(dat) %in% col[i]]))
          dat <- dat[rows, ]
        }
        col <- names(dat)[grep("yld|pro", names(dat))]
        for(i in 1:length(col)){
          rows <- which(dat[names(dat) %in% col[i]] > 0 |
                          is.na(dat[names(dat) %in% col[i]]))
          dat <- dat[rows, ]
        }
      }
      return(dat)
    },
    .centerDat = function(dat) {
      num_names <- names(dat)[sapply(dat, is.numeric)]
      num_names <- num_names[!grepl("^x$|^y$|^yld$|^pro$", num_names)]
      self$num_means <- sapply(dat[num_names], mean, na.rm = TRUE)
      if (self$center) {
        for (i in 1:length(num_names)) {
          dat[names(dat) %in% num_names[i]] <-
            dat[names(dat) %in% num_names[i]] - self$num_means[i]
        }
      }
      return(dat)
    },
    .splitDat = function() {
      set.seed(6201994)
      self$mod_dat <- lapply(self$mod_dat, private$.splitDatTrnVal) %>%
        `names<-`(names(self$mod_dat))

      trn_dat <- rep(list(NA), length(self$mod_dat)) %>%
        `names<-`(names(self$mod_dat))
      val_dat <- rep(list(NA), length(self$mod_dat)) %>%
        `names<-`(names(self$mod_dat))

      for(i in 1:length(self$mod_dat)){
        trn_dat[[i]] <- self$mod_dat[[i]]$trn
        val_dat[[i]] <- self$mod_dat[[i]]$val
      }
      self$mod_dat <- list(trn_dat = trn_dat, val_dat = val_dat)
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
    }
  )
)







