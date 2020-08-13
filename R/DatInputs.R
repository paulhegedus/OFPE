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
    #' @field respvar Response variable to aggregate data for, select/input
    #' 'Yield' or 'Protein'. Multiple options allowed.
    respvar = NULL,
    #' @field expvar Experimental variable to aggregate data for, select/input
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
    #' @field analysis_grid Select whether to use gridded or observed data
    #' locations for the analysis step. See the 'AggInputs' class for more
    #' information on the 'GRID' option. The user must have aggregated data
    #' with the specified GRID option prior to this step. (i.e. you will not
    #' have access to data aggregated with the 'Grid' option if you have not
    #' executed the process of aggregation with the 'Grid' option selected. The
    #' same principle applies for the 'Observed' option. It is recommended that
    #' the analysis is performed with 'Observed' data, and for the simulation to
    #' be performed with 'Grid' data.
    analysis_grid = NULL,
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
    #' @param analysis_grid Select whether to use gridded or observed data
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
    #' @return A new 'AggInputs' object.
    initialize = function(dbCon,
                          farmername = NULL,
                          fieldname = NULL,
                          respvar = NULL,
                          expvar = NULL,
                          yldyears = NULL,
                          proyears = NULL,
                          analysis_grid = NULL,
                          sim_grid = NULL,
                          dat_used = NULL,
                          veg_index = NULL,
                          prec_source = NULL,
                          gdd_source = NULL,
                          center = NULL) {
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
      if (!is.null(analysis_grid)) {
        stopifnot(is.character(analysis_grid),
                  grepl("Grid|Observed", analysis_grid))
        self$analysis_grid <- ifelse(analysis_grid == "Grid", "grid", "obs")
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
    #' and simulation.
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
    }

    # more fields
    # analysis dat (train & validation)
    # simulation dat
    # rx dat??
    # like year dat (if selected)

    # has selectInputs() method
    # has importDat() method
    # has processDat(center = TRUE, veg_index = 'ndvi') method

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
        ## save choices in self file
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
      self$analysis_grid <- ifelse(gridOrObs == "Grid", "grid", "obs")
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
    }


  )
)







