#' @title R6 Class for plotting outputs of the OFPE simulation.
#'
#' @description R6 class for generating maps and figures from observed data
#' in an OFPE database. This class can be initialized with arguments required
#' to run the methods for acquiring or making figures, or can be initialized solely
#' with a 'create' argument, described below. When an empty 'ObsOP' class is initialized,
#' the methods must have all arguments passed, whereas if the user initializes the
#' 'ObsOP' class with arguments, methods can be executed with minimal other arguments.
#'
#' It is recommended to initialize one 'ObsOP' class for each dataset desired for
#' analysis. Multiple fields will be treated as one, and only one year is allowed
#' for gathering.
#'
#' This class can be used to import data from an OFPE database for data
#' exploration, quality control, and general use. The user can fetch data
#' by providing specific arguments, or by providing a SQL statement for
#' customized fetching from the database. Data can be returned to an object
#' or can be used internally with the methods below.
#'
#' Methods are available for combining data from datasets by kriging. This can be
#' useful for putting protein and yield in the same data frame or either/both on
#' data from a 'sat' table on grid points. This would also be required to use the
#' method for calculating net-return with yield and protein.
#'
#' This class also contains methods for mapping and plotting variables from
#' user specified data. The user can map 10m rasters or the points of the
#' specified data, can create scatterplots of two variables, or can create
#' histograms of a variable. These figures are returned from the methods but also can be saved to a user
#' specified locations. This is an R6 class so all parameters needed for
#' generating figures can be supplied upon class initialization. However, to
#' use more generally, the class can be initialized and figure making methods
#' can be run individually.
#'
#' ## TODO: correlation/CV maps, summary statistics
#'
#' @export
ObsOP <- R6::R6Class(
  "ObsOP",
  public = list(
    #' @field out_path Optional, path to folder to save outputs. Also serveds
    #' as a logical for whether to create the 'Output' folder and save
    #' figures, maps, and tables. If this argument is left null, the user will
    #' have to provide the path to save outputs for each individual method.
    out_path = NULL,
    #' @field SAVE Whether to save outputs. This is set based off of the user
    #' providing a path to a folder to make outputs.
    SAVE = NULL,
    #' @field unique_fieldname Unique name for the field(s) used in the simulation.
    #' This is used for saving and labeling of outputs. e.g. "sec35middle" or
    #' "sec1east & sec1west". This must match the 'unique_fieldname' used to
    #' save the simulation outputs.
    unique_fieldname = NULL,
    #' @field utm_fieldname The name of the field (corresponding to the name in the
    #' OFPE database) that should be used for identifying the UTM zone. In the
    #' case of multiple fields (i.e. "sec1east & sec1west") choose one because
    #' their UTM code will be the same.
    utm_fieldname = NULL,

    #' @field db Connection to a database to gather data from. Must be
    #' set up and filled in the OFPE specific format.
    db = NULL,
    #' @field farmername The name of the farmer, corresponding to the format in the
    #' database, e.g. lowercase.
    farmername = NULL,
    #' @field fieldname The name of the field(s) to gather data for. Must
    #' match the format in the OFPE database.
    fieldname = NULL,
    #' @field year The year to gather data for.
    year = NULL,
    #' @field dat_tab The table that the data is in within the aggregated schema of
    #' the database. This corresponds to the type of data gathered (i.e. 'yld',
    #' 'pro', or 'sat').
    dat_tab = NULL,
    #' @field GRID Select whether to gather gridded or observed data
    #' locations. See the 'AggInputs' class for more information on the 'GRID'
    #' option. Provide either "grid" or "obs".
    GRID = NULL,
    #' @field dat_used Option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection.
    dat_used = NULL,

    #' @field orig_file The original file name of the data to gather.
    orig_file = NULL,

    #' @description
    #' Use this class to import data from an OFPE database, save figures and
    #' explore observed data. This class can be initialized with a arguments required
    #' for executing the methods of the class or with only a 'create' parameter,
    #' see below. If the user supplies all arguments to the class, they can
    #' typically use the methods with one or two arguments. A database connection
    #' must be supplied to import data from the database. If the user imports data,
    #' it is initialized into the class.
    #'
    #' The logical argument, 'create', gives the user a choice to select whether to save
    #' figures and maps. Depending on this choice, the output folders will be generated.
    #' This is done by a private method that sets up the output location for
    #' the figures that the model produces. This will not overwrite any previously
    #' generated diagnostic or validation plots from the ModClass. Functions for
    #' plots and maps all have save options which must be accompanied by a folder
    #' path. The folder created is named 'Outputs'. This folder contains a
    #' folder called 'ObsMaps' which contains maps of observed data.
    #'
    #' @param out_path Optional, path to folder to save outputs. Also serveds
    #' as a logical for whether to create the 'Output' folder and save
    #' figures, maps, and tables. If this argument is left null, the user will
    #' have to provide the path to save outputs for each individual method.
    #' @param db Optional, connection to a database to gather data from. Must be
    #' set up and filled in the OFPE specific format.
    #' @param farmername Optional, the name of the farmer, corresponding to the format in the
    #' database, e.g. lowercase.
    #' @param fieldname Optional, the name of the field(s) to gather data for. Must
    #' match the format in the OFPE database.
    #' @param year Optional, the year to gather data for.
    #' @param dat_tab Optional, the table that the data is in within the aggregated schema of
    #' the database. This corresponds to the type of data gathered (i.e. 'yld',
    #' 'pro', or 'sat').
    #' @param GRID Optional, select whether to gather gridded or observed data
    #' locations. See the 'AggInputs' class for more information on the 'GRID'
    #' option. Provide either "grid" or "obs".
    #' @param dat_used Optional, option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection. Provide either
    #' "decision_point" or "full_year".
    #' @param utm_fieldname The name of the field (corresponding to the name in the
    #' OFPE database) that should be used for identifying the UTM zone. In the
    #' case of multiple fields (i.e. "sec1east & sec1west") choose one because
    #' their UTM code will be the same.

    #' @return Initialized R6 class and a folder created in the path for
    #' model output figures if specified.
    initialize = function(out_path = NULL,
                          farmername = NULL,
                          fieldname = NULL,
                          year = NULL,
                          dat_tab = NULL,
                          GRID = NULL,
                          db = NULL,
                          dat_used = NULL,
                          utm_fieldname = NULL) {
      if (!is.null(out_path)) {
        stopifnot(is.character(out_path))
        self$out_path <- out_path
        private$.setupOP()
        self$SAVE <- TRUE
      } else {
        self$SAVE <- FALSE
      }
      if (!is.null(farmername)) {
        stopifnot(is.character(farmername))
        self$farmername <- farmername
      }
      if (!is.null(fieldname)) {
        stopifnot(is.character(fieldname))
        self$fieldname <- fieldname
        self$unique_fieldname <-
          OFPE::uniqueFieldname(self$fieldname)
      }
      if (!is.null(year)) {
        self$year <- year
      }
      if (!is.null(dat_tab)) {
        stopifnot(is.character(dat_tab),
                  any(grepl("yld|pro|sat|aa_", dat_tab)))
        self$dat_tab <- dat_tab
      }
      if (!is.null(GRID)) {
        stopifnot(is.character(GRID),
                  grepl("grid|obs", GRID))
        self$GRID <- GRID
      }
      if (!is.null(db)) {
        self$db <- db
      }
      if (!is.null(dat_used)) {
        stopifnot(is.character(dat_used),
                  grepl("decision_point|full_year", dat_used))
        self$dat_used <- dat_used
      }
      if (!is.null(utm_fieldname)) {
        stopifnot(is.character(utm_fieldname))
        self$utm_fieldname <- utm_fieldname
      }
      # ...
    },
    ## Data Gather and Manipulation
    #' @description Gather data from an aggregated table in an
    #' OFPE database. Provide the farmername, fieldname(s), year(s),
    #' and type of data to gather.
    #' @param db Connection to a database to gather data from. Must be
    #' set up and filled in the OFPE specific format.
    #' @param farmername The name of the farmer, corresponding to the format in the
    #' database, e.g. lowercase.
    #' @param fieldname The name of the field(s) to gather data for. Must
    #' match the format in the OFPE database.
    #' @param year The year(s) to gather data for.
    #' @param dat_tab The table that the data is in within the aggregated schema of
    #' the database. This corresponds to the type of data gathered (i.e. 'yld',
    #' 'pro', or 'sat').
    #' @param GRID Select whether to gather gridded or observed data
    #' locations. See the 'AggInputs' class for more information on the 'GRID'
    #' option. Provide either "grid" or "obs".
    #' @param dat_used Optional, option for the length of year to use data in the analysis,
    #' simulation, and prescription building steps. See the 'AggInputs' class
    #' documentation for more information on the 'dat_used' selection. Provide either
    #' "decision_point" or "full_year".
    #' @return Data table with specified data.
    fetchAggDat = function(db = self$db,
                           farmername = self$farmername,
                           fieldname = self$fieldname,
                           year = self$year,
                           dat_tab = self$dat_tab,
                           GRID = self$GRID,
                           dat_used = self$dat_used) {
      dat <- private$.fetchDat(GRID,
                               dat_tab,
                               year,
                               fieldname,
                               db,
                               farmername,
                               dat_used)
      return(dat)
    },
    #' @description Gather data from a raw table in an
    #' OFPE database. Provide the farmername, the type of data, and the
    #' name of the original file for the data ('orig_file').
    #' @param db Connection to a database to gather data from. Must be
    #' set up and filled in the OFPE specific format.
    #' @param farmername The name of the farmer, corresponding to the format in the
    #' database, e.g. lowercase.
    #' @param dat_tab The table that the data is in within the raw schema of
    #' the database. This corresponds to the type of data gathered (i.e. 'yld',
    #' 'aa_n_poly', 'aa_sr', etc.).
    #' @param orig_file The original file name of the data to gather.
    #' @return Data table with specified data.
    fetchRawDat = function(db = self$db,
                           farmername = self$farmername,
                           dat_tab = self$dat_tab,
                           orig_file = self$orig_file) {
      dat <- DBI::dbGetQuery(
        db,
        paste0("SELECT * FROM ",
               farmername, "_r.", dat_tab,
               " WHERE orig_file = '", orig_file, "'")
      )
      return(dat)
    },

    #' @description Interpolate data using kriging.
    #' @param source_dat The data to interpolate from.
    #' @param target_dat The data to krige data to.
    #' @param var The variable to interpolate.
    #' @return Data table with interpolated data.
    krigeDat = function(source_dat, target_dat, var) {
      ## krige data to another dataset (i.e. yld to pro)
      stopifnot(
        any(grepl("x|y", names(source_dat))),
        any(grepl("x|y", names(target_dat)))
      )
      dat_list <- list(source = source_dat,
                       target = target_dat) %>%
        lapply(as.data.frame) %>%
        lapply(function(x) {x$X <- x$x; x$Y <- x$y; sp::coordinates(x) <- ~X+Y; return(x)}) %>%
        lapply(private$.removeDupPts)


      krige_formula <- as.formula(paste0(var, " ~ x + y"))
      ## fit variogram
      dpVgm <- gstat::variogram(eval(krige_formula), dat_list$source)
      dpVgmFit <- suppressWarnings(
        gstat::fit.variogram(dpVgm,
                             gstat::vgm(c("Cir","Sph","Pen","Mat","Nug","Exp","Gau",
                                          "Exc","Ste","Lin", "Bes", "Per","Wav",
                                          "Hol","Log","Spl")))
      )
      #plot(dpVgm, dpVgmFit)

      krigVal <- gstat::krige(eval(krige_formula),
                              dat_list$source,
                              dat_list$target,
                              dpVgmFit)
      dat_list$target$pred <- krigVal$var1.pred
      names(dat_list$target)[grep("pred", names(dat_list$target))] <- var
      return(dat_list$target)
    },

    calcNR = function() {


    },

    ## Outputs
    #' @description
    #' This method is for plotting maps of observed variables. The user
    #' must supply the data and variable to map. Other arguments relate to
    #' labeling the map.
    #' @param dat Data frame with variables to map. Must include an x and y
    #' column.
    #' @param var The label of the variable to map. Used in figure name.
    #' @param var_col_name The name of the column of the variable in the
    #' supplied data ('dat').
    #' @param var_label The label to be applied to the legend of the map
    #' corresponding to the variable mapped.
    #' @param var_main_label The main label to apply to the map.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param year Year of the observed data.
    #' @param SAVE Logical, whether to save figure.
    #' @param farmername The name of the farmer that manages the field.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param db Connection to the OFPE database to identify UTM zone.
    #' @param utm_fieldname Name of the field for identifying the UTM zone.
    #' @return A 'ggmap' object and maps saved in 'Outputs/Maps' folder if selected.
    plotObsMaps = function(dat,
                           var,
                           var_col_name,
                           var_label,
                           var_main_label = NULL,
                           fieldname = self$unique_fieldname,
                           year = self$year,
                           SAVE = self$SAVE,
                           farmername = self$farmername,
                           out_path = self$out_path,
                           db = self$db,
                           utm_fieldname = self$utm_fieldname) {
      stopifnot(length(year) == 1)
      if (is.null(var_main_label)) {
        var_main_label <- paste0("Observed ", year," ", var)
      }
      utm_zone <- OFPE::findUTMzone(db,
                                    fieldname = utm_fieldname)
      p <- OFPE::plotMaps(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername,
                          utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()

      ## TODO: need to specify ObsMaps folder?? or leave up
      ## to user

      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = paste0(out_path,
                        fieldname, "_", tolower(var),
                        "_map_", year, ".png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      return(p)
    },

    plotScatters = function() {
      ## takes two vars and plot

    },

    plotHist = function() {
      ## make histogram of specified var

    }

    ## TODO:
    # correlation map maker
    #   - map the relationship between two variables across a field
    #     -> used to identify areas where observations are related
    #   - yld/pro correlations - only possible if pro data available

  ),
  private = list(
    .setupOP = function() {
      stopifnot(!is.null(self$out_path))
      cwd <- paste0(self$out_path, "/Outputs") # outputs working directory
      if (!file.exists(cwd)) {
        dir.create(cwd)
        dir.create(paste0(cwd, "/", "ObsMaps"))
      } else {
        if (!file.exists(paste0(cwd, "/", "ObsMaps"))) {
          dir.create(paste0(cwd, "/", "ObsMaps"))
        }
      }
    },
    .fetchDat = function(GRID,
                         dat_tab,
                         year,
                         fieldname,
                         db,
                         farmername,
                         dat_used) {
      dat <- as.list(fieldname) %>%
        `names<-`(fieldname)
      dat <- lapply(dat,
                    private$.getDBdat,
                    year,
                    dat_tab,
                    GRID,
                    db,
                    farmername,
                    dat_used) %>%
        data.table::rbindlist()
      return(dat)
    },
    .getDBdat = function(fieldname,
                         year,
                         respvar,
                         GRID,
                         db,
                         farmername,
                         dat_used) {
      browser()

      OFPE::removeTempFarmerTables(db, farmername)
      invisible(
        DBI::dbSendQuery(
          db,
          paste0(
            "CREATE TABLE ", farmername,"_a_OG.temp AS (SELECT *
            FROM ", farmername, "_a_OG.", respvar, " ", respvar,"
            WHERE field = '", fieldname, "'
            AND year = '", year, "'
            AND grid = '", GRID, "'
            AND datused = '", dat_used,"');"
          )
        )
      )
      invisible(
        DBI::dbSendQuery(
          db,
          paste0(
            "ALTER TABLE ",
            farmername, "_a_OG.temp
            DROP COLUMN geometry;"
          )
        )
      )
      db_dat <- invisible(
        DBI::dbGetQuery(
          db,
          paste0("SELECT * FROM ", farmername, "_a_OG.temp;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          db,
          paste0(
            "DROP TABLE ", farmername, "_a_OG.temp;"
          )
        )
      )
      return(db_dat)
    },
    .removeDupPts = function(x) {
      zd <- sp::zerodist(x)
      if (nrow(zd) != 0) {
        x <- x[-sp::zerodist(x)[,1], ]
      }
      return(x)
    }
  )
)




