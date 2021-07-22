#' @title R6 Class for generating a OFPE experiments
#'
#' @description R6 class for for creating a prescription or experiment for a
#' field of interest. The user can create a new experiment with inputs randomly
#' applied across the field with no stratification, or select data
#' on which to stratify experimental rates. This class randomly places experimental
#' input rates across a field(s) selected from the database, with the user's
#' choice for stratification.
#'
#' This class follows the generator interface that includes an initialization method
#' and an 'executeOutput' method.
#' @seealso \code{\link{DBCon}} for the database connection class, and
#' \code{\link{RxGen}} for the alternative class that creates
#' experimental prescriptions or pure prescriptions.
#' @export
ExpGen <- R6::R6Class(
  "ExpGen",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field trt_length Length, in meters, for which to apply treatments.
    trt_length = NULL,
    #' @field trt_width Width, in meters, for which to apply treatments.
    trt_width = NULL,
    #' @field heading Numeric, heading in degrees from true north to rotate the 
    #' experiment/prescription to. Default is 0 (no rotation). Note that if a 
    #' heading is provided, the grid is trimmed based on the buffered boundary but
    #' rotation caused by providing a heading may skew treatment rates so that 
    #' they encroach into the cleanup strip.
    heading = 0,
    #' @field fld_prop The proportion of the field to apply experimental
    #' or optimum check rates to.
    fld_prop = NULL,
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
    #' @field rx_for_year Provide the year that the experiment
    #' is made for. Used for labeling outputs.
    rx_for_year = NULL,
    #' @field SAVE Logical, whether to save figures and the experiment.
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise.
    SAVE = NULL,
    #' @field out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.).
    out_path = NULL,
    #' @field farmername If the user is creating a new experiment, provide or
    #' select the name of the farmer that owns or manages the field(s) that
    #' an experiment is going to be generated for.
    farmername = NULL,
    #' @field fieldname If the user is creating a new experiment, provide or
    #' select the field names of the field to use. The field list is from the
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
    strat_dat_parms = NULL,
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

    #' @field unique_fieldname Unique fieldname for the field(s) used for the experiment. This
    #' concatenates multiple fields with an ampersand. Used for labeling.
    unique_fieldname = NULL,
    #' @field rx_dt Data frame that contains the coordinates of locations to apply
    #' experimental inputs. For an experiment, these are the centroids of the grid
    #' made to aggregate data for the field.
    rx_dt = NULL,
    #' @field RX Table containing the geographic locations of the experiment. Also
    #' contains the rates in the experimental and as-applied units. This can be
    #' saved as a shapefile and given to the equipment applying the input.
    RX = NULL,
    #' @field out_name Created parameter including the unique fieldname and year the
    #' experiment is being generated for.
    out_name = NULL,
    #' @field out_map_name Created parameter with the file name for the map of the
    #' prescription. Used for saving the map to the 'Outputs' folder.
    out_map_name = NULL,
    #' @field cell_out_map_name Created parameter with the file name for the map of the
    #' rate type applied to each cell. Used for saving the map to the 'Outputs' folder.
    cell_out_map_name = NULL,
    #' @field var The label of the variable to map. Used in figure labeling for plotting
    #' in RxClass.
    var = NULL,
    #' @field var_col_name The name of the column of the variable in the
    #' supplied data ('dat'). Used in figure labeling for plotting
    #' in RxClass.
    var_col_name = NULL,
    #' @field var_label The label to be applied to the legend of the map
    #' corresponding to the variable mapped. Used in figure labeling for plotting
    #' in RxClass.
    var_label = NULL,
    #' @field var_main_label The main label to apply to the map. Used in figure
    #' labeling for plotting in RxClass.
    var_main_label = NULL,
    #' @field mgmt_scen For this class, the management scenario is always an experiment
    #' so this parameter is set to 'exp'.
    mgmt_scen = "exp",
    #' @field size The size of the treatment zones, which is the treatment width x
    #' the treatment length.
    size = NULL,
    #' @field strat_dat List holding all of the data used for stratification. 
    strat_dat = NULL,

    #' @description It is recommended to initialize this class through the RxClass,
    #' because it is integrated into the OFPE workflow. This also ensures all of the
    #' inputs are in the correct format and present.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param trt_length Length, in meters, for which to apply treatments.
    #' @param trt_width Width, in meters, for which to apply treatments.
    #' @param heading Numeric, heading in degrees from true north to rotate the 
    #' experiment/prescription to. Default is 0 (no rotation). Note that if a 
    #' heading is provided, the grid is trimmed based on the buffered boundary but
    #' rotation caused by providing a heading may skew treatment rates so that 
    #' they encroach into the cleanup strip.
    #' @param fld_prop The proportion of the field to apply experimental
    #' or optimum check rates to.
    #' @param expvar Experimental variable to optimize, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that is experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param conv The conversion factor between lbs of the input to the
    #' units of the as-applied input (i.e. lbs N/ac to unit input/ac)
    #' @param base_rate The rate to apply between the experimental rates
    #' and the field edge, or as check rates in the prescription selected
    #' option.
    #' @param rx_for_year Provide the year that experiment
    #' is made for. Used for labeling outputs.
    #' @param SAVE Logical, whether to save figures and the experiment
    #' Autofilled to FALSE if a user selects NA in the 'out_path' or is NULL.
    #' Autofilled to TRUE otherwise.
    #' @param out_path Provide the path to the folder in which to store and
    #' save figures and the prescription Type NA to not create any folders.
    #' You will not be able to save any outputs. (Note, even if a path is provided,
    #' the user can pass FALSE as the sole argument to the 'setupOP' method
    #' to prevent the creation of folders. This will automatically prevent
    #' any plots to be saved.).
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
    #' @return An initialized ExpGen R6 class object.
    initialize = function(dbCon,
                          trt_length,
                          trt_width,
                          heading = 0,
                          fld_prop,
                          expvar,
                          conv,
                          base_rate,
                          rx_for_year,
                          out_path = NULL,
                          SAVE,
                          fieldname,
                          farmername,
                          exp_rate_length,
                          exp_rates,
                          exp_rates_prop,
                          buffer_width = 0,
                          min_rate_jumps = NULL,
                          strat_dat_parms = NULL) {
      stopifnot(!is.null(dbCon),
                !is.null(trt_length),
                !is.null(trt_width),
                !is.null(heading),
                !is.null(fld_prop),
                !is.null(conv),
                !is.null(base_rate),
                !is.null(rx_for_year),
                !is.null(SAVE),
                !is.null(out_path),
                !is.null(farmername),
                !is.null(fieldname),
                !is.null(exp_rate_length),
                !is.null(exp_rates),
                !is.null(exp_rates_prop),
                is.numeric(trt_length),
                trt_length > 0,
                is.numeric(trt_width),
                trt_width > 0,
                is.numeric(heading),
                is.numeric(fld_prop),
                fld_prop >= 0 & fld_prop <= 1,
                is.numeric(conv),
                is.numeric(base_rate),
                is.numeric(rx_for_year),
                is.logical(SAVE),
                is.character(out_path),
                is.character(farmername),
                is.character(fieldname),
                is.numeric(exp_rate_length),
                is.numeric(exp_rates),
                is.numeric(exp_rates_prop),
                sum(exp_rates_prop) >= 0.99,
                is.character(expvar),
                any(grepl("aa_n|aa_sr", expvar)))
      self$dbCon <- dbCon
      self$trt_length <- round(trt_length, 0)
      self$trt_width <- round(trt_width, 0)
      self$buffer_width <- buffer_width
      self$heading <- heading
      self$fld_prop <- fld_prop
      self$conv <- conv
      self$base_rate <- round(base_rate, 0)
      self$rx_for_year <- rx_for_year
      self$SAVE <- SAVE
      self$out_path <- out_path
      self$expvar <- expvar
      if (is.na(self$out_path) | is.null(self$out_path)) {
        self$SAVE <- FALSE
      }
      self$farmername <- farmername
      self$fieldname <- fieldname
      self$exp_rate_length <- exp_rate_length
      self$exp_rates <- exp_rates
      self$exp_rates_prop <- exp_rates_prop
      self$strat_dat_parms <- strat_dat_parms
      if (!is.null(min_rate_jumps)) {
        stopifnot(is.character(min_rate_jumps),
                  any(grepl("N/S|E/W", min_rate_jumps)))
        self$min_rate_jumps <- min_rate_jumps
      }
      
      self$unique_fieldname<- OFPE::uniqueFieldname(self$fieldname)
      self$rx_dt <- private$.makeExpDt(self$fieldname,
                                       self$dbCon$db,
                                       self$base_rate,
                                       self$unique_fieldname,
                                       self$farmername)
      if (!is.null(self$strat_dat_parms)) {
        self$strat_dat <- private$.getStratDat()
      }
    },
    #' @description
    #' Method for calling the execution method of the experiment
    #' generator. This randomly applies the experimental rates across
    #' the field. If the user selected stratification data, these are
    #' used for stratification during the random placement.
    #' @param None All parameters supplied upon initialization.
    #' @return A completed experiment table containing the output.
    executeOutput = function() {
      rx_sdt <- OFPE::getRxGrid(self$dbCon$db,
                                self$rx_dt,
                                self$farmername,
                                self$fieldname,
                                self$trt_length,
                                self$trt_width,
                                self$unique_fieldname,
                                "base",
                                self$buffer_width,
                                self$heading) %>%
       # should be redundant - making sure cleanup strip added
        OFPE::trimGrid(self$fieldname,
                       self$dbCon$db,
                       self$farmername,
                       ifelse(self$heading == 0, self$buffer_width, 0))# 
      # get fieldname
      fields <- sf::st_read(dsn = self$dbCon$db, 
                            query = paste0("SELECT * FROM all_farms.fields;"),
                            geometry_column = "geom")
      temp_rx_sdt <- sf::st_transform(rx_sdt, 4326)
      temp_rx_sdt <- invisible(suppressWarnings(sf::st_intersection(temp_rx_sdt, fields)))
      rx_sdt$fieldname <- temp_rx_sdt$fieldname
      rm(fields, temp_rx_sdt)
      
      # apply exp rates with or without stratification
      if (is.null(self$strat_dat)) {
        rx_sdt <- OFPE::applyExpRates(rx_sdt, 
                                      self$exp_rates,
                                      self$exp_rates_prop,
                                      self$fld_prop,
                                      self$exp_rate_length)
      } else {
        rx_sdt <- private$.stratDatRates(rx_sdt,
                                         self$exp_rates,
                                         self$exp_rates_prop,
                                         self$fld_prop,
                                         self$exp_rate_length,
                                         self$base_rate,
                                         self$strat_dat_parms,
                                         self$strat_dat)
      }
      if (!is.null(self$min_rate_jumps)) {
        rx_sdt <- OFPE::minRateJumps(rx_sdt = rx_sdt, 
                                     min_rate_jumps = self$min_rate_jumps,
                                     rate_lengths = self$exp_rate_length + 1)
      }
      fld_bound <- OFPE::makeBaseRate(self$dbCon$db,
                                         self$fieldname,
                                         self$unique_fieldname,
                                         self$base_rate,
                                         self$trt_width,
                                         self$trt_length,
                                         self$farmername,
                                      "exp")
      self$RX <- OFPE::makeRx(rx_sdt, fld_bound, self$rx_for_year, self$conv, self$strat_dat_parms)
      private$.makeOutLabels()
    }
  ),
  private = list(
    .makeExpDt = function(fieldname, db, base_rate, unique_fieldname, farmername) {
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      fld_bound <- OFPE::getFldBound(fieldname, db, farmername)
      xy <- rep(list(NA), length(fieldname))
      for(i in 1:length(xy)) {
        OFPE::makeXmGrid(db, "No", fieldname[i], 10, farmername)
        xy[[i]] <- DBI::dbGetQuery(db,
            paste0("SELECT x, y 
                   FROM all_farms.gridtemp
                   WHERE field = '", fieldname[i], "'"))
        OFPE::removeTempTables(db)
      }
      xy <- do.call(rbind, xy)
      xy$X <- xy$x
      xy$Y <- xy$y
      sp::coordinates(xy) <- c("X", "Y")
      xy <- sf::st_as_sf(xy, remove_coordinates = FALSE)
      if (is.na(raster::crs(xy))) {
        sf::st_crs(xy) <- utm_epsg
      }
      xy <- suppressWarnings(sf::st_intersection(xy, fld_bound))
      xy <- as.data.frame(xy)[, 1:2]
      rx_dt <- matrix(nrow = nrow(xy), ncol = 24) %>%
        data.table::as.data.table()
      names(rx_dt) <- c("x", "y", "BaseP", "EXP.cost", "row", "col",
                        "field", "EXP.rate.ssopt", "NR.ssopt", "NR.min", "NR.opp",
                        "NR.fs", "yld.opt", "yld.min", "yld.fs", "pro.opt", "pro.min",
                        "pro.fs", "NR.ffopt", "EXP.rate.ffopt", "NR.act", "sim",
                        "base_rate", "bins")
      rx_dt[, 1:2] <- xy
      rx_dt$base_rate <- as.numeric(rx_dt$base_rate)
      rx_dt$base_rate <- base_rate
      rx_dt$cell_type <- "base"
      rx_dt$mgmt_scen <- "exp"
      return(rx_dt)
    },
    .getStratDat = function() {
      strat_dat <- rep(list(NA), length(self$fieldname)) %>%
        `names<-`(self$fieldname)
      for (i in 1:length(strat_dat)) {
        if (!is.na(self$strat_dat_parms[[i]]$col_name)) {
          strat_dat[[i]] <- rep(list(NA), length(self$strat_dat_parms[[i]]$col_name))
          names(strat_dat[[i]]) <- self$strat_dat_parms[[i]]$col_name
          for (j in 1:length(strat_dat[[i]])) { 
            table_selection <- ifelse(is.na(self$strat_dat_parms[[i]]$path[j]), 
                                      "Agg",
                                      "Raw")
            if (table_selection == "Raw") {
              strat_dat[[i]][[j]] <- sf::st_read(
                dsn = self$dbCon$db,
                query = paste0("SELECT *
                             FROM  ", self$farmername, "_r.", self$strat_dat_parms[[i]]$table[j], "
                             WHERE orig_file = '", self$strat_dat_parms[[i]]$path[j], "';")
              )
            } else {
              strat_dat[[i]][[j]] <- OFPE::fetchAggStratDat(
                table_var = self$strat_dat_parms[[i]]$table[j], 
                strat_var = self$strat_dat_parms[[i]]$col_name[j], 
                field = self$fieldname[i], 
                year = self$strat_dat_parms[[i]]$year[j], 
                farmername = self$farmername,
                grid_size = self$strat_dat_parms[[i]]$grid_size[j],
                db = self$dbCon$db
              )
            }
          }
        }
      }
      return(strat_dat)
    },
    .stratDatRates = function(rx_sdt,
                              exp_rates,
                              exp_rates_prop,
                              fld_prop,
                              exp_rate_length,
                              base_rate,
                              strat_dat_parms,
                              strat_dat) {
      stopifnot(length(exp_rates) == exp_rate_length,
                length(exp_rates_prop) == exp_rate_length)
      ## TODO: MAKE LESS CLUNKY & NOT GARBAGE CODE
      
      ## Stratification
      rx_sdt$strat_combo <- NA
      rx_sdt$fid <- 1:nrow(rx_sdt)
      # stratify rates by field
      for (i in 1:length(self$fieldname)) {
        if (!is.na(self$strat_dat_parms[[i]]$col_name)) {
          ## calculate 3 levels of strat_var & add to rx_sdt via nn
          for (j in 1:length(self$strat_dat[[i]])) {
            ## bin strat rates in strat dat and make spatial
            self$strat_dat[[i]][[j]] <- private$.binStratRates(
              self$strat_dat[[i]][[j]],
              self$strat_dat_parms[[i]]$col_name[j]) %>% 
              private$.makeSpatial(self$fieldname[i])
            
            ## get nearest neighbors b/w rx and strat dat
            nn_pnts <- private$.getNNpnts(rx_sdt, self$strat_dat[[i]][[j]], 30)
            
            ## add strat_col_f to rx data
            strat_col_f <- grep(paste0(self$strat_dat_parms[[i]]$col_name[j], "_f"), 
                                names(self$strat_dat[[i]][[j]]))
            strat_col <- grep(paste0(self$strat_dat_parms[[i]]$col_name[j], "_f"), 
                              names(rx_sdt))
            if (length(strat_col) == 0) {
              rx_sdt$strat_f <- self$strat_dat[[i]][[j]][nn_pnts[, 1], strat_col_f][[1]]
              rx_sdt$strat_f <- factor(as.character(rx_sdt$strat_f))
              names(rx_sdt)[grep("strat_f", names(rx_sdt))] <- 
                paste0(self$strat_dat_parms[[i]]$col_name[j], "_f")
            } else {
              fill_rows <- self$strat_dat[[i]][[j]][nn_pnts[, 1], strat_col_f][[1]] %>% 
                `names<-`(1:nrow(nn_pnts)) %>% 
                na.omit() %>% 
                names() %>%
                as.numeric()
              rx_sdt[fill_rows, strat_col] <- self$strat_dat[[i]][[j]][nn_pnts[, 1], strat_col_f][[1]] %>% 
                na.omit() %>% 
                as.numeric()
            }
            
            ## change any NA if polygon to 1 else as.integer(rnorm(1,1,3))??
            is_poly <- private$.isDatPoly(self$strat_dat[[i]][[j]])
            strat_col <- paste0(self$strat_dat_parms[[i]]$col_name[j], "_f") %>% 
              grep(names(rx_sdt))
            if (is_poly) {
              # if polygon and NA in the field assume it was a 0 rate where spray didn't record data
              rx_sdt[rx_sdt$fieldname == self$fieldname[i] & is.na(rx_sdt[, strat_col]), 
                     strat_col] <- 1
            } else {
              # randomly apply level ?
              # rx_sdt[rx_sdt$fieldname == self$fieldname[i] & is.na(rx_sdt[, strat_col]), 
              #        strat_col] <- as.integer(runif(1, 1, 4))
            }
            rm(strat_col)
          }
          
          ## Randomly apply experimental rates, stratified on vars
          # number of cells available for experimentation 
          fld_cols <- nrow(subset(rx_sdt, rx_sdt$fieldname == self$fieldname[i]))
          exp_cells <- DescTools::RoundTo(fld_cols * fld_prop, 1, floor)
          # get the number of cells per experimental treatment
          exp_reps <- OFPE::getExpReps(length(1:exp_cells), exp_rates_prop) %>%
            `names<-`(exp_rates)
          
          # make strat_combo from strat_var_f's 
          strat_f_cols <- self$strat_dat_parms[[i]]$col_name
          strat_f_col_nums <- grep(paste(strat_f_cols, collapse = "|"), names(rx_sdt))
          for (k in 1:length(strat_f_col_nums)) {
            strat_combo  <- paste0(
              names(rx_sdt)[strat_f_col_nums[k]], "-",
              rx_sdt[rx_sdt$fieldname == self$fieldname[i], strat_f_col_nums[k]][[1]]
            )
            if (k == 1) {
              rx_sdt[rx_sdt$fieldname == self$fieldname[i], "strat_combo"] <- strat_combo
            } else {
              rx_sdt[rx_sdt$fieldname == self$fieldname[i], "strat_combo"] <- paste0(
                rx_sdt[rx_sdt$fieldname == self$fieldname[i], "strat_combo"][[1]], 
                ".", strat_combo
              )
            }
          }
          
          # make sure that unique strat_combo < min(exp_rates) or min(unq_strat_combo) > length(exp_rates)
          temp <- rx_sdt[rx_sdt$fieldname == self$fieldname[i], "strat_combo"][[1]]
          unq_strat_combo <- unique(temp)[!is.na(unique(temp))]
          if (length(unq_strat_combo) > max(exp_reps)) {
            stop(print("ERROR: More stratification combinations than minimum number of reps for experimental rates (not enough reps of experimental rates to put in each stratification combo). SOLUTION: Reduce the number of experimental rates or decrease the amount of stratification."))
          }
          strat_rows <- rep(NA, length(unq_strat_combo)) %>% 
            `names<-`(unq_strat_combo)
          temp <- rx_sdt[rx_sdt$fieldname == self$fieldname[i], ]
          for (k in 1:length(unq_strat_combo)) {
            strat_rows[k] <- nrow(temp[temp$strat_combo == unq_strat_combo[k], ])
            # if (strat_rows < exp_rate_length) {
            #   stop(print("ERROR: More experimental rates than cells with stratification combination. SOLUTION: Reduce the number of experimental rates or decrease the amount of stratification."))
            # }
          }
          
          # create matrix with reps of each rate to apply in each strat_combo
          rep_mat <- matrix(NA, nrow = length(unq_strat_combo), ncol = exp_rate_length)
          row.names(rep_mat) <- unq_strat_combo
          colnames(rep_mat) <- exp_rates
          for (y in 1:ncol(rep_mat)){
            reps <- exp_reps[y]
            rep_mat[, y] <- floor(reps / nrow(rep_mat))
            reps <- reps - floor(reps / nrow(rep_mat)) * nrow(rep_mat)
            if (reps > 0 | reps == exp_reps[y]) {
              rep_mat[sample(1:nrow(rep_mat), reps), y] <- 
                rep_mat[sample(1:nrow(rep_mat), reps), y] + 1
            }
          }
          
          # balance reps on number of cells in each strat_combo
          # get difference between cells of strat combo and reps for strat_combo
          diffs <- strat_rows - rowSums(rep_mat) 
          for (x in 1:nrow(rep_mat)) {
            if (diffs[x] == 0 & any(rep_mat[x, ] != 1)) {
              # if same number of reps as cells of strat_combo but not one rep for each rate
              rep_mat[x, ] <- 1
            }
            if (diffs[x] < 0) {
              ## if number of strat cells < number of reps for a strat combo across rates
              # get number of reps to remove
              remove_tally <- -diffs[x] 
              ## 1) remove from cols with more than 1 rep
              repeat({
                if (any(rep_mat[x, ] > 1) & remove_tally != 0) {
                  # if any rates have more than one reps 
                  max_cols <- which(rep_mat[x, ] == max(rep_mat[x, ]))
                  if (length(max_cols) <= remove_tally) {
                    # if less or equal cols with more than 1 rep than number to remove or equal
                    # remove 1 from all cols with more than one rep 
                    rm_col <- max_cols
                  } else {
                    # if more cols with 2+ reps than reps to remove
                    # sample max_cols and subtract 1
                    rm_col <- sample(max_cols, remove_tally)
                  }
                  # subtract 1 from reps for columns with more than one rep
                  rep_mat[x, rm_col] <- rep_mat[x, rm_col] - 1
                  
                  # for each rep removed
                  # move rate to other strat_combo with enough reps
                  for (t in 1:length(rm_col)) {
                    # randomly sample rows != x
                    tar_row <- (1:nrow(rep_mat))[1:nrow(rep_mat) != x] %>% 
                      sample()
                    filled <- FALSE
                    # check each proposed target for if it can have a rep moved to it
                    for (k in 1:length(tar_row)) {
                      if (!filled & diffs[tar_row[k]] > 0) {
                        # if 1+ avail. cells in strat_combo 
                        rep_mat[tar_row[k], rm_col[t]] <- rep_mat[tar_row[k], rm_col[t]] + 1
                        filled <- TRUE
                        diffs <- strat_rows - rowSums(rep_mat) 
                      }
                    }
                  }
                  # subtract sample num from remove_tally (should = 0 now)
                  remove_tally <- remove_tally - length(rm_col)
                } else {
                  break
                }
              })
              ## 2) if more cells need removing after all have one rep
              if (remove_tally > 0) {
                # remove from rates != min median max
                if (remove_tally <= exp_rate_length) {
                  # the last rates you want to take reps from
                  last_resort <- c(round(length(exp_rates) / 2), # median
                                   exp_rate_length, # max
                                   1) # min
                  # randomize the other rates to take reps from
                  sample_rates <- (1:exp_rate_length)[!grepl(paste(paste0("^", last_resort, "$"), collapse = "|"), 
                                                             (1:exp_rate_length))] %>% 
                    sample()
                  # make order of removal
                  rem_order <- c(sample_rates, last_resort)
                  # columns to remove rep from
                  rm_col <- rem_order[1:remove_tally]
                  
                  # subtract 1 from reps for columns with more than one rep
                  rep_mat[x, rm_col] <- rep_mat[x, rm_col] - 1
                  
                  # for each rep removed
                  # move rate to other strat_combo with enough reps
                  for (t in 1:length(rm_col)) {
                    # randomly sample rows != x
                    tar_row <- (1:nrow(rep_mat))[1:nrow(rep_mat) != x] %>% 
                      sample()
                    filled <- FALSE
                    # check each proposed target for if it can have a rep moved to it
                    for (k in 1:length(tar_row)) {
                      if (!filled & diffs[tar_row[k]] > 0) {
                        # if 1+ avail. cells in strat_combo 
                        rep_mat[tar_row[k], rm_col[t]] <- rep_mat[tar_row[k], rm_col[t]] + 1
                        filled <- TRUE
                        diffs <- strat_rows - rowSums(rep_mat) 
                      }
                    }
                  }
                  remove_tally <- remove_tally - length(rm_col)
                } else {
                  # if number of reps to remove is more than there are experimental
                  # rates all reps go to 0 because no avail cells have the strat_combo
                  rep_mat[x, ] <- 0
                  remove_tally <- 0
                  diffs <- strat_rows - rowSums(rep_mat)
                }
              } # end if still need to remove cols and all exp rates have 1 rep
            } # end if more reps than strat_cells
          } # end rows of rep_mat (unq_strat_combo)
          
          # apply rates 
          # for each row and col of rep_mat
          for (y in 1:ncol(rep_mat)) {
            for (x in 1:nrow(rep_mat)) {
              if (rep_mat[x, y] != 0) {
                # get strat_f cols and levels for intersection
                strat_f_cols <- grep(paste(self$strat_dat_parms[[i]]$col_name,
                                           collapse = "|"), names(rx_sdt))
                temp <- row.names(rep_mat)[x]
                for (j in 1:length(strat_f_cols)) {
                  temp <- gsub(paste0(self$strat_dat_parms[[i]]$col_name[j], "_f-"), "", temp)
                }
                strat_f_rates <- strsplit(temp, ".", fixed = TRUE)[[1]]
                
                # subset rx_sdt cols with strat_f col levels
                temp <- rx_sdt[rx_sdt$fieldname == self$fieldname[i] & 
                                 rx_sdt$cell_type == "base", ]
                for (j in 1:length(strat_f_cols)) {
                  temp <- temp[temp[, strat_f_cols[j]][[1]] == strat_f_rates[j], ]
                }
                # sample and apply exp_rate level
                temp <- temp[sample(1:nrow(temp), rep_mat[x, y]), ]
                temp$exprate <- colnames(rep_mat)[y] %>% as.numeric()
                temp$cell_type <- "exp"
                # put back in rx_sdt
                if (nrow(temp) > 0){
                  rx_sdt[temp$fid, ] <- temp
                }
              }
            }
          } 
        } else {
          temp <- OFPE::applyExpRates(rx_sdt, 
                                      self$exp_rates,
                                      self$exp_rates_prop,
                                      self$fld_prop,
                                      self$exp_rate_length)
          rx_sdt[rx_sdt$fieldname == self$fieldname[i], ] <- temp[temp$fieldname == self$fieldname[i], ]
        }
      } ## End field
      
      # remove strat cols e.g.(strat_val_type, strat_val, strat_f, exp_f)
      rx_sdt$fid <- NULL
      # rx_sdt$strat_combo <- NULL
      return(rx_sdt)
    },
    .makeOutLabels = function() {
      self$out_name <- paste0(self$out_path,
                              "/Outputs/Rx/",
                              self$unique_fieldname, "_Exp_",
                              self$rx_for_year, ".shp")
      self$var <- paste0("exp", ifelse(self$expvar == "aa_n", "N", "Seed"), "Rates")
      self$var_col_name <- "exprate"
      self$var_label <- paste0(ifelse(self$expvar == "aa_n", "N", "Seed"), " Rate (lbs/ac)")
      self$var_main_label <- paste0(self$unique_fieldname, " experimental ",
                                    ifelse(self$expvar=="aa_n", "N", "Seed"),
                                    " rates for ", self$rx_for_year)
      self$out_map_name <- paste0(self$out_path, "/Outputs/Rx/",
                                  self$unique_fieldname, "_", tolower(self$var),
                                  "_ExpMap_", self$rx_for_year, ".png")
      self$cell_out_map_name <- paste0(self$out_path, "/Outputs/Rx/",
                                       self$unique_fieldname, "_rateTypeMap_",
                                       self$rx_for_year, ".png")
      self$size <- paste0(self$trt_width, " x ", self$trt_length)
    },
    .binStratRates = function(bin_df, strat_var_col) {
      stopifnot(!is.null(bin_df), 
                !is.null(strat_var_col))
      bin_df$fid <- 1:nrow(bin_df)
      strat_var_col_num <- grep(paste0("^", strat_var_col, "$"), names(bin_df))
      og_bin_df <- bin_df
      bin_df <- as.data.frame(bin_df)
      is_numeric <- private$.isStratNumeric(bin_df[, strat_var_col_num])
      if (!is_numeric) {
        bin_df[, strat_var_col_num] <- as.character(bin_df[, strat_var_col_num])
        bin_df$strat_f <- factor(bin_df[, strat_var_col_num])
      } else {
        bin_df[, strat_var_col_num] <- as.numeric(bin_df[, strat_var_col_num])
        mean_strat <- mean(bin_df[, strat_var_col_num], na.rm = TRUE)
        sd_strat <- sd(bin_df[, strat_var_col_num], na.rm = TRUE)
        bin_df <- bin_df[bin_df[, strat_var_col_num] < mean_strat + 2 * sd_strat, ]
        
        names(bin_df)[strat_var_col_num] <- "stratrate"
        rates <- bin_df$stratrate %>% na.omit()
        is_poly <- private$.isDatPoly(og_bin_df)
        
        if (sd(rates) != 0) {
          MIN <- ifelse(is_poly, 0, min(rates))
          qu1 <- summary(as.numeric(bin_df[, strat_var_col_num]))[[2]]
          qu3 <- summary(as.numeric(bin_df[, strat_var_col_num]))[[5]]
          breaks <- c(MIN, qu1, qu3, max(rates))
          
          tags <- rep(NA, 3)
          for (i in 1:3) {
            tags[i] <- paste0(breaks[i], " - ", breaks[i + 1])
          }
          bin_df$strat_f <- cut(bin_df$stratrate,
                                breaks = breaks,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = 1:3)
        } else {
          bin_df$strat_f <- NA
        }
        # names(bin_df)[strat_var_col_num] <- strat_var_col
      }
      og_bin_df <- og_bin_df[bin_df$fid, ]
      og_bin_df$strat_f <- bin_df$strat_f
      names(og_bin_df)[grep("strat_f", names(og_bin_df))] <- paste0(strat_var_col, "_f")
      return(og_bin_df)
    }, 
    .binExpRates = function(bin_df) {
      stopifnot(!is.null(bin_df),
                !is.null(self$exp_rate_length))
      if (length(unique(bin_df$exprate)) > (self$exp_rate_length + 1)) {
        rates <- bin_df$exprate 
        if (sd(rates) != 0) {
          breaks <- seq(min(rates), 
                        max(rates), 
                        (max(rates) - min(rates)) / self$exp_rate_length)
          tags <- rep(NA, self$exp_rate_length)
          for (i in 1:self$exp_rate_length) {
            tags[i] <- paste0(breaks[i], " - ", breaks[i + 1])
          }
          bin_df$exp_f <- cut(bin_df$exprate,
                              breaks = breaks,
                              include.lowest = TRUE,
                              right = FALSE,
                              labels = 1:self$exp_rate_length)
        } else {
          bin_df$exp_f <- NA
        }
      } else {
        bin_df[bin_df$exprate == self$base_rate, "exprate"] <- NA
        bin_df$exp_f <- factor(bin_df$exprate)
        levels(bin_df$exp_f) <- 1:length(unique(bin_df$exprate))
        bin_df[is.na(bin_df$exprate), "exprate"] <- self$base_rate
      }
      return(bin_df)
    },
    .makeSpatial = function(strat_dat, fieldname) {
      utm_epsg <- OFPE::findUTMzone(self$dbCon$db, fieldname = fieldname)
      
      if (any(grepl("^x$|^y$", names(strat_dat)))) {
        strat_dat <- strat_dat[!is.na(strat_dat$x), ]
        strat_dat <- strat_dat[!is.na(strat_dat$y), ]
        strat_dat$X <- strat_dat$x
        strat_dat$Y <- strat_dat$y
        sp::coordinates(strat_dat) <- c("X", "Y")
        strat_dat <- sf::st_as_sf(strat_dat, remove_coordinates = FALSE)
      } 
      if (is.na(raster::crs(strat_dat))) {
        sf::st_crs(strat_dat) <- utm_epsg
      } else {
        strat_dat <- sf::st_transform(strat_dat, utm_epsg)
      }
      return(strat_dat)
    },
    .isStratNumeric = function(vec) {
      if (class(vec) == "character") {
        # if it is a character
        is_numeric <- FALSE
        if (!anyNA(suppressWarnings(as.numeric(na.omit(vec))))) {
          is_numeric <- TRUE
        }
      } else {
        # if it is numeric
        is_numeric <- TRUE
        if (length(unique(vec)) <= 3) {
          # if unique <= 3
          is_numeric <- FALSE
        }
      }
      return(is_numeric)
    },
    .getNNpnts = function(rx_sdt, strat_dat, maxdist) {
      nn_pnts <- nngeo::st_nn(rx_sdt, strat_dat, maxdist = maxdist)
      nn_pnts <- lapply(nn_pnts, function(x) if (length(x) == 0) {x <- NA} else {x <- x})
      nn_pnts <- do.call(rbind, nn_pnts)
      return(nn_pnts)
    },
    .isDatPoly = function(dat) { 
      geom_clmn <- lapply(dat, class) %>% 
        rbind() %>%
        as.data.frame() %>%
        names()
      geom_clmn <- grep("geom", geom_clmn)
      is_poly <- FALSE
      if (length(geom_clmn) > 0) {
        for (k in 1:length(geom_clmn)) {
          if (any(grepl("polygon", class(dat[, geom_clmn[k]][[1]]), 
                        ignore.case = TRUE))) {
            is_poly <- TRUE
          }
        }
      }
      return(is_poly)
    }
  )
)
