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
    #' @field boom_width The width of the sprayer boom or spreader.
    boom_width = NULL,
    #' @field orientation TODO... Not implemented yet.
    orientation = NULL,
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
    #' @field strat_dat If the user is creating a new experiment with stratification
    #' they must apply the data that they are stratifying on. This provides an
    #' identifier for the data being used for the stratification. These must be codes
    #' used in the OFPE workflow such as 'yld', 'pro', 'aa_n', 'aa_sr', etc.
    strat_dat = NULL,
    #' @field strat_dat_year Follows the same structure of 'strat_dat', however contains
    #' the years to use for stratification of each of the data types for each of the fields.
    strat_dat_year = NULL,

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
    #' @field size The size of the treatment zones, which is the treatment length x
    #' the boom width x 2.
    size = NULL,

    #' @description It is recommended to initialize this class through the RxClass,
    #' because it is integrated into the OFPE workflow. This also ensures all of the
    #' inputs are in the correct format and present.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param trt_length Length, in meters, for which to apply treatments.
    #' @param boom_width The width of the sprayer boom or spreader.
    #' @param orientation TODO... Not implemented yet.
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
    #' @param strat_dat If the user is creating a new experiment with stratification
    #' they must apply the data that they are stratifying on. This provides an
    #' identifier for the data being used for the stratification. These must be codes
    #' used in the OFPE workflow sucha as 'yld', 'pro', 'aa_n', 'aa_sr', etc.
    #' @param strat_dat_year Follows the same structure of 'strat_dat', however contains
    #' the years to use for stratification of each of the data types for each of the fields.
    #' @return An initialized ExpGen R6 class object.
    initialize = function(dbCon,
                          trt_length,
                          boom_width,
                          orientation,
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
                          strat_dat = NULL,
                          strat_dat_year = NULL) {
      stopifnot(!is.null(dbCon),
                !is.null(trt_length),
                !is.null(boom_width),
                !is.null(orientation),
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
      self$boom_width <- round(boom_width, 0)
      self$orientation <- orientation
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

      if (!is.null(strat_dat)) {
        stopifnot(is.character(strat_dat),
                  length(strat_dat) == length(fieldname))
        for (i in 1:length(strat_dat)) {
          stopifnot(any(grepl("yld|pro|aa_n|aa_sr", strat_dat[i])))
        }
        self$strat_dat <- strat_dat
      }
      if (!is.null(strat_dat_year)) {
        stopifnot(is.character(strat_dat_year) | is.numeric(strat_dat_year),
                  length(strat_dat_year) == length(self$fieldname))
        self$strat_dat_year <- strat_dat_year
      }
      self$unique_fieldname<- OFPE::uniqueFieldname(self$fieldname)
      self$rx_dt <- private$.makeExpDt(self$fieldname,
                                       self$dbCon$db,
                                       self$base_rate,
                                       self$unique_fieldname,
                                       self$farmername)
      if (!is.null(self$strat_dat) &
          !is.null(self$strat_dat_year)) {
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
                                self$boom_width,
                                self$unique_fieldname,
                                "base") %>%
        ## TODO - orientation
        OFPE::trimGrid(self$fieldname,
                       self$dbCon$db,
                       self$farmername) %>%
        OFPE::applyExpRates(self$exp_rates,
                            self$exp_rates_prop,
                            self$fld_prop,
                            self$exp_rate_length)
      rx_sdt$fieldname <- NA
      if (!is.null(self$strat_dat)) {
        rx_sdt <- private$.stratDatRates(rx_sdt)
      }
      fld_bound <- OFPE::makeBaseRate(self$dbCon$db,
                                         self$fieldname,
                                         self$unique_fieldname,
                                         self$base_rate,
                                         self$boom_width,
                                         self$trt_length,
                                         self$farmername,
                                      "exp")
      self$RX <- OFPE::makeRx(rx_sdt, fld_bound, self$rx_for_year, self$conv)
      private$.makeOutLabels()
    }
  ),
  private = list(
    .makeExpDt = function(fieldname, db, base_rate, unique_fieldname, farmername) {
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      fld_bound <- OFPE::getFldBound(fieldname, db, farmername)
      xy <- rep(list(NA), length(fieldname))
      for(i in 1:length(xy)) {
        xy[[i]] <- DBI::dbGetQuery(db,
            paste0("SELECT x, y
                   FROM all_farms.grids
                   WHERE field = '", fieldname[i], "'"))
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
      strat_list <- as.list(self$fieldname)
      # get data
      for (i in 1:length(self$fieldname)) {
        if (!grepl("aa_", self$strat_dat[i])) {
          strat_list[[i]] <- private$.fetchStratDat(self$strat_dat[i],
                                                    self$strat_dat[i],
                                                    self$fieldname[i],
                                                    self$strat_dat_year[i],
                                                    self$farmername)
        } else {
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
            strat_list[[i]] <- private$.fetchStratDat("yld",
                                                      self$strat_dat[i],
                                                      self$fieldname[i],
                                                      self$strat_dat_year[i],
                                                      self$farmername)
          } else {
            ## Check for pro dat
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
              strat_list[[i]] <- private$.fetchStratDat("pro",
                                                        self$strat_dat[i],
                                                        self$fieldname[i],
                                                        self$strat_dat_year[i],
                                                        self$farmername)
            } else {
              ## get raw data???
              stop(print(paste0("STRAT DAT MUST BE IN farmername_a SCHEMA WHERE grid = 'grid' FOR SPECIFIED DATA.")))
            }
          }
        }
      }
      return(strat_list)
    },
    .fetchStratDat = function(table_var, strat_var, field, year, farmername, grid) {
      strat_df <- DBI::dbGetQuery(
        self$dbCon$db,
        paste0("SELECT x, y, cell_id, field, ", strat_var, "  FROM ",
               farmername, "_a.", table_var,
               " WHERE field = '", field, "'
               AND year = '", year, "'
               AND grid = 'grid'
               AND datused = 'decision_point';"))
      if (nrow(strat_df) == 0) {
        strat_df <- DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT x, y, cell_id, field, ", strat_var, "  FROM ",
                 farmername, "_a.", table_var,
                 " WHERE field = '", field, "'
               AND year = '", year, "'
               AND grid = 'grid'
               AND datused = 'full_year';"))
      }
      if (nrow(strat_df) == 0) {
        strat_df <- DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT x, y, cell_id, field, ", strat_var, "  FROM ",
                 farmername, "_a.", table_var,
                 " WHERE field = '", field, "'
               AND year = '", year, "'
               AND grid = 'obs'
               AND datused = 'decision_point';"))
      }
      if (nrow(strat_df) == 0) {
        strat_df <- DBI::dbGetQuery(
          self$dbCon$db,
          paste0("SELECT x, y, cell_id, field, ", strat_var, "  FROM ",
                 farmername, "_a.", table_var,
                 " WHERE field = '", field, "'
               AND year = '", year, "'
               AND grid = 'obs'
               AND datused = 'full_year';"))
      }
      if (nrow(strat_df) == 0) {
        stop(print(paste0("NO ", strat_var, " DATA AVAILABLE.")))
      }
      return(strat_df)
    },
    .stratDatRates = function(rx_sdt) {
      ## for each strat_dat
      for (i in 1:length(self$fieldname)) {
        self$strat_dat[[i]] <- private$.binStratRates(self$strat_dat[[i]])
      }
      ## make spatial
      strat_dat <- data.table::rbindlist(self$strat_dat)
      utm_epsg <- OFPE::findUTMzone(self$dbCon$db, fieldname = self$fieldname[1])
      strat_dat$X <- strat_dat$x
      strat_dat$Y <- strat_dat$y
      sp::coordinates(strat_dat) <- c("X", "Y")
      strat_dat <- sf::st_as_sf(strat_dat, remove_coordinates = FALSE)
      if (is.na(raster::crs(strat_dat))) {
        sf::st_crs(strat_dat) <- utm_epsg
      }
      ## rx_sdt point intersect with raster for strat_f
      nn_pnts <- nngeo::st_nn(rx_sdt, strat_dat) 
      nn_pnts <- do.call(rbind, nn_pnts)
      rx_sdt$strat_f <- strat_dat[nn_pnts[,1], "strat_f"]$strat_f
      rx_sdt$fieldname <- strat_dat[nn_pnts[,1], "field"]$field
      ## make bins for exp dat
      if (self$exp_rate_length > 1) {
        rx_sdt <- private$.binExpRates(rx_sdt)
      }
      ## if exp rate in same bin as strat rate, change bin ONLY for cell_type = exp
      rx_sdt$fid <- 1:nrow(rx_sdt)
      if (self$exp_rate_length > 1) {
        for (i in 1:nrow(rx_sdt)) {
          vec <- rx_sdt[i, ]
          if (vec$cell_type == "exp") {
            if (!is.na(vec$exp_f) & !is.na(vec$strat_f) &
                !is.null(vec$exp_f) & !is.null(vec$strat_f)) {
              if (vec$strat_f == vec$exp_f) {
                exp_levs <- 1:(self$exp_rate_length ) %>% as.numeric()
                lev <- as.numeric(vec$exp_f)
                stopifnot(lev == as.numeric(vec$strat_f))
                repeat({
                  fid <- sample(rx_sdt[rx_sdt$exp_f != lev & rx_sdt$strat_f != lev, "fid"]$fid, 1)
                  if (!is.na(fid)) {
                    if (as.numeric(rx_sdt[fid, "strat_f"]$strat_f) !=
                        as.numeric(rx_sdt[i, "exp_f"]$exp_f)) {
                      break
                    }
                  }
                })
                vec$exp_f <- rx_sdt[fid, "exp_f"]$exp_f %>% as.numeric()
                vec$exprate <- rx_sdt[fid, "exprate"]$exprate %>% as.numeric()
                vec$cell_type <- rx_sdt[fid, "cell_type"]$cell_type 
                
                rx_sdt[fid, "exp_f"] <- rx_sdt[i, "exp_f"]$exp_f %>% as.numeric()
                rx_sdt[fid, "exprate"] <- rx_sdt[i, "exprate"]$exprate %>% as.numeric()
                rx_sdt[fid, "cell_type"] <- rx_sdt[i, "cell_type"]$cell_type
                
                stopifnot(rx_sdt[fid, "exp_f"]$exp_f %>% as.numeric() != 
                            rx_sdt[fid, "strat_f"]$strat_f %>% as.numeric(),
                          vec$exp_f != vec$strat_f)
              }
            }
          }
          rx_sdt[i, ] <- vec
        }
      }
      # remove strat cols e.g.(strat_val_type, strat_val, strat_f, exp_f)
      rx_sdt$fid <- NULL
      rx_sdt$strat_f <- NULL
      rx_sdt$exp_f <- NULL
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
      self$size <- paste0(self$trt_length, " x ", self$boom_width * 2)
    },
    .binStratRates = function(bin_df) {
      stopifnot(!is.null(bin_df),
                !is.null(self$exp_rate_length))
      strat_var_col <- names(bin_df)[!grepl(paste(c("^x$", "^y$", "cell_id", "field"),
                                                              collapse = "|"),
                                                        names(bin_df))]
      strat_var_col_num <- grep(strat_var_col, names(bin_df))
      names(bin_df)[strat_var_col_num] <- "stratrate"
      rates <- bin_df$stratrate %>% na.omit()
      if (sd(rates) != 0) {
        breaks <- seq(min(rates), max(rates), (max(rates) - min(rates)) / self$exp_rate_length)
        tags <- rep(NA, self$exp_rate_length)
        for (i in 1:self$exp_rate_length) {
          tags[i] <- paste0(breaks[i], " - ", breaks[i + 1])
        }
        bin_df$strat_f <- cut(bin_df$stratrate,
                           breaks = breaks,
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = 1:self$exp_rate_length)
      } else {
        bin_df$strat_f <- NA
      }
      return(bin_df)
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
    }
  )
)
