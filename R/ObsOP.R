#' @title R6 Class for plotting outputs of the OFPE simulation.
#'
#' @description R6 class for generating maps and figures from observed data
#' in an OFPE database. This class can be initialized with arguments required
#' to run the methods for acquiring or making figures, or can be initialized empty. 
#' When an empty 'ObsOP' class is initialized, the methods must have all arguments
#' passed each time, whereas if the user initializes the 'ObsOP' class with arguments
#' specifying the data selections, the methods can be executed with minimal other arguments.
#' This is meant to simplify use and allow the user to have multiple ObsOP classes for 
#' different datasets.
#'
#' It is recommended to initialize one 'ObsOP' class for each dataset desired for
#' analysis by passing in the specifications for data in the instantiation of the class. 
#' Multiple fields will be treated as one, and only one year is allowed
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
#' can be run individually. TODO: correlation/CV maps, summary statistics
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
    #' @field agg_dat Aggregated data saved as a result of the fetchAggDat() method.
    agg_dat = NULL,
    #' @field raw_dat Raw data saved as a result of the fetchRawDat() method.
    raw_dat = NULL,

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
    },
    ## Data Gather and Manipulation
    #' @description Gather data from an aggregated table in an
    #' OFPE database. Provide the farmername, fieldname(s), year(s),
    #' and type of data to gather. Requires database connection.
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
    #' @param store Logical, whether to save the data internally in the class.
    #' Default is TRUE. Access via rxClass$agg_dat. If FALSE will return dataset.
    #' @return Data table with specified data.
    fetchAggDat = function(db = self$db,
                           farmername = self$farmername,
                           fieldname = self$fieldname,
                           year = self$year,
                           dat_tab = self$dat_tab,
                           GRID = self$GRID,
                           dat_used = self$dat_used,
                           store = TRUE) {
      stopifnot(!is.null(self$db), 
                length(year) == 1,
                length(dat_tab) == 1,
                length(GRID) == 1,
                length(dat_used) == 1)
      dat <- private$.fetchDat(GRID,
                               dat_tab,
                               year,
                               fieldname,
                               db,
                               farmername,
                               dat_used)
      if (store) {
        self$agg_dat <- dat
      } else {
        return(dat)
      }
    },
    #' @description Gather data from a raw table in an
    #' OFPE database. Provide the farmername, the type of data, and the
    #' name of the original file for the data ('orig_file'). Requires database
    #' connection.
    #' @param db Connection to a database to gather data from. Must be
    #' set up and filled in the OFPE specific format.
    #' @param farmername The name of the farmer, corresponding to the format in the
    #' database, e.g. lowercase.
    #' @param dat_tab The table that the data is in within the raw schema of
    #' the database. This corresponds to the type of data gathered (i.e. 'yld',
    #' 'aa_n_poly', 'aa_sr', etc.).
    #' @param orig_file The original file name of the data to gather.
    #' @param store Logical, whether to save the data internally in the class.
    #' Default is TRUE. Access via rxClass$raw_dat. If FALSE will return dataset.
    #' @return Data table with specified data.
    fetchRawDat = function(db = self$db,
                           farmername = self$farmername,
                           dat_tab = self$dat_tab,
                           orig_file,
                           store = TRUE) {
      stopifnot(!is.null(self$db), 
                length(farmername) == 1,
                length(dat_tab) == 1,
                length(orig_file) == 1)
      dat <- DBI::dbGetQuery(
        db,
        paste0("SELECT * FROM ",
               farmername, "_r.", dat_tab,
               " WHERE orig_file = '", orig_file, "'")
      )
      if (store) {
        self$raw_dat <- dat
      } else {
        return(dat)
      }
    },
    #' @description Interpolate data using kriging. Takes two datasets and the
    #' specified column name in the source data for the variable to be kriged
    #' to the the target data.
    #' @param source_dat The data to interpolate from.
    #' @param target_dat The data to krige data to.
    #' @param var The variable to interpolate.
    #' @return Data table with interpolated data.
    krigeDat = function(source_dat, target_dat, var) {
      stopifnot(is.data.frame(source_dat) | data.table::is.data.table(source_dat),
                is.data.frame(target_dat) | data.table::is.data.table(target_dat),
                any(grepl("x|y", names(source_dat))),
                any(grepl("x|y", names(target_dat))))
      source_dat <- as.data.frame(source_dat)
      source_dat <- source_dat[!is.na(source_dat[, var]), ] %>%
        data.table::as.data.table()
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
    #' @description Method for calculating net-return in a dataset. This method
    #' looks for columns specified as 'yld' or 'pro' as well as 'aa_X' specifying
    #' the experimental input variable. The user also specifies the economic
    #' conditions used to calculate net-return. If there is no protein data in
    #' the dataset, the protein premium/dockage specifications are not required.
    #' @param dat The data frame or data table.
    #' @param yld_col_name Character, name of column containing yield data. Leave
    #' NULL to calculate net-return based on just protein. If NULL, pro_col_name
    #' cannot also be NULL.
    #' @param pro_col_name Character, name of column containing protein data. Leave
    #' NULL to calculate net-return based on just yield. If NULL, yld_col_name
    #' cannot also be NULL.
    #' @param exp_col_name Character, name of the column for the experimental input
    #' data. Required.
    #' @param CEXP The cost of the experimental input.
    #' @param FC Fixed costs ($/acre) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    #' @param ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    #' @param Bp The base price corresponding to the price for the system
    #' type selected by the user (i.e. conventional or organic).
    #' @param B0pd The intercept for the protein premium/dockage equation.
    #' @param B1pd The coefficient for protein in the protein premium/dockage
    #' equation.
    #' @param B2pd The coefficient for protein squared for the protein
    #' premium/dockage equation.
    #' @return Data frame with new 'NR' column.
    calcNR = function(dat,
                      yld_col_name = NULL,
                      pro_col_name = NULL,
                      exp_col_name,
                      CEXP,
                      FC,
                      ssAC,
                      Bp,
                      B0pd = NULL,
                      B1pd = NULL,
                      B2pd = NULL) {
      dat <- as.data.frame(dat)
      stopifnot(!is.null(dat),
                is.data.frame(dat) | data.table::is.data.table(dat),
                !is.null(yld_col_name) | !is.null(pro_col_name),
                !is.null(CEXP),
                !is.null(FC),
                !is.null(ssAC),
                !is.null(Bp))
      if (!is.null(yld_col_name)) {
        yld_col <- grep(paste0("^", yld_col_name, "$"), names(dat))
      }
      if (!is.null(pro_col_name)) {
        pro_col <- grep(paste0("^", pro_col_name, "$"), names(dat))
      }
      exp_col <- grep(paste0("^", exp_col_name, "$"), names(dat))

      if (!is.null(pro_col_name)) {
        P <- Bp + (B0pd + B1pd * dat[, pro_col] + B2pd * dat[, pro_col]^2)
        dat$NR <- (dat[, yld_col] * P) - (CEXP * dat[, exp_col]) - FC - ssAC
      } else {
        dat$NR <- (dat[, yld_col] * Bp) - (CEXP * dat[, exp_col]) - FC - ssAC
      }
      return(dat)
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
    #' @param db Connection to the OFPE database to identify UTM zone. Optional, will try
    #' and calculate without. 
    #' @param utm_fieldname Name of the field for identifying the UTM zone.
    #' @return A 'ggmap' object and maps saved in the specified output folder if selected.
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
                           db = self$dbCon$db,
                           utm_fieldname = self$utm_fieldname) {
      stopifnot(length(year) == 1)
      if (is.null(var_main_label)) {
        var_main_label <- paste0("Observed ", year," ", var)
      }
      
      if (is.null(self$dbCon$db) | is.null(db)) {
        if (!any(grepl("sf|sp", class(dat)))) {
          dat_sf <- sf::st_as_sf(dat, coords = c("x", "y"), crs = 4326)
          cords <- sf::st_coordinates(dat_sf) %>%
            as.data.frame() %>% 
            `names<-`(c("x", "y"))
          dat_sf <- cbind(dat_sf, cords)
        } else {
          dat_sf <- sf::st_transform(dat, 4326)
        }
        utm_zone <- OFPE::calcUTMzone(dat_sf)
        rm(dat_sf)
      } else {
        utm_zone <- OFPE::findUTMzone(db, farmername, fieldname)
      }
      
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
    #' @description
    #' This method is for creating a scatterplot of variables specified by the
    #' user. The user specifies the x and y column to plot and a variable or
    #' variables to color points by.
    #' @param dat Data frame with variables to plot. Must include the columns for
    #' specified data.
    #' @param x_var The column name of the variable to plot on the x axis.
    #' @param y_var The column name of the variable to plot on the y axis.
    #' @param x_lab The label to be applied to the x axis of the plot.
    #' @param y_lab The label to be applied to the y axis of the plot.
    #' @param color_var The variable or variables, passed in as a character string,
    #' to color the data by. If left NULL no coloring is applied.
    #' @param color_lab The label to be applied to the legend for the fill color.
    #' @param main_label Title for the figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param save_label The label to apply to the filename. The y, x, and color
    #' variables will also be added to the filename. Optional, but if SAVE = TRUE
    #' and save_lable = NULL 'main_label' will be used.
    #' @param SAVE Logical, whether to save figure.
    #' @return A scatterplot and saved in the specified output folder if selected.
    plotScatters = function(dat,
                            x_var,
                            y_var,
                            x_lab = NULL,
                            y_lab = NULL,
                            color_var = NULL,
                            color_lab = NULL,
                            main_label,
                            out_path = self$out_path,
                            save_label = NULL,
                            SAVE = self$SAVE) {
      ## takes two vars and plots
      dat <- as.data.frame(dat)
      stopifnot(!is.null(dat),
                !is.null(x_var) & is.character(x_var),
                !is.null(y_var) & is.character(y_var),
                is.data.frame(dat) | data.table::is.data.table(dat))
      if (!is.null(out_path)) {
        stopifnot(!is.null(save_label),
                  !is.null(SAVE))
      }
      if (!is.null(color_var)) {
        stopifnot(is.character(color_var))
        for (i in 1:length(color_var)) {
          col_f <- grep(paste0("^", color_var[i], "$"), names(dat))
          if (i == 1) {
            dat$Factor <- dat[, col_f]
          } else {
            dat$Factor <- paste0(dat$Factor, ".", dat[, col_f])
          }
        }
        dat$Factor <- factor(dat$Factor)
      }
      x_col <- grep(paste0("^", x_var, "$"), names(dat))
      y_col <- grep(paste0("^", y_var, "$"), names(dat))
      if (!is.numeric(dat[, x_col])) {
        dat[, x_col] <- as.numeric(dat[, x_col])
      }
      if (!is.numeric(dat[, y_col])) {
        dat[, y_col] <- as.numeric(dat[, y_col])
      }
      x_round_to <- ifelse(max(dat[, x_col], na.rm = T) -
                             min(dat[, x_col], na.rm = T) > 5, 5, 1)
      y_round_to <- ifelse(max(dat[, y_col], na.rm = T) -
                             min(dat[, y_col], na.rm = T) > 5, 5, 1)
      xMIN <- DescTools::RoundTo(min(dat[, x_col], na.rm = T), x_round_to, floor)
      xMAX <- DescTools::RoundTo(max(dat[, x_col], na.rm = T), x_round_to, ceiling)
      xSTEP <- (DescTools::RoundTo(max(dat[, x_col], na.rm = T), x_round_to, ceiling) -
                  DescTools::RoundTo(min(dat[, x_col], na.rm = T), x_round_to, floor)) / 10
      yMIN <- DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)
      yMAX <- DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling)
      ySTEP <- (DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling) -
                  DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)) / 10

      if (is.null(color_var)) {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col])) +
          ggplot2::geom_point() +
          ggplot2::labs(x = x_lab, y = y_lab) 
      } else {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col])) +
          ggplot2::geom_point(ggplot2::aes(col = Factor)) +
          ggplot2::labs(x = x_lab, y = y_lab, col = color_lab) 
      }
      p <- p +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                           breaks = seq(yMIN, yMAX, ySTEP),
                           labels = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX),
                           breaks = seq(xMIN, xMAX, xSTEP),
                           labels = seq(xMIN, xMAX, xSTEP))  +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(main_label)
      
      if (is.null(color_var) & SAVE) {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_scatter.png")
      } else {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_vs_", 
                           color_var,  "_scatter.png")
      }
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = filename,
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      return(p)
    },
    #' @description
    #' This method is for creating a boxplot of variables specified by the
    #' user. The user specifies the x and y column to plot and a variable or
    #' variables to color points by.
    #' @param dat Data frame with variables to plot. Must include the columns for
    #' specified data.
    #' @param x_var The column name of the variable to plot on the x axis.
    #' @param y_var The column name of the variable to plot on the y axis.
    #' @param x_lab The label to be applied to the x axis of the plot.
    #' @param y_lab The label to be applied to the y axis of the plot.
    #' @param color_var The variable or variables, passed in as a character string,
    #' to color the data by. If left NULL no coloring is applied.
    #' @param color_lab The label to be applied to the legend for the fill color.
    #' @param main_label Title for the figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param save_label The label to apply to the filename. The y, x, and color
    #' variables will also be added to the filename. Optional, but if SAVE = TRUE
    #' and save_lable = NULL 'main_label' will be used.
    #' @param SAVE Logical, whether to save figure.
    #' @return A boxplot and saved in the specified output folder if selected.
    plotBoxplots = function(dat,
                            x_var,
                            y_var,
                            x_lab = NULL,
                            y_lab = NULL,
                            color_var = NULL,
                            color_lab = NULL,
                            main_label,
                            out_path = self$out_path,
                            save_label = NULL,
                            SAVE = self$SAVE) {
      ## takes two vars and plots
      dat <- as.data.frame(dat)
      stopifnot(!is.null(dat),
                !is.null(x_var) & is.character(x_var),
                !is.null(y_var),
                is.data.frame(dat) | data.table::is.data.table(dat))
      if (!is.null(out_path)) {
        stopifnot(!is.null(save_label),
                  !is.null(SAVE))
      }
      
      if (!is.null(color_var)) {
        stopifnot(is.character(color_var))
        col_f <- grep(paste0("^", color_var, "$"), names(dat))
        dat$Factor <- dat[, col_f]
      }
      if (length(levels(dat$Factor)) == 3) {
        color <- RColorBrewer::brewer.pal(3, "RdYlGn")
      } else {
        if (length(levels(dat$Factor)) == 2) {
          color <- c("#F8766D", "#00BFC4")
        } else {
          color <- randomcoloR::randomColor(length(levels(dat$Factor)))
        }
      }
      
      x_col <- grep(paste0("^", x_var, "$"), names(dat))
      if (!is.factor(dat[, x_col])) {
        dat[, x_col] <- factor(dat[, x_col])
      }
      y_col <- grep(paste0("^", y_var, "$"), names(dat))
      if (!is.numeric(dat[, y_col])) {
        dat[, y_col] <- as.numeric(dat[, y_col])
      }
      y_round_to <- ifelse(max(dat[, y_col], na.rm = T) -
                             min(dat[, y_col], na.rm = T) > 5, 5, 1)
      yMIN <- DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)
      yMAX <- DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling)
      ySTEP <- (DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling) -
                  DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)) / 10
      ## remove NA from factors
      # dat$Factor <- factor(dat$factor)
      # dat[, x_col] <- factor(dat[, x_col])
      dat <- dat[!is.na(dat[, x_col]), ]
      
      if (is.null(color_var)) {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col])) +
          ggplot2::geom_boxplot(na.rm = TRUE) +
          ggplot2::labs(x = x_lab, y = y_lab) 
      } else {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col])) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Factor), na.rm = TRUE) +
          ggplot2::scale_fill_manual(values = color) +
          ggplot2::labs(x = x_lab, y = y_lab, fill = color_lab) 
      }
      p <- p +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP),
                                    labels = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::theme_bw()+
        ggplot2::ggtitle(main_label)
      
      if (is.null(color_var) & SAVE) {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_boxplot.png")
      } else {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_vs_", 
                           color_var,  "_boxplot.png")
      }
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = filename,
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      return(p)
    },
    #' @description
    #' This method is for creating a violin plot of variables specified by the
    #' user. The user specifies the x and y column to plot and a variable or
    #' variables to color points by. The violin plot shows the probability 
    #' density of observations, and also contains a boxplot within the violin 
    #' to show the IQR. 
    #' @param dat Data frame with variables to plot. Must include the columns for
    #' specified data.
    #' @param x_var The column name of the variable to plot on the x axis.
    #' @param y_var The column name of the variable to plot on the y axis.
    #' @param x_lab The label to be applied to the x axis of the plot.
    #' @param y_lab The label to be applied to the y axis of the plot.
    #' @param color_var The variable or variables, passed in as a character string,
    #' to color the data by. If left NULL no coloring is applied.
    #' @param color_lab The label to be applied to the legend for the fill color.
    #' @param main_label Title for the figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param save_label The label to apply to the filename. The y, x, and color
    #' variables will also be added to the filename. Optional, but if SAVE = TRUE
    #' and save_lable = NULL 'main_label' will be used.
    #' @param SAVE Logical, whether to save figure.
    #' @return A violin plot and saved in the specified output folder if selected.
    plotViolins = function(dat,
                            x_var,
                            y_var,
                            x_lab = NULL,
                            y_lab = NULL,
                            color_var = NULL,
                           color_lab = NULL,
                            main_label,
                            out_path = self$out_path,
                            save_label = NULL,
                            SAVE = self$SAVE) {
      ## takes two vars and plots
      dat <- as.data.frame(dat)
      stopifnot(!is.null(dat),
                !is.null(x_var) & is.character(x_var),
                !is.null(y_var),
                is.data.frame(dat) | data.table::is.data.table(dat))
      if (!is.null(out_path)) {
        stopifnot(!is.null(save_label),
                  !is.null(SAVE))
      }
      
      if (!is.null(color_var)) {
        stopifnot(is.character(color_var))
        col_f <- grep(paste0("^", color_var, "$"), names(dat))
        dat$Factor <- dat[, col_f]
      }
      if (length(levels(dat$Factor)) == 3) {
        color <- RColorBrewer::brewer.pal(3, "RdYlGn")
      } else {
        if (length(levels(dat$Factor)) == 2) {
          color <- c("#F8766D", "#00BFC4")
        } else {
          color <- RColorBrewer::brewer.pal(length(levels(dat$Factor)), "Paired")
            # randomcoloR::randomColor(length(levels(dat$Factor)))
        }
      }
      
      x_col <- grep(paste0("^", x_var, "$"), names(dat))
      if (!is.factor(dat[, x_col])) {
        dat[, x_col] <- factor(dat[, x_col])
      }
      y_col <- grep(paste0("^", y_var, "$"), names(dat))
      if (!is.numeric(dat[, y_col])) {
        dat[, y_col] <- as.numeric(dat[, y_col])
      }
      y_round_to <- ifelse(max(dat[, y_col], na.rm = T) -
                             min(dat[, y_col], na.rm = T) > 5, 5, 1)
      yMIN <- DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)
      yMAX <- DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling)
      ySTEP <- (DescTools::RoundTo(max(dat[, y_col], na.rm = T), y_round_to, ceiling) -
                  DescTools::RoundTo(min(dat[, y_col], na.rm = T), y_round_to, floor)) / 10
      ## remove NA from factors
      # dat$Factor <- factor(dat$factor)
      # dat[, x_col] <- factor(dat[, x_col])
      dat <- dat[!is.na(dat[, x_col]), ]
      
      if (is.null(color_var)) {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col])) +
          ggplot2::geom_violin(na.rm = TRUE) +
          ggplot2::geom_boxplot(width = 0.1, na.rm = TRUE) + 
          ggplot2::labs(x = x_lab, y = y_lab)
      } else {
        p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col], y = dat[, y_col], fill = Factor)) +
          ggplot2::geom_violin(position = ggplot2::position_dodge(0.65), 
                               na.rm = TRUE) +
          ggplot2::geom_boxplot(width = 0.1,
                       position = ggplot2::position_dodge(0.65)) +
          ggplot2::scale_fill_manual(values = color) +
          ggplot2::labs(x = x_lab, y = y_lab, fill = color_lab)
      }
      p <- p +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP),
                                    labels = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(main_label)
      
      if (is.null(color_var) & SAVE) {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_violin.png")
      } else {
        filename <- paste0(out_path,
                           save_label, "_",
                           y_var, "_vs_", x_var, "_vs_", 
                           color_var,  "_violin.png")
      }
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = filename,
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      return(p)
    },
    #' @description
    #' This method is for creating a histogram of a variable specified by the
    #' user. The user specifies the column in the data holding the data of interest
    #' and provides a lavel.
    #' @param dat Data frame with variables to plot. Must include the columns for
    #' specified data.
    #' @param x_var The column name of the variable to plot on the x axis.
    #' @param x_lab The label to be applied to the x axis of the plot.
    #' @param main_label Title for the figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @param save_label The label to apply to the filename. The y, x, and color
    #' variables will also be added to the filename. Optional, but if SAVE = TRUE
    #' and save_lable = NULL 'main_label' will be used.
    #' @param SAVE Logical, whether to save figure.
    #' @return A scatterplot and saved in 'Outputs/Maps' folder if selected.
    plotHistogram = function(dat,
                             x_var,
                             x_lab = NULL,
                             main_label,
                             out_path = self$out_path,
                             save_label = NULL,
                             SAVE = self$SAVE) {
      ## make histogram of specified var
      dat <- as.data.frame(dat)
      stopifnot(!is.null(dat),
                !is.null(x_var) & is.character(x_var),
                is.data.frame(dat) | data.table::is.data.table(dat))
      if (!is.null(out_path)) {
        stopifnot(!is.null(save_label),
                  !is.null(SAVE))
      }
      x_col <- grep(paste0("^", x_var, "$"), names(dat))
      if (!is.numeric(dat[, x_col])) {
        dat[, x_col] <- as.numeric(dat[, x_col])
      }
      x_round_to <- ifelse(max(dat[, x_col], na.rm = T) -
                             min(dat[, x_col], na.rm = T) > 5, 5, 1)
      xMIN <- DescTools::RoundTo(min(dat[, x_col], na.rm = T), x_round_to, floor)
      xMAX <- DescTools::RoundTo(max(dat[, x_col], na.rm = T), x_round_to, ceiling)
      xSTEP <- (DescTools::RoundTo(max(dat[, x_col], na.rm = T), x_round_to, ceiling) -
                  DescTools::RoundTo(min(dat[, x_col], na.rm = T), x_round_to, floor)) / 10
      bin_width <- (max(dat[, x_col], na.rm = T) - min(dat[, x_col], na.rm = T)) * 0.05
      p <- ggplot2::ggplot(dat, ggplot2::aes(x = dat[, x_col])) +
        ggplot2::geom_histogram(stat = "bin", binwidth = bin_width, na.rm = TRUE,
                                fill = "grey70", color = "grey30") +
        ggplot2::labs(x = x_lab, y = "Frequency") +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX),
                                    breaks = seq(xMIN, xMAX, xSTEP),
                                    labels = seq(xMIN, xMAX, xSTEP))  +
        ggplot2::theme_bw()
      y_vec <- ggplot2::layer_data(p, 1)$count
      y_round_to <- ifelse(max(y_vec, na.rm = T) -
                             min(y_vec, na.rm = T) > 5, 5, 1)
      yMIN <- DescTools::RoundTo(min(y_vec, na.rm = T), y_round_to, floor)
      yMAX <- DescTools::RoundTo(max(y_vec, na.rm = T), y_round_to, ceiling)
      ySTEP <- (DescTools::RoundTo(max(y_vec, na.rm = T), y_round_to, ceiling) -
                  DescTools::RoundTo(min(y_vec, na.rm = T), y_round_to, floor)) / 10
      p <- p + ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                                           breaks = seq(yMIN, yMAX, ySTEP),
                                           labels = seq(yMIN, yMAX, ySTEP)) + 
        ggplot2::ggtitle(main_label)
      
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = paste0(out_path,
                        save_label, "_",
                        x_var, "_histogram.png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      return(p)
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
      OFPE::removeTempFarmerTables(db, farmername)
      invisible(
        DBI::dbSendQuery(
          db,
          paste0(
            "CREATE TABLE ", farmername,"_a.temp AS (SELECT *
            FROM ", farmername, "_a.", respvar, " ", respvar,"
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
            farmername, "_a.temp
            DROP COLUMN geometry;"
          )
        )
      )
      db_dat <- invisible(
        DBI::dbGetQuery(
          db,
          paste0("SELECT * FROM ", farmername, "_a.temp;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          db,
          paste0(
            "DROP TABLE ", farmername, "_a.temp;"
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




