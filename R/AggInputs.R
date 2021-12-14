#' @title R6 Class for storing inputs to the 'AggDat' class
#'
#' @description R6 class for for storing information needed for the 'AggDat'
#' class that executes the aggregation methods. These include the field and year
#' to aggregate data for, the locations to aggregate data to, the response variable
#' to use, the experimental variable, the length of year to gather for, etc.
#'
#' Inputs can be supplied directly to this class during instantiation, however
#' this is NOT recommended except for advanced users. It is recommended that the
#' user supplies the database connection and uses the interactive selection
#' methods to select user inputs.
#'
#' This class is passed to the 'AggDat' class that executes the methods for
#' aggregating data and storing in the database. Most methods are executed
#' in the database.
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{AggDat}} for the class responsible for aggregating on-farm data,
#' \code{\link{AggGEE}} for the class responsible for aggregating Google Earth
#' Engine data.
#' @export
AggInputs <- R6::R6Class(
  "AggInputs",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field boundary_import Yes/No, will user be uploading their own field boundary?
    #' Used for spatially querying the database for intersecting data.
    boundary_import = "No",
    #' @field boundary_location Only relevant if boundary_import == "Yes". Is the location
    #' of the shapefile containing the field boundary to use for spatial queries.
    boundary_location = NULL,
    #' @field fieldname Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    fieldname = NULL,
    #' @field farmername Name of the farmer that owns the selected field.
    farmername = NULL,
    #' @field respvar Response variable to aggregate data for, select/input
    #' 'Yield', 'Protein', 'Satellite'. 'Satellite' data aggregates only
    #' remotely sensed data and does not include any on-farm collected data.
    #' This option is important because the user needs to aggregate 'Satellite'
    #' data for years that they would like to simulate management outcomes in
    #' during the analysis and simulation step of the OFPE data cycle.
    respvar = NULL,
    #' @field expvar Experimental variable to aggregate data for. Select or supply
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    expvar = NULL,
    #' @field cy_resp The year of interest of the selected response variable.
    #' This is considered the 'current year' (CY), and differs from the 'previous
    #' year' (PY), which is the latest year before the CY during which the field
    #' was cropped. This is separate from the cy_exp variable because in some
    #' instances, applied data was categorized as or applied in the year prior
    #' (i.e. WW seeding rates occur in fall of the year before harvest in the
    #' following August).
    cy_resp = NULL,
    #' @field py_resp The year prior to the selected CY that a crop was harvested
    #' in the specified field. This is the latest year before the CY during which
    #' the field was cropped. If you do not data from any previous year, you can
    #' provide a year for labeling and annotations sake in output figures.
    py_resp = NULL,
    #' @field cy_exp The year of interest of the selected experimental variable.
    #' This is the year that the experimental variable was applied to grow the
    #' crop in the selected 'cy_resp' year. This is separate from the cy_resp
    #' variable because in some instances, applied data was categorized as or
    #' applied in the year prior (i.e. WW seeding rates occur in fall of the year
    #' before harvest in the following August).
    cy_exp = NULL,
    #' @field py_exp The application year of the experimental variable that was
    #' used to grow the crop in the 'py_resp' year. If you do not data from any
    #' previous year, you can provide a year for labeling and annotations sake in
    #' output figures.
    py_exp = NULL,
    #' @field GRID Determines the location of the aggregated data. Either select
    #' 'Grid' or 'Observed'. 'Grid' aggregates data to the centroids of a 10m grid
    #' laid across the field, while 'Observed' aggregates data to the locations
    #' of the observed response variable (yield or protein). Note that when
    #' 'Satellite' is selected there are no observed points to use, so the 'Grid'
    #' option is selected by default.
    GRID = NULL,
    #' @field dat_used Option for the length of year to use for data. Either select
    #' 'Decision Point' or 'Full Year'. In winter wheat conventional systems and
    #' organic spring wheat systems, there is a decision point around March 30th at
    #' which farmers must make decisions on their fertilizer or seeding rates,
    #' respectively. When making these decisions, farmers will only have data up to
    #' that decision point. This options determines the covariate data aggregated
    #' for the CY, where 'Decision Point' aggregates data for the CY from January
    #' 1st to March 30th of that year, while 'Full Year' aggregates data for the CY
    #' from January 1st to December 31st. This is a residual special use case for
    #' Paul Hegedus' 2020 ICPA work.
    dat_used = NULL,
    #' @field cy_resp_files Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'cy_resp' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.).
    cy_resp_files = NULL,
    #' @field py_resp_files Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'py_resp' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.). If you do
    #' not have data available from a previous year and are not using the
    #' interactive input selectors, simply input "None", otherwise when using the
    #' interactive methods this is handled for you.
    py_resp_files = NULL,
    #' @field cy_exp_files Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'cy_exp' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e.
    #' if application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons.
    cy_exp_files = NULL,
    #' @field py_exp_files Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'py_exp' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e. if
    #' application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons. If no data from the desired year
    #' is available, put "None" in the 'orig_file' column.
    py_exp_files = NULL,
    #' @field save_in_db Yes/No. Logical, whether to save the aggregated data into
    #' the OFPE formatted database. Not an option if you are supplying your own
    #' boundary for which to aggregate data.
    save_in_db = NULL,
    #' @field export Yes/No. Logical, whether to export the aggregated data as
    #' a '.csv' file. If yes, the user will need to provide the 'export_name'.
    export = NULL,
    #' @field export_name If exporting the aggregated data as a '.csv', the user
    #' needs to specify the name of the file to export. This includes the file
    #' path.
    export_name = NULL,
    #' @field cy_resp_col Data.frame with 3 columns, 'resp', 'dist', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('resp') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'orig_file' is the same as those selected in the
    #' 'cy_resp_files' vector.
    cy_resp_col = NULL,
    #' @field py_resp_col Data.frame with 3 columns, 'resp', 'dist', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('resp') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional and optionally used during a cleaning step where
    #' points are removed if they are more than 4SD from the mean distance
    #' between observations as this indicates that the equipment was moving
    #' at an abnormal speed and potentially resulting in erroneous measurements.
    #' The 'orig_file' is the same as those selected in the 'py_resp_files' vector.
    py_resp_col = NULL,
    #' @field cy_exp_col Data.frame with 4 columns, 'EXP', 'dist', 'product',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'product' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    cy_exp_col = NULL,
    #' @field py_exp_col Data.frame with 4 columns, 'EXP', 'dist', 'product',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'product' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'py_exp_files' table.
    py_exp_col = NULL,
    #' @field cy_exp_conv Data.frame with 3 columns, 'FORMULA', 'conversion',
    #' and 'orig_file'. Based on the users selection of 'product' in the
    #' cy_exp_col data.frame, the formula from the 'product' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilizer
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    cy_exp_conv = NULL,
    #' @field py_exp_conv Data.frame with 3 columns, 'FORMULA', 'conversion',
    #' and 'orig_file'. Based on the users selection of 'product' in the
    #' cy_exp_col data.frame, the formula from the 'product' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilizer
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    py_exp_conv = NULL,
    #' @field size Optional, the size, in meters, to make a grid across the field.
    #' Mostly necessary for when 'Grid' is selected for the 'GRID' parameter, however
    #' is the scale at which the finest resolution cleaning of data occurs. Defaults
    #' to 10m if left NULL.
    size = 10,

    #' @description
    #' Initialize an object for storing aggregation inputs.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param boundary_import Yes/No, will user be uploading their own field boundary?
    #' Used for spatially querying the database for intersecting data.
    #' @param boundary_location Only relevant if boundary_import == "Yes". Is the location
    #' of the shapefile containing the field boundary to use for spatial queries.
    #' @param fieldname Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    #' @param farmername Name of the farmer that owns the selected field.
    #' @param respvar Response variable to aggregate data for, select/input
    #' 'Yield', 'Protein', 'Satellite'. 'Satellite' data aggregates only
    #' remotely sensed data and does not include any on-farm collected data.
    #' @param expvar Experimental variable to aggregate data for, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param cy_resp The year of interest of the selected response variable.
    #' This is considered the 'current year' (CY), and differs from the 'previous
    #' year' (PY), which is the latest year before the CY during which the field
    #' was cropped. This is separate from the cy_exp variable because in some
    #' instances, applied data was categorized as or applied in the year prior
    #' (i.e. WW seeding rates occur in fall of the year before harvest in the
    #' following August).
    #' @param py_resp The year prior to the selected CY that a crop was harvested
    #' in the specified field. This is the latest year before the CY during which
    #' the field was cropped. If you do not data from any previous year, you can
    #' provide a year for labeling and annotations sake in output figures.
    #' @param cy_exp The year of interest of the selected experimental variable.
    #' This is the year that the experimental variable was applied to grow the
    #' crop in the selected 'cy_resp' year. This is separate from the cy_resp
    #' variable because in some instances, applied data was categorized as or
    #' applied in the year prior (i.e. WW seeding rates occur in fall of the year
    #' before harvest in the following August).
    #' @param py_exp The application year of the experimental variable that was
    #' used to grow the crop in the 'py_resp' year. If you do not data from any
    #' previous year, you can provide a year for labeling and annotations sake in
    #' output figures.
    #' @param GRID Determines the location of the aggregated data. Either select
    #' 'Grid' or 'Observed'. 'Grid' aggregates data to the centroids of a 10m grid
    #' laid across the field, while 'Observed' aggregates data to the locations
    #' of the observed response variable (yield or protein). Note that when
    #' 'Satellite' is selected there are no observed points to use, so the 'Grid'
    #' option is selected by default.
    #' @param dat_used Option for the length of year to use for CY data. In
    #' winter wheat conventional systems and organic spring wheat systems,
    #' there is a decision point around March 30th at which farmers must make
    #' decisions on their fertilizer or seeding rates, respectively. When making
    #' these decisions, farmers will only have data up to that decision point. This
    #' options determines the covariate data aggregated for the CY, where
    #' 'Decision Point' aggregates data for the CY from January 1st to March 30th
    #' of that year, while 'Full Year' aggregates data for the CY from January 1st
    #' to December 31st. This is a residual special use case for Paul Hegedus' 2020
    #' ICPA work.
    #' @param cy_resp_files Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'cy_resp' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.).
    #' @param py_resp_files Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'py_resp' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.). If you do
    #' not have data available from a previous year and are not using the
    #' interactive input selectors, simply input "None", otherwise when using the
    #' interactive methods this is handled for you.
    #' @param cy_exp_files Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'cy_exp' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e.
    #' if application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons.
    #' @param py_exp_files Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'py_exp' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e. if
    #' application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons. If no data from the desired year
    #' is available, put "None" in the 'orig_file' column.
    #' @param save_in_db Yes/No. Logical, whether to save the aggregated data into
    #' the OFPE formatted database. Not an option if you are supplying your own
    #' boundary for which to aggregate data.
    #' @param export Yes/No. Logical, whether to export the aggregated data as
    #' a '.csv' file. If yes, the user will need to provide the 'export_name'.
    #' @param export_name If exporting the aggregated data as a '.csv', the user
    #' needs to specify the name of the file to export. This includes the file
    #' path.
    #' @param cy_resp_col Data.frame with 3 columns, 'resp', 'dist', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('resp') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'orig_file' is the same as those selected in the
    #' 'cy_resp_files' vector.
    #' @param py_resp_col Data.frame with 3 columns, 'resp', 'dist', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('resp') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional and optionally used during a cleaning step where
    #' points are removed if they are more than 4SD from the mean distance
    #' between observations as this indicates that the equipment was moving
    #' at an abnormal speed and potentially resulting in erroneous measurements.
    #' The 'orig_file' is the same as those selected in the 'py_resp_files' vector.
    #' @param cy_exp_col Data.frame with 4 columns, 'EXP', 'dist', 'product',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'product' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    #' @param py_exp_col Data.frame with 4 columns, 'EXP', 'dist', 'product',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('dist'). The 'dist'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'product' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'py_exp_files' table.
    #' @param cy_exp_conv Data.frame with 3 columns, 'FORMULA', 'conversion',
    #' and 'orig_file'. Based on the users selection of 'product' in the
    #' cy_exp_col data.frame, the formula from the 'product' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilizer
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    #' @param py_exp_conv Data.frame with 3 columns, 'FORMULA', 'conversion',
    #' and 'orig_file'. Based on the users selection of 'product' in the
    #' cy_exp_col data.frame, the formula from the 'product' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilizer
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'cy_exp_files' table.
    #' @param size Optional, the size, in meters, to make a grid across the field.
    #' Mostly necessary for when 'Grid' is selected for the 'GRID' parameter, however
    #' is the scale at which the finest resolution cleaning of data occurs. Defaults
    #' to 10m if left NULL.
    #' @return A new 'AggInputs' object.
    initialize = function(dbCon,
                          boundary_import = "No",
                          boundary_location = NULL,
                          fieldname = NULL,
                          farmername = NULL,
                          respvar = NULL,
                          expvar = NULL,
                          cy_resp = NULL,
                          py_resp = NULL,
                          cy_exp = NULL,
                          py_exp = NULL,
                          GRID = NULL,
                          dat_used = NULL,
                          cy_resp_files = NULL,
                          py_resp_files = NULL,
                          cy_exp_files = NULL,
                          py_exp_files = NULL,
                          save_in_db = NULL,
                          export = NULL,
                          export_name = NULL,
                          cy_resp_col = NULL,
                          py_resp_col = NULL,
                          cy_exp_col = NULL,
                          py_exp_col = NULL,
                          cy_exp_conv = NULL,
                          py_exp_conv = NULL,
                          size = 10) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon
      if (!is.null(boundary_import)) {
        stopifnot(
          is.character(boundary_import),
          grepl("Yes|No", boundary_import)
        )
        #self$boundary_import <-  boundary_import
        if (self$boundary_import == "Yes") {
          stopifnot(
            !is.null(boundary_location)
          )
          self$boundary_location <- boundary_location
        }
      }
      if (!is.null(fieldname)) {
        stopifnot(
          is.character(fieldname)
        )
        self$fieldname <- fieldname
      }
      if (!is.null(farmername)) {
        stopifnot(
          is.character(farmername)
        )
        self$farmername <- farmername
      }
      if (!is.null(respvar)) {
        stopifnot(
          is.character(respvar)
        )
        self$respvar <- ifelse(respvar == "Yield",
                               "yld",
                               ifelse(respvar == "Protein",
                                      "pro",
                                      "sat"))
      }
      if (!is.null(expvar)) {
        stopifnot(
          is.character(expvar)
        )
        self$expvar <- ifelse(expvar == "As-Applied Nitrogen",
                               "aa_n",
                               "aa_sr")
      }
      if (!is.null(GRID)) {
        stopifnot(
          is.character(GRID)
        )
        self$GRID <- ifelse(GRID == "Grid",
                              "grid",
                              "obs")
      }
      if (!is.null(dat_used)) {
        stopifnot(
          is.character(dat_used)
        )
        self$dat_used <- ifelse(dat_used == "Decision Point",
                                "decision_point",
                                "full_year")
      }
      if (!is.null(cy_resp)) {
        stopifnot(
          is.numeric(cy_resp)|is.character(cy_resp)
        )
        self$cy_resp <- as.numeric(cy_resp)
      }
      if (!is.null(py_resp)) {
        stopifnot(
          is.numeric(py_resp)|is.character(py_resp)
        )
        self$py_resp <- as.numeric(py_resp)
      }
      if (!is.null(cy_exp)) {
        stopifnot(
          is.numeric(cy_exp)|is.character(cy_exp)
        )
        self$cy_exp <- as.numeric(cy_exp)
      }
      if (!is.null(py_exp)) {
        stopifnot(
          is.numeric(py_exp)|is.character(py_exp)
        )
        self$py_exp <- as.numeric(py_exp)
      }
      if (!is.null(cy_resp_files)) {
        stopifnot(
          is.character(cy_resp_files)
        )
        self$cy_resp_files <- cy_resp_files
      }
      if (!is.null(py_resp_files)) {
        stopifnot(
          is.character(py_resp_files)
        )
        self$py_resp_files <- py_resp_files
      }
      if (!is.null(cy_exp_files)) {
        stopifnot(
          is.data.frame(cy_exp_files),
          ncol(cy_exp_files) == 2,
          any(grepl("orig_file", names(cy_exp_files))),
          any(grepl("table", names(cy_exp_files))),
          !anyNA(cy_exp_files$orig_file)
        )
        self$cy_exp_files <- cy_exp_files
      }
      if (!is.null(py_exp_files)) {
        stopifnot(
          is.data.frame(py_exp_files),
          any(grepl("orig_file", names(cy_exp_files)))
        )
        self$py_exp_files <- py_exp_files
      }
      if (!is.null(save_in_db)) {
        stopifnot(
          is.character(save_in_db),
          grepl("Yes|No", save_in_db)
        )
        self$save_in_db <- save_in_db
      }
      if (!is.null(export)) {
        stopifnot(
          is.character(export),
          grepl("Yes|No", export)
        )
        self$export <- export
        if (self$export == "Yes") {
          stopifnot(
            !is.null(export_name),
            is.character(export_name)
          )
          self$export_name <- export_name
        }
      }
      if (!is.null(cy_resp_col)) {
        stopifnot(
          is.data.frame(cy_resp_col),
          ncol(cy_resp_col) == 3,
          nrow(cy_resp_col) == length(self$cy_resp_files),
          any(grepl("resp", names(cy_resp_col))),
          any(grepl("dist", names(cy_resp_col))),
          any(grepl("orig_file", names(cy_resp_col))),
          !anyNA(cy_resp_col$resp)
        )
        self$cy_resp_col <- cy_resp_col
      }
      if (!is.null(py_resp_col)) {
        stopifnot(
          is.data.frame(py_resp_col),
          ncol(py_resp_col) == 3,
          nrow(py_resp_col) == length(self$py_resp_files),
          any(grepl("resp", names(py_resp_col))),
          any(grepl("dist", names(py_resp_col))),
          any(grepl("orig_file", names(py_resp_col)))
        )
        self$py_resp_col <- py_resp_col
      }
      if (!is.null(cy_exp_col)) {
        stopifnot(
          is.data.frame(cy_exp_col),
          ncol(cy_exp_col) == 4,
          nrow(cy_exp_col) == nrow(self$cy_exp_files),
          any(grepl("EXP", names(cy_exp_col))),
          any(grepl("dist", names(cy_exp_col))),
          any(grepl("product", names(cy_exp_col))),
          any(grepl("orig_file", names(cy_exp_col)))
        )
        self$cy_exp_col <- cy_exp_col
      }
      if (!is.null(py_exp_col)) {
        stopifnot(
          is.data.frame(py_exp_col),
          ncol(py_exp_col) == 4,
          nrow(py_exp_col) == nrow(self$py_exp_col),
          any(grepl("EXP", names(py_exp_col))),
          any(grepl("dist", names(py_exp_col))),
          any(grepl("product", names(py_exp_col))),
          any(grepl("orig_file", names(py_exp_col)))
        )
        self$py_exp_col <- py_exp_col
      }
      if (!is.null(cy_exp_conv)) {
        stopifnot(
          is.data.frame(cy_exp_conv),
          ncol(cy_exp_conv) == 3,
          nrow(cy_exp_conv) == nrow(self$cy_exp_conv),
          any(grepl("FORMULA", names(cy_exp_conv))),
          any(grepl("conversion", names(cy_exp_conv))),
          any(grepl("orig_file", names(cy_exp_conv)))
        )
        self$cy_exp_conv <- cy_exp_conv
      }
      if (!is.null(py_exp_conv)) {
        stopifnot(
          is.data.frame(py_exp_conv),
          ncol(py_exp_conv) == 3,
          nrow(py_exp_conv) == nrow(self$py_exp_conv),
          any(grepl("FORMULA", names(py_exp_conv))),
          any(grepl("conversion", names(py_exp_conv))),
          any(grepl("orig_file", names(py_exp_conv)))
        )
        self$py_exp_conv <- py_exp_conv
      }
      if (!is.null(size)) {
        stopifnot(
          is.numeric(size)
        )
        self$size <- size
      }
    },
    #' @description
    #' Interactive method for selecting aggregation input options. The description
    #' below describes the process of interactively selecting the necessary parameters
    #' for automated data aggregation.
    #'
    #' The user selects whether to import bounding box or select from field in
    #' database. If user uploads their own field boundary they are asked for the
    #' field name and to identify the associated farmer. If using a field boundary
    #' from the database the user simply selects which field.
    #'
    #' Select the response variable to aggregate data for (yield or protein).
    #' Using this information the database is queried for years that data is
    #' available for this field. Also, if the user selected to only collect
    #' satellite data, the user can choose any year from 2000 to present to gather
    #' data from, however the user must have gathered data from that year in the
    #' database. This 'Satellite' data is used in the analysis and simulation step
    #' to simulate management outcomes under a selection of the years for which the
    #' the user aggregated 'Satellite' data. Also select the experimental variable
    #' to aggregate data for (as-applied nitrogen or as-applied seed rate).
    #'
    #' Select a data constraint for determining the time span for which to gather data.
    #' If 'Decision Point' is selected, then data from the current year is gathered up
    #' until 03-31. If 'Full Year' is selected, then data from the current year is
    #' gathered past the decision point and harvest through the entire year to 12-31
    #' of the current selected year.
    #'
    #' Select the variable that was experimentally varied across the field. This
    #' package was developed with options for "As-Applied Nitrogen" and "As-Applied
    #' Seed Rate".
    #'
    #' User needs to select whether to aggregate data to a grid or to use
    #' observed locations. This is required now because if using observed locations
    #' the user can only select one file, whereas with the grid option the user can
    #' select multiple files because data will be averaged to the grid cell centroid
    #' locations. This only applies to yield or protein data because by default
    #' satellite data is aggregated to the grid cell locations.
    #'
    #' Select current year and previous year(s) to aggregate data for. If user imports
    #' their own field boundary it needs to be added to a temporary folder to do PostGIS
    #' functions within the database and not R.
    #'
    #' Using the selected experimental variable the database is queried for years that
    #' data is available for this field. Select current year and previous year(s) to get
    #' experimental data for.
    #'
    #' Get column names from the current year response variable table to identify that
    #' which corresponds to response and the distance column. The distance column is
    #' used to clean the data and will be removed eventually. The distance column is
    #' also not typically present in protein data and can be omitted as it is an
    #' optional argument to the cleaning function. If no response files for the
    #' current year are selected this section will be skipped.
    #'
    #' Also, get column names from the previous year response variable table to identify
    #' that which corresponds to response and the distance column. The distance column
    #' is used to clean the data and will be removed eventually. The distance column
    #' is also not typically present in protein data and can be omitted as it is an
    #' optional argument to the cleaning function. If no previous year response
    #' variable selected this section will be skipped.
    #'
    #' Get column names from the current year experimental data to identify the column
    #' that corresponds to the experimental variable. Get column names from the previous
    #' year experimental data to identify the column that corresponds to the experimental
    #' variable.
    #'
    #' Fill in parameters for export, such as whether to save in the database
    #' or to export as a .csv file. If the user imported their own field boundary
    #' for aggregating data, it will not be saved in the database.
    #'
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A completed 'AggInputs' object.
    selectInputs = function() {
      OFPE::removeTempTables(self$dbCon$db) # removes temporary tables
      private$.selectField(self$dbCon$db)
      OFPE::removeTempFarmerTables(self$dbCon$db, self$farmername)

      private$.selectRespvar()
      private$.selectAggLOY()
      if (self$respvar != "sat") {
        private$.selectExpvar()
        private$.selectAggLocs()
        private$.selectRespFiles(self$dbCon$db)
        private$.selectExpFiles(self$dbCon$db)
        private$.selectRespCols(self$dbCon$db)
        private$.selectExpCols(self$dbCon$db)
      } else {
        self$GRID <- "grid"
      }
      private$.selectExportParms()
    }
  ),

  private = list(
    .selectField = function(db) {
      # self$boundary_import <- as.character(
      #   select.list(
      #     c("Yes", "No"),
      #     title = paste0("Will user be uploading their own field boundary?
      #                    If so, must be in working directory. If the user
      #                    uploaded field boundary will be accessed often,
      #                    the user is encouraged to update the database with
      #                    this boundary to provide for faster queries and long
      #                    term storage of the field boundary."))
      # )
      if (self$boundary_import == "No") {
        ## select field boundary
        self$fieldname <- as.character(
          select.list(
            unique(
              DBI::dbGetQuery(db, "SELECT fieldname FROM all_farms.fields")$fieldname
            ),
            title = "Select field to aggregate data for."
          )
        )
        ## get farmeridx from fieldname
        farmeridx <- DBI::dbGetQuery(
          db,
          paste0("SELECT farmeridx
                 FROM all_farms.fields
                 WHERE fieldname = '",
                 self$fieldname, "'")
        )
        ## fill in farmer and field info
        self$farmername <- as.character(
          DBI::dbGetQuery(db,
            paste0("SELECT farmer
                   FROM all_farms.farmers
                   WHERE farmeridx = '",
                   unique(farmeridx), "'")
          )
        )
        ## copy field boundary to temp file for later queries
        tt <- invisible(
          DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE all_farms.temp AS
                   SELECT * FROM all_farms.fields fields
                   WHERE fields.fieldname = '", self$fieldname, "';")
          )
        )
        DBI::dbClearResult(tt)
        tt <- invisible(
          DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE all_farms.temp
                   RENAME COLUMN geom TO geometry;")
          )
        )
        DBI::dbClearResult(tt)
      } else {
        ## enter filename for bounding box
        self$boundary_location <- readline(
          prompt = "Enter file name of field boundary (without file extension) : "
        )
        ## specify field and farmer name
        self$fieldname <- readline(prompt = "Enter field name : ")
        self$fieldname <- self$fieldname %>%
          OFPE::noSpecialChar() %>%
          tolower()
        self$farmername <- as.character(
          select.list(
            unique(
              DBI::dbGetQuery(db, "SELECT farmer FROM all_farms.farmers")$farmer
            ),
            title = "Select farmer name."
          )
        )
        self$farmername <- self$farmername %>%
          OFPE::noSpecialChar() %>%
          tolower()
      }
    },
    .selectRespvar = function() {
      respVar <- as.character(
        select.list(
          c("Yield", "Protein", "Satellite"),
          title = paste0("Select response variable to aggregate data for. If
                        the user selects 'Satellite', only data collected
                        from remote sensing sources is aggregated to the grid
                        cell centroids that are generated from the selected/
                        imported field boundary.")
        )
      )
      self$respvar <- ifelse(respVar == "Yield", "yld",
                               ifelse(respVar == "Protein", "pro",
                                      "sat"))
      if (self$respvar == "sat") {
        self$cy_resp <- as.character(
          select.list(
            seq(2000, as.integer(format(Sys.Date(), "%Y")), 1),
            title = paste0("Select the 'current' year to aggregate data on.
                           Data for this year is collected from 01-01 to 03-30.")
          )
        )
      }
    },
    .selectExpvar = function() {
      expVar <- as.character(
        select.list(
          c("As-Applied Nitrogen", "As-Applied Seed Rate"),
          title= "Select experimental variable."
        )
      )
      self$expvar <- ifelse(expVar == "As-Applied Nitrogen", "aa_n", "aa_sr")
    },
    .selectAggLocs = function() {
      # if response variable == yld or pro
      gridOrObs <- as.character(
        select.list(
          c("Grid", "Observed"),
          title = paste0("Select whether to aggregate data to the centroids of
                         10m grid cells (Grid) or to aggregate data to the
                         observed data locations (Observed). If the latter is
                         selected the user can only select one data file to
                         aggregate. If the former is selected, the user will be
                         able to use multiple data files because points within
                         grid cells are averaged.")
        )
      )
      self$GRID <- ifelse(gridOrObs == "Grid", "grid", "obs")

      self$size <- readline(prompt = "Enter the size of grid to use for aggregation and/or cleaning (meters): ")
    },
    .selectAggLOY = function() {
      ## Select constraints on data to gather
      data_used <- as.character(
        select.list(
          c("Decision Point",
            "Full Year"),
          title = paste0("Select a data constraint for determining the time
                         span for which to gather data. If 'Decision Point'
                         is selected, then data from the current year is
                         gathered up until 03-31. If 'Full Year' is selected,
                         then data from the current year is gathered past the
                         decision point and harvest through the entire year to
                         12-31 of the current selected year.")
        )
      )
      self$dat_used <- ifelse(data_used == "Decision Point",
                                "decision_point",
                                "full_year")
    },
    .selectRespFiles = function(db) {
      orig_files_resp <- private$.getFiles(self$respvar,
                                           db,
                                           self$farmername,
                                           self$fieldname)
      self$cy_resp <- as.character(
        select.list(
          unique(orig_files_resp$year)[
            order(as.numeric(unique(orig_files_resp$year)))
          ],
          title = "Select the harvest year to aggregate data for."
        )
      )
      orig_filesCY_resp <-
        orig_files_resp[orig_files_resp$year == self$cy_resp, ]
      self$cy_resp_files <- as.character(
        select.list(
          c(orig_filesCY_resp$orig_file, "None"),
          multiple = TRUE, # ifelse(self$GRID == "grid", TRUE, FALSE),
          title = paste0("Select file(s) to aggregate data on. Or select all
                         to gather data for all available files as one. If
                         unsure, it is recommended to visualize extent of each
                         file using the PostgreSQL connection in QGIS.")
        )
      )
      self$py_resp <- as.character(
        select.list(
          c(unique(orig_files_resp$year)[
              order(as.numeric(unique(orig_files_resp$year)))
            ],
            "Other"),
          title = "Select the previous harvest year to get prior data for."
        )
      )
      if (self$py_resp == "Other") {
        self$py_resp <- readline(prompt = "Enter previous harvest year: ")
        self$py_resp_files <- "None"
      } else {
        orig_filesPY_resp <-
          orig_files_resp[orig_files_resp$year == self$py_resp, ]
        self$py_resp_files <- as.character(
          select.list(
            c(orig_filesPY_resp$orig_file, "None"),
            multiple=ifelse(self$GRID == "grid", TRUE, FALSE),
            title = paste0("Select file(s) to use for previous harvest year.
                           Or select all to gather data for all available files
                           as one. If unsure, it is recommended to visualize
                           extent of each file using the PostgreSQL connection
                           in QGIS.")
          )
        )
      }
    },
    .selectExpFiles = function(db) {
      schema_tabs <- private$.tabNames(paste0(self$farmername, "_r"), db)
      schema_tabs <- schema_tabs[grepl(self$expvar, schema_tabs)]

      orig_files_exp <- list()
      orig_files_exp <- lapply(schema_tabs,
                               private$.getFiles,
                               db,
                               self$farmername,
                               self$fieldname)
      orig_files_exp <- do.call(rbind, orig_files_exp)
      ## Current year experimental data
      self$cy_exp <- as.character(
        select.list(
          c(unique(orig_files_exp$year)[
              order(as.numeric(unique(orig_files_exp$year)))
            ],
            "Other"),
          title = "Select the application year for the current harvest year."
        )
      )
      if (self$cy_exp == "Other") {
        self$cy_exp <- readline(prompt="Enter current application year: ")
        self$cy_exp_files <- "None"
        self$cy_exp_files <- data.frame(orig_file = "None")
      } else {
        orig_filesCY_exp <-
          orig_files_exp[orig_files_exp$year == self$cy_exp, ]
        self$cy_exp_files <- as.character(
          select.list(
            c(orig_filesCY_exp$orig_file, "None"),
            multiple = TRUE,
            title = paste0("Select as-applied file(s) from current harvest year to
                          aggregate. If unsure, it is recommended to visualize
                          extent of each file using the PostgreSQL connection in
                          QGIS.")
          )
        )
        ## import exp filenames and table to input object
        self$cy_exp_files <- private$.getExpFilenamesAndTables(
          self$cy_exp_files, orig_filesCY_exp
        )
      }
      ## Previous year experimental data
      self$py_exp <- as.character(
        select.list(
          c(unique(orig_files_exp$year)[
              order(as.numeric(unique(orig_files_exp$year)))
            ],
            "Other"),
          title= "Select the application year for the previous harvest year."
        )
      )
      if (self$py_exp == "Other") {
        self$py_exp <- readline(prompt = "Enter previous application year: ")
        self$py_exp_files <- data.frame(orig_file = "None")
      } else {
        orig_filesPY_exp <- orig_files_exp[orig_files_exp$year == self$py_exp, ]
        self$py_exp_files <- as.character(
          select.list(
            c(orig_filesPY_exp$orig_file, "None"),
            multiple = TRUE,
            title= paste0("Select as-applied file(s) from previous harvest year
                          to aggregate. If unsure, it is recommended to visualize
                          extent of each file using the PostgreSQL connection in
                          QGIS.")
          )
        )
        ## import exp filenames and table to input object
        self$py_exp_files <- private$.getExpFilenamesAndTables(
          self$py_exp_files, orig_filesPY_exp
        )
      }
    },
    .selectRespCols = function(db) {
      ## Current year resp file
      if (!any(self$cy_resp_files == "None")) {
        temp_tab_cols <- private$.getTempRespTableCols(
          self$cy_resp_files,
          db,
          self$respvar,
          self$cy_resp,
          self$farmername,
          self$fieldname
        )
        cy_resp_colnames <- rep(list(NA), length(self$cy_resp_files)) %>%
          `names<-`(self$cy_resp_files)
        cy_resp_colnames <- lapply(temp_tab_cols, colnames)
        self$cy_resp_col <- as.data.frame(
          matrix(NA, length(self$cy_resp_files), 2)) %>%
          `colnames<-`(c("resp", "dist"))
        self$cy_resp_col$orig_file <- self$cy_resp_files
        for (i in 1:length(self$cy_resp_files)) {
          self$cy_resp_col[i, "resp"] <- as.character(
            select.list(
              cy_resp_colnames[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the response
                             variable in the ", self$cy_resp_files[i], " table.")
            )
          )
          self$cy_resp_col[i, "dist"] <- as.character(
            select.list(
              c(cy_resp_colnames[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$cy_resp_files[i], " table that corresponds
                             to the distance between measured points OR select
                             NA if no column present. This is used to remove
                             observations when the combine is moving at irregular
                             speeds.")
            )
          )
        }
      }
      ## Previous year resp colnames
      if (!any(self$py_resp_files == "None")) {
        temp_tab_cols <- private$.getTempRespTableCols(
          self$py_resp_files,
          db,
          self$respvar,
          self$py_resp,
          self$farmername,
          self$fieldname
        )

        py_resp_colnames <- rep(list(NA), length(self$py_resp_files)) %>%
          `names<-`(self$py_resp_files)
        py_resp_colnames <- lapply(temp_tab_cols, colnames)

        self$py_resp_col <- as.data.frame(
          matrix(NA, length(self$py_resp_files), 2)) %>%
          `colnames<-`(c("resp", "dist"))
        self$py_resp_col$orig_file <- self$py_resp_files
        for (i in 1:length(self$py_resp_files)) {
          self$py_resp_col[i, "resp"] <- as.character(
            select.list(
              py_resp_colnames[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the response
                             variable in the ", self$py_resp_files[i], " table
                             from the previous harvest year.")
            )
          )
          self$py_resp_col[i, "dist"] <- as.character(
            select.list(
              c(py_resp_colnames[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$py_resp_files[i], " table that corresponds to
                             the distance between measured points OR select NA if
                             no column present. This is used to remove observations
                             when the combine is moving at irregular speeds.")))
        }
      }
    },
    .selectExpCols = function(db) {
      ## Current year experimental columns
      if (!any(self$cy_exp_files$orig_file == "None")) {
        # make temp table w/necessary data
        temp_tab_cols <- private$.getTempExpTableCols(
          self$cy_exp_files,
          db,
          self$respvar,
          self$cy_exp,
          self$farmername,
          self$fieldname
        )
        cy_exp_colnames <- rep(list(NA), nrow(self$cy_exp_files)) %>%
          `names<-`(self$cy_exp_files$orig_file)
        cy_exp_colnames <- lapply(temp_tab_cols, colnames)

        self$cy_exp_col <- as.data.frame(
          matrix(NA, nrow(self$cy_exp_files), 3)) %>%
          `colnames<-`(c("EXP", "dist", "product"))
        self$cy_exp_col$orig_file <- self$cy_exp_files$orig_file

        self$cy_exp_conv <- as.data.frame(
          matrix(NA, nrow(self$cy_exp_files), 2)) %>%
          `colnames<-`(c("FORMULA", "conversion"))
        self$cy_exp_conv$orig_file <- self$cy_exp_files$orig_file

        for (i in 1:nrow(self$cy_exp_files)) {
          self$cy_exp_col[i, "EXP"] <- as.character(
            select.list(
              cy_exp_colnames[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the
                             experimental variable in the ",
                             self$cy_exp_files$orig_file[i],
                            " table for the selected current harvest year.")
            )
          )
          self$cy_exp_col[i, "dist"] <- as.character(
            select.list(
              c(cy_exp_colnames[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$cy_exp_files$orig_file[i],
                             " table that corresponds to the distance between
                             measured points OR select NA if no column present.
                             This is used to remove observations when the sprayer
                             is moving at irregular speeds.")
            )
          )
          if (self$expvar == "aa_n") {
            self$cy_exp_col[i, "product"] <- as.character(
              select.list(
                c(cy_exp_colnames[[i]], NA),
                multiple = FALSE,
                title = paste0("Select the column name in the ",
                             self$cy_exp_files$orig_file[i],
                             " table that corresponds to the product applied.
                             This is used to determine the conversion from the
                             applied rate to lbs N per acre.")
              )
            )
          } else {
            self$cy_exp_col[i, "product"] <- NA
          }
          if (!is.na(self$cy_exp_col[i, "product"])) {
            tempTable <- private$.getTempExpTable(
              db,
              self$cy_exp_files,
              self$farmername,
              self$cy_exp,
              self$fieldname,
              i
            )
            self$cy_exp_conv$FORMULA[i] <-
              tempTable[1,
                        grep(self$cy_exp_col$product[i],
                             colnames(tempTable))]
            self$cy_exp_conv$conversion[i] <-
              readline(prompt=cat(paste0("The product formula is ",
                                         self$cy_exp_conv$FORMULA[i],
                                         ". Please provide the conversion rate
                                         from the applied rate to lbs N per acre.",
                                         "\n", "If the product is 32% UAN (gal/ac)
                                         the conversion is 3.5, otherwise it is
                                         typically the fractional form of the percent
                                         N from the NPK formula (i.e. 46-18-8 = 46% N
                                         = 0.46).", "\n", "Conversion Rate: ")))
          }
        }
      }
      ## Previous year experimental columns
      if (!any(self$py_exp_files$orig_file == "None")) {
        temp_tab_cols <- private$.getTempExpTableCols(
          self$py_exp_files,
          db,
          self$respvar,
          self$py_exp,
          self$farmername,
          self$fieldname
        )

        py_exp_colnames <- rep(list(NA), nrow(self$py_exp_files)) %>%
          `names<-`(self$py_exp_files$orig_file)
        py_exp_colnames <- lapply(temp_tab_cols, colnames)

        self$py_exp_col <- as.data.frame(
          matrix(NA, nrow(self$py_exp_files), 3)) %>%
          `colnames<-`(c("EXP", "dist", "product"))
        self$py_exp_col$orig_file <- self$py_exp_files$orig_file

        self$py_exp_conv <- as.data.frame(
          matrix(NA, nrow(self$py_exp_files), 2)) %>%
          `colnames<-`(c("FORMULA", "conversion"))
        self$py_exp_conv$orig_file <- self$py_exp_files$orig_file

        for (i in 1:nrow(self$py_exp_files)) {
          self$py_exp_col[i, "EXP"] <- as.character(
            select.list(
              py_exp_colnames[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the
                             experimental variable in the ",
                            self$py_exp_files$orig_file[i],
                            " table for the selected previous harvest year.")
            )
          )
          self$py_exp_col[i, "dist"] <- as.character(
            select.list(
              c(py_exp_colnames[[i]], NA),
              multiple = FALSE,
              title =  paste0("OPTIONAL: Select the column name in the ",
                              self$py_exp_files$orig_file[i],
                              " table that corresponds to the distance
                              between measured points OR select NA if no
                              column present. This is used to remove observations
                              when the sprayer is moving at irregular speeds.")
            )
          )
          if (self$expvar == "aa_n") {
            self$py_exp_col[i, "product"] <- as.character(
              select.list(
                c(py_exp_colnames[[i]], NA),
                multiple = FALSE,
                title = paste0("Select the column name in the ",
                               self$py_exp_files$orig_file[i],
                               " table that corresponds to the product applied.
                               This is used to determine the conversion from the
                               applied rate to lbs N per acre.")
              )
            )
          } else {
            self$py_exp_col[i, "product"] <- NA
          }
          if (!is.na(self$py_exp_col[i, "product"])) {
            tempTable <- private$.getTempExpTable(
              db,
              self$py_exp_files,
              self$farmername,
              self$py_exp,
              self$fieldname
            )
            self$py_exp_conv$FORMULA[i] <-
              tempTable[1,
                        grep(self$py_exp_col$product[i],
                             colnames(tempTable))]
            self$py_exp_conv$conversion[i] <-
              readline(prompt=cat(paste0("The product formula is ",
                                         self$py_exp_conv$FORMULA[i],
                                         ". Please provide the conversion rate
                                         from the applied rate to lbs N per acre.",
                                         "\n", "If the product is 32% UAN (gal/ac)
                                         the conversion is 3.5, otherwise it is
                                         typically the fractional form of the percent
                                         N from the NPK formula (i.e. 46-18-8 = 46% N
                                         = 0.46).", "\n", "Conversion Rate: ")))
          }
        }
      }
    },
    .selectExportParms = function() {
      if (self$boundary_import == "No") {
        self$save_in_db <- as.character(
          select.list(
            c("Yes", "No"),
            multiple=FALSE,
            title=  paste0("Save aggregated ",
                           self$cy_resp, "  ",
                           ifelse(self$respvar == "yld",
                                  "Yield",
                                  ifelse(self$respvar == "pro",
                                         "Protein",
                                         "Satellite")),
                           " data from ",
                           self$fieldname,
                           " in database?")
          )
        )
        self$export <- as.character(
          select.list(
            c("Yes", "No"),
            multiple = FALSE,
            title = paste0("Export aggregated ",
                           self$cy_resp, "  ",
                           ifelse(self$respvar == "yld",
                                  "Yield",
                                  ifelse(self$respvar == "pro",
                                         "Protein",
                                         "Satellite")),
                           " data from ",
                           self$fieldname,
                           " as a .csv?")
          )
        )
      } else {
        self$save_in_db <- "No"
        self$export <- "Yes"
      }
      if (self$export == "Yes") {
        self$export_name <- readline(
          prompt = paste0("Enter filename for export (without file extension,
                          and no spaces ideally) : ")
        )
        self$export_name <- gsub(" ", "", self$export_name)
      }
    },
    .getFiles = function(schema_tab, db, farmername, fieldname) {
      FILES <- DBI::dbGetQuery(
        db,
        paste0("SELECT DISTINCT year, orig_file
               FROM ", farmername, "_r.", schema_tab, " ", schema_tab, "
               JOIN all_farms.temp temp
               ON ST_Intersects(", schema_tab, ".geometry, temp.geometry)")
      )
      if (schema_tab != "pro"|schema_tab != "yld") {
        if (length(FILES) > 0) {
          FILES$table <- schema_tab
        } else {
          FILES <- NULL
        }
      }
      return(FILES)
    },
    .getExpFilenamesAndTables = function(exp_files, orig_files_exp) {
      exp_files <- data.frame(orig_file = exp_files,
                              table = rep(NA, length(exp_files)))
      if (any(grepl("None", exp_files$orig_file))) {
        exp_files$table <- NA
      } else {
        for (i in 1:nrow(exp_files)) {
          exp_files$table[i] <- as.character(
            orig_files_exp[grep(as.character(exp_files[i, "orig_file"]),
                                orig_files_exp$orig_file),
                           "table"]
          )
        }
      }
      return(exp_files)
    },
    .getTempRespTableCols = function(resp_files,
                                    db,
                                    respvar,
                                    year,
                                    farmername,
                                    fieldname) {
      temp_tab_cols <- rep(list(NULL), length(resp_files))
      names(temp_tab_cols) <- resp_files

      # make temp table w/necessary data to get columns
      temp_tab_cols <- suppressWarnings(lapply(resp_files,
                              private$.getDatForCols,
                              db,
                              respvar,
                              year,
                              farmername,
                              fieldname)) %>%
        lapply(as.data.frame) %>%
        lapply(private$.convNaN2NumNaN) %>%
        lapply(function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        lapply(function(df) {sapply(df, function(x) all(is.nan(x)|is.na(x)))})

      for (i in 1:length(temp_tab_cols)) {
        temp_tab_cols[[i]] <- as.data.frame(
          t(subset(temp_tab_cols[[i]],  temp_tab_cols[[i]] == FALSE))
        )
      }
      return(temp_tab_cols)
    },
    .getDatForCols = function(resp_file,
                              db,
                              respvar,
                              year,
                              farmername,
                              fieldname) {
      OUT_FILE <- invisible(DBI::dbGetQuery(
        db,
        paste0("SELECT ", respvar, ".*
               FROM  ", farmername, "_r.", respvar, "
               WHERE orig_file = '", resp_file, "'
               AND fieldname = '", fieldname, "'")
      ))
      # OUT_FILE <- invisible(
      #   DBI::dbGetQuery(
      #     db,
      #     paste0("
      #        (SELECT ", respvar, ".*
      #        FROM  ", farmername, "_r.", respvar, " ", respvar, "
      #        JOIN all_farms.temp temp
      #        ON ST_Within(", respvar, ".geometry, temp.geometry)
      #        WHERE ", respvar, ".year = '", year, "'
      #        AND ", respvar, ".orig_file = '", resp_file, "')
      #        ")
      #   )
      # )
      return(OUT_FILE)
    },
    .getTempExpTableCols = function(exp_files,
                                    db,
                                    respvar,
                                    year,
                                    farmername,
                                    fieldname) {
      temp_tab_cols <- rep(list(NULL), nrow(exp_files))
      names(temp_tab_cols) <- exp_files$orig_file
      for (i in 1:nrow(exp_files)) {
        # temp_tab_cols[[i]] <- invisible(DBI::dbGetQuery(
        #     db,
        #     paste0(
        #       "(SELECT ", exp_files$table[i], ".*
        #        FROM  ", farmername, "_r.", exp_files$table[i], "
        #        WHERE fieldname = '", fieldname, "'
        #        AND orig_file = '", exp_files$orig_file[i], "')"
        #     )
        #   ))
        temp_tab_cols[[i]] <- invisible(
          DBI::dbGetQuery(
            db,
            paste0(" (SELECT ", exp_files$table[i], ".*
                     FROM  ", farmername, "_r.", exp_files$table[i], " ", exp_files$table[i], "
                     JOIN all_farms.temp temp
                     ON ST_Intersects(", exp_files$table[i], ".geometry, temp.geometry)
                     WHERE ", exp_files$table[i], ".year = '", year, "'
                     AND ", exp_files$table[i], ".orig_file = '", exp_files$orig_file[i], "')
                     ")
          )
        )
      }
      temp_tab_cols <- temp_tab_cols %>%
        lapply(as.data.frame) %>%
        lapply(function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        lapply(function(df) {sapply(df, function(x) all(is.nan(x)|is.na(x)))})
      for (i in 1:length(temp_tab_cols)) {
        temp_tab_cols[[i]] <- as.data.frame(
          t(subset(temp_tab_cols[[i]], temp_tab_cols[[i]] == FALSE))
        )
      }
      return(temp_tab_cols)
    },
    .getTempExpTable = function(db, exp_files, farmername, year, fieldname, i) {
      tempTable <- sf::st_read(
        db,
        query = paste0("SELECT ", exp_files$table[i], ".*
                       FROM  ", farmername, "_r.", exp_files$table[i], " ", exp_files$table[i], "
                       JOIN all_farms.temp temp
                       ON ST_Intersects(", exp_files$table[i], ".geometry, temp.geometry)
                       WHERE ", exp_files$table[i], ".year = '", year, "'
                       AND ", exp_files$table[i], ".orig_file = '", exp_files$orig_file[i], "'
                       LIMIT 1"),
        geometry_column="geometry") %>%
        as.data.frame() %>%
        (function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        (function(df) {df <- df[, which(df != "NaN")];return(df)})
      return(tempTable)
    },
    .tabNames = function(SCHEMA, db) {
      schema_out <- DBI::dbGetQuery(
        db,
        paste0("SELECT table_name
               FROM information_schema.tables
               WHERE table_schema='", SCHEMA, "'")
      )
      schema_out <- schema_out[1:nrow(schema_out), ]
      if (length(schema_out) == 0) {
        schema_out <- NULL
      } else {
        schema_out <- as.list(schema_out[1:length(schema_out)])
      }
      return(schema_out)
    },
    .convNaN2NumNaN = function(df) {
      for (i in 1:ncol(df)) {
        if (all(grepl("NaN", df[, i]))) {
          df[, i] <- as.numeric(df[, i])
        }
      }
      return(df)
    }
  )
)


