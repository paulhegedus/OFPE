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
#' @export
AggInputs <- R6::R6Class(
  "AggInputs",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field bboxImport Yes/No, will user be uploading their own field boundary?
    #' Used for spatially querying the database for intersecting data.
    bboxImport = NULL,
    #' @field bboxLocation Only relevant if bboxImport == "Yes". Is the location
    #' of the shapefile containing the field boundary to use for spatial queries.
    bboxLocation = NULL,
    #' @field FIELDNAME Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    FIELDNAME = NULL,
    #' @field FARMERNAME Name of the farmer that owns the selected field.
    FARMERNAME = NULL,
    #' @field RESPVAR Response variable to aggregate data for, select/input
    #' 'Yield', 'Protein', 'Satellite'. 'Satellite' data aggregates only
    #' remotely sensed data and does not include any on-farm collected data.
    RESPVAR = NULL,
    #' @field EXPVAR Experimental variable to aggregate data for, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    EXPVAR = NULL,
    #' @field CY_RESP The year of interest of the selected response variable.
    #' This is considered the 'current year' (CY), and differs from the 'previous
    #' year' (PY), which is the latest year before the CY during which the field
    #' was cropped. This is separate from the CY_EXP variable because in some
    #' instances, applied data was categorized as or applied in the year prior
    #' (i.e. WW seeding rates occur in fall of the year before harvest in the
    #' following August).
    CY_RESP = NULL,
    #' @field PY_RESP The year prior to the selected CY that a crop was harvested
    #' in the specified field. This iss the latest year before the CY during which
    #' the field was cropped. If you do not data from any previous year, you can
    #' provide a year for labeling and annotations sake in output figures.
    PY_RESP = NULL,
    #' @field CY_EXP The year of interest of the selected experimental variable.
    #' This is the year that the experimental variable was applied to grow the
    #' crop in the selected 'CY_RESP' year. This is separate from the CY_RESP
    #' variable because in some instances, applied data was categorized as or
    #' applied in the year prior (i.e. WW seeding rates occur in fall of the year
    #' before harvest in the following August).
    CY_EXP = NULL,
    #' @field PY_EXP The application year of the experimental variable that was
    #' used to grow the crop in the 'PY_RESP' year. If you do not data from any
    #' previous year, you can provide a year for labeling and annotations sake in
    #' output figures.
    PY_EXP = NULL,
    #' @field GRID Determines the location of the aggregated data. Either select
    #' 'Grid' or 'Observed'. 'Grid' aggregates data to the centroids of a 10m grid
    #' laid across the field, while 'Observed' aggregates data to the locations
    #' of the observed response variable (yield or protein). Note that when
    #' 'Satellite' is selected there are no observed points to use, so the 'Grid'
    #' option is selected by default.
    GRID = NULL,
    #' @field DAT_USED Option for the length of year to use for CY data. In
    #' winter wheat conventional systems and organic spring wheat systems,
    #' there is a decision point around March 30th at which farmers must make
    #' decisions on their fertilizer or seeding rates, respectively. When making
    #' these decisions, farmers will only have data up to that decision point. This
    #' options determines the covariate data aggregated for the CY, where
    #' 'Decision Point' aggregates data for the CY from January 1st to March 30th
    #' of that year, while 'Full Year' aggregates data for the CY from January 1st
    #' to December 31st. This is a residual special use case for Paul Hegedus' 2020
    #' ICPA work.
    DAT_USED = NULL,
    #' @field CY_RESP_FILES Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'CY_RESP' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.).
    CY_RESP_FILES = NULL,
    #' @field PY_RESP_FILES Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'PY_RESP' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.). If you do
    #' not have data available from a previous year and are not using the
    #' interactive input selectors, simply input "None", otherwise when using the
    #' interactive methods this is handled for you.
    PY_RESP_FILES = NULL,
    #' @field CY_EXP_FILES Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'CY_EXP' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e.
    #' if application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons.
    CY_EXP_FILES = NULL,
    #' @field PY_EXP_FILES Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'PY_EXP' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e. if
    #' application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons. If no data from the desired year
    #' is available, put "None" in the 'orig_file' column.
    PY_EXP_FILES = NULL,
    #' @field saveInDB Yes/No. Logical, whether to save the aggregated data into
    #' the OFPE formatted database. Not an option if you are supplying your own
    #' boundary for which to aggregate data.
    saveInDB = NULL,
    #' @field export Yes/No. Logical, whether to export the aggregated data as
    #' a '.csv' file. If yes, the user will need to provide the 'exportName'.
    export = NULL,
    #' @field exportName If exporting the aggregated data as a '.csv', the user
    #' needs to specify the name of the file to export. This includes the file
    #' path.
    exportName = NULL,
    #' @field CY_RESP_COL Data.frame with 3 columns, 'RESP', 'DIST', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('RESP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'orig_file' is the same as those selected in the
    #' 'CY_RESP_FILES' vector.
    CY_RESP_COL = NULL,
    #' @field PY_RESP_COL Data.frame with 3 columns, 'RESP', 'DIST', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('RESP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional and optionally used during a cleaning step where
    #' points are removed if they are more than 4SD from the mean distance
    #' between observations as this indicates that the equipment was moving
    #' at an abnormal speed and potentially resulting in erroneous measurements.
    #' The 'orig_file' is the same as those selected in the 'PY_RESP_FILES' vector.
    PY_RESP_COL = NULL,
    #' @field CY_EXP_COL Data.frame with 4 columns, 'EXP', 'DIST', 'PRODUCT',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'PRODUCT' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    CY_EXP_COL = NULL,
    #' @field PY_EXP_COL Data.frame with 4 columns, 'EXP', 'DIST', 'PRODUCT',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'PRODUCT' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'PY_EXP_FILES' table.
    PY_EXP_COL = NULL,
    #' @field CY_EXP_CONV Data.frame with 3 columns, 'FORMULA', 'CONVERSION',
    #' and 'orig_file'. Based on the users selection of 'PRODUCT' in the
    #' CY_EXP_COL data.frame, the formula from the 'PRODUCT' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilzier
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    CY_EXP_CONV = NULL,
    #' @field PY_EXP_CONV Data.frame with 3 columns, 'FORMULA', 'CONVERSION',
    #' and 'orig_file'. Based on the users selection of 'PRODUCT' in the
    #' CY_EXP_COL data.frame, the formula from the 'PRODUCT' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilzier
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    PY_EXP_CONV = NULL,
    #' @field SIZE Optional, the size, in meters, to make a grid across the field.
    #' Mostly necessary for when 'Grid' is selected for the 'GRID' parameter, however
    #' is the scale at which the finest resolution cleaning of data occurs. Defaults
    #' to 10m if left NULL.
    SIZE = 10,

    #' @description
    #' Initialize an object for storing aggregation inputs.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param bboxImport Yes/No, will user be uploading their own field boundary?
    #' Used for spatially querying the database for intersecting data.
    #' @param bboxLocation Only relevant if bboxImport == "Yes". Is the location
    #' of the shapefile containing the field boundary to use for spatial queries.
    #' @param FIELDNAME Name of the field to aggregate data for. Selected from
    #' the 'all_farms.fields' table of an OFPE formatted database.
    #' @param FARMERNAME Name of the farmer that owns the selected field.
    #' @param RESPVAR Response variable to aggregate data for, select/input
    #' 'Yield', 'Protein', 'Satellite'. 'Satellite' data aggregates only
    #' remotely sensed data and does not include any on-farm collected data.
    #' @param EXPVAR Experimental variable to aggregate data for, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param CY_RESP The year of interest of the selected response variable.
    #' This is considered the 'current year' (CY), and differs from the 'previous
    #' year' (PY), which is the latest year before the CY during which the field
    #' was cropped. This is separate from the CY_EXP variable because in some
    #' instances, applied data was categorized as or applied in the year prior
    #' (i.e. WW seeding rates occur in fall of the year before harvest in the
    #' following August).
    #' @param PY_RESP The year prior to the selected CY that a crop was harvested
    #' in the specified field. This iss the latest year before the CY during which
    #' the field was cropped. If you do not data from any previous year, you can
    #' provide a year for labeling and annotations sake in output figures.
    #' @param CY_EXP The year of interest of the selected experimental variable.
    #' This is the year that the experimental variable was applied to grow the
    #' crop in the selected 'CY_RESP' year. This is separate from the CY_RESP
    #' variable because in some instances, applied data was categorized as or
    #' applied in the year prior (i.e. WW seeding rates occur in fall of the year
    #' before harvest in the following August).
    #' @param PY_EXP The application year of the experimental variable that was
    #' used to grow the crop in the 'PY_RESP' year. If you do not data from any
    #' previous year, you can provide a year for labeling and annotations sake in
    #' output figures.
    #' @param GRID Determines the location of the aggregated data. Either select
    #' 'Grid' or 'Observed'. 'Grid' aggregates data to the centroids of a 10m grid
    #' laid across the field, while 'Observed' aggregates data to the locations
    #' of the observed response variable (yield or protein). Note that when
    #' 'Satellite' is selected there are no observed points to use, so the 'Grid'
    #' option is selected by default.
    #' @param DAT_USED Option for the length of year to use for CY data. In
    #' winter wheat conventional systems and organic spring wheat systems,
    #' there is a decision point around March 30th at which farmers must make
    #' decisions on their fertilizer or seeding rates, respectively. When making
    #' these decisions, farmers will only have data up to that decision point. This
    #' options determines the covariate data aggregated for the CY, where
    #' 'Decision Point' aggregates data for the CY from January 1st to March 30th
    #' of that year, while 'Full Year' aggregates data for the CY from January 1st
    #' to December 31st. This is a residual special use case for Paul Hegedus' 2020
    #' ICPA work.
    #' @param CY_RESP_FILES Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'CY_RESP' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.).
    #' @param PY_RESP_FILES Vector of names. Based on the user's selected field
    #' and year, the database can be queried for the original file names of the
    #' data uploaded to the database. The user selects the correct file name that
    #' corresponds to the response variable data in the specified field and year.
    #' In this case the 'PY_RESP' year. Multiple files are allowed for selection
    #' because of the cases in which multiple files correspond to the data in the
    #' given year and field (i.e. if harvest took multiple days etc.). If you do
    #' not have data available from a previous year and are not using the
    #' interactive input selectors, simply input "None", otherwise when using the
    #' interactive methods this is handled for you.
    #' @param CY_EXP_FILES Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'CY_EXP' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e.
    #' if application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons.
    #' @param PY_EXP_FILES Data.frame with 2 columns, 'orig_file' and 'table'.
    #' Based on the user's selected field and year, the database can be queried
    #' for the original file names of the data uploaded to the database. The user
    #' selects the correct file name that corresponds to the experimental variable
    #' data in the specified field and year. In this case, the 'PY_EXP' year.
    #' Multiple files are allowed for selection because of the cases in which
    #' multiple files correspond to the data in the given year and field (i.e. if
    #' application took multiple days etc.). The 'orig_file' column contains
    #' the name of the original filename uploaded to the database with the raw
    #' data and 'table' contains the table within the farmer schema that the
    #' data is housed. This is because some as-applied experimental data are
    #' point vectors and some are polygons. If no data from the desired year
    #' is available, put "None" in the 'orig_file' column.
    #' @param saveInDB Yes/No. Logical, whether to save the aggregated data into
    #' the OFPE formatted database. Not an option if you are supplying your own
    #' boundary for which to aggregate data.
    #' @param export Yes/No. Logical, whether to export the aggregated data as
    #' a '.csv' file. If yes, the user will need to provide the 'exportName'.
    #' @param exportName If exporting the aggregated data as a '.csv', the user
    #' needs to specify the name of the file to export. This includes the file
    #' path.
    #' @param CY_RESP_COL Data.frame with 3 columns, 'RESP', 'DIST', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('RESP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'orig_file' is the same as those selected in the
    #' 'CY_RESP_FILES' vector.
    #' @param PY_RESP_COL Data.frame with 3 columns, 'RESP', 'DIST', and
    #' 'orig_file'. This data.frame contains the column containing data for
    #' the selected response variable ('RESP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional and optionally used during a cleaning step where
    #' points are removed if they are more than 4SD from the mean distance
    #' between observations as this indicates that the equipment was moving
    #' at an abnormal speed and potentially resulting in erroneous measurements.
    #' The 'orig_file' is the same as those selected in the 'PY_RESP_FILES' vector.
    #' @param CY_EXP_COL Data.frame with 4 columns, 'EXP', 'DIST', 'PRODUCT',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'PRODUCT' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    #' @param PY_EXP_COL Data.frame with 4 columns, 'EXP', 'DIST', 'PRODUCT',
    #' and 'orig_file'. This data.frame contains the column containing data for
    #' the selected experimental variable ('EXP') and the column (if any) that
    #' correspond to the distance between observations ('DIST'). The 'DIST'
    #' selection is optional (can be NA) and optionally used during a cleaning
    #' step where points are removed if they are more than 4SD from the mean
    #' distance between observations as this indicates that the equipment was
    #' moving at an abnormal speed and potentially resulting in erroneous
    #' measurements. The 'PRODUCT' column is also optional, and not relevant
    #' for seeding rate data, and used if there is a column corresponding to
    #' the product applied. When left blank, it is assumed that the 'EXP' column
    #' contains data in lbs/acre and the user is not given an option to provide
    #' a conversion rate. It is good practice to always select a column (even if
    #' no specific 'product' column) here to explicitly state a conversion factor.
    #' The 'orig_file' is the same as those selected in the 'PY_EXP_FILES' table.
    #' @param CY_EXP_CONV Data.frame with 3 columns, 'FORMULA', 'CONVERSION',
    #' and 'orig_file'. Based on the users selection of 'PRODUCT' in the
    #' CY_EXP_COL data.frame, the formula from the 'PRODUCT' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilzier
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    #' @param PY_EXP_CONV Data.frame with 3 columns, 'FORMULA', 'CONVERSION',
    #' and 'orig_file'. Based on the users selection of 'PRODUCT' in the
    #' CY_EXP_COL data.frame, the formula from the 'PRODUCT' column is extracted
    #' and used to ask the user the desired conversion factor from the product
    #' applied to lbs per acre. Again, this is only really applicable for fertilzier
    #' rates unless seeding rates are reported in units besides lbs per acre. The
    #' 'orig_file' is the same as those selected in the 'CY_EXP_FILES' table.
    #' @param SIZE Optional, the size, in meters, to make a grid across the field.
    #' Mostly necessary for when 'Grid' is selected for the 'GRID' parameter, however
    #' is the scale at which the finest resolution cleaning of data occurs. Defaults
    #' to 10m if left NULL.
    #' @return A new 'AggInputs' object.
    initialize = function(dbCon,
                          bboxImport = NULL,
                          bboxLocation = NULL,
                          FIELDNAME = NULL,
                          FARMERNAME = NULL,
                          RESPVAR = NULL,
                          EXPVAR = NULL,
                          CY_RESP = NULL,
                          PY_RESP = NULL,
                          CY_EXP = NULL,
                          PY_EXP = NULL,
                          GRID = NULL,
                          DAT_USED = NULL,
                          CY_RESP_FILES = NULL,
                          PY_RESP_FILES = NULL,
                          CY_EXP_FILES = NULL,
                          PY_EXP_FILES = NULL,
                          saveInDB = NULL,
                          export = NULL,
                          exportName = NULL,
                          CY_RESP_COL = NULL,
                          PY_RESP_COL = NULL,
                          CY_EXP_COL = NULL,
                          PY_EXP_COL = NULL,
                          CY_EXP_CONV = NULL,
                          PY_EXP_CONV = NULL,
                          SIZE = 10) {
      stopifnot(!is.null(dbCon))
      self$dbCon <- dbCon
      if (!is.null(bboxImport)) {
        stopifnot(
          is.character(bboxImport),
          grepl("Yes|No", bboxImport)
        )
        self$bboxImport <- bboxImport
        if (self$bboxImport == "Yes") {
          stopifnot(
            !is.null(bboxLocation)
          )
          self$bboxLocation <- bboxLocation
        }
      }
      if (!is.null(FIELDNAME)) {
        stopifnot(
          is.character(FIELDNAME)
        )
        self$FIELDNAME <- FIELDNAME
      }
      if (!is.null(FARMERNAME)) {
        stopifnot(
          is.character(FARMERNAME)
        )
        self$FARMERNAME <- FARMERNAME
      }
      if (!is.null(RESPVAR)) {
        stopifnot(
          is.character(RESPVAR)
        )
        self$RESPVAR <- ifelse(RESPVAR == "Yield",
                               "yld",
                               ifelse(RESPVAR == "Protein",
                                      "pro",
                                      "sat"))
      }
      if (!is.null(EXPVAR)) {
        stopifnot(
          is.character(EXPVAR)
        )
        self$EXPVAR <- ifelse(EXPVAR == "As-Applied Nitrogen",
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
      if (!is.null(DAT_USED)) {
        stopifnot(
          is.character(DAT_USED)
        )
        self$DAT_USED <- ifelse(DAT_USED == "Decision Point",
                                "decision_point",
                                "full_year")
      }
      if (!is.null(CY_RESP)) {
        stopifnot(
          is.numeric(CY_RESP)|is.character(CY_RESP)
        )
        self$CY_RESP <- as.numeric(CY_RESP)
      }
      if (!is.null(PY_RESP)) {
        stopifnot(
          is.numeric(PY_RESP)|is.character(PY_RESP)
        )
        self$PY_RESP <- as.numeric(PY_RESP)
      }
      if (!is.null(CY_EXP)) {
        stopifnot(
          is.numeric(CY_EXP)|is.character(CY_EXP)
        )
        self$CY_EXP <- as.numeric(CY_EXP)
      }
      if (!is.null(PY_EXP)) {
        stopifnot(
          is.numeric(PY_EXP)|is.character(PY_EXP)
        )
        self$PY_EXP <- as.numeric(PY_EXP)
      }
      if (!is.null(CY_RESP_FILES)) {
        stopifnot(
          is.character(CY_RESP_FILES)
        )
        self$CY_RESP_FILES <- CY_RESP_FILES
      }
      if (!is.null(PY_RESP_FILES)) {
        stopifnot(
          is.character(PY_RESP_FILES)
        )
        self$PY_RESP_FILES <- PY_RESP_FILES
      }
      if (!is.null(CY_EXP_FILES)) {
        stopifnot(
          is.data.frame(CY_EXP_FILES),
          ncol(CY_EXP_FILES) == 2,
          any(grepl("orig_file", names(CY_EXP_FILES))),
          any(grepl("table", names(CY_EXP_FILES))),
          !anyNA(CY_EXP_FILES$orig_file)
        )
        self$CY_EXP_FILES <- CY_EXP_FILES
      }
      if (!is.null(PY_EXP_FILES)) {
        stopifnot(
          is.data.frame(PY_EXP_FILES),
          any(grepl("orig_file", names(CY_EXP_FILES)))
        )
        self$PY_EXP_FILES <- PY_EXP_FILES
      }
      if (!is.null(saveInDB)) {
        stopifnot(
          is.character(saveInDB),
          grepl("Yes|No", saveInDB)
        )
        self$saveInDB <- saveInDB
      }
      if (!is.null(export)) {
        stopifnot(
          is.character(export),
          grepl("Yes|No", export)
        )
        self$export <- export
        if (self$export == "Yes") {
          stopifnot(
            !is.null(exportName),
            is.character(exportName)
          )
          self$exportName <- exportName
        }
      }
      if (!is.null(CY_RESP_COL)) {
        stopifnot(
          is.data.frame(CY_RESP_COL),
          ncol(CY_RESP_COL) == 3,
          nrow(CY_RESP_COL) == length(self$CY_RESP_FILES),
          any(grepl("RESP", names(CY_RESP_COL))),
          any(grepl("DIST", names(CY_RESP_COL))),
          any(grepl("orig_file", names(CY_RESP_COL))),
          !anyNA(CY_RESP_COL$RESP)
        )
        self$CY_RESP_COL <- CY_RESP_COL
      }
      if (!is.null(PY_RESP_COL)) {
        stopifnot(
          is.data.frame(PY_RESP_COL),
          ncol(PY_RESP_COL) == 3,
          nrow(PY_RESP_COL) == length(self$PY_RESP_FILES),
          any(grepl("RESP", names(PY_RESP_COL))),
          any(grepl("DIST", names(PY_RESP_COL))),
          any(grepl("orig_file", names(PY_RESP_COL)))
        )
        self$PY_RESP_COL <- PY_RESP_COL
      }
      if (!is.null(CY_EXP_COL)) {
        stopifnot(
          is.data.frame(CY_EXP_COL),
          ncol(CY_EXP_COL) == 4,
          nrow(CY_EXP_COL) == nrow(self$CY_EXP_FILES),
          any(grepl("EXP", names(CY_EXP_COL))),
          any(grepl("DIST", names(CY_EXP_COL))),
          any(grepl("PRODUCT", names(CY_EXP_COL))),
          any(grepl("orig_file", names(CY_EXP_COL)))
        )
        self$CY_EXP_COL <- CY_EXP_COL
      }
      if (!is.null(PY_EXP_COL)) {
        stopifnot(
          is.data.frame(PY_EXP_COL),
          ncol(PY_EXP_COL) == 4,
          nrow(PY_EXP_COL) == nrow(self$PY_EXP_COL),
          any(grepl("EXP", names(PY_EXP_COL))),
          any(grepl("DIST", names(PY_EXP_COL))),
          any(grepl("PRODUCT", names(PY_EXP_COL))),
          any(grepl("orig_file", names(PY_EXP_COL)))
        )
        self$PY_EXP_COL <- PY_EXP_COL
      }
      if (!is.null(CY_EXP_CONV)) {
        stopifnot(
          is.data.frame(CY_EXP_CONV),
          ncol(CY_EXP_CONV) == 3,
          nrow(CY_EXP_CONV) == nrow(self$CY_EXP_CONV),
          any(grepl("FORMULA", names(CY_EXP_CONV))),
          any(grepl("CONVERSION", names(CY_EXP_CONV))),
          any(grepl("orig_file", names(CY_EXP_CONV)))
        )
        self$CY_EXP_CONV <- CY_EXP_CONV
      }
      if (!is.null(PY_EXP_CONV)) {
        stopifnot(
          is.data.frame(PY_EXP_CONV),
          ncol(PY_EXP_CONV) == 3,
          nrow(PY_EXP_CONV) == nrow(self$PY_EXP_CONV),
          any(grepl("FORMULA", names(PY_EXP_CONV))),
          any(grepl("CONVERSION", names(PY_EXP_CONV))),
          any(grepl("orig_file", names(PY_EXP_CONV)))
        )
        self$PY_EXP_CONV <- PY_EXP_CONV
      }
      if (!is.null(SIZE)) {
        stopifnot(
          is.numeric(SIZE)
        )
        self$SIZE <- SIZE
      }
    },
    #' @description
    #' Interactive method for selecting aggregation input options.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A completed 'AggInputs' object.
    selectInputs = function() {
      private$.selectField(self$dbCon$db)
      OFPE::removeTempFarmerTables(self$dbCon$db, self$FARMERNAME)

      private$.selectRespvar()
      private$.selectAggLOY()
      if (self$RESPVAR != "sat") {
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
      self$bboxImport <- as.character(
        select.list(
          c("Yes", "No"),
          title = paste0("Will user be uploading their own field boundary?
                         If so, must be in working directory. If the user
                         uploaded field boundary will be accessed often,
                         the user is encouraged to update the database with
                         this boundary to provide for faster queries and long
                         term storage of the field boundary."))
      )
      if (self$bboxImport == "No") {
        ## select field boundary
        self$FIELDNAME <- as.character(
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
                 self$FIELDNAME, "'")
        )
        ## fill in farmer and field info
        self$FARMERNAME <- as.character(
          DBI::dbGetQuery(db,
            paste0("SELECT farmer
                   FROM all_farms.farmers
                   WHERE farmeridx = '",
                   unique(farmeridx), "'")
          )
        )
        ## copy field boundary to temp file for later queries
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE all_farms.temp AS
                   SELECT * FROM all_farms.fields fields
                   WHERE fields.fieldname = '", self$FIELDNAME, "';
                   ALTER TABLE all_farms.temp
                   RENAME COLUMN geom TO geometry;")
          )
        )
      } else {
        ## enter filename for bounding box
        self$bboxLocation <- readline(
          prompt = "Enter file name of field boundary (without file extension) : "
        )
        ## specify field and farmer name
        self$FIELDNAME <- readline(prompt = "Enter field name : ")
        self$FIELDNAME <- self$FIELDNAME %>%
          OFPE::noSpecialChar() %>%
          tolower()
        self$FARMERNAME <- as.character(
          select.list(
            unique(
              DBI::dbGetQuery(db, "SELECT farmer FROM all_farms.farmers")$farmer
            ),
            title = "Select farmer name."
          )
        )
        self$FARMERNAME <- self$FARMERNAME %>%
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
      self$RESPVAR <- ifelse(respVar == "Yield", "yld",
                               ifelse(respVar == "Protein", "pro",
                                      "sat"))
      if (self$RESPVAR == "sat") {
        self$CY_RESP <- as.character(
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
      self$EXPVAR <- ifelse(expVar == "As-Applied Nitrogen", "aa_n", "aa_sr")
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

      self$SIZE <- readline(prompt = "Enter the size of grid to use for aggregation and/or cleaning: ")
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
      self$DAT_USED <- ifelse(data_used == "Decision Point",
                                "decision_point",
                                "full_year")
    },
    .selectRespFiles = function(db) {
      orig_files_resp <- private$.getFiles(self$RESPVAR,
                                           db,
                                           self$FARMERNAME,
                                           self$FIELDNAME)
      self$CY_RESP <- as.character(
        select.list(
          unique(orig_files_resp$year)[
            order(as.numeric(unique(orig_files_resp$year)))
          ],
          title = "Select the harvest year to aggregate data for."
        )
      )
      orig_filesCY_resp <-
        orig_files_resp[orig_files_resp$year == self$CY_RESP, ]
      self$CY_RESP_FILES <- as.character(
        select.list(
          c(orig_filesCY_resp$orig_file, "None"),
          multiple = ifelse(self$GRID == "grid", TRUE, FALSE),
          title = paste0("Select file(s) to aggregate data on. Or select all
                         to gather data for all available files as one. If
                         unsure, it is recommended to visualize extent of each
                         file using the PostgreSQL connection in QGIS.")
        )
      )
      self$PY_RESP <- as.character(
        select.list(
          c(unique(orig_files_resp$year)[
              order(as.numeric(unique(orig_files_resp$year)))
            ],
            "Other"),
          title = "Select the previous harvest year to get prior data for."
        )
      )
      if (self$PY_RESP == "Other") {
        self$PY_RESP <- readline(prompt = "Enter previous harvest year: ")
        self$PY_RESP_FILES <- "None"
      } else {
        orig_filesPY_resp <-
          orig_files_resp[orig_files_resp$year == self$PY_RESP, ]
        self$PY_RESP_FILES <- as.character(
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
      schemaTabs <- private$.tabNames(paste0(self$FARMERNAME, "_r"), db)
      schemaTabs <- schemaTabs[grepl(self$EXPVAR, schemaTabs)]

      orig_files_exp <- list()
      orig_files_exp <- lapply(schemaTabs,
                               private$.getFiles,
                               db,
                               self$FARMERNAME,
                               self$FIELDNAME)
      orig_files_exp <- do.call(rbind, orig_files_exp)
      ## Current year experimental data
      self$CY_EXP <- as.character(
        select.list(
          c(unique(orig_files_exp$year)[
              order(as.numeric(unique(orig_files_exp$year)))
            ],
            "Other"),
          title = "Select the application year for the current harvest year."
        )
      )
      if (self$CY_EXP == "Other") {
        self$CY_EXP <- readline(prompt="Enter current application year: ")
        self$CY_EXP_FILES <- "None"
        self$CY_EXP_FILES <- data.frame(orig_file = "None")
      } else {
        orig_filesCY_exp <-
          orig_files_exp[orig_files_exp$year == self$CY_EXP, ]
        self$CY_EXP_FILES <- as.character(
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
        self$CY_EXP_FILES <- private$.getExpFilenamesAndTables(
          self$CY_EXP_FILES, orig_filesCY_exp
        )
      }
      ## Previous year experimental data
      self$PY_EXP <- as.character(
        select.list(
          c(unique(orig_files_exp$year)[
              order(as.numeric(unique(orig_files_exp$year)))
            ],
            "Other"),
          title= "Select the application year for the previous harvest year."
        )
      )
      if (self$PY_EXP == "Other") {
        self$PY_EXP <- readline(prompt = "Enter previous application year: ")
        self$PY_EXP_FILES <- data.frame(orig_file = "None")
      } else {
        orig_filesPY_exp <- orig_files_exp[orig_files_exp$year == self$PY_EXP, ]
        self$PY_EXP_FILES <- as.character(
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
        self$PY_EXP_FILES <- private$.getExpFilenamesAndTables(
          self$PY_EXP_FILES, orig_filesPY_exp
        )
      }
    },
    .selectRespCols = function(db) {
      ## Current year resp file
      if (!any(self$CY_RESP_FILES == "None")) {
        tempTableCols <- private$.getTempRespTableCols(
          self$CY_RESP_FILES,
          db,
          self$RESPVAR,
          self$CY_RESP,
          self$FARMERNAME,
          self$FIELDNAME
        )
        CY_RESP_COLNAMES <- rep(list(NA), length(self$CY_RESP_FILES)) %>%
          `names<-`(self$CY_RESP_FILES)
        CY_RESP_COLNAMES <- lapply(tempTableCols, colnames)
        self$CY_RESP_COL <- as.data.frame(
          matrix(NA, length(self$CY_RESP_FILES), 2)) %>%
          `colnames<-`(c("RESP", "DIST"))
        self$CY_RESP_COL$orig_file <- self$CY_RESP_FILES
        for (i in 1:length(self$CY_RESP_FILES)) {
          self$CY_RESP_COL[i, "RESP"] <- as.character(
            select.list(
              CY_RESP_COLNAMES[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the response
                             variable in the ", self$CY_RESP_FILES[i], " table.")
            )
          )
          self$CY_RESP_COL[i, "DIST"] <- as.character(
            select.list(
              c(CY_RESP_COLNAMES[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$CY_RESP_FILES[i], " table that corresponds
                             to the distance between measured points OR select
                             NA if no column present. This is used to remove
                             observations when the combine is moving at irregular
                             speeds.")
            )
          )
        }
      }
      ## Previous year resp colnames
      if (!any(self$PY_RESP_FILES == "None")) {
        tempTableCols <- private$.getTempRespTableCols(
          self$PY_RESP_FILES,
          db,
          self$RESPVAR,
          self$PY_RESP,
          self$FARMERNAME,
          self$FIELDNAME
        )

        PY_RESP_COLNAMES <- rep(list(NA), length(self$PY_RESP_FILES)) %>%
          `names<-`(self$PY_RESP_FILES)
        PY_RESP_COLNAMES <- lapply(tempTableCols, colnames)

        self$PY_RESP_COL <- as.data.frame(
          matrix(NA, length(self$PY_RESP_FILES), 2)) %>%
          `colnames<-`(c("RESP", "DIST"))
        self$PY_RESP_COL$orig_file <- self$PY_RESP_FILES
        for (i in 1:length(self$PY_RESP_FILES)) {
          self$PY_RESP_COL[i, "RESP"] <- as.character(
            select.list(
              PY_RESP_COLNAMES[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the response
                             variable in the ", self$PY_RESP_FILES[i], " table
                             from the previous harvest year.")
            )
          )
          self$PY_RESP_COL[i, "DIST"] <- as.character(
            select.list(
              c(PY_RESP_COLNAMES[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$PY_RESP_FILES[i], " table that corresponds to
                             the distance between measured points OR select NA if
                             no column present. This is used to remove observations
                             when the combine is moving at irregular speeds.")))
        }
      }
    },
    .selectExpCols = function(db) {
      ## Current year experimental columns
      if (!any(self$CY_EXP_FILES$orig_file == "None")) {
        # make temp table w/necessary data
        tempTableCols <- private$.getTempExpTableCols(
          self$CY_EXP_FILES,
          db,
          self$RESPVAR,
          self$CY_EXP,
          self$FARMERNAME,
          self$FIELDNAME
        )
        CY_EXP_COLNAMES <- rep(list(NA), nrow(self$CY_EXP_FILES)) %>%
          `names<-`(self$CY_EXP_FILES$orig_file)
        CY_EXP_COLNAMES <- lapply(tempTableCols, colnames)

        self$CY_EXP_COL <- as.data.frame(
          matrix(NA, nrow(self$CY_EXP_FILES), 3)) %>%
          `colnames<-`(c("EXP", "DIST", "PRODUCT"))
        self$CY_EXP_COL$orig_file <- self$CY_EXP_FILES$orig_file

        self$CY_EXP_CONV <- as.data.frame(
          matrix(NA, nrow(self$CY_EXP_FILES), 2)) %>%
          `colnames<-`(c("FORMULA", "CONVERSION"))
        self$CY_EXP_CONV$orig_file <- self$CY_EXP_FILES$orig_file

        for (i in 1:nrow(self$CY_EXP_FILES)) {
          self$CY_EXP_COL[i, "EXP"] <- as.character(
            select.list(
              CY_EXP_COLNAMES[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the
                             experimental variable in the ",
                             self$CY_EXP_FILES$orig_file[i],
                            " table for the selected current harvest year.")
            )
          )
          self$CY_EXP_COL[i, "DIST"] <- as.character(
            select.list(
              c(CY_EXP_COLNAMES[[i]], NA),
              multiple = FALSE,
              title = paste0("OPTIONAL: Select the column name in the ",
                             self$CY_EXP_FILES$orig_file[i],
                             " table that corresponds to the distance between
                             measured points OR select NA if no column present.
                             This is used to remove observations when the sprayer
                             is moving at irregular speeds.")
            )
          )
          if (self$EXPVAR == "aa_n") {
            self$CY_EXP_COL[i, "PRODUCT"] <- as.character(
              select.list(
                c(CY_EXP_COLNAMES[[i]], NA),
                multiple = FALSE,
                title = paste0("Select the column name in the ",
                             self$CY_EXP_FILES$orig_file[i],
                             " table that corresponds to the product applied.
                             This is used to determine the conversion from the
                             applied rate to lbs N per acre.")
              )
            )
          } else {
            self$CY_EXP_COL[i, "PRODUCT"] <- NA
          }
          if (!is.na(self$CY_EXP_COL[i, "PRODUCT"])) {
            tempTable <- private$.getTempExpTable(
              db,
              self$CY_EXP_FILES,
              self$FARMERNAME,
              self$CY_EXP,
              self$FIELDNAME,
              i
            )
            self$CY_EXP_CONV$FORMULA[i] <-
              tempTable[1,
                        grep(self$CY_EXP_COL$PRODUCT[i],
                             colnames(tempTable))]
            self$CY_EXP_CONV$CONVERSION[i] <-
              readline(prompt=cat(paste0("The product formula is ",
                                         self$CY_EXP_CONV$FORMULA[i],
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
      if (!any(self$PY_EXP_FILES$orig_file == "None")) {
        tempTableCols <- private$.getTempExpTableCols(
          self$PY_EXP_FILES,
          db,
          self$RESPVAR,
          self$PY_EXP,
          self$FARMERNAME,
          self$FIELDNAME
        )

        PY_EXP_COLNAMES <- rep(list(NA), nrow(self$PY_EXP_FILES)) %>%
          `names<-`(self$PY_EXP_FILES$orig_file)
        PY_EXP_COLNAMES <- lapply(tempTableCols, colnames)

        self$PY_EXP_COL <- as.data.frame(
          matrix(NA, nrow(self$PY_EXP_FILES), 3)) %>%
          `colnames<-`(c("EXP", "DIST", "PRODUCT"))
        self$PY_EXP_COL$orig_file <- self$PY_EXP_FILES$orig_file

        self$PY_EXP_CONV <- as.data.frame(
          matrix(NA, nrow(self$PY_EXP_FILES), 2)) %>%
          `colnames<-`(c("FORMULA", "CONVERSION"))
        self$PY_EXP_CONV$orig_file <- self$PY_EXP_FILES$orig_file

        for (i in 1:nrow(self$PY_EXP_FILES)) {
          self$PY_EXP_COL[i, "EXP"] <- as.character(
            select.list(
              PY_EXP_COLNAMES[[i]],
              multiple = FALSE,
              title = paste0("Select column name that corresponds to the
                             experimental variable in the ",
                            self$PY_EXP_FILES$orig_file[i],
                            " table for the selected previous harvest year.")
            )
          )
          self$PY_EXP_COL[i, "DIST"] <- as.character(
            select.list(
              c(PY_EXP_COLNAMES[[i]], NA),
              multiple = FALSE,
              title =  paste0("OPTIONAL: Select the column name in the ",
                              self$PY_EXP_FILES$orig_file[i],
                              " table that corresponds to the distance
                              between measured points OR select NA if no
                              column present. This is used to remove observations
                              when the sprayer is moving at irregular speeds.")
            )
          )
          if (self$EXPVAR == "aa_n") {
            self$PY_EXP_COL[i, "PRODUCT"] <- as.character(
              select.list(
                c(PY_EXP_COLNAMES[[i]], NA),
                multiple = FALSE,
                title = paste0("Select the column name in the ",
                               self$PY_EXP_FILES$orig_file[i],
                               " table that corresponds to the product applied.
                               This is used to determine the conversion from the
                               applied rate to lbs N per acre.")
              )
            )
          } else {
            self$PY_EXP_COL[i, "PRODUCT"] <- NA
          }
          if (!is.na(self$PY_EXP_COL[i, "PRODUCT"])) {
            tempTable <- private$.getTempExpTable(
              db,
              self$PY_EXP_FILES,
              self$FARMERNAME,
              self$PY_EXP,
              self$FIELDNAME
            )
            self$PY_EXP_CONV$FORMULA[i] <-
              tempTable[1,
                        grep(self$PY_EXP_COL$PRODUCT[i],
                             colnames(tempTable))]
            self$PY_EXP_CONV$CONVERSION[i] <-
              readline(prompt=cat(paste0("The product formula is ",
                                         self$PY_EXP_CONV$FORMULA[i],
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
      if (self$bboxImport == "No") {
        self$saveInDB <- as.character(
          select.list(
            c("Yes", "No"),
            multiple=FALSE,
            title=  paste0("Save aggregated ",
                           self$CY_RESP, "  ",
                           ifelse(self$RESPVAR == "yld",
                                  "Yield",
                                  ifelse(self$RESPVAR == "pro",
                                         "Protein",
                                         "Satellite")),
                           " data from ",
                           self$FIELDNAME,
                           " in database?")
          )
        )
        self$export <- as.character(
          select.list(
            c("Yes", "No"),
            multiple = FALSE,
            title = paste0("Export aggregated ",
                           self$CY_RESP, "  ",
                           ifelse(self$RESPVAR == "yld",
                                  "Yield",
                                  ifelse(self$RESPVAR == "pro",
                                         "Protein",
                                         "Satellite")),
                           " data from ",
                           self$FIELDNAME,
                           " as a .csv?")
          )
        )
      } else {
        self$saveInDB <- "No"
        self$export <- "Yes"
      }
      if (self$export == "Yes") {
        self$exportName <- readline(
          prompt = paste0("Enter filename for export (without file extension,
                          and no spaces ideally) : ")
        )
        self$exportName <- gsub(" ", "", self$exportName)
      }
    },
    .getFiles = function(SCHEMATAB, DB, FARMERNAME, FIELDNAME) {
      FILES <- DBI::dbGetQuery(
        DB,
        paste0("SELECT DISTINCT year, orig_file
               FROM ", FARMERNAME, "_r.", SCHEMATAB, " ", SCHEMATAB, "
               JOIN all_farms.temp temp
               ON ST_Intersects(", SCHEMATAB, ".geometry, temp.geometry)")
      )
      if (SCHEMATAB != "pro"|SCHEMATAB != "yld") {
        if (length(FILES) > 0) {
          FILES$table <- SCHEMATAB
        } else {
          FILES <- NULL
        }
      }
      return(FILES)
    },
    .getExpFilenamesAndTables = function(EXP_FILES, ORIG_FILES_EXP) {
      EXP_FILES <- data.frame(orig_file = EXP_FILES,
                              table = rep(NA, length(EXP_FILES)))
      if (any(grepl("None", EXP_FILES$orig_file))) {
        EXP_FILES$table <- NA
      } else {
        for (i in 1:nrow(EXP_FILES)) {
          EXP_FILES$table[i] <- as.character(
            ORIG_FILES_EXP[grep(as.character(EXP_FILES[i, "orig_file"]),
                                ORIG_FILES_EXP$orig_file),
                           "table"]
          )
        }
      }
      return(EXP_FILES)
    },
    .getTempRespTableCols = function(RESP_FILES,
                                    DB,
                                    RESPVAR,
                                    YEAR,
                                    FARMERNAME,
                                    FIELDNAME) {
      tempTableCols <- rep(list(NULL), length(RESP_FILES))
      names(tempTableCols) <- RESP_FILES

      # make temp table w/necessary data to get columns
      tempTableCols <- lapply(RESP_FILES,
                              private$.getDatForCols,
                              DB,
                              RESPVAR,
                              YEAR,
                              FARMERNAME,
                              FIELDNAME)
      tempTableCols <- tempTableCols %>%
        lapply(as.data.frame) %>%
        lapply(function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        lapply(function(df) {sapply(df, function(x) all(is.nan(x)|is.na(x)))})
      for (i in 1:length(tempTableCols)) {
        tempTableCols[[i]] <- as.data.frame(
          t(subset(tempTableCols[[i]],  tempTableCols[[i]] == FALSE))
        )
      }
      return(tempTableCols)
    },
    .getDatForCols = function(RESP_FILE,
                              DB,
                              RESPVAR,
                              YEAR,
                              FARMERNAME,
                              FIELDNAME) {
      OUT_FILE <- invisible(
        DBI::dbGetQuery(
          DB,
          paste0("
             (SELECT ", RESPVAR, ".*
             FROM  ", FARMERNAME, "_r.", RESPVAR, " ", RESPVAR, "
             JOIN all_farms.temp temp
             ON ST_Within(", RESPVAR, ".geometry, temp.geometry)
             WHERE ", RESPVAR, ".year = '", YEAR, "'
             AND ", RESPVAR, ".orig_file = '", RESP_FILE, "')
             ")
        )
      )
      return(OUT_FILE)
    },
    .getTempExpTableCols = function(EXP_FILES,
                                    DB,
                                    RESPVAR,
                                    YEAR,
                                    FARMERNAME,
                                    FIELDNAME) {
      tempTableCols <- rep(list(NULL), nrow(EXP_FILES))
      names(tempTableCols) <- EXP_FILES$orig_file
      for (i in 1:nrow(EXP_FILES)) {
        tempTableCols[[i]] <- invisible(
          DBI::dbGetQuery(
            DB,
            paste0(" (SELECT ", EXP_FILES$table[i], ".*
                     FROM  ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                     JOIN all_farms.temp temp
                     ON ST_Intersects(", EXP_FILES$table[i], ".geometry, temp.geometry)
                     WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                     AND ", EXP_FILES$table[i], ".orig_file = '", EXP_FILES$orig_file[i], "')
                     ")
          )
        )
      }
      tempTableCols <- tempTableCols %>%
        lapply(as.data.frame) %>%
        lapply(function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        lapply(function(df) {sapply(df, function(x) all(is.nan(x)|is.na(x)))})
      for (i in 1:length(tempTableCols)) {
        tempTableCols[[i]] <- as.data.frame(
          t(subset(tempTableCols[[i]], tempTableCols[[i]] == FALSE))
        )
      }
      return(tempTableCols)
    },
    .getTempExpTable = function(DB, EXP_FILES, FARMERNAME, YEAR, FIELDNAME, i) {
      tempTable <- sf::st_read(
        DB,
        query = paste0("SELECT ", EXP_FILES$table[i], ".*
                       FROM  ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                       JOIN all_farms.temp temp
                       ON ST_Intersects(", EXP_FILES$table[i], ".geometry, temp.geometry)
                       WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                       AND ", EXP_FILES$table[i], ".orig_file = '", EXP_FILES$orig_file[i], "'
                       LIMIT 1"),
        geometry_column="geometry") %>%
        as.data.frame() %>%
        (function(df) {df[, grep("geom", colnames(df))] <- NULL; return(df)}) %>%
        (function(df) {df <- df[, which(df != "NaN")];return(df)})
      return(tempTable)
    },
    .tabNames = function(SCHEMA, DB) {
      schemaOut <- DBI::dbGetQuery(
        DB,
        paste0("SELECT table_name
               FROM information_schema.tables
               WHERE table_schema='", SCHEMA, "'")
      )
      schemaOut <- schemaOut[1:nrow(schemaOut), ]
      if (length(schemaOut) == 0) {
        schemaOut <- NULL
      } else {
        schemaOut <- as.list(schemaOut[1:length(schemaOut)])
      }
      return(schemaOut)
    }
  )
)


