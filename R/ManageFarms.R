#' @title R6 Class for adding or updating farm information in OFPE database.
#'
#' @description R6 class that takes a database connection object, path to
#' the folder with the farm boundaries, and a list with farm information to
#' upload. Uploads boundaries to the database. Adds a unique ID field for
#' each farm that is used for internal relationships. If a farm is already
#' present in the database, it's information will be updated. This
#' follows the ManageDB interface and includes a setup and execute method that
#' is called from the ManageDB class.
#' @export
ManageFarms <- R6::R6Class(
  "ManageFarms",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field farm_info List of farms to update, with each element a list with a
    #' 'farm_name' field for the name of the farm, a 'farm_shp_name' field with
    #' the name of the shapefile with the farm boundary, and a field called
    #' 'farmer_name' with the name of the farmer who own the farm.
    farm_info = NULL,
    #' @field farm_path File path to the location of farm boundary shapefiles.
    farm_path = NULL,
    #' @field farms List with data prepared for upload to the database. Contains
    #' shapefiles with a column indicating the associated farmer name.
    farms = NULL,

    #' @description
    #' Create an ManageFarms object. The database connection and a list with
    #' farm information used to upload farm information to the database with
    #' the appropriate owner information. The farm_path argument indicates the
    #' file path in which to follow for the farm boundary shapefiles that are
    #' to be uploaded to the database. Instantiates the class for running the
    #' setup and execute methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param action_item List of inputs to the AddFarms method, from the
    #' list passed into the ManageDB class. This includes a list of farm
    #' information and a path to where the farm boundaries are stored.
    #' @return A new initialized 'ManageFarms' object.
    initialize = function(dbCon, action_item) {
      stopifnot(
        !is.null(action_item),
        !is.null(action_item$farms),
        !is.null(action_item$farm_path)
      )
      self$dbCon <- dbCon
      self$farm_info <- action_item$farms %>%
        lapply(function(x) lapply(x, tolower))
      self$farm_path <- action_item$farm_path
    },
    #' @description
    #' Prepares the farm data prior to uploading to the database. Imports
    #' shapefiles from the farm path and puts them into a list with the
    #' corresponding farmer to be uploaded to the database.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Prepared R6 class for uploading farmer information.
    setup = function() {
      self$farms <- lapply(self$farm_info, self$.setupFarms, self$farm_path)
    },
    #' @description
    #' Executes the upload of prepared farm information to the database in the
    #' 'all_farms' schema and the 'farms' table. If a farm boundary is already
    #' present with the same farm name and farmer that owns it, the entry in the
    #' database is replaced.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Farm data uploaded into the 'all_farms.farms' table in the database.
    execute = function() {
      lapply(self$farms, self$.uploadFarms, self$dbCon$db)
    },
    #' @description
    #' Converts a list with information about the farms into a format ready for
    #' uploading to the database. Imports the shapefile using the path name
    #' in the list provided and adds a column for the farmer name. Converts
    #' to EPSG 4326 for storage.
    #' @param farm_info List with information about the farm to uplaod.
    #' @param farm_path Path to the location of farm boundary shapefiles.
    #' @return ESRI shapefile of farm boundary.
    .setupFarms = function(farm_info, farm_path = NULL) {
      farm <- sf::st_read(paste0(self$farm_path,
                                 farm_info$farm_shp_name,
                                 ".shp"),
                          quiet = TRUE) %>%
        sf::st_zm() %>%
        sf::st_cast()
      farm$farm <- farm_info$farm_name
      farm$farmer <- farm_info$farmer_name
      if (any(grepl("geometry", names(farm)))) {
        geom <- farm$geometry
        farm <- sf::st_drop_geometry(farm)
        farm$geom <- geom
        farm <- sf::st_as_sf(farm) %>%
          sf::st_cast()
      }
      farm <- farm %>%
        sf::st_set_crs(4326) %>%
        sf::st_transform(4326)
      # farm$area <- sf::st_area(farm) %>%
      #   units::set_units("acre")
      return(farm)
    },
    #' @description
    #' Uploads a farm to the database. Takes the farm shapefile and identifies the
    #' coresponding farmeridx, adds it as a column, and then uploads to the
    #' 'all_farms.farms' table. Makes sure there is no conflict defined in the
    #' .buildTables method of the BuildDB class. A farmidx is automatically
    #' generated upon upload by PostgreSQL and the area of the farm is calculated
    #' and converted to acres. The dot indicates that this function would be private
    #' if not for documentations sake.
    #' @param farm Farm shapefile for upload into 'all_farms.farmers'.
    #' @param db Database connection.
    #' @return Farm upload into database.
    .uploadFarms = function(farm, db) {
      farm$farmeridx <- DBI::dbGetQuery(
        db,
        paste0("SELECT farmeridx
               FROM all_farms.farmers
               WHERE farmer = '", unique(farm$farmer), "'")
      )$farmeridx

      farm <- farm[, c("farm", "farmeridx","geom")]
      suppressMessages(
        rpostgis::pgInsert(db,
                           c("all_farms","farms"),
                           sf::as_Spatial(farm),
                           partial.match = TRUE,
                           upsert.using = c("farm", "farmeridx"))
      )
      DBI::dbSendQuery(
        db,
        "UPDATE all_farms.farms
          SET area = ST_AREA(geom::geography) * 0.000247105;"
      )
    }
  )
)

