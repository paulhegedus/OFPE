#' @title Set NA values to NaN
#'
#' @description Set Na values to NaN in all columns of a data.
#' This converts NA values in R to NaN's compatible with the
#' PostgreSQL database. This is done prior to import into the
#' database.
#'
#' @param dat Data to check for NA's and convert to NaN's.
#' @return Data with NA values converted to NaN.
#' @export
setNAtoNaN <- function(dat) {
  for (j in 1:ncol(dat)) { # convert NA to NaN for db import
    if (anyNA(as.data.frame(dat[, j])[1])) {
      na_index <- which(is.na(as.data.frame(dat[, j])[1]))
      dat[na_index, j] <- NaN
    }
  }
  return(dat)
}
#' @title Set columns in database table to text.
#'
#' @description Set column data type in PostgreSQL database
#' to text for storage. This is done after data import.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat_names Names of columns in data that was imported
#' to the database that are converted to a text data format in the
#' database.
#' @param schema Name of the schema with the table to convert
#' columns to text in.
#' @param dtype The name of the table in the schema
#' to convert the column names to text for.
#' @return Columns in database table set to text data type.
#' @export
makeDBcolsText = function(db, dat_names, schema, dtype) {
  for (j in 1:length(dat_names)) {
    tryCatch({
      tt <- DBI::dbSendQuery(db,
                       paste0("ALTER TABLE ", schema, ".", dtype,
                              " ALTER COLUMN ", dat_names[j], " TYPE TEXT"))
      DBI::dbClearResult(tt)
    },
    error = function(e) {print(paste0("warning in column ", dat_names[j]))})
  }
}
#' @title Convert polygon table geometry
#'
#' @description Convert a table with a Polygon geometry to a
#' table with a MultiPolygon geometry.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat Data of 'sp' object that was imported to the
#' specified schema and table to convert to multipolygon
#' geometry. Must have a column named 'geometry'.
#' @param schema Name of the schema with the table to convert
#' geometry in.
#' @param dtype The name of the table to convert geometry in.
#' @return Polygon geometry type converted to MultiPolygon.
#' @export
convPolyToMulti = function(db, dat, schema, dtype) {
  if (any(grepl("poly", class(sf::st_geometry(sf::st_as_sf(dat))),
                ignore.case = TRUE))) {
    if (!any(grepl("multi", class(sf::st_geometry(sf::st_as_sf(dat))),
                   ignore.case = TRUE))) {
      tt <- invisible(DBI::dbSendQuery(
        db,
        paste0("ALTER TABLE ", schema, ".", dtype, " ALTER COLUMN geometry
               SET DATA TYPE geometry;")
      ))
      DBI::dbClearResult(tt)
      tt <- invisible(DBI::dbSendQuery(
        db,
        paste0("ALTER TABLE ", schema, ".", dtype, "
               ALTER COLUMN geometry TYPE geometry(MultiPolygon, 4326)
               USING ST_Multi(geometry)")
      ))
      DBI::dbClearResult(tt)
    }
  }
}
#' @title Make spatial index
#'
#' @description Create a spatial index on a table in a
#' PostgreSQL database. Provide the index name which
#' must be unique across the database, and the schema
#' and table name.
#'
#' @param db Database connection to OFPE formatted database.
#' @param index_name The name of the spatial index to create.
#' @param schema Name of the schema with the table to convert
#' geometry in.
#' @param dtype The name of the table to convert geometry in.
#' @return Spatial index in the OFPE database for the given table.
#' @export
makeSpatialIndex = function(db, index_name, schema, dtype) {
  tryCatch({
    tt <- invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE INDEX ", index_name,
             " ON ", schema, ".", dtype,
             " USING GIST (geometry)")
    ))
    DBI::dbClearResult(tt)},
    error = function(e) {
      print(paste0("Error creating spatial index for ", schema, ".", dtype))
    }
  )
}
#' @title Create a new table in an OFPE database
#'
#' @description Create a new table in an OFPE database
#' by inserting a 'sp' data object from R. It is assumed
#' that the geometry column in the data is named 'geometry'.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat Data to be imported to the database to form the
#' new table. Must be 'sp' object.
#' @param schema Name of the schema to make.
#' @param dtype The name of the table to make.
#' @return NULL, a new table created in the database.
#' @export
importNewDat = function(db, dat, schema, dtype) {
  tryCatch({
    suppressMessages(rpostgis::pgInsert(
      db,
      c(schema, dtype),
      dat,
      geom = "geometry",
      new.id = "gid"
    ))
  },
  error = function(e) {
    print(paste0("Error importing data to ", schema, ".", dtype))
  })
}
#' @title Standardize columns in data and database table
#'
#' @description Add columns that are present in the data and not
#' in the database table to the database table and add the
#' columns present in the database table not present in the data
#' to the data.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat Data to be imported to the database.
#' @param schema Name of the schema where the target table is.
#' @param dtype The name of the target table.
#' @return Data with new columns, new columns in database table.
#' @export
standardizeColNames <- function(db, dat, schema, dtype) {
  db_cols <- DBI::dbGetQuery(db, paste0("SELECT column_name
                                   FROM information_schema.columns
                                   WHERE table_schema = '", schema, "'
                                   AND table_name = '", dtype, "'"))[, 1]
  db_cols <- db_cols[c(-which(db_cols == "gid"), -which(db_cols == "geometry"))]
  # get columns from db that are not in file & columns from file that are not in db
  in_db <- OFPE::noMatch(db_cols, names(dat))
  in_df <- OFPE::noMatch(names(dat), db_cols)
  # add cols from db to file and cols from file to db
  if (length(in_db) > 0) {
    row.names(dat) <- as.character(seq(1, nrow(dat), 1))
    from_db <- as.data.frame(matrix(NA, nrow(dat), length(in_db))) %>%
      `names<-`(in_db)
    dat <- maptools::spCbind(dat, from_db)
  }
  if (length(in_df) > 0) {
    for (j in 1:length(in_df)) {
      tt <- DBI::dbSendQuery(db,
                      paste0("ALTER TABLE ", schema, ".", dtype,
                             " ADD COLUMN \"", in_df[j], "\" TEXT"))
      DBI::dbClearResult(tt)
    }
  }
  return(dat)
}
#' @title Get names in source present in target
#'
#' @description Function for identifying which characters
#' in a source vector are present in a target vector. Used
#' to identify which columns don't match in database table
#' and data column names.
#'
#' @param source_vec Character vector to check if present in
#' target.
#' @param target_vec Character vector to check if source elements
#' are present in.
#' @return Elements of source vector not present in target.
#' @export
noMatch = function(source_vec, target_vec) {
  in_source_vec <- source_vec[!source_vec %in% target_vec]
  return(in_source_vec)
}
#' @title Check if table exists
#'
#' @description Check if a table exists in the OFPE database
#' by checking for the user provided schema and table.
#'
#' @param db Connection to OFPE database.
#' @param schema Name of the schema where the target table is.
#' @param dtype The name of the target table.
#' @return Logical, if table exists in database.
#' @export
tabExist <- function(db, schema, dtype) {
  tab_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
                  SELECT 1
                  FROM information_schema.tables
                  WHERE table_schema = '", schema, "'
                  AND table_name = '", dtype, "')")
  ) %>% 
    as.numeric() %>% 
    as.logical()
  return(tab_exist)
}
#' @title Add data to an existing table
#'
#' @description Adds data to an existing database table.
#' It is assumed that the geometry column in the data is
#' named 'geometry'.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat Data to be imported to the database to form the
#' new table. Must be 'sp' object.
#' @param schema Name of the schema with table.
#' @param dtype The name of the table to add data to
#' @param upserts Columns in the data to check against for
#' appending data.
#' @return NULL, data added to table in database.
#' @export
importDat = function(db, dat, schema, dtype, upserts) {
  tryCatch({
    suppressMessages(rpostgis::pgInsert(
      db,
      c(schema, dtype),
      dat,
      geom = "geometry",
      new.id = "gid",
      upsert.using = upserts
    ))
  },
  error = function(e) {
    print(paste0("Error importing data to ", schema, ".", dtype))
  })
}
#' @title Add data to an existing table
#'
#' @description Adds data to an existing database table.
#' It is assumed that the geometry column in the data is
#' named 'geometry'.
#'
#' @param db Database connection to OFPE formatted database.
#' @param dat Data to be imported to the database to form the
#' new table. Must be 'sp' object.
#' @param schema Name of the schema with target table.
#' @param dtype The name of the table to add data to.
#' @return Data uploaded to the database.
#' @export
importMulti <- function(db, dat, schema, dtype) {
  OFPE::importNewDat(db, dat, schema, "temp2")
  OFPE::convPolyToMulti(db, dat, schema, "temp2")
  rm(dat) # free up space
  temp_dat <- sf::st_read(
    db,
    query = paste0("SELECT * FROM ", schema, ".temp2")
  ) %>%
    sf::`st_crs<-`(4326) %>%
    sf::st_transform("epsg:4326") %>%
    sf::st_cast("MULTIPOLYGON")
  tryCatch({
    sf::st_write(temp_dat,
                 db,
                 c(schema, dtype),
                 layer_options = "OVERWRITE=false",
                 append = TRUE)
  },
  error = function(e) {
    print(paste0("Data already exists in ", schema, ".", dtype))
  })
  tt <- invisible(DBI::dbSendQuery(db, paste0("DROP TABLE ", schema, ".temp2")))
  DBI::dbClearResult(tt)
}
