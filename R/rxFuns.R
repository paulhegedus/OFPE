#' @title Return a grid for creating experiments and prescriptions.
#'
#' @description Returns a grid of the user specified dimensions combined
#' with data passed in by the user. The data available for use as an
#' experiment is clipped to the grid, removing areas between the field edge
#' and the grid within the field. This is used to apply the experimental
#' or check rates.
#'
#' @param db Connection to an OFPE formatted database.
#' @param rx_dt Data frame that contains the coordinates of locations to apply
#' experimental inputs. For an experiment, these are the centroids of the grid
#' made to aggregate data for the field.
#' @param farmername Provide the farmer name that manages the field
#' @param fieldname Provide the name of the field for the experiment or prescription.
#' @param trt_length Length, in meters, for which to apply treatments.
#' @param trt_width Width, in meters, for which to apply treatments.
#' @param unique_fieldname Unique fieldname for the field(s) used for the experiment. This
#' concatenates multiple fields with an ampersand. Used for labeling.
#' @param mgmt_scen If the user is creating a prescription or experimental
#' prescription, they must provide the management scenario to use for their
#' prescription. The user can choose from the management options listed in
#' the SimClass. The options are 'SSOPT': site-specific optimized rates,
#' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
#' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
#' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
#' input rate in conventional system types, and the farmer selected rate for
#' organic systems, both of which are already provided. Default to 'base' for new
#' experiments, override if making a prescription.
#' @param buffer_width Width from the edge of the field within which to apply treatments.
#' @return NULL, grid data stored in database in 'rxgrids' and 'rxgridtemp'.
#' @export
getRxGrid <- function(db,
                      rx_dt,
                      farmername,
                      fieldname,
                      trt_length,
                      trt_width,
                      unique_fieldname,
                      mgmt_scen = "base",
                      buffer_width) {
  # browser()
  
  utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
  # remove temp tables
  OFPE::removeTempTables(db)
  OFPE::removeTempFarmerTables(db, farmername)

  OFPE::makeTempBound(db, fieldname, farmername, buffer_width)

  # make a grid and add to db if not already present (rx_grids) saved in db
  OFPE::makeRXGrid(db,
                  fieldname,
                  trt_length,
                  trt_width,
                  farmername,
                  unique_fieldname,
                  mgmt_scen)
  #Make rxDt spatial and add to database (not saved)
  rx_sdt <- rx_dt %>%
    `names<-`(names(rx_dt))
  rx_sdt$X <- rx_sdt$x
  rx_sdt$Y <- rx_sdt$y
  sp::coordinates(rx_sdt) <- c("X", "Y")
  rx_sdt <- sf::st_as_sf(rx_sdt, remove_coordinates = FALSE)
  if (is.na(raster::crs(rx_sdt))) {
    sf::st_crs(rx_sdt) <- utm_epsg
  }
  suppressMessages(rpostgis::pgInsert(
    db,
    c(paste0(farmername, "_a"), "temp"),
    as(rx_sdt, "Spatial"),
    geom = "geometry",
    new.id = "gid"
  ))
  #Add cell_id to temp table from rxgridtemp and get the mean ssopt from each cell
  # Get the cell_id for each point in the temporary table
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE ", farmername, "_a.temp
          ADD COLUMN cell_id VARCHAR;")
  ))
  invisible(DBI::dbSendQuery(
    db,
    paste0("UPDATE ", farmername, "_a.temp temp
          SET cell_id = rxgridtemp.cell_id
          FROM all_farms.rxgridtemp
          WHERE ST_Within(temp.geometry, rxgridtemp.geom);")
  ))

  # Add the mean response to the aggregated table by cell_id
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE all_farms.rxgridtemp
          ADD COLUMN expRate REAL,
          ADD COLUMN applRate REAL;")
  ))
  invisible(DBI::dbSendQuery(
    db,
    paste0("WITH vtemp AS (
          SELECT
            b.cell_id,
            to_char(
              AVG (b.base_rate),
              '9999999999999999999'
            ) AS base_rate
          FROM ", farmername, "_a.temp b
          INNER JOIN all_farms.rxgridtemp a ON a.cell_id = b.cell_id
          GROUP BY  b.cell_id
          )

          UPDATE all_farms.rxgridtemp rxgridtemp
          SET expRate = CAST ( vtemp.base_rate AS REAL )
          FROM vtemp
          WHERE rxgridtemp.cell_id = vtemp.cell_id;")
  ))

  # Clip rxgridtemp to field boundary & bring into R
  # clip raster
  invisible(DBI::dbSendQuery(
    db,
    paste0( "ALTER TABLE all_farms.rxgridtemp
          ADD COLUMN id SERIAL;")
  ))
  invisible(DBI::dbSendQuery(
    db,
    paste0( "DELETE FROM all_farms.rxgridtemp  AS rxgridtemp
          WHERE rxgridtemp.id IN (
            SELECT a.id
            FROM all_farms.rxgridtemp  a, (
              SELECT ST_Union(geometry) As geometry FROM all_farms.temp
            ) b
          WHERE NOT ST_Within(a.geom, b.geometry)
          );")
  ))
  invisible(DBI::dbSendQuery(
    db,
    paste0( "ALTER TABLE all_farms.rxgridtemp
          DROP COLUMN id;")
  ))
  rx_sdt <- sf::st_read(
    db,
    query = paste0("SELECT * FROM all_farms.rxgridtemp"),
    geometry_column = "geom") %>%
    sf::st_cast("MULTIPOLYGON")
  # remove temp tables
  OFPE::removeTempTables(db)
  OFPE::removeTempFarmerTables(db, farmername)
  return(rx_sdt)
}
#' @title Make a temporary table of field boundaries
#'
#' @description Creates a temporary table in the 'all_farms'
#' schema containing one table with all of the field boundaries
#' for the fields selected by the user.
#'
#' @param db Connection to an OFPE formatted database.
#' @param fieldname Provide the field name for the experiment or prescription.
#' @param farmername Provide the name of the farmer that manages the field.
#' @param buffer_width Width from the edge of the field within which to apply treatments.
#' @return NULL, temporary field boundary table in 'all_farms'.
#' @export
makeTempBound <- function(db, fieldname, farmername, buffer_width) {
  # browser()
  
  utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
  for (i in 1:length(fieldname)) {
    if (i == 1) {
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.temp AS
              SELECT * FROM all_farms.fields fields
              WHERE fields.fieldname = '", fieldname[i], "';")
      ))
    } else {
      invisible(DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.temp
              SELECT * FROM all_farms.fields fields
              WHERE fields.fieldname = '", fieldname[i], "';")
      ))
    }
  }
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE all_farms.temp
          RENAME COLUMN geom TO geometry;")
  ))
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE all_farms.temp
          ALTER COLUMN geometry TYPE geometry(POLYGON, ", utm_epsg, ")
          USING ST_Transform(geometry, ", utm_epsg, ");")
  ))
}
#' @title Create a grid for creating experiments and prescriptions.
#'
#' @description Creates a grid of the user specified dimensions and
#' stores in the database for later use. The grid is used to place and
#' apply the experiment or prescription outputs and reflects the dimensions
#' of the user's equipment and desired treatment lengths. The 'trt_width',
#' or treatment width is used to create a buffer from the field to take into 
#' account cleanup passes around field edges.
#'
#' @param db Connection to an OFPE formatted database.
#' @param fieldname Provide the name of the field for the experiment or prescription.
#' @param trt_length Length, in meters, for which to apply treatments.
#' @param trt_width Width, in meters, for which to apply treatments.
#' @param farmername The name of the farmer that manages the field.
#' @param unique_fieldname Unique fieldname for the field(s) used for the experiment. This
#' concatenates multiple fields with an ampersand. Used for labeling.
#' @param mgmt_scen If the user is creating a prescription or experimental
#' prescription, they must provide the management scenario to use for their
#' prescription. The user can choose from the management options listed in
#' the SimClass. The options are 'SSOPT': site-specific optimized rates,
#' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
#' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
#' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
#' input rate in conventional system types, and the farmer selected rate for
#' organic systems, both of which are already provided.
#' @return NULL, grid data stored in database in 'rxgrids' and 'rxgridtemp'.
#' @export
makeRXGrid <- function(db,
                       fieldname,
                       trt_length,
                       trt_width,
                       farmername,
                       unique_fieldname,
                       mgmt_scen) {
  # browser()
  
  utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
  size <- paste0(trt_width, " x ", trt_length)
  grids_exist <- FALSE
  field_exist <- FALSE
  # check if grids exist
  grids_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
               SELECT 1
               FROM information_schema.tables
               WHERE  table_schema = 'all_farms'
               AND table_name = 'rxgrids')")
    )
  )
  # if grids exists check if field exists
  if (grids_exist) {
    # check if field has a grid
    field_exist <- as.logical(
      DBI::dbGetQuery(
        db,
        paste0("SELECT EXISTS (
                 SELECT 1
                 FROM all_farms.rxgrids rxgrids
                 WHERE rxgrids.field = '", unique_fieldname, "'
                 AND rxgrids.size = '", size, "'
                 AND rxgrids.mgmt_scen = '", mgmt_scen, "')")
      )
    )
    # if it does copy to gridtemp
    if (field_exist) {
      invisible(
        DBI::dbSendQuery(
          db,
          paste0("CREATE TABLE all_farms.rxgridtemp AS
                SELECT *
                FROM all_farms.rxgrids
                WHERE field = '", unique_fieldname, "'
                AND size = '", size, "'
                AND rxgrids.mgmt_scen = '", mgmt_scen, "'")
        )
      )
    }
  }
  # if field does not exists
  if (!field_exist) {
    BBOX <- sf::st_read(
      db,
      query = paste0("SELECT * FROM all_farms.temp"),
      geometry_column = "geometry") %>%
      sf::st_transform(paste0("epsg:", utm_epsg)) %>%
      as("Spatial") %>%
      sp::bbox()
    NCOL <- ceiling((BBOX["x", "max"] - BBOX["x", "min"]) / trt_width)
    NROW <- ceiling((BBOX["y", "max"] - BBOX["y", "min"]) / trt_length)

    invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE TABLE all_farms.rxgridtemp AS
               SELECT *
               FROM ST_CreateFishnet (", NROW, ", ", NCOL, ", ", trt_width, ", ", trt_length, ", ", BBOX["x", "min"], ", ", BBOX["y", "min"], ") AS cells;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.rxgridtemp
               ADD COLUMN cell_id VARCHAR,
               ADD COLUMN field VARCHAR,
               ADD COLUMN size VARCHAR,
               ADD COLUMN length double precision,
               ADD COLUMN width double precision,
               ADD COLUMN cell_type VARCHAR,
               ADD COLUMN mgmt_scen VARCHAR;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.rxgridtemp SET
               cell_id = row::text ||'_'|| col::text,
               field = '", unique_fieldname, "',
               size = '", size, "',
               length = ", trt_length, ",
               width = ", trt_width, ",
               cell_type = '", mgmt_scen, "',
               mgmt_scen = '", ifelse(mgmt_scen == "base", "exp", mgmt_scen), "';")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.rxgridtemp SET geom = ST_SetSRID (geom, ", utm_epsg, ");")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.rxgridtemp
               ADD COLUMN x double precision,
               ADD COLUMN y double precision;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.rxgridtemp SET
               x = ST_X(ST_Centroid(geom)),
               y = ST_Y(ST_Centroid(geom));")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.rxgridtemp
               ADD PRIMARY KEY (cell_id, field);")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE INDEX rxgridtemp_geom_idx
               ON all_farms.rxgridtemp
               USING gist (geom);")
    ))
  }
  # if boundary_import = no, field does not exist in grids
  if (!field_exist) {
    if (grids_exist) { # if grids_exists append
      invisible(DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.rxgrids
                 SELECT * FROM all_farms.rxgridtemp;")
      ))
    } else { # if not create grids
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.rxgrids AS
                 SELECT * FROM all_farms.rxgridtemp;")
      ))
    }
    invisible(DBI::dbSendQuery(
      db, paste0("VACUUM ANALYZE all_farms.rxgrids")
    ))
  }
  invisible(DBI::dbSendQuery(
    db, paste0("VACUUM ANALYZE all_farms.rxgridtemp")
  ))
}
#' @title Get field(s) boundaries from OFPE database.
#'
#' @description Gather and combine field boundaries from one or
#' more selected fields from an OFPE database. Fields must be from
#' the same farmer.
#'
#' @param fieldname The field(s) names(s) present in the database to get
#' field boundaries for.
#' @param db Connection to an OFPE formatted database.
#' @param farmername Provide the name of the farmer that owns or manages the
#' field(s) that an prescription is going to be generated for.
#' @return A field boundary sf object.
#' @export
getFldBound = function(fieldname, db, farmername) {
  utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
  fld_bound_list <- as.list(fieldname)
  for (i in 1:length(fieldname)) {
    fld_bound_list[[i]] <- sf::st_read(
      db,
      query = paste0("SELECT *
                       FROM all_farms.fields
                       WHERE fieldname = '", fieldname[i], "'"),
      geometry_column = "geom") %>%
      sf::st_transform(paste0("epsg:", utm_epsg)) %>%
      sf::st_cast("MULTIPOLYGON")
  }
  fld_bounds <- do.call(rbind, fld_bound_list)
  return(fld_bounds)
}
#' @title Apply experimental rates across the field
#'
#' @description This function applies experimental rates across a field
#' randomly, or stratified on user supplied data. The arguments contain
#' an 'sf' object with the grid cells available for applying rates to.
#' Based on the experimental rates and associated proportions.
#'
#' @param rx_sdt A 'sf' object containing the grid cells available for
#' applying rates to.
#' @param exp_rates Provide a vector of experimental rates equal to the
#' number of experimental rates provided in 'exp_rate_length'. This is
#' required for all new experiments, however can be left to null for
#' experimental prescriptions if experimental rates should be generated
#' based on gaps in optimum rates.
#' @param exp_rates_prop Provide proportions (0 - 1) for the length
#' of experimental rates provided in 'exp_rate_length'. This is required
#' for all new experiments and experimental prescriptions.
#' @param fld_prop The proportion of the field to apply experimental
#' rates to (i.e. 0.5 or 1.0), OR, the percent of available cells to
#' apply check rates to (i.e. 0.05, 0.1).
#' @param exp_rate_length Provide the length of experimental rates to apply.
#' This applies to new experiments and experimental prescriptions. This
#' represents the equipment constraints of the farmer. In the case of the
#' experimental prescription, this number of rates does not include the
#' number of rates for the optimized base map, so take your selections for
#' the management scenario and number of optimum rates into account.
#' @return Amended 'rx_sdt' object wit randomly placed experimental
#' rates applied.
#' @export
applyExpRates = function(rx_sdt,
                         exp_rates,
                         exp_rates_prop,
                         fld_prop,
                         exp_rate_length) {
  stopifnot(length(exp_rates) == exp_rate_length,
            length(exp_rates_prop) == exp_rate_length)
  
  # browser()
  ## TODO : should be for each field??
  fieldname <- unique(rx_sdt$fieldname)
  rx_sdt$fid <- 1:nrow(rx_sdt)
  for (i in 1:length(fieldname)) {
    temp_rx_sdt <- rx_sdt[rx_sdt$fieldname == fieldname[i], ]
    exp_cells <- DescTools::RoundTo(nrow(temp_rx_sdt) * fld_prop, 1, floor)
    exp_cells <- sample(row.names(temp_rx_sdt), exp_cells)
    exp_reps <- OFPE::getExpReps(length(exp_cells), exp_rates_prop)
    exp_rate_rep <- rep(exp_rates, exp_reps) %>% sample()

    temp_rx_sdt[exp_cells, "exprate"] <- exp_rate_rep
    temp_rx_sdt[exp_cells, "cell_type"] <- "exp"
    rx_sdt[temp_rx_sdt$fid, ] <- temp_rx_sdt
  }
  rx_sdt$fid <- NULL
  return(rx_sdt)
}

#' @title Apply experimental rates across the field
#'
#' @description This function gets the number of cells to apply
#' experimental rates to. Takes the length of available cells for
#' experimentation and the proportion of the experiment dedicated
#' to each experimental rate.
#'
#' @param exp_cells_length The length of available cells for applying
#' experimental rates to.
#' @param exp_rates_prop Provide proportions (0 - 1) for the length
#' of experimental rates provided in 'exp_rate_length'. This is required
#' for all new experiments and experimental prescriptions.
#' @return The number of cells to apply each rate to.
#' @export
getExpReps = function(exp_cells_length, exp_rates_prop) {
  exp_reps <- ceiling(exp_cells_length * exp_rates_prop)
  if (sum(exp_reps) != exp_cells_length) {
    cell_diff <- sum(exp_reps) - exp_cells_length
    midpoint <- floor(median(1:length(exp_reps)))
    fillNA <- rep(NA, length(exp_reps) *
                    ceiling(cell_diff/length(exp_reps)) - cell_diff)
    cell_diff_mat <- matrix(c(1:cell_diff, fillNA),
                            ncol = length(exp_reps),
                            nrow = ceiling(cell_diff / length(exp_reps)),
                            byrow = TRUE)
    for (i in 1:nrow(cell_diff_mat)) {
      exp_reps <- OFPE::trimExpReps(na.omit(cell_diff_mat[i, ]),
                                       exp_reps,
                                       midpoint)
    }
  }
  return(exp_reps)
}
#' @title Trim excess experimental rates
#'
#' @description This function trims the number of cells to apply
#' experimental rates to the number of available experimental rates.
#' Due to rounding errors with proportions, more cells are selected
#' than available. This removes reps from median rates first before
#' moving to the extremes because it is assumed that the majority of
#' optimum or other base rates will be around the median experimental
#' rate. Additionally, when the applicator is moving from low to high
#' or high to low rates, these middle rates will be applied. The minimum
#' number of cells to apply a rate to is 1.
#'
#' @param cell_diff Vector with the number of cells difference between
#' the number of rates and the number of available cells for experimentation
#' @param exp_reps The number or cells to apply to each
#' epxerimental rate.
#' @param midpoint The median rate number for removing reps from middle
#' rates before the extremes.
#' @return The number of cells to apply each rate to.
#' @export
trimExpReps = function(cell_diff, exp_reps, midpoint) {
  for (i in 1:length(cell_diff)) {
    if (i == 1) {
      exp_reps[midpoint] <- exp_reps[midpoint] - 1
    } else {
      even <- ifelse((as.integer(i / 2) - i / 2) == 0, TRUE, FALSE)
      if (even) {
        ind <- ifelse(i == 2, i - 1, ind + 1)
        exp_reps[midpoint + ind] <- ifelse(
          exp_reps[midpoint + ind] - 1 < 0,
          exp_reps[midpoint] <- exp_reps[midpoint] - 1,
          exp_reps[midpoint + ind] - 1
        )

      } else {
        exp_reps[midpoint - ind] <- ifelse(
          exp_reps[midpoint - ind] - 1 < 0,
          exp_reps[midpoint] <- exp_reps[midpoint] - 1,
          exp_reps[midpoint - ind] - 1
        )
      }
    }
  }
  return(exp_reps)
}
#' @title Trim excess experimental rates
#'
#' @description This function takes field boundaries from the OFPE
#' database, sets the base rate, and adds columns required to merge
#' with the experiment or prescription output.
#' @param db Connection to an OFPE formatted database.
#' @param fieldname If the user is creating a new experiment, provide or
#' select the fieldname of the field to use. The field list is from the
#' available fields in the database for experimentation.
#' @param unique_fieldname Unique fieldname for the field(s) used for the experiment. This
#' concatenates multiple fields with an ampersand. Used for labeling.
#' @param base_rate The rate to apply between the experimental rates
#' and the field edge, or as check rates in the prescription selected
#' option.
#' @param trt_width Width, in meters, for which to apply treatments.
#' @param trt_length Length, in meters, for which to apply treatments.
#' @param farmername If the user is creating a new experiment, provide or
#' select the name of the farmer that owns or manages the field(s) that
#' an experiment is going to be generated for. Must be same for all
#' fields.
#' @param mgmt_scen If the user is creating a prescription or experimental
#' prescription, they must provide the management scenario to use for their
#' prescription. The user can choose from the management options listed in
#' the SimClass. The options are 'SSOPT': site-specific optimized rates,
#' 'FFOPT': full-field optimum uniform rate, 'FS': farmer selected uniform
#' rate, 'Min': applying the least intensive input rates (i.e. 0 lbs N/ac, or
#' 25 lbs seed/ac), 'Opp' is omitted because this strategy is the least intensive
#' input rate in conventional system types, and the farmer selected rate for
#' organic systems, both of which are already provided. Default to 'base' for new
#' experiments, override if making a prescription.
#' @return A 'sf' object with field boundaries and the base rate.
#' @export
makeBaseRate = function(db,
                        fieldname,
                        unique_fieldname,
                        base_rate,
                        trt_width,
                        trt_length,
                        farmername,
                        mgmt_scen) {
  fld_bound <- OFPE::getFldBound(fieldname, db, farmername)
  # names(fld_bound)[grep("fieldname", names(fld_bound))] <- "field"

  fld_bound$exprate <- base_rate
  fld_bound$row <- rep(NA, nrow(fld_bound))
  fld_bound$col <- rep(NA, nrow(fld_bound))
  fld_bound$cell_id <- fld_bound$wfid
  fld_bound$field <- unique_fieldname
  fld_bound$width <- trt_width
  fld_bound$length <- trt_length
  fld_bound$cell_type <- "base"
  cols <- grep("fieldidx|wfid|farmidx|farmeridx|area", names(fld_bound))
  fld_bound[, cols] <- NULL
  coords <- suppressWarnings(sf::st_centroid(fld_bound) %>% sf::st_coordinates())
  fld_bound$x <- coords[, 1]
  fld_bound$y <- coords[, 2]
  fld_bound$applrate <- NA
  fld_bound$size <- paste0(trt_width, " x ", trt_length)
  fld_bound$mgmt_scen <- mgmt_scen
  fld_bound$strat_combo <- NA
  # fld_bound$fieldname <- NA
  return(fld_bound)
}
#' @title Trim grid to field buffer
#'
#' @description This function takes the experiment or prescription grid
#' from the database and trims it to the boundary of the treatment width
#' buffer from the edge of the field.
#' @param rx_sdt A 'sf' object containing the experiment or prescription
#' grid generated by user inputs.
#' @param fieldname Provide the name of the field for the experiment or prescription.
#' @param db Connection to an OFPE formatted database.
#' @param farmername Name of the farmer that owns or manages the field(s) that
#' an experiment or prescription is going to be generated for. Must be same for all
#' fields.
#' @param buffer_width Width from the edge of the field within which to apply treatments.
#' @return A 'sf' object with a clipped grid for experimentation or prescription generation.
#' @export
trimGrid <- function(rx_sdt, fieldname, db, farmername, buffer_width) {
  # browser()
  
  buff_bound <- sf::st_buffer(
    OFPE::getFldBound(fieldname, db, farmername), -buffer_width
  )
  rx_sdt <- suppressWarnings(sf::st_intersection(rx_sdt, buff_bound))
  rx_sdt[, c("wfid", "fieldidx", "farmidx",
             "farmeridx", "fieldname", "area")] <- NULL
  return(rx_sdt)
}
#' @title Make experiment or prescription
#'
#' @description This function takes the experiment or prescription 'sf'
#' object as well as the field boundary 'sf' object. The area of the experiment
#' or prescription is subtracted from the field boundary, and the two are
#' combined to create one output with the base rate around the experiment or
#' prescription rates.
#'
#' While named 'makeRx', this is the function for creating new experiments
#' as well.
#' @param rx_sdt A 'sf' object containing the experiment or prescription
#' generated by user inputs.
#' @param fld_bound A 'sf' object containing the field boundary(ies) with
#' a base rate application.
#' @param rx_for_year The year that the prescription or experiment
#' is made for. Added to a column to specify the data.
#' @param conv The conversion factor between lbs of the input to the
#' units of the as-applied input (i.e. lbs N/ac to gal urea/ac).
#' Should be from RxClass object or is 1 / the conversion factor to
#' go from lbs of the experimental input to units of the as-applied
#' input.
#' @param strat_dat_parms Optional, only if stratification used to make the 
#' experimental rates. Named list by fieldname that contains a list for each field
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
#' @return A 'sf' object with the final output for the producer.
#' @export
makeRx <- function(rx_sdt, fld_bound, rx_for_year, conv, strat_dat_parms = NULL) {
  if (!is.null(strat_dat_parms)) {
    unq_col_names <- vector("character")
    for (i in 1:length(strat_dat_parms)) {
      unq_col_names <- c(unq_col_names, paste0(strat_dat_parms[[i]]$col_name, "_f")) %>% 
        unique()
    }
    rm_cols <- grep(paste(unq_col_names, collapse = "|"), names(rx_sdt))
    rx_sdt[, rm_cols] <- NULL
  }
  # clip fld bound to area around the prescription rates and within field bounds
  clip_field <- suppressWarnings(as(fld_bound, "Spatial") -
                                   as(rx_sdt, "Spatial"))
  fld_bound <-  as(clip_field, "sf")
  names(rx_sdt)[grep("geom", names(rx_sdt))] <- "geometry"
  attr(rx_sdt, "sf_column") <- "geometry"
  # bind together for output data
  RX <- rbind(rx_sdt, fld_bound)
  # add year of rx
  RX$rxyear <- rx_for_year
  # make conversion
  RX$applrate <- RX$exprate * conv
  # make column with size in feet 
  RX$size_ft <- paste0(DescTools::RoundTo(RX$width / 0.3048, 5), 
                       " x ", 
                       DescTools::RoundTo(RX$length / 0.3048, 5))
  return(RX)
}


#' @title Fetch data for stratification
#'
#' @description This function fetches user specified data from an OFPE database.
#' This function is for gathering stratified data from an aggregated table. A 
#' much simpler query can easily be written for raw data using the original 
#' filename. This function can be executed with the 'strat_var' variable set to
#' NULL (default) to return all columns in the database. If 'strat_var' is provided,
#' the 'x', 'y', 'cell_id', and 'field' columns will be returned, as well as the 
#' column corresponding to the 'strat_var'. 
#' 
#' This function queries the aggregated database sequentially for different combinations
#' of the 'grid' and 'datused' parameters. The aggregated table is specified by the 
#' 'table_var', and the schema name is created from the 'farmername' parameter. 
#' This table is queried for data with the user specified 'field' for the 
#' stratification data, 'year' of the stratification data, and the size of the 
#' grid used for cleaning and aggregation ('grid_size'). The user must also 
#' specify the connection to an OFPE formatted database.
#' @param table_var The name of the table in the farmer's aggregated schema ('_a')
#' that contains the stratification data.
#' @param strat_var Optional, default is NULL. Leave NULL to return all columns 
#' in the specified data. Otherwise provide the column name with the variable for 
#' stratification. This returns the selected column and 'x', 'y', 'cell_id', 
#' and 'field'.
#' @param field Field that the stratification data is from.
#' @param year The year to gather the stratification for.
#' @param farmername The farmer that manages the field the stratification data 
#' is from.
#' @param grid_size The size of the grid (in meters) that was used to clean and 
#' aggregate the data on. Even if observed data points are used they were cleaned
#' using a specified grid size, which is what is passed in here. 
#' @param db Connection to an OFPE formatted database.
#' @return A data frame with either all colums or the 'x', 'y', 'cell_id', 
#' 'field', and strat_var columns.
#' @export
fetchAggStratDat = function(table_var, 
                         strat_var = NULL, 
                         field, 
                         year, 
                         farmername, 
                         grid_size,
                         db) {
  if (is.null(strat_var)) {
    fetch_cols <- "*"
  } else {
    fetch_cols <- c("x", "y", "cell_id", "field", strat_var)
  }
  strat_df <- suppressWarnings(DBI::dbGetQuery(
    db,
    paste0("SELECT ", paste(fetch_cols, collapse = ", "), "  FROM ",
           farmername, "_a.", table_var, " 
           WHERE field = '", field, "'
           AND year = '", year, "'
           AND grid = 'grid'
           AND datused = 'decision_point'
           AND size = '", grid_size, "';")))
  if (nrow(strat_df) == 0) {
    strat_df <- suppressWarnings(DBI::dbGetQuery(
      db,
      paste0("SELECT ", paste(fetch_cols, collapse = ", "), "  FROM ",
             farmername, "_a.", table_var, " 
             WHERE field = '", field, "'
             AND year = '", year, "'
             AND grid = 'obs'
             AND datused = 'decision_point'
             AND size = '", grid_size, "';")))
  }
  if (nrow(strat_df) == 0) {
    strat_df <- suppressWarnings(DBI::dbGetQuery(
      db,
      paste0("SELECT ", paste(fetch_cols, collapse = ", "), "  FROM ",
             farmername, "_a.", table_var, " 
             WHERE field = '", field, "'
             AND year = '", year, "'
             AND grid = 'grid'
             AND datused = 'full_year'
             AND size = '", grid_size, "';")))
  }
  if (nrow(strat_df) == 0) {
    strat_df <- suppressWarnings(DBI::dbGetQuery(
      db,
      paste0("SELECT ", paste(fetch_cols, collapse = ", "), "  FROM ",
             farmername, "_a.", table_var, " 
             WHERE field = '", field, "'
             AND year = '", year, "'
             AND grid = 'obs'
             AND datused = 'full_year'
             AND size = '", grid_size, "';")))
  }
  if (nrow(strat_df) == 0) {
    stop(print(paste0("NO ", strat_var, " DATA AVAILABLE.")))
  }
  return(strat_df)
}