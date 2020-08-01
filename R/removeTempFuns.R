#' @title Remove temporary tables from 'all_farms' schema
#'
#' @description Removes any temporary tables from the 'all_farms' schema of an
#' OFPE formatted database.
#'
#' @param DB Connection to an OFPE formatted database.
#' @return NULL, tables removed in database.
#' @export
removeTempTables <- function(DB){
  ## remove old temp bboxes (user input bboxes)
  geomtempExist <- as.logical(
    DBI::dbGetQuery(
      DB,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM   information_schema.tables
             WHERE  table_schema = 'all_farms'
             AND table_name = 'temp')")
    )
  )
  if(geomtempExist){
    invisible(
      DBI::dbGetQuery(
        DB,
        paste0("DROP TABLE all_farms.temp")
      )
    )

  }
  ## remove any temporary grids
  gridtempExist <- as.logical(
    DBI::dbGetQuery(
      DB,
      paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'gridtemp')")
    )
  )
  if(gridtempExist){
    invisible(
      DBI::dbGetQuery(
        DB,
        paste0("DROP TABLE all_farms.gridtemp")
      )
    )
  }
  ## remove any temporary geetemp
  geetempExist <- as.logical(
    DBI::dbGetQuery(
      DB,
      paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'geetemp')")
    )
  )
  if(geetempExist){
    invisible(
      DBI::dbGetQuery(
        DB,
        paste0("DROP TABLE all_farms.geetemp")
      )
    )
  }
}
#' @title Remove temporary tables from a farmer's schemas
#'
#' @description Removes any temporary tables from the schemas
#' in an OFPE formatted database of the specified farmer. .
#'
#' @param DB Connection to an OFPE formatted database,
#' @param FARMERNAME Name of the farmer corresponding to the schemas to search
#' in for removal of temporary tables.
#' @return NULL, tables removed in database.
#' @export
removeTempFarmerTables <- function(DB,FARMERNAME){
  ## remove old temporary tables
  tempExist <- as.logical(
    DBI::dbGetQuery(
      DB,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",FARMERNAME,"_a'
             AND table_name = 'temp')")
    )
  )
  if(tempExist){
    invisible(
      DBI::dbGetQuery(
        DB,
        paste0("DROP TABLE ",
               FARMERNAME,
               "_a.temp")
      )
    )
  }
  tempExist <- as.logical(
    DBI::dbGetQuery(
      DB,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",FARMERNAME,"_r'
             AND table_name = 'temp')")
    )
  )
  if(tempExist){
    invisible(
      DBI::dbGetQuery(
        DB,
        paste0("DROP TABLE ",
               FARMERNAME,
               "_r.temp")
      )
    )
  }
  return(invisible())
}















