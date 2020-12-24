## ---- out.width='100%', fig.align='center', fig.cap='**Figure 1.** Key is found in the top right corner of the schematic. The green ring represents the R-Shiny OFPE web spplication which is driven by the OFPE R-Package (blue ring). These both require connection to a PostgreSQL spatial database with PostGIS enabled. The yellow boxes represent different pages of the OFPE web application and vignettes in the R-package. Black boxes represent user inputs and orange clouds represent cloud based tools.', echo = FALSE----
knitr::include_graphics('ofpe_data_workflow.png')

## ----setup, message = FALSE, eval = FALSE-------------------------------------
#  #devtools::install_github("paulhegedus/OFPE")
#  #devtools::install_github("paulhegedus/OFPEDATA")
#  library(OFPE)
#  library(OFPEDATA)

## ---- eval = FALSE------------------------------------------------------------
#  dbCon <- DBCon$new(
#    user = "postgres",
#    password = "<your_password>",
#    dbname = "<your_db_name>",
#    host = "localhost",
#    port = "5432"
#  )
#  OFPE::removeTempTables(dbCon$db)

## ---- eval = FALSE------------------------------------------------------------
#  dat <- data(package = "OFPEDATA")
#  dat_names <- dat$results[, "Item"] %>%
#    subset(!grepl("_bbox", dat$results[, "Item"]))
#  dat_list <- as.list(dat_names) %>%
#    `names<-`(dat_names) %>%
#    lapply(function (x) eval(parse(text = x)))
#  rm(dat, dat_names)

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  temp_path <- "~/INIT_UPLOADS/"
#  ifelse(dir.exists(temp_path),
#         {do.call(file.remove, list(list.files(temp_path, full.names = TRUE)));
#           file.remove(temp_path);
#           dir.create(temp_path)},
#         dir.create(temp_path)) %>%
#    invisible()
#  toTempFolder <- function (x, y, farmer) {
#    if (grepl(farmer, y)) {
#      if (!grepl("pro", y)) {
#        sf::st_write(x, paste0(temp_path, y, ".shp"), quiet = TRUE)
#      } else {
#        if (grepl("2019", y)) {
#          data.table::fwrite(x, paste0(temp_path, y, ".csv"))
#        } else {
#          sf::st_write(x, paste0(temp_path, y, ".shp"), quiet = TRUE)
#        }
#      }
#    }
#  }
#  # only using farmer B as an example, repeat for other farmers if desired
#  invisible(mapply(toTempFolder,
#         dat_list,
#         names(dat_list),
#         MoreArgs = list(farmer = "FarmerB")))
#  rm(dat_list, temp_path)

## ---- eval = FALSE------------------------------------------------------------
#  dat_path <- "<your/folder/path>"
#  importOF <- ImportOF$new(dbCon, dat_path)

## ---- eval = FALSE------------------------------------------------------------
#  importOF$executeUpload()

## ---- eval = FALSE------------------------------------------------------------
#  importOF$status

## ---- eval = FALSE------------------------------------------------------------
#  googledrive::drive_auth("<your_email@address>")

## ---- eval = FALSE------------------------------------------------------------
#  dat_path <- "<your_google_drive_foldername>"
#  importGEE <- ImportGEE$new(dbCon, dat_path, FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  importGEE$executeUpload()

## ---- eval = FALSE------------------------------------------------------------
#  dbCon$disconnect()

