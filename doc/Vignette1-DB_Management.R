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
#    host = "localhost", # if you made your own db
#    port = "5432" # default
#  )

## ---- eval = FALSE------------------------------------------------------------
#  dat <- data(package = "OFPEDATA")
#  dat_names <- dat$results[, "Item"] %>%
#    subset(grepl("_bbox", dat$results[, "Item"]))
#  dat_list <- as.list(dat_names) %>%
#    `names<-`(dat_names) %>%
#    lapply(function (x) eval(parse(text = x)))
#  rm(dat, dat_names)

## ---- eval = FALSE------------------------------------------------------------
#  temp_path <- "~/INIT_UPLOADS/"
#  ifelse(dir.exists(temp_path),
#         {do.call(file.remove, list(list.files(temp_path, full.names = TRUE)));
#           file.remove(temp_path);
#           dir.create(temp_path)},
#         dir.create(temp_path)) %>%
#    invisible
#  mapply(function (x, y) sf::st_write(x,
#                                      paste0(temp_path, y, ".shp"),
#                                      quiet = TRUE),
#         dat_list,
#         names(dat_list)) %>%
#    invisible()
#  rm(dat_list, temp_path)

## ---- eval = FALSE------------------------------------------------------------
#  # an initial farmer name to setup database with
#  farmers <- c("FarmerB")
#  # this example used PostGIS version 2.5, change to 3.0 if needed
#  buildDB <- BuildDB$new(dbCon, "2.5", farmers)

## ---- eval = FALSE------------------------------------------------------------
#  invisible(buildDB$buildDatabase())

## ---- eval = FALSE------------------------------------------------------------
#  farms <- list(
#    list(farm_name = "FarmerB_FarmName",
#         farm_shp_name = "FarmerB_FarmName_bbox",
#         farmer_name = "FarmerB")
#  )
#  farm_path <- "~/INIT_UPLOADS/"

## ---- eval = FALSE------------------------------------------------------------
#  fields <- list(
#    list(field_name = "sec35middle",
#         field_shp_name = "sec35middle_bbox",
#         farmer_name = "FarmerB"),
#    list(field_name = "sec1east",
#         field_shp_name = "sec1east_bbox",
#         farmer_name = "FarmerB"),
#    list(field_name = "sec1west",
#         field_shp_name = "sec1west_bbox",
#         farmer_name = "FarmerB")
#  )
#  field_path <- "~/INIT_UPLOADS/"

## ---- eval = FALSE------------------------------------------------------------
#  action_list <- list(
#    list(action = "ManageFarmers",
#         farmers = farmers),
#    list(action = "ManageFarms",
#         farms = farms,
#         farm_path = farm_path),
#    list(action = "ManageFields",
#         fields = fields,
#         field_path = field_path)
#  )

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  manageDB <- ManageDB$new(dbCon, action_list)
#  invisible(manageDB$setupActions())

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  invisible(manageDB$executeActions())

## ---- eval = FALSE------------------------------------------------------------
#  farmers <- c("FarmerC", "FarmerI")
#  farms <- list(
#    list(farm_name = "FarmerC_FarmName",
#         farm_shp_name = "FarmerC_FarmName_bbox",
#         farmer_name = "FarmerC"),
#    list(farm_name = "FarmerI_FarmName",
#         farm_shp_name = "FarmerI_FarmName_bbox",
#         farmer_name = "FarmerI")
#  )
#  farm_path <- "~/INIT_UPLOADS/"
#  fields <- list(
#    list(field_name = "millview",
#         field_shp_name = "millview_bbox",
#         farmer_name = "FarmerC"),
#    list(field_name = "henrys",
#         field_shp_name = "henrys_bbox",
#         farmer_name = "FarmerI")
#  )
#  field_path <- "~/INIT_UPLOADS/"

## ---- eval = FALSE------------------------------------------------------------
#  action_list <- list(
#    list(action = "ManageFarmers",
#         farmers = farmers),
#    list(action = "ManageFarms",
#         farms = farms,
#         farm_path = farm_path),
#    list(action = "ManageFields",
#         fields = fields,
#         field_path = field_path)
#  )
#  manageDB <- ManageDB$new(dbCon, action_list)
#  invisible(manageDB$setupActions())

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  invisible(manageDB$executeActions())

## ---- eval = FALSE------------------------------------------------------------
#  dbCon$disconnect()

