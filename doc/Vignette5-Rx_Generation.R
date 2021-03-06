## ---- out.width='100%', fig.align='center', fig.cap='**Figure 1.** Key is found in the top right corner of the schematic. The green ring represents the R-Shiny OFPE web spplication which is driven by the OFPE R-Package (blue ring). These both require connection to a PostgreSQL spatial database with PostGIS enabled. The yellow boxes represent different pages of the OFPE web application and vignettes in the R-package. Black boxes represent user inputs and orange clouds represent cloud based tools.', echo = FALSE----
knitr::include_graphics('ofpe_data_workflow.png')

## ----setup, message = FALSE, eval = FALSE-------------------------------------
#  #devtools::install_github("paulhegedus/OFPE")
#  library(OFPE)

## ---- eval = FALSE------------------------------------------------------------
#  dbCon <- DBCon$new(
#    user = "postgres",
#    password = "<your_password>",
#    dbname = "<your_db_name>",
#    host = "localhost",
#    port = "5432"
#  )
#  ggmap::register_google(key = "your_google_key")
#  OFPE::removeTempTables(dbCon$db) # removes temporary tables. good practice

## ---- eval = FALSE------------------------------------------------------------
#  rxClass <- RxClass$new(dbCon)
#  rxClass$selectInputs()

## ---- eval = FALSE------------------------------------------------------------
#  datClass <- DatClass$new(dbCon)
#  datClass$selectInputs()
#  modClass <- ModClass$new()
#  modClass$selectInputs(datClass$respvar)
#  econDat <- EconDat$new()
#  econDat$selectInputs()
#  simClass <- SimClass$new(dbCon)
#  simClass$selectInputs(datClass$farmername, datClass$fieldname)
#  invisible(datClass$setupDat())
#  invisible(modClass$setupOP())
#  invisible(modClass$setupMod(datClass))
#  invisible(modClass$fitModels())
#  invisible(modClass$savePlots())
#  invisible(simClass$setupSim(datClass, modClass, econDat))
#  simClass$executeSim()
#  simOP <- SimOP$new(simClass, create = TRUE)
#  invisible(simOP$savePlots())

## ---- eval = FALSE------------------------------------------------------------
#  rxClass <- RxClass$new(dbCon, simClass)
#  rxClass$selectInputs()

## ---- eval = FALSE------------------------------------------------------------
#  rxClass$setupOP()
#  rxClass$setupGen()
#  rxClass$executeOutGen()
#  invisible(rxClass$saveOutputs())

## ---- eval = FALSE------------------------------------------------------------
#  dbCon$disconnect()

