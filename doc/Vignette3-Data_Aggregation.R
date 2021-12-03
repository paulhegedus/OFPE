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
#  OFPE::removeTempTables(dbCon$db) # removes temporary tables

## ---- eval = FALSE------------------------------------------------------------
#  aggInputs <- AggInputs$new(dbCon)
#  aggInputs$selectInputs()

## ---- eval = FALSE------------------------------------------------------------
#  aggDat <- AggDat$new(aggInputs)
#  aggDat$aggregateData()

## ---- eval = FALSE------------------------------------------------------------
#  dbCon$disconnect()

