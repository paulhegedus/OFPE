#' @title R6 Class for storing economic data used in the OFPE simulation
#'
#' @description R6 class for for storing economic data that is used in the
#' simulation and prescription building steps of the OFPE data cycle. This
#' class has methods for gathering economic data or using the default economic
#' data on prices received for conventional and organic winter wheat, and the
#' cost of nitrogen. The default data is gathered from the Billings, MT grain
#' elevator from 2000 - 2016. This class also can receive data to model
#' protein premiums/dockages or uses the default data from the Billings, MT
#' grain elevator in 2016.
#'
#' Thes user has the option of providing their own data to use for the simulation
#' that feeds into the prescription generation in order to simulate a specific
#' economic conditions. To specify the protein premium and dockage function, the
#' user must supply a data frame with a column name 'pro' with the protein level
#' and a column named 'PremDock' with the premium(+)/dockage(-) price. To specify
#' the economic conditions to simulate, the user must supply a data frame with a
#' column indicating the year, named 'Year', a column named 'HRWWorg' for the
#' price received for organic hard red winter wheat, a column named 'HRWWconv'
#' for the price received for conventional hard red winter wheat, and a column
#' named 'Ncost' with the price of nitrogen fertilizer per pound.
#'
#' This class also stores the coefficients from the protein premium/dockage
#' model derived from the protein premium/dockage data, and user supplied
#' inputs for the cost of site-specific technology, and the fixed ownership
#' costs per acre related to production, not including the cost of fertilzier.
#' @export
EconDat <- R6::R6Class(
  "EconDat",
  public = list(
    #' @field fc Fixed costs ($/acre) associated with production, not including
    #' fertilizer.
    fc = NULL,
    #' @field ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications.
    ssAC = NULL

    # field
    # year
    # analysis grid (grid vs observed)
    # simulation grid (grid vs observed)
    # rx grid?? ^ could be same as simulation
    #...

    # will hold
    # analysis dat (train & validation)
    # simulation dat
    # rx dat??
    # like year dat (if selected)


  )#,
  #private = list()
)












