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
#' column indicating the year, named 'Year', a column named 'org' for the price
#' received per bushel for the crop grown organically , a column named 'conv' for
#' the price received per bushel for a conventionally grown crop, and a column named
#' 'cost' with the cost per pound of the user's seed or nitrogen fertilizer.
#'
#' This class also stores the coefficients from the protein premium/dockage
#' model derived from the protein premium/dockage data, and user supplied
#' inputs for the cost of site-specific technology, and the fixed ownership
#' costs per acre related to production, not including the cost of the input.
#'
#' This class, like all other 'inputs' classes, gives the user the option of supplying
#' their own parameter selections or to use an interactive method to select parameter
#' values and options. Like all other 'inputs' classes, it is recommended that the user
#' uses the interactive selection method unless providing their own economic and premium/
#' dockage data.
#' @seealso \code{\link{DBCon}} for database connection class,
#' \code{\link{SimClass}} for simulation class that rely on data in EconDat.
#' @export
EconDat <- R6::R6Class(
  "EconDat",
  public = list(
    #' @field FC Fixed costs ($/acre) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    FC = NULL,
    #' @field ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    ssAC = NULL,
    #' @field Prc Data frame containing information on the prices received for the crop
    #' and the cost of nitrogen. The user has the option to specify the economic
    #' conditions to simulate by supplying a data frame with a column indicating
    #' the year, named 'Year', a column named 'org' for the price received per bushel for
    #' the crop grown organically , a column named 'conv' for the price received per bushel
    #' for a conventionally grown crop, and a column named 'cost' with the cost per pound of
    #' the user's seed or nitrogen fertilizer. Alternatively, the user can elect to use
    #' the default economic conditions gathered from the Billings, MT elevator that conatin
    #' data from 2000 - 2016 on the price received for organic and conventionally grown hard
    #' red winter wheat and the cost of nitrogen fertilizer. This option can be accessed
    #' with the interactive selection method or by passing in 'Default' to this parameter.
    Prc = NULL,
    #' @field PD Data frame containing information on the protein premium/dockage received
    #' for the user's crop. To specify the protein premium and dockage function, the
    #' user must supply a data frame with a column name 'pro' with the protein level
    #' and a column named 'PremDock' with the premium(+)/dockage(-) price. Alternatively,
    #' the user can elect to use the default premium/dockage data gathered from the Billings,
    #' MT elevator in 2016.
    PD = NULL,
    #' @field B0pd Intercept for the protein premium/dockage model fit to protein premium/dockage
    #' data. Used to calculate estimated net-return based off of predicted protein in the OFPE
    #' Monte Carlo simulation.
    B0pd = NULL,
    #' @field B1pd Coefficient for observed protein in the protein premium/dockage model fit to
    #' protein premium/dockage data. Used to calculate estimated net-return based off of predicted
    #' protein in the OFPE Monte Carlo simulation.
    B1pd = NULL,
    #' @field B2pd Coefficient for the squared protein term in the protein premium/dockage model fit
    #' to protein premium/dockage data. Used to calculate estimated net-return based off of predicted
    #' protein in the OFPE Monte Carlo simulation.
    B2pd = NULL,

    #' @param FC Fixed costs ($/acre) associated with production, not including the input of
    #' interest. This includes things like the cost of labor, fuel, etc.
    #' @param ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    #' @param Prc Data frame containing information on the prices received for the crop
    #' and the cost of nitrogen. The user has the option to specify the economic
    #' conditions to simulate by supplying a data frame with a column indicating
    #' the year, named 'Year', a column named 'org' for the price received per bushel for
    #' the crop grown organically , a column named 'conv' for the price received per bushel
    #' for a conventionally grown crop, and a column named 'cost' with the cost per pound of
    #' the user's seed or nitrogen fertilizer. Alternatively, the user can elect to use
    #' the default economic conditions gathered from the Billings, MT elevator that conatin
    #' data from 2000 - 2016 on the price received for organic and conventionally grown hard
    #' red winter wheat and the cost of nitrogen fertilizer. This option can be accessed
    #' with the interactive selection method or by passing in 'Default' to this parameter.
    #' @param PD Data frame containing information on the protein premium/dockage received
    #' for the user's crop. To specify the protein premium and dockage function, the
    #' user must supply a data frame with a column name 'pro' with the protein level
    #' and a column named 'PremDock' with the premium(+)/dockage(-) price. Alternatively,
    #' the user can elect to use the default premium/dockage data gathered from the Billings,
    #' MT elevator in 2016. This option can be accessed with the interactive selection method
    #' or by passing in 'Default' to this parameter.
    #' @return A new 'EconDat' object.
    initialize = function(FC = NULL,
                          ssAC = NULL,
                          Prc = NULL,
                          PD = NULL) {
      if (!is.null(ssAC)) {
        stopifnot(is.numeric(ssAC))
        self$ssAC <- ssAC
      }
      if (!is.null(Prc)) {
        if (any(Prc != "Default")){
          Prc <- as.data.frame(Prc)
          stopifnot(is.data.frame(Prc),
                    any(names(Prc) == "Year"),
                    any(names(Prc) == "org"),
                    any(names(Prc) == "conv"),
                    any(names(Prc) == "cost"))
          self$Prc <- Prc
        } else {
          self$Prc <- MT_Organic_vs_Conv_wheat_N_prices
          if (!is.null(FC)) {
            stopifnot(is.numeric(FC))
            self$Prc$FC <- FC
          }
        }
      }
      if (!is.null(PD)) {
        if (PD == "Default") {
          self$PD <- Billings_PremDock_2021
          private$.fitPremDock()
        } else {
          PD <- as.data.frame(PD)
          stopifnot(is.data.frame(PD),
                    any(names(PD) == "pro"),
                    any(names(PD) == "PremDock"))
          self$PD <- PD
        }
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the economic data
    #' and conditions of the Monte Carlo simulation. The user will be able
    #' to provide their inputs for the fixed costs per acre (FC) associated
    #' with production not including the experimental input. They also can
    #' input their costs associated with site-specific application of their
    #' experimental input (ssAC).
    #'
    #' The user will be asked whether they would like to use the default
    #' economic conditions. The user can elect to use the default economic
    #' conditions gathered from the Billings, MT elevator that contain
    #' data from 2000 - 2016 on the price received for organic and
    #' conventionally grown hard red winter wheat and the cost of nitrogen
    #' fertilizer. ... Alternatively, the user will have the option of selecting
    #' from a tool that scrapes economic data from the web (in progress) ...
    #' However, if the user is interested in specific economic conditions or
    #' is analyzing data of a crop that is not hard red winter wheat or an
    #' experimental variable that is not nitrogen fertilizer, the user must
    #' supply their own data.frame in the initialization process.
    #'
    #' Finally, the user will be asked whether they would like to use the default
    #' protein premium/dockage conditions. The user can elect to use the default
    #' data gathered from the Billings, MT elevator from 2016, or elect to forgo
    #' this input if not optimizing on protein data. However, if the user is interested
    #' in specific premium/dockage conditions or is analyzing data of a crop
    #' that is not hard red winter wheat, the user must supply their own data.frame
    #' in the initialization process.
    #' @param None No arguments needed because passed in during class
    #' instantiation.
    #' @return A completed 'EconDat' object.
    selectInputs = function() {
      private$.selectFC()
      private$.selectSSAC()
      if (is.null(self$Prc)) {
        private$.selectPrc()
      }
      if (is.null(self$PD)) {
        private$.selectPD()
      }
    }
  ),
  private = list(
    .selectFC = function() {
      self$FC <- as.numeric(readline(
        "Input the fixed ownership costs associated with production, not including the input of interest. This includes things like the cost of labor, fuel, etc.: "
      ))
    },
    .selectSSAC = function() {
      self$ssAC <- as.numeric(readline(
        "Input the cost ($/acre) of using site-specific technology or variable rate applications.(i.e. $4): "
      ))
    },
    .selectPrc = function() {
      Prc_option <- as.character(select.list(
        c("Default", "Gather From Web"),
        title = "Select whether to use default economic conditions for HRWW from Billings, MT (2000-2016) or to gather economic data from the web. "
      ))
      if (Prc_option == "Default") {
        self$Prc <- MT_Organic_vs_Conv_wheat_N_prices
      } else {

        # TODO
        # web scrape from antons app? where to get N cost data? how to make flexible?
        # self$Prc <- result from web scrape

      }
    },
    .selectPD = function() {
      PD_option <- as.character(select.list(
        c("Default", "None"),
        title = "Select whether to use default protein premium/dockage data for HRWW from Billings, MT (2016) or select 'None' to skip because protein is not used for optimization. "
      ))
      if (PD_option == "Default") {
        self$PD <- Billings_PremDock_2021
        private$.fitPremDock()
      }
    },
    .fitPremDock = function() {
      prosq <- self$PD$pro^2
      fm <- lm(self$PD$PremDock ~ self$PD$pro + prosq)
      self$B0pd <- as.vector(coef(fm)[1])
      self$B1pd <- as.vector(coef(fm)[2])
      self$B2pd <- as.vector(coef(fm)[3])
    }
  )
)












