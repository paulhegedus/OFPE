#' Default protein premium/dockage data used for OFPE simulations.
#'
#' A dataset containing protein levels of hard red winter wheat and
#' the associated premium or dockage gathered in 2016 from the Billings,
#' MT grain elevator. This dataset is used to fit a function of premium
#' dockage related to grain protein that is used in the OFPE Monte Carlo
#' simulation to calculate estimated net-return. The protein premiums
#' are 15 cents per 1/2 percent protein from 11.5% to 15%. The protein
#' dockages are 25 cents per 1/2 percent protein from 11.5% to 7%. 
#' simulation to calculate estimated net-return.
#'
#' @format A data frame with 33 rows and 2 variables:
#' \describe{
#'   \item{pro}{Protein percent level (real number).}
#'   \item{PremDock}{Price gained or lost from the base price related to the associated protein level, in dollars.}
#' }
"Billings_PremDock_2016"
