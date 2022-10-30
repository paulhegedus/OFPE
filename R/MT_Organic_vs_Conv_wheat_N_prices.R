#' Default economic conditions used for OFPE simulations.
#'
#' A dataset containing the prices received for organic and conventionally
#' grown hard red winter wheat (HRWW), the cost of nitrogen fertilizer, and the
#' year the data is related to.
#'
#' @format A data frame with 98 rows and 4 variables:
#' \describe{
#'   \item{Year}{Year observation corresponds to.}
#'   \item{org}{Price of HRWW grown organically, in dollars per bushel.}
#'   \item{conv}{Price of HRWW grown conventionally, in dollars per bushel.}
#'   \item{cost}{Cost of nitrogen fertilizer, in dollars per pound.}
#'   \item{FC}{Fixed costs of farm operations besides seed and fertilizer, in dollars per acre.
#'   This includes chemicals (besides fertilizer), fuel, lube, electricity, repairs, 
#'   variable expenses, interest, taxes, general overhead, and capital recovery of equipment.}
#' }
"MT_Organic_vs_Conv_wheat_N_prices"
