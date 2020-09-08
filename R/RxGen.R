#' @title R6 Class for generating a OFPE prescriptions or experimental prescriptions
#'
#' @description R6 class for for creating a prescription or experimental prescription
#' for a field of interest. The user can create a prescription or experimental
#' prescription based on the optimized rates from the user selected management
#' scenario for given economic and weather conditions. This process builds off of
#' the simulation R6 class (SimClass), where the Monte Carlo simulation for a year
#' or year(s) the user thinks the upcoming year is going to resemble, with the user
#' specified economic data, is used for identifying the optimum rates for each
#' management strategy at each part.
#'
#' If the user decides to do an experimental prescription, the optimum rates of the
#' user specified management scenario from the simulation is used as the base map
#' for the experimental prescription. Experimental rates can be generated from gaps
#' in the optimum rates to create a distribution of rates across the field to be used
#' for fitting crop response models in upcoming years, or from user specified
#' experimental rates. These are then randomly applied to the field
#' according to user specified proportions for each rate, stratified on the
#' optimum rates.
#'
#' If the user decides to make an OFPE prescription with no experiment, the
#' optimum rates for each point for the user specified management scenario is
#' taken as the prescription. Instead of applying experimental rates, a farmer
#' selected rate representing the rate they would have applied is randomly
#' placed across the field, stratified on the optimum rates. These are used
#' as a check to evaluate the performance of the optimized rates or whether
#' further experimentation is needed.
#'
#' When creating an experimental prescription or a prescription, the user must pass
#' an initialized SimClass. This implies that the user has initialized and set up
#' the required DatClass, ModClass, and EconDat R6 objects. The user has the option
#' of passing a SimClass object that has been executed or not. Regardless, the RxClass
#' will check 1) for simulation output data and 2) execute the simulation for any year(s)
#' that the user specified for generating a prescription for that is not present in the
#' simulation data. This simulation output data is used to generate the experimental
#' prescriptions and the prescriptions.
#'
#' This class follows the generator interface that includes an initialization method
#' and an 'executeOutput' method.
#'
#' @export
RxGen <- R6::R6Class(
  "RxGen",
  public = list(),
  private = list()
)
