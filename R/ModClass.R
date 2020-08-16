#' @title R6 Class for storing the user selected model class and associated inputs
#'
#' @description R6 class for storing the user selected model class and associated
#' inputs for instantiating the user selected model. The model selection is used
#' to fit functions of the response variables selected to as-applied experimental
#' and covariate data. The instantiation of this class requires the user to provide
#' the exact name of the model they desire for each response variable. Additionally,
#' in order to maintain flexibility in the analysis, the user can provide a path to
#' a folder location with a sourcable model that they have written and would like
#' to use for the analysis. Otherwise, the user can leave this argument NULL and
#' select a model provided in this package.
#'
#' The user is required to provide the exact name of the model that they want to use.
#' This exact name corresponds to the file containing the scripts and algorithms used
#' to execute that model. If the user writes their own model methods, they must use an
#' R6 class. Also, if a user writes their own model class please contact the developer
#' for testing and inclusion in the OFPE package.
#'
#' Each model class also requires the user to specify a path to a folder in which to
#' save and store outputs from the model fitting process. These include figures such
#' as diagnostic and validation plots to assess the model fit ability.
#'
#' While this class stores the inputs for the model the user selects for each response
#' variable, it also will hold the instantiated model used beyond the analysis
#' by providing the fitted model used in the simulation and subsequent prescription
#' generation.
#'
#' Inputs can be supplied directly to this class during instantiation, however
#' this is NOT recommended except for advanced users. It is recommended that the
#' user supplies the response variables, and uses the interactive selection methods
#' to select user inputs.
#' @export
ModClass <- R6::R6Class(
  "ModClass",
  public = list(
    #' @field fxn Provide the functional form of a model to use for analysis.
    #' The user must provide the name of the file that contains the scripts
    #' for the model. Current models available include 'GAM' and
    #' 'NonLinear_Logistic', however this frees users to create their own
    #' models and supply the file name for these. This must be a list named
    #' by each response variable used in the model using 'yld' for the model
    #' to use for yield responses, and 'pro' for the model to use for protein
    #' responses.
    fxn = NULL,
    #' @field fxn_path Provide the path to the folder where a user created
    #' model class is stored. If using 'GAM' or 'NonLinear_Logistic' models
    #' this will be left NULL as these classes are stored in this package.
    #' This must be a list named by each response variable used in the model
    #' using 'yld' for the model to use for yield responses, and 'pro' for
    #' the model to use for protein responses.
    fxn_path = NULL,
    #' @field out_path Provide the path to the folder in which to store and
    #' save outputs from the model fitting process, including diagnostic and
    #' validation plots.
    out_path = NULL,

    #' @param respvar Response variable(s) to optimize on, input
    #' 'Yield' or 'Protein'. Multiple options allowed. This can be passed in
    #' from the 'DatInputs' class where the response variables for optimization
    #' where selected. This argument must be passed in on class instantiation
    #' and is not available to select from the interactive method because it
    #' has been selected in the 'DatInputs' class.
    #' @param fxn Provide the functional form of a model to use for analysis.
    #' The user must provide the name of the file that contains the scripts
    #' for the model. Current models available include 'GAM' and
    #' 'NonLinear_Logistic', however this frees users to create their own
    #' models and supply the file name for these.
    #' @param fxn_path Provide the path to the folder where a user created
    #' model class is stored. If using 'GAM' or 'NonLinear_Logistic' models
    #' this will be left NULL as these classes are stored in this package.
    #' @param out_path Provide the path to the folder in which to store and
    #' save outputs from the model fitting process, including diagnostic and
    #' validation plots.
    #' @return A instantiated 'ModClass' object.
    initialize = function(fxn = NULL,
                          fxn_path = NULL,
                          out_path = NULL) {
      if (!is.null(fxn)) {
        stopifnot(is.list(fxn),
                  any(names(fxn) == "yld|pro"))
        self$fxn <- fxn
      }
      if (!is.null(fxn_path)) {
        stopifnot(is.list(fxn_path),
                  any(names(fxn_path) == "yld|pro"))
        self$fxn_path <- fxn_path
      }
      if (!is.null(out_path)) {
        stopifnot(is.character(out_path))
        self$out_path <- out_path
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the models used to
    #' fit crop responses to experimental and covariate data. The user must
    #' pass in the response variables selected in the 'DatInputs' class in
    #' order to select models the user wishes to use for each response
    #' variable. If the user is using a model that they have created and written,
    #' they must provide the path to the folder where the script of this model
    #' is stored. Finally, the user provides the path to a folder in which to
    #' store outputs from the model such as diagnostic and validation plots.
    #' @param respvar Response variable(s) to optimize experimental inputs based
    #' off of. These are selected in the 'DatInputs' class and should be passed
    #' in from that class here.
    #' @return A completed 'ModClass' object.
    selectInputs = function(respvar) {
      private$.selectFxn(respvar)
      private$.selectFxnPath(respvar)
      private$.selectOutPath()
    }
  ),
  private = list(
    .selectFxn = function(respvar) {
      self$fxn <- as.list(respvar) %>%
        `names<-`(respvar)
      for (i in 1:length( self$fxn)) {
        self$fxn[[i]] <- as.character(readline(
          paste0("Provide the name of the model to use for ",ifelse( self$fxn[[i]] == "yld", "yield", "protein")," responses (i.e. 'GAM' or 'NonLinear_Logistic'). Omit the file extension (.R).: ")
        ))
      }
    },
    .selectFxnPath = function(respvar) {
      self$fxn_path <- as.list(respvar) %>%
        `names<-`(respvar)
      for (i in 1:length(self$fxn_path)) {
        if (!grepl("GAM|NonLinear_Logistic", self$fxn[[i]])) {
          self$fxn_path[[i]] <- as.character(readline(
            paste0("Provide the path to a folder where the desired model script for fitting ",ifelse(self$fxn_path[[i]] == "yld", "yield", "protein")," responses is stored (i.e. '~/path/to/folder' or 'C:/path/to/folder'): ")
          ))
        }
      }
    },
    .selectOutPath = function() {
      self$out_path <- as.character(readline(
        "Provide the path to a folder in which to save simulation outputs (i.e. '~/path/to/folder' or 'C:/path/to/folder'): "
      ))
    }
  )
)








