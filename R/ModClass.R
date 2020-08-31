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
#' to execute that model. If the user writes their own model class, they must use a
#' R6 class. It must follow the interface design of the 'GAM' and 'NonLinear_Logistic'
#' classes. Also, if a user writes their own model class please contact the developer
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
    #' validation plots. Type NA to not create any folders. You will not be
    #' able to save any outputs. (Note, even if a path is provided, the user
    #' can pass FALSE as the sole argument to the 'setupOP' method to prevent
    #' the creation of folders. This will automatically prevent any plots to
    #' be saved.)
    out_path = NULL,
    #' @field SAVE Logical, whether to save figures. Autofilled to FALSE if
    #' a user selects NA in the 'out_path' or is NULL. Autofilled to TRUE
    #' otherwise. This will be triggered to FALSE if the user passes FALSE
    #' as the only argument to the 'setupOP' method. The user can also select
    #' to save/not save individual figures.
    SAVE = NULL,

    #' @field mod_list List containing the initialized R6 class for the specified
    #' models. All model classes follow the same interface with standardized
    #' field and method names. This class is accessed in the analysis and
    #' simulation steps.
    mod_list = NULL,

    #' @param respvar Response variable(s) to optimize on, input
    #' 'Yield' or 'Protein'. Multiple options allowed. This can be passed in
    #' from the 'datClass' class where the response variables for optimization
    #' where selected. This argument must be passed in on class instantiation
    #' and is not available to select from the interactive method because it
    #' has been selected in the 'datClass' class.
    #' @param fxn Provide the functional form of a model to use for analysis.
    #' The user must provide the name of the file that contains the scripts
    #' for the model. Current models available include 'GAM' and
    #' 'NonLinear_Logistic', however this frees users to create their own
    #' models and supply the file name for these.
    #' @param fxn_path Provide the path to the folder where a user created
    #' model class is stored. If using 'GAM' or 'NonLinear_Logistic' models
    #' this will be left NULL as these classes are stored in this package.
    #' @param SAVE Logical, whether to save figures. Autofilled to FALSE if
    #' a user selects NA in the 'out_path' or is NULL. Autofilled to TRUE
    #' otherwise. This will be triggered to FALSE if the user passes FALSE
    #' as the only argument to the 'setupOP' method. The user can also select
    #' to save/not save individual figures.
    #' @param out_path Provide the path to the folder in which to store and
    #' save outputs from the model fitting process, including diagnostic and
    #' validation plots. Type NA to not create any folders. You will not be
    #' able to save any outputs. (Note, even if a path is provided, the user
    #' can pass FALSE as the sole argument to the 'setupOP' method to prevent
    #' the creation of folders. This will automatically prevent any plots to
    #' be saved.)
    #' @return A instantiated 'ModClass' object.
    initialize = function(fxn = NULL,
                          fxn_path = NULL,
                          SAVE = NULL,
                          out_path = NULL) {
      if (!is.null(fxn)) {
        stopifnot(is.list(fxn),
                  any(grepl("yld|pro", names(fxn))))
        self$fxn <- fxn
      }
      if (!is.null(fxn_path)) {
        stopifnot(is.list(fxn_path),
                  any(grepl("yld|pro", names(fxn_path))),
                  length(fxn_path) == length(fxn))
        self$fxn_path <- fxn_path
      }
      if (!is.null(SAVE)) {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }
      if (!is.null(out_path)) {
        stopifnot(is.character(out_path))
        self$out_path <- out_path

        if (is.na(self$out_path) | is.null(self$out_path)) {
          self$SAVE <- FALSE
        }
      }
    },
    #' @description
    #' Interactive method for selecting inputs related to the models used to
    #' fit crop responses to experimental and covariate data. The user must
    #' pass in the response variables selected in the 'datClass' class in
    #' order to select models the user wishes to use for each response
    #' variable. If the user is using a model that they have created and written,
    #' they must provide the path to the folder where the script of this model
    #' is stored. Finally, the user provides the path to a folder in which to
    #' store outputs from the model such as diagnostic and validation plots.
    #' @param respvar Response variable(s) to optimize experimental inputs based
    #' off of. These are selected in the 'datClass' class and should be passed
    #' in from that class here.
    #' @return A completed 'ModClass' object.
    selectInputs = function(respvar) {
      private$.selectFxn(respvar)
      private$.selectOutPath()
    },
    #' @description
    #' Method used to setup the output location for the figures that the model
    #' produces. These include diagnostic and validation plots. Pass FALSE to
    #' 'create' to skip any creation of folders. The folder created is named
    #' 'Outputs'. This folder contains a folder called 'Diagnostics' for model
    #' diagnostic plots and a folder called 'Validation' for plots of the
    #' predicted vs. observed responses, and both predicted and observed responses
    #' vs. the experimental variable.
    #' @param create Logical, whether to create folders for output. If not,
    #' no plots will be saved by default.
    #' @return A folder created in the path for model output figures.
    setupOP = function(create = TRUE) {
      stopifnot(is.logical(create))
      if (!create) {
        self$SAVE <- FALSE
      }
      if (self$SAVE) {
        cwd <- paste0(self$out_path, "/Outputs") # outputs working directory
        if (create) {
          if(!file.exists(cwd)){
            dir.create(cwd)
            dir.create(paste0(cwd,"/","Diagnostics"))
            dir.create(paste0(cwd,"/","Validation"))
          }else{
            if(!file.exists(paste0(cwd,"/","Diagnostics"))){
              dir.create(paste0(cwd,"/","Diagnostics"))
            }
            if(!file.exists(paste0(cwd,"/","Validation"))){
              dir.create(paste0(cwd,"/","Validation"))
            }
          }
        }
      }
    },
    #' @description
    #' Method used to setup the GAM. This initializes the specified model for each
    #' response variable. The initialization of each model creates a table of the
    #' parameters and associated information related to the specific model.
    #' @param datClass datClass class object. Stores the data and inputs
    #' necessary for initializing the model.
    #' @param create Logical, whether to create folders for output. If not,
    #' no plots will be saved by default.
    #' @return A folder created in the path for model output figures.
    setupMod = function(datClass) {
      self$mod_list <- as.list(self$fxn) %>%
        `names<-`(names(self$fxn))
      respvar <- as.list(datClass$respvar)
      expvar <- as.list(datClass$expvar)
      if (is.null(self$fxn_path)) {
        self$mod_list <- mapply(private$.loadModules,
                                self$mod_list,
                                datClass$mod_dat,
                                as.list(datClass$respvar),
                                as.list(datClass$expvar),
                                datClass$mod_num_means)
      } else {
        self$mod_list <- mapply(private$.loadModules,
                                self$mod_list,
                                datClass$mod_dat,
                                as.list(datClass$respvar),
                                as.list(datClass$expvar),
                                datClass$mod_num_means,
                                self$fxn_path)
      }
    },
    #' @description
    #' Method for calling the specific model class' method for executing the model
    #' fitting function. This can differ between model types and is thus model
    #' specific.
    #' @param None All parametes supplied upon initialization.
    #' @return Diagnostic and validation plots in the 'Outputs' folder.
    fitModels = function() {
      lapply(self$mod_list, function(mod) mod$fitMod())
    },
    #' @description
    #' Method for saving diagnostic and validation plots. The diagnostic plots
    #' are methods of the specific model class used, while the validation plots
    #' of the predicted vs. observed responses, and both predicted and observed
    #' responses vs. the experimental variable are generated as methods of this
    #' class. These only save plots if the user has supplied a folder path to save the
    #' plots to, and if the user does not select SAVE == FALSE. If the user passes in
    #' an argument to SAVE, this replaces any previously selected SAVE option (e.g.
    #' if SAVE was set to TRUE in the class initialization and the user passes FALSE
    #' as an argument here, the class' selection for SAVE is set to FALSE from TRUE).
    #' @param SAVE Whether to save diagnostic plots. If NULL uses the user selected
    #' choice. If not NULL and is logical, argument replaces previously set SAVE
    #' options for the entire class.
    #' @return Diagnostic and validation plots in the 'Outputs' folder.
    savePlots = function(SAVE = NULL) {
      if (is.null(SAVE)) {
        SAVE <- self$SAVE
      } else {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }
      lapply(self$mod_list, private$.saveDiagnostics, SAVE) %>% invisible()
      lapply(self$mod_list, private$.saveValidation, SAVE) %>% invisible()
    }
  ),
  private = list(
    .selectFxn = function(respvar) {
      self$fxn <- as.list(respvar) %>%
        `names<-`(respvar)
      for (i in 1:length(self$fxn)) {
        self$fxn[[i]] <- as.character(readline(
          paste0("Provide the name of the model to use for ",ifelse(respvar[i] == "yld", "yield", "protein")," responses (i.e. 'GAM' or 'NonLinear_Logistic'). Omit the file extension (.R).: ")
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
        } else {
          self$fxn_path[[i]] <- NA
        }
      }
      if (all(is.na(self$fxn_path))) {
        self$fxn_path <- NULL
      }
    },
    .selectOutPath = function() {
      self$SAVE <- as.character(select.list(
        c(TRUE, FALSE),
        title = "Select whether to save output plots from the model fitting and analysis "
      ))
      self$out_path <- as.character(readline(
        "Provide the path to a folder in which to save analysis and model fitting outputs (i.e. '~/path/to/folder' or 'C:/path/to/folder'). Type NA to prevent any folders from being created.: "
      ))
      if (is.na(self$out_path) | is.null(self$out_path)) {
        self$SAVE <- FALSE
      }
    },
    .loadModules = function(fxn, dat, respvar, expvar, num_means, fxn_path = NULL) {
      if (!is.null(fxn_path)) {
        source(paste0(fxn_path, fxn, ".R"))
      }
      init_text <- "$new(dat, respvar, expvar, num_means)"
      return(eval(parse(text = paste0(fxn, init_text))))
    },
    .saveDiagnostics = function(mod, SAVE) {
      mod$saveDiagnostics(self$out_path, SAVE)
    },
    .saveValidation = function(mod, SAVE) {
      if (SAVE) {
        private$.plotObsPredRespVsExp(mod, self$out_path, SAVE)
        private$.plotObsVsPred(mod, self$out_path, SAVE)
      }
    },
    .plotObsPredRespVsExp = function(mod, out_path, SAVE) {
      set.seed(13113)
      if (mod$respvar == "yld") {
        cols <- c("black", "red")
      } else {
        cols <- c("black", "cyan")
      }
      shps <- as.integer(runif(length(unique(mod$dat$val$year.field)), 1, 10))
      yMIN <- DescTools::RoundTo(min(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T), 5, floor)
      yMAX <- DescTools::RoundTo(max(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T), 5, ceiling)
      ySTEP <- (yMAX -  yMIN) / 10
      xMIN <- DescTools::RoundTo(min(mod$dat$val[which(names(mod$dat$val) %in% mod$expvar)][[1]], na.rm = T), 5, floor)
      xMAX <- DescTools::RoundTo(max(mod$dat$val[which(names(mod$dat$val) %in% mod$expvar)][[1]], na.rm = T), 5, ceiling)
      xSTEP <- (xMAX - xMIN) / 10
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(data = mod$dat$val,
                   aes(x = get(mod$expvar), y = get(mod$respvar), col = cols[1], shape = year.field)) +
        ggplot2::labs(y = ifelse(mod$respvar == "yld", "Yield (bu/ac)", "Grain Protein Content (%)"),
             x=paste0(ifelse(mod$expvar == "aa_n", "Nitrogen", "Seed"), " (lbs/ac)")) +
        ggplot2::ggtitle(paste0(mod$fieldname," ", mod$mod_type ," Analysis"),
                subtitle = paste0("AIC = ", round(AIC(mod$mod), 4))) +
        ggplot2::geom_point(data = mod$dat$val,
                   aes(x = get(mod$expvar), y = pred,
                       col = cols[2],
                       shape = year.field)) +
        ggplot2::scale_color_manual(name = "", values = cols, labels = c("Observed", "Predicted")) +
        ggplot2::scale_shape_manual(name = "", values = shps) +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX), breaks = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX), breaks = seq(xMIN, xMAX, xSTEP)) +
        ggplot2::theme_bw()
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/Validation/",
                      mod$fieldname, "_", mod$mod_type, "_pred&Obs_", mod$respvar, "_vs_",
                      ifelse(mod$expvar == "aa_n", "N", "SR"), ".png"),
               plot = p, device = "png", scale = 1, width = 7.5, height = 5, units = "in")
      }
      return(p)
    },
    .plotObsVsPred = function(mod, out_path, SAVE) {
      set.seed(13113)
      shps <- as.integer(runif(length(unique(mod$dat$val$year.field)), 1, 10))

      MAX <- ifelse(max(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T) > max(mod$dat$val$pred, na.rm = T),
                    DescTools::RoundTo(max(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T), 5, ceiling),
                    DescTools::RoundTo(max(mod$dat$val$pred, na.rm = T), 5, ceiling))
      MIN <- ifelse(min(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T) < min(mod$dat$val$pred, na.rm = T),
                    DescTools::RoundTo(min(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]], na.rm = T), 5, floor),
                    DescTools::RoundTo(min(mod$dat$val$pred, na.rm = T), 5, floor))
      p <- ggplot2::ggplot(data = mod$dat$val) +
        ggplot2::geom_point(aes(x = get(mod$respvar), y = mod$dat$val$pred, shape = year.field)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = ifelse(mod$respvar == "yld", "red", "cyan")) +
        ggplot2::labs(x = paste0("Observed ", ifelse(mod$respvar == "yld", "Yield", "Protein")),
             y = paste0("Predicted ", ifelse(mod$respvar == "yld", "Yield", "Protein"))) +
        ggplot2::scale_shape_manual(name = "", values = shps) +
        ggplot2::scale_y_continuous(limits = c(MIN, MAX), breaks = seq(MIN, MAX, (MAX - MIN) / 10)) +
        ggplot2::scale_x_continuous(limits = c(MIN, MAX), breaks = seq(MIN, MAX, (MAX - MIN) / 10)) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste0("Predicted vs. Observed ", ifelse(mod$respvar=="yld", "Yield", "Protein")),
                subtitle = paste0("Line = 1:1, RMSE = ",
                                  suppressWarnings(round(Metrics::rmse(
                                    na.omit(mod$dat$val[which(names(mod$dat$val) %in% mod$respvar)][[1]]),
                                    na.omit(mod$dat$val$pred)),
                                    4
                                  ))))
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/Validation/",
                      mod$fieldname, "_", mod$mod_type, "_predVSobs_", mod$respvar, ".png"),
               plot = p, device = "png", scale = 1, width = 7.5, height = 5, units = "in"
        )
      }
      return(p)
    }
  )
)









