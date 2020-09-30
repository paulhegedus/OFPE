#' @title R6 Class for plotting outputs of the OFPE simulation.
#'
#' @description R6 class for plotting outputs of the OFPE simulation such as
#' figures comparing management scenarios, probability tables, and maps.
#'
#' This class should be initialized and executed after the user has initialized
#' and executed all other classes and has ran the SimClass$executeSim() method.
#' These figures rely on outputs from the simulation, so will cause errors if
#' used prior to execution of the simulation.
#'
#' See the SimClass$executeSim() method documentation for more detail on the management
#' scenarios reported. NR is net-return. The abbreviations used are; NRmin: uniform minimum
#' rate ('AAmin'), NRfs: uniform farmer selected rate, NRact: if experiment applied in
#' simulated year, NRssopt: site-specific optimum rates, NRffopt: uniform full-
#' field optimum rate, NRopp: if the crop received the opposite system crop price (e.g.
#' 'conventional' vs. 'organic' or reverse, see SimClass$executeSim() method
#' documentation for specifics on the difference in the calculation of this net-return
#' between systems).
#'
#' Error checking inputs to these functions is scant because the user should only use
#' these after executing the simulation method in the SimClass R6 class to generate
#' the necessary data for the output generation methods.
#' @seealso \code{\link{SimClass}} for the class that executes the Monte Carlo
#' simulation and provides the data for saving and plotting in this class.
#' @export
SimOP <- R6::R6Class(
  "SimOP",
  public = list(
    #' @field simClass SimClass R6 object that has been initialized and the OFPE
    #' simulation has been executed. This class holds the information necessary
    #' to produce the output figures, maps, and tables from the Monte Carlo
    #' simulation.
    simClass = NULL,

    #' @description
    #' Pass in a SimClass R6 object that has results from an executed simulation.
    #' Also pass in a logical to 'create' to override any previously selected
    #' SAVE options form the SimClass object. Depending on this choice, the output
    #' folder will be generated. This is done by a private method that sets up the
    #' output location for the figures that the model produces. This will not
    #' overwrite any previously generated diagnostic or validation plots from the
    #' ModClass. The folder created is named 'Outputs'. This folder contains a
    #' folder called 'Maps' which contains maps of estimated responses, net-return,
    #' and site-specific optimized rates. Another folder called 'EXP' is created
    #' that holds figures related to the amounts and distributions for as-applied
    #' rates of the experimental variable. Within this folder, subfolders called
    #' 'ffoptEXP', 'ssoptEXP', and 'EXPapplied' are created. The acronyms SSOPT and
    #' FFOPT correspond to site-specific optimized and full-field optimized rates.
    #' Another main folder named 'NR' is created to hold net-return figures comparing
    #' management outcomes. Subfolders within 'NR' are 'NRboxplots', 'NRbarplots'
    #' and 'NRprobabilities'. The two former contain figures, and the latter contains
    #' a table with the probability that the SSOPT strategy yields a higher net-return
    #' than each of the other management strategies.
    #' @param simClass SimClass R6 object that has been initialized and the OFPE
    #' simulation has been executed. This class holds the information necessary
    #' to produce the output figures, maps, and tables from the Monte Carlo
    #' simulation.
    #' @param create create Logical, whether to create the 'Output' folder and save
    #' figures, maps, and tables. Default is TRUE, pass FALSE to skip output folder
    #' step and prevent any output generation.
    #' @return A folder created in the path for model output figures.
    initialize = function(simClass, create = TRUE) {
      stopifnot(
        !is.null(simClass$sim_out),
        !is.null(simClass$sim_list),
        is.logical(create)
      )
      self$simClass <- simClass
      if (create) {
        private$.setupOP()
      } else {
        self$simClass$SAVE <- FALSE
      }
    },
    #' @description
    #' Method for saving simulation output plots. This method is used to call
    #' the public methods for each figure, map,  and table generated. This
    #' applies over the user selected simulation years. The user can pass a
    #' logical argument to this function which will disable all plotting and
    #' not save anything to your computer It will override any previous
    #' setting you have set for 'SAVE'. If calling plots individually, a
    #' SAVE option for each can be selected.
    #'
    #' @param SAVE Whether to save simulation outputs If NULL uses the user selected
    #' choice. If not NULL and is logical, argument replaces previously set SAVE
    #' options for the entire class.
    #' @return Simulation outputs in the 'Outputs' folder.
    savePlots = function(SAVE = NULL) {
      browser()

      if (is.null(SAVE)) {
        SAVE <- self$simClass$SAVE
      } else {
        stopifnot(is.logical(SAVE))
        self$simClass$SAVE <- SAVE
      }
      sim_list_names <- lapply(self$simClass$sim_list,
                               function(x) names(x[[1]]))
      sim_years <- names(self$simClass$sim_list)
      Bp_col <- grep(self$simClass$datClass$sys_type,
                     names(self$simClass$econDat$Prc))
      BpOpp_col <- grep(self$simClass$datClass$opp_sys_type,
                        names(self$simClass$econDat$Prc))
      cd <- list(
        Bp = mean(
          self$simClass$econDat$Prc[, Bp_col],
          na.rm = TRUE
        ),
        CEXP = mean(
          self$simClass$econDat$Prc[, "cost"],
          na.rm = TRUE
        ),
        BpOpp = mean(
          self$simClass$econDat$Prc[, BpOpp_col],
          na.rm = TRUE
        )
      )
      unique_fxn <- do.call(rbind, self$simClass$modClass$fxn)
      unique_fxn <- paste0(row.names(unique_fxn), "-", unique_fxn[, 1]) %>%
        paste(collapse = "--")

      for (i in 1:length(self$simClass$sim_out)) {
        unique_fieldname <-
          OFPE::uniqueFieldname(self$simClass$sim_out[[i]]$NRopt)

        self$saveSimData(self$simClass$sim_out[[i]],
                         unique_fieldname,
                         unique_fxn,
                         sim_years[i],
                         self$simClass$opt,
                         SAVE,
                         self$simClass$out_path)
        self$plotEstsVsExp( # plotEstsVsExp
          self$simClass$sim_list[[i]],
          cd,
          self$simClass$econDat$B0pd,
          self$simClass$econDat$B1pd,
          self$simClass$econDat$B2pd,
          self$simClass$econDat$FC,
          self$simClass$fs,
          self$simClass$econDat$ssAC,
          self$simClass$AAmin,
          self$simClass$datClass$respvar,
          self$simClass$datClass$expvar,
          sim_list_names[[i]],
          self$simClass$AArateCutoff,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          SAVE,
          self$simClass$out_path
        )
        self$plotNRbox(
          self$simClass$sim_out[[i]]$Bp.var,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        self$mgmtNRprobTable( # saveProbability
          "NR.ssopt",
          self$simClass$sim_out[[i]]$Bp.var,
          1000,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        self$plotNRbar( # plotAvNR()
          self$simClass$sim_out[[i]]$Bp.var,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        NRdat <- private$.setupNRdat(simClass$sim_out[[i]]$NRopt,
                                     simClass$sim_out[[i]]$NRffmax)
        TF4 <- private$.getTF4(NRdat$NRopt,
                               NRdat$NRffmax,
                               self$simClass$fieldsize,
                               self$simClass$fs)
        self$plotTotExpAppl(
          TF4,
          self$simClass$datClass$expvar,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        self$plotSSOPThist(
          NRdat$NRopt,
          TF4,
          self$simClass$datClass$expvar,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        self$plotFFOPThist(
          self$simClass$sim_out[[i]]$Bp.var,
          TF4,
          self$simClass$datClass$expvar,
          unique_fieldname,
          unique_fxn,
          sim_years[i],
          self$simClass$opt,
          SAVE,
          self$simClass$out_path
        )
        # SSOPT map (avg all sim)
        self$plotSimMaps(
          NRdat$NRopt,
          paste0("SSOPT_", ifelse(self$simClass$datClass$expvar == "aa_n", "N", "SR")),
          "EXP.rate.ssopt",
          paste0(ifelse(self$simClass$datClass$expvar=="aa_n", "N", "Seed Rate"), " Rate (lbs/ac)"),
          paste0("SS.opt ",
                 ifelse(self$simClass$datClass$expvar=="aa_n", "N", "Seed"),
                 " rates for ", sim_years[i], " conditions"),
          unique_fieldname,
          sim_years[i],
          unique_fxn,
          self$simClass$opt,
          SAVE,
          self$simClass$datClass$farmername,
          self$simClass$out_path
        )
        # SSOPT NR map
        self$plotSimMaps(
          NRdat$NRopt,
          "SSOPTNR",
          "NR.ssopt",
          "Net Return ($/ac)",
          paste0("SS.Opt NR for ", sim_years[i], "  conditions"),
          unique_fieldname,
          sim_years[i],
          unique_fxn,
          self$simClass$opt,
          SAVE,
          self$simClass$datClass$farmername,
          self$simClass$out_path
        )
        # FFOPT NR map
        self$plotSimMaps(
          NRdat$NRopt,
          "FFOPTNR",
          "NR.ffopt",
          "Net Return ($/ac)",
          paste0("FF.Opt NR (", NRdat$NRopt$EXP.rate.ffopt[1],
                 " lbs/ac) for ", sim_years[i],
                 " conditions"),
          unique_fieldname,
          sim_years[i],
          unique_fxn,
          self$simClass$opt,
          SAVE,
          self$simClass$datClass$farmername,
          self$simClass$out_path
        )
        # SSOPT Est. Yld map
        self$plotSimMaps(
          NRdat$NRopt,
          "estYld",
          "yld.opt",
          "Yield (bu/ac)",
          paste0("Predicted yield for ", sim_years[i], " conditions"),
          unique_fieldname,
          sim_years[i],
          unique_fxn,
          self$simClass$opt,
          SAVE,
          self$simClass$datClass$farmername,
          self$simClass$out_path
        )
        if (any(grepl("pro", self$simClass$datClass$respvar))) {
          # SSOPT Est. Pro map
          self$plotSimMaps(
            NRdat$NRopt,
            "estPro",
            "pro.opt",
            "Protein (%)",
            paste0("Predicted protein for ", sim_years[i], " conditions"),
            unique_fieldname,
            sim_years[i],
            unique_fxn,
            self$simClass$opt,
            SAVE,
            self$simClass$datClass$farmername,
            self$simClass$out_path
          )
        }
      }
      # Actual NR (both years from mod fitting)
      # Get all of the observed data (trn + val) from the more
      # densely populated respvar.
      index <- lapply(self$simClass$datClass$mod_dat, lapply, dim) %>%
        lapply(function(x) do.call(rbind, x)) %>%
        lapply(function(x) sum(x[, 1]))
      index <- do.call(rbind, index)
      index <- which(index[, 1] == max(index[, 1])) %>% as.numeric()
      dat <- rbind(self$simClass$datClass$mod_dat[[index]]$trn,
                   self$simClass$datClass$mod_dat[[index]]$val)
      years <- unique(dat$year) %>% as.character()
      # use the most recent economic years available
      cd <- aggregate(.~Year, data = self$simClass$econDat$Prc, FUN = mean)
      cd <- cd[cd$Year == max(cd$Year), ]
      Bp <- cd[, grep(self$simClass$datClass$sys_type, names(cd))]
      CEXP <- cd[, "cost"]
      for (i in 1:length(years)) {
        self$plotActNR(dat,
                       years[i],
                       Bp,
                       self$simClass$econDat$B0pd,
                       self$simClass$econDat$B1pd,
                       self$simClass$econDat$B2pd,
                       CEXP,
                       self$simClass$econDat$FC,
                       self$simClass$econDat$ssAC,
                       self$simClass$datClass$respvar,
                       self$simClass$datClass$expvar,
                       self$simClass$modClass,
                       unique_fieldname,
                       self$simClass$datClass$farmername,
                       unique_fxn,
                       SAVE,
                       self$simClass$out_path)
      }
    },
    #' @description
    #' Method for saving the data from the Monte Carlo simulation for
    #' a given simulation year. Saves data to the 'SimData' subfolder of
    #' the 'Outputs' folder created for storing outputs from the model
    #' fitting and/or the simulation. Note that these outputs can be
    #' quite large depending on your field size, iterations selected
    #' for the simulation, and number of simulation years.
    #' @param out_list List of outputs generated from the Monte Carlo
    #' simulation for a given simulation year. Includes 'Bp.var' for
    #' the mean net-returns for each iteration of the simulation,
    #' 'NRopt', which is a data.table for each observation for every
    #' iteration of the simulation containing net-return and optimum
    #' information, 'NRffmax' which is a data.table of the net-return
    #' of the entire field and the full-field optimum rate for
    #' each iteration of the simulation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/SimData'.
    saveSimData = function(out_list,
                           fieldname,
                           fxn,
                           sim_year,
                           opt,
                           SAVE,
                           out_path) {
      data.table::fwrite(out_list$Bp.var,
                         paste0(out_path,
                                "/Outputs/SimData/",
                                fieldname, "_BpVar_",
                                fxn, "_",
                                sim_year, "_",
                                opt, ".csv"))
      data.table::fwrite(out_list$NRopt,
                         paste0(out_path,
                                "/Outputs/SimData/",
                                fieldname, "_NRopt_",
                                fxn, "_",
                                sim_year, "_",
                                opt, ".csv"))
      data.table::fwrite(out_list$NRffmax,
                         paste0(out_path,
                                "/Outputs/SimData/",
                                fieldname, "_NRffMaxData_",
                                fxn, "_",
                                sim_year, "_",
                                opt, ".csv"))
    },
    #' @description
    #' Plot the average net-return, yield, and protein, of all points vs the
    #' experimental variable for the average price and experimental cost scenario
    #' from the Monte Carlo simulation. Calculates net-return for
    #' every point under every experimental rate in the user selected
    #' range under the average economic conditions from the simulation.
    #'
    #' Figure shows the selected response (net-return or predicted yield or
    #' predicted protein) for every point for every experimental rate. The
    #' mean response across rates is shown as a colored line.
    #' @param sim_list List for every rate simulated over with data.tables
    #' for every point in the simulation year.
    #' @param cd Named list with the mean base price for the real and opposite
    #' system types, and the mean cost of the experimental input. All are
    #' calculated from the prices and cost for the economic conditions used in
    #' the simulation.
    #' @param B0pd Intercept for the protein premium/dockage model fit to protein premium/dockage
    #' data. Used to calculate estimated net-return based off of predicted protein in the OFPE
    #' Monte Carlo simulation.
    #' @param B1pd Coefficient for observed protein in the protein premium/dockage model fit to
    #' protein premium/dockage data. Used to calculate estimated net-return based off of predicted
    #' protein in the OFPE Monte Carlo simulation.
    #' @param B2pd Coefficient for the squared protein term in the protein premium/dockage model fit
    #' to protein premium/dockage data. Used to calculate estimated net-return based off of predicted
    #' protein in the OFPE Monte Carlo simulation.
    #' @param FC Fixed costs ($/acre) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    #' @param fs Input the uniform as-applied rate the farmer would have applied
    #' to the field if an experiment was not conducted (i.e. 70 lbs N or SR/acre).
    #' fs = Farmer Selected.
    #' @param ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    #' @param AAmin Minimum as-applied rate used in simulation of management
    #' outcomues from (i.e. 0 lbs N per acre or 25 lbs seed per acre).
    #' @param respvar Response variable(s) used to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param sim_list_names Names of the columns in the simulated data.
    #' @param AArateCutoff The maximum as-applied rate to simulate management
    #' outcomues to (i.e. 200 lbs N or seed per acre).
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/Predictions'.
    plotEstsVsExp = function(sim_list,
                               cd,
                               B0pd,
                               B1pd,
                               B2pd,
                               FC,
                               fs,
                               ssAC,
                               AAmin,
                               respvar,
                               expvar,
                               sim_list_names,
                               AArateCutoff,
                               fieldname,
                               fxn,
                               sim_year,
                               SAVE,
                               out_path) {
      ## apply NRcalcCpp fxn to dnr with the cd econ scenario
      Bp <- cd$Bp
      CEXP <- cd$CEXP
      BpOpp <- cd$BpOpp
      rr <- nrow(sim_list[[1]])
      ## Calc NR for every point for every EXP rate (calc NR0 and NRorg for N = 0)
      sim_list <- lapply(sim_list, function(x) as.matrix(x) %>% `colnames<-`(NULL)) %>%
        lapply(function(x) apply(x, 2, as.numeric)) %>%
        lapply(OFPE::NRcalcCpp,
               Bp,
               B0pd,
               B1pd,
               B2pd,
               CEXP,
               BpOpp,
               FC,
               fs,
               ssAC,
               ifelse(any(respvar == "pro"), 1, 0),
               rr,
               grep(paste0("^", expvar, "$"), sim_list_names) - 1,
               grep("pred_yld", sim_list_names) - 1,
               grep("pred_pro", sim_list_names) - 1,
               grep("^NR$", sim_list_names) - 1,
               grep("NRmin", sim_list_names) - 1,
               grep("NRopp", sim_list_names) - 1,
               grep("NRfs", sim_list_names) - 1,
               AAmin)
      sim_list <- lapply(sim_list, function(x) data.table::as.data.table(x) %>%
                           `names<-`(sim_list_names))
      DNR <- data.table::rbindlist(sim_list)

      nr_plot <- private$.estsVsExpPlot("NR",
                                       DNR,
                                       expvar,
                                       fieldname,
                                       sim_year,
                                       Bp,
                                       CEXP,
                                       AAmin,
                                       AArateCutoff,
                                       fxn)
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/",
                               fieldname, "_NRvs",
                               ifelse(expvar == "aa_n","N","SR"),
                               "_", fxn, "_", sim_year, ".png"),
                        plot = nr_plot, device = "png", scale = 1,
                        width = 7.5, height = 5, units = "in")
      }
      resp_plots <- lapply(respvar,
                           private$.estsVsExpPlot,
                           DNR,
                           expvar,
                           fieldname,
                           sim_year,
                           Bp,
                           CEXP,
                           AAmin,
                           AArateCutoff,
                           fxn)
      if (SAVE) {
        for (i in 1:length(resp_plots)) {
          ggplot2::ggsave(paste0(out_path, "/Outputs/Predictions/",
                                 fieldname, "_", toupper(respvar[i]), "vs",
                                 ifelse(expvar == "aa_n","N","SR"),
                                 "_", fxn, "_", sim_year, ".png"),
                          plot = resp_plots[[i]], device = "png", scale = 1,
                          width = 7.5, height = 5, units = "in")
        }
      }
      resp_plots$nr <- nr_plot
      big_plot <- cowplot::plot_grid(plotlist = resp_plots, nrow = 3, ncol = 1)
      #return(big_plot)
    },
    #' @description
    #' Convert the Bp.var matrix to a dataframe for plotting. Reorder the levels
    #' of the method for plotting. Now, start to save the tables and plots showing
    #' results from the simulations. First, the variation in net returns over
    #' different price years.
    #' @param Bp.var Data.table containing the average net-returns from the field for each
    #' management type for every iteration of the simulation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/NR/NRboxplots'.
    plotNRbox = function(Bp.var,
                         fieldname,
                         fxn,
                         sim_year,
                         opt,
                         SAVE,
                         out_path) {
      Bp.plot <- as.data.frame(Bp.var)
      Bp.plot <- tidyr::pivot_longer(Bp.plot,
                                     -c("BaseP", "ffopt.EXPrate", "EXP.cost"),
                                     names_to = "Method",
                                     values_to = "NR")
      Bp.plot$Method <- factor(Bp.plot$Method)
      level_order <- c(grep("NR.min", levels(Bp.plot$Method)),
                       grep("NR.fs", levels(Bp.plot$Method)),
                       grep("NR.ssopt", levels(Bp.plot$Method)),
                       grep("NR.ffopt", levels(Bp.plot$Method)),
                       grep("NR.act", levels(Bp.plot$Method)),
                       grep("NR.opp", levels(Bp.plot$Method)))
      Bp.plot$Method <- factor(Bp.plot$Method,
                               levels(Bp.plot$Method)[level_order])
      mgmt_labels <- c("Min. Rate",
                       "FS",
                       "SS.Opt",
                       "FF.Opt",
                       "Actual",
                       "Alt. Price")
      yMIN <- DescTools::RoundTo(min(Bp.plot$NR,na.rm=T),5,floor)
      yMAX <- DescTools::RoundTo(max(Bp.plot$NR,na.rm=T),5,ceiling)
      ySTEP <- (yMAX - yMIN) / 10
      p <-
        ggplot2::ggplot(Bp.plot) +
          ggplot2::geom_boxplot(ggplot2::aes(x = Method, y = NR),
                                fill = "green3",
                                notch = FALSE) +
          ggplot2::scale_y_continuous(name =  "Average Net Return ($/acre)",
                                      limits = c(yMIN, yMAX),
                                      breaks = seq(yMIN, yMAX, ySTEP)) +
          ggplot2::scale_x_discrete(name = "Management Strategy", labels = mgmt_labels) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 14))
      if (any(Bp.plot$NR < 0)) {
        p <- p +
          ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2)
      }
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/NR/NRboxPlots/",
                               fieldname, "_avgNR_box_",
                               fxn, "_", sim_year, "_", opt, ".png"),
                        plot = p, device = "png", scale = 1,
                        width = 7.5, height = 7.5, units = "in")
      }
      #return(p)
    },
    #' @description
    #' Calculate the propabability that a given management method is more profitable
    #' than the other management strategies. For example, compare the probability that
    #' the site-specific optimization strategy yielded a higher net-retrun than the
    #' other management strategies. This probability is generated from across all
    #' iterations of the Monte Carlo simulation, so reflects the variation in economic
    #' conditions.
    #' @param mgmt Select the management strategy to compare, select from; 'NR.ssopt',
    #' 'NR.min', 'NR.fs', 'NR.ffopt', 'NR.act', 'NR.opp'. See SimClass documentation
    #' or this class' description for acronym definitions.
    #' @param Bp.var Data.table containing the average net-returns from the field for each
    #' management type for every iteration of the simulation.
    #' @param rt Number of times to sample the data with replacement to generate the
    #' probability that the net-return of the selected strategy was higher than each
    #' other strategy.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/NR/NRProbabilities'.
    mgmtNRprobTable = function(mgmt,
                               Bp.var,
                               rt,
                               fieldname,
                               fxn,
                               sim_year,
                               opt,
                               SAVE,
                               out_path) {
      Bp.var <- as.data.frame(Bp.var)
      mgmt_opts <- c("NR.min", "NR.fs", "NR.ssopt", "NR.ffopt", "NR.act", "NR.opp")
      mgmt_opts <- mgmt_opts[-grep(mgmt, mgmt_opts)]
      mgmt_prob <- rep(list(0), length(mgmt_opts)) %>%
        `names<-`(mgmt_opts)
      mgmt_col <- grep(mgmt, names(Bp.var))

      for (nn in 1:rt) {
        rp <- as.integer(runif(1, 1, nrow(Bp.var)))
        for (j in 1:length(mgmt_prob)) {
          alt_col <- grep(names(mgmt_prob)[j], names(Bp.var))
          mgmt_prob[[j]] <- ifelse(Bp.var[rp, mgmt_col] > Bp.var[rp, alt_col],
                                   1 + mgmt_prob[[j]],
                                   0 + mgmt_prob[[j]])
        }
      }
      mgmt_prob <- lapply(mgmt_prob, function(x) x / rt)
      Table.p <- do.call(rbind, mgmt_prob) %>%
        t() %>%
        as.data.frame()
      colnames(Table.p) <- paste0(mgmt, ">", names(Table.p))
      row.names(Table.p) <- c("Probability")
      if (SAVE) {
        data.table::fwrite(Table.p,paste0(out_path, "/Outputs/NR/NRProbabilities/",
                                          fieldname, "_mgmtProbs_", fxn,
                                          "_", sim_year, "_", opt, ".txt"),
                           sep="\t")
      }
      #return(Table.p)
    },
    #' @description
    #' Plot the mean net return for each management strategy as a
    #' bar plot and a quarter standard deviation around each mean.
    #' @param Bp.var Data.table containing the average net-returns from the field for each
    #' management type for every iteration of the simulation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/NR/NRbarplots'.
    plotNRbar = function(Bp.var,
                         fieldname,
                         fxn,
                         sim_year,
                         opt,
                         SAVE,
                         out_path) {
      Bp.var <- as.data.frame(Bp.var)
      #Average net return over the field for the different N application methods without organic
      TF3 <- data.frame(NR.ssopt = mean(Bp.var[, 'NR.ssopt'], na.rm = TRUE),
                        NR.min = mean(Bp.var[, 'NR.min'], na.rm = TRUE),
                        NR.fs = mean(Bp.var[, 'NR.fs'], na.rm = TRUE),
                        NR.ffopt = mean(Bp.var[, 'NR.ffopt'], na.rm = TRUE),
                        NR.act = mean(Bp.var[, 'NR.act'], na.rm = TRUE),
                        NR.opp = mean(Bp.var[, 'NR.opp'], na.rm = TRUE))
      #If plotting metric values don't forget to relabel y-axis in barplot
      #Put std. error bars on
      TF3 <- tidyr::pivot_longer(TF3,
                                 dplyr::everything(),
                                 names_to = "Method",
                                 values_to = "MeanNR")
      TF3$Method <- factor(TF3$Method)
      level_order <- c(grep("NR.min", levels(TF3$Method)),
                       grep("NR.fs", levels(TF3$Method)),
                       grep("NR.ssopt", levels(TF3$Method)),
                       grep("NR.ffopt", levels(TF3$Method)),
                       grep("NR.act", levels(TF3$Method)),
                       grep("NR.opp", levels(TF3$Method)))
      TF3$Method <- factor(TF3$Method,
                               levels(TF3$Method)[level_order])
      TF3$SDNR <- c(sd(Bp.var[, 'NR.ssopt'], na.rm = TRUE),
                    sd(Bp.var[, 'NR.min'], na.rm = TRUE),
                    sd(Bp.var[, 'NR.fs'], na.rm = TRUE),
                    sd(Bp.var[, 'NR.ffopt'], na.rm = TRUE),
                    sd(Bp.var[, 'NR.act'], na.rm = TRUE),
                    sd(Bp.var[, 'NR.opp'], na.rm = TRUE))
      TF3$max <- TF3$MeanNR + (1.96 * TF3$SDNR / 10)
      TF3$min <- TF3$MeanNR - (1.96 * TF3$SDNR / 10)

      if (any(any(na.omit(TF3$min) < 0) | any(na.omit(TF3$max) > 0))) { # if means above and below zero
        MIN <- DescTools::RoundTo((min(TF3$MeanNR, na.rm = T) +
                                     (1.96 * min(TF3$SDNR, na.rm = T) / 10)),
                                  5, floor)
        MAX <- DescTools::RoundTo((max(TF3$MeanNR, na.rm = T) +
                                     (1.96 * max(TF3$SDNR, na.rm = T) / 10)),
                                  5, ceiling)
        STEP <- (MAX - MIN) / 10
      }
      if (all(na.omit(TF3$min) < 0)) { # if means below zero
        MIN <- DescTools::RoundTo((min(TF3$MeanNR, na.rm = T) -
                                     (1.96 * min(TF3$SDNR, na.rm = T) / 10)),
                                  5, floor)
        MAX <- 0
        STEP <- (MAX - MIN) / 10
      }
      if (all(na.omit(TF3$min > 0))) { # if means above zero
        MIN <- 0
        MAX <- DescTools::RoundTo((max(TF3$MeanNR, na.rm = T) +
                                     (1.96 * max(TF3$SDNR, na.rm = T) / 10)),
                                  5, ceiling)
        STEP <- (MAX - MIN) / 10
      }
      mgmt_labels <- c("Min. Rate",
                       "FS",
                       "SS.Opt",
                       "FF.Opt",
                       "Actual",
                       "Alt. Price")
      p <-
        ggplot2::ggplot(TF3, ggplot2::aes(x = Method, y = MeanNR)) +
          ggplot2::geom_bar(stat = "identity", fill = "green3") +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = MeanNR - (1.96 * SDNR / 10),
                                     ymax = MeanNR + (1.96 * SDNR / 10)),
                                 width = .2) + #
          ggplot2::scale_y_continuous(name = "Average net return ($/acre)",
                               limits = c(MIN, MAX),
                               breaks = seq(MIN, MAX, STEP)) +
          ggplot2::scale_x_discrete(name = "Management Strategy",
                                    labels = mgmt_labels) +
          ggplot2::geom_text(data = TF3,
                             ggplot2::aes(x = Method, y = MeanNR, label = round(MeanNR, 2)),
                      hjust = 1.15,
                      vjust = 1.25) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 14))
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/NR/NRbarPlots/",
                               fieldname, "_avgNR_bar_",
                               fxn, "_", sim_year, "_", opt, ".png"),
                        plot = p, device = "png", scale = 1,
                        width = 7.5, height = 7.5, units = "in")
      }
      #return(p)
    },
    #' @description
    #' Create a bar plot of the total experimental input applied with
    #' each strategy. The amount of the site-specific input applied
    #' is averaged across the simulation for each point and summed to
    #' get a total amount of experimental input applied with the site-
    #' specific method. The total amount of experimental input applied
    #' with all other strategies is calculated the same way.
    #' @param TF4 Data.frame with columns for the amount of experimental rates applied
    #' for each strategy.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/EXP/EXPapplied'.
    plotTotExpAppl = function(TF4,
                              expvar,
                              fieldname,
                              fxn,
                              sim_year,
                              opt,
                              SAVE,
                              out_path) {
      if (TF4[which(TF4$Method == "EXP.ffopt"), "EXP"] == 0) {
        txt <- "FF.Opt Rate = 0"
      } else {
        if (TF4[which(TF4$Method == "EXP.ssopt"),"EXP"] >
            TF4[which(TF4$Method == "EXP.ffopt"), "EXP"]) {
          txt <- paste0(as.integer(
              (TF4[which(TF4$Method == "EXP.ssopt"), "EXP"] -
                 TF4[which(TF4$Method == "EXP.ffopt"), "EXP"]) /
                TF4[which(TF4$Method == "EXP.ffopt"), "EXP"] * 100),
            "% more ",
            ifelse(expvar == "aa_n", "N", "Seed"), " used in SS.Opt than FF.Opt")
        }else{
          txt <- paste0(as.integer(
              (TF4[which(TF4$Method == "EXP.ssopt"), "EXP"] -
                 TF4[which(TF4$Method == "EXP.ffopt"), "EXP"]) /
                TF4[which(TF4$Method == "EXP.ffopt"), "EXP"] * -100),
            "% less ",
            ifelse(expvar == "aa_n", "N", "Seed"), " used in SS.Opt than FF.Opt")
        }
      }
      y_lab <- paste0(ifelse(expvar == "aa_n", "Nitrogen", "Seed"),
                      " used on field (lbs/field)")
      yMIN <- 0
      yMAX <- DescTools::RoundTo(max(TF4$EXP, na.rm = TRUE), 5, ceiling)
      ySTEP <- (yMAX - yMIN) / 10
      gg_title <- paste0(ifelse(expvar == "aa_n", "Nitrogen", "Seed"),
                         " used with each application strategy")
      mgmt_labels <- c("Min. Rate", "FS", "SS.Opt", "FF.Opt")
      p <-
        ggplot2::ggplot(TF4, ggplot2::aes(x = Method, y = EXP)) +
          ggplot2::geom_bar(stat = "identity", fill = "blue") +
          ggplot2::scale_y_continuous(name = y_lab,
                             limits=c(yMIN, yMAX),
                             breaks=seq(yMIN, yMAX, ySTEP)) +
          ggplot2::scale_x_discrete(name = "Management Strategy",
                                    labels = mgmt_labels) +
          ggplot2::geom_text(data = TF4,
                             ggplot2::aes(x = Method, y = EXP, label = round(EXP, 2)),
                    vjust = -.25) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 14)) +
          ggplot2::ggtitle(gg_title, subtitle = txt)
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/EXP/EXPapplied/",
                               fieldname, "_tot",
                               ifelse(expvar == "aa_n", "N", "SR"),
                               "applied_bar_", fxn, "_", sim_year, "_",
                               opt, ".png"),
                        plot = p, device = "png", scale = 1,
                        width = 7.5, height = 7.5, units = "in")
      }
      #return(p)
    },
    #' @description
    #' Generate a histogram of site specific experimental inptut rates within
    #' a field. Uses the average site-specific rate for each point across the
    #' simulation, indicating the mean optimum rate at each point across all
    #' economic conditions.
    #' @param NRopt Data frame with the net-returns and experimental optimums
    #' for every point for every simulation iteration.
    #' @param TF4 Data.frame with columns for the amount of experimental rates applied
    #' for each strategy.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/EXP/ssoptEXP'.
    plotSSOPThist = function(NRopt,
                             TF4,
                             expvar,
                             fieldname,
                             fxn,
                             sim_year,
                             opt,
                             SAVE,
                             out_path) {
      NRplot <- as.data.frame(NRopt)
      gg_title <- paste0("Site-specific profit maximizing ",
                         ifelse(expvar=="aa_n", "nitrogen", "seed"), " rates")
      sub_title <-  paste0("Mean SS.Opt ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Rate = ",
                           round(mean(NRopt[, "EXP.rate.ssopt"], na.rm = TRUE), 2),
                           " lbs/acre, SD = ",
                           round(sd(NRopt[, "EXP.rate.ssopt"], na.rm = TRUE), 2),
                           " lbs/acre")
      if (TF4[which(TF4$Method == "EXP.ssopt"), "EXP"] == 0) {
        p <-
          ggplot2::ggplot(NRplot) +
            ggplot2::geom_histogram(ggplot2::aes(x = EXP.rate.ssopt + 1),
                           bins = 1,
                           col = "white",
                           fill = "blue",
                           na.rm = TRUE) +
            ggplot2::labs(x = "lbs/acre") +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(gg_title, subtitle = sub_title) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14))
      } else {
        bin_num <- DescTools::RoundTo(max(NRplot$EXP.rate.ssopt,
                                          na.rm = TRUE), 5, ceiling) / 5
        xMIN <- DescTools::RoundTo(min(NRplot$EXP.rate.ssopt,
                                       na.rm = TRUE), 5, floor)
        xMAX <- DescTools::RoundTo(max(NRplot$EXP.rate.ssopt,
                                       na.rm = TRUE), 5, ceiling)
        xSTEP <- (xMAX - xMIN) / 10
        p <-
          ggplot2::ggplot(NRplot) +
            ggplot2::geom_histogram(ggplot2::aes(x = EXP.rate.ssopt + 1),
                                    bins = bin_num,
                                    col = "white",
                                    fill = "blue",
                                    na.rm = TRUE)  +
            ggplot2::scale_x_continuous(name = "lbs/acre",
                               limits = c(xMIN, xMAX),
                               breaks = seq(xMIN, xMAX, xSTEP),
                               oob = scales::squish) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(gg_title, subtitle = sub_title) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14))
      }
      yMIN <- 0
      yMAX <- DescTools::RoundTo(max(ggplot2::ggplot_build(p)$data[[1]]$count), 5, ceiling)
      ySTEP <- (yMAX - yMIN) / 10
      p <- p +
        ggplot2::geom_point(ggplot2::aes(x = round(mean(NRopt[, "EXP.rate.ssopt"],
                                               na.rm = TRUE), 0),
                                y = yMIN + ySTEP),
                            col = "red",
                            cex = 2) +
        ggplot2::scale_y_continuous(name = "Frequency",
                                    limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP))
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/EXP/ssoptEXP/",
                               fieldname, "_ssopt",
                               ifelse(expvar == "aa_n", "N", "SR"),
                               "_hist_", fxn, "_", sim_year, "_",
                               opt, ".png"),
                        plot = p, device = "png", scale = 1,
                        width = 7.5, height = 7.5, units = "in")
      }
      #return(p)
    },
    #' @description
    #' Generate a histogram of full-field experimental input rates identified
    #' in the simulation. Shows the distribution of full-field optimum rates
    #' across all economic scenarios in the simulation.
    #' @param Bp.var Data.table containing the average net-returns from the field for each
    #' management type for every iteration of the simulation.
    #' @param TF4 Data.frame with columns for the amount of experimental rates applied
    #' for each strategy.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param fxn The functional form of the models used for analysis.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Data saved in 'Outputs/EXP/ffoptEXP'.
    plotFFOPThist = function(Bp.var,
                             TF4,
                             expvar,
                             fieldname,
                             fxn,
                             sim_year,
                             opt,
                             SAVE,
                             out_path) {
      Bp.var <- as.data.frame(Bp.var)
      x_lab <- paste0("Profit Maximizing Top-Dress ",
                      ifelse(expvar == "aa_n", "N", "Seed"),
                      " Rate (lbs/acre)")
      gg_title <- paste0("Variation in best full-field uniform ",
                         ifelse(expvar == "aa_n", "N", "seed"),
                         " rate over years")
      sub_title <- paste0("Mean FF.Opt ",
                          ifelse(expvar == "aa_n", "N", "Seed"),
                          " Rate = ",
                          round(mean(Bp.var[, "ffopt.EXPrate"], na.rm = TRUE), 2),
                          " lbs/ac, SD = ",
                          round(sd(Bp.var[, "ffopt.EXPrate"], na.rm = TRUE), 2),
                          " lbs/ac")
      if (TF4[which(TF4$Method == "EXP.ffopt"), "EXP"] == 0) {
        p <-
          ggplot2::ggplot(Bp.var) +
            ggplot2::geom_histogram(ggplot2::aes(x = ffopt.EXPrate + 1),
                           bins=1,
                           col="white",
                           fill="blue",
                           na.rm=TRUE) +
            ggplot2::labs(x=x_lab) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle(gg_title, subtitle=sub_title) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14))
      } else {
        bin_num <- DescTools::RoundTo(max(Bp.var$ffopt.EXPrate,
                                          na.rm = TRUE), 5, ceiling) / 5
        xMIN <- DescTools::RoundTo(min(Bp.var$ffopt.EXPrate,
                                       na.rm = TRUE), 5, floor)
        xMAX <- DescTools::RoundTo(max(Bp.var$ffopt.EXPrate,
                                       na.rm = TRUE), 5, ceiling)
        xSTEP <- (xMAX - xMIN) / 10
        p <-
          ggplot2::ggplot(Bp.var) +
          ggplot2::geom_histogram(ggplot2::aes(x = ffopt.EXPrate),
                                  bins = bin_num,
                                  col = "white",
                                  fill = "blue",
                                  na.rm = TRUE) +
          ggplot2::scale_x_continuous(name = x_lab,
                                      limits = c(xMIN, xMAX),
                                      breaks = seq(xMIN, xMAX, xSTEP),
                                      oob = scales::squish) +
          ggplot2::theme_bw() +
          ggplot2::ggtitle(gg_title, subtitle = sub_title) +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 14))
      }
      yMIN <- DescTools::RoundTo(min(ggplot2::ggplot_build(p)$data[[1]]$count), 5, floor)
      yMAX <- DescTools::RoundTo(max(ggplot2::ggplot_build(p)$data[[1]]$count), 5, ceiling)
      ySTEP <- (yMAX - yMIN) / 10
      p <- p +
        ggplot2::geom_point(ggplot2::aes(x = round(mean(Bp.var[, "ffopt.EXPrate"],
                                               na.rm = TRUE), 0),
                                y = yMIN + ySTEP),
                            col = "red",
                            cex = 2) +
        ggplot2::scale_y_continuous(name = "Frequency",
                                    limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP))
      if (SAVE) {
        ggplot2::ggsave(paste0(out_path, "/Outputs/EXP/ffoptEXP/",
                               fieldname, "_ffopt",
                               ifelse(expvar == "aa_n", "N", "SR"),
                               "_hist_", fxn, "_", sim_year, "_",
                               opt, ".png"),
                        plot = p, device = "png", scale = 1,
                        width = 7.5, height = 7.5, units = "in")
      }
      #return(p)
    },
    #' @description
    #' This method is for plotting maps of simulation outcomes. These
    #' include the net-returns from the management strategies, predicted
    #' responses, and optimized rates. Is a wrapper to OFPE::plotMaps().
    #' @param dat Data frame with the net-returns and experimental optimums
    #' for every point for every simulation iteration.
    #' @param var The label of the variable to map. Used in figure name.
    #' @param var_col_name The name of the column of the variable in the
    #' supplied data ('dat').
    #' @param var_label The label to be applied to the legend of the map
    #' corresponding to the variable mapped.
    #' @param var_main_label The main label to apply to the map.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param sim_year Year that the simulation was performed for. Indicates the
    #' weather conditions used.
    #' @param fxn The functional form of the models used for analysis.
    #' @param opt The optimization method used in the simulation.
    #' @param SAVE Logical, whether to save figure.
    #' @param farmername The name of the farmer that manages the field.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Maps saved in 'Outputs/Maps'.
    plotSimMaps = function(dat,
                           var,
                           var_col_name,
                           var_label,
                           var_main_label,
                           fieldname,
                           sim_year,
                           fxn,
                           opt = NULL,
                           SAVE = TRUE,
                           farmername,
                           out_path) {
      utm_zone <- OFPE::findUTMzone(self$simClass$dbCon$db,
                                    fieldname = self$simClass$datClass$fieldname[1])
      p <- OFPE::plotMaps(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername,
                          utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()
      if (SAVE) {
        ggplot2::ggsave(
          file = paste0(out_path, "/Outputs/Maps/",
                      fieldname, "_", tolower(var),
                      "_map_", fxn, "_", sim_year, "_", opt, ".png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      #return(p)
    },
    #' @description
    #' Function for creating a map of the actual net-return observed in the
    #' field for a selected year available in the data the user specified
    #' for fitting the models (i.e. if you used multiple years to fit a model
    #' you can map the net-return for either year).
    #'
    #' This function uses the observed data for the specified year and if
    #' there is multiple response variables, predicts them to the yield
    #' dataset because this is more spatially dense. This uses the model
    #' fit in the analysis step that was used in the simulation. After
    #' both responses are in the same dataset, net-return is calculated
    #' based on the actual as-applied experimental input and mapped across
    #' the field.
    #' @param dat Data.table or data.frame to use for calculating net-return.
    #' Must include all covariates used in the model fitting processes. These
    #' are the locations actual net-return is calculated for.
    #' @param year A single year, present in 'dat', for which to calculate and
    #' map the actual net-return.
    #' @param Bp The base price corresponding to the price for the system
    #' type selected by the user (i.e. conventional or organic).
    #' @param B0pd The intercept for the protein premium/dockage equation.
    #' @param B1pd The coefficient for protein in the protein premium/dockage
    #' equation.
    #' @param B2pd The coefficient for protein squared for the protein
    #' premium/dockage equation.
    #' @param CEXP The cost of the experimental input.
    #' @param FC Fixed costs ($/acre) associated with production, not including
    #' the input of interest. This includes things like the cost of labor, fuel, etc.
    #' @param ssAC The cost ($/acre) of using site-specific technology or variable rate
    #' applications. For farmers that have variable rate technology this cost may be zero,
    #' otherwise is the cost per acre to hire the equipment/operators with variable rate
    #' technology.
    #' @param respvar Response variable(s) used to optimize experimental inputs based
    #' off of. The user can select 'Yield' and/or 'Protein' based on data
    #' availability.
    #' @param expvar Experimental variable optimized, select/input
    #' 'As-Applied Nitrogen' or 'As-Applied Seed Rate'. This is the type of
    #' input that was experimentally varied across the field as part of the
    #' on-farm experimentation.
    #' @param modClass ModClass R6 object that has been initialized and with
    #' fitted models. The fitted models in this object are used to predict
    #' crop responses at each location in the simulation data for all
    #' experimental rates in the user specified range.
    #' @param fieldname Unique field name corresponding to all fields used in the simulation.
    #' @param farmername The name of the farmer that manages the field.
    #' @param fxn The functional form of the models used for analysis.
    #' @param SAVE Logical, whether to save figure.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the simulation.
    #' @return Map of the actual net-return from a given year in 'Outputs/Maps'.
    plotActNR = function(dat,
                         year,
                         Bp,
                         B0pd,
                         B1pd,
                         B2pd,
                         CEXP,
                         FC,
                         ssAC,
                         respvar,
                         expvar,
                         modClass,
                         fieldname,
                         farmername,
                         fxn,
                         SAVE,
                         out_path) {
      dat <- dat[which(dat$year == year), ]

      # if respvar > 1 and all(respvar ! in names(dat))
      # -> predict other respvar to data
      if (length(respvar > 1)) {
        miss_resp <- respvar[which(!respvar %in% names(dat))]
        # predict for all missing respvars (i.e. protein)
        for (i in 1:length(miss_resp)) {
          miss_index <- grep(miss_resp[i], respvar)
          dat$pred <- modClass$mod_list[[miss_index]]$predResps(
            dat, modClass$mod_list[[miss_index]]$m
          )
          names(dat)[grep("^pred$", names(dat))] <- respvar[miss_index]
        }
      }
      # calculate NR for each point
      names(dat)[grep(expvar, names(dat))] <- "exp"
      predInd <- ifelse(any(respvar == "pro"), 1, 0)
      if (predInd == 1) {
        P <- Bp + (B0pd + B1pd * dat$pro + B2pd * dat$pro^2);
        dat$NR <- (dat$yld * P) - CEXP * dat$exp - FC - ssAC;
      } else {
        dat$NR <- (dat$yld * (Bp + B0pd)) - CEXP * dat$exp - FC - ssAC;
      }
      # pass to plot sim maps
      var_col_name <- "NR"
      var_label <- "Net-return ($/ac)"
      var_main_label <- paste0("Observed net-return in ", year)
      utm_zone <- OFPE::findUTMzone(self$simClass$dbCon$db,
                                    fieldname = self$simClass$datClass$fieldname[1])
      p <- OFPE::plotMaps(dat,
                          var_col_name,
                          var_label,
                          var_main_label,
                          fieldname,
                          farmername,
                          utm_zone) %>%
        suppressMessages() %>%
        suppressWarnings()
      if (SAVE) {
        ggplot2::ggsave(
          file = paste0(out_path, "/Outputs/Maps/",
                        fieldname, "_observed_NR",
                        "_map_", year, "_", fxn, ".png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      #return(p)
    }
  ),
  private = list(
    .setupOP = function() {
      cwd <- paste0(self$simClass$out_path, "/Outputs") # outputs working directory
      if (!file.exists(cwd)) {
        dir.create(cwd)
        dir.create(paste0(cwd, "/", "Predictions"))
        dir.create(paste0(cwd, "/", "Maps"))
        #dir.create(paste0(cwd, "/", "Exploratory"))
        dir.create(paste0(cwd, "/", "EXP"))
        dir.create(paste0(cwd, "/", "EXP/ffoptEXP"))
        dir.create(paste0(cwd, "/", "EXP/ssoptEXP"))
        dir.create(paste0(cwd, "/", "EXP/EXPapplied"))
        dir.create(paste0(cwd, "/", "NR"))
        dir.create(paste0(cwd, "/", "NR/NRbarPlots"))
        dir.create(paste0(cwd, "/", "NR/NRboxPlots"))
        dir.create(paste0(cwd, "/", "NR/NRprobabilities"))
        dir.create(paste0(cwd, "/", "SimData"))
      } else {
        if (!file.exists(paste0(cwd, "/", "Predictions"))) {
          dir.create(paste0(cwd, "/", "Predictions"))
        }
        if (!file.exists(paste0(cwd, "/", "Maps"))) {
          dir.create(paste0(cwd, "/", "Maps"))
        }
        # if (!file.exists(paste0(cwd, "/", "Exploratory"))) {
        #   dir.create(paste0(cwd, "/", "Exploratory"))
        # }
        if (!file.exists(paste0(cwd, "/", "EXP"))) {
          dir.create(paste0(cwd, "/", "EXP"))
        }
        if (!file.exists(paste0(cwd, "/", "EXP/ffoptEXP"))) {
          dir.create(paste0(cwd, "/", "EXP/ffoptEXP"))
        }
        if (!file.exists(paste0(cwd, "/", "EXP/ssoptEXP"))) {
          dir.create(paste0(cwd, "/", "EXP/ssoptEXP"))
        }
        if (!file.exists(paste0(cwd, "/", "EXP/EXPapplied"))) {
          dir.create(paste0(cwd, "/", "EXP/EXPapplied"))
        }
        if (!file.exists(paste0(cwd, "/", "NR"))) {
          dir.create(paste0(cwd, "/", "NR"))
        }
        if (!file.exists(paste0(cwd, "/", "NR/NRbarPlots"))) {
          dir.create(paste0(cwd, "/", "NR/NRbarPlots"))
        }
        if (!file.exists(paste0(cwd, "/", "NR/NRboxPlots"))) {
          dir.create(paste0(cwd, "/", "NR/NRboxPlots"))
        }
        if (!file.exists(paste0(cwd, "/", "NR/NRprobabilities"))) {
          dir.create(paste0(cwd, "/", "NR/NRprobabilities"))
        }
        if (!file.exists(paste0(cwd, "/", "SimData"))) {
          dir.create(paste0(cwd, "/", "SimData"))
        }
      }
    },
    .setupNRdat = function(NRopt, NRffmax) {
      NRopt$field <- match(NRopt$field, self$simClass$datClass$fieldname_codes$field)
      NRopt <- aggregate(NRopt, list(NRopt$x, NRopt$y), mean, na.rm = TRUE)
      NRopt$field <-
        self$simClass$datClass$fieldname_codes[match(NRopt$field,
                                                     self$simClass$datClass$fieldname_codes$field_code),
                                               "field"]
      NRffmax <- sapply(NRffmax, mean) %>% as.matrix() %>% t() %>% as.data.frame()
      NRdat <- list(NRopt = NRopt,
                    NRffmax = NRffmax)
      return(NRdat)
    },
    .getTF4 = function(NRopt, NRffmax, fieldsize, fs) {
      TF4 <- data.frame(EXP.ssopt = (sum(NRopt[,'EXP.rate.ssopt'], na.rm=T) *
                                       fieldsize) / nrow(NRopt),
                        EXP.min = 0,
                        EXP.fs = fs * fieldsize,
                        EXP.ffopt = NRffmax[, 'EXP.rate'] * fieldsize)
      TF4 <- tidyr::pivot_longer(TF4,
                                 dplyr::everything(),
                                 names_to = "Method",
                                 values_to = "EXP")
      TF4$Method <- factor(TF4$Method)
      mgmt_order <- c(grep("EXP.min", levels(TF4$Method)),
                      grep("EXP.fs", levels(TF4$Method)),
                      grep("EXP.ssopt", levels(TF4$Method)),
                      grep("EXP.ffopt", levels(TF4$Method)))
      TF4$Method <- factor(TF4$Method, levels(TF4$Method)[mgmt_order])
      return(TF4)
    },
    .estsVsExpPlot = function(var, # NR, pred_yld, pred_pro
                              DNR,
                              expvar,
                              fieldname,
                              sim_year,
                              Bp,
                              CEXP,
                              AAmin,
                              AArateCutoff,
                              fxn) {
      stopifnot(any(grepl("NR|yld|pro", var)))
      if (grepl("NR", var)) {
        names(DNR)[grep(paste0("^", var, "$"), names(DNR))] <- "var"
      } else {
        names(DNR)[grep(paste0("^pred_", var, "$"), names(DNR))] <- "var"
      }
      xMIN <- AAmin
      xMAX  <- AArateCutoff
      xSTEP <- (AArateCutoff - AAmin) / 10
      if (grepl("pro", var)) {
        yMIN <- DescTools::RoundTo(min(DNR$var, na.rm = T), 5, floor)
        yMAX <- DescTools::RoundTo(max(DNR$var, na.rm = T), 5, ceiling)
        ySTEP <- (yMAX - yMIN) / 5
      } else {
        yMIN <- DescTools::RoundTo(min(DNR$var, na.rm = T), 10, floor)
        yMAX <- DescTools::RoundTo(max(DNR$var, na.rm = T), 10, ceiling)
        ySTEP <- (yMAX - yMIN) / 10
      }
      names(DNR)[grep(expvar, names(DNR))] <- "exp"
      var_color <- ifelse(grepl("NR", var), "green",
                          ifelse(grepl("yld", var), "red",
                                 "cyan"))
      y_lab <- ifelse(grepl("NR", var), "Estimated Net-Return ($)",
                      ifelse(grepl("yld", var), "Predicted Yield (bu/ac)",
                             "Predicted Grain Protein %"))
      x_lab <- paste0(ifelse(expvar == "aa_n", "Nitrogen",  "Seed"), " (lbs/ac)")
      sub_title <-  paste0(fxn, " : Base Price = $",
                           round(Bp, 2), ", ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Cost = $", round(CEXP, 2))
      var_plot <-
        ggplot2::ggplot(DNR, ggplot2::aes(x = exp, y = var)) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_smooth(color = var_color) +
        ggplot2::labs(y = y_lab,
                      x =x_lab) +
        ggplot2::ggtitle(paste0(fieldname, " ", sim_year),
                         subtitle = sub_title) +
        ggplot2::scale_y_continuous(limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP)) +
        ggplot2::scale_x_continuous(limits = c(xMIN, xMAX),
                                    breaks = seq(xMIN, xMAX, xSTEP)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 14))
      if (any(DNR$var < 0)) {
        var_plot <- var_plot +
          ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2)
      }
      return(var_plot)
    }
  )
)
