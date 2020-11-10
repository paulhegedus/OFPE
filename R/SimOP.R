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
    #' @field input_list Optional, list of inputs required for generating the
    #' output figures. Recreation of relevant info from a SimClass object. Only
    #' needed if a SimClass object is not passed into class.
    input_list = NULL,
    #' @field out_path Output path to create folder for outputs. Taken from either
    #' a SimClass object or an optional 'input_list' field upon initialization.
    out_path = NULL,
    #' @field SAVE Whether to save simulation outputs If NULL uses the user selected
    #' choice. If not NULL and is logical, argument replaces previously set SAVE
    #' options for the entire class.
    SAVE = NULL,

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
    #' @param simClass Optional, SimClass R6 object that has been initialized and the OFPE
    #' simulation has been executed. This class holds the information necessary
    #' to produce the output figures, maps, and tables from the Monte Carlo
    #' simulation.
    #' @param input_list Optional, list of inputs required for generating the
    #' output figures. Recreation of relevant info from a SimClass object. Only
    #' needed if a SimClass object is not passed into class.
    #' @param create create Logical, whether to create the 'Output' folder and save
    #' figures, maps, and tables. Default is TRUE, pass FALSE to skip output folder
    #' step and prevent any output generation.
    #' @return A folder created in the path for model output figures.
    initialize = function(simClass = NULL,
                          input_list = NULL,
                          create = TRUE) {
      stopifnot(
        is.logical(create)
      )
      if (!is.null(simClass)) {
        self$simClass <- simClass
        self$out_path <- self$simClass$out_path
        self$SAVE <- self$simClass$SAVE
      }
      if (!is.null(input_list)) {
        stopifnot(
          !is.null(input_list$dat_path),
          !is.null(input_list$unique_fieldname),
          !is.null(input_list$unique_fxn),
          !is.null(input_list$sim_years),
          !is.null(input_list$opt),
          !is.null(input_list$fieldsize),
          !is.null(input_list$fs),
          !is.null(input_list$expvar),
          !is.null(input_list$farmername),
          !is.null(input_list$respvar),
          !is.null(input_list$db),
          !is.null(input_list$utm_fieldname)
        )
        self$input_list <- input_list
        stopifnot(!is.null(input_list$out_path))
        self$out_path <- input_list$out_path
      }
      if (create) {
        private$.setupOP()
      } else {
        self$SAVE <- FALSE
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
      stopifnot(
        !is.null(self$simClass) | !is.null(self$input_list)
      )
      if (is.null(SAVE)) {
        SAVE <- self$SAVE
        if (is.null(SAVE)) {
          SAVE <- FALSE
        }
      } else {
        stopifnot(is.logical(SAVE))
        self$SAVE <- SAVE
      }

      if (!is.null(self$simClass)) {
        dat_path <- self$simClass$dat_path
        unique_fieldname <- self$simClass$unique_fieldname
        unique_fxn <- self$simClass$unique_fxn
        sim_years <- self$simClass$sim_years
        opt <- self$simClass$opt
        fieldsize <- self$simClass$fieldsize
        fs <- self$simClass$fs
        expvar <- self$simClass$datClass$expvar
        farmername <- self$simClass$datClass$farmername
        respvar <- self$simClass$datClass$respvar
        db <- self$simClass$dbCon$db
        utm_fieldname <- self$simClass$datClass$fieldname[1]
      } else {
        dat_path <- self$input_list$dat_path
        unique_fieldname <- self$input_list$unique_fieldname
        unique_fxn <- self$input_list$unique_fxn
        sim_years <- self$input_list$sim_years
        opt <- self$input_list$opt
        fieldsize <- self$input_list$fieldsize
        fs <- self$input_list$fs
        expvar <- self$input_list$expvar
        farmername <- self$input_list$farmername
        respvar <- self$input_list$respvar
        db <- self$input_list$db
        utm_fieldname <- self$input_list$utm_fieldname
      }

      if (SAVE) {
        for (i in 1:length(sim_years)) {
          #### Bp.var plots ####
          Bp.var <- data.table::fread(
            paste0(dat_path,
                   unique_fieldname, "_BpVar_",
                   unique_fxn, "_",
                   sim_years[i], "_",
                   opt, ".csv")
          )

          self$plotNRbox(
            Bp.var,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          self$mgmtNRprobTable(
            "NR.ssopt",
            Bp.var,
            1000,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          self$plotNRbar(
            Bp.var,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          #### Get NRopt & NRffmax & make TF4 ####
          ## gets a subset of all points for all rates and iterations
          NRopt <- private$.loadNRopt(dat_path,
                                      unique_fieldname,
                                      unique_fxn,
                                      sim_years[i],
                                      opt)
          NRffmax <- data.table::fread(
            paste0(dat_path,
                   unique_fieldname, "_NRffMaxData_",
                   unique_fxn, "_",
                   sim_years[i], "_",
                   opt, ".csv")
          )
          #NRdat <- private$.setupNRdat(NRopt, NRffmax)
          TF4 <- private$.getTF4(NRopt,
                                 NRffmax,
                                 fieldsize,
                                 fs)
          rm(NRffmax)
          #### NRopt & NRffmax plots ####
          self$plotFFOPThist(
            Bp.var,
            TF4,
            expvar,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          rm(Bp.var)
          #### End Bp.var Plots ####
          self$plotTotExpAppl(
            TF4,
            expvar,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          self$plotSSOPThist(
            NRopt,
            TF4,
            expvar,
            unique_fieldname,
            unique_fxn,
            sim_years[i],
            opt,
            SAVE,
            self$out_path
          )
          #### Maps ####
          ## SSOPT map (avg all sim)
          self$plotSimMaps(
            NRopt,
            paste0("SSOPT_", ifelse(expvar == "aa_n", "N", "SR")),
            "EXP.rate.ssopt",
            paste0(ifelse(expvar == "aa_n", "N", "Seed Rate"), " Rate (lbs/ac)"),
            paste0("SS.opt ",
                   ifelse(expvar == "aa_n", "N", "Seed"),
                   " rates for ", sim_years[i], " conditions"),
            unique_fieldname,
            sim_years[i],
            unique_fxn,
            opt,
            SAVE,
            farmername,
            self$out_path,
            db,
            utm_fieldname
          )
          # SSOPT NR map
          self$plotSimMaps(
            NRopt,
            "SSOPTNR",
            "NR.ssopt",
            "Net Return ($/ac)",
            paste0("SS.Opt NR for ", sim_years[i], " conditions"),
            unique_fieldname,
            sim_years[i],
            unique_fxn,
            opt,
            SAVE,
            farmername,
            self$out_path,
            db,
            utm_fieldname
          )
          # FFOPT NR map
          self$plotSimMaps(
            NRopt,
            "FFOPTNR",
            "NR.ffopt",
            "Net Return ($/ac)",
            paste0("FF.Opt NR (", NRopt$EXP.rate.ffopt[1],
                   " lbs/ac) for ", sim_years[i],
                   " conditions"),
            unique_fieldname,
            sim_years[i],
            unique_fxn,
            opt,
            SAVE,
            farmername,
            self$out_path,
            db,
            utm_fieldname
          )
          # SSOPT Est. Yld map
          self$plotSimMaps(
            NRopt,
            "estYld",
            "yld.opt",
            "Yield (bu/ac)",
            paste0("Predicted yield for ", sim_years[i], " conditions"),
            unique_fieldname,
            sim_years[i],
            unique_fxn,
            opt,
            SAVE,
            farmername,
            self$out_path,
            db,
            utm_fieldname
          )
          if (any(grepl("pro", respvar))) {
            # SSOPT Est. Pro map
            self$plotSimMaps(
              NRopt,
              "estPro",
              "pro.opt",
              "Protein (%)",
              paste0("Predicted protein for ", sim_years[i], " conditions"),
              unique_fieldname,
              sim_years[i],
              unique_fxn,
              opt,
              SAVE,
              farmername,
              self$out_path,
              db,
              utm_fieldname
            )
          }
        }
        rm(NRopt, TF4)
      } else{
        if (!is.null(self$simClass)) {
          self$simClass$cleanUp()
        }
      }
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
                                     -c("BaseP", "ffopt.EXPrate", "EXP.cost", "sim"),
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
        try({dev.off()}, silent = TRUE)
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
        try({dev.off()}, silent = TRUE)
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
        try({dev.off()}, silent = TRUE)
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
      NRopt <- as.data.frame(NRopt)
      gg_title <- paste0("Site-specific profit maximizing ",
                         ifelse(expvar=="aa_n", "nitrogen", "seed"), " rates")
      sub_title <-  paste0("Mean SS.Opt ",
                           ifelse(expvar == "aa_n", "N", "Seed"),
                           " Rate = ",
                           round(mean(NRopt$EXP.rate.ssopt, na.rm = TRUE), 2),
                           " lbs/acre, SD = ",
                           round(sd(NRopt$EXP.rate.ssopt, na.rm = TRUE), 2),
                           " lbs/acre")
      if (TF4[which(TF4$Method == "EXP.ssopt"), "EXP"] == 0) {
        p <-
          ggplot2::ggplot(NRopt) +
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
        bin_num <- DescTools::RoundTo(max(NRopt$EXP.rate.ssopt,
                                          na.rm = TRUE), 5, ceiling) / 5
        xMIN <- DescTools::RoundTo(min(NRopt$EXP.rate.ssopt,
                                       na.rm = TRUE), 5, floor)
        xMAX <- DescTools::RoundTo(max(NRopt$EXP.rate.ssopt,
                                       na.rm = TRUE), 5, ceiling)
        xSTEP <- (xMAX - xMIN) / 10
        p <-
          ggplot2::ggplot(NRopt) +
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
        ggplot2::geom_vline(ggplot2::aes(xintercept = round(mean(NRopt$EXP.rate.ssopt,
                                                        na.rm = TRUE), 0)),
                            col = "red",
                            linetype = 2) +
        ggplot2::scale_y_continuous(name = "Frequency",
                                    limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP))
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
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
        ggplot2::geom_vline(ggplot2::aes(xintercept = round(mean(Bp.var[, "ffopt.EXPrate"],
                                                        na.rm = TRUE), 0)),
                            col = "red",
                            linetype = 2) +
        ggplot2::scale_y_continuous(name = "Frequency",
                                    limits = c(yMIN, yMAX),
                                    breaks = seq(yMIN, yMAX, ySTEP))
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
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
    #' @param db Connection to the OFPE database to identify UTM zone.
    #' @param utm_fieldname Name of the field for identifying the UTM zone.
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
                           out_path,
                           db,
                           utm_fieldname) {
      utm_zone <- OFPE::findUTMzone(db,
                                    fieldname = utm_fieldname)
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
        try({dev.off()}, silent = TRUE)
        ggplot2::ggsave(
          file = paste0(out_path, "/Outputs/Maps/",
                      fieldname, "_", tolower(var),
                      "_map_", fxn, "_", sim_year, "_", opt, ".png"),
          plot = p, device = "png",
          width = 7.5, height = 7.5, units = "in"
        )
      }
      #return(p)
    }
  ),
  private = list(
    .setupOP = function() {
      cwd <- paste0(self$out_path, "/Outputs") # outputs working directory
      if (!file.exists(cwd)) {
        dir.create(cwd)
        #dir.create(paste0(cwd, "/", "Exploratory"))
        dir.create(paste0(cwd, "/", "Maps"))
        dir.create(paste0(cwd, "/", "EXP"))
        dir.create(paste0(cwd, "/", "EXP/ffoptEXP"))
        dir.create(paste0(cwd, "/", "EXP/ssoptEXP"))
        dir.create(paste0(cwd, "/", "EXP/EXPapplied"))
        dir.create(paste0(cwd, "/", "NR"))
        dir.create(paste0(cwd, "/", "NR/NRbarPlots"))
        dir.create(paste0(cwd, "/", "NR/NRboxPlots"))
        dir.create(paste0(cwd, "/", "NR/NRprobabilities"))
      } else {
        # if (!file.exists(paste0(cwd, "/", "Exploratory"))) {
        #   dir.create(paste0(cwd, "/", "Exploratory"))
        # }
        if (!file.exists(paste0(cwd, "/", "Maps"))) {
          dir.create(paste0(cwd, "/", "Maps"))
        }
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
      }
    },
    # .setupNRdat = function(NRopt, NRffmax) {
    #   browser()
    #
    #   NRopt$field <- match(NRopt$field, self$simClass$datClass$fieldname_codes$field)
    #   NRopt <- aggregate(NRopt, list(NRopt$x, NRopt$y), mean, na.rm = TRUE)
    #   NRopt$field <-
    #     self$simClass$datClass$fieldname_codes[match(NRopt$field,
    #                                                  self$simClass$datClass$fieldname_codes$field_code),
    #                                            "field"]
    #   NRffmax2 <- sapply(NRffmax, mean) %>% as.matrix() %>% t() %>% as.data.frame()
    #   NRdat <- list(NRopt = NRopt,
    #                 NRffmax = NRffmax)
    #   return(NRdat)
    # },
    .getTF4 = function(NRopt, NRffmax, fieldsize, fs) {
      TF4 <- data.frame(EXP.ssopt = (sum(NRopt[,'EXP.rate.ssopt'], na.rm = T) *
                                       fieldsize) / nrow(NRopt),
                        EXP.min = 0,
                        EXP.fs = fs * fieldsize,
                        EXP.ffopt = mean(NRffmax$EXP.rate, na.rm = T) * fieldsize)
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
    .loadNRopt = function(dat_path, unique_fieldname,
                          unique_fxn, sim_year, opt) {
      on.exit(closeAllConnections())
      ## get col names
      NRopt_names <- read.csv(file = paste0(dat_path,
                                            unique_fieldname, "_NRopt_",
                                            unique_fxn, "_",
                                            sim_year, "_",
                                            opt, ".csv"),
                              nrows = 1,
                              header = TRUE) %>%
        names()
      ## average all cols by x and y
      NRopt_names <- NRopt_names[!1:length(NRopt_names) %in%
                                          grep("^x$|^y$|^field$", NRopt_names)]
      avg_query <- paste0("AVG([", NRopt_names, "])") %>%
        paste(collapse = ", ")
      NRopt <- sqldf::read.csv.sql(
        paste0(dat_path,
               unique_fieldname, "_NRopt_",
               unique_fxn, "_",
               sim_year, "_",
               opt, ".csv"),
        sql = paste0("SELECT x, y, field, ", avg_query,
                     "FROM file GROUP BY x, y")
      ) %>%
        `names<-`(c("x", "y", "field", NRopt_names))
      NRcols <- grep("NR", names(NRopt))
      for (i in 1:length(NRcols)) {
        NRopt <- NRopt[NRopt[, NRcols[i]] > -50000 &
                         NRopt[, NRcols[i]] < 50000, ]
      }

      ## ORIGINAL LOAD ALL ROWS
      # NRopt2 <- data.table::fread(
      #   paste0(dat_path,
      #          unique_fieldname, "_NRopt_",
      #          unique_fxn, "_",
      #          sim_year, "_",
      #          opt, ".csv"))
      # NRopt2 <- aggregate(NRopt2, list(NRopt2$x, NRopt2$y), mean, na.rm = TRUE)
      return(NRopt)
    }
  )
)
