# When using this code in past versions of httk, we load the needed data tables
# to assure we are evaluating against similar values:
#   load("NewInVivoTablesForHTTK.RData")
#   pc.data <- read.csv("Pearce2017-PC-data.txt")

#' Assess the current performance of httk relative to historical benchmarks
#' 
#' The function performs a series of "sanity checks" and predictive performance
#' benchmarks so that the impact of changes to the data, models, and 
#' implementation of the R package can be tested. Plots can be generated showing 
#' how the performance of the current version compares with past releases of
#' httk.
#' 
#' Historically some refinements made to one aspect of httk have unintentionally
#' impacted other aspects. Most notably errors have occasionally been introduced 
#' with respect to units (v1.9, v2.1.0). This benchmarking tool is intended to
#' reduce the chance of these errors occurring in the future.
#' 
#' Past performance was retroactively evaluated by manually installing previous 
#' versions of 
#' the package from \url{https://cran.r-project.org/src/contrib/Archive/httk/} and
#' then adding the code for \code{benchmark_httk} at the command line
#' interface. 
#'
#' The basic tests are important -- if the output units for key functions are wrong, not
#' much can be right. Past unit errors were linked to an incorrect unit 
#' conversions made within an individual function. Since the usage of 
#' \code{\link{convert_units}} became standard throughout httk,
#' unit problems are hopefully less likely.
#'
#' There are two Monte Carlo tests. One compares \code{\link{calc_mc_css}} 95th percentile
#' steady-state plasma concentrations for a 1 mg/kg/day exposure
#' against the Css values calculated by SimCyp and reported in Wetmore et al.
#' (2012,2015). These have gradually diverged as the assumptions for httk have
#' shifted to better describe non-pharmaceutical, commercial chemicals.
#'
#' The in vivo tests are in some ways the most important, as they establish the
#' overall predictability for httk for Cmax, AUC, and Css. The in vivo 
#' statistics are currently based on comparisons to the in vivo
#' data compiled by Wambaugh et al. (2018). We see that when the tissue
#' partition coefficient calibrations were introduced in v1.6 that the
#' overall predictability for in vivo endpoints was reduced (increased RMSLE).
#' If this phenomena continues as new in vivo evaluation data become available,
#' we may need to revisit whether evaluation against experimentally-derived 
#' partition coefficients can actually be used for calibration, or just merely
#' for establishing confidence intervals.
#'
#' The partition coefficient tests provide an important check of the httk
#' implementation of the Schmitt (2008) model for tissue:plasma equilibrium 
#' distribution. These predictions heavily rely on accurate description of 
#' tissue composition and the ability to predict the ionization state of the
#' compounds being modeled.
#' 
#' @param basic.check Whether to run the basic checks, including uM and 
#' mg/L units for \code{\link{calc_analytic_css}}, \code{\link{calc_mc_css}}, 
#' and \code{\link{solve_pbtk}} as well as 
#' the number of chemicals with sufficient data to run the steady_state model 
#' (defaults to TRUE)
#'
#' @param calc_mc_css.check Whether to check the Monte Carlo sample. A 
#' comparison of the output of \code{\link{calc_mc_css}} to the SimCyp outputs 
#' reported in 
#' the Wetmore et al. (2012,2015) papers is performed. A comparison between the
#' output of \code{\link{calc_analytic_css}} (no Monte Carlo) to the median of the output of
#' \code{\link{calc_mc_css}} is also performed. (defaults to TRUE)
#'
#' @param in_vivo_stats.check Whether to compare the outputs of 
#' \code{\link{calc_mc_css}}
#' and \code{\link{calc_tkstats}} to in vivo measurements of Css, AUC, and 
#' Cmax collected
#' by Wambaugh et al. (2018). (defaults to TRUE)
#'
#' @param tissuepc.check Whether to compare the tissue-specific partition
#' coefficient predictions from the calibrated Schmitt (2008) model to the 
#' in vivo data-derived estimates compiled by Pearce et al. (2017). (defaults 
#' to TRUE)
#'
#' @param suppress.messages Whether or not output messages are suppressed
#' (defaults to TRUE)
#'
#' @param make.plots Whether current benchmarks should be plotted with 
#' historical performance (defaults to TRUE)
#'
#' @return named list, whose elements depend on the selected checks
#' \tabular{ll}{
#'   basic \tab A list with four metrics:
#'  N.steadystate -- Number of chemicals with sufficient data for steady-state IVIVE
#'     calc_analytic.units -- Ratio of mg/L to uM * 1000 / molecular weight -- should be 1
#'     calc_mc.units -- Ratio should be 1
#'     solve_pbtk.units -- Ratio should be 1 \cr
#'
#'   calc_mc_css \tab A list with four metrics:
#' RMSLE.Wetmore -- Root mean squared log10 error (RMSLE) in predicted Css between literature values (SimCyp, Wetmore et al. 2012,2015) and \code{\link{calc_mc_css}}
#' N.Wetmore -- Number of chemicals in Wetmore comparison
#' RMSLE.noMC -- RMSLE between \code{\link{calc_analytic_css}} and \code{\link{calc_mc_css}}
#'     N.noMC -- Number of chemicals in noMC comparison \cr
#'
#'   in_vivo_stats \tab A list with two metrics:
#' RMSLE.InVivoCss -- RMSLE between the predictions of \code{\link{calc_analytic_css}} and in vivo estimates of Css
#' N.InVivoCss -- Number of chemicals in comparison \cr                    
#'   
#'   units.plot \tab A ggplot2 figure showing units tests of various functions. 
#' Output is generated for mg/L and uM, and then the ratio mg/L/uM*1000/MW is
#' calculated. If the units are correct the ratio should be 1 (within the 
#' precision of the functions -- usually four significant figures). \cr
#'
#'   rmsle.plot \tab A ggplot2 figure showing RMSLE tests of various functions. 
#' Output generated is the root mean square log10 error for parameters estimated
#' by the package. \cr
#'
#'   count.plot \tab A ggplot2 figure showing count of chemicals of various functions. 
#' Output generated is a count of the chemicals available for the each of the
#' parameters estimated by and used for benchmarking the package. \cr
#' }
#' 
#' @author John Wambaugh
#'
#' @references 
#' \insertRef{DavidsonFritzUnpublishedModelAdding}{httk}
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @export benchmark_httk
benchmark_httk <- function(
                           basic.check=TRUE,
                           calc_mc_css.check=TRUE,
                           in_vivo_stats.check=TRUE,
                           tissuepc.check=TRUE,
                           suppress.messages=TRUE,
                           make.plots=TRUE)
{
  ## Setting up binding for Global Variables ##
  Compound <- Source <- Reference <- fu <- Exp_PC <- Tissue <- Species <- 
    CAS <- logMA <- Benchmark <- Version <- Value <- NULL
  ####
  benchmarks <- list()

  if (basic.check)
  {
    MW.BPA <- httk::chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="80-05-7","MW"] 
    set.seed(1234)
    if (!("method" %in% formalArgs(calc_mc_css)) |
        !is.null(formals(calc_mc_css)[["method"]]))
    {
      mc1 <- try(calc_mc_css(chem.cas="80-05-7",
                         output.units="mg/L",
                         suppress.messages=suppress.messages)[1])
      set.seed(1234)
      mc2 <- try(calc_mc_css(chem.cas="80-05-7",
                         output.units="uM",
                         suppress.messages=suppress.messages)[1])
    # v1.5 did not have a default method for resampling in httk-pop
    } else {
      mc1 <- try(calc_mc_css(chem.cas="80-05-7",
                         output.units="mg/L",
                         httkpop.generate.arg.list = list(
                           method="dr" 
                         ),
                         suppress.messages=suppress.messages)[1])
      set.seed(1234)
      mc2 <- try(calc_mc_css(chem.cas="80-05-7",
                         output.units="uM",
                         httkpop.generate.arg.list = list(
                           method="dr" 
                         ),
                         suppress.messages=suppress.messages)[1])
    }
    benchmarks[["basic"]] <- list(
      N.steadystate = length(get_cheminfo()),
      calc_analytic.units = signif(
        calc_analytic_css(chem.cas="80-05-7", 
                          output.units="mg/L",
                          suppress.messages=suppress.messages) /
        calc_analytic_css(chem.cas="80-05-7", 
                          output.units="uM",
                          suppress.messages=suppress.messages) *
        1000/MW.BPA, 4),
      calc_mc.units = signif(signif(mc1/mc2*1000,4) / signif(MW.BPA,4),4),
      solve_pbtk.units = signif(
        solve_pbtk(chem.cas="80-05-7", 
                   output.units="mg/l",
                   suppress.messages=suppress.messages)[500,"Cplasma"] /
        solve_pbtk(chem.cas="80-05-7",
                   output.units="um",
                   suppress.messages=suppress.messages)[500,"Cplasma"] *
        1000/MW.BPA, 4)
      )
  }
  
  if (calc_mc_css.check)
  {
# Get the chemicals with SimCYP numbers:
    if (exists("get_lit_cheminfo")) wetmore.chems <- get_lit_cheminfo()
    else wetmore.chems <- get_wetmore_cheminfo()
# Get list of chemicals where 3comp steadystate model works:  
  if ("suppress.messages" %in% formalArgs(get_cheminfo)) valid.chems <- 
    get_cheminfo(suppress.messages=suppress.messages)
  else valid.chems <- get_cheminfo()
# Build a table by looping over chemicals with Wetmore-reported SimCYP values:  
  css.table <- NULL
  for (this.cas in wetmore.chems)
    if (this.cas %in% valid.chems)
    {
        ids <- httk::get_chem_id(chem.cas=this.cas)
        if (!("dtxsid" %in% names(ids)))
        {
          ids$dtxsid <- NA 
        }
        this.row <- data.frame(Compound=ids$chem.name,
                                  DTXSID=ids$dtxsid,
                                  CAS=this.cas)
# Analytical Css, no Monte Carlo
        css.analytic <- try(as.data.frame(
                            as.numeric(calc_analytic_css(
                              chem.cas = this.cas,
                              model = "3compartmentss",
                              output.units = "uM",
                              suppress.messages=suppress.messages))))
        if (is(css.analytic, "try-error")) css.analytic <- NA
        this.row <- cbind(this.row, css.analytic)
# SimCYP Css from Wetmore papers
        if (exists("get_lit_css")) lit.func <- "get_lit_css"
        else lit.func <- "get_wetmore_css"
        css.lit <- try(as.data.frame(
                        do.call(lit.func, args=list(
                          chem.cas = this.cas,
                          which.quantile = 0.50,
                          output.units = "uM",
                          suppress.messages=suppress.messages))))
        if (is(css.lit, "try-error")) css.lit <- NA
        this.row <- cbind(this.row, css.lit)
# Median Css from Monte Carlo                                                      
        set.seed(2347030)
        if (!("method" %in% formalArgs(calc_mc_css)) |
            !is.null(formals(calc_mc_css)[["method"]]))
        {
          # v1.4 and earlier did not have argument "model"
          if ("model" %in% formalArgs(calc_mc_css))
          {
            css.mc <- try(as.data.frame(
                        calc_mc_css(
                          chem.cas = this.cas,
                          which.quantile = 0.50,
                          model = "3compartmentss",
                          output.units = "uM",
                          suppress.messages=suppress.messages)))
          } else {
            css.mc <- try(as.data.frame(
                        calc_mc_css(
                          chem.cas = this.cas,
                          which.quantile = 0.50,
                          output.units = "uM",
                          suppress.messages=suppress.messages)))
          }
        } else {
          css.mc <- try(as.data.frame(
                      calc_mc_css(
                        chem.cas = this.cas,
                        which.quantile = 0.50,
                        model = "3compartmentss",
                        output.units = "uM",
                        httkpop.generate.arg.list = list(
                          method="dr" 
                        ),
                        suppress.messages=suppress.messages)))
        }
        if (is(css.mc, "try-error")) css.mc <- NA
# Handle weird output:
        if (!is.null(dim(css.mc))) css.mc <- median(css.mc[,1])  
        this.row <- cbind(this.row, css.mc)
# Add row to the table:  
        colnames(this.row) <-  c(
             "Compound",
             "DTXSID","CAS", 
             "Analytic", 
             "Wetmore",
             "MC")                        
        css.table <- rbind(css.table, this.row)
    }

    
    RMSLE.wetmore <- signif(mean(log10(css.table$Wetmore/css.table$MC)^2,
                            na.rm=TRUE)^(1/2),4)
    RMSLE.nomc <- signif(mean(log10(css.table$Analytic/css.table$MC)^2,
                         na.rm=TRUE)^(1/2),4)
    
    benchmarks[["calc_mc_css"]] <- list(
                      RMSLE.Wetmore = RMSLE.wetmore,
                      N.Wetmore = sum(!is.na(css.table$Wetmore/css.table$MC)),
                      RMSLE.noMC = RMSLE.nomc,
                      N.noMC = sum(!is.na(css.table$Wetmore/css.table$MC)))
  }
  
  if (in_vivo_stats.check)
  {
    # Subset 'FitData' - exclude Bensulide for <justification for exclusion>
    FitData <- subset(chem.invivo.PK.aggregate.data,
                      Compound!="Bensulide" |
                      Source=="Wambaugh et al. (2018), NHEERL/RTI")
    # Subset 'FitData' - exclude Propyzamide for <justification for exclusion>
    FitData <- subset(FitData,Compound!="Propyzamide" |
                      Source=="Wambaugh et al. (2018), NHEERL/RTI")
    if ("parameterize.args" %in% formalArgs(calc_analytic_css))
    {
      FitData$Css.pred <- sapply(FitData$CAS,
        function(x) as.numeric(try(ifelse(x %in% get_cheminfo(),
          calc_analytic_css(
            chem.cas=x,
            species="Rat",
            model="3compartmentss",
            suppress.messages=suppress.messages,
            output.units="mg/l",
            parameterize.args=list(default.to.human=TRUE)),
            NA))))    
    } else if ("default.to.human" %in% formalArgs(calc_analytic_css)) {
      FitData$Css.pred <- sapply(FitData$CAS,
        function(x) as.numeric(try(ifelse(x %in% get_cheminfo(),
          calc_analytic_css(
            chem.cas=x,
            species="Rat",
            model="3compartmentss",
            suppress.messages=suppress.messages,
            output.units="mg/l",
            default.to.human=TRUE),
            NA))))
    # No default.to.human argument in v1.1
    } else {
      FitData$Css.pred <- sapply(FitData$CAS,
        function(x) as.numeric(try(ifelse(x %in% get_cheminfo(),
          calc_analytic_css(
            parameters=parameterize_steadystate(
              chem.cas=x,
              species="Rat",
              default.to.human=TRUE),
            model="3compartmentss",
            suppress.messages=suppress.messages,
            output.units="mg/l"),
            NA))))
    }
    
    RMSLE.invivocss <- signif(mean(log10(as.numeric(FitData$Css.pred) /
                              as.numeric(FitData$Css))^2,
                         na.rm=TRUE)^(1/2),4) 
    
    # Subset 'FitData2' - exclude Bensulide for <justification for exclusion>
    FitData2 <- subset(chem.invivo.PK.summary.data,
                      Compound!="Bensulide" |
                      Reference %in% c("RTI 2015","NHEERL 2015"))
    # Subset 'FitData2' - exclude Propyzamide for <justification for exclusion>
    FitData2 <- subset(FitData2,Compound!="Propyzamide" |
                      Reference %in% c("RTI 2015","NHEERL 2015"))
    # v1.1 did not have an argument "dose" for calc_stats:
    if ("dose" %in% formalArgs(calc_stats)) 
    {
      out <- apply(FitData2, 1,
        function(x) ifelse(
          x[["CAS"]] %in% get_cheminfo(model="pbtk"),
          try(return(unlist(calc_stats(
            parameters=parameterize_pbtk(
              chem.cas=x[["CAS"]],
              species=x[["Species"]],
              default.to.human=TRUE
              ),
            model="pbtk",
            days=as.numeric(x[["time"]])/24,
            daily.dose=NULL,
            doses.per.day=NULL,
            dose=as.numeric(x[["dose"]]),
            suppress.messages=suppress.messages,
            output.units="mg/l",
            iv.dose=(x[["Route"]]=="iv"))))), 
          NA))
    } else {
      out <- apply(FitData2, 1,
        function(x) ifelse(
          x[["CAS"]] %in% get_cheminfo(model="pbtk"),
          try(return(unlist(calc_stats(
            parameters=parameterize_pbtk(
              chem.cas=x[["CAS"]],
              species=x[["Species"]],
              default.to.human=TRUE
              ),
            model="pbtk",
            days=as.numeric(x[["time"]])/24,
            daily.dose=as.numeric(x[["dose"]]),
            doses.per.day=1,
            suppress.messages=suppress.messages,
            output.units="mg/l",
            iv.dose=(x[["Route"]]=="iv"))))), 
          NA))
    }
    FitData2$Pred.AUC <- unlist(lapply(out,function(x) as.numeric(x["AUC"])*24))
    FitData2$Pred.Cmax <- unlist(lapply(out,function(x) x["peak"]))    
    
    RMSLE.invivoauc <- signif(mean(log10(as.numeric(FitData2$Pred.AUC) /
                          as.numeric(FitData2$AUC))^2,
                          na.rm=TRUE)^(1/2),4)
    RMSLE.invivocmax <- signif(mean(log10(as.numeric(FitData2$Pred.Cmax) /
                          as.numeric(FitData2$Cmax))^2,
                          na.rm=TRUE)^(1/2),4)                     

    benchmarks[["in_vivo_stats"]] <- list(
                                          RMSLE.InVivoCss = RMSLE.invivocss,
                                          N.InVivoCss = sum(!is.na(
                                            as.numeric(FitData$Css.pred) /
                                            as.numeric(FitData$Css))),
                                          RMSLE.InVivoAUC = RMSLE.invivoauc,
                                          N.InVivoAUC = sum(!is.na(
                                            as.numeric(FitData2$Pred.AUC) /
                                            as.numeric(FitData2$AUC))),
                                          RMSLE.InVivoCmax = RMSLE.invivocmax,
                                          N.InVivoCmax = sum(!is.na(
                                            as.numeric(FitData2$Pred.Cmax) /
                                            as.numeric(FitData2$Cmax)))
                                        )
                                                                
#    write.table(FitData[,c(1,3,5,6,29,31)], 
#                file=paste("invivovcsscheck-",
#                           packageVersion("httk"),
#                           ".txt",sep=""),
#                           row.names=FALSE,
#                           sep="\t")
  }

  #
  if (tissuepc.check)
  {
    pc.table <- NULL
    # Subset 'pc.data' - exclude chemicals for <justification for exclusion>
    pc.data <- subset(pc.data,fu != 0 & Exp_PC != 0 & Tissue %in% c("Adipose","Bone","Brain","Gut",
        "Heart","Kidney","Liver","Lung","Muscle","Skin","Spleen","Blood Cells") & 
        tolower(Species) == 'rat' & !CAS %in% c('10457-90-6','5786-21-0','17617-23-1','69-23-8','2898-12-6',
        '57562-99-9','59-99-4','2955-38-6','155-97-5','41903-57-5','58-55-9','77-32-7','59-05-2','60-54-8'))
    if ("suppress.messages" %in% formalArgs(get_cheminfo))
    {
      cas.list <- get_cheminfo(
                    model='schmitt',
                    species='rat',
                    suppress.messages=TRUE)
    } else {
      cas.list <- get_cheminfo(
                    model='schmitt',
                    species='rat')
    }
    cas.list <-  cas.list[cas.list %in% pc.data[,'CAS']]
    ma.data.list <- subset(chem.physical_and_invitro.data,!is.na(logMA))[,'CAS']
    for(this.cas in cas.list)
    {
      if ("suppress.messages" %in% formalArgs(parameterize_schmitt))
      {
        parameters <- parameterize_schmitt(
          chem.cas=this.cas,
          species='rat',
          suppress.messages=TRUE)
      } else {
        parameters <- parameterize_schmitt(
          chem.cas=this.cas,
          species='rat')
      }
      # Switched the name of this parameter aroung v1.9
      if ("Funbound.plasma.uncorrected" %in% names(parameters))
        parameters$unadjusted.Funbound.plasma <- 
          parameters$Funbound.plasma.uncorrected
      # Added this parameter in v1.6:
      if (!("unadjusted.Funbound.plasma" %in% names(parameters)))
        parameters$unadjusted.Funbound.plasma <- parameters$Funbound.plasma
        
      init.parameters <- parameters
      if (exists("calc_ionization"))
      {
        charge <- calc_ionization(
          chem.cas=this.cas,
          pH=7.4)$fraction_charged
      } else charge <- 0
      if(!this.cas %in% ma.data.list){
        init.parameters$MA <- 10^(0.999831 - 0.016578*38.7 + 0.881721 * log10(parameters$Pow))
      }
      # Versions prior to 1.6 did not have regression arg:
      if (!("regression" %in% formalArgs(predict_partitioning_schmitt)))
      {
        pcs <- predict_partitioning_schmitt(
          parameters=parameters,
          species='rat')
        init.pcs <- predict_partitioning_schmitt(
          parameters=init.parameters,
          species='rat')
      # Suppress messages added later:
      } else if (!("suppress.messages" %in% 
                   formalArgs(predict_partitioning_schmitt)))
      {
        pcs <- predict_partitioning_schmitt(
          parameters=parameters,
          species='rat',
          regression=FALSE)
        init.pcs <- predict_partitioning_schmitt(
          parameters=init.parameters,
          species='rat',
          regression=FALSE)
      } else {
        pcs <- predict_partitioning_schmitt(
          parameters=parameters,
          species='rat',
          regression=FALSE,
          suppress.messages=TRUE)
        init.pcs <- predict_partitioning_schmitt(
          parameters=init.parameters,
          species='rat',
          regression=FALSE,
          suppress.messages=TRUE)
      }
      for(this.tissue in subset(pc.data,CAS==this.cas)[,'Tissue']){
        if(this.tissue == 'Blood Cells') this.pc <- 'rbc'
        else this.pc <- this.tissue
        pc.table <- rbind(pc.table,
                      cbind(
                        as.data.frame(this.cas),
                        as.data.frame(this.tissue),
                        as.data.frame(log10(
                          init.pcs[[which(substr(names(init.pcs),
                            2,
                            nchar(names(init.pcs))-3) == 
                            tolower(this.pc))]] * 
                          init.parameters$Funbound.plasma)),
                        as.data.frame(log10(
                          pcs[[which(substr(names(pcs),
                            2,
                            nchar(names(pcs))-3) == 
                            tolower(this.pc))]] *
                          parameters$unadjusted.Funbound.plasma)),
                        as.data.frame(log10(
                          init.pcs[[which(substr(names(init.pcs),
                            2,
                            nchar(names(init.pcs))-3) == 
                            tolower(this.pc))]] *
                          init.parameters$unadjusted.Funbound.plasma)),
                        as.data.frame(log10(
                          pcs[[which(substr(names(pcs),
                            2,
                            nchar(names(pcs))-3) == tolower(this.pc))]] * 
                            parameters$Funbound.plasma)),
                        as.data.frame(log10(
                          subset(pc.data,
                            CAS==this.cas & Tissue==this.tissue)[,'Exp_PC'])),
                        as.data.frame(subset(pc.data,
                          CAS==this.cas & Tissue==this.tissue)[,'LogP']),
                        as.data.frame(charge),
                        as.data.frame(as.character(subset(pc.data,
                          CAS == this.cas)[1,'A.B.N'])),
                        as.data.frame(subset(pc.data, 
                          CAS == this.cas)[1,'fu'])))
      }
    }
    colnames(pc.table) <- c('CAS','Tissue','fup.correction','ma.correction','init.Predicted',
                            'Predicted','Experimental','logP','charge','type','fup')
    init.error <- pc.table[,'Experimental'] - pc.table[,'init.Predicted']
    fup.error <- pc.table[,'Experimental'] - pc.table[,'fup.correction']
    ma.error <- pc.table[,'Experimental'] - pc.table[,'ma.correction']
    final.error <- pc.table[,'Experimental'] - pc.table[,'Predicted']
    fup.improvement <- abs(init.error) - abs(fup.error)
    ma.improvement <- abs(init.error) - abs(ma.error)
    final.improvement <- abs(init.error) - abs(final.error)
    pc.table <- cbind(pc.table,fup.improvement,ma.improvement, final.improvement,
                      final.error,init.error,ma.error,fup.error)
    
    if (any(!is.na(final.error))) RMSLE.TissuePC <- (mean(
        final.error^2, na.rm=TRUE))^(1/2)
    else RMSLE.TissuePC <- (mean(
        init.error^2, na.rm=TRUE))^(1/2)
    
    benchmarks[["tissuepc"]] <- list(RMSLE.TissuePC = signif(RMSLE.TissuePC,4),
                                     N.TissuePC = 
                                       length(final.error[!is.na(final.error)]))
    
  }
    
  if (make.plots)
  {
    plot.table <- NULL
    for (this.check in names(benchmarks))
      for (this.benchmark in names(benchmarks[[this.check]]))
      {
        if (!(this.benchmark %in% colnames(httk.performance)))
          stop(paste0("Missing column named ", this.benchmark, 
                      " in httk.performance.")) 
        
        new.rows <- cbind(httk.performance[,c("Version",this.benchmark)])
        new.rows <- rbind(new.rows,
                          c("Current",
                          benchmarks[[this.check]][[this.benchmark]]))
        new.rows <- cbind(new.rows,this.benchmark)
        colnames(new.rows) <- c("Version","Value","Benchmark")
        plot.table <- rbind(plot.table,new.rows)
      }

    plot.table$Version <- ordered(plot.table$Version,
                                  levels=unique(plot.table$Version))
    plot.table$Value <- as.numeric(plot.table$Value)
    plot.table$Benchmark <- as.factor(plot.table$Benchmark)

    units.plot <- 
      ggplot2::ggplot(subset(plot.table, regexpr("units", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark, group=Benchmark)) + 
      geom_point() +
      geom_line() +
      ylab("Ratio mg/L / uM * 1000 / MW") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["units.plot"]] <- units.plot
    

    rmsle.plot <- 
      ggplot2::ggplot(subset(plot.table, regexpr("RMSLE", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark, group=Benchmark)) + 
      geom_point() +
      geom_line() +
      ylab("Root Mean Squared Log10 Error (RMSLE)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["rmsle.plot"]] <- rmsle.plot
    
    count.plot <- 
      ggplot2::ggplot(subset(plot.table, regexpr("N.", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark, group=Benchmark)) + 
      geom_point() +
      geom_line() +
      ylab("Chemical Count") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["count.plot"]] <- count.plot
  }

  return(benchmarks)
}

# We uncomment this code when using this function in past versions of httk:
#library(httk)
#out <- benchmark_httk(basic.check=FALSE, 
#                      calc_mc_css.check=FALSE,
#                      in_vivo_stats.check=FALSE,
#                      tissuepc.check=TRUE, 
#                      make.plots=FALSE)
