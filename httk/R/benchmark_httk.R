#load("NewInVivoTablesForHTTK.RData")

#' Assess the curent performance of httk relative to historical benchmarks
#' 
#' The function performs a series of "sanity checks" and predictive performance
#' benchmarks so that the impace of changes to the data, models, and 
#' implementation of the R package can be tested. Plots can be generated showing 
#' how the performance of he current version compares with past releases of
#' httk.
#' 
#' Historically some refinements made to one aspect of httk have unitentiionally
#' impacted other aspects. Most notably errors have ocasionally been introduced 
#' with respect to units (v1.9, v2.1.0). This benchmarking tool is intended to
#' reduce the chance of these errors occuring in the future.
#' 
#' Past performance was retroactively evaluated by manually installing previous 
#' versions of 
#' the package from https://cran.r-project.org/src/contrib/Archive/httk/ and
#' then adding the code for \code{benchmark_httk} at the command line
#' interface. The results are stored in the data.frame 
#' \code{\link{httk.performance}.
#'
#' The in vivo statistics are currently based on comparisons to the in vivo
#' data compiled by Wambaugh et al. (2018).
#'
#' @param basic.check Whether to run the basic checks, including units uM and 
#' mg/L units for calc_analytic_css, calc_mc_css, and solve_pbtk as well as 
#' the number of chemicals with sufficient data to run the steady_stat emodel 
#' (defaults to TRUE)
#'
#' @param calc_mc_css.check A character string of three numbers separated by two 
#' dashes (defaults to TRUE)
#'
#' @param in_vivo_stats.check A character string of three numbers separated by two 
#' dashes (defaults to TRUE)
#'
#' @param suppress.messages Whether or not output messages are suppressed
#' (defaults to TRUE)
#'
#' @param make.plots Whether current benchmarks should be plotted with 
#' historical performance (defaults to TRUE)
#'
#' @return named list, whose elements depend on the selected checks
#' \tabular{rl}{
#'   basic \tab A list with four metrics:
#'   \itemize{
#'     \item{N.steadystate}{Number of chemicals with sufficient data for steady-state IVIVE}
#'     \item{calc_analytic.units}{Ratio of mg/L to uM * 1000 / molecular weight -- should be 1}
#'     \item{calc_mc.units}{Ratio should be 1}
#'     \item{solve_pbtk.units}{Ratio should be 1}
#'   } \cr
#'
#'   calc_mc_css \tab A list with four metrics:
#'   \itemize{
#'     \item{RMSLE.Wetmore}{Root mean squared log10 error (RMSLE) in predicted Css between literature valuse (SimCyp, Wetmore et al. 2012,2015) and calc_mc_css}
#'     \item{N.Wetmore}{Number of chemicals in Wetmore comparison}
#'     \item{RMSLE.noMC}{RMSLE between calc_analytic_css and calc_mc_css}
#'     \item{N.noMC}{Number of chemicals in noMC comparison}
#'   } \cr
#'
#'   in_vivo_stats \tab A list with two metrics:
#'   \itemize{
#'     \item{RMSLE.InVivoCss}{RMSLE between the predictions of calc_analytic_css and in vivo estimates of Css}
#'     \item{N.InVivoCss}{Number of chemcials in comparison}
#'   } \cr                    
#'
#'   units.plot \tab A ggplot2 figure showing units tests of various functions. 
#' Output is generated for mg/L and uM, and then the ratio mg/L/uM*1000/MW is
#' calculated. If the units are correct the ratio should be 1 (within the 
#' precision of the functions -- usually four significant figures). \cr
#'
#'   units.plot \tab A ggplot2 figure showing units tests of various functions. 
#' Output is generated for mg/L and uM, and then the ratio mg/L/uM*1000/MW is
#' calculated. If the units are correct the ratio should be 1 (within the 
#' precision of the functions -- usually four significant figures). \cr
#'
#'   units.plot \tab A ggplot2 figure showing units tests of various functions. 
#' Output is generated for mg/L and uM, and then the ratio mg/L/uM*1000/MW is
#' calculated. If the units are correct the ratio should be 1 (within the 
#' precision of the functions -- usually four significant figures). \cr
#'
#' }
#' 
#' @author John Wambaugh
#'
#' @references
#' Wambaugh et al. "Developing Generic Toxicokinetic Models with R Package 
#' "httk" for Enhanced Reporting Accuracy and Statistical Evaluation",
#' in preparation
#'
#' @import ggplot2
#'
#' @export benchmark_httk
benchmark_httk <- function(
                           basic.check=TRUE,
                           calc_mc_css.check=TRUE,
                           in_vivo_stats.check=TRUE,
                           suppress.messages=TRUE,
                           make.plots=TRUE)
{
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
                         method="dr",
                         suppress.messages=suppress.messages)[1])
      set.seed(1234)
      mc2 <- try(calc_mc_css(chem.cas="80-05-7",
                         output.units="uM",
                         method="dr",
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
        ids <- httk:::get_chem_id(chem.cas=this.cas)
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
                        method="dr",
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
    FitData <- subset(chem.invivo.PK.aggregate.data,
                      Compound!="Bensulide" |
                      Source=="Wambaugh et al. (2018), NHEERL/RTI")
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
    
    FitData2 <- subset(chem.invivo.PK.summary.data,
                      Compound!="Bensulide" |
                      Reference %in% c("RTI 2015","NHEERL 2015"))
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
      ggplot(subset(plot.table, regexpr("units", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark)) + 
      geom_point() +
      geom_path(group=1) +
      ylab("Ratio mg/L / uM * 1000 / MW") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["units.plot"]] <- units.plot
    
    rmsle.plot <- 
      ggplot(subset(plot.table, regexpr("RMSLE", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark)) + 
      geom_point() +
      geom_path(group=1) +
      ylab("Root Mean Squared Log10 Error (RMSLE)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["rmsle.plot"]] <- rmsle.plot
    
    count.plot <- 
      ggplot(subset(plot.table, regexpr("N.", Benchmark)!=-1),
             aes(x=Version, y=Value, color=Benchmark)) + 
      geom_point() +
      geom_path(group=1) +
      ylab("Chemical Count") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    benchmarks[["count.plot"]] <- count.plot
  }

  return(benchmarks)
}

#library(httk)
#out <- benchmark_httk(basic.check=TRUE, calc_mc_css.check=TRUE, make.plots=FALSE)


#out <- benchmark_httk(basic.check=FALSE, calc_mc_css.check=FALSE, make.plots=FALSE)
