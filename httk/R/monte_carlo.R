#' Monte Carlo for pharmacokinetic models
#' 
#' This function performs Monte Carlo to assess uncertainty and variability for
#' toxicokinetic models. 
#' 
#' @param parameters These parameters that are also listed in either
#' cv.params or censored.params are sampled using Monte Carlo.
#' @param cv.params The parameters listed in cv.params are sampled from a
#' normal distribution that is truncated at zero. This argument should be a
#' list of coefficients of variation (cv) for the normal distribution. Each
#' entry in the list is named for a parameter in "parameters". New values are
#' sampled with mean equal to the value in "parameters" and standard deviation
#' equal to the mean times the cv.
#' @param censored.params The parameters listed in censored.params are sampled
#' from a normal distribution that is censored for values less than the limit
#' of detection (specified separately for each parameter). This argument should
#' be a list of sub-lists. Each sublist is named for a parameter in "params"
#' and contains two elements: "cv" (coefficient of variation) and "LOD" (limit
#' of detection), below which parameter values are censored. New values are
#' sampled with mean equal to the value in "params" and standard deviation
#' equal to the mean times the cv. Censored values are sampled on a uniform
#' distribution between 0 and the limit of detection.
#' @param samples This argument is the number of samples to be generated for
#' calculating quantiles.
#'
#' @author John Wambaugh
#'
#' @references 
#' Wambaugh, John F., et al. "Toxicokinetic triage for 
#' environmental chemicals." Toxicological Sciences 147.1 (2015): 55-67.
#'
#' Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' @keywords Monte-Carlo
#'
#' @examples
#' 
#' #Example from Pearce et al. (2017):
#' \dontrun{
#' library(ggplot2)
#' library(scales)
#' vary.params <- NULL
#' parameters <- parameterize_pbtk(chem.name = "Zoxamide")
#' for(this.param in names(subset(parameters,
#' names(parameters) != "Funbound.plasma"))) vary.parameters[this.param] <- .2
#' censored.params <- list(Funbound.plasma = list(cv = 0.2, lod = 0.01))
#' set.seed(1)
#' out <- monte_carlo(parameters, cv.params = vary.params,
#' censored.params = censored.params, model = "pbtk", suppress.messages = T)
#' zoxamide <- ggplot(as.data.frame(out), aes(out)) +
#' geom_histogram(fill="blue", binwidth=1/6) + scale_x_log10() +
#' ylab("Number of Samples") + xlab("Steady State Concentration (uM)") +
#' theme(axis.text = element_text(size = 16),
#' axis.title = element_text(size = 16))
#' print(zoxamide)
#' 
#' 
#' # Fig 1 in Wambaugh et al. (2015) SimCYP vs. our predictions:
#' 
#' vary.params <- list(BW=0.3)
#' vary.parameters[["Vliverc"]]<-0.3
#' vary.parameters[["Qgfrc"]]<-0.3
#' vary.parameters[["Qtotal.liverc"]]<-0.3
#' vary.parameters[["million.cells.per.gliver"]]<-0.3
#' vary.parameters[["Clint"]]<-0.3
#' censored.params<-list(Funbound.plasma=list(cv=0.3,lod=0.01))
#' 
#' pValues <- get_cheminfo(c("Compound","CAS","Clint.pValue"))
#' pValues.rat <- get_cheminfo(c("Compound","CAS","Clint.pValue"),species="Rat")
#' 
#' 
#'  
#' Wetmore.table <- NULL
#' for (this.CAS in get_cheminfo(model="3compartmentss")){
#'   if (this.CAS %in% get_wetmore_cheminfo()){
#'     print(this.CAS)
#'     these.params <- parameterize_steadystate(chem.cas=this.CAS)
#'     if (these.parameters[["Funbound.plasma"]] == 0.0) 
#'     {
#'       these.parameters[["Funbound.plasma"]] <- 0.005
#'     }
#'     these.parameters[["Fhep.assay.correction"]] <- 1
#'     vLiver.human.values <- monte_carlo(these.params,
#'                                        cv.params=vary.params,
#'                                        censored.params=censored.params,
#'                                        output.units="mg/L",
#'                                        model='3compartmentss',
#'                                        suppress.messages=T,
#'                                        well.stirred.correction=F,
#'                                        Funbound.plasma.correction=F)
#'     percentiles <- c("5","50","95")
#'     for (this.index in 1:3)
#'     {
#'       this.row <- as.data.frame(get_wetmore_css(chem.cas=this.CAS)) 
#'       this.row <- cbind(this.row, as.data.frame(vLiver.human.values[this.index]))
#'       this.row <- cbind(this.row, as.data.frame(percentiles[this.index]))
#'       this.row <- cbind(this.row, as.data.frame("Human"))
#'       this.row <- cbind(this.row, as.data.frame(this.CAS))
#'       this.row <- cbind(this.row, as.data.frame(pValues[pValues$CAS==this.CAS,
#'                                                 "Human.Clint.pValue"]<0.05))
#'       colnames(this.row) <- c("Wetmore", "Predicted", "Percentile", "Species",
#'                               "CAS", "Systematic")
#'       if (is.na(this.row["Systematic"])) this.row["Systematic"] <- F
#'       Wetmore.table <- Wetmore.table <- rbind(Wetmore.table,this.row)
#'     }
#'   }
#' }
#' 
#' scientific_10 <- function(x) {
#'   out <- gsub("1e", "10^", scientific_format()(x))
#'   out <- gsub("\+","",out)
#'   out <- gsub("10\^01","10",out)
#'   out <- parse(text=gsub("10\^00","1",out))
#' }
#' 
#' 
#' Fig1 <- ggplot(Wetmore.table, aes(Predicted,Wetmore,group = CAS)) +
#'   geom_line() +
#'   geom_point(aes(colour=factor(Percentile),shape=factor(Percentile))) +
#'   scale_colour_discrete(name="Percentile") +
#'   scale_shape_manual(name="Percentile", values=c("5"=21, "50"=22,"95"=24)) +
#'   scale_x_log10(expression(paste(C[ss]," Predicted (mg/L) with Refined Assumptions")),
#'                 label=scientific_10) + 
#'   scale_y_log10(expression(paste(C[ss]," Wetmore ",italic("et al.")," (2012) (mg/L)")),
#'                 label=scientific_10) +
#'   geom_abline(intercept = 0, slope = 1,linetype="dashed")+
#'   theme_bw()+
#'   theme(legend.position="bottom", text  = element_text(size=18))
#' 
#' print(Fig1)
#' 
#' Fig1a.fit <- lm(log(Wetmore) ~ log(Predicted)*Percentile, Wetmore.table)
#' ## End(**Not run**)
#' }
#' 
#' @import stats data.table
#' @export monte_carlo
monte_carlo <- function(
                 parameters,
                 cv.params=NULL,
                 censored.params=NULL,
                 samples=1000)
{

# Create a data table with the same parameters in every row:  
  MC.matrix <- as.data.table(parameters)[rep(1,samples)]

# Number of different results to be obtained for the different paramters:
  sample.vec <- rep(NA,samples)
# Any parameter given in cv.params is sampled from a normal distribution 
# truncated at zero:
  for (this.param in names(cv.params))
  {
    if (!(this.param %in% names(parameters))) 
      stop(paste("Cannot find cv.params parameter",
        this.param,
        "in parameter list."))
    if (parameters[[this.param]]>0) 
      MC.matrix[,this.param] <- 
      rtnorm(
        samples,
        mean=parameters[[this.param]],
        sd=parameters[[this.param]]*cv.params[[this.param]],
        lower=0)
    else 
    {
      MC.matrix[,this.param] <- 0  
      warning(paste(
        this.param,
        "has mean of zero, yielding SD of zero for fixed cv.\n\
Parameter value fixed at zero."))
    }
  }
  
# Any parameter given in censored parameters is sampled from a censored distribution:
  for (this.param in names(censored.params))
  {
    if (!(this.param %in% names(parameters))) 
      stop(paste("Cannot find censored.params parameter",
        this.param,"in parameter list."))
    if (!("cv" %in% names(censored.params[[this.param]]))) 
      stop(paste("cv (coefficient of variation) must be specified for parameter",
        this.param))
    if (!("lod" %in% names(censored.params[[this.param]]))) 
      stop(paste("lod (limit of detection) must be specified for parameter",
        this.param))
    if(this.param %in% c('Funbound.plasma','Fhep.assay.correction'))  upper <- 1
    else upper <- Inf
    MC.matrix[,this.param] <- r_left_censored_norm(samples,
      mean=parameters[[this.param]],
      sd=parameters[[this.param]]*censored.params[[this.param]]$cv,
      lod=censored.params[[this.param]]$lod,
      upper=upper)
  }

  return(MC.matrix)
}


