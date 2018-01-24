\name{monte_carlo}
\alias{monte_carlo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Monte Carlo for pharmacokinetic models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function performs Monte Carlo to assess uncertainty and variability for toxicokinetic models.
}
\usage{
monte_carlo(params,which.quantile=0.95,cv.params=NULL,censored.params=NULL,samples=1000,
            name.model='calc_analytic_css',output.col.model=NA,return.samples=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
All parameters needed by the function indicated by the argument "name.model". These 
paramters that are also listed in either cv.params or censored.params are 
sampled using Monte Carlo.
}
  \item{which.quantile}{
This argument specifies which quantiles are to be calculated. It can be a vector
or a single value. It defaults to the 0.95 quantile (95\%).
}
  \item{cv.params}{
The parameters listed in cv.params are sampled from a normal distribution that is
truncated at zero. This argument should be a list of coefficients of variation 
(cv) for the normal distribution. Each entry in the list is named for a 
parameter in "params". New values are sampled with mean equal to the value in
"params" and standard deviation equal to the mean times the cv.

}
  \item{censored.params}{
The parameters listed in censored.params are sampled from a normal distribution 
that is censored for values less than the limit of detection (specified separately
for each paramter). This argument should be a list of sub-lists. Each sublist 
is named for a parameter in "params"
and contains two elements: "cv" (coefficient of variation) and "LOD" (limit of
detection), below which parameter values are censored. New values are sampled 
with mean equal to the value in "params" and standard deviation equal to the 
mean times the cv.Censored values are sampled on a uniform distribution between
0 and the limit of detection.
}
  \item{samples}{
  This argument is the number of samples to be generated for calculating quantiles.
}
  \item{name.model}{
  This argument is a character vector giving the name of the model to be sampled. 
  Defaults to 'calc_analytic_css'.
}
  \item{output.col.model}{
  If the evaluation of the function indicated by "model" returns a list, then
  model.output.col is the element from that list that is sampled and is used for
  calculating quantiles. Defaults to NA (i.e., the function returns a single
  value).
}
  \item{return.samples}{Whether or not to return the vector containing the samples from the simulation instead of the selected quantile.}
  \item{...}{Additional arguments passed to name.model.}
}



\author{
John Wambaugh
}

\examples{
#Example from httk jss paper:
\dontrun{
library(ggplot2)
library(scales)
vary.params <- NULL
params <- parameterize_pbtk(chem.name = "Zoxamide")
for(this.param in names(subset(params,
names(params) != "Funbound.plasma"))) vary.params[this.param] <- .2
censored.params <- list(Funbound.plasma = list(cv = 0.2, lod = 0.01))
set.seed(1)
out <- monte_carlo(params, cv.params = vary.params,
censored.params = censored.params, return.samples = T,
model = "pbtk", suppress.messages = T)
zoxamide <- ggplot(as.data.frame(out), aes(out)) +
geom_histogram(fill="blue", binwidth=1/6) + scale_x_log10() +
ylab("Number of Samples") + xlab("Steady State Concentration (uM)") +
theme(axis.text = element_text(size = 16),
axis.title = element_text(size = 16))
print(zoxamide)


# Fig 1 in Wambaugh et al. (2015) SimCYP vs. our predictions:

vary.params <- list(BW=0.3)
vary.params[["Vliverc"]]<-0.3
vary.params[["Qgfrc"]]<-0.3
vary.params[["Qtotal.liverc"]]<-0.3
vary.params[["million.cells.per.gliver"]]<-0.3
vary.params[["Clint"]]<-0.3
censored.params<-list(Funbound.plasma=list(cv=0.3,lod=0.01))

pValues <- get_cheminfo(c("Compound","CAS","Clint.pValue"))
pValues.rat <- get_cheminfo(c("Compound","CAS","Clint.pValue"),species="Rat")


 
Wetmore.table <- NULL
for (this.CAS in get_cheminfo(model="3compartmentss")){
  if (this.CAS \%in\% get_wetmore_cheminfo()){
    print(this.CAS)
    these.params <- parameterize_steadystate(chem.cas=this.CAS)
    if (these.params[["Funbound.plasma"]] == 0.0) 
    {
      these.params[["Funbound.plasma"]] <- 0.005
    }
    these.params[["Fhep.assay.correction"]] <- 1
    vLiver.human.values <- monte_carlo(these.params,
                                       cv.params=vary.params,
                                       censored.params=censored.params,
                                       which.quantile=c(0.05,0.5,0.95),
                                       output.units="mg/L",
                                       model='3compartmentss',
                                       suppress.messages=T,
                                       well.stirred.correction=F,
                                       Funbound.plasma.correction=F)
    percentiles <- c("5","50","95")
    for (this.index in 1:3)
    {
      this.row <- as.data.frame(get_wetmore_css(chem.cas=this.CAS,
                                which.quantile=as.numeric(percentiles[this.index])/100))
      this.row <- cbind(this.row, as.data.frame(vLiver.human.values[this.index]))
      this.row <- cbind(this.row, as.data.frame(percentiles[this.index]))
      this.row <- cbind(this.row, as.data.frame("Human"))
      this.row <- cbind(this.row, as.data.frame(this.CAS))
      this.row <- cbind(this.row, as.data.frame(pValues[pValues$CAS==this.CAS,
                                                "Human.Clint.pValue"]<0.05))
      colnames(this.row) <- c("Wetmore", "Predicted", "Percentile", "Species",
                              "CAS", "Systematic")
      if (is.na(this.row["Systematic"])) this.row["Systematic"] <- F
      Wetmore.table <- Wetmore.table <- rbind(Wetmore.table,this.row)
    }
  }
}

scientific_10 <- function(x) {
  out <- gsub("1e", "10^", scientific_format()(x))
  out <- gsub("\\+","",out)
  out <- gsub("10\\^01","10",out)
  out <- parse(text=gsub("10\\^00","1",out))
}


Fig1 <- ggplot(Wetmore.table, aes(Predicted,Wetmore,group = CAS)) +
  geom_line() +
  geom_point(aes(colour=factor(Percentile),shape=factor(Percentile))) +
  scale_colour_discrete(name="Percentile") +
  scale_shape_manual(name="Percentile", values=c("5"=21, "50"=22,"95"=24)) +
  scale_x_log10(expression(paste(C[ss]," Predicted (mg/L) with Refined Assumptions")),
                label=scientific_10) + 
  scale_y_log10(expression(paste(C[ss]," Wetmore ",italic("et al.")," (2012) (mg/L)")),
                label=scientific_10) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed")+
  theme_bw()+
  theme(legend.position="bottom", text  = element_text(size=18))

print(Fig1)

Fig1a.fit <- lm(log(Wetmore) ~ log(Predicted)*Percentile, Wetmore.table)
## End(**Not run**)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Monte Carlo }
