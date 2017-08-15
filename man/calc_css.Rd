\name{calc_css}
\alias{calc_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the steady state concentration and the day it is reached.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function finds the day a chemical comes within the specified range of the analytical steady state venous blood or plasma concentration(from calc_analytic_css) for the multiple compartment, three compartment, and one compartment models, the fraction of the true steady state value reached on that day, the maximum concentration, and the average concentration at the end of the simulation.
}
\usage{
calc_css(parameters=NULL,chem.name=NULL,chem.cas=NULL,species="Human", f = .01,
         daily.dose=1, doses.per.day=3,days = 10,output.units = "uM",
         concentration='plasma',suppress.messages=F,model='pbtk',default.to.human=F,
         f.change=0.00001,Funbound.plasma.pc.correction=T,
         well.stirred.correction=T, restrictive.clearance=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
 \item{chem.name}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{pred} here~~
}

  \item{f}{
  Fractional distance from the final steady state concentration that the average concentration must come within to be considered at steady state.
%%     ~~Describe \code{pred} here~~
}
  \item{parameters}{Chemical parameters from parameterize_pbtk function, overrides chem.name and chem.cas.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
 \item{daily.dose}{Total daily dose, mg/kg BW.}
  \item{doses.per.day}{Number of doses per day.}
  \item{days}{Initial number of days to run simulation that is multiplied on each iteration.}
  \item{output.units}{Units for returned concentrations, defaults to uM (specify units = "uM") but can also be mg/L.}
   \item{concentration}{Desired concentration type, 'blood' or default 'plasma'.}
   \item{suppress.messages}{Whether or not to suppress messages.}
   \item{model}{Model used in calculation, 'pbtk' for the multiple compartment model,'3compartment' for the three compartment model, and '1compartment' for the one compartment model.}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).
  }
  \item{f.change}{Fractional change of daily steady state concentration reached to stop calculating.}
  \item{Funbound.plasma.pc.correction}{Uses corrected Funbound.plasma when set to TRUE along with parition coefficients calculated with this value and regressions.}
  \item{well.stirred.correction}{Uses correction in calculation of hepatic clearance for well-stirred model if TRUE for model 1compartment elimination rate.  
                                  This assumes clearance relative to amount unbound in whole blood instead of plasma, but converted to use with plasma concentration.}
 \item{restrictive.clearance}{Protein binding not taken into account (set to 1) in liver clearance if FALSE.}
  \item{...}{Additional arguments passed to model solver (default of solve_pbtk).}
  


}

\value{
\item{frac}{Fraction of the true steady state concentration reached on the day steady state is reached.}
\item{max}{The maximum concentration of the simulation.}
\item{avg}{The average concentration on the final day of the simulation.}
\item{the.day}{The day the average concentration comes within 100 * p percent of the true steady state concentration.}
}

\author{
Robert Pearce
}

\examples{
calc_css(chem.name='Bisphenol-A',doses.per.day=5,f=.001,output.units='mg/L')
\dontrun{
parms <- parameterize_3comp(chem.name='Bisphenol-A')
parms$Funbound.plasma <- .07
calc_css(parms,concentration='blood',model='3compartment')


library("ggplot2")
out <- solve_pbtk(chem.name = "Bisphenol A", days = 50, doses.per.day = 3)
plot.data <- as.data.frame(out)
css <- calc_analytic_css(chem.name = "Bisphenol A")
c.vs.t <- ggplot(plot.data,aes(time, Cplasma)) + geom_line() +
geom_hline(yintercept = css) + ylab("Plasma Concentration (uM)") +
xlab("Day") + theme(axis.text = element_text(size = 16), axis.title =
element_text(size = 16), plot.title = element_text(size = 17)) +
ggtitle("Bisphenol A")
print(c.vs.t)

days <- NULL
avg <- NULL
max <- NULL
for(this.cas in get_cheminfo()){
css.info <- calc_css(chem.cas = this.cas, doses.per.day = 1,suppress.messages=T)
days[[this.cas]] <- css.info[["the.day"]]
avg[[this.cas]] <- css.info[["avg"]]
max[[this.cas]] <- css.info[["max"]]
}
days.data <- as.data.frame(days)
hist <- ggplot(days.data, aes(days)) +
geom_histogram(fill = "blue", binwidth = 1/6) + scale_x_log10() +
ylab("Number of Chemicals") + xlab("Days") + theme(axis.text =
element_text(size = 16), axis.title = element_text(size = 16))
print(hist)
avg.max.data <- as.data.frame(cbind(avg, max))
avg.vs.max <- ggplot(avg.max.data, aes(avg, max)) + geom_point() +
geom_abline() + scale_x_log10() + scale_y_log10() +
xlab("Average Concentration at Steady State (uM)") +
ylab("Max Concentration at Steady State (uM)") +
theme(axis.text = element_text(size = 16),
axis.title = element_text(size = 16))
print(avg.vs.max)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Steady State}