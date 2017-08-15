\name{calc_stats}
\alias{calc_stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the statistics.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the area under the curve, the mean, and the peak values for the venous blood or plasma concentration of a specified chemical or all chemicals if none is specified for the multiple compartment model with a given number of days, dose, and number of doses per day.
}
\usage{
calc_stats(days,chem.name=NULL,chem.cas=NULL,parameters=NULL,stats=c("AUC","peak","mean"),
           species='Human',exclude.fub.zero=F,daily.dose=1,dose=NULL,doses.per.day=NULL,
           output.units='uM',concentration='plasma',model='pbtk',
           default.to.human=F,suppress.messages=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{days}{Length of the simulation.}
 \item{chem.name}{Name of desired chemical.}
  \item{chem.cas}{CAS number of desired chemical.}  
  \item{parameters}{Chemical parameters from parameterize_pbtk function, overrides chem.name and chem.cas.}  
  \item{stats}{Desired values (either 'AUC', 'mean', 'peak', or a vector containing any combination).}
  \item{daily.dose}{Total daily dose, mg/kg BW.}
  \item{dose}{Amount of a single dose, mg/kg BW.  Overwrites daily.dose.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
  \item{exclude.fub.zero}{Whether or not to exclude chemicals with a fraction of unbound plasma equal to zero or include them with a value of 0.005, only used when chem.name, chem.cas, and parameters are not specified.}
  \item{doses.per.day}{Number of doses per day.}
  \item{output.units}{Desired units (either "mg/L", "mg", "umol", or default "uM").}
  \item{model}{Model used in calculation, 'pbtk' for the multiple compartment model,'3compartment' for the three compartment model, '3compartmentss' for the three compartment steady state model, and '1compartment' for one compartment model.}
   \item{concentration}{Desired concentration type, 'blood' or default 'plasma'.}
   \item{default.to.human}{Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).}
   \item{suppress.messages}{Whether to suppress output message.}
  \item{...}{Arguments passed to solve function.}
}

\details{
Default value of 0 for doses.per.day solves for a single dose.

%%When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{AUC}{Area under the plasma concentration curve.}
\item{mean}{The area under the curve divided by the number of days.}
\item{peak}{The highest concentration.}
}

\author{
John Wambaugh and Robert Pearce
}

\examples{
calc_stats(chem.name='Bisphenol-A',days=100,stats='mean',model='3compartment')
calc_stats(chem.name='Bisphenol-A',days=100,stats=c('peak','mean'),species='Rat')
\dontrun{
all.peak.stats <- calc_stats(days=10, doses.per.day = 3, stats = "peak")
}
triclosan.stats <- calc_stats(days=10, chem.name = "triclosan")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Solve}
\keyword{Statistics}% __ONLY ONE__ keyword per line
