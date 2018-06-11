\name{calc_mc_oral_equiv}
\alias{calc_mc_oral_equiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Monte Carlo Oral Equivalent Dose
}
\description{
This functions converts a chemical plasma concetration to an oral equivalent dose using a concentration obtained from calc_mc_css. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_mc_oral_equiv(conc,chem.name=NULL,chem.cas=NULL,which.quantile=0.95,
                   species="Human",input.units='uM',output.units='mg',
                   suppress.messages=F,return.samples=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conc}{
  Bioactive in vitro concentration in units of uM.
%%     ~~Describe \code{ssparams.i} here~~
}
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{suppress.messages}{
  Suppress text messages.
%%     ~~Describe \code{sds} here~~
}
  \item{input.units}{Units of given concentration, default of uM but can also be mg/L.}
  \item{output.units}{Units of dose, default of 'mg' for mg/kg BW/ day or 'mol' for mol/ kg BW/ day.}
  \item{which.quantile}{
  Which quantile from Monte Carlo steady-state simulation (calc_mc_css) is requested. Can be a vector. Note that 95th concentration quantile is the same population as the 5th dose quantile.
%%     ~~Describe \code{ssparams.mean} here~~
}
 \item{species}{
   Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
%%     ~~Describe \code{ssparams.var.inv} here~~
}
   \item{return.samples}{Whether or not to return the vector containing the samples from the simulation instead of the selected quantile.}
  \item{...}{Additional parameters passed to calc_mc_css for httkpop and variance of parameters.}
}

\value{

Equivalent dose in specified units, default of mg/kg BW/day.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
John Wambaugh
}

\examples{

\dontrun{
calc_mc_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95),
                   method='vi',samples=100,tissue='brain')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Steady State}
\keyword{Monte Carlo}% __ONLY ONE__ keyword per line
