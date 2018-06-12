\name{solve_aerosol_pbtk}
\alias{solve_aerosol_pbtk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Solve_aerosol_PBTK
}
\description{
This function solves for the amounts or concentrations in uM of a chemical in different tissues as functions of time based on the dose and dosing frequency.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solve_aerosol_pbtk(chem.name = NULL, chem.cas = NULL, times=NULL, parameters=NULL,
           days=10, tsteps = 4, initial.values=NULL,plots=F,suppress.messages=F,
           species="Human", method="lsoda",rtol=1e-8,atol=1e-12,
           default.to.human=F,recalc.blood2plasma=F,recalc.clearance=F,
           dosing.matrix=NULL,adjusted.Funbound.plasma=T,regression=T, 
           use.amounts=F,restrictive.clearance = T,dae = 1, particle.density = 3,
           air.concentration=1, period = 1, exposure = .5,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{times}{Optional time sequence for specified number of days.  Dosing sequence begins at the beginning of times.}
  \item{parameters}{Chemical parameters from parameterize_pbtk function, overrides chem.name and chem.cas.}
  \item{days}{Length of the simulation.}
   \item{tsteps}{The number time steps per hour.}
  \item{initial.values}{Vector containing the initial concentrations or amounts of the chemical in specified tissues with units corresponding to air.concentration.  Defaults are zero.}
  \item{plots}{Plots all outputs if true.}
  \item{suppress.messages}{Whether or not the output message is suppressed.} 
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). }
  \item{method}{Method used by integrator (deSolve).}
  \item{rtol}{Argument passed to integrator (deSolve).}
  \item{atol}{Argument passed to integrator (deSolve).}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).
  }
  \item{recalc.blood2plasma}{Recalculates the ratio of the amount of chemical in the blood to plasma using the input parameters, calculated with hematocrit, Funbound.plasma, and Krbc2pu.} 
   \item{recalc.clearance}{Recalculates the the hepatic clearance (Clmetabolism) with new million.cells.per.gliver parameter.}
  \item{dosing.matrix}{Vector of dosing times or a matrix consisting of two columns or rows named "dose" and "time" containing the time and amount, in mg/kg BW, of each dose.}
 \item{adjusted.Funbound.plasma}{Uses adjusted Funbound.plasma when set to TRUE along with partition coefficients calculated with this value.}
  \item{regression}{Whether or not to use the regressions in calculating partition coefficients.}
    \item{use.amounts}{Return output as concentrations (units/L) or amounts (units), corresponding to air.concentration units/L.}
 \item{restrictive.clearance}{Protein binding not taken into account (set to 1) in liver clearance if FALSE.}
  \item{dae}{Aerodynamic diameter of aerosol, units of micrometers.}
  \item{particle.density}{Particle density of aerosol, g/cm^3.}
  \item{air.concentration}{Concentration of substance in air, units/L. Output units will be the same.}
  \item{period}{Length of one cycle of exposure and non-exposure, days.}
  \item{exposure}{Length of exposure to air.concentration at beginning of exposure cycle, days.}
  \item{...}{Additional arguments passed to the integrator.}
}

\details{
Note that the model parameters have units of hours while the model output is in days.

Default NULL value for doses.per.day solves for a single dose.

The compartments used in this model are the gutlumen, gut, liver, kidneys, veins, arteries, lungs, and the rest of the body.  

The extra compartments include the amounts or concentrations metabolized by the liver and excreted by the kidneys through the tubules.  

AUC is the area under the curve of the plasma concentration.

%%When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix of class deSolve with a column for time(in days), each compartment, the area under the curve, and plasma concentration and a row for each time point.
     
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
John Wambaugh and Robert Pearce
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Solve}

