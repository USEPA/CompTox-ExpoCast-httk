% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_tkstats.R
\name{calc_stats}
\alias{calc_stats}
\title{Calculate toxicokinetic summary statistics (deprecated).}
\usage{
calc_stats(
  chem.name = NULL,
  chem.cas = NULL,
  dtxsid = NULL,
  parameters = NULL,
  route = "oral",
  stats = c("AUC", "peak", "mean"),
  species = "Human",
  days = 28,
  daily.dose = 1,
  dose = NULL,
  doses.per.day = 1,
  output.units = "uM",
  concentration = "plasma",
  tissue = "plasma",
  model = "pbtk",
  default.to.human = FALSE,
  adjusted.Funbound.plasma = TRUE,
  regression = TRUE,
  restrictive.clearance = TRUE,
  suppress.messages = FALSE,
  ...
)
}
\arguments{
\item{chem.name}{Name of desired chemical.}

\item{chem.cas}{CAS number of desired chemical.}

\item{dtxsid}{EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})
the chemical must be identified by either CAS, name, or DTXSIDs}

\item{parameters}{Chemical parameters from parameterize_pbtk function,
overrides chem.name and chem.cas.}

\item{route}{String specification of route of exposure for simulation:
"oral", "iv", "inhalation", ...}

\item{stats}{Desired values (either 'AUC', 'mean', 'peak', or a vector
containing any combination).}

\item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
default "Human").}

\item{days}{Length of the simulation.}

\item{daily.dose}{Total daily dose, mg/kg BW.}

\item{dose}{Amount of a single dose at time zero, mg/kg BW.}

\item{doses.per.day}{Number of doses per day.}

\item{output.units}{Desired units (either "mg/L", "mg", "umol", or default
"uM").}

\item{concentration}{Desired concentration type, 'blood' or default
'plasma'.}

\item{tissue}{Desired steady state tissue conentration.}

\item{model}{Model used in calculation, 'pbtk' for the multiple compartment
model,'3compartment' for the three compartment model, '3compartmentss' for
the three compartment steady state model, and '1compartment' for one
compartment model.}

\item{default.to.human}{Substitutes missing animal values with human values
if true (hepatic intrinsic clearance or fraction of unbound plasma).}

\item{adjusted.Funbound.plasma}{Uses adjusted Funbound.plasma when set to
TRUE along with partition coefficients calculated with this value.}

\item{regression}{Whether or not to use the regressions in calculating
partition coefficients.}

\item{restrictive.clearance}{Protein binding not taken into account (set to
1) in liver clearance if FALSE.}

\item{suppress.messages}{Whether to suppress output message.}

\item{...}{Arguments passed to solve function.}
}
\value{
\item{AUC}{Area under the plasma concentration curve.}
\item{mean.conc}{The area under the curve divided by the number of days.}
\item{peak.conc}{The highest concentration.}
}
\description{
#' This function is included for backward compatibility. It calls
\code{\link{calc_tkstats}} which 
calculates the area under the curve, the mean, and the peak values
for the venous blood or plasma concentration of a specified chemical or all
chemicals if none is specified for the multiple compartment model with a
given number of days, dose, and number of doses per day.
}
\details{
Default value of 0 for doses.per.day solves for a single dose.

When species is specified as rabbit, dog, or mouse, the function uses the
appropriate physiological data(volumes and flows) but substitues human
fraction unbound, partition coefficients, and intrinsic hepatic clearance.
}
\author{
Robert Pearce and John Wambaugh
}
\keyword{Solve}
\keyword{Statistics}
