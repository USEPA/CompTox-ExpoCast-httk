\name{calc_vdist}
\alias{calc_vdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the volume of distribution for a one compartment model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 This function predicts partition coefficients for all tissues, then lumps them into a single compartment.
}
\usage{
calc_vdist(chem.cas=NULL, chem.name=NULL, parameters=NULL,
           default.to.human=F,species="Human", suppress.messages=F,
           Funbound.plasma.pc.correction=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified when Funbound.plasma is not given in parameter list.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number or the chemical name must be specified when Funbound.plasma is not given in parameter list.
%%     ~~Describe \code{pred} here~~
}
  \item{parameters}{Parameters from parameterize_3comp, parameterize_pbtk or predict_partitioning_schmitt.}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true.
  }
  \item{species}{
Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
\item{suppress.messages}{Whether or not the output message is suppressed.}
  \item{Funbound.plasma.pc.correction}{Uses corrected Funbound.plasma and corrected partition coefficients (using regressions) 
  in calculation when set to TRUE and Funbound.plasma or partition coefficients are not included in the list parameters.}
}
\details{
 The effective volume of distribution is calculated by summing each tissues volume times it's partition coefficient relative to plasma. Plasma, and the paritioning into RBCs are also added to get the total volume of distribution in L/KG BW. Partition coefficients are calculated using Schmitt's (2008) method. 
%%When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Volume of distribution}{Units of L/ kg BW.} 
}
\references{
Schmitt W. "General approach for the calculation of tissue to plasma partition coefficients." Toxicology In Vitro, 22, 457-467 (2008).
Peyret, T., Poulin, P., Krishnan, K., "A unified algorithm for predicting partition coefficients for PBPK modeling of drugs and environmental chemicals." Toxicology and Applied Pharmacology, 249, 197-207 (2010).
}
\author{
John Wambaugh
}


\examples{
calc_vdist(chem.cas="80-05-7")
calc_vdist(chem.name="Bisphenol A")
calc_vdist(chem.name="Bisphenol A",species="Rat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Parameter}
