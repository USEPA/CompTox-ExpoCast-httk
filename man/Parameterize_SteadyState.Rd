\name{parameterize_steadystate}
\alias{parameterize_steadystate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Parameterize_SteadyState
}
\description{
This function initializes the parameters needed in the functions calc_mc_css, calc_mc_oral_equiv, and calc_analytic_css for the three compartment steady state model ('3compartmentss'). 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
 parameterize_steadystate(chem.cas=NULL,chem.name=NULL,species="Human",
                          clint.pvalue.threshold=0.05,default.to.human=F,
                          human.clint.fub=F,Funbound.plasma.correction=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{Either the chemical name or the CAS number must be specified.}
  \item{chem.cas}{Either the chemical name or the CAS number must be specified.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
  \item{clint.pvalue.threshold}{Hepatic clearances with clearance assays having p-values greater than the threshold are set to zero.}
   \item{default.to.human}{
  Substitutes missing rat values with human values if true.
  }
   \item{human.clint.fub}{
  Uses human hepatic intrinsic clearance and fraction of unbound plasma in calculation of partition coefficients for rats if true.
 }
  \item{Funbound.plasma.correction}{Returns corrected Funbound.plasma when set to TRUE.}
}


\value{
   \item{Clint}{Hepatic Intrinsic Clearance, uL/min/10^6 cells.}
   \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the dose that enters the gutlumen.}
   \item{Funbound.plasma}{Fraction of plasma that is not bound.}
   \item{Qtotal.liverc}{Flow rate of blood exiting the liver, L/h/kg BW^3/4.}
   \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid filtered from kidney and excreted.}
  \item{BW}{Body Weight, kg}
  \item{MW}{Molecular Weight, g/mol}
  \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
  \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
   \item{liver.density}{Liver tissue density, kg/L.}
   \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte assay using the method of Kilford et al. (2008)}
     
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
 parameters <- parameterize_steadystate(chem.name='Bisphenol-A',species='Rat')
 parameters <- parameterize_steadystate(chem.cas='80-05-7')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
