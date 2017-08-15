\name{calc_hepatic_clearance}
\alias{calc_hepatic_clearance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the hepatic clearance.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the hepatic clearance in plasma for a well-stirred model or other type if specified.
}
\usage{
calc_hepatic_clearance(chem.name=NULL,chem.cas=NULL,parameters=NULL,
                       species='Human',default.to.human=F,
                       hepatic.model='well-stirred',
                       suppress.messages=F,well.stirred.correction=T,
                       restrictive.clearance=T,Funbound.plasma.pc.correction=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{species}{
  Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true.
  }
  \item{parameters}{Chemical parameters from parameterize_steadystate function, overrides chem.name and chem.cas.}
  \item{hepatic.model}{Model used in calculating hepatic clearance, unscaled, parallel tube, dispersion, or default well-stirred.}  
   \item{suppress.messages}{Whether or not to suppress the output message.}
  \item{well.stirred.correction}{Uses correction in calculation of hepatic clearance for well-stirred model if TRUE for hepatic.model well-stirred.  
                                  This assumes clearance relative to amount unbound in whole blood instead of plasma, but converted to use with plasma concentration.}
 \item{restrictive.clearance}{Protein binding not taken into account (set to 1) in liver clearance if FALSE.}
    \item{Funbound.plasma.pc.correction}{Uses corrected Funbound.plasma when set to TRUE along with parition coefficients calculated with this value and regressions.  Only applies to calculation of Rblood2plasma in well-stirred correction when in vivo Rblood2plasma is unavailable.}
   \item{...}{Additional parameters passed to parameterize_steadystate if parameters is NULL.}
}

\value{
\item{Hepatic Clearance}{Units of L/h/kg BW.}
}

\author{
John Wambaugh
}

\examples{
calc_hepatic_clearance(chem.name="Ibuprofen",hepatic.model='unscaled')
calc_hepatic_clearance(chem.name="Ibuprofen",well.stirred.correction=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
