\name{parameterize_1comp}
\alias{parameterize_1comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Parameterize_1comp
}
\description{
This function initializes the parameters needed in the function solve_1comp.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
 parameterize_1comp(chem.cas=NULL,chem.name=NULL,species="Human",
                    default.to.human=F,Funbound.plasma.pc.correction=T,
                    restrictive.clearance=T,well.stirred.correction=T,
                    suppress.messages=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{species}{
    Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
}
  \item{default.to.human}{
  Substitutes missing rat values with human values if true.
  }
  \item{Funbound.plasma.pc.correction}{If set to TRUE, volume of distribution and elimination are calculated with the corrected partition coefficients, using regressions, and corrected Funbound.plasma.}
 \item{restrictive.clearance}{In calculating elimination rate, protein binding is not taken into account (set to 1) in liver clearance if FALSE.}
  \item{well.stirred.correction}{Uses correction in calculation of hepatic clearance for well-stirred model if TRUE.  
                                  This assumes clearance relative to amount unbound in whole blood instead of plasma, but converted to use with plasma concentration.}
 \item{suppress.messages}{Whether or not to suppress messages.}
}


\value{

  \item{Vdist}{Volume of distribution, units of L/kg BW.}
  \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the dose that enters the gutlumen.}
  \item{kelim}{Elimination rate, units of 1/h.}
    \item{hematocrit}{Percent volume of red blood cells in the blood.}
  \item{kgutabs}{Rate chemical is absorbed, 1/h.}
  \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
   \item{MW}{Molecular Weight, g/mol.} 
   \item{Rblood2plasma}{The ratio of the concentration of the chemical in the blood to the concentration in the plasma.}
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
 parameters <- parameterize_1comp(chem.name='Bisphenol-A',species='Rat')
 parameters <- parameterize_1comp(chem.cas='80-05-7',restrictive.clearance=FALSE,
                                  species='rabbit',default.to.human=TRUE)
 out <- solve_1comp(parameters=parameters)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
