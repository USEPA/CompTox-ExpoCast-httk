\name{parameterize_3comp}
\alias{parameterize_3comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Parameterize_3comp
}
\description{
This function initializes the parameters needed in the function solve_3comp.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
 parameterize_3comp(chem.cas=NULL,chem.name=NULL,species="Human",
                    default.to.human=F,force.human.clint.fub = F,
                    clint.pvalue.threshold=0.05,adjusted.Funbound.plasma=T,
                    regression=T,suppress.messages=F)
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
  Substitutes missing animal values with human values if true.
  }

  \item{force.human.clint.fub}{
  Forces use of human values for hepatic intrinsic clearance and fraction of unbound plasma if true.
 }
 \item{clint.pvalue.threshold}{Hepatic clearances with clearance assays having p-values greater than the threshold are set to zero.}
 \item{adjusted.Funbound.plasma}{Returns adjusted Funbound.plasma when set to TRUE along with parition coefficients calculated with this value.}
  \item{regression}{Whether or not to use the regressions in calculating partition coefficients.}
  \item{suppress.messages}{Whether or not the output message is suppressed.} 
 }

\value{

  \item{BW}{Body Weight, kg.}
    \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.}
        \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the dose that enters the gutlumen.}
    \item{Funbound.plasma}{Fraction of plasma that is not bound.}
       \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte assay using the method of Kilford et al. (2008)}
    \item{hematocrit}{Percent volume of red blood cells in the blood.}
    \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound concentration in plasma.}
     \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to unbound concentration in plasma.}
      \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to unbound concentration in plasma.}
     \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to unbound concentration in plasma.}
       \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
    \item{MW}{Molecular Weight, g/mol.}
    \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.}
     \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid filtered from kidney and excreted.}
     \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
     \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
      \item{Rblood2plasma}{The ratio of the concentration of the chemical in the blood to the concentration in the plasma.}
      \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
      \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}      
      \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Robert Pearce and John Wambaugh
}
\references{
Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A. (2008). Hepatocellular binding of drugs: correction for unbound fraction in hepatocyte incubations using microsomal binding or drug lipophilicity data. Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
}
\examples{
 parameters <- parameterize_3comp(chem.name='Bisphenol-A',species='Rat')
 parameters <- parameterize_3comp(chem.cas='80-05-7',
                                  species='rabbit',default.to.human=TRUE)
 out <- solve_3comp(parameters=parameters,plots=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
