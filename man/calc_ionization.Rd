\name{calc_ionization}
\alias{calc_ionization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the ionization.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the ionization of a compound at a given pH.  The pKa's are 
either entered as parameters or taken from a specific compound in the package.
}
\usage{
 calc_ionization(chem.cas=NULL,chem.name=NULL,pH=NULL,pKa_Donor=NA,pKa_Accept=NA)
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
  \item{pH}{pH where ionization is evaluated.}
  
  \item{pKa_Donor}{
Compound H dissociation equilibirum constant(s).  Overwrites chem.name and chem.cas.
}
  \item{pKa_Accept}{
Compound H association equilibirum constant(s).  Overwrites chem.name and chem.cas.
  } 
}
\value{

 \item{fraction_neutral}{fraction of compound neutral}
  \item{fraction_charged}{fraction of compound charged}
  \item{fraction_negative}{fraction of compound negative}
   \item{fraction_positive}{fraction of compound positive}
    \item{fraction_zwitter}{fraction of compound zwitterionic}
}


\details{
The fractions are calculated by determining the coefficients for each species and 
dividing the particular species by the sum of all three.  The positive, negative
and zwitterionic/neutral coefficients are given by: 
\deqn{zwitter/netural = 1}  
\deqn{for(i in 1:pkabove) negative = negative + 10^(i * pH - pKa1 - ... - pKai)}  
\deqn{for(i in 1:pkbelow) positive = positive + 10^(pKa1 + ... + pKai - i * pH)}
where i begins at 1 and ends at the number of points above(for negative) or 
below(for positive) the neutral/zwitterionic range.  The neutral/zwitterionic range 
is either the pH range between 2 pKa's where the number of acceptors above is equal
to the number of donors below, everything above the pKa acceptors if there are 
no donors, or everything below the pKa donors if there are no acceptors.  Each
of the terms in the sums represent a different ionization. 

}

\author{
Robert Pearce
}

\examples{
calc_ionization(chem.name='bisphenola',pH=7.4)
calc_ionization(pKa_Donor=8,pKa_Accept=c(1,4),pH=9)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
