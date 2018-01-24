\name{get_rblood2plasma}
\alias{get_rblood2plasma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get ratio of the blood concentration to the plasma concentration.
}
\description{
This function retrieves the in vivo ratio of the blood concentration to the plasma concentration.
}
\usage{
get_rblood2plasma(chem.name=NULL, chem.cas=NULL, species="Human",default.to.human=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{species}{
   Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true.
  }
}
\details{
A value of NA is returned when the requested value is unavailable.  Values are retrieved from chem.physical_and_invitro.data.
%%  ~~ If necessary, more details than the description above ~~
}

\author{
Robert Pearce
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
get_rblood2plasma(chem.name="Bisphenol A")
get_rblood2plasma(chem.name="Bisphenol A",species="Rat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
