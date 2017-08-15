\name{available_rblood2plasma}
\alias{available_rblood2plasma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the best available ratio of the blood to plasma concentration constant.
}
\description{
This function finds the best available constant ratio of the blood concentration to the plasma concentration, using get_rblood2plasma and calc_rblood2plasma.
}
\usage{
available_rblood2plasma(chem.cas=NULL, chem.name=NULL,species="Human",
                        Funbound.plasma.pc.correction=T)
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
\item{Funbound.plasma.pc.correction}{
 If TRUE, the Funbound.plasma correction is applied in calculating Rblood2plasma.
} 
}
\details{
If available, in vivo data (from chem.physical_and_invitro.data) for the given species is returned, substituting the human in vivo value when missing. 
In the absence of in vivo data, the value is calculated with calc_rblood2plasma for the given species.
If Funbound.plasma is unvailable for the given species, the human Funbound.plasma is substituted.
If none of these are available, the mean human Rblood2plasma from chem.physical_and_invitro.data is returned. 
%%  ~~ If necessary, more details than the description above ~~
}

\author{
Robert Pearce
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
available_rblood2plasma(chem.name="Bisphenol A",Funbound.plasma.pc.correction=FALSE)
available_rblood2plasma(chem.name="Bisphenol A",species="Rat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
