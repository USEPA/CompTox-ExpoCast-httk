\name{load_sipes2017}
\alias{load_sipes2017}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load data from Sipes et al 2017.
}
\description{
This function returns an updated version of chem.physical_and_invitro.data that includes data predicted with Simulations Plus' ADMET predictor that was used in Sipes et al. 2017, included in admet.data.
}
\usage{
load_sipes2017(overwrite=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{overwrite}{If overwrite=TRUE then existing data in chem.physical_and_invitro.data will be replaced by any data in Sipes2017 that is for the same chemical and property. If overwrite=FALSE (DEFAULT) then new data for the same chemical and property are ignored.  Funbound.plasma values of 0 (below limit of detection) are overwritten either way.}
}

\value{
\item{data.frame}{An updated version of chem.physical_and_invitro.data.}}

\author{
Robert Pearce
}
\source{ADMET, Simulations Plus}
\references{
Sipes, Nisha S., et al. "An Intuitive Approach for Predicting Potential Human Health Risk with the Tox21 10k Library." Environmental Science & Technology 51.18 (2017): 10786-10796.
}
\examples{
\dontrun{
chem.physical_and_invitro.data <- load_sipes2017()
chem.physical_and_invitro.data <- load_sipes2017(overwrite=T) 
}                        
}                                   
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
