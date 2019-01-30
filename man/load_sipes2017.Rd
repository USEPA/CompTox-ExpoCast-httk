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
load_sipes2017(load.image=T,overwrite=F,target.env=.GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{load.image}{If overwrite=TRUE (DEFAULT)) then the default HTTK chemical data plus the any new data/predictions from Sipes et al. (2017) will be quickly loaded. This is the same as load.image=F, but much faster, however any other data added by the user will be deleted.}
\item{overwrite}{Only matters if load.image=FALSE. If overwrite=TRUE then existing data in chem.physical_and_invitro.data will be replaced by any data/predictions in Sipes et al. (2017) that is for the same chemical and property. If overwrite=FALSE (DEFAULT) then new data for the same chemical and property are ignored.  Funbound.plasma values of 0 (below limit of detection) are overwritten either way.}
\item{target.env}{The environment where the new chem.physical_and_invitro.data is loaded. Defaults to global environment.}
}

\value{
\item{data.frame}{An updated version of chem.physical_and_invitro.data.}}

\author{
Robert Pearce and John Wambaugh
}

\references{
Sipes, Nisha S., et al. "An intuitive approach for predicting potential human health risk with the Tox21 10k library." Environmental Science & Technology 51.18 (2017): 10786-10796.
}

\examples{
\dontrun{
chem.physical_and_invitro.data <- load_sipes2017()
chem.physical_and_invitro.data <- load_sipes2017(overwrite=T) 
}                        
}                                   
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
