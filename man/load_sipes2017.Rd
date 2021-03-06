% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_sipes2017.R
\name{load_sipes2017}
\alias{load_sipes2017}
\title{Load data from Sipes et al 2017.}
\usage{
load_sipes2017(overwrite = F, target.env = .GlobalEnv)
}
\arguments{
\item{overwrite}{Only matters if load.image=FALSE. If overwrite=TRUE then
existing data in chem.physical_and_invitro.data will be replaced by any
data/predictions in Sipes et al. (2017) that is for the same chemical and
property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
and property are ignored.  Funbound.plasma values of 0 (below limit of
detection) are overwritten either way.}

\item{target.env}{The environment where the new
chem.physical_and_invitro.data is loaded. Defaults to global environment.}
}
\value{
\item{data.frame}{An updated version of
chem.physical_and_invitro.data.}
}
\description{
This function returns an updated version of chem.physical_and_invitro.data
that includes data predicted with Simulations Plus' ADMET predictor that was
used in Sipes et al. 2017, included in admet.data.
}
\examples{

\dontrun{
chem.physical_and_invitro.data <- load_sipes2017()
chem.physical_and_invitro.data <- load_sipes2017(overwrite=T) 
}                        

}
\references{
Sipes, Nisha S., et al. "An intuitive approach for predicting
potential human health risk with the Tox21 10k library." Environmental
Science & Technology 51.18 (2017): 10786-10796.
}
\author{
Robert Pearce and John Wambaugh
}
