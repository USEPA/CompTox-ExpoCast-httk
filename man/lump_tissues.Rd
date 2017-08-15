\name{lump_tissues}
\alias{lump_tissues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lump tissue parameters
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function takes the parameters from predict_partitioning_schmitt and lumps the partition coefficients along with the volumes and flows based on the given tissue list.
It is useful in Monte Carlo simulation of individual partition coefficients when calculating the rest of body partition coefficient.
}
\usage{
lump_tissues(Ktissue2pu.in, tissuelist=NULL, species="Human")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Ktissue2pu.in}{List of partition coefficients from predict_partitioning_schmitt.} 
\item{tissuelist}{Specifies compartment names and tissues groupings. Remaining tissues in tissue.data are lumped in the rest of the body.}
\item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
}


\value{
\item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to unbound concentration in plasma.}
\item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to unbound concentration in plasma.}
\item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
\item{Vliverc}{ Volume of the liver per kg body weight, L/kg BW.}
\item{Qtotal.liverf}{Fraction of cardiac output flowing to the gut and liver, i.e. out of the liver.}
\item{Qgutf}{Fraction of cardiac output flowing to the gut.}
\item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
}

\author{
John Wambaugh
}

\examples{
pcs <- predict_partitioning_schmitt(chem.name='bisphenola')
tissuelist <- list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")
,muscle.bone=c('muscle','bone'))
lump_tissues(pcs,tissuelist=tissuelist)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
