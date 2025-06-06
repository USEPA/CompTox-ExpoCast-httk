% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_hep_fu.R
\name{calc_hep_fu}
\alias{calc_hep_fu}
\title{Calculate the free chemical in the hepaitic clearance assay}
\usage{
calc_hep_fu(
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  parameters = NULL,
  Vr = 0.005,
  pH = 7.4
)
}
\arguments{
\item{chem.cas}{Chemical Abstract Services Registry Number (CAS-RN) -- if
parameters is not specified then the chemical must be identified by either
CAS, name, or DTXISD}

\item{chem.name}{Chemical name (spaces and capitalization ignored) --  if
parameters is not specified then the chemical must be identified by either
CAS, name, or DTXISD}

\item{dtxsid}{EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})
 -- if parameters is not specified then the chemical must be identified by 
either CAS, name, or DTXSIDs}

\item{parameters}{Parameters from the appropriate parameterization function
for the model indicated by argument model}

\item{Vr}{Ratio of cell volume to incubation volume. Default (0.005) is taken from}

\item{pH}{pH of the incupation medium.}
}
\value{
A numeric fraction between zero and one
}
\description{
This function uses the method from Kilford et al. (2008) to calculate the
fraction of unbound chemical in the 
 hepatocyte intrinsic clearance assay. The bound chemical is presumed to be
unavailable during the performance of the assay, so this fraction can be
used to increase the apparent clearance rate to better estimate in vivo 
clearance. 
For bases, the fraction of chemical unbound in hepatocyte clearance assays 
(\ifelse{html}{\out{fu<sub>hep</sub>}}{\eqn{fu_{hep}}}) is calculated in terms of 
\ifelse{html}{\out{logP<sub>ow</sub>}}{\eqn{logP_{ow}}}
but for neutrual and acidic compounds we use 
\ifelse{html}{\out{logD<sub>ow</sub>}}{\eqn{logD_{ow}}} (from \code{\link{calc_dow}}). 
Here we denote the appropriate partition coefficient as "logP/D".
Kilford et al. (2008) calculates
\ifelse{html}{\out{fu<sub>hep</sub> = 1/(1 + 125*V<sub>R</sub>*10^(0.072*logP/D<sup>2</sup> + 0.067*logP/D-1.126))}}{\deqn{fu_{hep} = \frac{1}{1+125*V_{R}*10^{0.072*logP*D^2 + 0.067*logP/D - 1.126}}}}
}
\details{
Note that octanal:water partitioning above 1:1,000,000 
(\ifelse{html}{\out{LogP<sub>ow</sub> > 6}}{\eqn{LogP_{ow} > 6}})
are truncated at 1:1,000,000 because greater partitioning would
likely take longer than hepatocyte assay itself.
}
\references{
\insertRef{kilford2008hepatocellular}{httk} 

\insertRef{wetmore2015incorporating}{httk}
}
\seealso{
\code{\link{apply_clint_adjustment}}
}
\author{
John Wambaugh and Robert Pearce
}
\keyword{in-vitro}
