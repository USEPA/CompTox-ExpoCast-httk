% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tissue_mass_functions.R
\name{blood_weight}
\alias{blood_weight}
\title{Predict blood mass.}
\usage{
blood_weight(BSA, gender)
}
\arguments{
\item{BSA}{Body surface area in m^2. May be a vector.}

\item{gender}{Either 'Male' or 'Female'. May be a vector.}
}
\value{
A vector of blood masses in kg the same length as \code{BSA} and
\code{gender}.
}
\description{
Predict blood mass based on body surface area and gender, using equations
from Bosgra et al. 2012
}
\references{
Bosgra, Sieto, et al. "An improved model to predict 
physiologically based model parameters and their inter-individual variability 
from anthropometry." Critical reviews in toxicology 42.9 (2012): 751-767.

\insertRef{ring2017identifying}{httk}
}
\author{
Caroline Ring
}
\keyword{httk-pop}
