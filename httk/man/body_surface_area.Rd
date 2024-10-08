% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tissue_mass_functions.R
\name{body_surface_area}
\alias{body_surface_area}
\title{Predict body surface area.}
\usage{
body_surface_area(BW, H, age_years)
}
\arguments{
\item{BW}{A vector of body weights in kg.}

\item{H}{A vector of heights in cm.}

\item{age_years}{A vector of ages in years.}
}
\value{
A vector of body surface areas in cm^2.
}
\description{
Predict body surface area from weight, height, and age, using Mosteller's
formula for age>18 and Haycock's formula for age<18
}
\references{
Mosteller, R. D. "Simplified calculation of body surface area." 
N Engl J Med 317 (1987): 1098..

Haycock, George B., George J. Schwartz, and David H. Wisotsky. "Geometric 
method for measuring body surface area: a height-weight formula validated in 
infants, children, and adults." The Journal of pediatrics 93.1 (1978): 62-66.

\insertRef{ring2017identifying}{httk}
}
\author{
Caroline Ring
}
\keyword{httk-pop}
