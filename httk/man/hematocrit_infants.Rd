% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hematocrit_infants.R
\name{hematocrit_infants}
\alias{hematocrit_infants}
\title{Predict hematocrit in infants under 1 year old.}
\usage{
hematocrit_infants(age_months)
}
\arguments{
\item{age_months}{Vector of ages in months; all must be <= 12.}
}
\value{
Vector of hematocrit percentages corresponding to the input vector
  of ages.
}
\description{
For infants under 1 year, hematocrit was not measured in NHANES. Assume a
log-normal distribution where plus/minus 1 standard deviation of the
underlying normal distribution is given by the reference range. Draw
hematocrit values from these distributions by age.
}
\details{
\tabular{cc}{ 
Age \tab Reference range\cr 
<1 month \tab 31-49\cr 
1-6 months \tab 29-42\cr 
7-12 months \tab 33-38 
}
}
\references{
\insertRef{ring2017identifying}{httk}
}
\author{
Caroline Ring
}
\keyword{httk-pop}
