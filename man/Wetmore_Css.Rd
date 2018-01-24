\name{get_wetmore_css}
\alias{get_wetmore_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Get Wetmore Css
}
\description{
This function retrives a steady-state plasma concentration as a result of infusion dosing from the Wetmore et al. (2012) and (2013) publications.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_wetmore_css(chem.cas=NULL,chem.name=NULL,daily.dose=1,which.quantile=0.95,
                species="Human",clearance.assay.conc=NULL,output.units="mg/L",
                suppress.messages=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%     ~~Describe \code{ssparams.i} here~~
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the cas number or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}

  \item{which.quantile}{
  Which quantile from the SimCYP Monte Carlo simulation is requested. Can be a vector.
%%     ~~Describe \code{ssparams.mean} here~~
}
  \item{species}{
 Species desired (either "Rat" or default "Human"). 
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{clearance.assay.conc}{Concentration of chemical used in measureing intrinsic clearance data, 1 or 10 uM.}
  \item{daily.dose}{
  Total daily dose infused in units of mg/kg BW/day.  Defaults to 1 mg/kg/day.
  %%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{output.units}{
  Returned units for function, defaults to mg/L but can also be uM (specify units = "uM").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{suppress.messages}{Whether or not the output message is suppressed.}
  
}


\references{
Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A., Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck, K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125 157-174 (2012)

Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Li, L., Clewell, H.J. III, Judson, R.S., Freeman, K., Bao, W, Sochaski, M.A., Chu T.-M., Black, M.B., Healy, E, Allen, B., Andersen M.E., Wolfinger, R.D., and Thomas R.S., "The Relative Impact of Incorporating Pharmacokinetics on Predicting in vivo Hazard and Mode-of-Action from High-Throughput in vitro Toxicity Assays" Toxicological Sciences, 132:327-346 (2013).

Wetmore, B. A., Wambaugh, J. F., Allen, B., Ferguson, S. S., Sochaski, M. A., Setzer, R. W., Houck, K. A., Strope, C. L., Cantwell, K., Judson, R. S., LeCluyse, E., Clewell, H.J. III, Thomas, R.S., and Andersen, M. E. (2015). "Incorporating High-Throughput Exposure Predictions with Dosimetry-Adjusted In Vitro Bioactivity to Inform Chemical Toxicity Testing" Toxicological Sciences, kfv171.
}
\author{
John Wambaugh
}

\examples{

get_wetmore_css(chem.cas="34256-82-1")


get_wetmore_css(chem.cas="34256-82-1",species="Rat",which.quantile=0.5)

get_wetmore_css(chem.cas="80-05-7", daily.dose = 1,which.quantile = 0.5, output.units = "uM")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Wetmore}
\keyword{Monte Carlo}% __ONLY ONE__ keyword per line
