\name{get_wetmore_oral_equiv}
\alias{get_wetmore_oral_equiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Wetmore Oral Equivalent Dose
}
\description{
This function converts a chemical plasma concetration to an oral equivalent dose using the values from the Wetmore et al. (2012) and (2013) publications.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_wetmore_oral_equiv(conc,chem.name=NULL,chem.cas=NULL,suppress.messages=F,
                       which.quantile=0.95,species="Human",input.units='uM',
                       output.units='mg',clearance.assay.conc=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conc}{
  Bioactive in vitro concentration in units of specified input.units, default of uM.
%%     ~~Describe \code{ssparams.i} here~~
}
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{input.units}{Units of given concentration, default of uM but can also be mg/L.}
  \item{output.units}{Units of dose, default of 'mg' for mg/kg BW/ day or 'mol' for mol/ kg BW/ day.}
  \item{suppress.messages}{
  Suppress output messages.
%%     ~~Describe \code{sds} here~~
}
  \item{which.quantile}{
  Which quantile from the SimCYP Monte Carlo simulation is requested. Can be a vector.  Papers include 0.05, 0.5, and 0.95 for humans and 0.5 for rats.
%%     ~~Describe \code{ssparams.mean} here~~
}
  \item{species}{
  Species desired (either "Rat" or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{clearance.assay.conc}{Concentration of chemical used in measureing intrinsic clearance data, 1 or 10 uM.}
  \item{...}{Additional parameters passed to get_wetmore_css.}
}

\value{

Equivalent dose in specified units, default of mg/kg BW/day.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
table <- NULL
for(this.cas in sample(get_wetmore_cheminfo(),50)) table <- rbind(table,cbind(
as.data.frame(this.cas),as.data.frame(get_wetmore_oral_equiv(conc=1,chem.cas=this.cas))))




get_wetmore_oral_equiv(0.1,chem.cas="34256-82-1")

get_wetmore_oral_equiv(0.1,chem.cas="34256-82-1",which.quantile=c(0.05,0.5,0.95))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Wetmore}
\keyword{Monte Carlo}% __ONLY ONE__ keyword per line
