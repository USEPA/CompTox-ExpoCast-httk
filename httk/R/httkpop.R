#' CDC BMI-for-age charts for ages 2-20 years
#'
#' A dataset containing relevant data from the CDC BMI-for-age growth charts
#'
#' CDC sex-specific BMI-for-age percentiles are used to define childhood weight
#' status. See \code{\link{get_weight_class}}.
#'
#' @format A data.table with 434 rows and 5 variables: \describe{
#'   \item{Sex}{Female or Male} \item{Agemos}{Age in months} \item{P5}{The 5th
#'   percentile BMI for the corresponding sex and age} \item{P85}{The 85th
#'   percentile BMI for the corresponding sex and age} \item{P95}{The 95th
#'   percentile BMI for the corresponding sex and age} }
#' @source \url{http://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv}
"bmiage"

#' WHO weight-for-length charts for ages 0-24 months
#'
#' A dataset containing relevant data from the WHO sex-specific
#' weight-for-length charts
#'
#' WHO sex-specific weight-for-length percentiles are used to define weight
#' class for infants and children less than 2 years old. See
#' \code{\link{get_weight_class}}.
#'
#' @format a data.table with 262 rows and 4 variables:
#'
#'   \describe{ \item{Sex}{"Male" or "Female"} \item{Length}{Recumbent length in
#'   cm} \item{P2.3}{The 2.3rd percentile weight in kg for the corresponding sex
#'   and recumbent length} \item{P97.7}{The 97.7th percentile weight in kg for
#'   the corresponding sex and recumbent length}}
#'   
#' @source 
#' \url{https://www.cdc.gov/growthcharts/who/boys_weight_head_circumference.htm}
#' \url{https://www.cdc.gov/growthcharts/who/girls_weight_head_circumference.htm}
"wfl"

#' Table data from McNally et al. (2014)
#'
#' Data from Table 1, Table 4, and Table 5 of McNally et al. (2014) describing
#' reference values for tissue masses and flows, and coefficients of variation
#' describing residual variability in tissue masses and flows.
#'
#' @format A data.table with 36 rows and 10 variables: \describe{
#'   \item{tissue}{Tissue or organ: Lung, Heart, Skin, Adipose, Muscle, Bone,
#'   Brain, Gonads, Kidney, Stomach, Small_intestine, Large_intestine, Spleen,
#'   Pancreas, Liver, CO (for cardiac output), Blood, and Skeleton.}
#'   \item{gender}{"Male" or "Female"} \item{mass_ref}{Mass of this tissue (kg)
#'   for Reference Man or Reference Woman} \item{mass_cv}{Coefficient of
#'   variation for residual variability in this tissue's mass}
#'   \item{mass_dist}{Distribution type for residual variability in this
#'   tissue's mass: either "Log-normal" or "Normal"} \item{flow_ref}{Blood flow
#'   rate (L/h) for this tissue in Reference Man or Reference Woman.}
#'   \item{flow_cv}{Coefficient of variation for residual variability in this
#'   tissue's flow.} \item{height_ref}{Height (cm) of Reference Man or Referenc
#'   Woman, for allometric scaling of tissue mass or flow.}
#'   \item{CO_ref}{Cardiac output (L/h) of Reference Man or Reference Woman.}
#'   \item{flow_frac}{Blood flow rate for each tissue as a fraction of cardiac
#'   output for Reference Man or Reference Woman.} }
#'
#' @source McNally K, Cotton R, Hogg A, Loizou G. PopGen: A virtual human
#'   population generator. Toxicology. 2014;315:70-85.
#'   \url{http://dx.doi.org/10.1016/j.tox.2013.07.009}
"mcnally_dt"

