#' Reference tissue masses and flows from tables in McNally et al. 2014.
#' 
#' Reference tissue masses, flows, and marginal distributions from McNally et
#' al. 2014.
#' 
#' @format A data.table with variables: \describe{\item{\code{tissue}}{Body 
#'   tissue} \item{\code{gender}}{Gender: Male or Female} 
#'   \item{\code{mass_ref}}{Reference mass in kg, from Reference Man} 
#'   \item{\code{mass_cv}}{Coefficient of variation for mass} 
#'   \item{\code{mass_dist}}{Distribution for mass: Normal or Log-normal} 
#'   \item{\code{flow_ref}}{Reference flow in L/h, from Reference Man} 
#'   \item{\code{flow_cv}}{Coefficient of variation for flow (all normally 
#'   distributed)} \item{\code{height_ref}}{Reference heights (by gender)} 
#'   \item{\code{CO_ref}}{Reference cardiac output by gender} 
#'   \item{\code{flow_frac}}{Fraction of CO flowing to each tissue: 
#'   \code{flow_ref}/\code{CO_ref}}}
#' @source McNally K, Cotton R, Hogg A, Loizou G. "PopGen: A virtual human 
#'   population generator." Toxicology 315, 70-85, 2004.
"mcnally_dt"

#'Smoothing splines for log hematocrit vs. age in months, and KDE residuals, by
#'race and gender.
#'
#'Smoothing splines and KDE residuals pre-calculated from NHANES hematocrit and
#'age data by race/ethnicity and gender.
#'
#'@format A data.table with 6 variables: \describe{ \item{\code{gender}}{Gender:
#'  Male or Female} \item{\code{reth}}{Race/ethnicity: Mexican American, Other 
#'  Hispanic, Non-Hispanic White, Non-Hispanic Black, Other} 
#'  \item{\code{hct_spline}}{A list of smooth.spline objects, each giving a 
#'  smoothed relationship between log hematocrit and age in months} 
#'  \item{\code{hct_kde}}{A list of kde objects; each is a KDE of the
#'  distribution of residuals about the smoothing spline.}}
"spline_hematocrit"

#'Smoothing splines for log serum creatinine vs. age in months, along with KDE
#'residuals, by race and gender.
#'
#'#'Smoothing splines and KDE residuals pre-calculated from NHANES serum creatinine and
#'age data by race/ethnicity and gender.
#'
#'@format A data.table with 6 variables: \describe{ 
#'\item{\code{gender}}{Gender:
#'  Male or Female} 
#'  \item{\code{reth}}{Race/ethnicity: Mexican American, Other 
#'  Hispanic, Non-Hispanic White, Non-Hispanic Black, Other} 
#'  \item{\code{sc_spline}}{A list of smooth.spline objects, each giving a 
#'  smoothed relationship between log serum creatinine and age in months} 
#'  \item{\code{sc_kde}}{A list of kde 
#'  objects; each is a KDE of the distribution of residuals about the smoothing 
#'  spline.}
#'   }
"spline_serumcreat"

#'Smoothing splines for log height vs. age and log body weight vs. age, along 
#'with 2-D KDE residuals, by race and gender.
#'
#'#'Smoothing splines and KDE fits to joint distribution of height and weight
#'residuals pre-calculated from NHANES height, weight, and age data by
#'race/ethnicity and gender.
#'
#'@format A data.table with 6 variables: \describe{ \item{\code{g}}{Gender: Male
#'  or Female} \item{\code{r}}{Race/ethnicity: Mexican American, Other Hispanic,
#'  Non-Hispanic White, Non-Hispanic Black, Other} \item{\code{height_spline}}{A
#'  list of smooth.spline objects, each giving a smoothed relationship between 
#'  log height in cm and age in months} \item{\code{weight_spline}}{A list of 
#'  smooth.spline objects, each giving a smoothed relationship between log body 
#'  weight in kg and age in months} \item{\code{hw_kde}}{A list of kde objects;
#'  each is a 2-D KDE of the distribution of log height and log body weight
#'  residuals about the smoothing splines.} }
"spline_heightweight"

#'Pre-processed NHANES data.
#'
#'NHANES data on demographics, anthropometrics, and some laboratory measures, 
#'cleaned and combined into a single data set.
#'
#'@format A survey.design2 object, including masked cluster and strata. 
#'  Variables are available as a data.table by \code{nhanes_mec_svy$variables}. 
#'  Variables are as described in NHANES Demographics and Examination 
#'  documentation, with the exception of: \describe{ 
#'  \item{\code{wtmec6yr}}{6-year sample weights for combining 3 cycles, 
#'  computed by dividing 2-year sample weights by 3.} 
#'  \item{\code{bmxhtlenavg}}{Average of height and recumbent length if both 
#'  were measured; if only one was measured, takes value of the one that was 
#'  measured.} \item{\code{logbmxwt}}{Natural log of measured body weight.} 
#'  \item{\code{logbmxhtlenavg}}{Natural log of \code{bmxhtlenavg}.} 
#'  \item{\code{weight_class}}{One of Underweight, Normal, Overweight, or Obese.
#'  Assigned using methods in \code{get_weight_class}.} }
#'  
#'@source \url{www.cdc.gov/nhanes/nhanes_questionnaires.htm}
"nhanes_mec_svy"

#'Smoothed age distributions by race and gender.
#'
#'Distributions of ages in months, computed from NHANES data smoothed using
#'survey::svysmooth(), for each combination of race/ethnicity and gender.
#'
#'@format A data.table object with three variables: \describe{ 
#'  \item{\code{gender}}{Gender: Male or Female} 
#'  \item{\code{reth}}{Race/ethnicity} \item{\code{smth}}{A list of 
#'  \code{svysmooth} objects, each encoding a weighted smoothed distribution of 
#'  ages.}}
"age_dist_smooth"

#'CDC BMI-for-age charts
#'
#'Charts giving the BMI-for-age percentiles for boys and girls ages 2-18
#'
#'For children ages 2 to 18, weight class depends on the BMI-for-age percentile.
#'\describe{
#'\item{Underweight}{<5th percentile}
#'\item{Normal weight}{5th-85th percentile}
#'\item{Overweight}{85th-95th percentile}
#'\item{Obese}{>=95th percentile}
#'}
#'
#'@format A data.table object with variables \describe{
#'\item{\code{Sex}}{'Male' or 'Female'} 
#'  \item{\code{Agemos}}{Age in months}
#'  \item{\code{L}, 
#'  \code{M}, \code{S}}{LMS parameters; see 
#'  \url{www.cdc.gov/growthcharts/percentile_data_files.htm}} 
#'  \item{\code{P3}, 
#'  \code{P5}, \code{P10}, \code{P25}, \code{P50}, \code{P75}, \code{P85}, 
#'  \code{P90}, \code{P95}, and \code{P97}}{BMI percentiles}}
#'  
#'  
#'@source \url{www.cdc.gov/growthcharts/percentile_data_files.htm}
"bmiage"

#'WHO weight-for-length charts
#'
#'Charts giving weight-for-length percentiles for boys and girls under age 2.
#'
#'For infants under age 2, weight class depends on weight for length percentile.
#'#'\describe{ \item{Underweight}{<2.3rd percentile} \item{Normal
#'weight}{2.3rd-97.7th percentile} \item{Obese}{>=97.7th percentile} }
#'
#'@format A data.table object with variables \describe{ \item{\code{Sex}}{'Male'
#'  or 'Female'} \item{\code{Length}}{length in cm} \item{\code{L}, \code{M},
#'  \code{S}}{LMS parameters; see 
#'  \url{www.cdc.gov/growthcharts/percentile_data_files.htm}} \item{\code{P2.3},
#'  \code{P5}, \code{P10}, \code{P25}, \code{P50}, \code{P75}, \code{P90},
#'  \code{P95}, and \code{P97.7}}{weight percentiles} }
#'  
#'@source
#'  \url{http://www.cdc.gov/growthcharts/who/girls_weight_head_circumference.htm}
#'  and
#'  \url{http://www.cdc.gov/growthcharts/who/boys_weight_head_circumference.htm}
"wfl"