#This file is used by roxygen2 to generate man files (documentation) for data
#sets included in the package.
#
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
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
#'@keywords data
#'
#'@author Caroline Ring
#'@keywords httk-pop
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
"wfl"

#' Microtiter Plate Well Descriptions for Armitage et al. (2014) Model
#'
#' Microtiter Plate Well Descriptions for Armitage et al. (2014) model from
#' Honda et al. (2019)
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{area_bottom}{}
#'   \item{cell_yield}{}
#'   \item{diam}{} 
#'   \item{sysID}{}
#'   \item{v_total}{} 
#'   \item{v_working}{} 
#'   \item{well_desc}{}
#'   \item{well_number}{}
#' }
#' @source \url{http://www.diamondse.info/}
#'@keywords data
#'@keywords httk-pop
#'
#'@author Greg Honda
#'@references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol. 
#'2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#'@references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#'In Vivo Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
"well_param"

#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#'
#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{MP}{}
#'   \item{MW}{} 
#'   \item{casrn}{}
#'   \item{compound_name}{}
#'   \item{gkaw}{}
#'   \item{gkow}{}
#'   \item{gswat}{}
#' }
#' @source \url{http://www.diamondse.info/}
#'
#'@keywords data
#'
#'@author Greg Honda
#'
#'@references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol. 
#'2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#'@references Honda, Gregory S., et al. "Using the Concordance of In Vitro and 
#'In Vivo Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
"armitage_input"

#' DRUGS|NORMAN: Pharmaceutical List with EU, Swiss, US Consumption Data
#'
#' SWISSPHARMA is a list of pharmaceuticals with consumption data from 
#' Switzerland, France, Germany and the USA, used for a suspect 
#' screening/exposure modelling approach described in 
#' Singer et al 2016, DOI: 10.1021/acs.est.5b03332. The original data is 
#' available on the NORMAN Suspect List Exchange.
#'
#'@source \url{https://comptox.epa.gov/dashboard/chemical_lists/swisspharma}
#'@keywords data
#'
#'@references Wambaugh et al. "Assessing Toxicokinetic Uncertainty and 
#'Variability in Risk Prioritization ", submitted.
"pharma"

#' in vitro Toxicokinetic Data from Wambaugh et al. (submitted)
#'
#' These data are the new HTTK in vitro data for chemicals reported in Wambaugh
#' et al. (submitted) They
#' are the processed values used to make the figures in that manuscript.
#' These data summarize the results of Bayesian analysis of the in vitro
#' toxicokinetic experiments conducted by Cyprotex to characterize fraction 
#' unbound in the presence of pooled human plasma protein and the intrnsic 
#' hepatic clearance of the chemical by pooled human hepatocytes.
#'
#' @format A data frame with 496 rows and 17 variables:
#' describe{
#'   \item{Compound}{The name of the chemical}
#'   \item{CAS}{The Chemical Abstracts Service Registry Number}                         
#'   \item{Human.Clint}{Median of Bayesian credible interval for intrinsic 
#' hepatic clearance (uL/min/million hepatocytes)]}
#'   \item{Human.Clint.pValue}{Probability that there is no clearance}          
#'   \item{Human.Funbound.plasma}{Median of Bayesian credibl interval for 
#' fraction of chemical free in the presence of plasma}
#'   \item{pKa_Accept}{pH(s) at which hydrogen acceptor sites (if any) are at 
#' equilibrium}                  
#'   \item{pKa_Donor}{pH(s) at which hydrogne donor sites (if any) are at 
#' equilibrium}
#'   \item{DSSTox_Substance_Id}{Identifier for CompTox Chemical Dashboard}         
#'   \item{SMILES}{Simplified Molecular-Input Line-Entry System structure 
#' description}
#'   \item{Human.Clint.Low95}{Lower 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance (uL/min/million hepatocytes)}        
#'   \item{Human.Clint.High95}{Uppper 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance (uL/min/million hepatocytes)}
#'   \item{Human.Clint.Point}{Point estimate of intrinsic hepatic clearance 
#' (uL/min/million hepatocytes)}
#'   \item{Human.Funbound.plasma.Low95}{Lower 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma}
#'   \item{Human.Funbound.plasma.High95}{Upper 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma}
#'   \item{Human.Funbound.plasma.Point}{Point estimate of the fraction of
#' chemical free in the presence of plasma}
#'   \item{MW}{Molecular weight (Daltons)}                         
#'   \item{logP}{log base ten of octanol:water partiion coefficient}
#' }
#' @source Wambaugh et al. (submitted)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#'@references Wambaugh et al. "Assessing Toxicokinetic Uncertainty and 
#' Variability in Risk Prioritization", submitted.
"wambaugh2019"

#' Raw Bayesian in vitro Toxicokinetic Data Analysis from Wambaugh et al. (submitted)
#'
#' These data are the new HTTK in vitro data for chemicals reported in Wambaugh
#' et al. (submitted) They
#' are the output of different Bayesian models evaluated to compare using a
#' single protein concentration vs. the new three concentration titration
#' protocol. These data summarize the results of Bayesian analysis of the in vitro
#' toxicokinetic experiments conducted by Cyprotex to characterize fraction 
#' unbound in the presence of pooled human plasma protein and the intrnsic 
#' hepatic clearance of the chemical by pooled human hepatocytes.
#' This file includes replicates (diferent CompoundName id's but same chemical')
#'
#' @format A data frame with 530 rows and 28 variables:
#' describe{
#'   \item{DTXSID}{Identifier for CompTox Chemical Dashboard}         
#'   \item{Name}{The name of the chemical}
#'   \item{CAS}{The Chemical Abstracts Service Registry Number}                         
#'   \item{CompoundName}{Sample name provided by EPA to Cyprotex}
#'   \item{Fup.point}{Point estimate of the fraction of
#' chemical free in the presence of plasma}
#'   \item{Base.Fup.Med}{Median of Bayesian credible interval for 
#' fraction of chemical free in the presence of plasma for analysis of 100%
#' physiological plasma protein data only (base model)}
#'   \item{Base.Fup.Low}{Lower 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma for analysis of 100%
#' physiological plasma protein data only (base model)}
#'   \item{Base.Fup.High}{Upper 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma for analysis of 100%
#' physiological plasma protein data only (base model)}
#'   \item{Affinity.Fup.Med}{Median of Bayesian credible interval for 
#' fraction of chemical free in the presence of plasma for analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Affinity.Fup.Low}{Lower 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma for analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Affinity.Fup.High}{Upper 95th percentile of Bayesian credible 
#' interval for fraction of chemical free in the presence of plasma for analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Affinity.Kd.Med}{Median of Bayesian credible interval for 
#' protein binding affinity from analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Affinity.Kd.Low}{Lower 95th percentile of Bayesian credible 
#' interval for protein binding affinity from analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Affinity.Kd.High}{Upper 95th percentile of Bayesian credible 
#' interval for protein binding affinity from analysis of protein
#' titration protocol data (affinity model)}
#'   \item{Decreases.prob}{Probability that the chemical concentration decreased
#' systematiclally during hepatic clearance assay.}
#'   \item{Saturates.prob}{Probability that the rate of chemical concentration
#' decrease varied between the 1 and 10 uM hepatic clearance experiments.}
#'   \item{Slope.1uM.Median}{Estimated slope for chemcial concentration decrease
#' in the 1 uM hepatic clearance assay.}
#'   \item{Slope.10uM.Median}{Estimated slope for chemcial concentration decrease
#' in the 10 uM hepatic clearance assay.}
#'   \item{Clint.1uM.Median}{Median of Bayesian credible interval for intrinsic 
#' hepatic clearance at 1 uM initital chemical concentration (uL/min/million hepatocytes)]}
#'   \item{Clint.1uM.Low95th}{Lower 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance at 1 uM initital chemical 
#' concentration (uL/min/million hepatocytes)}        
#'   \item{Clint.1uM.High95th}{Uppper 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance at 1 uM initital chemical 
#' concentration(uL/min/million hepatocytes)}
#'   \item{Clint.10uM.Median}{Median of Bayesian credible interval for intrinsic 
#' hepatic clearance at 10 uM initital chemical concentration (uL/min/million hepatocytes)]}
#'   \item{Clint.10uM.Low95th}{Lower 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance at 10 uM initital chemical 
#' concentration (uL/min/million hepatocytes)}        
#'   \item{Clint.10uM.High95th}{Uppper 95th percentile of Bayesian credible 
#' interval for intrinsic hepatic clearance at 10 uM initital chemical 
#' concentration(uL/min/million hepatocytes)}
#'   \item{Clint.1uM.Point}{Point estimate of intrinsic hepatic clearance 
#' (uL/min/million hepatocytes) for 1 uM initial chemical concentration}
#'   \item{Clint.10uM.Point}{Point estimate of intrinsic hepatic clearance 
#' (uL/min/million hepatocytes) for 10 uM initial chemical concentration}
#'   \item{fit}{Classification of clearance observed}                         
#'   \item{SMILES}{Simplified Molecular-Input Line-Entry System structure 
#' description}
#' }
#' @source Wambaugh et al. (submitted)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#'@references Wambaugh et al. "Assessing Toxicokinetic Uncertainty and 
#' Variability in Risk Prioritization", submitted.
"wambaugh2019.raw"

#' NHANES Chemical Intake Rates for chemicals in Wambaugh et al. (submitted))
#'
#' These data are a subset of the Bayesian inferrences reported by Ring et al.
#' (2017) from the U.S. Centers for Disease Control and Prevention (CDC)
#' National Health and Nutrition Examination Survey (NHANES). They reflect the
#' populaton median intake rate (mg/kg body weight/day), with uncertainty.
#'
#' @format A data frame with 20 rows and 4 variables:
#' describe{
#'   \item{lP}{The median of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.min}{The lower 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.max}{The upper 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{CASRN}{The Chemical Abstracts Service Registry Number}
#' }
#' @source Wambaugh et al. (submitted)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#' evironmental chemicals by simulating toxicokinetic variability." Environment 
#' international 106 (2017): 105-118
"wambaugh2019.nhanes"

#' ExpoCast SEEM3 Consensus Exposure Model Predictions for Chemical Intake Rates
#'
#' These data are a subset of the Bayesian inferrences reported by Ring et al.
#' (2019) for a consensus model of twelve exposue predictors. The predictors 
#' were calibrated based upon their ability to predict intake rates inferred
# 'from the U.S. Centers for Disease Control and Prevention (CDC)
#' National Health and Nutrition Examination Survey (NHANES). They reflect the
#' populaton median intake rate (mg/kg body weight/day), with uncertainty.
#'
#' @format A data frame with 385 rows and 38 variables:
#' @source Wambaugh et al. (submitted)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#'@references Ring, Caroline L., et al. "Consensus modeling of median chemical 
#' intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
"wambaugh2019.seem3"

#' Subset of the ToxCast and Tox21 Bioactivites Observed with HTS
#'
#' These data are all ToxCast and Tox21 high throughput screening (HTS) assay
#' results for the subset of chemicals appearing in Wambaugh et al. (2019). The
#' values are NA if no systematic concentration-response was observed, otherwise
#' the log base ten uM concentration of the activity concentration at cutoff (ACC)
#' is given. Each column correspons to a different in vitro HTS assay. 
#'
#' @format A data frame with 489 rows and 1197 variables:
#' @source \url{ftp://newftp.epa.gov/COMPTOX/High_Throughput_Screening_Data/Previous_Data/ToxCast_Data_Release_Oct_2015/}
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#'@references Wambaugh et al. "Assessing Toxicokinetic Uncertainty and 
#' Variability in Risk Prioritization", submitted.
"wambaugh2019.Tox21"

