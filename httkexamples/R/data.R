#This file is used by roxygen2 to generate man files (documentation) for data
#sets included in the package.



#' DRUGS|NORMAN: Pharmaceutical List with EU, Swiss, US Consumption Data
#'
#' SWISSPHARMA is a list of pharmaceuticals with consumption data from
#' Switzerland, France, Germany and the USA, used for a suspect
#' screening/exposure modelling approach described in
#' Singer et al 2016, (\doi{10.1021/acs.est.5b03332}). The original data is
#' available on the NORMAN Suspect List Exchange.
#'
#'@source \url{https://comptox.epa.gov/dashboard/chemical_lists/swisspharma}
#'@keywords data
#'
#' @references 
#' \insertRef{singer2016rapid}{httkexamples}
#' 
#' \insertRef{wambaugh2019assessing}{httkexamples}
#' @importFrom Rdpack reprompt
#' @import knitr
#' @importFrom rmarkdown html_vignette
#' @import httk
"pharma"

#' in vitro Toxicokinetic Data from Wambaugh et al. (2019)
#'
#' These data are the new HTTK in vitro data for chemicals reported in Wambaugh
#' et al. (2019) (\doi{10.1093/toxsci/kfz205}). They
#' are the processed values used to make the figures in that manuscript.
#' These data summarize the results of Bayesian analysis of the in vitro
#' toxicokinetic experiments conducted by Cyprotex to characterize fraction
#' unbound in the presence of pooled human plasma protein and the intrinsic
#' hepatic clearance of the chemical by pooled human hepatocytes.
#'
#' @format A data frame with 496 rows and 17 variables:
#' \describe{
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
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019"


#' NHANES Chemical Intake Rates for chemicals in Wambaugh et al. (2019)
#'
#' These data are a subset of the Bayesian inferences reported by Ring et al.
#' (2017) (\doi{10.1016/j.envint.2017.06.004}) from the U.S. Centers for Disease Control and Prevention (CDC)
#' National Health and Nutrition Examination Survey (NHANES). They reflect the
#' population median intake rate (mg/kg body weight/day), with uncertainty.
#'
#' @format A data frame with 20 rows and 4 variables:
#' \describe{
#'   \item{lP}{The median of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.min}{The lower 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.max}{The upper 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{CASRN}{The Chemical Abstracts Service Registry Number}
#' }
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{ring2017identifying}{httkexamples}
#' 
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019.nhanes"

#' ExpoCast SEEM3 Consensus Exposure Model Predictions for Chemical Intake Rates
#'
#' These data are a subset of the Bayesian inferences reported by Ring et al.
#' (2019) (\doi{10.1021/acs.est.8b04056}) for a consensus model of twelve exposure predictors. The predictors
#' were calibrated based upon their ability to predict intake rates inferred
# 'from the U.S. Centers for Disease Control and Prevention (CDC)
#' National Health and Nutrition Examination Survey (NHANES). They reflect the
#' population median intake rate (mg/kg body weight/day), with uncertainty.
#'
#' @format A data frame with 385 rows and 38 variables:
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{Ring2017SEEM}{httkexamples}
#'
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019.seem3"

#' Tox21 2015 Active Hit Calls (EPA)
#'
#' The ToxCast and Tox21 research programs employ batteries of high-throughput
#' assays to assess chemical bioactivity in vitro. Not every chemical is tested
#' through every assay. Most assays are conducted in concentration response,
#' and each corresponding assay endpoint is analyzed statistically to determine
#' if there is a concentration-dependent response or "hit" using the ToxCast
#' Pipeline.  Most assay endpoint-chemical combinations are non-responsive.
#' Here, only the hits are treated as potential indicators of bioactivity. This
#' bioactivity does not have a direct toxicological interpretation. The October
#' 2015 release (invitrodb_v2) of the ToxCast and Tox21 data were used for this
#' analysis. This object contains just the chemicals in Wambaugh et al. (2019)
#' and only the quantiles across all assays for the ACC.
#'
#' @name wambaugh2019.tox21
#' @docType data
#' @format A data.table with 401 rows and 6 columns
#' @author John Wambaugh
#' @references 
#' \insertRef{kavlock2012update}{httkexamples}
#' 
#' \insertRef{tice2013improving}{httkexamples}
#'
#' \insertRef{richard2016toxcast}{httkexamples}
#'
#' \insertRef{filer2017tcpl}{httkexamples}
#' 
#' \insertRef{wambaugh2019assessing}{httkexamples}
#'
#' @keywords data
"wambaugh2019.tox21"



#' Howgate et al. (2006)
#'
#' This data set is only used in Vignette 5.
#'
#' @docType data
#' @format A data.table containing 24 rows and 11 columns.
#' @keywords data
#' @author Caroline Ring
#' @references
#' Howgate, E. M., et al. "Prediction of in vivo drug clearance from in vitro
#' data. I: impact of inter-individual variability." Xenobiotica 36.6 (2006):
#' 473-497.
"howgate"


#' Johnson et al. (2006)
#'
#' This data set is only used in Vignette 5.
#'
#' @docType data
#'
#' @format A data.table containing 60 rows and 11 columns.
#'
#' @author Caroline Ring
#' @references
#' Johnson, Trevor N., Amin Rostami-Hodjegan, and Geoffrey T. Tucker.
#' "Prediction of the clearance of eleven drugs and associated variability in
#' neonates, infants and children." Clinical pharmacokinetics 45.9 (2006):
#' 931-956.
#'
#' @keywords data
"johnson"


#' Published Pharmacokinetic Parameters from Obach et al. 2008
#'
#' This data set is used in Vignette 4 for steady state concentration.
#'
#'
#' @docType data
#' @format A data.frame containing 670 rows and 8 columns.
#' @references Obach, R. Scott, Franco Lombardo, and Nigel J. Waters. "Trend
#' analysis of a database of intravenous pharmacokinetic parameters in humans
#' for 670 drug compounds." Drug Metabolism and Disposition 36.7 (2008):
#' 1385-1405.
#' @keywords data
"Obach2008"


#' NHANES Exposure Data
#'
#' This data set is only used in Vignette 6.
#'
#' @docType data
#'
#' @format A data.table containing 1060 rows and 5 columns.
#'
#' @author Caroline Ring
#'
#' @references
#' Wambaugh, John F., et al. "High throughput heuristics for prioritizing human
#' exposure to environmental chemicals." Environmental science & technology
#' 48.21 (2014): 12760-12767.
#'
#' @keywords data
"onlyp"



#' Partition Coefficient Data
#'
#' Measured rat in vivo partition coefficients and data for predicting them.
#'
#'
#' @docType data
#' @format A data.frame.
#' @author Jimena Davis and Robert Pearce
#' @references
#' \insertRef{schmitt2008general}{httkexamples}
#'
#' \insertRef{schmitt2008corrigendum}{httkexamples}
#'
#' Poulin, P. and F.P. Theil, A priori prediction of tissue: plasma partition
#' coefficients of drugs to facilitate the use of physiologically based
#' pharmacokinetic models in drug discovery. Journal of pharmaceutical
#' sciences, 2000. 89(1): p. 16-35.
#'
#' Rodgers, T. and M. Rowland, Physiologically based pharmacokinetic modelling
#' 2: predicting the tissue distribution of acids, very weak bases, neutrals
#' and zwitterions. Journal of pharmaceutical sciences, 2006. 95(6): p.
#' 1238-1257.
#'
#' Rodgers, T., D. Leahy, and M. Rowland, Physiologically based pharmacokinetic
#' modeling 1: predicting the tissue distribution of moderate-to-strong bases.
#' Journal of pharmaceutical sciences, 2005. 94(6): p. 1259-1276.
#'
#' Rodgers, T., D. Leahy, and M. Rowland, Tissue distribution of basic drugs:
#' Accounting for enantiomeric, compound and regional differences amongst
#' beta-blocking drugs in rat. Journal of pharmaceutical sciences, 2005. 94(6):
#' p. 1237-1248.
#'
#' Gueorguieva, I., et al., Development of a whole body physiologically based
#' model to characterise the pharmacokinetics of benzodiazepines. 1: Estimation
#' of rat tissue-plasma partition ratios. Journal of pharmacokinetics and
#' pharmacodynamics, 2004. 31(4): p. 269-298.
#'
#' Poulin, P., K. Schoenlein, and F.P. Theil, Prediction of adipose tissue:
#' plasma partition coefficients for structurally unrelated drugs. Journal of
#' pharmaceutical sciences, 2001. 90(4): p. 436-447.
#'
#' Bjorkman, S., Prediction of the volume of distribution of a drug: which
#' tissue-plasma partition coefficients are needed? Journal of pharmacy and
#' pharmacology, 2002. 54(9): p. 1237-1245.
#'
#' \insertRef{yun2013correlation}{httkexamples}
#'
#' Uchimura, T., et al., Prediction of human blood-to-plasma drug concentration
#' ratio. Biopharmaceutics & drug disposition, 2010. 31(5-6): p. 286-297.
#' @keywords data
"pc.data"

#' Published toxicokinetic predictions based on in vitro data from Wetmore et
#' al. 2012.
#'
#' This data set overlaps with Wetmore.data and is used only in Vignette 4 for
#' steady state concentration.
#'
#'
#' @docType data
#' @format A data.frame containing 13 rows and 15 columns.
#' @references 
#' \insertRef{wetmore2012integration}{httkexamples}
#' @keywords data
"Wetmore2012"

#' Metabolism data involved in Linakis et al. 2020 (\doi{10.1038/s41370-020-0238-y}) vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#'
#' @source Matt Linakis
#' @references
#' \insertRef{linakis2020development}{httkexamples}
#' @keywords data
"metabolism_data_Linakis2020"

#' Concentration data involved in Linakis 2020 vignette analysis.
#'
#' These rat and human TK concentration vs. time (CvT) data are drawn from
#' the CvTdb (Sayre et el., 2020, \doi{10.1038/s41597-020-0455-1}). Concentrations have all been converted to
#' the units of uM. All data are from inhalation studies.
#'
#' Abbreviations used for sampling matrix:
#' BL : blood
#' EEB : end-exhaled breath
#' MEB : mixed exhaled breath
#' VBL : venous blood
#' ABL : arterial blood
#' EB : unspecified exhaled breath sample (assumed to be EEB)
#' PL: plasma
#' +W with work/exercise
#'
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#'  PREFERRED_NAME        \tab Substance preferred name \cr                      
#'  DTXSID     \tab Identifier for CompTox Chemical Dashboard \cr
#'  CASRN  \tab Chemical abstracts service registration number\cr
#'  AVERAGE_MASS  \tab Substance molecular weight g/mol \cr
#'   DOSE DOSE_U   \tab Inhalation exposure concentration in parts per million \cr  
#'  EXP_LENGTH   \tab Duration of inhalation exposur \cr
#'   TIME  \tab  Measurment time \cr          
#'  TIME_U   \tab  Time units for all times reported \cr 
#'  CONC_SPECIES  \tab Species for study \cr 
#'  SAMPLING_MATRIX   \tab Matrix analyzed \cr 
#'  SOURCE_CVT   \tab Data source identifier within CvTdb \cr 
#'  ORIG_CONC_U   \tab Original reported units for concentration \cr 
#'  CONCENTRATION   \tab Analyte concentration in uM units\cr 
#' }
#'
#' @docType data
#' @format A data.frame containing 2142 rows and 16 columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' \insertRef{sayre2020database}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"concentration_data_Linakis2020"

#' Supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"supptab1_Linakis2020"

#' More supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"supptab2_Linakis2020"

#' Literature In Vivo Data on Doses Causing Neurological Effects
#'
#' Studies were selected from Table 1 in Mundy et al., 2015, as
#' the studies in that publication were cited as examples of
#' compounds with evidence for developmental neurotoxicity. There
#' were sufficient in vitro toxicokinetic data available for this
#' package for only 6 of the 42 chemicals.
#'
#' @docType data
#'
#' @format A data.frame containing 14 rows and 16 columns.
#'
#' @author Timothy J. Shafer
#'
#' @references
#' Frank, Christopher L., et al. "Defining toxicological tipping points
#' in neuronal network development." Toxicology and Applied
#' Pharmacology 354 (2018): 81-93.
#'
#' Mundy, William R., et al. "Expanding the test set: Chemicals with
#' potential to disrupt mammalian brain development." Neurotoxicology
#' and Teratology 52 (2015): 25-35.
#'
#' @keywords data
"Frank2018invivo"


#' Aylward et al. 2014
#' 
#' Aylward et al. (2014) compiled measurements of the ratio of maternal to fetal 
#' cord blood chemical concentrations at birth for a range of chemicals with 
#' environmental routes of exposure, including bromodiphenyl ethers, fluorinated 
#' compounds, organochlorine pesticides, polyaromatic hydrocarbons, tobacco smoke 
#' components, and vitamins.
#' 
#' @name aylward2014
#' @aliases Aylward2014
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Aylward2014matfet}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"aylward2014"

#' AUCs for Pregnant and Non-Pregnant Women
#' 
#' Dallmann et al. (2018) includes compiled literature descriptions of 
#' toxicokinetic summary statistics, including time-integrated plasma 
#' concentrations (area under the curve or AUC) for drugs administered to a 
#' sample of subjects including both pregnant and non-pregnant women. The 
#' circumstances of the dosing varied slightly between drugs and are summarized 
#' in the table.
#' 
#' @name pregnonpregaucs
#' @aliases pregnonpregaucs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{dallmann2018pregpbtk}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"pregnonpregaucs"  

#' Partition Coefficients from PK-Sim
#' 
#' Dallmann et al. (2018) made use of PK-Sim to predict chemical- and tissue-
#' specific partition coefficients. The methods include both the default
#' PK-Sim approach and PK-Sim Standard and Rodgers & Rowland (2006).
#' 
#' @name pksim.pcs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{dallmann2018pregpbtk}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"pksim.pcs"  

#' Fetal Partition Coefficients
#' 
#' Partition coefficients were measured for tissues, including placenta, in 
#' vitro by Csanady et al. (2002) for Bisphenol A and Diadzen. Curley et al. 
#' (1969) measured the concentration of a variety of pesticides in the cord 
#' blood of newborns and in the tissues of infants that were stillborn. 
#' 
#' Three of the chemicals studied by Curley et al. (1969) were modeled by 
#' Weijs et al. (2013) using the same partition coefficients for mother and 
#' fetus. The values used represented "prior knowledge" summarizing the 
#' available literature. 
#' 
#' @name fetalpcs
#' @aliases fetalPCs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Csanady2002fetalpc}{httkexamples}
#' \insertRef{Curley1969fetalpc}{httkexamples}
#' \insertRef{Weijs2013fetalpc}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"fetalpcs"

#' Wang et al. 2018
#' Wang et al. (2018) screened the blood of 75 pregnant women for the presence 
#' of environmental organic acids (EOAs) and identified mass spectral features 
#' corresponding to 453 chemical formulae of which 48 could be mapped to likely 
#' structures. Of the 48 with tentative structures the identity of six were 
#' confirmed with available chemical standards. 
#' @name wang2018
#' @aliases Wang2018
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Wang2018matbloodnta}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"wang2018"

#' ToxCast thyroid-related bioactivity data 
#'
#' Truong et al. 2025 uses ToxCast data for 4 thyroid-related assay endpoints
#' concerning inhibition of deiodinases ("DIO1", "DIO2", "DIO3", and "IYD") and 
#' identified 120 priority chemicals with activity for at least one deiodinase. 
#' These 120 chemicals were curated after assessment for target selectivity and 
#' assay interference. 
#'
#' The AC50s (in uM) for each of the 120 chemicals were retrieved from ToxCast 
#' invitrodb v3.5 and are used in the "Full Human Gestational IVIVE" vignette.
#' 
#' @name thyroid.ac50s
#' @docType data
#' @format data.table and data.frame
#' @keywords data
#' @references 
#' \insertRef{truong2025fullpregnancy}{httkexamples}
"thyroid.ac50s"

#' SEEM3 Example Data for Truong et al. 2025 
#' 
#' We can grab SEEM daily intake rate predictions already in RData format from
#' https://github.com/HumanExposure/SEEM3RPackage/blob/main/scripts/
#' Download the file chem.preds-2018-11-28.RData
#' 
#' We do not have the space to distribute all the SEEM predictions within
#' this R package, but we can give you our "Full Human Gestational IVIVE" example chemicals.
#' @name truong25.seem3
#' @docType data
#' @format data.table and data.frame
#' @keywords data
#' @references 
#' \insertRef{truong2025fullpregnancy}{httkexamples}
#'
#' \insertRef{Ring2017SEEM}{httkexamples}
"truong25.seem3"

#' Literature Measurements of In Vitro Cellular and Nominal Concentration 
#' 
#' @author Meredith Scherer
#' @name Scherer2025.IVD
#' @docType data
#' @format data.table and data.frame
'Scherer2025.IVD' 

#' Dimitrijevic et al. (2022)In Vitro Cellular and Nominal Concentration 
#' 
#' @author Jon Arnot
#' @name Dimitrijevic.IVD
#' @docType data
#' @format data.table and data.frame
#' @references
#' \insertRef{dimitrijevic2022toward}{httkexamples}
'Dimitrijevic.IVD'


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
#' @references 
#' \insertRef{wambaugh2019assessing}{httkexamples}
"pharma"

#' in vitro Toxicokinetic Data from Wambaugh et al. (2019)
#'
#' These data are the new HTTK in vitro data for chemicals reported in Wambaugh
#' et al. (2019) They
#' are the processed values used to make the figures in that manuscript.
#' These data summarize the results of Bayesian analysis of the in vitro
#' toxicokinetic experiments conducted by Cyprotex to characterize fraction
#' unbound in the presence of pooled human plasma protein and the intrnsic
#' hepatic clearance of the chemical by pooled human hepatocytes.
#'
#' @format A data frame with 496 rows and 17 variables:
#' \describe{
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
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019"

#' Raw Bayesian in vitro Toxicokinetic Data Analysis from Wambaugh et al. (2019)
#'
#' These data are the new HTTK in vitro data for chemicals reported in Wambaugh
#' et al. (2019) They
#' are the output of different Bayesian models evaluated to compare using a
#' single protein concentration vs. the new three concentration titration
#' protocol. These data summarize the results of Bayesian analysis of the in vitro
#' toxicokinetic experiments conducted by Cyprotex to characterize fraction
#' unbound in the presence of pooled human plasma protein and the intrnsic
#' hepatic clearance of the chemical by pooled human hepatocytes.
#' This file includes replicates (diferent CompoundName id's but same chemical')
#'
#' @format A data frame with 530 rows and 28 variables:
#' \describe{
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
#'   \item{Decreases.Prob}{Probability that the chemical concentration decreased
#' systematiclally during hepatic clearance assay.}
#'   \item{Saturates.Prob}{Probability that the rate of chemical concentration
#' decrease varied between the 1 and 10 uM hepatic clearance experiments.}
#'   \item{Slope.1uM.Median}{Estimated slope for chemcial concentration decrease
#' in the 1 uM hepatic clearance assay.}
#'   \item{Slope.10uM.Median}{Estimated slope for chemcial concentration decrease
#' in the 10 uM hepatic clearance assay.}
#'   \item{CLint.1uM.Median}{Median of Bayesian credible interval for intrinsic
#' hepatic clearance at 1 uM initital chemical concentration (uL/min/million hepatocytes)]}
#'   \item{CLint.1uM.Low95th}{Lower 95th percentile of Bayesian credible
#' interval for intrinsic hepatic clearance at 1 uM initital chemical
#' concentration (uL/min/million hepatocytes)}
#'   \item{CLint.1uM.High95th}{Uppper 95th percentile of Bayesian credible
#' interval for intrinsic hepatic clearance at 1 uM initital chemical
#' concentration(uL/min/million hepatocytes)}
#'   \item{CLint.10uM.Median}{Median of Bayesian credible interval for intrinsic
#' hepatic clearance at 10 uM initital chemical concentration (uL/min/million hepatocytes)]}
#'   \item{CLint.10uM.Low95th}{Lower 95th percentile of Bayesian credible
#' interval for intrinsic hepatic clearance at 10 uM initital chemical
#' concentration (uL/min/million hepatocytes)}
#'   \item{CLint.10uM.High95th}{Uppper 95th percentile of Bayesian credible
#' interval for intrinsic hepatic clearance at 10 uM initital chemical
#' concentration(uL/min/million hepatocytes)}
#'   \item{CLint.1uM.Point}{Point estimate of intrinsic hepatic clearance
#' (uL/min/million hepatocytes) for 1 uM initial chemical concentration}
#'   \item{CLint.10uM.Point}{Point estimate of intrinsic hepatic clearance
#' (uL/min/million hepatocytes) for 10 uM initial chemical concentration}
#'   \item{Fit}{Classification of clearance observed}
#'   \item{SMILES}{Simplified Molecular-Input Line-Entry System structure
#' description}
#' }
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019.raw"

#' NHANES Chemical Intake Rates for chemicals in Wambaugh et al. (2019)
#'
#' These data are a subset of the Bayesian inferrences reported by Ring et al.
#' (2017) from the U.S. Centers for Disease Control and Prevention (CDC)
#' National Health and Nutrition Examination Survey (NHANES). They reflect the
#' populaton median intake rate (mg/kg body weight/day), with uncertainty.
#'
#' @format A data frame with 20 rows and 4 variables:
#' \describe{
#'   \item{lP}{The median of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.min}{The lower 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{lP.max}{The upper 95th percentile of the Bayesian credible interval for median population
#' intake rate (mg/kg bodyweight/day)}
#'   \item{CASRN}{The Chemical Abstracts Service Registry Number}
#' }
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{ring2017identifying}{httkexamples}
#' 
#' \insertRef{wambaugh2019assessing}{httkexamples}
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
#' @source Wambaugh et al. (2019)
#'
#'@keywords data
#'
#'@author John Wambaugh
#'
#' @references 
#' \insertRef{Ring2017SEEM}{httkexamples}
#'
#' \insertRef{wambaugh2019assessing}{httkexamples}
"wambaugh2019.seem3"


#' Tox21 2015 Active Hit Calls (EPA)
#'
#' The ToxCast and Tox21 research programs employ batteries of high-throughput
#' assays to assess chemical bioactivity in vitro. Not every chemical is tested
#' through every assay. Most assays are conducted in concentration response,
#' and each corresponding assay endpoint is analyzed statistically to determine
#' if there is a concentration-dependent response or "hit" using the ToxCast
#' Pipeline.  Most assay endpoint-chemical combinations are non-responsive.
#' Here, only the hits are treated as potential indicators of bioactivity. This
#' bioactivity does not have a direct toxicological interpretation. The October
#' 2015 release (invitrodb_v2) of the ToxCast and Tox21 data were used for this
#' analysis. This object contains just the chemicals in Wambaugh et al. (2019)
#' and only the quantiles across all assays for the ACC.
#'
#' @name wambaugh2019.tox21
#' @docType data
#' @format A data.table with 401 rows and 6 columns
#' @author John Wambaugh
#' @references Kavlock, Robert, et al. "Update on EPA's ToxCast program:
#' providing high-throughput decision support tools for chemical risk
#' management." Chemical research in toxicology 25.7 (2012): 1287-1302.
#'
#' Tice, Raymond R., et al. "Improving the human hazard characterization of
#' chemicals: a Tox21 update." Environmental health perspectives 121.7 (2013):
#' 756-765.
#'
#' Richard, Ann M., et al. "ToxCast chemical landscape: paving the road to 21st
#' century toxicology." Chemical research in toxicology 29.8 (2016): 1225-1251.
#'
#' Filer, Dayne L., et al. "tcpl: the ToxCast pipeline for high-throughput
#' screening data." Bioinformatics 33.4 (2016): 618-620.
#' 
#' Wambaugh, John F., et al. "Assessing Toxicokinetic Uncertainty and 
#' Variability in Risk Prioritization." Toxicological Sciences 172.2 (2019): 
#' 235-251.
#'
#' @keywords data
"wambaugh2019.tox21"



#' Howgate 2006
#'
#' This data set is only used in Vignette 5.
#'
#' @docType data
#' @format A data.table containing 24 rows and 11 columns.
#' @keywords data
#' @author Caroline Ring
#' @references
#' Howgate, E. M., et al. "Prediction of in vivo drug clearance from in vitro
#' data. I: impact of inter-individual variability." Xenobiotica 36.6 (2006):
#' 473-497.
"howgate"


#' Johnson 2006
#'
#' This data set is only used in Vignette 5.
#'
#' @docType data
#'
#' @format A data.table containing 60 rows and 11 columns.
#'
#' @author Caroline Ring
#' @references
#' Johnson, Trevor N., Amin Rostami-Hodjegan, and Geoffrey T. Tucker.
#' "Prediction of the clearance of eleven drugs and associated variability in
#' neonates, infants and children." Clinical pharmacokinetics 45.9 (2006):
#' 931-956.
#'
#' @keywords data
"johnson"


#' Published Pharmacokinetic Parameters from Obach et al. 2008
#'
#' This data set is used in Vignette 4 for steady state concentration.
#'
#'
#' @docType data
#' @format A data.frame containing 670 rows and 8 columns.
#' @references Obach, R. Scott, Franco Lombardo, and Nigel J. Waters. "Trend
#' analysis of a database of intravenous pharmacokinetic parameters in humans
#' for 670 drug compounds." Drug Metabolism and Disposition 36.7 (2008):
#' 1385-1405.
#' @keywords data
"Obach2008"


#' NHANES Exposure Data
#'
#' This data set is only used in Vignette 6.
#'
#' @docType data
#'
#' @format A data.table containing 1060 rows and 5 columns.
#'
#' @author Caroline Ring
#'
#' @references
#' Wambaugh, John F., et al. "High throughput heuristics for prioritizing human
#' exposure to environmental chemicals." Environmental science & technology
#' 48.21 (2014): 12760-12767.
#'
#' @keywords data
"onlyp"



#' Partition Coefficient Data
#'
#' Measured rat in vivo partition coefficients and data for predicting them.
#'
#'
#' @docType data
#' @format A data.frame.
#' @author Jimena Davis and Robert Pearce
#' @references
#' \insertRef{schmitt2008general}{httkexamples}
#'
#' \insertRef{schmitt2008corrigendum}{httkexamples}
#'
#' Poulin, P. and F.P. Theil, A priori prediction of tissue: plasma partition
#' coefficients of drugs to facilitate the use of physiologically based
#' pharmacokinetic models in drug discovery. Journal of pharmaceutical
#' sciences, 2000. 89(1): p. 16-35.
#'
#' Rodgers, T. and M. Rowland, Physiologically based pharmacokinetic modelling
#' 2: predicting the tissue distribution of acids, very weak bases, neutrals
#' and zwitterions. Journal of pharmaceutical sciences, 2006. 95(6): p.
#' 1238-1257.
#'
#' Rodgers, T., D. Leahy, and M. Rowland, Physiologically based pharmacokinetic
#' modeling 1: predicting the tissue distribution of moderate-to-strong bases.
#' Journal of pharmaceutical sciences, 2005. 94(6): p. 1259-1276.
#'
#' Rodgers, T., D. Leahy, and M. Rowland, Tissue distribution of basic drugs:
#' Accounting for enantiomeric, compound and regional differences amongst
#' beta-blocking drugs in rat. Journal of pharmaceutical sciences, 2005. 94(6):
#' p. 1237-1248.
#'
#' Gueorguieva, I., et al., Development of a whole body physiologically based
#' model to characterise the pharmacokinetics of benzodiazepines. 1: Estimation
#' of rat tissue-plasma partition ratios. Journal of pharmacokinetics and
#' pharmacodynamics, 2004. 31(4): p. 269-298.
#'
#' Poulin, P., K. Schoenlein, and F.P. Theil, Prediction of adipose tissue:
#' plasma partition coefficients for structurally unrelated drugs. Journal of
#' pharmaceutical sciences, 2001. 90(4): p. 436-447.
#'
#' Bjorkman, S., Prediction of the volume of distribution of a drug: which
#' tissue-plasma partition coefficients are needed? Journal of pharmacy and
#' pharmacology, 2002. 54(9): p. 1237-1245.
#'
#' \insertRef{yun2013correlation}{httkexamples}
#'
#' Uchimura, T., et al., Prediction of human blood-to-plasma drug concentration
#' ratio. Biopharmaceutics & drug disposition, 2010. 31(5-6): p. 286-297.
#' @keywords data
"pc.data"

#' Published toxicokinetic predictions based on in vitro data from Wetmore et
#' al. 2012.
#'
#' This data set overlaps with Wetmore.data and is used only in Vignette 4 for
#' steady state concentration.
#'
#'
#' @docType data
#' @format A data.frame containing 13 rows and 15 columns.
#' @references 
#' \insertRef{wetmore2012integration}{httkexamples}
#' @keywords data
"Wetmore2012"

#' Metabolism data involved in Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#'
#' @source Matt Linakis
#' @references
#' \insertRef{linakis2020development}{httkexamples}
#' @keywords data
"metabolism_data_Linakis2020"

#' Concentration data involved in Linakis 2020 vignette analysis.
#'
#' These rat and human TK concentration vs. time (CvT) data are drawn from
#' the CvTdb (Sayre et el., 2020). Concentrations have all been converted to
#' the units of uM. All data are from inhalation studies.
#'
#' Abbreviations used for sampling matrix:
#' BL : blood
#' EEB : end-exhaled breath
#' MEB : mixed exhaled breath
#' VBL : venous blood
#' ABL : arterial blood
#' EB : unspecified exhaled breath sample (assumed to be EEB)
#' PL: plasma
#' +W with work/exercise
#'
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#'  PREFERRED_NAME        \tab Substance preferred name \cr                      
#'  DTXSID     \tab Identifier for CompTox Chemical Dashboard \cr
#'  CASRN  \tab Chemical abstracts service registration number\cr
#'  AVERAGE_MASS  \tab Substance molecular weight g/mol \cr
#'   DOSE DOSE_U   \tab Inhalation exposure concentration in parts per million \cr  
#'  EXP_LENGTH   \tab Duration of inhalation exposur \cr
#'   TIME  \tab  Measurment time \cr          
#'  TIME_U   \tab  Time units for all times reported \cr 
#'  CONC_SPECIES  \tab Species for study \cr 
#'  SAMPLING_MATRIX   \tab Matrix analyzed \cr 
#'  SOURCE_CVT   \tab Data source identifier within CvTdb \cr 
#'  ORIG_CONC_U   \tab Original reported units for concentration \cr 
#'  CONCENTRATION   \tab Analyte concentration in uM units\cr 
#' }
#'
#' @docType data
#' @format A data.frame containing 2142 rows and 16 columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' \insertRef{sayre2020database}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"concentration_data_Linakis2020"

#' Supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"supptab1_Linakis2020"

#' More supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references 
#' \insertRef{linakis2020development}{httkexamples}
#' @source Matt Linakis
#' @keywords data
"supptab2_Linakis2020"

#' Literature In Vivo Data on Doses Causing Neurological Effects
#'
#' Studies were selected from Table 1 in Mundy et al., 2015, as
#' the studies in that publication were cited as examples of
#' compounds with evidence for developmental neurotoxicity. There
#' were sufficient in vitro toxicokinetic data available for this
#' package for only 6 of the 42 chemicals.
#'
#' @docType data
#'
#' @format A data.frame containing 14 rows and 16 columns.
#'
#' @author Timothy J. Shafer
#'
#' @references
#' Frank, Christopher L., et al. "Defining toxicological tipping points
#' in neuronal network development." Toxicology and Applied
#' Pharmacology 354 (2018): 81-93.
#'
#' Mundy, William R., et al. "Expanding the test set: Chemicals with
#' potential to disrupt mammalian brain development." Neurotoxicology
#' and Teratology 52 (2015): 25-35.
#'
#' @keywords data
"Frank2018invivo"

#' Aylward et al. 2014
#' 
#' Aylward et al. (2014) compiled measurements of the ratio of maternal to fetal 
#' cord blood chemical concentrations at birth for a range of chemicals with 
#' environmental routes of exposure, including bromodiphenyl ethers, fluorinated 
#' compounds, organochlorine pesticides, polyaromatic hydrocarbons, tobacco smoke 
#' components, and vitamins.
#' 
#' @name aylward2014
#' @aliases Aylward2014
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Aylward2014matfet}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"aylward2014"

#' AUCs for Pregnant and Non-Pregnant Women
#' 
#' Dallmann et al. (2018) includes compiled literature descriptions of 
#' toxicokinetic summary statistics, including time-integrated plasma 
#' concentrations (area under the curve or AUC) for drugs administered to a 
#' sample of subjects including both pregnant and non-pregnant women. The 
#' circumstances of the dosing varied slightly between drugs and are summarized 
#' in the table.
#' 
#' @name pregnonpregaucs
#' @aliases pregnonpregaucs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{dallmann2018pregpbtk}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"pregnonpregaucs"  

#' Partition Coefficients from PK-Sim
#' 
#' Dallmann et al. (2018) made use of PK-Sim to predict chemical- and tissue-
#' specific partition coefficients. The methods include both the default
#' PK-Sim approach and PK-Sim Standard and Rodgers & Rowland (2006).
#' 
#' @name pksim.pcs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{dallmann2018pregpbtk}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"pksim.pcs"  

#' Fetal Partition Coefficients
#' 
#' Partition coefficients were measured for tissues, including placenta, in 
#' vitro by Csanady et al. (2002) for Bisphenol A and Diadzen. Curley et al. 
#' (1969) measured the concentration of a variety of pesticides in the cord 
#' blood of newborns and in the tissues of infants that were stillborn. 
#' 
#' Three of the chemicals studied by Curley et al. (1969) were modeled by 
#' Weijs et al. (2013) using the same partition coefficients for mother and 
#' fetus. The values used represented "prior knowledge" summarizing the 
#' available literature. 
#' 
#' @name fetalpcs
#' @aliases fetalPCs
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Csanady2002fetalpc}{httkexamples}
#' \insertRef{Curley1969fetalpc}{httkexamples}
#' \insertRef{Weijs2013fetalpc}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"fetalpcs"

#' Wang et al. 2018
#' Wang et al. (2018) (\doi{10.1289/EHP2920}) screened the blood of 75 pregnant women for the presence 
#' of environmental organic acids (EOAs) and identified mass spectral features 
#' corresponding to 453 chemical formulae of which 48 could be mapped to likely 
#' structures. Of the 48 with tentative structures the identity of six were 
#' confirmed with available chemical standards. 
#' @name wang2018
#' @aliases Wang2018
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{Wang2018matbloodnta}{httkexamples}
#' @source
#' \insertRef{kapraun2022fetalmodel}{httkexamples}
#' @keywords data
"wang2018"

#' Abraham et al. 2024
#' Abraham et al. (2024) (\doi{10.1016/j.envint.2024.109047}) determined the 
#' half-lives of 15 per- and 
#' polyfluoroalkyl substances in a single male volunteer.
#' @name abraham2024
#' @aliases Abraham2024
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{abraham2024kinetics}{httkexamples}
#' @source
#' Wambaugh et al., Applying High Throughput Toxicokinetics (HTTK) to Per- and
#' Polyfluoro Alkyl Substances (PFAS), submitted
#' @keywords data
"abraham2024"
       
#' Parameters for in vitro distribution analysis in Honda et al. (2019)
#' Honda et al. (2019) (\doi{10.1371/journal.pone.0217564}) used the
#' Armitage et al. (2014) (\doi{10.1021/es501955g}) mass-balance model to 
#' predict the impact of
#' in vitro partitioning on free chemical concentrations.
#' @name armitage_input
#' @aliases Armitage_input
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{armitage2014application}{httkexamples}
#' \insertRef{honda2019using}{httkexamples}
#' @source
#' \insertRef{honda2019using}{httkexamples}
#' @keywords data
"armitage_input"   

#' Non-volatile chemicals with ToxCast data
#' Meade et al. (submitted) performed in vitro-in vivo extrapolation for 
#' dermal exposures assuming 8 hours of exposure via hands submerged in a
#' liquid with 1 ppm of chemical. These were the chemicals analyzed.
#' @name dermal.nonvolatilechems
#' @docType data
#' @format data.frame
#' @references
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @source
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @keywords data
"dermal.nonvolatilechems"

#' Chemicals with ToxCast data for Meade et al. (submitted) chemicals
#' Meade et al. (submitted) performed in vitro-in vivo extrapolation for 
#' dermal exposures assuming 8 hours of exposure via hands submerged in a
#' liquid with 1 ppm of chemical. These are the ToxCast in vitro screening
#' data for those chemicals.
#' @name dermal.toxcast
#' @docType data
#' @format data.frame
#' @references
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @source
#' https://www.epa.gov/comptox-tools/exploring-toxcast-data
#' @keywords data
"dermal.toxcast"

#' Non-volatile chemicals with ToxCast data
#' Meade et al. (submitted) performed in vitro-in vivo extrapolation for 
#' dermal exposures assuming 8 hours of exposure via hands submerged in a
#' liquid with 1 ppm of chemical. These were the chemicals analyzed.
#' @name dermal.nonvolatilechems
#' @docType data
#' @format data.frame
#' @references
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @source
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @keywords data
"dermal.nonvolatilechems"

#' Toxicokinetic concentration vs. time (CvT) data for Meade et al. (submitted) chemicals
#' Meade et al. (submitted) evaluated a generic PBTK model for dermal exposure
#' using in vivo CvT data curated from the literature. These data will
#' eventually be incorporated in the the CvTdb (Sayre et al., 2020, \doi{10.1038/s41597-020-0455-1}).
#' @name dermalCvT2025
#' @docType data
#' @format data.frame
#' @references
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' \insertRef{sayre2020database}{httkexamples}
#' @source
#' \insertRef{sayre2020database}{httkexamples}
#' @keywords data
"dermalCvT2025"

#' Simulation outputs from Meade et al. (submitted)
#' Meade et al. (submitted) performed generic PBTK simulations for dermal
#' exposure under a variety of assumptions. Although the code to recreate these
#' simulations is provided, it is time-intensive. The 2025 outputs from the 
#' simulations are stored in this list of data.frames.
#' @name meade2025
#' @aliases Meade2025
#' @docType data
#' @format list
#' @references
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @source
#' Meade et al., Incorporating a dermal absorption
#' route into high throughput toxicokinetic modeling, submitted.
#' @keywords data
"meade2025"

#' Huh et al. 2011
#' Huh et al. (2011) (\doi{10.3109/00498254.2011.598582}) provided interspecies
#' allometric scaling parameters for whole body clearance for a a variety of 
#' pharmaceuticals. 
#' @name huh2011
#' @aliases Huh2011
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{huh2011interspecies}{httkexamples}
#' @source
#' Wambaugh et al., Applying High Throughput Toxicokinetics (HTTK) to Per- and
#' Polyfluoro Alkyl Substances (PFAS), submitted
#' @keywords data
"huh2011"

#' Wallis et al. 2023
#' Wallis et al. (2023) (\doi{10.1021/acs.est.2c08241}) estimated the human
#' toxicokinetic half-lives for a range of per- and poly-fluorinated alkyl 
#' substances (PFAS).
#' @name wallis2023
#' @aliases Wallis2023
#' @docType data
#' @format data.frame
#' @references
#' \insertRef{wallis2023estimation}{httkexamples}
#' @source
#' Wambaugh et al., Applying High Throughput Toxicokinetics (HTTK) to Per- and
#' Polyfluoro Alkyl Substances (PFAS), submitted
#' @keywords data
"wallis2023"