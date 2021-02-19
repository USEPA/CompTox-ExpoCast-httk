#This file is used by roxygen2 to generate man files (documentation) for data
#sets included in the package.


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

#' A timestamp of table creation
#'
#' The Tables.RData file is separately created as part of building a new
#' release of HTTK. This time stamp indicates the script used to build the file
#' and when it was run.
#'
#' @author John Wambaugh
"Tables.Rdata.stamp"

#' Reference for EPA Physico-Chemical Data
#'
#' The physico-chemical data in the chem.phys_and_invitro.data table are
#' obtained from EPA's Comptox Chemicals dashboard. This variable indicates
#' the date the Dashboard was accessed.
#' @source \url{https://comptox.epa.gov/dashboard}
#'
#' @author John Wambaugh
"EPA.ref"

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
#'@source \url{https://wwwn.cdc.gov/nchs/nhanes/Default.aspx}
#'
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
#'  \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}}
#'  \item{\code{P3},
#'  \code{P5}, \code{P10}, \code{P25}, \code{P50}, \code{P75}, \code{P85},
#'  \code{P90}, \code{P95}, and \code{P97}}{BMI percentiles}}
#'
#'
#'@source \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
#'
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
#'  \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}} \item{\code{P2.3},
#'  \code{P5}, \code{P10}, \code{P25}, \code{P50}, \code{P75}, \code{P90},
#'  \code{P95}, and \code{P97.7}}{weight percentiles} }
#'
#'@source
#'  \url{https://www.cdc.gov/growthcharts/who/girls_weight_head_circumference.htm}
#'  and
#'  \url{https://www.cdc.gov/growthcharts/who/boys_weight_head_circumference.htm}
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
#' @format A data frame / data table with 11 rows and 8 variables:
#' \describe{
#'   \item{sysID}{Identifier for each multi-well plate system}
#'   \item{well_desc}{Well description}
#'   \item{well_number}{Number of wells on plate}
#'   \item{area_bottom}{Area of well bottom in mm^2}
#'   \item{cell_yield}{Number of cells}
#'   \item{diam}{Diameter of well in mm}
#'   \item{v_total}{Total volume of well in uL or mm^3)}
#'   \item{v_working}{Working volume of well in uL or mm^3}
#' }
#'
#' @source \url{https://www.corning.com/catalog/cls/documents/application-notes/CLS-AN-209.pdf}
#'
#' @keywords data
#'
#' @author Greg Honda
#'
#' @references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol.
#'2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#' @references Honda, Gregory S., et al. "Using the Concordance of In Vitro and
#'In Vivo Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
"well_param"

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
#' @source \url{https://www.diamondse.info/}
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
#'@references Wambaugh et al. (2019) "Assessing Toxicokinetic Uncertainty and
#'Variability in Risk Prioritization", Toxicological Sciences, 172(2), 235-251.
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
#'@references Wambaugh et al. (2019) "Assessing Toxicokinetic Uncertainty and
#'Variability in Risk Prioritization", Toxicological Sciences, 172(2), 235-251.
"wambaugh2019"


#' Published toxicokinetic time course measurements
#'
#' This data set includes time and dose specific measurements of chemical
#' concentration in tissues taken from animals administered control doses of
#' the chemicals either orally or intravenously. This plasma concentration-time
#' data is from rat experiments reported in public sources. Toxicokinetic data
#' were retrieved from those studies by the Netherlands Organisation for
#' Applied Scientific Research (TNO) using curve stripping (TechDig v2).  This
#' data is provided for statistical analysis as in Wambaugh et al. 2018.
#'
#'
#' @docType data
#' @format A data.frame containing 597 rows and 13 columns.
#' @author Sieto Bosgra
#' @references Aanderud L, Bakke OM (1983). Pharmacokinetics of antipyrine,
#' paracetamol, and morphine in rat at 71 ATA. Undersea Biomed Res.
#' 10(3):193-201. PMID: 6636344
#'
#' Aasmoe L, Mathiesen M, Sager G (1999). Elimination of methoxyacetic acid and
#' ethoxyacetic acid in rat. Xenobiotica. 29(4):417-24. PMID: 10375010
#'
#' Ako RA. Pharmacokinetics/pharmacodynamics (PK/PD) of oral diethylstilbestrol
#' (DES) in recurrent prostate cancer patients and of oral dissolving film
#' (ODF)-DES in rats. PhD dissertation, College of Pharmacy, University of
#' Houston, USA, 2011.
#'
#' Anadon A, Martinez-Larranaga MR, Fernandez-Cruz ML, Diaz MJ, Fernandez MC,
#' Martinez MA (1996). Toxicokinetics of deltamethrin and its 4'-HO-metabolite
#' in the rat. Toxicol Appl Pharmacol. 141(1):8-16. PMID: 8917670
#'
#' Binkerd PE, Rowland JM, Nau H, Hendrickx AG (1988). Evaluation of valproic
#' acid (VPA) developmental toxicity and pharmacokinetics in Sprague-Dawley
#' rats. Fundam Appl Toxicol. 11(3):485-93. PMID: 3146521
#'
#' Boralli VB, Coelho EB, Cerqueira PM, Lanchote VL (2005). Stereoselective
#' analysis of metoprolol and its metabolites in rat plasma with application to
#' oxidative metabolism. J Chromatogr B Analyt Technol Biomed Life Sci.
#' 823(2):195-202. PMID: 16029965
#'
#' Chan MP, Morisawa S, Nakayama A, Kawamoto Y, Sugimoto M, Yoneda M (2005).
#' Toxicokinetics of 14C-endosulfan in male Sprague-Dawley rats following oral
#' administration of single or repeated doses. Environ Toxicol. 20(5):533-41.
#' PMID: 16161119
#'
#' Cruz L, Castaneda-Hernandez G, Flores-Murrieta FJ, Garcia-Lopez P,
#' Guizar-Sahagun G (2002). Alteration of phenacetin pharmacokinetics after
#' experimental spinal cord injury. Proc West Pharmacol Soc. 45:4-5. PMID:
#' 12434508
#'
#' Della Paschoa OE, Mandema JW, Voskuyl RA, Danhof M (1998).
#' Pharmacokinetic-pharmacodynamic modeling of the anticonvulsant and
#' electroencephalogram effects of phenytoin in rats. J Pharmacol Exp Ther.
#' 284(2):460-6. PMID: 9454785
#'
#' Du B, Li X, Yu Q, A Y, Chen C (2010). Pharmacokinetic comparison of orally
#' disintegrating, beta-cyclodextrin inclusion complex and conventional tablets
#' of nicardipine in rats. Life Sci J. 7(2):80-4.
#'
#' Farris FF, Dedrick RL, Allen PV, Smith JC (1993). Physiological model for
#' the pharmacokinetics of methyl mercury in the growing rat. Toxicol Appl
#' Pharmacol. 119(1):74-90. PMID: 8470126
#'
#' Hays SM, Elswick BA, Blumenthal GM, Welsch F, Conolly RB, Gargas ML (2000).
#' Development of a physiologically based pharmacokinetic model of
#' 2-methoxyethanol and 2-methoxyacetic acid disposition in pregnant rats.
#' Toxicol Appl Pharmacol. 163(1):67-74. PMID: 10662606
#'
#' Igari Y, Sugiyama Y, Awazu S, Hanano M (1982). Comparative physiologically
#' based pharmacokinetics of hexobarbital, phenobarbital and thiopental in the
#' rat. J Pharmacokinet Biopharm. 10(1):53-75. PMID: 7069578
#'
#' Ito K, Houston JB (2004). Comparison of the use of liver models for
#' predicting drug clearance using in vitro kinetic data from hepatic
#' microsomes and isolated hepatocytes. Pharm Res. 21(5):785-92. PMID: 15180335
#'
#' Jia L, Wong H, Wang Y, Garza M, Weitman SD (2003). Carbendazim: disposition,
#' cellular permeability, metabolite identification, and pharmacokinetic
#' comparison with its nanoparticle. J Pharm Sci. 92(1):161-72. PMID: 12486692
#'
#' Kawai R, Mathew D, Tanaka C, Rowland M (1998). Physiologically based
#' pharmacokinetics of cyclosporine A: extension to tissue distribution
#' kinetics in rats and scale-up to human. J Pharmacol Exp Ther. 287(2):457-68.
#' PMID: 9808668
#'
#' Kim YC, Kang HE, Lee MG (2008). Pharmacokinetics of phenytoin and its
#' metabolite, 4'-HPPH, after intravenous and oral administration of phenytoin
#' to diabetic rats induced by alloxan or streptozotocin. Biopharm Drug Dispos.
#' 29(1):51-61. PMID: 18022993
#'
#' Kobayashi S, Takai K, Iga T, Hanano M (1991). Pharmacokinetic analysis of
#' the disposition of valproate in pregnant rats. Drug Metab Dispos.
#' 19(5):972-6. PMID: 1686245
#'
#' Kotegawa T, Laurijssens BE, Von Moltke LL, Cotreau MM, Perloff MD,
#' Venkatakrishnan K, Warrington JS, Granda BW, Harmatz JS, Greenblatt DJ
#' (2002). In vitro, pharmacokinetic, and pharmacodynamic interactions of
#' ketoconazole and midazolam in the rat. J Pharmacol Exp Ther. 302(3):1228-37.
#' PMID: 12183684
#'
#' Krug AK, Kolde R, Gaspar JA, Rempel E, Balmer NV, Meganathan K, Vojnits K,
#' Baquie M, Waldmann T, Ensenat-Waser R, Jagtap S, Evans RM, Julien S,
#' Peterson H, Zagoura D, Kadereit S, Gerhard D, Sotiriadou I, Heke M,
#' Natarajan K, Henry M, Winkler J, Marchan R, Stoppini L, Bosgra S, Westerhout
#' J, Verwei M, Vilo J, Kortenkamp A, Hescheler J, Hothorn L, Bremer S, van
#' Thriel C, Krause KH, Hengstler JG, Rahnenfuhrer J, Leist M, Sachinidis A
#' (2013). Human embryonic stem cell-derived test systems for developmental
#' neurotoxicity: a transcriptomics approach. Arch Toxicol. 87(1):123-43. PMID:
#' 23179753
#'
#' Leon-Reyes MR, Castaneda-Hernandez G, Ortiz MI (2009). Pharmacokinetic of
#' diclofenac in the presence and absence of glibenclamide in the rat. J Pharm
#' Pharm Sci. 12(3):280-7. PMID: 20067705
#'
#' Nagata M, Hidaka M, Sekiya H, Kawano Y, Yamasaki K, Okumura M, Arimori K
#' (2007). Effects of pomegranate juice on human cytochrome P450 2C9 and
#' tolbutamide pharmacokinetics in rats. Drug Metab Dispos. 35(2):302-5. PMID:
#' 17132763
#'
#' Okiyama M, Ueno K, Ohmori S, Igarashi T, Kitagawa H (1988). Drug
#' interactions between imipramine and benzodiazepines in rats. J Pharm Sci.
#' 77(1):56-63. PMID: 2894451
#'
#' Pelissier-Alicot AL, Schreiber-Deturmeny E, Simon N, Gantenbein M,
#' Bruguerolle B (2002). Time-of-day dependent pharmacodynamic and
#' pharmacokinetic profiles of caffeine in rats. Naunyn Schmiedebergs Arch
#' Pharmacol. 365(4):318-25. PMID: 11919657
#'
#' Piersma AH, Bosgra S, van Duursen MB, Hermsen SA, Jonker LR, Kroese ED, van
#' der Linden SC, Man H, Roelofs MJ, Schulpen SH, Schwarz M, Uibel F, van
#' Vugt-Lussenburg BM, Westerhout J, Wolterbeek AP, van der Burg B (2013).
#' Evaluation of an alternative in vitro test battery for detecting
#' reproductive toxicants. Reprod Toxicol. 38:53-64. PMID: 23511061
#'
#' Pollack GM, Li RC, Ermer JC, Shen DD (1985). Effects of route of
#' administration and repetitive dosing on the disposition kinetics of
#' di(2-ethylhexyl) phthalate and its mono-de-esterified metabolite in rats.
#' Toxicol Appl Pharmacol. Jun 30;79(2):246-56. PMID: 4002226
#'
#' Saadeddin A, Torres-Molina F, Carcel-Trullols J, Araico A, Peris JE (2004).
#' Pharmacokinetics of the time-dependent elimination of all-trans-retinoic
#' acid in rats. AAPS J. 6(1):1-9. PMID: 18465253
#'
#' Satterwhite JH, Boudinot FD (1991). Effects of age and dose on the
#' pharmacokinetics of ibuprofen in the rat. Drug Metab Dispos. 19(1):61-7.
#' PMID: 1673423
#'
#' Szymura-Oleksiak J, Panas M, Chrusciel W (1983). Pharmacokinetics of
#' imipramine after single and multiple intravenous administration in rats. Pol
#' J Pharmacol Pharm. 35(2):151-7. PMID: 6622297
#'
#' Tanaka C, Kawai R, Rowland M (2000). Dose-dependent pharmacokinetics of
#' cyclosporin A in rats: events in tissues. Drug Metab Dispos. 28(5):582-9.
#' PMID: 10772639
#'
#' Timchalk C, Nolan RJ, Mendrala AL, Dittenber DA, Brzak KA, Mattsson JL
#' (2002). A Physiologically based pharmacokinetic and pharmacodynamic
#' (PBPK/PD) model for the organophosphate insecticide chlorpyrifos in rats and
#' humans. Toxicol Sci. Mar;66(1):34-53. PMID: 11861971
#'
#' Tokuma Y, Sekiguchi M, Niwa T, Noguchi H (1988). Pharmacokinetics of
#' nilvadipine, a new dihydropyridine calcium antagonist, in mice, rats,
#' rabbits and dogs. Xenobiotica 18(1):21-8. PMID: 3354229
#'
#' Treiber A, Schneiter R, Delahaye S, Clozel M (2004). Inhibition of organic
#' anion transporting polypeptide-mediated hepatic uptake is the major
#' determinant in the pharmacokinetic interaction between bosentan and
#' cyclosporin A in the rat. J Pharmacol Exp Ther. 308(3):1121-9. PMID:
#' 14617681
#'
#' Tsui BC, Feng JD, Buckley SJ, Yeung PK (1994). Pharmacokinetics and
#' metabolism of diltiazem in rats following a single intra-arterial or single
#' oral dose. Eur J Drug Metab Pharmacokinet. 19(4):369-73. PMID: 7737239
#'
#' Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#'
#' Wang Y, Roy A, Sun L, Lau CE (1999). A double-peak phenomenon in the
#' pharmacokinetics of alprazolam after oral administration. Drug Metab Dispos.
#' 27(8):855-9. PMID: 10421610
#'
#' Wang X, Lee WY, Or PM, Yeung JH (2010). Pharmacokinetic interaction studies
#' of tanshinones with tolbutamide, a model CYP2C11 probe substrate, using
#' liver microsomes, primary hepatocytes and in vivo in the rat. Phytomedicine.
#' 17(3-4):203-11. PMID: 19679455
#'
#' Yang SH, Lee MG (2008). Dose-independent pharmacokinetics of ondansetron in
#' rats: contribution of hepatic and intestinal first-pass effects to low
#' bioavailability. Biopharm Drug Dispos. 29(7):414-26. PMID: 18697186
#'
#' Yeung PK, Alcos A, Tang J (2009). Pharmacokinetics and Hemodynamic Effects
#' of Diltiazem in Rats Following Single vs Multiple Doses In Vivo. Open Drug
#' Metab J. 3:56-62.
#' @source Wambaugh et al. 2018 Toxicological Sciences, in press
#' @keywords data
"chem.invivo.PK.data"


#' Summary of published toxicokinetic time course experiments
#'
#' This data set summarizes the time course data in the chem.invivo.PK.data
#' table. Maximum concentration (Cmax), time integrated plasma concentration
#' for the duration of treatment (AUC.treatment) and extrapolated to zero
#' concentration (AUC.infinity) as well as half-life are calculated. Summary
#' values are given for each study and dosage. These data can be used to
#' evaluate toxicokinetic model predictions.
#'
#'
#' @docType data
#' @format A data.frame containing 100 rows and 25 columns.
#' @author John Wambaugh
#' @references Aanderud L, Bakke OM (1983). Pharmacokinetics of antipyrine,
#' paracetamol, and morphine in rat at 71 ATA. Undersea Biomed Res.
#' 10(3):193-201. PMID: 6636344
#'
#' Aasmoe L, Mathiesen M, Sager G (1999). Elimination of methoxyacetic acid and
#' ethoxyacetic acid in rat. Xenobiotica. 29(4):417-24. PMID: 10375010
#'
#' Ako RA. Pharmacokinetics/pharmacodynamics (PK/PD) of oral diethylstilbestrol
#' (DES) in recurrent prostate cancer patients and of oral dissolving film
#' (ODF)-DES in rats. PhD dissertation, College of Pharmacy, University of
#' Houston, USA, 2011.
#'
#' Anadon A, Martinez-Larranaga MR, Fernandez-Cruz ML, Diaz MJ, Fernandez MC,
#' Martinez MA (1996). Toxicokinetics of deltamethrin and its 4'-HO-metabolite
#' in the rat. Toxicol Appl Pharmacol. 141(1):8-16. PMID: 8917670
#'
#' Binkerd PE, Rowland JM, Nau H, Hendrickx AG (1988). Evaluation of valproic
#' acid (VPA) developmental toxicity and pharmacokinetics in Sprague-Dawley
#' rats. Fundam Appl Toxicol. 11(3):485-93. PMID: 3146521
#'
#' Boralli VB, Coelho EB, Cerqueira PM, Lanchote VL (2005). Stereoselective
#' analysis of metoprolol and its metabolites in rat plasma with application to
#' oxidative metabolism. J Chromatogr B Analyt Technol Biomed Life Sci.
#' 823(2):195-202. PMID: 16029965
#'
#' Chan MP, Morisawa S, Nakayama A, Kawamoto Y, Sugimoto M, Yoneda M (2005).
#' Toxicokinetics of 14C-endosulfan in male Sprague-Dawley rats following oral
#' administration of single or repeated doses. Environ Toxicol. 20(5):533-41.
#' PMID: 16161119
#'
#' Cruz L, Castaneda-Hernandez G, Flores-Murrieta FJ, Garcia-Lopez P,
#' Guizar-Sahagun G (2002). Alteration of phenacetin pharmacokinetics after
#' experimental spinal cord injury. Proc West Pharmacol Soc. 45:4-5. PMID:
#' 12434508
#'
#' Della Paschoa OE, Mandema JW, Voskuyl RA, Danhof M (1998).
#' Pharmacokinetic-pharmacodynamic modeling of the anticonvulsant and
#' electroencephalogram effects of phenytoin in rats. J Pharmacol Exp Ther.
#' 284(2):460-6. PMID: 9454785
#'
#' Du B, Li X, Yu Q, A Y, Chen C (2010). Pharmacokinetic comparison of orally
#' disintegrating, beta-cyclodextrin inclusion complex and conventional tablets
#' of nicardipine in rats. Life Sci J. 7(2):80-4.
#'
#' Farris FF, Dedrick RL, Allen PV, Smith JC (1993). Physiological model for
#' the pharmacokinetics of methyl mercury in the growing rat. Toxicol Appl
#' Pharmacol. 119(1):74-90. PMID: 8470126
#'
#' Hays SM, Elswick BA, Blumenthal GM, Welsch F, Conolly RB, Gargas ML (2000).
#' Development of a physiologically based pharmacokinetic model of
#' 2-methoxyethanol and 2-methoxyacetic acid disposition in pregnant rats.
#' Toxicol Appl Pharmacol. 163(1):67-74. PMID: 10662606
#'
#' Igari Y, Sugiyama Y, Awazu S, Hanano M (1982). Comparative physiologically
#' based pharmacokinetics of hexobarbital, phenobarbital and thiopental in the
#' rat. J Pharmacokinet Biopharm. 10(1):53-75. PMID: 7069578
#'
#' Ito K, Houston JB (2004). Comparison of the use of liver models for
#' predicting drug clearance using in vitro kinetic data from hepatic
#' microsomes and isolated hepatocytes. Pharm Res. 21(5):785-92. PMID: 15180335
#'
#' Jia L, Wong H, Wang Y, Garza M, Weitman SD (2003). Carbendazim: disposition,
#' cellular permeability, metabolite identification, and pharmacokinetic
#' comparison with its nanoparticle. J Pharm Sci. 92(1):161-72. PMID: 12486692
#'
#' Kawai R, Mathew D, Tanaka C, Rowland M (1998). Physiologically based
#' pharmacokinetics of cyclosporine A: extension to tissue distribution
#' kinetics in rats and scale-up to human. J Pharmacol Exp Ther. 287(2):457-68.
#' PMID: 9808668
#'
#' Kim YC, Kang HE, Lee MG (2008). Pharmacokinetics of phenytoin and its
#' metabolite, 4'-HPPH, after intravenous and oral administration of phenytoin
#' to diabetic rats induced by alloxan or streptozotocin. Biopharm Drug Dispos.
#' 29(1):51-61. PMID: 18022993
#'
#' Kobayashi S, Takai K, Iga T, Hanano M (1991). Pharmacokinetic analysis of
#' the disposition of valproate in pregnant rats. Drug Metab Dispos.
#' 19(5):972-6. PMID: 1686245
#'
#' Kotegawa T, Laurijssens BE, Von Moltke LL, Cotreau MM, Perloff MD,
#' Venkatakrishnan K, Warrington JS, Granda BW, Harmatz JS, Greenblatt DJ
#' (2002). In vitro, pharmacokinetic, and pharmacodynamic interactions of
#' ketoconazole and midazolam in the rat. J Pharmacol Exp Ther. 302(3):1228-37.
#' PMID: 12183684
#'
#' Krug AK, Kolde R, Gaspar JA, Rempel E, Balmer NV, Meganathan K, Vojnits K,
#' Baquie M, Waldmann T, Ensenat-Waser R, Jagtap S, Evans RM, Julien S,
#' Peterson H, Zagoura D, Kadereit S, Gerhard D, Sotiriadou I, Heke M,
#' Natarajan K, Henry M, Winkler J, Marchan R, Stoppini L, Bosgra S, Westerhout
#' J, Verwei M, Vilo J, Kortenkamp A, Hescheler J, Hothorn L, Bremer S, van
#' Thriel C, Krause KH, Hengstler JG, Rahnenfuhrer J, Leist M, Sachinidis A
#' (2013). Human embryonic stem cell-derived test systems for developmental
#' neurotoxicity: a transcriptomics approach. Arch Toxicol. 87(1):123-43. PMID:
#' 23179753
#'
#' Leon-Reyes MR, Castaneda-Hernandez G, Ortiz MI (2009). Pharmacokinetic of
#' diclofenac in the presence and absence of glibenclamide in the rat. J Pharm
#' Pharm Sci. 12(3):280-7. PMID: 20067705
#'
#' Nagata M, Hidaka M, Sekiya H, Kawano Y, Yamasaki K, Okumura M, Arimori K
#' (2007). Effects of pomegranate juice on human cytochrome P450 2C9 and
#' tolbutamide pharmacokinetics in rats. Drug Metab Dispos. 35(2):302-5. PMID:
#' 17132763
#'
#' Okiyama M, Ueno K, Ohmori S, Igarashi T, Kitagawa H (1988). Drug
#' interactions between imipramine and benzodiazepines in rats. J Pharm Sci.
#' 77(1):56-63. PMID: 2894451
#'
#' Pelissier-Alicot AL, Schreiber-Deturmeny E, Simon N, Gantenbein M,
#' Bruguerolle B (2002). Time-of-day dependent pharmacodynamic and
#' pharmacokinetic profiles of caffeine in rats. Naunyn Schmiedebergs Arch
#' Pharmacol. 365(4):318-25. PMID: 11919657
#'
#' Piersma AH, Bosgra S, van Duursen MB, Hermsen SA, Jonker LR, Kroese ED, van
#' der Linden SC, Man H, Roelofs MJ, Schulpen SH, Schwarz M, Uibel F, van
#' Vugt-Lussenburg BM, Westerhout J, Wolterbeek AP, van der Burg B (2013).
#' Evaluation of an alternative in vitro test battery for detecting
#' reproductive toxicants. Reprod Toxicol. 38:53-64. PMID: 23511061
#'
#' Pollack GM, Li RC, Ermer JC, Shen DD (1985). Effects of route of
#' administration and repetitive dosing on the disposition kinetics of
#' di(2-ethylhexyl) phthalate and its mono-de-esterified metabolite in rats.
#' Toxicol Appl Pharmacol. Jun 30;79(2):246-56. PMID: 4002226
#'
#' Saadeddin A, Torres-Molina F, Carcel-Trullols J, Araico A, Peris JE (2004).
#' Pharmacokinetics of the time-dependent elimination of all-trans-retinoic
#' acid in rats. AAPS J. 6(1):1-9. PMID: 18465253
#'
#' Satterwhite JH, Boudinot FD (1991). Effects of age and dose on the
#' pharmacokinetics of ibuprofen in the rat. Drug Metab Dispos. 19(1):61-7.
#' PMID: 1673423
#'
#' Szymura-Oleksiak J, Panas M, Chrusciel W (1983). Pharmacokinetics of
#' imipramine after single and multiple intravenous administration in rats. Pol
#' J Pharmacol Pharm. 35(2):151-7. PMID: 6622297
#'
#' Tanaka C, Kawai R, Rowland M (2000). Dose-dependent pharmacokinetics of
#' cyclosporin A in rats: events in tissues. Drug Metab Dispos. 28(5):582-9.
#' PMID: 10772639
#'
#' Timchalk C, Nolan RJ, Mendrala AL, Dittenber DA, Brzak KA, Mattsson JL
#' (2002). A Physiologically based pharmacokinetic and pharmacodynamic
#' (PBPK/PD) model for the organophosphate insecticide chlorpyrifos in rats and
#' humans. Toxicol Sci. Mar;66(1):34-53. PMID: 11861971
#'
#' Tokuma Y, Sekiguchi M, Niwa T, Noguchi H (1988). Pharmacokinetics of
#' nilvadipine, a new dihydropyridine calcium antagonist, in mice, rats,
#' rabbits and dogs. Xenobiotica 18(1):21-8. PMID: 3354229
#'
#' Treiber A, Schneiter R, Delahaye S, Clozel M (2004). Inhibition of organic
#' anion transporting polypeptide-mediated hepatic uptake is the major
#' determinant in the pharmacokinetic interaction between bosentan and
#' cyclosporin A in the rat. J Pharmacol Exp Ther. 308(3):1121-9. PMID:
#' 14617681
#'
#' Tsui BC, Feng JD, Buckley SJ, Yeung PK (1994). Pharmacokinetics and
#' metabolism of diltiazem in rats following a single intra-arterial or single
#' oral dose. Eur J Drug Metab Pharmacokinet. 19(4):369-73. PMID: 7737239
#'
#' Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#'
#' Wang Y, Roy A, Sun L, Lau CE (1999). A double-peak phenomenon in the
#' pharmacokinetics of alprazolam after oral administration. Drug Metab Dispos.
#' 27(8):855-9. PMID: 10421610
#'
#' Wang X, Lee WY, Or PM, Yeung JH (2010). Pharmacokinetic interaction studies
#' of tanshinones with tolbutamide, a model CYP2C11 probe substrate, using
#' liver microsomes, primary hepatocytes and in vivo in the rat. Phytomedicine.
#' 17(3-4):203-11. PMID: 19679455
#'
#' Yang SH, Lee MG (2008). Dose-independent pharmacokinetics of ondansetron in
#' rats: contribution of hepatic and intestinal first-pass effects to low
#' bioavailability. Biopharm Drug Dispos. 29(7):414-26. PMID: 18697186
#'
#' Yeung PK, Alcos A, Tang J (2009). Pharmacokinetics and Hemodynamic Effects
#' of Diltiazem in Rats Following Single vs Multiple Doses In Vivo. Open Drug
#' Metab J. 3:56-62.
#' @source Wambaugh et al. 2018 Toxicological Sciences, in press
#' @keywords data
"chem.invivo.PK.summary.data"



#' Parameter Estimates from Wambaugh et al. (2018)
#'
#' This table includes 1 and 2 compartment fits of plasma concentration vs time
#' data aggregated from chem.invivo.PK.data, performed in Wambaugh et al. 2018.
#' Data includes volume of distribution (Vdist, L/kg), elimination rate (kelim,
#' 1/h), gut absorption rate (kgutabs, 1/h), fraction absorbed (Fgutabs), and
#' steady state concentration (Css, mg/L).
#'
#'
#' @docType data
#' @format data.frame
#' @author John Wambaugh
#' @source Wambaugh et al. 2018 Toxicological Sciences, in press
#' @keywords data
"chem.invivo.PK.aggregate.data"


#' Chemical membership in different research projects
#'
#' A static list of lists identifying chemical membership in different research
#' projects. While it is our intent to keep these lists up-to-date, the
#' information here is only for convenience and should not be considered to be
#' definitive.
#'
#'
#' @docType data
#' @format A list containing ten lists.
#' @author John Wambaugh
#' @references Bucher, J. R. (2008). Guest Editorial: NTP: New Initiatives, New
#' Alignment. Environ Health Perspect 116(1).
#'
#' Judson, R. S., Houck, K. A., Kavlock, R. J., Knudsen, T. B., Martin, M. T.,
#' Mortensen, H. M., Reif, D. M., Rotroff, D. M., Shah, I., Richard, A. M. and
#' Dix, D. J. (2010). In Vitro Screening of Environmental Chemicals for
#' Targeted Testing Prioritization: The ToxCast Project. Environmental Health
#' Perspectives 118(4), 485-492.
#'
#' Wambaugh, J. F., Wang, A., Dionisio, K. L., Frame, A., Egeghy, P., Judson,
#' R. and Setzer, R. W. (2014). High Throughput Heuristics for Prioritizing
#' Human Exposure to Environmental Chemicals. Environmental Science &
#' Technology, 10.1021/es503583j.
#'
#' CDC (2014). National Health and Nutrition Examination Survey. Available at:
#' https://www.cdc.gov/nchs/nhanes.htm.
#' @keywords data
"chem.lists"


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
#'@references Wambaugh et al. (2019) "Assessing Toxicokinetic Uncertainty and
#'Variability in Risk Prioritization", Toxicological Sciences, 172(2), 235-251.
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
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to
#' evironmental chemicals by simulating toxicokinetic variability." Environment
#' international 106 (2017): 105-118
#'
#'@references Wambaugh et al. (2019) "Assessing Toxicokinetic Uncertainty and
#'Variability in Risk Prioritization", Toxicological Sciences, 172(2), 235-251.
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
#'@references Ring, Caroline L., et al. "Consensus modeling of median chemical
#' intake for the US population based on predictions of exposure pathways."
#' Environmental science & technology 53.2 (2018): 719-732.
#'
#'@references Wambaugh et al. (2019) "Assessing Toxicokinetic Uncertainty and
#'Variability in Risk Prioritization", Toxicological Sciences, 172(2), 235-251.
"wambaugh2019.seem3"


#' Physico-chemical properties and in vitro measurements for toxicokinetics
#'
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models. See variable EPA.ref for information on the reference EPA.
#'
#' @docType data
#' @format A data.frame containing 565 rows and 33 columns.
#' @author John Wambaugh
#' @references DSStox database (https:// www.epa.gov/ncct/dsstox
#'
#' EPI Suite, https://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#'
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
#' 
#' Honda, G. S., Pearce, R. G., Pham, L. L., Setzer, R. W., Wetmore, B. A., 
#' Sipes, N. S., ... & Wambaugh, J. F. (2019). Using the concordance of in 
#' vitro and in vivo data to evaluate extrapolation assumptions. PloS one, 
#' 14(5), e0217564.
#'
#' Ito, K. and Houston, J. B. (2004). Comparison of the use of liver models for
#' predicting drug clearance using in vitro kinetic data from hepatic
#' microsomes and isolated hepatocytes. Pharm Res 21(5), 785-92.
#'
#' Jones, O. A., Voulvoulis, N. and Lester, J. N. (2002). Aquatic environmental
#' assessment of the top 25 English prescription pharmaceuticals. Water
#' research 36(20), 5013-22.
#'
#' Lau, Y. Y., Sapidou, E., Cui, X., White, R. E. and Cheng, K. C. (2002).
#' Development of a novel in vitro model to predict hepatic clearance using
#' fresh, cryopreserved, and sandwich-cultured hepatocytes. Drug Metabolism and
#' Disposition 30(12), 1446-54.
#'
#' Linakis, M. W., Sayre, R. R., Pearce, R. G., Sfeir, M. A., Sipes, N. S., 
#' Pangburn, H. A., ... & Wambaugh, J. F. (2020). Development and evaluation of 
#' a high throughput inhalation model for organic chemicals. Journal of 
#' Exposure Science & Environmental Epidemiology, 1-12.
#' 
#' Lombardo, F., Berellini, G., & Obach, R. S. (2018). Trend analysis of a 
#' database of intravenous pharmacokinetic parameters in humans for 1352 drug 
#' compounds. Drug Metabolism and Disposition, 46(11), 1466-1477.
#'
#' McGinnity, D. F., Soars, M. G., Urbanowicz, R. A. and Riley, R. J. (2004).
#' Evaluation of fresh and cryopreserved hepatocytes as in vitro drug
#' metabolism tools for the prediction of metabolic clearance. Drug Metabolism
#' and Disposition 32(11), 1247-53, 10.1124/dmd.104.000026.
#'
#' Naritomi, Y., Terashita, S., Kagayama, A. and Sugiyama, Y. (2003). Utility
#' of Hepatocytes in Predicting Drug Metabolism: Comparison of Hepatic
#' Intrinsic Clearance in Rats and Humans in Vivo and in Vitro. Drug Metabolism
#' and Disposition 31(5), 580-588, 10.1124/dmd.31.5.580.
#'
#' Obach, R. S. (1999). Prediction of human clearance of twenty-nine drugs from
#' hepatic microsomal intrinsic clearance data: An examination of in vitro
#' half-life approach and nonspecific binding to microsomes. Drug Metabolism
#' and Disposition 27(11), 1350-9.
#'
#' Paini, Alicia; Cole, Thomas; Meinero, Maria; Carpi, Donatella; Deceuninck, 
#' Pierre; Macko, Peter; Palosaari, Taina; Sund, Jukka; Worth, Andrew; Whelan, 
#' Maurice (2020):  EURL ECVAM in vitro hepatocyte clearance and blood plasma 
#' protein binding dataset for 77 chemicals. European Commission, Joint Research 
#' Centre (JRC) [Dataset] PID: https://data.europa.eu/89h/a2ff867f-db80-4acf-8e5c-e45502713bee
#' 
#' Paixao, P., Gouveia, L. F., & Morais, J. A. (2012). Prediction of the human
#' oral bioavailability by using in vitro and in silico drug related parameters
#' in a physiologically based absorption model. International journal of
#' pharmaceutics, 429(1), 84-98.
#'
#' Pirovano, Alessandra, et al. "QSARs for estimating intrinsic hepatic
#' clearance of organic chemicals in humans." Environmental toxicology and
#' pharmacology 42 (2016): 190-197.
#'
#' Schmitt, W. (2008). General approach for the calculation of tissue to plasma
#' partition coefficients. Toxicology in vitro : an international journal
#' published in association with BIBRA 22(2), 457-67,
#' 10.1016/j.tiv.2007.09.010.
#'
#' Shibata, Y., Takahashi, H., Chiba, M. and Ishii, Y. (2002). Prediction of
#' Hepatic Clearance and Availability by Cryopreserved Human Hepatocytes: An
#' Application of Serum Incubation Method. Drug Metabolism and Disposition
#' 30(8), 892-896, 10.1124/dmd.30.8.892.
#'
#' Tonnelier, A., Coecke, S. and Zaldivar, J.-M. (2012). Screening of chemicals
#' for human bioaccumulative potential with a physiologically based
#' toxicokinetic model. Archives of Toxicology 86(3), 393-403,
#' 10.1007/s00204-011-0768-0.
#'
#' Uchimura, Takahide, et al. "Prediction of human blood-to-plasma drug
#' concentration ratio." Biopharmaceutics & drug disposition 31.5-6 (2010):
#' 286-297.
#'
#' Wambaugh, J. F., Wetmore, B. A., Ring, C. L., Nicolas, C. I., Pearce, R. G., 
#' Honda, G. S., ... & Badrinarayanan, A. (2019). Assessing Toxicokinetic 
#' Uncertainty and Variability in Risk Prioritization. Toxicological Sciences, 
#' 172(2), 235-251.
#' 
#' Wetmore, B. A., Wambaugh, J. F., Ferguson, S. S., Sochaski, M. A., Rotroff,
#' D. M., Freeman, K., Clewell, H. J., 3rd, Dix, D. J., Andersen, M. E., Houck,
#' K. A., Allen, B., Judson, R. S., Singh, R., Kavlock, R. J., Richard, A. M.
#' and Thomas, R. S. (2012). Integration of dosimetry, exposure, and
#' high-throughput screening data in chemical toxicity assessment.
#' Toxicological sciences : an official journal of the Society of Toxicology
#' 125(1), 157-74, 10.1093/toxsci/kfr254.
#'
#' Wetmore, B. A., Wambaugh, J. F., Ferguson, S. S., Li, L., Clewell, H. J.,
#' Judson, R. S., Freeman, K., Bao, W., Sochaski, M. A., Chu, T.-M., Black, M.
#' B., Healy, E., Allen, B., Andersen, M. E., Wolfinger, R. D. and Thomas, R.
#' S. (2013). Relative Impact of Incorporating Pharmacokinetics on Predicting
#' In Vivo Hazard and Mode of Action from High-Throughput In Vitro Toxicity
#' Assays. Toxicological Sciences 132(2), 327-346, 10.1093/toxsci/kft012.
#'
#' Wetmore, B. A., Wambaugh, J. F., Allen, B., Ferguson, S. S., Sochaski, M.
#' A., Setzer, R. W., Houck, K. A., Strope, C. L., Cantwell, K., Judson, R. S.,
#' LeCluyse, E., Clewell, H.J. III, Thomas, R.S., and Andersen, M. E. (2015).
#' "Incorporating High-Throughput Exposure Predictions with Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing" Toxicological
#' Sciences, kfv171.
#' @source Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#' @keywords data
"chem.physical_and_invitro.data"


#' Sipes et al. 2017 data
#'
#' This table includes in silico predicted chemical-specifc plasma protein 
#' unbound fraction (fup) and intrinsic hepatic clearance values for the entire
#' Tox21 library 
#' (see \url{https://www.epa.gov/chemical-research/toxicology-testing-21st-century-tox21}). 
#' Predictions were made with Simulations Plus ADMET predictor,
#' as reported in Sipes et al. (2017). 
#'
#' @name sipes2017
#' @aliases Sipes2017
#' @docType data
#' @format data.frame
#' @author Nisha Sipes
#' @references Sipes, Nisha S., et al. "An Intuitive Approach for Predicting
#' Potential Human Health Risk with the Tox21 10k Library." Environmental
#' Science & Technology 51.18 (2017): 10786-10796.
#' @source ADMET, Simulations Plus
#' @keywords data
"sipes2017"


#' Tox21 2015 Active Hit Calls (EPA)
#'
#' The ToxCast and Tox21 research programs employ batteries of high throughput
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
#' providing high throughput decision support tools for chemical risk
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
#'
#' @source \url{ftp://newftp.epa.gov/COMPTOX/High_Throughput_Screening_Data/Previous_Data/ToxCast_Data_Release_Oct_2015/}
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
#' @references Schmitt, W., General approach for the calculation of tissue to
#' plasma partition coefficients. Toxicology in Vitro, 2008. 22(2): p. 457-467.
#'
#' Schmitt, W., Corrigendum to:"General approach for the calculation of tissue
#' to plasma partition coefficients"[Toxicology in Vitro 22 (2008) 457-467].
#' Toxicology in Vitro, 2008. 22(6): p. 1666.
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
#' Yun, Y. and A. Edginton, Correlation-based prediction of tissue-to-plasma
#' partition coefficients using readily available input parameters.
#' Xenobiotica, 2013. 43(10): p. 839-852.
#'
#' Uchimura, T., et al., Prediction of human blood-to-plasma drug concentration
#' ratio. Biopharmaceutics & drug disposition, 2010. 31(5-6): p. 286-297.
#' @keywords data
"pc.data"



#' Species-specific physiology parameters
#'
#' This data set contains values from Davies and Morris (1993) necessary to
#' paramaterize a toxicokinetic model for human, mouse, rat, dog, or rabbit.
#' The temperature for each species are taken from Robertshaw et al. (2004),
#' Gordon (1993), and Stammers(1926).
#'
#'
#' @docType data
#' @format A data.frame containing 11 rows and 7 columns.
#' @author John Wambaugh and Nisha Sipes
#' @references Davies, B. and Morris, T. (1993). Physiological Parameters in
#' Laboratory Animals and Humans. Pharmaceutical Research 10(7), 1093-1095,
#' 10.1023/a:1018943613122.  %gfr and other flows Anderson and Holford (2009)
#' %scaling gfr by 3/4 Robertshaw, D., Temperature Regulation and Thermal
#' Environment, in Dukes' Physiology of Domestic Animals, 12th ed., Reece W.O.,
#' Ed. Copyright 2004 by Cornell University.  Stammers (1926) The blood count
#' and body temperature in normal rats Gordon (1993) Temperature Regulation in
#' Laboratory Rodents
#' @source Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#' @keywords data
"physiology.data"



#' Tissue composition and species-specific physiology parameters
#'
#' This data set contains values from Schmitt (2008) and Ruark et al. (2014)
#' describing the composition of specific tissues and from Birnbaum et al.
#' (1994) describing volumes of and blood flows to those tissues, allowing
#' parameterization of toxicokinetic models for human, mouse, rat, dog, or
#' rabbit. Tissue volumes were calculated by converting the fractional mass of
#' each tissue with its density (both from ICRP), lumping the remaining tissues
#' into the rest-of-body, excluding the mass of the gastrointestinal contents
#'
#'
#' @docType data
#' @format A data.frame containing 13 rows and 20 columns.
#' @author John Wambaugh, Robert Pearce, and Nisha Sipes
#' @references Birnbaum, L and Brown, R and Bischoff, K and Foran, J and
#' Blancato, J and Clewell, H and Dedrick, R (1994). Physiological parameter
#' values for PBPK model. International Life Sciences Institute, Risk Science
#' Institute, Washington, DC
#'
#' Ruark, Christopher D., et al. "Predicting passive and active tissue: plasma
#' partition coefficients: Interindividual and interspecies variability."
#' Journal of pharmaceutical sciences 103.7 (2014): 2189-2198.
#'
#' Schmitt, W. (2008). General approach for the calculation of tissue to plasma
#' partition coefficients. Toxicology in vitro : an international journal
#' published in association with BIBRA 22(2), 457-67,
#' 10.1016/j.tiv.2007.09.010.
#'
#' ICRP. Report of the Task Group on Reference Man. ICRP Publication 23 1975
#' @source Pearce et al. (2017), in preparation,
#'
#' Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#' @keywords data
"tissue.data"



#' Published toxicokinetic predictions based on in vitro data
#'
#' This data set gives the chemical specific predictions for serum
#' concentration at steady state resulting from constant infusion exposure, as
#' published in a series of papers from Barbara Wetmore's group at the Hamner
#' Institutes for Life Sciences. Predictions include the median and 90\%
#' interval in uM and mg/L. Calculations were made using the 1 and 10 uM in
#' vitro measured clearances.
#'
#'
#' @docType data
#' @format A data.frame containing 577 rows and 20 columns.
#' @references Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A.,
#' Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck,
#' K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and
#' Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput
#' Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125
#' 157-174 (2012)
#'
#' Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Li, L., Clewell, H.J. III,
#' Judson, R.S., Freeman, K., Bao, W, Sochaski, M.A., Chu T.-M., Black, M.B.,
#' Healy, E, Allen, B., Andersen M.E., Wolfinger, R.D., and Thomas R.S., "The
#' Relative Impact of Incorporating Pharmacokinetics on Predicting in vivo
#' Hazard and Mode-of-Action from High-Throughput in vitro Toxicity Assays"
#' Toxicological Sciences, 132:327-346 (2013).
#'
#' Wetmore, B. A., Wambaugh, J. F., Allen, B., Ferguson, S. S., Sochaski, M.
#' A., Setzer, R. W., Houck, K. A., Strope, C. L., Cantwell, K., Judson, R. S.,
#' LeCluyse, E., Clewell, H.J. III, Thomas, R.S., and Andersen, M. E. (2015).
#' "Incorporating High-Throughput Exposure Predictions with Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing" Toxicological
#' Sciences, kfv171.
#' @source Wambaugh, John F., et al. "Toxicokinetic triage for environmental
#' chemicals." Toxicological Sciences (2015): 228-237.
#' @keywords data
"Wetmore.data"



#' Published toxicokinetic predictions based on in vitro data from Wetmore et
#' al. 2012.
#'
#' This data set overlaps with Wetmore.data and is used only in Vignette 4 for
#' steady state concentration.
#'
#'
#' @docType data
#' @format A data.frame containing 13 rows and 15 columns.
#' @references Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A.,
#' Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck,
#' K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and
#' Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput
#' Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125
#' 157-174 (2012)
#' @keywords data
"Wetmore2012"

#' Metabolism data involved in Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references DSStox database (https:// www.epa.gov/ncct/dsstox
#'
#' @source Matt Linakis
#' @keywords data
"metabolism_data_Linakis2020"

#' Concentration data involved in Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references DSStox database (https:// www.epa.gov/ncct/dsstox
#'
#' @source Matt Linakis
#' @keywords data
"concentration_data_Linakis2020"

#' Supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references DSStox database (https:// www.epa.gov/ncct/dsstox
#'
#' @source Matt Linakis
#' @keywords data
"supptab1_Linakis2020"

#' More supplementary output from Linakis 2020 vignette analysis.
#'
#'
#' @docType data
#' @format A data.frame containing x rows and y columns.
#' @author Matt Linakis
#' @references DSStox database (https:// www.epa.gov/ncct/dsstox
#'
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
