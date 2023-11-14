#' httkpop: Virtual population generator for HTTK.
#' 
#' The httkpop package generates virtual population physiologies for use in 
#' population TK.
#'
#' To simulate inter-individual variability in the TK model, a MC approach
#' is used: the model parameters are sampled from known or assumed
#' distributions, and the model is evaluated for each sampled set of
#' parameters. To simulate variability across subpopulations, the MC approach
#' needs to capture the parameter correlation structure. For example,
#' kidney function changes with age (Levey et al., 2009), thus the
#' distribution of GFR is likely different in 6-year-olds than in 65-yearolds.
#' To directly measure the parameter correlation structure, all parameters
#' need to be measured in each individual in a representative
#' sample population. Such direct measurements are extremely limited.
#' However, the correlation structure of the physiological parameters can
#' be inferred from their known individual correlations with demographic
#' and anthropometric quantities for which direct population measurements
#' do exist. These quantities are sex, race/ethnicity, age, height, and
#' weight (Howgate et al., 2006; Jamei et al., 2009a; Johnson et al., 2006;
#' McNally et al., 2014; Price et al., 2003). Direct measurements of these
#' quantities in a large, representative sample of the U.S. population are
#' publicly available from NHANES. NHANES also includes laboratory
#' measurements, including both serum creatinine, which can be used to
#' estimate GFR (Levey et al., 2009), and hematocrit. For conciseness, sex,
#' race/ethnicity, age, height, weight, serum creatinine, and hematocrit
#' will be called the NHANES quantities.
#'
#' HTTK-Pop's correlated MC approach begins by sampling from the
#' joint distribution of the NHANES quantities to simulate a population.
#' Then, for each individual in the simulated population, HTTKePop
#' predicts the physiological parameters from the NHANES
#' quantities using regression equations from the literature (Barter et al.,
#' 2007; Baxter-Jones et al., 2011; Bosgra et al., 2012; Koo et al., 2000;
#' Levey et al., 2009; Looker et al., 2013; McNally et al., 2014; Ogiu et al.,
#' 1997; Price et al., 2003; Schwartz and Work, 2009; Webber and Barr 2012). 
#' Correlations among the physiological parameters are induced by
#' their mutual dependence on the correlated NHANES quantities. Finally,
#' residual variability is added to the predicted physiological parameters
#' using estimates of residual marginal variance (i.e., variance not explained
#' by the regressions on the NHANES quantities) (McNally et al.,
#' 2014).
#' 
#' Data were combined from the three most recent publicly-available
#' NHANES cycles: 2007-2008, 2009-2010, and 2011-2012. For each
#' cycle, some NHANES quantities - height, weight, serum creatinine,
#' and hematocrit - were measured only in a subset of respondents. Only
#' these subsets were included in HTTKePop. The pooled subsets from the
#' three cycles contained 29,353 unique respondents. Some respondents
#' were excluded from analysis: those with age recorded as 80 years (because
#' all NHANES respondents 80 years and older were marked as
#' "80"); those with missing height, weight or hematocrit data; and those
#' aged 12 years or older with missing serum creatinine data. These criteria
#' excluded 4807 respondents, leaving 24,546 unique respondents. Each
#' NHANES respondent was assigned a cycle-specific sample weight,
#' which can be interpreted as the number of individuals in the total U.S.
#' population represented by each NHANES respondent in each cycle
#' (Johnson et al., 2013). Because data from three cycles were combined,
#' the sample weights were rescaled (divided by the number of cycles
#' being combined, as recommended in NHANES data analysis documentation)
#' (Johnson et al., 2013). To handle the complex NHANES
#' sampling structure, the R survey package was used to analyze the
#' NHANES data (Lumley, 2004). 
#'
#' To allow generation of virtual populations specified by weight class,
#' we coded a categorical variable for each NHANES respondent. The
#' categories Underweight, Normal, Overweight, or Obese were assigned
#' based on weight, age, and height/length (Grummer-Strawn et al., 2010;
#' Kuczmarski et al., 2002; Ogden et al., 2014; WHO, 2006, 2010). 

#' We implemented two population simulation methods within HTTK-Pop:
#' the direct-resampling method and the virtual-individuals method.
#' The direct-resampling method simulated a population by sampling
#' NHANES respondents with replacement, with probabilities proportional
#' to the sample weights. Each individual in the resulting simulated population
#' was an NHANES respondent, identified by a unique NHANES
#' sequence number. By contrast, the second method generates "virtual
#' individuals" - sets of NHANES quantities that obey the approximate
#' joint distribution of the NHANES quantities (calculated using weighted
#' smoothing functions and kernel density estimators), but do not necessarily correspond to
#' any particular NHANES respondent. The direct-resampling method removed
#' the possibility of generating unrealistic combinations of the
#' NHANES quantities; the virtual-individuals method allowed the use of
#' interpolation to simulate subpopulations represented by only a small
#' number of NHANES respondents. 
#' 
#' For either method, HTTK-Pop
#' takes optional specifications about the population to be simulated
#' and then samples from the appropriate conditional joint
#' distribution of the NHANES quantities.
#' 
#' Once HTTK-Pop has simulated a population characterized by the
#' NHANES quantities, the physiological parameters of the TK model
#'  are predicted from the NHANES quantities using regression
#' equations from the literature. Liver mass was predicted for individuals
#' over age 18 using allometric scaling with height from Reference Man
#' (Valentin, 2002), and for individuals under 18 using regression relationships
#' with height and weight published by Ogiu et al. (1997).
#' Residual marginal variability was added for each individual as in
#' PopGen (McNally et al., 2014). Similarly, hepatic portal vein blood
#' flows (in L/h) are predicted as fixed fractions of a cardiac output allometrically
#' scaled with height from Reference Man (Valentin, 2002),
#' and residual marginal variability is added for each individual (McNally
#' et al., 2014). Glomerular filtration rate (GFR) (in L/h/1.73 m2 body
#' surface area) is predicted from age, race, sex, and serum creatinine
#' using the CKD-EPI equation, for individuals over age 18 (Levey et al.,
#' 2009). For individuals under age 18, GFR is estimated from body surface
#' area (BSA) (Johnson et al., 2006); BSA is predicted using Mosteller's
#' formula (Verbraecken et al., 2006) for adults and Haycock's
#' formula (Haycock et al., 1978) for children. Hepatocellularity (in millions
#' of cells per gram of liver tissue) is predicted from age using an
#' equation developed by Barter et al. (2007). Hematocrit is estimated
#' from NHANES data for individuals 1 year and older. For individuals
#' younger than 1 year, for whom NHANES did not measure hematocrit
#' directly, hematocrit was predicted from age in months, using published
#' reference ranges (Lubin, 1987).
#' 
#' In addition to the HTTK physiological parameters, the HTTK models
#' include chemical-specific parameters representing the fraction of chemical
#' unbound in plasma (Fup) and intrinsic clearance (CLint). Because
#' these parameters represent interactions of the chemical with the body,
#' their values will vary between individuals. To simulate this variability,
#' Fub and CLint were included in MC simulations, by sampling from estimated
#' or assumed distributions for the parameters defining them.
#' 
#' Variability in hematocrit was simulated either using NHANES data
#' (for individuals ages 1 and older) or using age-based reference ranges
#' (for individuals under age 1). Fup was treated as a random variable
#' obeying a distribution censored below the average limit of quantification
#' (LOQ) of the in vitro assay. Specifically, Fup was assumed to obey a
#' normal distribution truncated below at 0 and above at 1, centered at the
#' Fup value measured in vitro, with a 30% coefficient of variation. Below
#' the average LOQ (0.01), Fup was instead drawn from a uniform distribution
#' between 0 and 0.01. Fup was assumed to be independent of all
#' other parameters. This censored normal distribution was chosen to
#' match that used in Wambaugh et al. (2015).
#' 
#' Variability in hepatocellularity (106 cells/g liver) and Mliver (kg)
#' were simulated. The remaining source of variability in CLint,h is variability
#' in CLint, which was simulated using a Gaussian mixture distribution
#' to represent the population proportions of poor metabolizers
#' (PMs) and non-PMs of each substance. The true prevalence of PMs is
#' isozyme-specific (Ma et al., 2002; Yasuda et al., 2008); however, isozyme-
#' specific metabolism data were not available for the majority of
#' chemicals considered. We therefore made a simplifying assumption that
#' 5% of the population are PMs, metabolizing each substance ten times
#' slower than average.
#' With 95% probability, CLint was drawn from a nonePM distribution:
#' a normal distribution truncated below at zero, centered at the value
#' measured in vitro, with a 30% coefficient of variation. With 5% probability,
#' CLint was drawn from a PM distribution: a truncated normal
#' distribution centered on one-tenth of the in vitro value with 30% CV.
#' Both CLint itself and the probability of being a PM were assumed to be
#' independent of all other parameters. The truncated normal nonePM
#' distribution was chosen because it has been used (with 100% probability)
#' in previous work (Rotroff et al., 2010; Wambaugh et al., 2015;
#' Wetmore et al., 2014; Wetmore et al., 2015; Wetmore et al., 2012); the
#' PM distribution was chosen to comport with the nonePM distribution.
#' 
#' @name httkpop
#' @aliases httkpop httkpop-package
#' @section Main function to generate a population: 
#' If you just want to generate
#'   a table of (chemical-independent) population physiology parameters, use 
#'   \code{\link{httkpop_generate}}.
#'   
#' @section Using HTTK-Pop with HTTK: 
#' To generate a population and then run an 
#'   HTTK model for that population, the workflow is as follows: \enumerate{ 
#'   \item Generate a population using \code{\link{httkpop_generate}}. \item For
#'   a given HTTK chemical and general model, convert the population data to 
#'   corresponding sets of HTTK model parameters using 
#'   \code{\link{httkpop_mc}}.}
#'   
#'   
#' @import data.table
#' @docType package
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references 
#' \insertRef{ring2017identifying}{httk}     
#' 
#' Levey, A.S., Stevens, L.A., Schmid, C.H., Zhang, Y.L., Castro, A.F., Feldman, H.I., et al.,
#' 2009. A new equation to estimate glomerular filtration rate. Ann. Intern. Med. 150,
#' 604-612.
#' 
#' Howgate, E., Rowland-Yeo, K., Proctor, N., Tucker, G., Rostami-Hodjegan, A., 2006.
#' Prediction of in vivo drug clearance from in vitro data. I: impact of inter-individual
#' variability. Xenobiotica 36, 473-497.
#' 
#' Jamei, M., Dickinson, G.L., Rostami-Hodjegan, A., 2009a. A framework for assessing
#' inter-individual variability in pharmacokinetics using virtual human populations and
#' integrating general knowledge of physical chemistry, biology, anatomy, physiology
#' and genetics: a tale of 'bottom-up' vs 'top-down' recognition of covariates. Drug Metab.
#' Pharmacokinet. 24, 53-75.
#' 
#' Johnson, T.N., Rostami-Hodjegan, A., Tucker, G.T., 2006. Prediction of the clearance of
#' eleven drugs and associated variability in neonates, infants and children. Clin.
#' Pharmacokinet. 45, 931-956.
#' 
#' McNally, K., Cotton, R., Hogg, A., Loizou, G., 2014. PopGen: a virtual human population
#' generator. Toxicology 315, 70-85.
#' 
#' Price, P.S., Conolly, R.B., Chaisson, C.F., Gross, E.A., Young, J.S., Mathis, E.T., et al.,
#' 2003. Modeling interindividual variation in physiological factors used in PBPK
#' models of humans. Crit. Rev. Toxicol. 33, 469-503.
#' 
#' Barter, Z.E., Bayliss, M.K., Beaune, P.H., Boobis, A.R., Carlile, D.J., Edwards, R.J., et al.,
#' 2007. Scaling factors for the extrapolation of in vivo metabolic drug clearance from
#' in vitro data: reaching a consensus on values of human micro-somal protein and
#' hepatocellularity per gram of liver. Curr. Drug Metab. 8, 33-45.
#' 
#' Baxter-Jones, A.D., Faulkner, R.A., Forwood, M.R., Mirwald, R.L., Bailey, D.A., 2011.
#' Bone mineral accrual from 8 to 30 years of age: an estimation of peak bone mass. J.
#' Bone Miner. Res. 26, 1729-1739.
#' 
#' Bosgra, S., van Eijkeren, J., Bos, P., Zeilmaker, M., Slob, W., 2012. An improved model to
#' predict physiologically based model parameters and their inter-individual variability
#' from anthropometry. Crit. Rev. Toxicol. 42, 751-767.
#' 
#' Koo, W.W., Walters, J.C., Hockman, E.M., 2000. Body composition in human infants at
#' birth and postnatally. J. Nutr. 130, 2188-2194.
#' 
#' Looker, A., Borrud, L., Hughes, J., Fan, B., Shepherd, J., Sherman, M., 2013. Total body
#' bone area, bone mineral content, and bone mineral density for individuals aged 8
#' years and over: United States, 1999-2006. In: Vital and health statistics Series 11,
#' Data from the National Health Survey, pp. 1-78.
#' 
#' Ogiu, N., Nakamura, Y., Ijiri, I., Hiraiwa, K., Ogiu, T., 1997. A statistical analysis of the
#' internal organ weights of normal Japanese people. Health Phys. 72, 368-383.
#' 
#' Schwartz, G.J., Work, D.F., 2009. Measurement and estimation of GFR in children and
#' adolescents. Clin. J. Am. Soc. Nephrol. 4, 1832-1843.
#' 
#' Webber, C.E., Barr, R.D., 2012. Age-and gender-dependent values of skeletal muscle mass
#' in healthy children and adolescents. J. Cachex. Sarcopenia Muscle 3, 25-29.
#' 
#' Johnson, C.L., Paulose-Ram, R., Ogden, C.L., Carroll, M.D., Kruszon-Moran, D.,
#' Dohrmann, S.M., et al., 2013. National health and nutrition examination survey:
#' analytic guidelines, 1999-2010. Vital and health statistics Series 2. Data Eval.
#' Methods Res. 1-24.
#' 
#' Lumley, T., 2004. Analysis of complex survey samples. J. Stat. Softw. 9, 1-19.
#' 
#' Grummer-Strawn, L.M., Reinold, C.M., Krebs, N.F., Control, C.f.D.; Prevention, 2010. Use
#' of World Health Organization and CDC Growth Charts for Children Aged 0-59
#' Months in the United States. Department of Health and Human Services, Centers for
#' Disease Control and Prevention.
#' 
#' Kuczmarski, R.J., Ogden, C.L., Guo, S.S., Grummer-Strawn, L.M., Flegal, K.M., Mei, Z.,
#' et al., 2002. 2000 CDC growth charts for the United States: methods and development.
#' Vital Health Stat. Series 11, Data from the national health survey 246, 1-190.
#' 
#' Ogden, C.L., Carroll, M.D., Kit, B.K., Flegal, K.M., 2014. Prevalence of childhood and
#' adult obesity in the United States, 2011-2012. JAMA 311, 806-814.
#' 
#' WHO, 2006. In: WHO D.o.N.f.H.a.D. (Ed.), WHO Child Growth Standards: Length/Heightfor-
#' Age, Weight-for-Age, Weight-for-Length, Weight-for-Height and Body Mass Indexfor-
#' Age: Methods and Development.
#' 
#' WHO, 2010. In: (WHO) W.H.O. (Ed.), WHO Anthro for Personal Computers Manual:
#' Software for Assessing Growth and Development of the World's Children, Version
#' 3.2.2, 2011. WHO, Geneva.
#' 
#' Valentin, J., 2002. Basic anatomical and physiological data for use in radiological protection:
#' reference values: ICRP publication 89. Ann. ICRP 32, 1-277.
#' 
#' Johnson, T.N., Rostami-Hodjegan, A., Tucker, G.T., 2006. Prediction of the clearance of
#' eleven drugs and associated variability in neonates, infants and children. Clin.
#' Pharmacokinet. 45, 931-956.
#' 
#' Verbraecken, J., Van de Heyning, P., De Backer, W., Van Gaal, L., 2006. Body surface area
#' in normal-weight, overweight, and obese adults. A comparison study. Metabolism 55,
#' 515-524
#' 
#' Haycock, G.B., Schwartz, G.J., Wisotsky, D.H., 1978. Geometric method for measuring
#' body surface area: a height-weight formula validated in infants, children, and adults.
#' J. Pediatr. 93, 62-66.
#' 
#' Lubin, B., 1987. Reference values in infancy and childhood. In: Nathan, D., Oski, F. (Eds.),
#' Hematology of Infancy and Childhood.
#' 
#' Wambaugh, J.F., Wetmore, B.A., Pearce, R., Strope, C., Goldsmith, R., Sluka, J.P., et al.,
#' 2015. Toxicokinetic triage for environmental chemicals. Toxicol. Sci. 147, 55-67
#' 
#' Ma, M.K., Woo, M.H., Mcleod, H.L., 2002. Genetic basis of drug metabolism. Am. J.
#' Health Syst. Pharm. 59, 2061-2069.
#' 
#' Yasuda, S.U., Zhang, L., Huang, S.M., 2008. The role of ethnicity in variability in response
#' to drugs: focus on clinical pharmacology studies. Clin. Pharmacol. Ther. 84, 417-423.
#' 
#' Rotroff, D.M., Wetmore, B.A., Dix, D.J., Ferguson, S.S., Clewell, H.J., Houck, K.A., et al.,
#' 2010. Incorporating human dosimetry and exposure into high-throughput in vitro
#' toxicity screening. Toxicol. Sci. 117, 348-358.
#' 
#' Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A., Rotroff, D.M., Freeman,
#' K., et al., 2012. Integration of dosimetry, exposure, and high-throughput screening
#' data in chemical toxicity assessment. Toxicol. Sci. 125, 157-174.
#' 
#' Wetmore, B.A., Allen, B., Clewell 3rd, H.J., Parker, T., Wambaugh, J.F., Almond, L.M.,
#' et al., 2014. Incorporating population variability and susceptible subpopulations into
#' dosimetry for high-throughput toxicity testing. Toxicol. Sci. 142, 210-224.
#' 
#' Wetmore, B.A., Wambaugh, J.F., Allen, B., Ferguson, S.S., Sochaski, M.A., Setzer, R.W.,
#' et al., 2015. Incorporating high-throughput exposure predictions with Dosimetryadjusted
#' in vitro bioactivity to inform chemical toxicity testing. Toxicol. Sci. 148,
#' 121-136.
NULL




#' \Sexpr{tools:::Rd_package_title("httk")}
#' 
#' \Sexpr{tools:::Rd_package_description("httk")}
#'  
#' @name httk-package
#' @aliases httk-package httk
#' @docType package
#' @author John Wambaugh, Robert Pearce, Caroline Ring, Gregory Honda, Nisha
#' Sipes, Jimena Davis, Barbara Wetmore, Woodrow Setzer, Mark Sfeir
#' @seealso
#' \href{https://www.epa.gov/chemical-research/computational-toxicology-communities-practice-high-throughput-toxicokinetic-httk}{
#' PowerPoint Presentation: High-Throughput Toxicokinetics (HTTK) R package}
#' 
#' \doi{10.1080/17425255.2021.1935867}{Breen et al. (2021): High-throughput 
#' PBTK models for in vitro to in vivo extrapolation}
#' 
#' \doi{10.18637/jss.v079.i04}{Pearce et al. (2017): httk: R
#' Package for High-Throughput Toxicokinetics}
#' 
#' \doi{10.1021/es501955g}{Armitage et al. (2014): Application of mass balance 
#' models and the chemical activity concept to facilitate the use of in vitro 
#' toxicity data for risk assessment}
#' 
#' \doi{10.1093/toxsci/kfv171}{Incorporating High-Throughput Exposure 
#' Predictions with Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing}
#' 
#' \doi{10.1093/toxsci/kfv118}{Wambaugh et al. (2015):
#' Toxicokinetic Triage for Environmental Chemicals}
#' 
#' \doi{10.1007/s10928-017-9548-7}{Pearce et al. (2017):
#' Evaluation and calibration of high-throughput predictions of chemical
#' distribution to tissues}
#' 
#' \doi{10.1016/j.envint.2017.06.004}{Ring et al. (2017):
#' Identifying populations sensitive to environmental chemicals by simulating
#' toxicokinetic variability}
#' 
#' \doi{10.1021/acs.est.7b00650}{Sipes et al. (2017): An
#' Intuitive Approach for Predicting Potential Human Health Risk with the Tox21
#' 10k Library}
#' 
#' \doi{10.1093/toxsci/kfy020}{Wambaugh et al. (2018):
#' Evaluating In Vitro-In Vivo Extrapolation of Toxicokinetics}
#'
#' \doi{10.1371/journal.pone.0217564}{Honda et al. (2019):
#' Using the concordance of in vitro and in vivo data to evaluate extrapolation 
#' assumptions}
#' 
#' \doi{10.1093/toxsci/kfz205}{Wambaugh et al. (2019):
#' Assessing Toxicokinetic Uncertainty and Variability in Risk Prioritization}
#' 
#' \doi{10.1038/s41370-020-0238-y}{Linakis et al. (2020):
#' Development and evaluation of a high-throughput inhalation model for organic 
#' chemicals}
#' 
#' \href{https://www.epa.gov/chemical-research/rapid-chemical-exposure-and-dose-research}{
#' The U.S. EPA ExpoCast (Exposure Forecasting) Project}
#' @keywords package
NULL