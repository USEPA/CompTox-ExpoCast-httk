---
title: "News for R package httk"
date: "September 14, 2022"
output: html_document
---

# version 2.2.2 (November, 2022)

* Corrected analytic steady-state functions for PBTK, 3-compartment, and 3-compartment steady-state models to return plasma, not blood concentrations (via blood:plasma ratio)
* `get_cheminfo` now lists required parameters when chemicals are excluded (thanks Ben Savage)
* Returned 'daily.dose' argument to 'calc_mc_css' (still defaults to 1 mg/kg/day)
* FIX GAS MODEL (thanks Cecilia Tan)
* Added updated vignette from Pearce et al. (2017): v79i04.R
* Added new vignette on "Introduction to IVIVE"
* Simplified arguments needed for 'calc_mc_css' and 'calc_mc_tk' since now using 'do.call'
* Fixed bugs that prevented using Monte Carlo with phys-chem parameters
* Revised restrictive.clearance argument for function 'solve_model'

# version 2.2.1 (September, 2022)

This minor update removes UTF-8 characters from the package and changes the
calculation of 'kUrt' on line 292 of 'model_gas_pbtk.c' to reduce vulnerability
to machine precision errors.

# version 2.2.0 (September, 2022)
This version accompanies the submission of the Breen et al. manuscript 
"Simulating Toxicokinetic Variability to Identify Susceptible and Highly 
Exposed Populations"

## Enhancements
* HTTK-Pop population simulator:
  * Replaced HTTK-Pop data from NHANES cycles 2007-2012 with data from most 
  recent 3 NHANES cycles (2013-2018)  
  * Reduced size of data file httkpop.RData. NHANES data now stored as object 
  `mecdt` of class `data.table`, rather than as object `nhanes_mec_svy` of 
  class `survey.design2`. Also, no longer storing pre-calculated spline fits 
  for serum creatinine and hematocrit vs. age, or pre-calculated age 
  distributions (used by HTTK-Pop in virtual-individuals mode); these are now 
  calculated "on the fly".
  * In CKD-EPI equation used to estimate GFR for simulated adults based on serum 
  creatinine, age, sex, and race (black/non-black): set "race factor" to 1 by 
  default (i.e., treat all simulated adults as "non-black" for purposes of GFR 
  estimation), to reflect recent changes in clinical practice. (Control this 
  behavior with `httkpop_generate()` argument "ckd_epi_race_factor")
  * Add residual variability to GFR estimated using CKD-EPI equation, by 
  default. (Control whether to add residual variability using 
  `httkpop_generate()` argument "gfr_resid_var") 
* Phys-chem properties:
  * Replaced pKa values from Strope et al. (2018) 
  (<https://doi.org/10.1016/j.scitotenv.2017.09.033>) with OPERA 
  (v2.7) predictions (<https://github.com/kmansouri/OPERA>)
    * This may slightly change all predictions chemicals that are ionized in 
    tissue.
* PBTK model equations:
  * Revised renal clearance to be GFR x [Unbound conc in arterial plasma] 
  (previously it was GFR x [Unbound conc in kidney plasma])
* Miscellaneous:
  * Added suggestion message to set default.to.human=TRUE when rat Fup is 0 
  (Thanks Jim Sluka)
  * Added wrapper functions (`get_wetmore...`) for backward compatibility 
  (Thanks Jim Sluka)
  * Updated `invitro_mc` to remove inconsistencies and correct handling of fup 
  where median is zero but upper 95th is non-zero
  * Added internal function `remd0non0u95` to draw random numbers such that the 
  median is zero and the upper 97.5th quantile is non-zero, taking limit of 
  detection into account
  * Revised and expanded documentation for `calc_mc_css` and 
  `calc_mc_oral_equiv`
* Added logical arguments to `invitro_mc` to directly allow user to turn 
uncertainty and variability off (previously this was done by setting CV to 
NULL)
* If fup measurement (that is, uncertainty) Monte Carlo is turned off user may 
choose to provide columns for "unadjusted.Funbound.plasma" or  "fup.mean" from 
their own methods
* Moved Kilford (2008) (<https://doi.org/10.1124/dmd.108.020834>) correction 
for fraction unbound in hepatocyte assay from `calc_hep_clearance` to the 
parameterize functions and `invitro_mc` -- can now be toggled with argument 
"adjusted.Clint"
* New vignette "Introduction to HTTK" added including material from Breen et al. 
(2021) (<https://doi.org/10.1080/17425255.2021.1935867>)

## Bug Fixes 
* uM units on `calc_mc_css` were incorrectly calculated in v2.1.0 (only), mg/L 
units unaffected, but this will have impacted equivalent doses calculated with 
`calc_mc_oralequiv` (Thank you Marc Beal!)
* User provided DTXSID chemical identifiers were not passed appropriately in the 
`calc_half_life` and prohibited the ability to obtain steady state parameters.
* Error fixed in `create_mc_samples` related to default.to.human argument not 
being based to `parameterize_schmitt`


# version 2.1.0 (March, 2022)
This version accompanies the submission of the Kapraun et al. manuscript "Evaluation of a Rapid, Generic Human Gestational Dose Model"

## New Features
* New HT-PBTK model added as described by Kapraun et al. (submitted) including functions `solve_fetal_pbtk` and `parameterize_fetal_pbtk`
* QSAR predicted chemical-specific plasma protein unbound plasma fraction and intrinsic hepatic clearance values data from Dawson et al. (2021) (<https://doi.org/10.1021/acs.est.0c06117>) is now included as Dawson2021 and can be added with the new function: `load_dawson2021`
* QSAR predicted chemical-specific plasma protein unbound plasma fraction and intrinsic hepatic clearance values data from Pradeep et al. (2020) (<https://doi.org/10.1016/j.comtox.2020.100136>) is now included as Pradeep2020 and can be added with the new function: `load_pradeep2020`
* Added function `calc_halflife` (thank you Imran Shah)

## Enhancements
* Updated `predict_partitioning_schmitt` removing the hard coded predicted fup regression values from Pearce et al. (2017) (<https://doi.org/10.1007/s10928-017-9548-7>) and created stand-alone data matrix 'pearce2017regression' read in by the function.
* Internal reusable function convert_units() added to ensure consistency in unit conversions across functions
* Units corrected for gas_pbtk model to more naturally handle ppmv (parts per million by volume) and uM
* Reworked code for `predict_partitioning_schmitt` -- now we read list of tissues needed for a model from modelinfo variable alltissues
* Further revised documentation to Armitage et al. (2014) (<https://doi.org/10.1021/es501955g>) functions (thank you Madison Feshuk)
* Expanded documentation for function `get_cheminfo` and table chem.phys_and_invitro.data (thank you Lynne Haber and Mark Bradley)  
* Expanded example for `add_chemtable` to address ionization (thank you Johann Fribl)
* Added Clint data from Dawson (2021) training set (CHEMBL)
* Revised `get_cheminfo` to incorporate a chemical class filter to remove "PFAS" compounds for all models, except "3compartmentss", based on Wambaugh et al. (2015) (<https://doi.org/10.1093/toxsci/kfv118>).

## Bug Fixes 
* Corrected swapped "area_bottom" values in table well_param for the Armitage model. (thank you Todor Antonijevic) 
*  Contribution from Todor Antonijevic:
  * this.conc_ser_alb, this.conc_ser_lip and this.Vdom added to the list of arguments.
  * the volume of headspace calculated as in Armitage et al. 2014.
  * the volume of medium calculated as in Armitage et al. 2014.
  * f_ratio calculated as in Armitage et al. 2014
  * kow added in the denominator of cwat, i.e. kowP_domf_oc*Vdom
* Corrected major bug introduced in 2.0.0 (vectorization of `calc_ionization`) that caused pKa's to be ignored in many cases (thank you Wu Yaoxing)
* Corrected monkey cardiac output (thank you Peter Egeghy)
* Corrected rabbit plasma volume and total body water (thank you Johanna Nyffeler)

# version 2.0.4 (May 7, 2021)

## Enhancements
* Sarah Davidson is new lead HTTK software engineer (thank you Mark Sfeir!)
* Added Xiaoqing Chang and Shannon Bell as contributors thanks to substantial efforts improving the package
* Changed DESCRIPTION to indicate LazyDataCompression is xz
* Revised and expanded documentation for functions related to Armitage et al. 2014 (<https://doi.org/10.1021/es501955g>) in vitro distribution model -- armitage_eval() and armitage_estimate_sarea()
* Revised documentation to several functions missing value description (thank you Julia Haider and Gregor Seyer)
* Revised examples where arguments had changed (thank you Julia Haider)
* Revised and expanded documentation for functions related to Armitage et al. 2014 (<https://doi.org/10.1021/es501955g>) in vitro distribution model -- armitage_eval() and armitage_estimate_sarea()
* Revised `get_cheminfo` behavior to change chemical hepatic clearance values where p-value is not consistent with decrease (p-value > clint.pvalue.threshold, default 0.05) to zero.
* Revised `get_cheminfo` behavior to remove fraction unbound in plasma values if credible interval spans from < 0.1 to > 0.9 (turn off with fup.ci.cutoff=FALSE).
* Revised `get_cheminfo` to include "median.only" argument allowing confidence intervals to be removed for chemical intrinsic hepatic clearance values and fraction unbound in plasma values where they exist (turn on with median.only=TRUE).
*  Revised `get_cheminfo` to filter volatile compounds using Henry's law constant for all models, excluding the "gas_pbtk" model.

## Bug Fixes 
* Fixed problems with Clint values reported from Wood et al. 2017 (<https://doi.org/10.1124/dmd.117.077040>), fraction unbound in hepatocyte assay adjustment was being applied twice (thank you Xiaoqing Chang)
* Fixed problems with clearance from source "Ito/Riley": "not determined" was mistakenly being interpreted as "0" rather than not measured (thank you Xiaoqing Chang)

# version 2.0.3 (August 16, 2020)

## Enhancements
* Updated literature chemical-specific human and rat in vitro data:
  * Revised human and rat VOC chemical numbers to match Linakis et al. (2020) (<https://doi.org/10.1038/s41370-020-0238-y>)
  * Replaced "Obach" human pharmaceutical data from Obach et al. (2008) (<https://doi.org/10.1124/dmd.108.020479>) with data from Lombardo et al. (2018) (<https://doi.org/10.1124/dmd.118.082966>)
  * Added new human data from EU JRC (Paini et al., 2020)
  * Steady-state model now works for 1016 chemicals in human and 212 in rat
* Renamed `calc_stats` to `calc_tkstats` -- `calc_stats` remains temporarily but calls `calc_tkstats`
* Added warnings to deprecated function names `calc_stats` and `calc_hepatocyte_clearance`
* Revised how default.to.human works, so that `get_cheminfo` and `parameterize_schmitt` now handle odd cases (like species is zero but human is not) better
* Argument "info" for `get_cheminfo` is now case insensitive
* `add_chemtable` (really internal function augment.table) changed to enforce significant figures (default 4)
* OPERA phys-chem properites provided by CompTox Chemicals Dashboard have been slightly revised
* Updated documentation to well parameters for Armitage et al. (2014) (<https://doi.org/10.1021/es501955g>) model (thank you Katie Paul-Friedman and Greg Honda)
* added "allow.na"" argument to `add_chemtable` so that values can be deleted (thanks Nisha Sipes)

## Bug Fixes 
* Fixed logic statement in solve_model to eliminate warning
* Problem with `create_mc_samples` not setting parameter.names variable when parameters are passed to it was fixed by Tom Moxon -- thank you!
* `add_chemtable` changed so that pValue and pValue.Reference set to NA when clint is changed (thanks Nisha Sipes)
* Output for `calc_tkstats` corrected to display "Rblood2plasma"
* Minor fix with argument "suppress.messages"" in `parameterize_pbtk`

# version 2.0.2 (July 18, 2020)

## Enhancements
* Updated default dosing scheme so that a single-time, initial "dose" comes into effect if no other dosing information is specified, and any dosing info that is specified using whatever dosing arguments overrides the default. Combinations of dosing arguments can still be specified. 
* Adjusted 3compss model to effectively make use of any passed chemical identifier information, especially as it is needed in using `get_physchem_param` to look up any missing parameter needed in predicting partitioning coefficients using `predict_partitioning_schmitt`.

## Bug Fixes 
* Fixed errors in the different models' steady state solver functions to support parameter input of key object types, especially lists and compound data.tables/data.frames. (thank you, Nisha Sipes)

# version 2.0.1 (February 28, 2020)

## New Features
* New function `set_httk_precision` is now used throughout code to enforce a standard set of significant figures (4) and precision (nothing less than 1e-9).

## Enhancements
* Added `calc_hepatic_clearance` wrapper function for calc_hep_clearance to allow backwards compatibility
* Revised `get_chemid` to not crash in certain cases (thank you, Shannon Bell)
* Revised Linakis et al. (submitted) vignette

## Bug Fixes 
* Fixed output of `calc_mc_oral_equivalent` (was sometimes returning all samples unasked, thank you Dan Dawson)

# version 2.0.0 (February 10, 2020)
This version is consistent with consistent with Linakis et al. (submitted) "Development and Evaluation of a High Throughput Inhalation Model for Organic Chemicals"

## New Features
*	New generic inhalation PBPK model 
* New chemical specific parameters for volatile chemicals have been added:
  * 43 in human
  * 41 in rat
 
## Enhancements
* Rewrote underlying code to allow more easy integration of new models. (goodbye, spaghetti code!)
  * Rewritten functions include:
    * `calc_analytic_css`
    * `calc_mc_css`
    * `convert_httkpop` (renamed from `convert_httk`)
    * `solve_*` model functions
  * Renamed a few httk-pop functions for clarity:
    * `httkpop_biotophys_default` replaces `httkpop_bio`
    * `convert_httkpop` replaces `convert_httk`
  * New functions introduced:
    * `solve_model` (mostly used by `solve_*` model functions)
    * `calc_mc_tk` (performs Monte Carlo simulation using a `solve_*` function)
  * Models must be much more thoroughly described now, with all relevant information placed in modelinfo_* files in the /R directory.
  * New model-specific functions introduced:
    * `analytic_css_*`: Model-specific analytic steady-state solution
    * `convert_httkpop_*`: Model-specific functions for converting HTTK-pop biometrics to model parameters
  * Beta testing and bug reports provided by Xiaoqing Chang.

* EPA's DSSTox Chemical Structure ID's (DTXSIDs, see <http://comptox.epa.gov/dashboard/>) now work as chemical identifiers in addition to name and CAS.
* Results now truncated to appropriate significant figures (4) and precision (1e-12).
* New physiological parameters have been added for monkeys
* To decrease package size the load image option of load_sipes2017 was eliminated
 * Added vignette for Figure 6 from Frank, Christopher L., et al. "Defining toxicological tipping points in neuronal network development." (<https://doi.org/10.1016/j.taap.2018.01.017>)
   
# version 1.10.1 (Septmeber 9, 2019)

## Enhancements
* Changed all file name starting letters to lowercase.

## Bug Fixes 
* Many bug fixes (thank you David Trudel).
  
# version 1.10.0 (July 9, 2019)
This version is consistent with the submitted manuscript Wambaugh et al. 
"Assessing Toxicokinetic Uncertainty and Variability in Risk Prioritization". 
Major enhancements were made to allow propagation of measurement-specific uncertainty
and population variability into IVIVE predictions.

## New Features
* New human experimental measurements of fup and Clint are reported for 418 and 467 chemicals, respectively.
  * Data on both fup and Clint are jointly available for 389 of those chemicals.
* Clint and fup values can now be either numeric values (as before) or distributions characterized by as "MEDIAN,LOWER95TH,UPPER95TH,PVALUE" for Clint and "MEDIAN,LOWER95TH,UPPER95TH" for fup. The code has been substantially revised to accommodate this.

## Enhancements
* Added a minimum.Funbound.plasma since some of the Bayesian estimates are very low and at some point the values seem implausible. A value of 0.0001 was selected since it half the lowest reported measured value. Setting minimum.Funbound.plasma=0 removes this restriction.
* Monte Carlo coefficient of variation for Clint and fup has been divided into separate values for uncertainty (from measurement) and variability (population/genetic). Default values for coefficients of variation are fup.meas.cv=0.4, 
clint.meas.cv=0.3, fup.pop.cv=0.3, clint.pop.cv=0.3, (from Wambaugh et al, submitted). Note that most of the new fup measurements have a lower CV than 0.3.
* All documentation converted to roxygen2 format.
* Vignette names have been updated to make the related publication clear.
* All references to "fub" have been converted to "fup" where appropriate.
* Rewrote `calc_analytic_css` to handle all models in the same manner.
* Changed arguments "mg" and "mol" for output.units in calc_mc_oral_equivalent to "mgpkgpday" and "umolpkgpday". (idea from Katie Paul-Friedman)
* Changed httk-pop argument "fup.censor" to "fup.censored.dist".
* Armitage et al. (2014) (<https://doi.org/10.1021/es501955g>) model functions now work with input of vectors (to work well with data.table) or input of data.table
* Added the physchem parameters needed to run Armitage et al. model 
* Updated honda.ivive, reduced to 4 options as in Honda et al. (2019) (<https://doi.org/10.1371/journal.pone.0217564>) Figure 8 panels a-d, changed "plasma.binding" to "bioactive.free.invivo", and exported function to allow user to call help file
* Added concentration as an option set by honda.ivive
* Added concentration = "tissue" as an option to `calc_css` functions
* Added bioactive.free.invivo as an option to `calc_analytic_css` functions, and `calc_mc...` functions
* Function `get_physchem_param`: exported and now works with vectors of CAS and/or parameters

## Bug Fixes 
* Corrected error where non-human species were using the incorrect p-value for Clint when default.to.human=TRUE (human p-value is now used). (thank you Jason Phillips and Shyam Patel for bug report).
* Shyam Patel (Sciome) identified an error in how flow means were scaled by age in httk-pop Monte Carlo sampler.
* Fixed calc_mc_css warnings
  
# version 1.9.2 (April 22, 2019)
## Bug Fixes 
* Updated tests to reflect correct model predictions.
* Fixed errors that was causing the 3compartmentss and 1compartment models to not work with Monte Carlo. (thank you Johanna Nyffeler for bug report).

# version 1.9.1 (April 15, 2019)

## Bug Fixes 
* Fixed significant errors in `calc_analytic_css` that were causing Css to be over-estimated roughly 10x, therefore reducing the oral equivalent dose 10x (thank you Nisha Sipes for bug report).
   
# version 1.9 (February 4, 2019)
This version is consistent with the submitted version of Honda et al. "Using the Concordance of In Vitro and In Vivo Data to Evaluate Extrapolation Assumptions"

## New Features
* New rat-specific in vitro TK data provided for 65 chemicals. 
* New functions for calculating in vitro disposition according to the Armitage et al. model (<https://doi.org/10.1021/es501955g>) (thank you James Armitage)
  * `armitage_eval`
  * `armitage_estimate_sarea` 

## Enhancements
* Mark Sfeir is new lead HTTK software engineer (thank you Robert Pearce!)
* Moved code base to Bitbucket internally (thank you Sean Watford and Jeremy Dunne)
* Added arguments to IVIVE functions (for example, `calc_mc_css`) to use sets of assumptions identified by Honda et al. (for example, IVIVE="Honda1")(thank you Katie Paul-Friedman)
* Changed all model parameter sets to include physico-chemical properties to better facilitate Monte Carlo analysis
* Updated `load_sipes2017` to be much faster by loading an image by default
* Updated help files for Sipes2017 and `load_sipes2017`.
* `get_wetmore` functions changed to `get_lit`
* httkpop_bio exported to user functions (fx name since changed to 'httkpop_biotophys_default')
* For time point after first dose: bug now corrected when not starting at time 0(thank you Xiaoqing Chang)
* Added figures to help files of `solve_[MODEL]` functions
* Added `hematocrit` argument to `calc_rblood2plasma`
* Made amounts in 1comp model not scaled by body weight, adding BW to parameters for that model thank you Tom Moxon)
* Converted all phys-chem properties except pKa to values predicted by OPERA (Mansouri et al., 2018)
* Added missing logP and MW for some chemicals from OPERA
* Renamed and added vignettes

## Bug Fixes 
* Corrected mistake in `get_cheminfo` help file: exlude.fub.zero defaults to FALSE for 3compartmentss and TRUE for others
* Corrected (thank you Jason Phillips), updated, and added pKa values from Strope et al. (2018) (<https://doi.org/10.1016/j.scitotenv.2017.09.033>)
* Corrected calc_mc_css bug: species passed to monte_carlo function


# version 1.8 (January 23, 2018)
This version is consistent with the published version of Pearce et al. "Evaluation and calibration of high-throughput predictions of chemical distribution to tissues". This version contains calibrations for tissue:plasma partition coefficient calibration predictions. 

## New Features
* Added arguments for whether or not to use new calibration regressions (regression) and adjusted Funbound.plasma (adjusted.Funbound.plasma).
* Hepatic clearance and plasma binding predictions for ~8000 chemicals from Simulations Plus ADMET Predictor used in Sipes et al. (2017) (<https://doi.org/10.1021/acs.est.7b00650>) is now included as Sipes2017 and can be added with the new function: load_sipes2017().
* New data has been added from an IVIVE evaluation of toxicokinetics (Wambaugh et al. 2018 <https://doi.org/10.1093/toxsci/kfy020>)
  * New data were added to chem.invivo.PK.data and chem.invivo.PK.summary.data.
  * A new table is included: chem.invivo.PK.aggregate data
  * kgutabs default changed to 2.18.

## Enhancements
* Funbound.plasma values from Wetmore 2012 and 2013 that were previously rounded to 2 decimal places are now rounded to 3, resulting in additional compounds with measurable Funbound.plasma that were otherwise assumed to be below the limit of detection.
* pKa data is now readable when values are separated by a semicolon rather than a comma. These values were previously misread as neutral.
* Partition coefficients can now be predicted without calculating all of them, using the tissues argument.
* Calc_mc_css runs faster when not using httkpop and calculating Rblood2plasma, now only calculated once.
* chem.lists is updated, and is.pharma has been added as a function.
* calc_analytic_css does not recalculate all partition coefficients when specifying a tissue.
* logP values from EPISuite or valued NA have been replaced with predictions from OPERA where available. 
* hepatic.bioavailability is added as a parameter to the models 1compartment (parameterize_1comp) and 3compartmentss (parameterize_steadystate) and now used with these models (multiplied by the dose and Fgutabs).
* kinhabs and kdermabs, both of which were unused in the models, are removed.
* modelPBTK.c, the source file for the pbtk model, now has updated variable names, and corresponding changes are made in solve_pbtk.
* The time step immediately after addition of dose is added to better capture peak concentration for iv dosing.

## Bug Fixes 
* Corrected calc_mc_css bug: daily.dose now working as an argument (previously only running as 1).

# version 1.7 (July 15, 2017)
This version is consistent with the JSS publication of Pearce et al. "httk: R Package for High-Throughput Toxicokinetics".

## Bug Fixes 
* Corrected minor bugs including: corrected intrinsic clearances for (about 10) compounds from Brown 2007, corrected output message from calc_mc_css
* Corrected Funbound.plasma used for predicting partitioning into interstitial protein (negligible difference in predictions)
* Corrected bug in calculating Rblood2plasma in calc_mc_css, and added faster method for calculating Rblood2plasma for 3compartmentss.

# version 1.6 (June 8, 2017)
This version includes data and modifications as reported in the recently submitted Pearce et al. paper "Evaluation and Calibration of High-Throughput Predictions of Chemical Distribution to Tissues". 

## Enhancements

* The Schmitt (2008) method for partition coefficients has been modified and calibrated using experimental data. 
  * The new method is now default, although the previous approach is available (set regression=FALSE and Funbound.plasma.pc.correction=FALSE for other models).  
* The membrane affinity regression has been updated and always used in place of the old approach 
* Added function available_rblood2plasma
* In vivo Rblood2plasma used when available
*  well-stirred blood correction and restrictive.clearance options added 
* New in vitro data from Uchimura 2010, brown 2007 and Pirovano 2016, Gulden 2002
*  Tonnelier Funbound.plasma values of 0.005 changed to 0 in 
chem.physical_and_invitro.data
* New tissue.data table with Ruark 2014 that contains different formatting with human and rat specific data
* parameterize_schmitt: added force.human.fub argument
* added plasma protein and neutral lipid volume fractions to physiology.data for use in package
* calc_mc_css: defaults to direct resampling. no longer coerces species to human when `httkpop=TRUE`. When another species is entered, a warning is thrown and the function behaves as if httkpop=FALSE.
* updated help file references and examples
* removed temperature from Schmitt parameters
* overwrite 0 values for `Fubound.plasma when overwrite=FALSE` in add_chemtable
* added vignette for generating partition coefficient plots
* added DSSTOX info, new columns: "DSSTox_Substance_Id","Structure_Formula", or "Substance_Type".  overwrote: MW and SMILES
* added pc.data and obach2008 tables
* httkpop option in calc_mc_css: well-stirred correction and new Funbound.plasma used by default. New partition coefficients used with other models by default.

## Bug Fixes
* corrected parameterize_3comp default.to.human bug: always set to false

# version 1.5 (March 3, 2017)
This version is consistent with  Ring et al. "Identifying populations sensitive 
to environmental chemicals by simulating toxicokinetic variability", which is 
accepted for publication at Environment International. Revisions include models, 
data, and vignettes for "httk-pop" functionality. "httk-pop" allows Monte Carlo 
simulation of physiological variability using data from the National Health and 
Nutrition Examination Survey. 

## New Features
* httk-pop Monte Carlo human variability functionality is the new default, although the previous approach is available (set `httkpop=FALSE`).

## Enhancements
* `default.to.human` argument added to `calc_hepatic_clearance` and `calc_stats`.
* `calc_hepatic_clearance` and calc_total_clearance do not necessarily require all parameters.
* Argument "tissue" added to `calc_analytic_css`, `calc_mc_css`, and 
`calc_mc_oral_equiv`, enabling tissue specific calculations in addition to plasma.
* `calc_dow`: fraction neutral argument changed to fraction charged, thus treating Zwitter ions as neutrals
* Multiple iv doses enabled in `solve_*` functions.
* `get_rblood2plasma` function added to retrieve in vivo Rblood2plasma from "chem.physical_and_invitro.data".

## Bug Fixes 
* Corrected minor bug for `get_cheminfo`
* Corrected bug in `monte_carlo`: Upper bound placed at limit of detection for censored params truncated normal distribution.  However, this has no impact on the default case where the limit of detection is .01 the mean .005 because of the small standard deviation size (.0015). Only large coefficients of variation or `Funbound.plasma` values close to the limit of detection would be affected.

# version: 1.4 (February 3, 2016)
This revision incorporates changes suggested by the reviewers of Pearce et al. "httk: R Package for High-Throughput Toxicokinetics", which was accepted, pending minor revision, in the Journal of Statistical Software (now included in vignettes). 
* Table name "PK.physiology.data" changed to "physiology.data".

# version 1.3 (October 14, 2015)
This revision adds ~200 more chemicals (from two recent publications including Wetmore et al. 2015) and make several small changes to improve usability and stability. 

# version 1.2 (May 11, 2015)
This version is consistent with a newly submitted article Pearce et al. "httk: R Package for High-Throughput Toxicokinetics" to the Journal of Statistical SoftwareJ describing use of this package.

* This revision changes some model parameter names to follow a more systematic naming convention. 
* Minor bugs have been corrected. 

# Version 1.1
Initial public (CRAN) release (March 6, 2015).
