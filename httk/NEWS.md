# httk 2.5.0 (2024-11-14)
This release accompanies the submission of the new manuscript
"A Simple Physiologically-Based Toxicokinetic Model for Multi-Route <i>In Vitro-In Vivo</i> Extrapolation"
and includes new models incorporating inhalation/exhalation ("sumclearances" and "3compartment2").

## Bug Fixes
* Corrected units in Armitage model documentation (does not impact performance)
* Corrected calculation in 'calc_analytic_css_1comp.R' to reflect that Vdist is the effective plasma (not blood) volume (thanks Shenghong Wang)

## Enhancements
* New model functions for TK models with inhalation/exhalation: 'parameterize_sumclearances', 'parameterize_3comp2', 'solve_3comp2'
* Separate forcing functions are now declared in init.c for models (such as "gas_pbtk") that use the [deSolve forcing functionality](https://tpetzoldt.github.io/deSolve-forcing/deSolve-forcing.html).
* Revised init.c file to try to make it clearer that forcing function handler needs to be defined if forcings are used.
* Revised examples with respect to adding species

# httk 2.4.0 (2024-8-14)
This release accompanies the submission of the new manuscript
"Enabling Transparent Toxicokinetic Modeling for Public Health Risk Assessment"
and includes changes intended to better facilitate development of new HTTK
models through improved model clarity.

In addition we have incorporated comments received
on manuscript "Impact of Gut Permeability on Estimation of 
Oral Bioavailability for Chemicals in Commerce and the Environment" 
provided by reviewers at ALTEX.

## Bug Fixes
* Cleaned up functions for model 3compartment
* Corrected error where non-restrictive clearance option was not working for model pbtk
* Set restrictive.clearance=TRUE by defailt in 'calc_hep_clearance' when model = "unscaled"
* Corrected compartment names for model "gas_pbk" -- "Calv", "Cendexh", and "Cmixexh" were being returned in ppmv units, while "Calvppmv", "Cendexhppmv", and "Cmixexhppmv" were in uM
* Calculation of Fabs corrected for non-human species to follow [Yu and Amidon (1999)](https://doi.org/10.1016/s0378-5173(99)00147-7) using small intestine mean residence time and radius. (Thank you ALTEX reviewers)
* Intestinal flow rate correction to the Qgut model now scales with body weight (rodent Fgut was being predicted way too low) 
* Corrected units of Peff in calculation of Fabs by 'calc_fabs.oral'-- calculations now indicate that more chemicals are poorly absorbed.
* Revised 'calc_css' to handle models with no specified analytic solution
* Revised ionization code in 'armitage_eval' so that pka_donor and pka_accept values now correctly used (thank you Meredith Scherer)
* Corrected bug in 'solve_model' when only specific times requesed and plots=TRUE (thank you Kimberly Troung)
* Corrected bug with 'get_chem_id' when using 'add_chemtable' without DTXSIDs (thank you Marc Beal and Miyuki Breen)
* Corrected bug with 'create_mc_samples' where arguments were not getting passed to 'invitro_mc' (thank you Hsing-Chieh Lin and Weihsueh Chiu)
* Corrected bug in 'solve_model' where tsteps was ignored if times were specified

## Enhancements
* A physiology data table from 'httkpop_generate' can now be passed to 'calc_mc_css' and 'calc_mc_tk' (and 'calc_mc_css' via ...) so that a consistent population can be used across monte carlo runs. See argument httkpop.dt.
* Physico-chemical properties are now retrieved from the CompTox Chemicals Dashboard programmatically using [R package ctxR](https://cran.r-project.org/package=ctxR) (Thank you Paul Kruse)
* 'calc_fabs.oral' now calculates oral uptake rate kgutabs using Caco-2 permeability, according to method of [Lennernas (1997)](https://doi.org/10.1111/j.2042-7158.1997.tb06084.x) (Thank you ALTEX reviewers)
* Revised and changed name of 'get_fabsgut' to 'get_fbio' and modified function to use 'calc_fbio.oral' rather than call oral bioavailability subfunctions directly
* Replaced conversion of human effective gut permeability to rat using [Wahajudin et al. (2011)](https://doi.org/10.1055/s-0031-1296240) regression (Thank you ALTEX reviewers) 
* Loading message displaying version now appears when package is loaded (thank you EPA NAMs class)
* Cleaned up code for various ODE models to make them more consistent and better annotated (added more comments)
* Reordered variables in modelinfo files for consistency so that diff can be used more easily to compare two models
* Modified 'calc_kair' to only allow neutral chemical fraction to partition into air  (thank you Jon Arnot)
* Updated help files describing models
* Default ODE solver tolerances increased to just below significant figures reported by HTTK (we report 4 sig figs, now require the solver to only converge to 5)
* 'solve_[MODEL]' functions now exclusively pass arguments to deSolve through "..."
* New modelinfo file variable default.solver.method can be set -- specifies the default ODE solver approach for deSolve if "lsoda" is not desired
* Revised 'calc_css' to better calculate the day on which steady-state is reached
* Added internal function 'check_model' to provide more informative error messages when key model parameters are missing
* Updated scoping on several functions so that data.tables are handled locally within the functions and not passed by reference.
* Precision of time points added by tsteps argument in 'solve_model' now limited to ten times higher than small.time
* Additional time points now reported in 'solve_model' immediately after dose events to improve plotting

# httk 2.3.1 (2024-3-19)
This patch addresses a number of bugs.

## Bug Fixes
* Argument keepit100 was being improperly ignored by `get_fabsgut`
* Fixed issue where `create_mc_samples` could not handle argument 
***parameters*** being a list (as in, 
parameters=parameterize_steadstate(chem.name="bisphenola"))
* Error messages for `calc_css` now explain that function is only applicable
to dynamical (time-evolving) models and handles errors with other models
(such as 3compartmentss) more gracefully
* Changed Rblood2plasma to Rfblood2plasma for fetal plasma in model fetal_pbtk
(Thank you to Kimberly Troung)
* Liquid densities previous referred to as ppmv for `convert_units` were
actually ppmw. Cannot calculate ppmv without chemical-specific liquid density,
which we do not know.
* Added model descriptor compartment.state to indicate which compartments are
liquid and which are gaseous
* 'calc_analytic_css_3compss' was reporting blood concentrations when asked for plasma
 
## Enhancements
* Changed `armitage_eval` to allow chemical specification by usual arguments
chem.name, chem.cas, and DTXSID. Preserved casrn.vector for backward compatibility.
* Changed `armitage_eval` to allow multiple instances of chemicals (no longer 
using CASRN as row names) -- thank you Katie Paul Friedman for suggestion
* Added Katie Paul Friedman (USEPA) as contributor for long history of 
suggesting refinements and putting up with bugs
* Function `solve_model` now gives warnings when ignoring elements of 
***dosing*** for a given model and route (acceptible dosing.params are now
specified by the modelinfo_[MODEL].R file)

# httk 2.3.0 (2023-12-05)
This version accompanies the submission of manuscript Honda et al. 
"Impact of Gut Permeability on Estimation of Oral Bioavailability for Chemicals 
in Commerce and the Environment". Find the analysis scripts on 
[GitHub](https://github.com/USEPA/comptox-expocast-caco2)

## Bug Fixes
* Added parameter ***plasma.vol*** to one compartment model so that Monte Carlo 
works for non-human species
* Added default units for ***Aexh*** and ***Ainh*** state variables in 
***gas_pbtk*** model so that `calc_css` works for accumulative chemcials
* Corrected the 
[Linakis et al. (2020)](https://doi.org/10.1038/s41370-020-0238-y) vignette to 
reflect that all CvTdb data used there already are in uM
* Corrected ppbv unit conversions in `convert_units`
* Precision of time output in `solve_model` is no longer restricted to four 
significant figures
* Fixed bug with Monte Carlo functions (for example, `calc_mc_oral_equiv`) 
wherein you could not specify the argument parameters to be a table created by 
`create_mc_samples` (thanks Jayme Coyle and Tyler Lalonde)
* Revised `convert_units` to handle multiple molecular weights -- this enables 
`convert_mc_oral_equivalent` to take a table of parameters for Monte Carlo
* Updated the checks and reported error messages in `get_clint` and 
`get_invtroPK_param` to be more informative
* Corrected calculation of mean blood:plasma partition coefficient when 
measured RBlood2plasma is avaialble
* ***Clint*** and ***fup*** are now adjusted for *in vitro* binding when 
`invitrouv=FALSE` (thanks cm16120)

## New Features
* Added *in vitro* measured Caco-2 membrane permeability data for 310 chemicals
allowing characterization of oral bioavailability
* Added new function `load_honda2023` to load QSPR (quantitative 
structure-property relationship model) predictions for Caco-2 membrane 
permeability for ~10,000 chemicals -- QSPR is optimized to detect low 
permeability chemicals and therefore predicts only three values 
(low/medium/high permeability)
* Added new functions `calc_fbio.oral`, `calc_fabs.oral`, and `calc_fgut.oral` 
for calculating systemic bioavailability as $Fbio = Fabs \times Fgut \times Fhep$ 
where first-pass hepatic metabolism was already available from 
`calc_hep_bioavailability`.
* Changed the name of the variable describing fraction absorbed from the gut
prior to first-pass hepatic metabolism to $Fabsgut$ to reflect that
$Fabs$ and $Fgut$ are now modeled separately
(that is, ***Fabsgut = Fabs \times Fgut***).
* Integrated $Fabs$ and $Fgut$ into oral exposure for all TK models and 
integrated into population variability and uncertainty functions within 
`invitro_uv`
* Added new function `benchmark_httk` to compare current function of the 
package against historical performance (stored in data.frame `httk.performance`)
* We now skip over the first five minutes when calculating Cmax in 
`calc_tkstats` to allow PBTK model to distribute iv doses

## Enhancements
* Added QSPR predictions for Fup and Clint for several thousand chemicals using 
the [Dawson et al. (2020)](https://doi.org/10.1021/acs.est.0c06117) models --
accessible from `load_dawson2021` (thank you Alex Fisher and Mike Tornero!)
* Predicted phys-chem properties for most chemicals using 
[OPERA v2.9](https://github.com/NIEHS/OPERA) (updated `armitage_eval` to 
properly convert water solubility from OPERA units)
* Package now requires **ggplot2** -- will gradually shift all plotting from 
base R 
* Returned and updated the 
[Pearce et al. (2017)](https://doi.org/10.1007/s10928-017-9548-7) vignette on 
Evaluation of Tissue Partitioning
* Revised function `convert_units`, expanding the variety of unit conversions 
available -- it is critical to distringuish between state of matter 
(liquid vs. gas)
* Model ***1compartment*** allows volatile chemicals again since clearance is 
amorphous for that model (likely underestimated without exhalation)
* Many manuscript references listed in function documentation were converted to 
a BibTex format from manual insertion of the citations. (thanks Lily Whipple)
* Updated `get_physchem_param` to be case-insensitive
* New ***Clint*** and ***Fup*** data curated from literature by 
[ICF](https://www.icf.com/work/environment) from 
[Black et al. (2021)](https://doi.org/10.1016/j.tox.2021.152819), 
[Williamson et al. (2020)](https://doi.org/10.1124/dmd.120.000131), 
[Zanelli et al. (2012)](https://doi.org/10.1124/dmd.111.042309), 
[Yamagata et al. (2017)](https://doi.org/10.1080/00498254.2016.1222639), and 
[Zanelli et al. (2019)](https://doi.org/10.1080/00498254.2018.1451010) 
(thank you Noelle Sinski and Colin Guider)


# httk 2.2.2 (2023-02-20)
## Bug Fixes
* Corrected analytic steady-state functions for PBTK, 3-compartment, and 
3-compartment steady-state models to return plasma, not blood concentrations 
(via blood:plasma ratio)
* Removed inappropriate second adjustment for binding in intrinsic hepatic 
clearance assay from `cal_hep_clearance` -- 
[Kilford (2008)](https://doi.org/10.1124/dmd.108.020834) adjustment now only 
occurs in parameterization functions* Added new function 
`apply_clint_adjustment` to standardize implementation of adjustment (thanks 
Todor Antonijevic)
* Fixed major bug in `calc_ionization` that caused error when argument pH was a 
vector -- impacts Monte Carlo for ionized compounds
* Corrected equation tracking amount inhaled in gas pbtk model (thanks Cecilia 
Tan)
* Fixed bugs that prevented using Monte Carlo with phys-chem parameters
* Fixed error for species with missing *in vitro* data (thanks Lu En-Hsuan)
* Fixed bug where `solve_model` returned other than requested times when 
argument times was specified (thanks Kimberly Truong)

## New Features
* Added updated vignette from 
[Pearce et al. (2017)](https://doi.org/10.18637%2Fjss.v079.i04): v79i04.R
* Added new vignette on "Introduction to IVIVE"
* Added functions `calc_fup_correction` and `apply_fup_adjustment` to 
consolidate and make uniform application of the 
[Pearce et al. (2017)](https://doi.org/10.1007/s10928-017-9548-7) lipid binding 
adjustment to *in vitro* measured fup
* We now export function `calc_dow` for the distribution coefficient
* New function `calc_ma` separates membrane affinity calculation from 
`parameterize_schmitt`
* New function `calc_kair` separates calculation of blood:air, water:air, and 
mucus:air partition coefficients from `parameterize_gas_pbtk`

## Enhancements
* Added cutoff of logKow/logDow no greater than 6 for `calc_fup_correction` and 
`calc_hep_fu` based on the idea that the *in vitro* assays are not long enough 
to reach concentration ratios greater than 1,000,000 to 1
* Updated `calc_analytic_css_pbtk` to reflect 
[Breen et al. (2022)](https://doi.org/10.1038/s41370-022-00491-0) modification 
to glomerular filtration in the kidney
* `get_cheminfo` now lists required parameters when chemicals are excluded 
(thanks Ben Savage)
* Returned `daily.dose` argument to `calc_mc_css` (still defaults to 1 
mg/kg/day)
* Simplified arguments needed for `calc_mc_css` and `calc_mc_tk` since now 
internally using `do.call` wherever possible to pass arguments
* Revised restrictive.clearance argument for function `solve_model`
* Added inline code comments to `predict_partitioning_schmitt` identifying 
corresponding equations in 
[Schmitt (2008)](https://doi.org/10.1016/j.tiv.2007.09.010)
* Added option `class.exclude` to `get_cheminfo` -- defaults to `TRUE`, but if 
`FALSE` then chemical classes are not excluded on the basis of specified model
* Updated various function documentation

# httk 2.2.1 (2022-09-24)

This minor update removes UTF-8 characters from the package and changes the
calculation of ***kUrt*** on line 292 of `model_gas_pbtk.c` to reduce 
vulnerability to machine precision errors.

# httk 2.2.0 (2022-09-08)
This version accompanies the submission of the [Breen et al. manuscript 
"Simulating Toxicokinetic Variability to Identify Susceptible and Highly 
Exposed Populations"](https://doi.org/10.1038/s41370-022-00491-0)

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
  creatinine, age, sex, and race (black/non-black): set `race factor` to 1 by 
  default (that is, treat all simulated adults as "non-black" for purposes of GFR 
  estimation), to reflect recent changes in clinical practice. (Control this 
  behavior with `httkpop_generate()` argument `ckd_epi_race_factor`)
  * Add residual variability to GFR estimated using CKD-EPI equation, by 
  default. (Control whether to add residual variability using 
  `httkpop_generate()` argument `gfr_resid_var`) 
* Phys-chem properties:
  * Replaced ***pKa*** values from 
  [Strope et al. (2018)](<https://doi.org/10.1016/j.scitotenv.2017.09.033>) with 
  OPERA (v2.7) predictions (<https://github.com/kmansouri/OPERA>)
    * This may slightly change all predictions chemicals that are ionized in 
    tissue.
* PBTK model equations:
  * Revised renal clearance to be GFR x [Unbound concentration in arterial 
  plasma] 
  (previously it was GFR x [Unbound conc in kidney plasma])
* Miscellaneous:
  * Added suggestion message to set `default.to.human=TRUE` when rat ***fup*** 
  is 0 (Thanks Jim Sluka)
  * Added wrapper functions (`get_wetmore...`) for backward compatibility 
  (Thanks Jim Sluka)
  * Updated `invitro_mc` to remove inconsistencies and correct handling of 
  ***fup*** where median is zero but upper 95th is non-zero
  * Added internal function `remd0non0u95` to draw random numbers such that the 
  median is zero and the upper 97.5th quantile is non-zero, taking limit of 
  detection into account
  * Revised and expanded documentation for `calc_mc_css` and 
  `calc_mc_oral_equiv`
* Added logical arguments to `invitro_mc` to directly allow user to turn 
uncertainty and variability off (previously this was done by setting CV to 
NULL)
* If Monte Carlo for the ***fup*** measurement (that is, uncertainty) 
is turned off user may 
choose to provide columns for ***unadjusted.Funbound.plasma*** or  
***fup.mean*** from their own methods
* Moved [Kilford et al. (2008)](<https://doi.org/10.1124/dmd.108.020834>) correction 
for fraction unbound in hepatocyte assay from `calc_hep_clearance` to the 
`parameterize_X` functions and `invitro_mc` -- can now be toggled with argument 
***adjusted.Clint***
* New vignette "Introduction to HTTK" added that includes material from 
[Breen et al. (2021)](<https://doi.org/10.1080/17425255.2021.1935867>)

## Bug Fixes 
* uM units on `calc_mc_css` were incorrectly calculated in v2.1.0 (only), mg/L 
units unaffected, but this will have impacted equivalent doses calculated with 
`calc_mc_oralequiv` (Thank you Marc Beal!)
* User provided DTXSID chemical identifiers were not passed appropriately in the 
`calc_half_life` and prohibited the ability to obtain steady state parameters.
* Error fixed in `create_mc_samples` related to `default.to.human` argument not 
being passed to `parameterize_schmitt`


# httk 2.1.0 (2022-03-26)
This version accompanies the submission of the 
[Kapraun et al. manuscript "Evaluation of a Rapid, Generic Human Gestational
Dose Model"](https://doi.org/10.1016/j.reprotox.2022.09.004)

## New Features
* New HT-PBTK model added as described by 
[Kapraun et al. (submitted)](https://doi.org/10.1016/j.reprotox.2022.09.004) 
including functions `solve_fetal_pbtk` and `parameterize_fetal_pbtk`
* QSAR predicted chemical-specific plasma protein unbound plasma fraction 
(***fup***) and intrinsic hepatic clearance (***Clint***) values data from 
[Dawson et al. (2021)](<https://doi.org/10.1021/acs.est.0c06117>) is now 
included as Dawson2021 and can be added with the new function: `load_dawson2021`
* QSAR predicted chemical-specific plasma protein unbound plasma fraction 
(***fup***) and intrinsic hepatic clearance values (***Clint***) data from 
[Pradeep et al. (2020)](<https://doi.org/10.1016/j.comtox.2020.100136>) is now 
included as Pradeep2020 and can be added with the new function: 
`load_pradeep2020`
* Added function `calc_halflife` (thank you Imran Shah)

## Enhancements
* Updated `predict_partitioning_schmitt` removing the hard coded predicted 
***fup*** regression values from 
[Pearce et al. (2017)](<https://doi.org/10.1007/s10928-017-9548-7>) and created 
stand-alone data matrix ***pearce2017regression*** read in by the function.
* Internal reusable function `convert_units` added to ensure consistency in 
unit conversions across functions
* Units corrected for gas_pbtk model to more naturally handle ppmv (parts per 
million by volume) and uM
* Reworked code for `predict_partitioning_schmitt` -- now we read list of 
tissues needed for a model from `modelinfo_X.R` variable ***alltissues***
* Further revised documentation to 
[Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>) functions 
(thank you Madison Feshuk)
* Expanded documentation for function `get_cheminfo` and table 
***chem.phys_and_invitro.data*** (thank you Lynne Haber and Mark Bradley)  
* Expanded example for `add_chemtable` to address ionization (thank you Johann Fribl)
* Added ***Clint*** data from Dawson (2021) training set (CHEMBL)
* Revised `get_cheminfo` to incorporate a chemical class filter to remove 
"PFAS" compounds for all models, except ***3compartmentss***, based on 
[Wambaugh et al.(2015)](<https://doi.org/10.1093/toxsci/kfv118>).

## Bug Fixes 
* Corrected swapped ***area_bottom*** values in table ***well_param*** for the 
[Armitage](<https://doi.org/10.1021/es501955g>) model. 
(thank you Todor Antonijevic) 
*  Contribution from Todor Antonijevic:
  * ***this.conc_ser_alb***, ***this.conc_ser_lip***, and ***this.Vdom*** added 
  to the list of arguments.
  * the volume of headspace calculated as in 
  [Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>).
  * the volume of medium calculated as in 
  [Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>).
  * ***f_ratio*** calculated as in 
  [Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>)
  * ***kow*** added in the denominator of ***cwat***, that is 
  ***kowP_domf_oc * Vdom***
* Corrected major bug introduced in 2.0.0 (vectorization of `calc_ionization`) 
that caused ***pKa`s*** to be ignored in many cases (thank you Wu Yaoxing)
* Corrected monkey cardiac output (thank you Peter Egeghy)
* Corrected rabbit plasma volume and total body water (thank you Jo Nyffeler)

# httk 2.0.4 (2021-05-10)

## Enhancements
* Sarah Davidson is new lead HTTK software engineer (thank you Mark Sfeir!)
* Added Xiaoqing Chang and Shannon Bell as contributors thanks to substantial 
efforts improving the package
* Changed DESCRIPTION to indicate LazyDataCompression is xz
* Revised and expanded documentation for functions related to 
[Armitage et al. 2014](<https://doi.org/10.1021/es501955g>) *in vitro* 
distribution model -- armitage_eval() and armitage_estimate_sarea()
* Revised documentation to several functions missing value description 
(thank you Julia Haider and Gregor Seyer)
* Revised examples where arguments had changed (thank you Julia Haider)
* Revised and expanded documentation for functions related to 
[Armitage et al. 2014](<https://doi.org/10.1021/es501955g>) *in vitro* 
distribution model -- armitage_eval() and armitage_estimate_sarea()
* Revised `get_cheminfo` behavior to change chemical hepatic clearance values 
where p-value is not consistent with decrease (p-value > 
***clint.pvalue.threshold***, default 0.05) to zero.
* Revised `get_cheminfo` behavior to remove fraction unbound in plasma values 
if credible interval spans from < 0.1 to > 0.9 (turn off with 
***fup.ci.cutoff=FALSE***).
* Revised `get_cheminfo` to include ***median.only*** argument allowing 
confidence intervals to be removed for chemical intrinsic hepatic clearance 
(***Clint***) values and fraction unbound in plasma (***fup***) values where 
they exist (turn on with median.only=TRUE).
*  Revised `get_cheminfo` to filter volatile compounds using Henry`s law 
constant for all models, excluding the ***gas_pbtk*** model.

## Bug Fixes 
* Fixed problems with ***Clint*** values reported from 
[Wood et al. 2017](<https://doi.org/10.1124/dmd.117.077040>), fraction unbound 
in hepatocyte assay adjustment was being applied twice (thank you Xiaoqing 
Chang)
* Fixed problems with clearance from source "Ito/Riley": "not determined" was 
mistakenly being interpreted as "0" rather than not measured (thank you 
Xiaoqing Chang)

# httk 2.0.3 (2020-09-25)

## Enhancements
* Updated literature chemical-specific human and rat *in vitro* data:
  * Revised human and rat VOC chemical numbers to match 
  [Linakis et al. (2020)](<https://doi.org/10.1038/s41370-020-0238-y>)
  * Replaced "Obach" human pharmaceutical data from 
  [Obach et al. (2008)](<https://doi.org/10.1124/dmd.108.020479>) with data 
  from [Lombardo et al. (2018)](<https://doi.org/10.1124/dmd.118.082966>)
  * Added new human data from EU JRC 
([Paini et al., 
2020](https://data.jrc.ec.europa.eu/dataset/a2ff867f-db80-4acf-8e5c-e45502713bee))
  * Steady-state model ***3compartmentss***) now works for 1016 chemicals in 
  human and 212 in rat
* Renamed `calc_stats` to `calc_tkstats` -- `calc_stats` remains temporarily 
but calls `calc_tkstats`
* Added warnings to deprecated function names `calc_stats` and 
`calc_hepatocyte_clearance`
* Revised how default.to.human works, so that `get_cheminfo` and 
`parameterize_schmitt` now handle odd cases (like species is zero but human is 
not) better
* Argument ***info*** for `get_cheminfo` is now case insensitive
* `add_chemtable` (really internal function `augment.table`) changed to enforce 
significant figures (default 4)
* OPERA phys-chem properites provided by CompTox Chemicals Dashboard have been 
slightly revised
* Updated documentation to well parameters for 
[Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>) model 
(thank you Katie Paul-Friedman and Greg Honda)
* added `allow.na` argument to `add_chemtable` so that values can be 
deleted (thanks Nisha Sipes)

## Bug Fixes 
* Fixed logic statement in solve_model to eliminate warning
* Problem with `create_mc_samples` not setting parameter.names variable when 
parameters are passed to it was fixed by Tom Moxon -- thank you!
* `add_chemtable` changed so that pValue and `pValue.Reference` set to `NA` 
when ***Clint*** is changed (thanks Nisha Sipes)
* Output for `calc_tkstats` corrected to display ***Rblood2plasma***
* Minor fix with argument ***suppress.messages*** in `parameterize_pbtk`

# httk 2.0.2 (2020-07-19)

## Enhancements
* Updated default dosing scheme so that a single-time, initial ***dose*** comes 
into effect if no other dosing information is specified, and any dosing info 
that is specified using whatever dosing arguments overrides the default. 
Combinations of dosing arguments can still be specified. 
* Adjusted ***3compartmentss*** model to effectively make use of any passed 
chemical identifier information, especially as it is needed in using 
`get_physchem_param` to look up any missing parameter needed in predicting 
tissue:plasma partition coefficients using `predict_partitioning_schmitt`.

## Bug Fixes 
* Fixed errors in the different models` steady state solver functions to 
support parameter input of key object types, especially lists and compound 
data.tables/data.frames. (thank you, Nisha Sipes)

# httk 2.0.1 (2020-03-02)

## New Features
* New function `set_httk_precision` is now used throughout code to enforce a 
standard set of significant figures (4) and precision (nothing less than 1e-9).

## Enhancements
* Added `calc_hepatic_clearance` wrapper function for `calc_hep_clearance` to 
allow backwards compatibility
* Revised `get_chemid` to not crash in certain cases (thank you, Shannon Bell)
* Revised 
[Linakis et al. (submitted)](https://doi.org/10.1038/s41370-020-0238-y) vignette

## Bug Fixes 
* Fixed output of `calc_mc_oral_equivalent` (was sometimes returning all 
samples unasked, thank you Dan Dawson)

# httk 2.0.0 (2020-02-17)
This version is consistent with consistent with 
[Linakis et al. (submitted) "Development and Evaluation of a High Throughput 
Inhalation Model for Organic 
Chemicals"](https://doi.org/10.1038/s41370-020-0238-y)

## New Features
*	New generic inhalation PBPK model ***gas_pbtk***
* New chemical specific parameters for volatile chemicals have been added:
  * 43 in human
  * 41 in rat
 
## Enhancements
* Significantly rewrote underlying code to allow more easy integration of new 
models. 
(goodbye spaghetti code!)
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
  * Models must be much more thoroughly described now, with all relevant 
  information placed in modelinfo_* files in the /R directory.
  * New model-specific functions introduced:
    * `analytic_css_*`: Model-specific analytic steady-state solution
    * `convert_httkpop_*`: Model-specific functions for converting HTTK-pop 
    biometrics to model parameters
  * Beta testing and bug reports provided by Xiaoqing Chang.

* EPA`s DSSTox Chemical Structure ID`s (DTXSIDs, see 
<https://comptox.epa.gov/dashboard>) now work as chemical identifiers in 
addition to name and CAS.
* Results now truncated to appropriate significant figures (4) and precision 
(1e-12).
* New physiological parameters have been added for monkeys
* To decrease package size the ***load image*** option of `load_sipes2017` was 
eliminated
* Added vignette for Figure 6 from 
 [Frank, et al. (2018) "Defining toxicological tipping points in neuronal 
 network development."](<https://doi.org/10.1016/j.taap.2018.01.017>)
   
# httk 1.10.1 (2019-09-10)

## Enhancements
* Changed all file name starting letters to lowercase.

## Bug Fixes 
* Many bug fixes (thank you David Trudel).
  
# version 1.10.0 (2019-07-12)
This version is consistent with the submitted manuscript [Wambaugh et al. 
"Assessing Toxicokinetic Uncertainty and Variability in Risk
Prioritization"](http://dx.doi.org/10.1093/toxsci/kfz205). 
Major enhancements were made to allow propagation of measurement-specific 
uncertainty
and population variability into IVIVE predictions.

## New Features
* New human experimental measurements of ***fup*** and ***Clint*** are reported 
for 418 and 467 chemicals, respectively.
  * Data on both ***fup*** and ***Clint*** are jointly available for 389 of 
  those chemicals.
* ***Clint*** and ***fup*** values can now be either numeric values (as before) 
or distributions characterized by as "MEDIAN,LOWER95TH,UPPER95TH,PVALUE" for 
***Clint*** and "MEDIAN,LOWER95TH,UPPER95TH" for ***fup***. The code has been 
substantially revised to accommodate this.

## Enhancements
* Added a `minimum.Funbound.plasma` argument since some of the Bayesian estimates 
are very low and at some point the values seem implausible. A value of 0.0001 
was selected since it half the lowest reported measured value. Setting 
`minimum.Funbound.plasma=0` removes this restriction.
* Monte Carlo coefficient of variation for ***Clint*** and ***fup*** has been 
divided into separate values for uncertainty (from measurement) and 
variability (population/genetic). Default values for coefficients of variation 
are `fup.meas.cv=0.4`, `clint.meas.cv=0.3`, `fup.pop.cv=0.3`, 
`clint.pop.cv=0.3`, (from 
[Wambaugh et al, submitted](http://dx.doi.org/10.1093/toxsci/kfz205)). Note that 
most of the new ***fup*** measurements have a lower CV than 0.3.
* All documentation converted to **roxygen2** format.
* Vignette names have been updated to make the related publication clear
* All references to ***fub*** (previously "fraction unbound" but confusing with 
"fraction unbound in blood") have been converted to ***fup*** (the intended 
"fraction unbpund in plasma") where appropriate.
* Rewrote `calc_analytic_css` to handle all models in the same manner.
* Changed argument values "mg" and "mol" for ***output.units*** in 
`calc_mc_oral_equivalent` to "mgpkgpday" and "umolpkgpday". 
(idea from Katie Paul-Friedman)
* Changed httk-pop argument ***fup.censor*** to ***fup.censored.dist***.
* [Armitage et al. (2014)](<https://doi.org/10.1021/es501955g>) model functions 
now work with input of vectors (to allow *data.table* compatibility) or input of 
*data.table*
* Added the physico-chemical parameters needed to run 
[Armitage et al.](<https://doi.org/10.1021/es501955g>) model 
* Updated` honda.ivive` argument functionality, reduced to four options as in 
[Honda et al. (2019)](<https://doi.org/10.1371/journal.pone.0217564>) Figure 8 
panels a-d, changed "plasma.binding" to "bioactive.free.invivo", and exported 
function to allow user to call help file
* Added ***concentration*** as an option set by `honda.ivive`
* Added ***concentration = "tissue"*** as an option to `calc_css` functions
* Added ***bioactive.free.invivo*** as an option to `calc_analytic_css` 
functions, and `calc_mc...` functions
* Function `get_physchem_param`: exported and now works with vectors of CAS 
and/or parameters

## Bug Fixes 
* Corrected error where non-human species were using the incorrect p-value for
***Clint*** when `default.to.human=TRUE` (human p-value is now used). 
(thank you Jason Phillips and Shyam Patel for bug report).
* Shyam Patel (Sciome) identified an error in how flow means were scaled by age 
in httk-pop Monte Carlo sampler.
* Fixed `calc_mc_css` warnings
  
# httk 1.9.2 (2019-04-22)
## Bug Fixes 
* Updated tests to reflect correct model predictions.
* Fixed errors that was causing the ***3compartmentss*** and ***1compartment***
models to not work with Monte Carlo. (thank you Jo Nyffeler for bug report).

# httk 1.9.1 (2019-04-15)

## Bug Fixes 
* Fixed significant errors in `calc_analytic_css` that were causing ***Css*** 
to be over-estimated roughly 10x, therefore reducing the oral equivalent dose 
10x (thank you Nisha Sipes for bug report).
   
# httk 1.9 (2019-02-04)
This version is consistent with the submitted version of 
[Honda et al. "Using the Concordance of *In Vitro* and *In Vivo* Data to Evaluate 
Extrapolation Assumptions"](https://doi.org/10.1371/journal.pone.0217564)

## New Features
* New rat-specific *in vitro* TK data provided for 65 chemicals 
([Honda et al.](https://doi.org/10.1371/journal.pone.0217564))
* New functions for calculating *in vitro* disposition according to the 
[Armitage et al. (2014) model](<https://doi.org/10.1021/es501955g>) 
(thank you James Armitage):
  * `armitage_eval`
  * `armitage_estimate_sarea` 

## Enhancements
* Mark Sfeir is new lead HTTK software engineer (thank you Robert Pearce!)
* Moved code base to Bitbucket internally (thank you Sean Watford and Jeremy 
Dunne)
* Added arguments to IVIVE functions (for example, `calc_mc_css`) to use sets 
of assumptions identified by 
[Honda et al.](https://doi.org/10.1371/journal.pone.0217564) 
(for example, IVIVE="Honda1") (thank you Katie Paul-Friedman)
* Changed all model parameter sets to include physico-chemical properties to 
better facilitate Monte Carlo analysis
* Updated `load_sipes2017` to be much faster by loading an image by default
* Updated help files for Sipes2017 and `load_sipes2017`.
* `get_wetmore_X` functions changed to `get_lit_X`
* `httkpop_bio` exported to user functions 
(function name since changed to `httkpop_biotophys_default`)
* For time point after first dose: bug now corrected when not starting at 
time 0 (thank you Xiaoqing Chang)
* Added figures to help files of `solve_[MODEL]` functions
* Added `hematocrit` argument to `calc_rblood2plasma`
* Changed amounts in model ***1compartment*** to not bescaled by body weight, 
added ***BW*** to parameters for that model thank you Tom Moxon)
* Converted all phys-chem properties except ***pKa*** to values predicted by 
[OPERA (Mansouri et al., 2018)](https://doi.org/10.1186/s13321-018-0263-1) --
see https://github.com/NIEHS/OPERA
* Added missing ***logP*** and ***MW*** for some chemicals using predictions 
from OPERA
* Renamed and added vignettes

## Bug Fixes 
* Corrected mistake in `get_cheminfo` help file: `exlude.fub.zero` defaults to 
`FALSE` for model ***3compartmentss*** and `TRUE` for others
* Corrected (thank you Jason Phillips), updated, and added ***pKa*** values from 
[Strope et al. (2018)](<https://doi.org/10.1016/j.scitotenv.2017.09.033>)
* Corrected `calc_mc_css` bug: species now passed to function `monte_carlo`

# httk 1.8 (2018-01-23)
This version is consistent with the published version of [Pearce et al. 
"Evaluation and calibration of high-throughput predictions of chemical 
distribution to tissues".](https://doi.org/10.1007/s10928-017-9548-7) This 
version contains calibrations for tissue:plasma partition coefficient 
calibration predictions. 

## New Features
* Added arguments to multiple functions 
for whether or not to use new calibration regressions 
(`regression`) and adjusted ***Funbound.plasma*** (`adjusted.Funbound.plasma`).
* Hepatic clearance and plasma binding predictions for ~8000 chemicals from 
Simulations Plus ADMET Predictor used in 
[Sipes et al. (2017)](<https://doi.org/10.1021/acs.est.7b00650>) is now 
included as ***Sipes2017*** and can be added with the new function: 
`load_sipes2017()`.
* New data has been added from an IVIVE evaluation of toxicokinetics 
[Wambaugh et al. 2018](<https://doi.org/10.1093/toxsci/kfy020>)
  * New toxicokinetic concentration vs. time data were added to 
  ***chem.invivo.PK.data*** (full time course) and 
  ***chem.invivo.PK.summary.data*** (TK statistics such as Cmax and AUC on a per
  treatment basis).
  * A new table is included: ***chem.invivo.PK.aggregate data*** (TK statistics
  such as volume of distribution and elimination rate on a per chemical basis)
  * ***kgutabs*** default changed to 2.18.

## Enhancements
* ***Funbound.plasma*** values from 
[Wetmore et al. 2012](https://doi.org/10.1093/toxsci/kfr254) and 
[2013](https://doi.org/10.1093/toxsci/kft012) that were previously rounded to 2 
decimal places are now rounded to 3, resulting in additional compounds with 
measurable ***Funbound.plasma*** that were otherwise assumed to be below the 
limit of detection.
* ***pKa*** data is now readable when values are separated by a semicolon 
rather than a comma. These values were previously misread as neutral.
* Partition coefficients can now be predicted without calculating all of them,
using the tissues argument.
* `calc_mc_css` runs faster when not using httkpop and calculating 
***Rblood2plasma***, now only calculated once.
* ***chem.lists*** is updated, and `is.pharma` has been added as a function.
* `calc_analytic_css` does not recalculate all partition coefficients when 
specifying a tissue.
* ***logP*** values from EPISuite or valued `NA` have been replaced with 
predictions from OPERA where available. 
* First-pass hepatic metabolism has been added in the form of the parameter
***hepatic.bioavailability*** to the models 
***1compartment*** (`parameterize_1comp`) and ***3compartmentss*** 
(`parameterize_steadystate`). Oral doses for these models are now multiplied by
***hepatic.bioavailability*** and ***Fgutabs*** before entering systemic
circulation.
* ***kinhabs*** and ***kdermabs***, both of which were unused in the models, are removed.
* `modelPBTK.c`, the source file for the ***pbtk*** model, now has updated 
variable names, and corresponding changes are made in `solve_pbtk`.
* The time step immediately after addition of dose is added to better capture 
peak concentration for iv dosing.

## Bug Fixes 
* Corrected `calc_mc_css bug`: daily.dose now working as an argument (previously only running as 1).

# httk 1.7 (2017-07-15)
This version is consistent with the JSS publication of 
[Pearce et al. "httk: R Package for High-Throughput 
Toxicokinetics"](https://doi.org/10.18637%2Fjss.v079.i04).

## Bug Fixes 
* Corrected intrinsic clearances for (about 10) compounds from 
[Brown et al. (2007)](https://doi.org/10.1124/dmd.106.011569), 
* Corrected output message from `calc_mc_css`
* Corrected ***Funbound.plasma*** used for predicting partitioning into 
interstitial protein (negligible difference in predictions)
* Corrected bug in calculating ***Rblood2plasma*** in `calc_mc_css`, and added faster
method for calculating ***Rblood2plasma*** for `3compartmentss`.

# httk 1.6 (2017-06-08)
This version includes data and modifications as reported in the recently 
submitted [Pearce et al. paper "Evaluation and Calibration of High-Throughput 
Predictions of Chemical Distribution to 
Tissues"](https://doi.org/10.1007/s10928-017-9548-7). 

## Enhancements

* The [Schmitt (2008)](https://doi.org/10.1016/j.tiv.2007.09.010) method for 
partition coefficients has been modified and calibrated using experimental data. 
* The new method is now default, although the previous approach is available 
(set `regression=FALSE` and `Funbound.plasma.pc.correction=FALSE` for other 
models).  
* The membrane affinity regression has been updated and always used in place of 
the old approach 
* Added function `available_rblood2plasma`
* *in vivo* ***Rblood2plasma*** used when available
*  well-stirred blood correction and restrictive.clearance options added 
* New *in vitro* data from 
[Uchimura et al. (2010)](https://doi.org/10.1002/bdd.711), 
[Brown et al. (2007)](https://doi.org/10.1124/dmd.106.011569) and 
[Pirovano et al. (2016)](https://doi.org/10.1016/j.etap.2016.01.017), 
[Gulden et al. (2002)](https://doi.org/10.1016/S0300-483X(02)00085-9)
* [Tonnelier et al. (2012)](https://doi.org/10.1007/s00204-011-0768-0) 
***Funbound.plasma*** values of 0.005 changed to 0 in 
***chem.physical_and_invitro.data***
* New ***tissue.data table*** with 
[Ruark et al. (2014)](https://doi.org/10.1002/jps.24011) that contains different 
formatting with human and rat specific data
* `parameterize_schmitt`: added `force.human.fub` argument
* added plasma protein and neutral lipid volume fractions to 
***physiology.data*** for use in package
* `calc_mc_css`: defaults to direct resampling. no longer coerces species to 
human when `httkpop=TRUE`. When another species is entered, a warning is thrown 
and the function behaves as if `httkpop=FALSE`.
* updated help file references and examples
* removed temperature from Schmitt parameters
* overwrite 0 values for `Fubound.plasma` when `overwrite=FALSE` in 
`add_chemtable`
* added vignette for generating partition coefficient plots
* added DSSTOX info, new columns: ***DSSTox_Substance_Id***,
***Structure_Formula***, or 
***Substance_Type***.  overwrote: ***MW*** and ***SMILES***
* added ***pc.data*** and ***obach2008*** tables
* httkpop option in `calc_mc_css`: well-stirred correction and new 
`Funbound.plasma` used by default. New partition coefficients used with other models by default.

## Bug Fixes
* corrected `parameterize_3comp` `default.to.human` bug -- no longer always set to false

# httk 1.5 (2017-03-02)
This version is consistent with  [Ring et al. "Identifying populations sensitive 
to environmental chemicals by simulating toxicokinetic 
variability"](https://doi.org/10.1016/j.envint.2017.06.004), which is 
accepted for publication at Environment International. Revisions include models, 
data, and vignettes for "httk-pop" functionality. "httk-pop" allows Monte Carlo 
simulation of physiological variability using data from the National Health and 
Nutrition Examination Survey. 

## New Features
* httk-pop Monte Carlo human variability functionality is the new default, 
although the previous approach is available (set `httkpop=FALSE`).

## Enhancements
* `default.to.human` argument added to `calc_hepatic_clearance` and 
`calc_stats`.
* `calc_hepatic_clearance` and `calc_total_clearance` do not necessarily require 
all parameters.
* Argument `tissue` added to `calc_analytic_css`, `calc_mc_css`, and 
`calc_mc_oral_equiv`, enabling tissue specific calculations in addition to 
plasma.
* `calc_dow` argument `fraction.neutral` changed to `fraction.charged`, thus 
treating Zwitter ions as neutrals
* Multiple iv doses enabled in `solve_*` functions.
* `get_rblood2plasma` function added to retrieve *in vivo* ***Rblood2plasma*** 
from ***chem.physical_and_invitro.data***.

## Bug Fixes 
* Corrected minor bug for `get_cheminfo`
* Corrected bug in `monte_carlo`: Upper bound placed at limit of detection for 
`censored.params` truncated normal distribution.  However, this has no impact 
on the default case where the limit of detection is .01 the mean .005 because 
of the small standard deviation size (.0015). Only large coefficients of 
variation or ***Funbound.plasma*** values close to the limit of detection would 
be affected.

# httk 1.4 (2016-02-03)
This revision incorporates changes suggested by the reviewers of 
[Pearce et al.](https://doi.org/10.18637%2Fjss.v079.i04), which was accepted, 
pending minor revision, in the Journal of Statistical Software (now included in 
vignettes).

* Table name ***PK.physiology.data*** changed to ***physiology.data***.

# httk 1.3 (2015-10-14)
This revision adds ~200 more chemicals (from two recent publications including 
[Wetmore et al. (2015)](https://doi.org/10.1093/toxsci/kfv171) and make several 
small changes to improve usability and stability. 

# httk 1.2 (2015-05-11)
This version is consistent with a newly submitted article 
[Pearce et al. "httk: R Package for High-Throughput Toxicokinetics" to the 
Journal of Statistical Software](https://doi.org/10.18637%2Fjss.v079.i04) 
describing use of this package.

* This revision changes some model parameter names to follow a more systematic 
naming convention. 
* Minor bugs have been corrected. 

# httk 1.1 (2015-03-06)
Initial public (CRAN) release (March 6, 2015)