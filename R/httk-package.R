

#' Smoothed age distributions by race and gender.
#' 
#' Distributions of ages in months, computed from NHANES data smoothed using
#' survey::svysmooth(), for each combination of race/ethnicity and gender.
#' 
#' 
#' @name age_dist_smooth
#' @docType data
#' @format A data.table object with three variables: \describe{
#' \item{list("gender")}{Gender: Male or Female}
#' \item{list("reth")}{Race/ethnicity} \item{list("smth")}{A list of
#' \code{svysmooth} objects, each encoding a weighted smoothed distribution of
#' ages.}}
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords data httk-pop
NULL





#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#' 
#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#' 
#' 
#' @name armitage_input
#' @docType data
#' @format A data frame with 53940 rows and 10 variables: \describe{
#' \item{MP}{} \item{MW}{} \item{casrn}{} \item{compound_name}{} \item{gkaw}{}
#' \item{gkow}{} \item{gswat}{} }
#' @author Greg Honda
#' @references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol.
#' 2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#' 
#' Honda, Gregory S., et al. "Using the Concordance of In Vitro and In Vivo
#' Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
#' @source \url{http://www.diamondse.info/}
#' @keywords data
NULL





#' CDC BMI-for-age charts
#' 
#' Charts giving the BMI-for-age percentiles for boys and girls ages 2-18
#' 
#' For children ages 2 to 18, weight class depends on the BMI-for-age
#' percentile. \describe{ \item{Underweight}{<5th percentile} \item{Normal
#' weight}{5th-85th percentile} \item{Overweight}{85th-95th percentile}
#' \item{Obese}{>=95th percentile} }
#' 
#' @name bmiage
#' @docType data
#' @format A data.table object with variables \describe{
#' \item{list("Sex")}{'Male' or 'Female'} \item{list("Agemos")}{Age in months}
#' \item{list("L")}{LMS parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{ }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("M")}{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("S")}{LMS
#' parameters; see \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}
#' \item{list("P3")}{BMI percentiles}\item{, }{BMI percentiles}\item{ }{BMI
#' percentiles}\item{list("P5")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P10")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P25")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P50")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P75")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P85")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{ }{BMI percentiles}\item{list("P90")}{BMI
#' percentiles}\item{, }{BMI percentiles}\item{list("P95")}{BMI
#' percentiles}\item{, and }{BMI percentiles}\item{list("P97")}{BMI
#' percentiles}}
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source \url{www.cdc.gov/growthcharts/percentile_data_files.htm}
#' @keywords data httk-pop
NULL





#' Parameter Estimates from Wambaugh et al. (2018)
#' 
#' This table includes 1 and 2 compartment fits of plasma concentration vs time
#' data aggregated from chem.invivo.PK.data, performed in Wambaugh et al. 2018.
#' Data includes volume of distribution (Vdist, L/kg), elimination rate (kelim,
#' 1/h), gut absorption rate (kgutabs, 1/h), fraction absorbed (Fgutabs), and
#' steady state concentration (Css, mg/L).
#' 
#' 
#' @name chem.invivo.PK.aggregate.data
#' @docType data
#' @format data.frame
#' @author John Wambaugh
#' @source Wambaugh et al. 2018 Toxicological Sciences, in press
#' @keywords datasets
NULL





#' Chemical membership in different research projects
#' 
#' A static list of lists identifying chemical membership in different research
#' projects. While it is our intent to keep these lists up-to-date, the
#' information here is only for convenience and should not be considered to be
#' definitive.
#' 
#' 
#' @name chem.lists
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
#' http://www.cdc.gov/nchs/nhanes.htm.
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.data
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
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.summary.data
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
#' @keywords datasets
NULL





#' Physico-chemical properties and in vitro measurements for toxicokinetics
#' 
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models.
#' 
#' 
#' @name chem.physical_and_invitro.data
#' @docType data
#' @format A data.frame containing 565 rows and 33 columns.
#' @author John Wambaugh
#' @references DSStox database (http:// www.epa.gov/ncct/dsstox
#' 
#' EPI Suite, http://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#' 
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
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
#' Obach, R. S., Lombardo, F. and Waters, N. J. (2008). Trend analysis of a
#' database of intravenous pharmacokinetic parameters in humans for 670 drug
#' compounds. Drug Metabolism and Disposition 36(7), 1385-405,
#' 10.1124/dmd.108.020479.
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
#' @keywords datasets
NULL





#' Howgate 2006
#' 
#' This data set is only used in Vignette 5.
#' 
#' 
#' @name howgate
#' @docType data
#' @keywords datasets
NULL




#' httkpop: Virtual population generator for HTTK.
#' 
#' The httkpop package generates virtual population physiologies for use in
#' population TK.
#' 
#' 
#' @name httkpop
#' @aliases httkpop httkpop-package
#' @docType package
#' @section Main function to generate a population:
#' 
#' If you just want to generate a table of (chemical-independent) population
#' physiology parameters, use \code{\link{httkpop_generate}}.
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords httk-pop
NULL





#' Johnson 2006
#' 
#' This data set is only used in Vignette 5.
#' 
#' 
#' @name johnson
#' @docType data
#' @keywords datasets
NULL





#' Reference tissue masses and flows from tables in McNally et al. 2014.
#' 
#' Reference tissue masses, flows, and marginal distributions from McNally et
#' al. 2014.
#' 
#' 
#' @name mcnally_dt
#' @docType data
#' @format A data.table with variables: \describe{\item{list("tissue")}{Body
#' tissue} \item{list("gender")}{Gender: Male or Female}
#' \item{list("mass_ref")}{Reference mass in kg, from Reference Man}
#' \item{list("mass_cv")}{Coefficient of variation for mass}
#' \item{list("mass_dist")}{Distribution for mass: Normal or Log-normal}
#' \item{list("flow_ref")}{Reference flow in L/h, from Reference Man}
#' \item{list("flow_cv")}{Coefficient of variation for flow (all normally
#' distributed)} \item{list("height_ref")}{Reference heights (by gender)}
#' \item{list("CO_ref")}{Reference cardiac output by gender}
#' \item{list("flow_frac")}{Fraction of CO flowing to each tissue:
#' \code{flow_ref}/\code{CO_ref}}}
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source McNally K, Cotton R, Hogg A, Loizou G. "PopGen: A virtual human
#' population generator." Toxicology 315, 70-85, 2004.
#' @keywords data httk-pop
NULL





#' Pre-processed NHANES data.
#' 
#' NHANES data on demographics, anthropometrics, and some laboratory measures,
#' cleaned and combined into a single data set.
#' 
#' 
#' @name nhanes_mec_svy
#' @docType data
#' @format A survey.design2 object, including masked cluster and strata.
#' Variables are available as a data.table by \code{nhanes_mec_svy$variables}.
#' Variables are as described in NHANES Demographics and Examination
#' documentation, with the exception of: \describe{
#' \item{list("wtmec6yr")}{6-year sample weights for combining 3 cycles,
#' computed by dividing 2-year sample weights by 3.}
#' \item{list("bmxhtlenavg")}{Average of height and recumbent length if both
#' were measured; if only one was measured, takes value of the one that was
#' measured.} \item{list("logbmxwt")}{Natural log of measured body weight.}
#' \item{list("logbmxhtlenavg")}{Natural log of \code{bmxhtlenavg}.}
#' \item{list("weight_class")}{One of Underweight, Normal, Overweight, or
#' Obese.  Assigned using methods in \code{get_weight_class}.} }
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source \url{www.cdc.gov/nhanes/nhanes_questionnaires.htm}
#' @keywords data httk-pop
NULL





#' Published Pharmacokinetic Parameters from Obach et al. 2008
#' 
#' This data set is used in Vignette 4 for steady state concentration.
#' 
#' 
#' @name Obach2008
#' @docType data
#' @format A data.frame containing 670 rows and 8 columns.
#' @references Obach, R. Scott, Franco Lombardo, and Nigel J. Waters. "Trend
#' analysis of a database of intravenous pharmacokinetic parameters in humans
#' for 670 drug compounds." Drug Metabolism and Disposition 36.7 (2008):
#' 1385-1405.
#' @keywords datasets
NULL





#' NHANES Exposure Data
#' 
#' This data set is only used in Vignette 6.
#' 
#' 
#' @name onlyp
#' @docType data
#' @keywords datasets
NULL





#' Partition Coefficient Data
#' 
#' Measured rat in vivo partition coefficients and data for predicting them.
#' 
#' 
#' @name pc.data
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
#' @keywords datasets
NULL





#' Species-specific physiology parameters
#' 
#' This data set contains values from Davies and Morris (1993) necessary to
#' paramaterize a toxicokinetic model for human, mouse, rat, dog, or rabbit.
#' The temperature for each species are taken from Robertshaw et al. (2004),
#' Gordon (1993), and Stammers(1926).
#' 
#' 
#' @name physiology.data
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
#' @keywords datasets
NULL





#' Sipes et al. 2017 data
#' 
#' This table includes data predicted with Simulations Plus' ADMET predictor,
#' used in load_sipes2017, that was used in Sipes et al. 2017. The column names
#' are equivalent to those of chem.physical_and_invitro.data.
#' 
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
#' @keywords datasets
NULL





#' Physico-chemical properties and toxicokinetics, measured values and Sipes et
#' al. (2017)
#' 
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models. The Sipes et al. (2017) predictions have been added.
#' 
#' 
#' @name sipes2017.table
#' @docType data
#' @format A data.frame containing 9211 rows and 47 columns.
#' @author John Wambaugh
#' @references DSStox database (http:// www.epa.gov/ncct/dsstox
#' 
#' EPI Suite, http://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#' 
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
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
#' Obach, R. S., Lombardo, F. and Waters, N. J. (2008). Trend analysis of a
#' database of intravenous pharmacokinetic parameters in humans for 670 drug
#' compounds. Drug Metabolism and Disposition 36(7), 1385-405,
#' 10.1124/dmd.108.020479.
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
#' Sipes, Nisha S., et al. "An Intuitive Approach for Predicting Potential
#' Human Health Risk with the Tox21 10k Library." Environmental Science &
#' Technology 51.18 (2017): 10786-10796.
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
#' @keywords datasets
NULL





#' Smoothing splines for log height vs. age and log body weight vs. age, along
#' with 2-D KDE residuals, by race and gender.
#' 
#' #'Smoothing splines and KDE fits to joint distribution of height and weight
#' residuals pre-calculated from NHANES height, weight, and age data by
#' race/ethnicity and gender.
#' 
#' 
#' @name spline_heightweight
#' @docType data
#' @format A data.table with 6 variables: \describe{ \item{list("g")}{Gender:
#' Male or Female} \item{list("r")}{Race/ethnicity: Mexican American, Other
#' Hispanic, Non-Hispanic White, Non-Hispanic Black, Other}
#' \item{list("height_spline")}{A list of smooth.spline objects, each giving a
#' smoothed relationship between log height in cm and age in months}
#' \item{list("weight_spline")}{A list of smooth.spline objects, each giving a
#' smoothed relationship between log body weight in kg and age in months}
#' \item{list("hw_kde")}{A list of kde objects; each is a 2-D KDE of the
#' distribution of log height and log body weight residuals about the smoothing
#' splines.} }
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords data httk-pop
NULL





#' Smoothing splines for log hematocrit vs. age in months, and KDE residuals,
#' by race and gender.
#' 
#' Smoothing splines and KDE residuals pre-calculated from NHANES hematocrit
#' and age data by race/ethnicity and gender.
#' 
#' 
#' @name spline_hematocrit
#' @docType data
#' @format A data.table with 6 variables: \describe{
#' \item{list("gender")}{Gender: Male or Female}
#' \item{list("reth")}{Race/ethnicity: Mexican American, Other Hispanic,
#' Non-Hispanic White, Non-Hispanic Black, Other} \item{list("hct_spline")}{A
#' list of smooth.spline objects, each giving a smoothed relationship between
#' log hematocrit and age in months} \item{list("hct_kde")}{A list of kde
#' objects; each is a KDE of the distribution of residuals about the smoothing
#' spline.}}
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords data httk-pop
NULL





#' Smoothing splines for log serum creatinine vs. age in months, along with KDE
#' residuals, by race and gender.
#' 
#' #'Smoothing splines and KDE residuals pre-calculated from NHANES serum
#' creatinine and age data by race/ethnicity and gender.
#' 
#' 
#' @name spline_serumcreat
#' @docType data
#' @format A data.table with 6 variables: \describe{
#' \item{list("gender")}{Gender: Male or Female}
#' \item{list("reth")}{Race/ethnicity: Mexican American, Other Hispanic,
#' Non-Hispanic White, Non-Hispanic Black, Other} \item{list("sc_spline")}{A
#' list of smooth.spline objects, each giving a smoothed relationship between
#' log serum creatinine and age in months} \item{list("sc_kde")}{A list of kde
#' objects; each is a KDE of the distribution of residuals about the smoothing
#' spline.} }
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords data httk-pop
NULL





#' Toxcast Data
#' 
#' This data set is used in Vignette 6.
#' 
#' 
#' @name tc.dt
#' @docType data
#' @keywords datasets
NULL





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
#' @name tissue.data
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
#' @keywords datasets
NULL





#' Microtiter Plate Well Descriptions for Armitage et al. (2014) Model
#' 
#' Microtiter Plate Well Descriptions for Armitage et al. (2014) model from
#' Honda et al. (2019)
#' 
#' 
#' @name well_param
#' @docType data
#' @format A data frame with 53940 rows and 10 variables: \describe{
#' \item{area_bottom}{} \item{cell_yield}{} \item{diam}{} \item{sysID}{}
#' \item{v_total}{} \item{v_working}{} \item{well_desc}{} \item{well_number}{}
#' }
#' @author Greg Honda
#' @references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol.
#' 2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#' 
#' Honda, Gregory S., et al. "Using the Concordance of In Vitro and In Vivo
#' Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
#' @source \url{http://www.diamondse.info/}
#' @keywords data httk-pop
NULL





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
#' @name Wetmore.data
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
#' @keywords datasets
NULL





#' Published toxicokinetic predictions based on in vitro data from Wetmore et
#' al. 2012.
#' 
#' This data set overlaps with Wetmore.data and is used only in Vignette 4 for
#' steady state concentration.
#' 
#' 
#' @name Wetmore2012
#' @docType data
#' @format A data.frame containing 13 rows and 15 columns.
#' @references Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A.,
#' Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck,
#' K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and
#' Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput
#' Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125
#' 157-174 (2012)
#' @keywords datasets
NULL





#' WHO weight-for-length charts
#' 
#' Charts giving weight-for-length percentiles for boys and girls under age 2.
#' 
#' For infants under age 2, weight class depends on weight for length
#' percentile. #'\describe{ \item{Underweight}{<2.3rd percentile} \item{Normal
#' }{2.3rd-97.7th percentile}\item{weight}{2.3rd-97.7th percentile}
#' \item{Obese}{>=97.7th percentile} }
#' 
#' @name wfl
#' @docType data
#' @format A data.table object with variables \describe{
#' \item{list("Sex")}{'Male' or 'Female'} \item{list("Length")}{length in cm}
#' \item{list("L")}{LMS parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("M")}{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{ }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("S")}{LMS
#' parameters; see \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}
#' \item{list("P2.3")}{weight percentiles}\item{, }{weight percentiles}\item{
#' }{weight percentiles}\item{list("P5")}{weight percentiles}\item{, }{weight
#' percentiles}\item{list("P10")}{weight percentiles}\item{, }{weight
#' percentiles}\item{list("P25")}{weight percentiles}\item{, }{weight
#' percentiles}\item{list("P50")}{weight percentiles}\item{, }{weight
#' percentiles}\item{list("P75")}{weight percentiles}\item{, }{weight
#' percentiles}\item{list("P90")}{weight percentiles}\item{, }{weight
#' percentiles}\item{ }{weight percentiles}\item{list("P95")}{weight
#' percentiles}\item{, and }{weight percentiles}\item{list("P97.7")}{weight
#' percentiles} }
#' @author Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source
#' \url{http://www.cdc.gov/growthcharts/who/girls_weight_head_circumference.htm}
#' and
#' \url{http://www.cdc.gov/growthcharts/who/boys_weight_head_circumference.htm}
#' @keywords data httk-pop
NULL





#' Smoothed age distributions by race and gender.
#' 
#' Distributions of ages in months, computed from NHANES data smoothed using
#' survey::svysmooth(), for each combination of race/ethnicity and gender.
#' 
#' Distributions of ages in months, computed from NHANES data smoothed using
#' survey::svysmooth(), for each combination of race/ethnicity and gender.
#' 
#' 
#' @name age_dist_smooth
#' @docType data
#' @format A data.table object with three variables: \describe{
#' \item{list("gender")}{Gender: Male or Female}
#' \item{list("reth")}{Race/ethnicity} \item{list("smth")}{A list of
#' \code{svysmooth} objects, each encoding a weighted smoothed distribution of
#' ages.}}
#' @author Caroline Ring
#' 
#' Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' 
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords data httk-pop
NULL





#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#' 
#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#' 
#' Armitage et al. (2014) Model Inputs from Honda et al. (2019)
#' 
#' 
#' @name armitage_input
#' @docType data
#' @format A data frame with 53940 rows and 10 variables: \describe{
#' \item{MP}{} \item{MW}{} \item{casrn}{} \item{compound_name}{} \item{gkaw}{}
#' \item{gkow}{} \item{gswat}{} }
#' @author Greg Honda
#' 
#' Greg Honda
#' @references Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol.
#' 2014, 48, 9770-9779. dx.doi.org/10.1021/es501955g
#' 
#' Honda, Gregory S., et al. "Using the Concordance of In Vitro and In Vivo
#' Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
#' 
#' Armitage, J. M.; Wania, F.; Arnot, J. A. Environ. Sci. Technol. 2014, 48,
#' 9770-9779. dx.doi.org/10.1021/es501955g
#' 
#' Honda, Gregory S., et al. "Using the Concordance of In Vitro and In Vivo
#' Data to Evaluate Extrapolation Assumptions", PloS ONE 14.5 (2019): e0217564.
#' @source \url{http://www.diamondse.info/}
#' 
#' \url{http://www.diamondse.info/}
#' @keywords data
NULL





#' CDC BMI-for-age charts
#' 
#' Charts giving the BMI-for-age percentiles for boys and girls ages 2-18
#' 
#' Charts giving the BMI-for-age percentiles for boys and girls ages 2-18
#' 
#' For children ages 2 to 18, weight class depends on the BMI-for-age
#' percentile. \describe{ \item{Underweight}{<5th percentile} \item{Normal
#' weight}{5th-85th percentile} \item{Overweight}{85th-95th percentile}
#' \item{Obese}{>=95th percentile} }
#' 
#' For children ages 2 to 18, weight class depends on the BMI-for-age
#' percentile. \describe{ \item{Underweight}{<5th percentile} \item{Normal
#' }{5th-85th percentile}\item{weight}{5th-85th percentile}
#' \item{Overweight}{85th-95th percentile} \item{Obese}{>=95th percentile} }
#' 
#' @name bmiage
#' @docType data
#' @format A data.table object with variables \describe{
#' \item{list("Sex")}{'Male' or 'Female'} \item{list("Agemos")}{Age in months}
#' \item{list("L")}{LMS parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{ }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("M")}{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{, }{LMS
#' parameters; see
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}\item{list("S")}{LMS
#' parameters; see \url{www.cdc.gov/growthcharts/percentile_data_files.htm}}
#' \item{list("P3")}{BMI percentiles}\item{, }{BMI percentiles}\item{ }{BMI
#' percentiles}\item{list("P5")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P10")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P25")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P50")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P75")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{list("P85")}{BMI percentiles}\item{, }{BMI
#' percentiles}\item{ }{BMI percentiles}\item{list("P90")}{BMI
#' percentiles}\item{, }{BMI percentiles}\item{list("P95")}{BMI
#' percentiles}\item{, and }{BMI percentiles}\item{list("P97")}{BMI
#' percentiles}}
#' @author Caroline Ring
#' 
#' Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' 
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source \url{www.cdc.gov/growthcharts/percentile_data_files.htm}
#' 
#' \url{www.cdc.gov/growthcharts/percentile_data_files.htm}
#' @keywords data httk-pop
NULL





#' Parameter Estimates from Wambaugh et al. (2018)
#' 
#' This table includes 1 and 2 compartment fits of plasma concentration vs time
#' data aggregated from chem.invivo.PK.data, performed in Wambaugh et al. 2018.
#' Data includes volume of distribution (Vdist, L/kg), elimination rate (kelim,
#' 1/h), gut absorption rate (kgutabs, 1/h), fraction absorbed (Fgutabs), and
#' steady state concentration (Css, mg/L).
#' 
#' 
#' @name chem.invivo.PK.aggregate.data
#' @docType data
#' @format data.frame
#' @author John Wambaugh
#' @source Wambaugh et al. 2018 Toxicological Sciences, in press
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.data
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
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.summary.data
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
#' @keywords datasets
NULL





#' Chemical membership in different research projects
#' 
#' A static list of lists identifying chemical membership in different research
#' projects. While it is our intent to keep these lists up-to-date, the
#' information here is only for convenience and should not be considered to be
#' definitive.
#' 
#' 
#' @name chem.lists
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
#' http://www.cdc.gov/nchs/nhanes.htm.
#' @keywords datasets
NULL





#' Physico-chemical properties and in vitro measurements for toxicokinetics
#' 
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models.
#' 
#' 
#' @name chem.physical_and_invitro.data
#' @docType data
#' @format A data.frame containing 565 rows and 33 columns.
#' @author John Wambaugh
#' @references DSStox database (http:// www.epa.gov/ncct/dsstox
#' 
#' EPI Suite, http://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#' 
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
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
#' Obach, R. S., Lombardo, F. and Waters, N. J. (2008). Trend analysis of a
#' database of intravenous pharmacokinetic parameters in humans for 670 drug
#' compounds. Drug Metabolism and Disposition 36(7), 1385-405,
#' 10.1124/dmd.108.020479.
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
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.data
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
#' @keywords datasets
NULL





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
#' @name chem.invivo.PK.summary.data
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
#' @keywords datasets
NULL





#' Physico-chemical properties and in vitro measurements for toxicokinetics
#' 
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models.
#' 
#' 
#' @name chem.physical_and_invitro.data
#' @docType data
#' @format A data.frame containing 565 rows and 33 columns.
#' @author John Wambaugh
#' @references DSStox database (http:// www.epa.gov/ncct/dsstox
#' 
#' EPI Suite, http://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#' 
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
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
#' Obach, R. S., Lombardo, F. and Waters, N. J. (2008). Trend analysis of a
#' database of intravenous pharmacokinetic parameters in humans for 670 drug
#' compounds. Drug Metabolism and Disposition 36(7), 1385-405,
#' 10.1124/dmd.108.020479.
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
#' @keywords datasets
NULL





#' Howgate 2006
#' 
#' This data set is only used in Vignette 5.
#' 
#' 
#' @name howgate
#' @docType data
#' @keywords datasets
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
#' \href{https://cfpub.epa.gov/si/si_public_record_report.cfm?dirEntryId=311211}{PowerPoint
#' Presentation: High-Throughput Toxicokinetics (HTTK) R package}
#' 
#' \href{https://doi.org/10.18637/jss.v079.i04}{Pearce et al. (2017): httk: R
#' Package for High-Throughput Toxicokinetics}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfv171}{Wetmore et al. (2015):
#' Incorporating High-Throughput Exposure Predictions With Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfv118}{Wambaugh et al. (2015):
#' Toxicokinetic Triage for Environmental Chemicals}
#' 
#' \href{https://doi.org/10.1007/s10928-017-9548-7}{Pearce et al. (2017):
#' Evaluation and calibration of high-throughput predictions of chemical
#' distribution to tissues}
#' 
#' \href{https://doi.org/10.1016/j.envint.2017.06.004}{Ring et al. (2017):
#' Identifying populations sensitive to environmental chemicals by simulating
#' toxicokinetic variability}
#' 
#' \href{https://doi.org/10.1021/acs.est.7b00650}{Sipes et al. (2017): An
#' Intuitive Approach for Predicting Potential Human Health Risk with the Tox21
#' 10k Library}
#' 
#' \href{https://doi.org/10.1093/toxsci/kfy020}{Wambaugh et al. (2018):
#' Evaluating In Vitro-In Vivo Extrapolation of Toxicokinetics}
#'
#' \href{https://doi.org/10.1371/journal.pone.0217564}{Honda et al. (2019):
#' Using the concordance of in vitro and in vivo data to evaluate extrapolation assumptionss}
#' 
#' \href{https://www.epa.gov/chemical-research/rapid-chemical-exposure-and-dose-research}{EPA's
#' ExpoCast (Exposure Forecasting) Project}
#' @keywords package
NULL





#' httkpop: Virtual population generator for HTTK.
#' 
#' The httkpop package generates virtual population physiologies for use in
#' population TK.
#' 
#' The httkpop package generates virtual population physiologies for use in
#' population TK.
#' 
#' 
#' @name httkpop
#' @aliases httkpop httkpop-package
#' @docType package
#' @section Main function to generate a population:
#' 
#' If you just want to generate a table of (chemical-independent) population
#' physiology parameters, use \code{\link{httkpop_generate}}.
#' 
#' If you just want to generate a table of (chemical-independent) population
#' physiology parameters, use \code{\link{httkpop_generate}}.
#' @author Caroline Ring
#' 
#' Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' 
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @keywords httk-pop
NULL





#' Johnson 2006
#' 
#' This data set is only used in Vignette 5.
#' 
#' 
#' @name johnson
#' @docType data
#' @keywords datasets
NULL





#' Reference tissue masses and flows from tables in McNally et al. 2014.
#' 
#' Reference tissue masses, flows, and marginal distributions from McNally et
#' al. 2014.
#' 
#' Reference tissue masses, flows, and marginal distributions from McNally et
#' al. 2014.
#' 
#' 
#' @name mcnally_dt
#' @docType data
#' @format A data.table with variables: \describe{\item{list("tissue")}{Body
#' tissue} \item{list("gender")}{Gender: Male or Female}
#' \item{list("mass_ref")}{Reference mass in kg, from Reference Man}
#' \item{list("mass_cv")}{Coefficient of variation for mass}
#' \item{list("mass_dist")}{Distribution for mass: Normal or Log-normal}
#' \item{list("flow_ref")}{Reference flow in L/h, from Reference Man}
#' \item{list("flow_cv")}{Coefficient of variation for flow (all normally
#' distributed)} \item{list("height_ref")}{Reference heights (by gender)}
#' \item{list("CO_ref")}{Reference cardiac output by gender}
#' \item{list("flow_frac")}{Fraction of CO flowing to each tissue:
#' \code{flow_ref}/\code{CO_ref}}}
#' @author Caroline Ring
#' 
#' Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' 
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source McNally K, Cotton R, Hogg A, Loizou G. "PopGen: A virtual human
#' population generator." Toxicology 315, 70-85, 2004.
#' 
#' McNally K, Cotton R, Hogg A, Loizou G. "PopGen: A virtual human population
#' generator." Toxicology 315, 70-85, 2004.
#' @keywords data httk-pop
NULL





#' Pre-processed NHANES data.
#' 
#' NHANES data on demographics, anthropometrics, and some laboratory measures,
#' cleaned and combined into a single data set.
#' 
#' NHANES data on demographics, anthropometrics, and some laboratory measures,
#' cleaned and combined into a single data set.
#' 
#' 
#' @name nhanes_mec_svy
#' @docType data
#' @format A survey.design2 object, including masked cluster and strata.
#' Variables are available as a data.table by \code{nhanes_mec_svy$variables}.
#' Variables are as described in NHANES Demographics and Examination
#' documentation, with the exception of: \describe{
#' \item{list("wtmec6yr")}{6-year sample weights for combining 3 cycles,
#' computed by dividing 2-year sample weights by 3.}
#' \item{list("bmxhtlenavg")}{Average of height and recumbent length if both
#' were measured; if only one was measured, takes value of the one that was
#' measured.} \item{list("logbmxwt")}{Natural log of measured body weight.}
#' \item{list("logbmxhtlenavg")}{Natural log of \code{bmxhtlenavg}.}
#' \item{list("weight_class")}{One of Underweight, Normal, Overweight, or
#' Obese.  Assigned using methods in \code{get_weight_class}.} }
#' @author Caroline Ring
#' 
#' Caroline Ring
#' @references Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' 
#' Ring, Caroline L., et al. "Identifying populations sensitive to
#' environmental chemicals by simulating toxicokinetic variability."
#' Environment International 106 (2017): 105-118
#' @source \url{www.cdc.gov/nhanes/nhanes_questionnaires.htm}
#' 
#' \url{www.cdc.gov/nhanes/nhanes_questionnaires.htm}
#' @keywords data httk-pop
NULL





#' Published Pharmacokinetic Parameters from Obach et al. 2008
#' 
#' This data set is used in Vignette 4 for steady state concentration.
#' 
#' 
#' @name Obach2008
#' @docType data
#' @format A data.frame containing 670 rows and 8 columns.
#' @references Obach, R. Scott, Franco Lombardo, and Nigel J. Waters. "Trend
#' analysis of a database of intravenous pharmacokinetic parameters in humans
#' for 670 drug compounds." Drug Metabolism and Disposition 36.7 (2008):
#' 1385-1405.
#' @keywords datasets
NULL





#' NHANES Exposure Data
#' 
#' This data set is only used in Vignette 6.
#' 
#' 
#' @name onlyp
#' @docType data
#' @keywords datasets
NULL





#' Partition Coefficient Data
#' 
#' Measured rat in vivo partition coefficients and data for predicting them.
#' 
#' 
#' @name pc.data
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
#' @keywords datasets
NULL





#' Species-specific physiology parameters
#' 
#' This data set contains values from Davies and Morris (1993) necessary to
#' paramaterize a toxicokinetic model for human, mouse, rat, dog, or rabbit.
#' The temperature for each species are taken from Robertshaw et al. (2004),
#' Gordon (1993), and Stammers(1926).
#' 
#' 
#' @name physiology.data
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
#' @keywords datasets
NULL





#' Species-specific physiology parameters
#' 
#' This data set contains values from Davies and Morris (1993) necessary to
#' paramaterize a toxicokinetic model for human, mouse, rat, dog, or rabbit.
#' The temperature for each species are taken from Robertshaw et al. (2004),
#' Gordon (1993), and Stammers(1926).
#' 
#' 
#' @name physiology.data
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
#' @keywords datasets
NULL





#' Sipes et al. 2017 data
#' 
#' This table includes data predicted with Simulations Plus' ADMET predictor,
#' used in load_sipes2017, that was used in Sipes et al. 2017. The column names
#' are equivalent to those of chem.physical_and_invitro.data.
#' 
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
#' @keywords datasets
NULL





#' Physico-chemical properties and toxicokinetics, measured values and Sipes et
#' al. (2017)
#' 
#' This data set contains the necessary information to make basic,
#' high-throughput toxicokinetic (HTTK) predictions for compounds, including
#' Funbound.plasma, molecular weight (g/mol), logP, logMA (membrane affinity),
#' intrinsic clearance(uL/min/10^6 cells), and pKa. These data have been
#' compiled from multiple sources, and can be used to parameterize a variety of
#' toxicokinetic models. The Sipes et al. (2017) predictions have been added.
#' 
#' 
#' @name sipes2017.table
#' @docType data
#' @format A data.frame containing 9211 rows and 47 columns.
#' @author John Wambaugh
#' @references DSStox database (http:// www.epa.gov/ncct/dsstox
#' 
#' EPI Suite, http://www.epa.gov/opptintr/exposure/pubs/episuite.htm
#' 
#' Hilal, S., Karickhoff, S. and Carreira, L. (1995). A rigorous test for
#' SPARC's chemical reactivity models: Estimation of more than 4300 ionization
#' pKas. Quantitative Structure-Activity Relationships 14(4), 348-355.
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
#' Obach, R. S., Lombardo, F. and Waters, N. J. (2008). Trend analysis of a
#' database of intravenous pharmacokinetic parameters in humans for 670 drug
#' compounds. Drug Metabolism and Disposition 36(7), 1385-405,
#' 10.1124/dmd.108.020479.
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
#' Sipes, Nisha S., et al. "An Intuitive Approach for Predicting Potential
#' Human Health Risk with the Tox21 10k Library." Environmental Science &
#' Technology 51.18 (2017): 10786-10796.
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
#' @keywords datasets
NULL



