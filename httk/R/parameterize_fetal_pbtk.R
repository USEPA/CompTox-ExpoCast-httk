#' Parameterize_fetal_PBTK
#' 
#' This function initializes the parameters needed in the functions
#' solve_fetal_pbtk by calling solve_pbtk and adding additional parameters.
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). Currently only a narrow human model is supported. 
#' @param fetal_fup_adjustment Logical indicator of whether to use an adjusted
#' estimate for fetal fup based on the fetal:maternal plasma protein binding
#' ratios presented in McNamara and Alcorn's 2002 study "Protein Binding
#' Predictions in Infants." Defaults to TRUE.
#' @param return.kapraun2019 If TRUE (default) the empirical parameters for the
#' Kapraun et al. (2019) maternal-fetal growth parameters are provided.
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param ... Arguments passed to parameterize_pbtk.
#'
#' @return \item{pre_pregnant_BW}{Body Weight before pregnancy, kg.}
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} \item{Fgutabs}{Fraction
#' of the oral dose absorbed, i.e. the fraction of the dose that enters the
#' gutlumen.} \item{Funbound.plasma}{Fraction of plasma that is not bound.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} \item{hematocrit}{Percent
#' volume of red blood cells in the blood.} \item{Kgut2pu}{Ratio of
#' concentration of chemical in gut tissue to unbound concentration in plasma.}
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.}
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue to
#' unbound concentration in plasma.} \item{Kliver2pu}{Ratio of concentration of
#' chemical in liver tissue to unbound concentration in plasma.}
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue to unbound
#' concentration in plasma.} \item{Krbc2pu}{Ratio of concentration of chemical
#' in red blood cells to unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} \item{million.cells.per.gliver}{Millions
#' cells per gram of liver tissue.} \item{MW}{Molecular Weight, g/mol.}
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid
#' filtered from kidney and excreted.} \item{Rblood2plasma}{The ratio of the
#' concentration of the chemical in the blood to the concentration in the
#' plasma from available_rblood2plasma.} \item{Vgutc}{Volume of the gut per kg body
#' weight, L/kg BW.} \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg
#' BW.} \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vthyroidc}{Volume of the thyroid per kg body weight, L/kg BW.}
#' \item{Kfgut2pu}{Ratio of concentration of chemical in fetal gut tissue to
#' unbound concentration in plasma.} \item{Kfkidney2pu}{Ratio of concentration
#' of chemical in fetal kidney tissue to unbound concentration in plasma.}
#' \item{Kfliver2pu}{Ratio of concentration of chemical in fetal liver tissue
#' to unbound concentration in plasma.} \item{Kflung2pu}{Ratio of concentration
#' of chemical in fetal lung tissue to unbound concentration in plasma.}
#' \item{Kfrest2pu}{Ratio of concentration of chemical in fetal rest of body
#' tissue to unbound concentration in plasma.} \item{Kfbrain2pu}{Ratio of
#' concentration of chemical in fetal brain tissue to unbound concentration in
#' plasma.} \item{Kthyroid2pu}{Ratio of concentration of chemical in fetal
#' thyroid tissue to unbound concentration in plasma.}
#' \item{Kfthyroid2pu}{Ratio of concentration of chemical in fetal thyroid
#' tissue to unbound concentration in plasma.} \item{Kplacenta2pu}{Ratio of
#' concentration of chemical in placental tissue to unbound concentration in
#' maternal plasma.} \item{Kfplacenta2pu}{Ratio of concentration of chemical in
#' placental tissue to unbound concentration in fetal plasma.} 
#'
#' @author Robert Pearce, Mark Sfeir, John Wambaugh, and Dustin Kapraun
#'
#' @references Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' 
#' McNamara PJ, Alcorn J. Protein binding predictions in infants. 
#' AAPS PharmSci. 2002;4(1):E4. doi: 10.1208/ps040104. PMID: 12049488.
#'
#' @keywords Parameter
#'
#' @seealso \code{\link{solve_fetal_pbtk}}
#'
#' @seealso \code{\link{parameterize_pbtk}}
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @seealso \code{\link{kapraun2019}}
#'
#' @examples
#' 
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.cas='80-05-7')
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.name='Bisphenol-A',species='Rat')
#' 
#'  
#' @author Mark Sfeir, Dustin Kapraun, John Wambaugh
#' 
#' @export parameterize_fetal_pbtk
parameterize_fetal_pbtk<- function(
  chem.cas=NULL,
  chem.name=NULL,
  dtxsid = NULL,
  species="Human",
  fetal_fup_adjustment=TRUE,
  return.kapraun2019=TRUE,
  suppress.messages=FALSE,
  ...)
{
  #initialize a parms list for fetal model parameters to output
  parms <- list()

  #
  # MATERNAL PARAMETERS:
  #
    
  #Key ICRP 2002 data for females, corresponding to reference BW of 60 kg:
  ICRP_2002_female_tissue_mass_fractions_data <- 10^-2 * c(
    Vthyroidc = 0.0283,
    Vkidneyc = 0.458,
    Vgutc = 1.90,
    Vliverc = 2.33,
    Vlungc = 1.58,
    Vbrainc = 2.17 
    #^^though brain not in maternal side of model
  )
  
  parms$pre_pregnant_BW <- 61.103 #kg
  
  parms$Vthyroidc <- ICRP_2002_female_tissue_mass_fractions_data[['Vthyroidc']]
  parms$Vkidneyc <- ICRP_2002_female_tissue_mass_fractions_data[['Vkidneyc']]
  parms$Vgutc <- ICRP_2002_female_tissue_mass_fractions_data[['Vgutc']]
  parms$Vliverc <- ICRP_2002_female_tissue_mass_fractions_data[['Vliverc']]
  parms$Vlungc <- ICRP_2002_female_tissue_mass_fractions_data[['Vlungc']]
  Vbrainc_capture <- ICRP_2002_female_tissue_mass_fractions_data[['Vbrainc']]
  #brain volume fraction value not added to parms output
  
  #set density values as they are generally useful in converting between
  #weights and volumes here
  parms$gut_density <- 1.04
  parms$kidney_density <- 1.05
  parms$liver_density <- 1.05
  parms$lung_density <- 1.05
  parms$thyroid_density <- 1.05
  parms$adipose_density <- 0.950
  parms$ffmx_density <- 1.1
  parms$placenta_density <- 1.02
  parms$amnf_density <- 1.01
  parms$brain_density <- 1.04

  
  #Capture Schmitt parameters for maternal case
  maternal_schmitt_parms <- parameterize_schmitt(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid,
    species=species,
    suppress.messages=TRUE)
  
  maternal.blood.pH <- 7.38 #average maternal blood pH value measured by and 
  #reported in K.H. Lee 1972 for over 80 mothers.
  maternal_schmitt_parms$plasma.pH <- maternal.blood.pH
  
  #capture maternal partition coefficients
  maternal_pcs <- predict_partitioning_schmitt(
    parameters = maternal_schmitt_parms,
    model = "fetal_pbtk",
    suppress.messages=TRUE)
  
  #preset our tissue.vols object to pass exact tissue volume information
  #for this model to lump_tissues.R, which may not exactly match
  #values present in tissue.data (from pkdata.xlsx)
  #Values for thyroid, liver, gut, kidney, lung, and brain.
  tissue.vols.list <- list(
    thyroid = parms$Vthyroidc*parms$pre_pregnant_BW/parms$thyroid_density,
    liver = parms$Vliverc*parms$pre_pregnant_BW/parms$liver_density,
    gut = parms$Vgutc*parms$pre_pregnant_BW/parms$gut_density,
    kidney = parms$Vkidneyc*parms$pre_pregnant_BW/parms$kidney_density,
    lung = parms$Vlungc*parms$pre_pregnant_BW/parms$lung_density,
    brain = Vbrainc_capture*parms$pre_pregnant_BW/parms$brain_density)
  
  #Run lump_tissues twice, once to get the mother's partition coefficients, and
  #once for the fetal partition coefficients. There is a slightly different
  #lumping scheme in each case, since we are not modeling the maternal brain.
  lumped_tissue_values_maternal <-
    lump_tissues(
      Ktissue2pu.in = maternal_pcs, 
      species="Human",
      model = "fetal_pbtk",
      tissue.vols = tissue.vols.list,
      tissuelist=list(
        adipose = c("adipose"), 
        gut = c("gut"), 
        liver=c("liver"),
        kidney=c("kidney"), 
        lung=c("lung"), 
        thyroid = c("thyroid"),
        placenta = c("placenta")),
      suppress.messages=TRUE)
  parms <- c(parms, lumped_tissue_values_maternal[substr(names(
    lumped_tissue_values_maternal),1,1) == 'K']) #only add the partition coefficients
  parms$pH_Plasma_mat <- maternal.blood.pH

  #
  # JOINT PARAMETERS:
  #

  #Call parameterize_pbtk function to obtain useful parameters that these
  #models exactly share. 
  pbtk_parms <- parameterize_pbtk(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid,
    species=species,
    suppress.messages=TRUE,
    ...)
  pbtk_parms$BW <- parms$pre_pregnant_BW #Override parameterize_pbtk's
    #body weight listing with average prepregnant case, as scale dosing 
    #requires an entry named 'BW'
  
  
  #Commit the parameters from parameterize_pbtk that aren't redundant with
  #parameters in this lump_tissues run to the output, after trimming away
  #what we don't need (trim the volume fractions V, partition coefficients K,
  #and flows Q):
  pbtk_parms_desired <- 
    pbtk_parms[!( substr(names(pbtk_parms),1,1) %in% c('K','V','Q') )]
  pbtk_parms_desired <- 
    pbtk_parms_desired[!(names(pbtk_parms_desired) %in% c("hematocrit",
            "liver.density", "Rblood2plasma"))] #we don't use a hematocrit value 
  #from parameterize_pbtk, we've already captured our liver density value, and
  #Rblood2plasma and Rfblood2plasma are calculated in the dynamics of the 
  #corresponding .c file using other parameters. 
  
  #capture our desired parameters from parameterize_pbtk in "parms," too
  parms <- c(parms, pbtk_parms_desired)
  
  
  #
  # FETAL PARAMETERS:
  # 



  #set fetal plasma.pH
  fetal.blood.pH <- 7.28 #average fetal value measured by and reported in 
  #K.H. Lee 1972 for over 80 fetuses studied within 30 min of delivery.
  #Before estimating the fetal partition coefficients, we need fetal Schmitt
  #parameters based on the fetal plasma pH
  

  #Now enter the scheme for adjusting fetal fraction of chemical unbound
  #in plasma according to plasma concentration ratios described in McNamara
  #and Alcorn 2002 if desired. "fup" also goes by "Funbound.plasma"...
  if (fetal_fup_adjustment == TRUE)
  {
  #McNamara and Alcorn's "Protein Binding Predictions in Infants" from 2002
  #provides the infant:maternal plasma protein concentration parameters set 
  #here for use in implementing an adjusted Funbound.plasma scheme. We make
  #different estimates for fetal and maternal Funbound.plasma depending
  #on which plasma protein is assumed to predominate in binding the chemical.
    Pinfant2Pmaternal_hsa = 0.777 # S_HSA McNamara (2019)
    Pinfant2Pmaternal_aag = 0.456 # S_AAG McNamara (2019)
  
  #Our assumption of which plasma protein predominates in binding the chemical
  #is based on a general observation of how acids and neutral chemicals 
  #preferentially bind to hsa, while basic chemicals tend to bind to aag. 
  #At the plasma pH of 7.4, if the fraction of a chemical that is in positive
  #ionic form according to the calc_ionization function is greater than 50%,
  #we treat the chemical as a base (which is in its conjugate acid form)
  #and use only the Pinfant2Pmaternal_aag value. Otherwise, we assume the 
  #chemical is an acid and use Pinfant2Pmaternal_hsa. 
  
  #To run the calc_ionization function, though, we need the following values:
    pKa_Donor <- parms$pKa_Donor
    pKa_Accept <- parms$pKa_Accept
 
  #Now let's use calc_ionization to estimate the chemical's charge profile:
    ion <- calc_ionization(
      pH=fetal.blood.pH,
      pKa_Donor=pKa_Donor,
      pKa_Accept=pKa_Accept)
    
    fraction_positive <- ion$fraction_positive
    
    if (fraction_positive > 0.5) 
    {
      Pinfant2Pmaternal <- Pinfant2Pmaternal_aag
    } else Pinfant2Pmaternal <- Pinfant2Pmaternal_hsa
  
    Funbound.plasma <- parms$Funbound.plasma #value of Funbound.plasma for mother
    Fraction_unbound_plasma_fetus <- 
      1 / (1 + Pinfant2Pmaternal*(1 - Funbound.plasma)/Funbound.plasma)
  } else Fraction_unbound_plasma_fetus <- parms$Funbound.plasma
  parms$Fraction_unbound_plasma_fetus <- Fraction_unbound_plasma_fetus
  
 #We'll need some fetal Schmitt params so we can calculate the fetal
  #Kfplacenta2pu, as well as potentially adjust the fraction unbound to plasma
  #estimate in the fetus.
  
  fetal_schmitt_parms <- maternal_schmitt_parms
  fetal_schmitt_parms$plasma.pH <- fetal.blood.pH
  fetal_schmitt_parms$Funbound.plasma <- Fraction_unbound_plasma_fetus
  
  fetal_pcs <- predict_partitioning_schmitt(
    parameters = fetal_schmitt_parms,
    model = "fetal_pbtk",
    suppress.messages=suppress.messages)
  
  #now for the fetus, with the brain included as a compartment. These
  #partition coefficients are based on the same Schmitt parameters except
  #for the plasma pH
  lumped_tissue_values_fetus <- 
    lump_tissues(
      Ktissue2pu.in = fetal_pcs, 
      species="Human",
      model = "fetal_pbtk", 
      tissue.vols = tissue.vols.list,
      tissuelist=list(
        gut = c("gut"), 
        liver=c("liver"), 
        kidney=c("kidney"), 
        lung=c("lung"), 
        thyroid = c("thyroid"), 
        brain = c("brain"),
        placenta = c("placenta")),
      suppress.messages=TRUE)
  lumped_fetal_pcs <- lumped_tissue_values_fetus[substr(names(
    lumped_tissue_values_fetus),1,1) == 'K']
  
  #Now we need to rename the fetal pcs to distinguish them from the mother's
  num_lumped_fetal_pcs <- length(lumped_fetal_pcs)
  names_fetal_pcs_vec <- c() #initialize empty names vec
  
  for (entry in 1:num_lumped_fetal_pcs)
  {
    names_fetal_pcs_vec[entry] <- paste(
      substr(names(lumped_fetal_pcs)[entry],1,1),
      "f",
      substr(names(lumped_fetal_pcs)[entry],
        2,
        nchar(names(lumped_fetal_pcs)[entry])),
      sep = "")
  }
  
  names(lumped_fetal_pcs) <- names_fetal_pcs_vec
  
  parms <- c(parms, lumped_fetal_pcs) #Keep expanding our parms list
  parms$pH_Plasma_fet <- fetal.blood.pH

  

          
# Set appropriate precision:
  parms <- lapply(parms[sort(names(parms))],set_httk_precision)
           

#Now for the many parameters associated with the dynamic physiologic equations
#for pregnancy from Kapraun et al. (2019):
  if (return.kapraun2019) parms <- c(parms, httk::kapraun2019)
 
 return(parms)                             
}
