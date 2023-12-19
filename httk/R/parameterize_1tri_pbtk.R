#' Parameterize_1tri_PBTK
#' 
#' This function initializes the parameters needed in the functions
#' solve_1tri_pbtk by calling solve_pbtk and adding additional parameters.
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
#' @param Kconceptus2pu Ratio of concentration of chemical in the 'conceptus'
#' compartment of the model to unbound concentration in plasma. 
#' @param return.kapraun2019 If TRUE (default) empirical parameters from 
#' Kapraun et al. (2019) necessary for defining the model are provided.
#' This is a subset of the httk::kapraun2019 list object with additional parameters. 
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param ... Arguments passed to parameterize_pbtk.
#'
#' @return \item{pre_pregnant_BW}{Body Weight before pregnancy, kg.}
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the dose that enters the
#' gutlumen.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} 
#' \item{Kadipose2pu}{Ratio of concentration of chemical in adipose tissue to unbound concentration in plasma.}
#' \item{Kbrain2pu}{Ratio of concentration of chemical in brain tissue to unbound 
#' concentration in plasma.} 
#' \item{Kconceptus2pu}{Ratio of concentration of chemical in "conceptus" 
#' compartment to unbound concentration in plasma. This parameter is user-defined.} 
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound 
#' concentration in plasma.} 
#' \item{kgutabs}{Rate that chemical enters the gut from 
#' gutlumen, 1/h.} 
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue 
#' to unbound concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to unbound concentration in plasma.}
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue to unbound
#' concentration in plasma.} 
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} 
#' \item{Kthyroid2pu}{Ratio of concentration of chemical in thyroid tissue to unbound concentration in plasma.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.} 
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{pH_Plasma_mat}{pH of the maternal plasma.}
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid
#' filtered from kidney and excreted.} 
#' \item{Vbrainc}{Volume of the brain per kg body weight, L/kg BW.} 
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.} 
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.} 
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vthyroidc}{Volume of the thyroid per kg body weight, L/kg BW.}
#'
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
#' @seealso \code{\link{solve_1tri_pbtk}}
#'
#' @seealso \code{\link{parameterize_1tri_pbtk}}
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
#'  Note: These examples should be worked out after testing the functions. 
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.cas='80-05-7')
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.name='Bisphenol-A',species='Rat')
#' 
#'  
#' @author Mark Sfeir, Dustin Kapraun, John Wambaugh
#' 
#' @export parameterize_1tri_pbtk
parameterize_1tri_pbtk<- function(
  chem.cas=NULL,
  chem.name=NULL,
  dtxsid = NULL,
  species="Human",
  Kconceptus2pu = 1, # set to 1 for now
  return.kapraun2019=TRUE, # this is mostly a subset of httk::kapraun2019 
  suppress.messages=FALSE, # adding Qbrain_percent_{initial,terminal}
  ...)
{
  #initialize a parms list for 1tri model parameters to output
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
    Vbrainc = 2.17 # though brain is not included in maternal side of model
  )
  
  parms$pre_pregnant_BW <- 61.103 #kg
  
  # tissue volume fractions to compute respective Vtissue
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
    model = "1tri_pbtk",
    suppress.messages=TRUE)
  
  #preset our tissue.vols object to pass exact tissue volume information
  #for this model to lump_tissues.R, which may not exactly match
  #values present in tissue.data (from pkdata.xlsx)
  #Values for thyroid, liver, gut, kidney, lung.
  tissue.vols.list <- list(
    thyroid = parms$Vthyroidc*parms$pre_pregnant_BW/parms$thyroid_density,
    liver = parms$Vliverc*parms$pre_pregnant_BW/parms$liver_density,
    gut = parms$Vgutc*parms$pre_pregnant_BW/parms$gut_density,
    kidney = parms$Vkidneyc*parms$pre_pregnant_BW/parms$kidney_density,
    lung = parms$Vlungc*parms$pre_pregnant_BW/parms$lung_density,
    brain = Vbrainc_capture*parms$pre_pregnant_BW/parms$brain_density
    )
  
  #Run lump_tissues twice, once to get the mother's partition coefficients, and
  #once for the fetal partition coefficients. There is a slightly different
  #lumping scheme in each case, since we are not modeling the maternal brain.
  lumped_tissue_values_maternal <-
    lump_tissues(
      Ktissue2pu.in = maternal_pcs, 
      species="Human",
      model = "1tri_pbtk",
      tissue.vols = tissue.vols.list,
      tissuelist=list(
        adipose = c("adipose"), 
        # brain = c("brain"),
        gut = c("gut"), 
        liver=c("liver"),
        kidney=c("kidney"), 
        lung=c("lung"), 
        thyroid = c("thyroid")
        # conceptus = c("placenta")
        ),
      suppress.messages=TRUE)
  parms <- c(parms, lumped_tissue_values_maternal[substr(names(
    lumped_tissue_values_maternal),1,1) == 'K']) #only add the partition coefficients
  parms$pH_Plasma_mat <- maternal.blood.pH

  # add Kconceptus2pu which is equivocal and hence, user-defined 
  parms$Kconceptus2pu <- Kconceptus2pu
  
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
  #Rblood2plasma is calculated in the dynamics of the 
  #corresponding .c file using other parameters. 
  
  #capture our desired parameters from parameterize_pbtk in "parms," too
  parms <- c(parms, pbtk_parms_desired)
  
# Set appropriate precision:
  parms <- lapply(parms[sort(names(parms))], set_httk_precision)
           

#Now for the many parameters associated with the dynamic physiologic equations
#for pregnancy from Kapraun et al. (2019):
  if (return.kapraun2019)
    # strip away most fetal growth params except for fBW, Vplacenta, Vamnf 
    kapraun2019.1tri <- 
      httk::kapraun2019[! ( substr(names(httk::kapraun2019), 1, 2) %in% c('Wf', 'Qf') )]
    kapraun2019.1tri <- 
      kapraun2019.1tri[! ( names(kapraun2019.1tri) %in% c(paste0("fhematocrit_cubic_theta", 1:3),
                                                          "fblood_weight_ratio",
                                                          paste("Q", 
                                                                c("brain", "kidney", "gut"), 
                                                                "_percent", sep = "")) )]
    # we do not need a fhematocrit or fblood_weight_ratio bc all tissues are lumped into the conceptus 
    # for the same reason, we don't need proportions of fcardiac output allotted to tissues 
    
    # add flow rates for maternal brain 
    # kapraun2019.1tri$Qbrain_percent_initial <- 12
    # kapraun2019.1tri$Qbrain_percent_terminal <- 8.8
    
    parms <- c(parms, kapraun2019.1tri)
 
 return(parms)                             
}
