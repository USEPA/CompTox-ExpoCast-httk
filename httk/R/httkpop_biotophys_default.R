#' Convert HTTK-Pop-generated parameters to HTTK physiological parameters
#' 
#' @param indiv_dt The data.table object returned by \code{httkpop_generate()}
#'   
#' @return A data.table with the physiological parameters expected by any HTTK 
#'   model, including body weight (BW), hematocrit, tissue volumes per kg body
#'   weight, tissue flows as fraction of CO, CO per (kg BW)^3/4, GFR per (kg
#'   BW)^3/4, portal vein flow per (kg BW)^3/4, and liver density.
#'
#'@author Caroline Ring
#'
#'@references 
#'\insertRef{ring2017identifying}{httk} 
#'
#' @keywords httk-pop monte-carlo
#'
#' @export httkpop_biotophys_default 

httkpop_biotophys_default <- function(indiv_dt){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  BW <- weight_adj <- NULL
  hematocrit <- Qgfrc <- gfr_est <- NULL
  BSA_adj <- Qtotal.liverc <- Liver_flow <- NULL
  Stomach_flow <- Small_intestine_flow <- NULL
  Large_intestine_flow <- Spleen_flow <- NULL
  Pancreas_flow <- liver.density <- Vadiposec <- NULL
  Adipose_mass <- Vbonec <- Skeleton_mass <- NULL
  Vbrainc <- Brain_mass <- Vstomachc <- Stomach_mass <- NULL
  Vlargeintestinec <- Large_intestine_mass <- NULL
  Vsmallintestinec <- Small_intestine_mass <- NULL
  Vgutc <- Vheartc <- Heart_mass <- Vkidneyc <- NULL
  Kidneys_mass <- Vliverc <- Liver_mass <- NULL
  Vlungc <- Lung_mass <- Vmusclec <- Muscle_mass <- NULL
  Vskinc <- Skin_mass <- Vspleenc <- Spleen_mass <- NULL
  Vpancreasc <- Pancreas_mass <- Vgonadsc <- Gonads_mass <- NULL
  plasma.vol <- Blood_mass <- enum_mass_sum <- Vrestc <- NULL
  Qadiposef <- Adipose_flow <- CO <- Qbonef <- NULL
  Skeleton_flow <- Qbrainf <- Brain_flow <- Qheartf <- NULL
  Heart_flow <- Qmusclef <- Muscle_flow <- Qskinf <- NULL
  Skin_flow <- Qspleenf <- Qstomachf <- Qlargeintestinef <- NULL
  Qsmallintestinef <- Qpancreasf <- Qgonadsf <- Gonads_flow <- NULL
  Qgutf <- Qkidneyf <- Kidneys_flow <- Qliverf <- Qlungf <- NULL
  Lung_flow <- Qrestf <- Qcardiacc <- Vvenc <- Vartc <- NULL
  #End R CMD CHECK appeasement.
  
  #make a copy of the population data.table, to avoid altering it globally.
  indiv_model <- data.table::copy(indiv_dt)
  
  #Physiological parameters used by all HTTK models:
  # BW  Body Weight, kg.
  indiv_model[, BW:=weight_adj]
  # hematocrit  Percent volume of red blood cells in the blood.
  indiv_model[, hematocrit:=hematocrit/100] #convert to fraction rather than percentage
  # QGFRc   Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid filtered
  #from kidney and excreted.
  indiv_model[, Qgfrc:=indiv_dt[, gfr_est*
                                  (BSA_adj/ #correct for BSA normalization
                                     (1.73*100^2))*
                                  60/(1000*weight_adj^(3/4))]]
  
  #Portal vein flow = liver arterial flow plus gut arterial flow.
  indiv_model[, Qtotal.liverc:=indiv_dt[, (Liver_flow+
                                             Stomach_flow+
                                             Small_intestine_flow+
                                             Large_intestine_flow+
                                             Spleen_flow+
                                             Pancreas_flow)/
                                          weight_adj^(3/4)]]
  
  indiv_model[, liver.density:=1.05] #Set liver density for all individuals
  
  #Convert tissue masses to tissue volumes using tissue densities from Birnbaum
  #et al.
  indiv_model[, Vadiposec:=Adipose_mass/
                (weight_adj*0.916)]
  indiv_model[, Vbonec:= Skeleton_mass* #Skeleton is 14.2% of body mass
                ((5.7/14.2)/1.99 + #cortical bone, 5.7% of body mass
                   (1.4/14.2)/1.92+ #trabecular bone, 1.4% of body mass
                   (2.1/14.2)/1.028 + #red marrow, 2.1% of body mass
                   (2.1/14.2)/0.783+#yellow marrow, 2.1% of body mass
                   (1.6/14.2)/1.1 + #cartilage, 1.6% of body mass
                   (1.3/14.2)/1.1)/  #periarticular tissue, 1.3% of body mass;
                #assume perarticular tissue density == cartilage density
                weight_adj] 
  
  indiv_model[, Vbrainc:=Brain_mass/
                (weight_adj*1.03)]
  indiv_model[, Vstomachc:=Stomach_mass/
                (weight_adj*1.05)]
  indiv_model[, Vlargeintestinec:=Large_intestine_mass/
                (weight_adj*1.042)]
  indiv_model[, Vsmallintestinec:=Small_intestine_mass/
                (weight_adj*1.045)]
  #Gut volume: lump stomach, small intestine, large intestine
  indiv_model[, Vgutc:=(Stomach_mass/1.05 +
                          Large_intestine_mass/1.042 +
                          Small_intestine_mass/1.045)/
                (weight_adj)]
  indiv_model[, Vheartc:=Heart_mass/
                (weight_adj*1.03)]
  indiv_model[, Vkidneyc:=Kidneys_mass/
                (weight_adj*1.05)]
  indiv_model[, Vliverc:=Liver_mass/
                (weight_adj*1.05)]
  indiv_model[, Vlungc:=Lung_mass/
                (weight_adj*1.05)]
  indiv_model[, Vmusclec:=Muscle_mass/ #Skeletal muscle
                (weight_adj*1.041)]
  indiv_model[, Vskinc:=Skin_mass/
                (weight_adj*1.116)]
  indiv_model[, Vspleenc:=Spleen_mass/
                (weight_adj*1.054)]
  indiv_model[, Vpancreasc:=Pancreas_mass/
                (weight_adj*1.045)]
  indiv_model[, Vgonadsc:=Gonads_mass/
                (weight_adj*1.05)]
  #Plasma volume computed from hematocrit and blood volume
  indiv_model[, plasma.vol:=(1-hematocrit)*
                Blood_mass/
                (weight_adj*1.06)]
  
  #To get the volume of body tissues not enumerated in this list of tissues, 
  #first get the sum of masses of enumerated tissues...
  indiv_model[, enum_mass_sum:=Reduce('+', .SD),
              .SDcols=paste(c('Adipose',
                              'Skeleton',
                              'Brain',
                              'Stomach',
                              'Large_intestine',
                              'Small_intestine',
                              'Heart',
                              'Kidneys',
                              'Liver',
                              'Lung',
                              'Muscle',
                              'Skin',
                              'Spleen',
                              'Blood'),
                            'mass', sep='_')]
  
  #To get remaining volume, assume that it's (bodyweight - sum of masses of
  #enumerated tissues)/ (tissue density * bodyweight), and that remaining tissue
  #density is approximately 1.
  indiv_model[,Vrestc:=(weight_adj-
                          enum_mass_sum)/
                weight_adj]
  
  #Do flows for all tissues in tissue.data as well
  #Convert flows to fraction of cardiac output.
  indiv_model[, Qadiposef:=Adipose_flow/CO]
  indiv_model[, Qbonef:=Skeleton_flow/CO]
  indiv_model[, Qbrainf:=Brain_flow/CO]
  indiv_model[, Qheartf:=Heart_flow/CO]
  indiv_model[, Qmusclef:=Muscle_flow/CO]
  indiv_model[, Qskinf:=Skin_flow/CO]
  indiv_model[, Qspleenf:=Spleen_flow/CO]
  indiv_model[, Qstomachf:=Stomach_flow/CO]
  indiv_model[, Qlargeintestinef:=Large_intestine_flow/CO]
  indiv_model[, Qsmallintestinef:=Small_intestine_flow/CO]
  indiv_model[, Qpancreasf:=Pancreas_flow/CO]
  indiv_model[, Qgonadsf:=Gonads_flow/CO]
  indiv_model[, Qgutf:=(Stomach_flow +
                          Large_intestine_flow +
                          Small_intestine_flow)/CO]
  indiv_model[, Qkidneyf:=Kidneys_flow/CO]
  indiv_model[, Qliverf:=Liver_flow/CO]
  indiv_model[, Qlungf:=Lung_flow/CO]
  
  #Flow to tissues not otherwise enumerated is 100% of CO - fraction of CO
  #accounted for by enumerated tissues. Use regular expressions to find all Q*f
  #variable names, but exclude Qgutf (lumped Qstomachf, Qsmallintestinef,
  #Qlargeintestinef), and also exclude Qgfrc because it's not really a tissue
  #flow.
  indiv_model[, Qrestf:=1-Reduce('+', .SD),
              .SDcols=grep(x=names(indiv_model)[!(names(indiv_model)%in% 
                                                    c('Qgfrc',
                                                      'Qgutf'))], 
                           pattern='(?<=Q)(\\w+)(?=f)', 
                           perl=TRUE,
                           value=TRUE)]
  #Convert cardiac output to be per (kg BW)^(3/4)
  indiv_model[, Qcardiacc:=CO/(weight_adj^(3/4))]
  
  
  #venous blood: fixed fraction of blood weight of 0.67, 
  #according to Bosgra et al. 2012
  indiv_model[, Vvenc:=0.67*Blood_mass/
                (1.06*weight_adj)]
  #which leaves 0.33 of blood weight as arterial blood.
  indiv_model[, Vartc:=0.33*Blood_mass/
                (1.06*weight_adj)]
  
  #Return only the physiological variables used by HTTK
  return(indiv_model[, 
                     c("million.cells.per.gliver",
                       "hematocrit",
                       names(indiv_model)[!(names(indiv_model) %in% 
                                              names(indiv_dt))]),
                     with=FALSE])
}
