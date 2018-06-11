load_sipes2017 <- function(overwrite=F){
Sipes2017 <- Sipes2017                  

chem.physical_and_invitro.data <- add_chemtable(Sipes2017,current.table=chem.physical_and_invitro.data,
                                                data.list=list(Compound='Compound', CAS='CAS', MW = 'MW', logP = 'logP',
                                                pKa_Donor = 'pKa_Donor', pKa_Accept = 'pKa_Accept',
                                                Funbound.plasma = 'Human.Funbound.plasma', Clint = 'Human.Clint', SMILES.desalt = 'SMILES.desalt'),
                                                reference = 'Sipes 2017', species= 'Human', overwrite=overwrite)
return(chem.physical_and_invitro.data)                                                
}