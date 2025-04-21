library(httk)
# Bifenthrin has no pka donor or acceptors
# Look at properties:
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="82657-04-3",]
# Current NA means no ionization is predicted:
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="82657-04-3",c("pKa_Accept","pKa_Donor")] <- NA
# Check to see what we get using these parameters:
set.seed(1234)
calc_mc_oral_equiv(1,chem.name="Bifenthrin")
# Use a blank " " to indicate that the chemical was run through a model and no ionization was predicted
# Only use NA to mean no prediction made
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="82657-04-3",c("pKa_Accept","pKa_Donor")] <- " "
# Check to see what we get using these parameters:
set.seed(1234)
calc_mc_oral_equiv(1,chem.name="Bifenthrin")
#
# Acetochlor (34256-82-1) has an acceptor but no donor (it's a base)
# Look at properties:
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="34256-82-1",]
# Current NA means no ionization is predicted:
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="34256-82-1",c("pKa_Donor")] <- NA
# Check to see what we get using these parameters:
set.seed(1234)
calc_mc_oral_equiv(1,chem.name="Acetochlor")
# Use a blank " " to indicate that the chemical was run through a model and no ionization was predicted
# Only use NA to mean no prediction made
chem.physical_and_invitro.data[chem.physical_and_invitro.data$CAS=="34256-82-1",c("pKa_Donor")] <- " "
# Check to see what we get using these parameters:
set.seed(1234)
calc_mc_oral_equiv(1,chem.name="Acetochlor")
                                              

                                                                      