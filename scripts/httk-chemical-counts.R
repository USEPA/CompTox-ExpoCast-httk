length(unique(c(get_cheminfo(species="rat"),get_cheminfo(),get_cheminfo(model="gas_pbtk"),get_cheminfo(model="gas_pbtk",species="rat"))))

sum(unique(c(get_cheminfo(species="rat"),get_cheminfo(),get_cheminfo(model="gas_pbtk"),get_cheminfo(model="gas_pbtk",species="rat")))%in%chem.lists[["ToxCast"]]$CAS)

sum(unique(c(get_cheminfo(species="rat"),get_cheminfo(),get_cheminfo(model="gas_pbtk"),get_cheminfo(model="gas_pbtk",species="rat")))%in%chem.lists[["pharma"]]$CAS)