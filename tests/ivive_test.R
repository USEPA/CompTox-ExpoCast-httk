#R CMD BATCH other_tests.R other_tests.Rout
library(httk)
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda1",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda1")
params <- parameterize_steadystate(chem.name="bisphenol a")
3/Css/params[["Funbound.plasma"]]
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda2",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda2")
params <- parameterize_steadystate(chem.name="bisphenol a")
3/Css/params[["Funbound.plasma"]]
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda3",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda3")
3/Css
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda4",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda4")
3/Css
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda5",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda5")
3/Css
set.seed(12345)
Css <- calc_mc_css(chem.name="bisphenol a",IVIVE="Honda6",output.units="uM")
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",IVIVE="Honda6")
3/Css