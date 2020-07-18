#R CMD BATCH --no-timing --no-restore --no-save ivive_test.R ivive_test.Rout
library(httk)
options(warn=-1)

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) if (script.args[1]=="mctest")
{
  set.seed(12345)
  Css <- calc_mc_css(chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda1"),
    output.units="uM")
  set.seed(12345)
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda1"))
  params <- parameterize_steadystate(chem.name="bisphenol a")
  3/Css/params[["Funbound.plasma"]]
  set.seed(12345)
  Css <- calc_mc_css(chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda2"),
    output.units="uM")
  set.seed(12345)
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda2"))
  params <- parameterize_steadystate(chem.name="bisphenol a")
  3/Css/params[["Funbound.plasma"]]
  set.seed(12345)
  calc_mc_css(chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda3"),
    output.units="uM")
  set.seed(12345)
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda3"))
  3/Css
  set.seed(12345)
  Css <- calc_mc_css(chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda4"),
    output.units="uM")
  set.seed(12345)
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda4"))
  3/Css
}

quit("no")