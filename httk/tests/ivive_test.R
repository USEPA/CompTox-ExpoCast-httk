# R CMD BATCH --no-timing --no-restore --no-save ivive_test.R ivive_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

<<<<<<< HEAD
=======
# Reduce the number of samples used by Monte Carlo to decrease runtime for
# CRAN checks (never use predictions with only ten draws):
>>>>>>> feature/oral
NSAMP <- 10

# From Honda et al. (2019) (currently only use mean conc's because steady-state 
# calculation does not give max):
#
# Default HTTK function arguments correspond to "Honda3"
#
#       in vivo Conc.	   Metabolic Clearance  In Vivo Conc.  In Vitro Conc.
#Honda1	Veinous (Plasma) Restrictive	        Free           Free
#Honda2	Veinous	         Restrictive	        Free	         Nominal
#Honda3	Veinous	         Restrictive	        Total	         Nominal
#Honda4	Target Tissue    Non-restrictive	    Total	         Nominal
#
# "Honda1" uses plasma concentration, restrictive clearance, and treats the 
# unbound invivo concentration as bioactive. For IVIVE, any input nominal 
# concentration in vitro should be converted to cfree.invitro using 
# \code{\link{armitage_eval}}, otherwise performance will be the same as 
# "Honda2". 
#
# Use \code{\link{show_honda.ivive()}} to print summary of Honda et al. (2019)
# results.

# Default HTTK: 
set.seed(12345)
<<<<<<< HEAD
Css <- calc_mc_css(chem.name="bisphenol a",
  output.units="uM",
  samples=NSAMP)
set.seed(12345)
calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
  samples=NSAMP)
params <- parameterize_steadystate(chem.name="bisphenol a")
# This should be the same as calc_mc_oral_equiv:
signif(3/Css,4)

## Honda1:
#set.seed(12345)
#Css <- calc_mc_css(chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(
#    restrictive.clearance=TRUE,
#    bioactive.free.invivo = T),
#  output.units="uM",
#  samples=NSAMP)
#temp <- armitage_eval(
#  casrn.vector = c("80-05-7"), 
#  this.FBSf = 0.1,
#  this.well_number = 384, 
#  nomconc = 3)
#cfree <- temp$cfree.invitro
#set.seed(12345)
#calc_mc_oral_equiv(cfree,chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(IVIVE="Honda1"),
#  samples=NSAMP)
## This should be the same as calc_mc_oral_equiv:
#signif(cfree/Css,4)
#
## Honda2:
#set.seed(12345)
#Css <- calc_mc_css(chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(
#    restrictive.clearance=TRUE,
#    bioactive.free.invivo = T),
#  output.units="uM",
#  samples=NSAMP)
#set.seed(12345)
#calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(IVIVE="Honda2"),
#  samples=NSAMP)
## This should be the same as calc_mc_oral_equiv:
#signif(3/Css,4)
#
## Honda 3 (should be the same as degault HTTK):
#set.seed(12345)
#Css <- calc_mc_css(chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(
#    restrictive.clearance=TRUE,
#    bioactive.free.invivo = F),
#  output.units="uM",
#  samples=NSAMP)
#set.seed(12345)
#calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(IVIVE="Honda3"),
#  samples=NSAMP)
## This should be the same as calc_mc_oral_equiv:
#signif(3/Css,4)
#
## Honda4:
#set.seed(12345)
#Css <- calc_mc_css(chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(
#    tissue="liver",
#    restrictive.clearance=FALSE,
#    bioactive.free.invivo = F),
#  output.units="uM",
#  samples=NSAMP)
#set.seed(12345)
#calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
#  calc.analytic.css.arg.list=list(IVIVE="Honda4"),
#  samples=NSAMP)
## This should be the same as calc_mc_oral_equiv:
#signif(3/Css,4)

=======
Css0 <- calc_mc_css(chem.name="bisphenol a",
  output.units="uM",
  samples=NSAMP)
set.seed(12345)
# This should be the same as calc_mc_oral_equiv:
signif(3/Css0,4) ==
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    samples=NSAMP)

# Honda1:
set.seed(12345)
Css1 <- calc_mc_css(chem.name="bisphenol a",
  calc.analytic.css.arg.list=list(
    restrictive.clearance = TRUE,
    bioactive.free.invivo = TRUE),
  output.units="uM",
  samples=NSAMP)
temp <- armitage_eval(
  casrn.vector = c("80-05-7"), 
  this.FBSf = 0.1,
  this.well_number = 384, 
  nomconc = 3)
cfree <- temp$cfree.invitro
set.seed(12345)
# This should be the same as calc_mc_oral_equiv with IVIVE=="Honda1":
signif(cfree/Css1,4) ==
  calc_mc_oral_equiv(cfree,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda1"),
    samples=NSAMP)
# Should be different from default:
!(Css1 %in% c(Css0))

# Honda2:
set.seed(12345)
Css2 <- calc_mc_css(chem.name="bisphenol a",
  calc.analytic.css.arg.list=list(
    restrictive.clearance = TRUE,
    bioactive.free.invivo = TRUE),
  output.units="uM",
  samples=NSAMP)
set.seed(12345)
# This should be the same as calc_mc_oral_equiv with IVIVE=="Honda2":
signif(3/Css2,4) ==
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda2"),
    samples=NSAMP)
# Should be different from previous:
!(Css2 %in% c(Css0))

# Honda 3 (should be the same as degault HTTK):
set.seed(12345)
Css3 <- calc_mc_css(chem.name="bisphenol a",
  calc.analytic.css.arg.list=list(
    restrictive.clearance = TRUE,
    bioactive.free.invivo = FALSE),
  output.units="uM",
  samples=NSAMP)
set.seed(12345)
# This should be the same as calc_mc_oral_equiv with IVIVE=="Honda3":
signif(3/Css3,4) ==
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda3"),
    samples=NSAMP)
# Should be same as default:
Css0 == Css3
# Should be different from previous:
!(Css3 %in% c(Css1, Css2))

# Honda4:
set.seed(12345)
Css4 <- calc_mc_css(chem.name="bisphenol a",
  calc.analytic.css.arg.list=list(
    tissue="liver",
    restrictive.clearance = FALSE,
    bioactive.free.invivo = FALSE),
  model="pbtk",
  output.units="uM",
  samples=NSAMP)
set.seed(12345)
# This should be the same as calc_mc_oral_equiv with IVIVE=="Honda4":
signif(3/Css4,4) ==
  calc_mc_oral_equiv(3.0,chem.name="bisphenol a",
    calc.analytic.css.arg.list=list(IVIVE="Honda4"),
    samples=NSAMP,
    model="pbtk")
# Should be different from previous:
!(Css4 %in% c(Css0, Css1, Css2, Css3))
>>>>>>> feature/oral

# Quit without saving or displaying messages:
quit("no")