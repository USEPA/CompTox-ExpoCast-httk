library(httk)
# Test that the underlying PK models give the same answers:
calc_analytic_css(chem.cas=get_cheminfo()[11])
calc_analytic_css(chem.cas=get_cheminfo()[11],model="1compartment")
calc_analytic_css(chem.cas=get_cheminfo()[11],model="pbtk")
calc_analytic_css(chem.cas=get_cheminfo()[11],model="3compartment")
# Now test Monte Carlo for a variety of chemicals:
# well-behaved chemical data:
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[11])
# Clint and Fup are distributions, clint is zero:
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[10])
# Human.Clint.pvalue > 0.05
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[12])
# Human.Funbound.plasma is below LOD (0.005)
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[15])
# Now test that MC works across different models:
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[11],model="pbtk")
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[11],model="3compartment")
set.seed(1234)
calc_mc_css(chem.cas=get_cheminfo()[11],model="1compartment")
