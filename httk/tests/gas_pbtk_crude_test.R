#R CMD BATCH --no-timing --no-restore --no-save gas_pbtk_test.R gas_pbtk_test.Rout
library(httk)
options(warn=-1)

calc_analytic_css(chem.name="pyrene",route="inhalation",model="gas_pbtk")
calc_analytic_css(chem.cas="129-00-0",route="inhalation",model="gas_pbtk")
calc_analytic_css(chem.name="pyrene",route="inhalation",exp.conc=1,period=24,exp.duration=24,model="gas_pbtk")

calc_css(chem.name="pyrene",route="inhalation",exp.duration =24,model="gas_pbtk")
calc_css(chem.cas="129-00-0",route="inhalation",exp.conc=1,period=24,exp.duration=24,model="gas_pbtk")

head(solve_gas_pbtk(chem.name='pyrene',exp.conc=1,period=24,expduration=24))
head(solve_gas_pbtk(chem.name='pyrene',exp.conc=3,period=24,exp.duration = 6, exercise = TRUE))
params <- parameterize_gas_pbtk(chem.cas="80-05-7")
head(solve_gas_pbtk(parameters=params))

set.seed(12345)
calc_mc_css(chem.name='pyrene',model="gas_pbtk", output.units="uM")
set.seed(12345)
calc_mc_css(chem.cas="129-00-0",model="gas_pbtk")
set.seed(12345)
calc_mc_css(chem.cas="129-00-0",model="gas_pbtk",samples=500,httkpop=F)
set.seed(12345)
calc_mc_css(chem.cas="129-00-0",model="gas_pbtk",samples=500,httkpop=F,invitrouv=F)
set.seed(12345)
calc_mc_css(chem.cas="129-00-0",model="gas_pbtk",samples=500,httkpop=F,invitrouv=F, output.units="uM")

quit("no")