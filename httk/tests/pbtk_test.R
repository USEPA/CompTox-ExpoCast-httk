# R CMD BATCH --no-timing --no-restore --no-save pbtk_test.R pbtk_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

calc_analytic_css(chem.name="bisphenol a",model="pbtk")
calc_analytic_css(chem.cas="80-05-7",model="pbtk")
calc_analytic_css(parameters=parameterize_pbtk(chem.cas="80-05-7"),model="pbtk")
calc_analytic_css(chem.name="bisphenol a",model="pbtk",tissue="liver")
calc_analytic_css(chem.name="bisphenol a",model="pbtk",tissue="brain")

head(solve_pbtk(chem.name="bisphenol a"))
head(solve_pbtk(chem.cas="80-05-7"))
head(solve_pbtk(parameters=parameterize_pbtk(chem.cas="80-05-7")))

p <- parameterize_pbtk(chem.name="Aminopterin")[sort(names(parameterize_pbtk(chem.name="Aminopterin")))]
for (this.param in sort(tolower(names(p)))) cat(paste(this.param,": ",p[[this.param]],"\n"))

quit("no")