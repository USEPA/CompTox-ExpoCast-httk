#R CMD BATCH --no-timing --no-restore --no-save httk2.0_test.R httk2.0_test.Rout
library(httk)

#head(solve_pbtk(chem.name="Terbufos"))
#head(solve_model(chem.name="Terbufos",model="pbtk",dosing=list(
#                    initial.dose = 1, # Assume dose is in mg/kg BW/day  
#                    doses.per.day=NULL,
#                    dosing.matrix = NULL,
#                    daily.dose = NULL)))

dm <- matrix(c(0,1,2,5,5,5),nrow=3)
colnames(dm) <- c("time","dose")
solve_pbtk(chem.name="Methenamine",dosing.matrix=dm,dose=NULL,daily.dose=NULL)[190:201,]
solve_model(chem.name="Methenamine",model="pbtk",dosing=list(
  initial.dose =NULL,
  doses.per.day=NULL,
  daily.dose=NULL,
  dosing.matrix=dm))[190:201,]
  
#head(solve_pbtk(chem.name="Terbufos",iv.dose=TRUE))
#
#solve_model(chem.name="Besonprodil",model="pbtk",dosing=list(
#  initial.dose =NULL,
#  doses.per.day=4,
#  daily.dose=1,
#  dosing.matrix=NULL))[190:205,]
#solve_pbtk(chem.name="Besonprodil",daily.dose=1,dose=NULL,doses.per.day=4)[190:205,]
#
#
#calc_analytic_css(chem.name="Betaxolol")
#calc_analytic_css(chem.name="Tacrine",model="pbtk")
#calc_analytic_css(chem.name="Dicofol",model="1compartment")
#calc_analytic_css(chem.name="Diflubenzuron",model="3compartment")
#calc_analytic_css(chem.name="Theobromine",model="3compartmentss")

#head(solve_1comp(chem.name="Terbufos",daily.dose=NULL,dose=1))
#head(solve_1comp(chem.name="Terbufos",daily.dose=NULL,dose=1,iv.dose=TRUE))
solve_1comp(chem.name="Methenamine",dosing.matrix=dm,dose=NULL,daily.dose=NULL)[190:201,]
solve_1comp(chem.name="Besonprodil",daily.dose=1,dose=NULL,doses.per.day=4)[190:205,]
#
#head(solve_3comp(chem.name="Terbufos",daily.dose=NULL,dose=1))
#head(solve_3comp(chem.name="Terbufos",daily.dose=NULL,dose=1,iv.dose=TRUE))
#solve_3comp(chem.name="Methenamine",dosing.matrix=dm,dose=NULL,daily.dose=NULL)[190:201,]
#solve_3comp(chem.name="Besonprodil",daily.dose=1,dose=NULL,doses.per.day=4)[190:205,]

quit("no")