#R CMD BATCH --no-timing --no-restore --no-save pbtk_test.R pbtk_test.Rout
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

#parameterize_pbtk(chem.name="Aminopterin")[sort(names(parameterize_pbtk(chem.name="Aminopterin")))]

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) 
{
  if (any(script.args=="mctest"))
  {
    set.seed(12345)
    calc_mc_css(chem.name="bisphenol a",model="pbtk")
    set.seed(12345)
    calc_mc_css(chem.cas="80-05-7",model="pbtk")
    set.seed(12345)
    calc_mc_css(parameters=parameterize_pbtk(chem.cas="80-05-7"),model="pbtk")
  }
  if (any(script.args=="wetmoretest"))
  {
    wetmore.chems <- subset(chem.physical_and_invitro.data,
      regexpr("Wetmore",Human.Clint.Reference)!=-1 &
      regexpr("Wetmore",Human.Funbound.plasma.Reference)!=-1)
    Css.table <- data.frame()
    for (this.cas in sort(get_lit_cheminfo()))
      if (this.cas %in% get_cheminfo(model="pbtk") &
        this.cas %in% wetmore.chems$CAS)
      {
        Css.table[this.cas,"Css.lit"] <- get_lit_css(chem.cas=this.cas,
          output.units="uM",
          which.quantile=0.5)
        Css.table[this.cas,"Css.calc"] <- calc_analytic_css(chem.cas=this.cas,
          model="pbtk")
      }                                                                          
    cat(lm(log(Css.lit)~log(Css.calc),data=Css.table)$coefficients)
    
    library(ggplot2)
    test.fig <- ggplot(Css.table,aes(x=Css.calc,y=Css.lit)) +
             geom_point(size=3)+
       scale_y_log10()+
       scale_x_log10()+
      ylab(expression(paste(C[ss]," Literature (uM)"))) +
      xlab(expression(paste(C[ss]," pbtk model (uM)"))) +
      geom_abline(intercept = 0, slope = 1,linetype="dashed", colour="Blue") 
    
    dev.new()
    print(test.fig)
  }
}

quit("no")