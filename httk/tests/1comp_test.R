#R CMD BATCH --no-timing --no-restore --no-save 1comp_test.R 1comp_test.Rout
library(httk)

calc_analytic_css(chem.name="bisphenol a",model="1compartment")
calc_analytic_css(chem.cas="80-05-7",model="1compartment")
calc_analytic_css(parameters=parameterize_1comp(chem.cas="80-05-7"),model="1compartment")
calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="liver")
calc_analytic_css(chem.cas="80-05-7",model="1compartment",tissue="brain")

head(solve_1comp(chem.name="bisphenol a"))
head(solve_1comp(chem.cas="80-05-7"))
head(solve_1comp(parameters=parameterize_1comp(chem.cas="80-05-7")))

calc_vdist(chem.name="triclosan")
calc_vdist(chem.cas="80-05-7")
params <- parameterize_schmitt(chem.name="triclosan")
params <- c(params, predict_partitioning_schmitt(parameters = params))
calc_vdist(parameters=params)
params <- parameterize_3comp(chem.name="triclosan")
calc_vdist(parameters=params)
params <- parameterize_pbtk(chem.name="triclosan")
calc_vdist(parameters=params)

script.args <- commandArgs(TRUE)
if (length(script.args) > 0) 
{
  if (any(script.args=="mctest"))
  {
    set.seed(12345)
    calc_mc_css(chem.name="bisphenol a",model="1compartment")
    set.seed(12345)
    calc_mc_css(chem.cas="80-05-7",model="1compartment")
    set.seed(12345)
    calc_mc_css(parameters=parameterize_1comp(chem.cas="80-05-7"),model="1compartment")
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
          model="1compartment")
      }                                                                          
    cat(lm(log(Css.lit)~log(Css.calc),data=Css.table)$coefficients)
   
    #library(ggplot2)
    #test.fig <- ggplot(Css.table,aes(x=Css.calc,y=Css.lit)) +
    #         geom_point(size=3)+
    #   scale_y_log10()+
    #   scale_x_log10()+
    #  ylab(expression(paste(C[aa]," Literature (uM)"))) +
    #  xlab(expression(paste(C[aa]," 1compartment model (uM)"))) +
    #  geom_abline(intercept = 0, slope = 1,linetype="dashed", colour="Blue") 
    #
    #dev.new()
    #print(test.fig)
  }
}

quit("no")