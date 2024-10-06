library(httk)

load_honda2024()
pfas.httk.table <- get_2023pfasinfo(info="all")
for (this.chem in pfas.httk.table$DTXSID)
{
  this.row <- pfas.httk.table$DTXSID==this.chem
  pfas.httk.table[this.row, "Css.Classic"] <- try(calc_analytic_css(
                                                dtxsid=this.chem,
                                                parameterize.args=list(class.exclude=FALSE),
                                                model="3compartmentss"))
  pfas.httk.table[this.row, "Css.Exhale"] <- try(calc_analytic_css(
                                                dtxsid=this.chem,
                                                parameterize.args=list(class.exclude=FALSE),
                                                model="sumclearances"))
  pfas.httk.table[this.row, "Css.PFAS"] <- try(calc_analytic_css(
                                                dtxsid=this.chem,
                                                parameterize.args=list(class.exclude=FALSE),
                                                model="sumclearancespfas"))
}
pfas.httk.table$Css.Classic <- as.numeric(pfas.httk.table$Css.Classic)
pfas.httk.table$Css.Exhale <- as.numeric(pfas.httk.table$Css.Exhale)
pfas.httk.table$Css.PFAS <- as.numeric(pfas.httk.table$Css.PFAS)
pfas.httk.table$Css.Exhale.FoldChange <- signif(pfas.httk.table$Css.Exhale/pfas.httk.table$Css.Classic, 3)
pfas.httk.table$Css.PFAS.FoldChange <- signif(pfas.httk.table$Css.PFAS/pfas.httk.table$Css.Classic, 3)

write.csv(pfas.httk.table, row.names=FALSE, file="new-pfas-css-uM.txt")

