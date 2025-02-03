## ----configure_knitr, eval = TRUE---------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')
options(rmarkdown.html_vignette.check_title = FALSE)

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls()) 

## ----install_httk, eval = FALSE-----------------------------------------------
#  install.packages("httk")

## ----install_readxl, eval = FALSE---------------------------------------------
#  install.packages("readxl")

## ----install_ggplot2, eval = FALSE--------------------------------------------
#  install.packages("ggplot2")

## ----runchunks, eval = TRUE---------------------------------------------------
# Set whether or not the following chunks will be executed (run):
execute.vignette <- FALSE

## ----load_packages, eval = execute.vignette-----------------------------------
#  library(httk)
#  library(readxl)
#  library(ggplot2)

## ----MC_samples, eval = execute.vignette--------------------------------------
#  NSAMP <- 25

## ----change_directory, eval = FALSE-------------------------------------------
#  setwd("FOLDERPATH")

## ----load_toxcast, eval = FALSE-----------------------------------------------
#  load("invitrodb_3_5_mc5.Rdata")

## ----subset_toxcast1, eval = FALSE--------------------------------------------
#  toxcast.httk <- subset(mc5, dsstox_substance_id %in% get_cheminfo(
#           info="DTXSID",
#           suppress.messages=TRUE))

## ----subset_toxcast2, eval = FALSE--------------------------------------------
#  set.seed(1234)
#  my.chems <- sample(mc5$dsstox_substance_id,50)
#  example.toxcast <- as.data.frame(mc5[mc5$dsstox_substance_id %in% my.chems,])

## ----subset_toxcast3, eval = FALSE--------------------------------------------
#  example.toxcast <- example.toxcast[, c("chnm",
#                "dsstox_substance_id",
#                "spid",
#                "hitc",
#                "modl",
#                "aeid",
#                "modl_ga",
#                "modl_ac10",
#                "modl_acc")]
#  # reduce precision to decrease space:
#  cols <- c("modl_ga", "modl_ac10", "modl_acc")
#  for (this.col in cols)
#    example.toxcast[, this.col] = signif(example.toxcast[, this.col], 3)
#  save(example.toxcast,file="introtoivive-toxcast.Rdata",version=2)

## ----display_toxcast, eval = TRUE---------------------------------------------
example.toxcast <- httk::example.toxcast
knitr::kable(head(example.toxcast), caption = "ToxCast In Vitro Bioactivity Data",
             floating.environment="sidewaystable")

## ----toxcast_summary.table, eval = TRUE---------------------------------------
toxcast.table <- NULL
for (this.id in unique(example.toxcast$dsstox_substance_id))
{
  this.subset <- subset(example.toxcast, dsstox_substance_id == this.id)
  these.hits <- subset(this.subset, hitc==1)
  if (dim(these.hits)[1]>0){
      this.row <- data.frame(Compound=as.character(this.subset[1,"chnm"]),
                         DTXSID=this.id,
                         Total.Assays = dim(this.subset)[1],
                         Unique.Assays = length(unique(this.subset$aeid)),
                         Total.Hits = dim(these.hits)[1],
                         Unique.Hits = length(unique(these.hits$aeid)),
                         Low.AC50 = signif(min(these.hits$modl_ga),3),
                         Low.AC10 = signif(min(these.hits$modl_ac10),3),
                         Low.ACC = signif(min(these.hits$modl_acc),3),
                         Q10.AC50 = signif(quantile(these.hits$modl_ga,probs=0.1),3),
                         Q10.AC10 = signif(quantile(these.hits$modl_ac10,probs=0.1),3),
                         Q10.ACC = signif(quantile(these.hits$modl_acc,probs=0.1),3),
                         Med.AC50 = signif(median(these.hits$modl_ga),3),
                         Med.AC10 = signif(median(these.hits$modl_ac10),3),
                         Med.ACC = signif(median(these.hits$modl_acc),3),
                         Q90.AC50 = signif(quantile(these.hits$modl_ga,probs=0.9),3),
                         Q90.AC10 = signif(quantile(these.hits$modl_ac10,probs=0.9),3),
                         Q90.ACC = signif(quantile(these.hits$modl_acc,probs=0.9),3)
                         )
    toxcast.table <- rbind(toxcast.table, this.row)
  }
}
rownames(toxcast.table) <- seq(1,dim(toxcast.table)[1])
knitr::kable(head(toxcast.table[,1:6]), caption = "Summarized ToxCast Data",
             floating.environment="sidewaystable")

## ----calc_css1, eval = execute.vignette---------------------------------------
#  for (this.id in unique(toxcast.table$DTXSID))
#  {
#  # get_cheminfo() gives a list of all the CAS numbers for which HTTK will work:
#    if (this.id %in% get_cheminfo(info="dtxsid", suppress.messages=TRUE))
#    {
#  # Set a random number generator seed so that the Monte Carlo will always give
#  # the same random sequence:
#      set.seed(12345)
#      toxcast.table[toxcast.table$DTXSID==this.id,"Css"] <-
#        calc_mc_css(dtxsid=this.id,
#                    output.units="uM",
#                    samples=NSAMP,
#                    suppress.messages=TRUE)
#      toxcast.table[toxcast.table$DTXSID==this.id,"Css.Type"] <- "in vitro"
#    }
#  }

## ----css_table1, eval = execute.vignette--------------------------------------
#  knitr::kable(toxcast.table[1:10,c("Compound","Q10.AC50","Css","Css.Type")],
#                                  caption = "Summarized ToxCast Data",
#               floating.environment="sidewaystable")

## ----load_qspr, eval = execute.vignette---------------------------------------
#  load_sipes2017()

## ----calc_css2, eval = execute.vignette---------------------------------------
#  for (this.id in unique(toxcast.table$DTXSID))
#  {
#    if (this.id %in% get_cheminfo(info="dtxsid", suppress.messages=TRUE) &
#      is.na(toxcast.table[toxcast.table$DTXSID==this.id,"Css"]))
#    {
#  # Set a random number generator seed so that the Monte Carlo will always give
#  # the same random sequence:
#      set.seed(12345)
#      toxcast.table[toxcast.table$DTXSID==this.id,"Css"] <-
#        calc_mc_css(dtxsid=this.id,
#                    output.units="uM",
#                    samples=NSAMP,
#                    suppress.messages=TRUE)
#      toxcast.table[toxcast.table$DTXSID==this.id,"Css.Type"] <- "in silico"
#    }
#  }

## ----css_table2, eval = execute.vignette--------------------------------------
#  knitr::kable(toxcast.table[1:10,c("Compound","Q10.AC50","Css","Css.Type")],
#                                  caption = "Summarized ToxCast Data",
#               floating.environment="sidewaystable")

## ----calc_equivalent_dose16, eval = execute.vignette--------------------------
#  toxcast.table$EquivDose <- signif(10^toxcast.table$Q10.AC50 / toxcast.table$Css,
#                                    3)

## ----display_table2, eval = execute.vignette----------------------------------
#  knitr::kable(toxcast.table[1:10,c("Compound","Q10.AC50","Css","EquivDose")],
#                                   caption = "Summarized ToxCast Data",
#                floating.environment="sidewaystable")

## ----load_seem1, eval = FALSE-------------------------------------------------
#  SEEM <- read.csv("SupTable-all.chem.preds-2018-11-28.txt",stringsAsFactors=F)

## ----load_seem2, eval = FALSE-------------------------------------------------
#  SEEM <- read_excel("CCD-Batch-Search_DATE.xlsx",sheet=2)

## ----load_seem3, eval = FALSE-------------------------------------------------
#  load("Ring2018Preds.RData")
#  SEEM <- Ring2018.preds

## ----save_seem, eval = FALSE--------------------------------------------------
#  example.seem <- as.data.frame(subset(SEEM,
#                                dsstox_substance_id %in% toxcast.table$DTXSID))
#  for (this.col in 4:36)
#    example.seem[,this.col] <- signif(example.seem[,this.col],4)
#  save(example.seem,file="introtoivive-seem.Rdata",version=2)

## ----mergetoxexposure, eval = execute.vignette--------------------------------
#  example.seem <- httk::example.seem
#  toxcast.table <- merge(toxcast.table,
#                         example.seem[,c(
#                           "dsstox_substance_id",
#                           "seem3",
#                           "seem3.l95",
#                           "seem3.u95",
#                           "Pathway",
#                           "AD")],
#                         by.x="DTXSID",
#                         by.y="dsstox_substance_id",
#                         all.x = TRUE)

## ----calc_ber, eval = execute.vignette----------------------------------------
#  toxcast.table$BER <- signif(toxcast.table$EquivDose/toxcast.table$seem3.u95, 3)

## ----sort_by_ber, eval = execute.vignette-------------------------------------
#  toxcast.table <- toxcast.table[order(toxcast.table$BER),]
#  knitr::kable(toxcast.table[1:10,c("Compound","EquivDose","seem3.u95","Pathway","BER")],
#                                   caption = "Bioactivity:Exposure Ratios",
#                floating.environment="sidewaystable")

## ----chem_name_factors, eval = execute.vignette-------------------------------
#  toxcast.table$Compound <- factor(toxcast.table$Compound,
#                                      levels=toxcast.table$Compound)

## ----ber_plot, eval = execute.vignette----------------------------------------
#  BER.plot <- ggplot(data=toxcast.table) +
#    geom_segment(aes(x=Compound,
#                     xend=Compound,
#                     y=(10^Q10.AC50)/Css,
#                     yend=(10^Q90.AC50)/Css),
#                 size=1,
#                 color="red",
#                 alpha=0.5) +
#    geom_segment(aes(x=Compound,
#                     xend=Compound,
#                     y=seem3.l95,
#                     yend=seem3.u95),
#                 size=1,
#                 color="blue",
#                 alpha=0.5) +
#    geom_point(aes(x=Compound,y=(10^Med.AC50)/Css),shape=3,color="red") +
#    geom_point(aes(x=Compound,y=seem3),shape=3,color="blue") +
#    theme_bw() +
#    theme(axis.title.x = element_text(size=8)) +
#    theme(axis.title.y = element_text(size=8)) +
#    scale_y_log10() +
#    theme(axis.text.x = element_text(
#      face = "bold",
#      hjust = 1,
#      vjust = 1,
#      size = 4,
#      angle = 45)) +
#    ylab("Bioactive Dose & Exposure\nmg/kg BW/day") +
#    xlab("Chemicals Ranked By BER")
#  print(BER.plot)

