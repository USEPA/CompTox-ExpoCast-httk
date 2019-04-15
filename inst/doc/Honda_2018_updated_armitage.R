## ----setup, include=FALSE, EVAL=F----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=F-------------------------------------------------------------
#  library(data.table)
#  library(magrittr)
#  library(ggplot2)
#  library(httk)
#  armitage.dt <- copy(armitage_input)
#  armitage.dt[,well_number:=384] %>%
#    .[,option.bottom:=TRUE] %>%
#    .[,option.plastic:=TRUE] %>%
#    .[,Tsys:=37] %>%
#    .[,Tref:=298.15] %>%
#    .[,FBSf:=0.1] %>%
#    .[,ac50:=50]
#  
#  armitage.dt2 <- armitage_estimate_sarea(armitage.dt)
#  armitage_output1 <- armitage_eval(armitage.dt2)
#  armitage_output2 <- armitage_eval(armitage.dt2[,ac50:=1])
#  armitage_output3 <- armitage_eval(armitage.dt2[,ac50:=0.001])
#  
#  
#  

## ---- echo=TRUE, eval=F--------------------------------------------------
#  armitage_output <- rbind(armitage_output1[,xfill:="50 \U00B5M"],
#                           armitage_output2[,xfill:="1 \U00B5M"],
#                           armitage_output3[,xfill:="1 nM"])
#  armitage_output[,xfill:=factor(xfill,levels=c("50 \U00B5M","1 \U00B5M","1 nM"))]
#  
#  ggplot(armitage_output) +
#    geom_point(aes(x=gkow,y=log10(ccells/MW*1e9),color=xfill),shape=1) +
#    labs(color=expression("AC"["50"]),x="log P", y = "log(ng chemical/g cells)") +
#    theme_bw()

