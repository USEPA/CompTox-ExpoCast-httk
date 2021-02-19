load("C:\\Users\\jwambaug\\git\\httk-dev\\work\\v202.RData")
human.httk.data.complete.202 <- human.httk.data.complete
load("C:\\Users\\jwambaug\\git\\httk-dev\\work\\v203.RData")
both <- merge(human.httk.data.complete.202,human.httk.data.complete,by=c("CAS","Compound","DTXSID"))

# .x variables are v 2.0.2, .y variables are v2.0.3
colnames(both)

# Check for differences:
summary(lm(Human.Clint.Bin3.x~Human.Clint.Bin3.y,data=both))

summary(lm(Human.Clint.QSAR.Bin3.x~Human.Clint.QSAR.Bin3.y,data=both))

summary(lm(CSS.InVitro.x~CSS.InVitro.y,data=both)

# turns out the OPERA log P values have changed:
plot(both$logP.x,both$logP.y)
hist(log10(both$logP.x/both$logP.y))
# by a 100-fold for Propamocarb hydrochloride (sketchy!)
subset(both,abs(log10(logP.x/logP.y))>1)


# compare in vitro values for two versions:
summary(lm(log10(CSS.InVitro.x)~log10(CSS.InVitro_CL3BinsMD.x),data=both))

# Compare Css for in vitro vs. binned in vitro:
summary(lm(log10(CSS.InVitro.x)~log10(CSS.InVitro_CL3BinsMD.x),data=both))
summary(lm(log10(CSS.InVitro.y)~log10(CSS.InVitro_CL3BinsMD.y),data=both))
summary(lm(log10(CSS.InVitro.x)~log10(CSS.InVitro_CL3BinsMD.x),
  data=subset(both,abs(log10(logP.x/logP.y))<0.5)))
summary(lm(log10(CSS.InVitro.y)~log10(CSS.InVitro_CL3BinsMD.y),
  data=subset(both,abs(log10(logP.x/logP.y))<0.5)))
  

# Compare Css for in vitro vs. QSAR:
summary(lm(log10(CSS.InVitro.x)~log10(CSS.QSAR_CL3BinsMD.x),data=both))
summary(lm(log10(CSS.InVitro.y)~log10(CSS.QSAR_CL3BinsMD.y),data=both))
