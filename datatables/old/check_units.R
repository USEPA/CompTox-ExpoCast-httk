library(httk)
library(readxl)

rm(list=ls())

ref.factors <- as.data.frame(read_excel("convert_units_test.xlsx"))
for (this.row in 1:dim(ref.factors)[1])
{
  ref.factors[this.row,"httk.factor"] <- 
    convert_units(input.units=ref.factors[this.row,"Initial.Unit"],
                 output.units=ref.factors[this.row,"Final.Unit"],
                 MW=ref.factors[this.row,"MW"],
                 temp=ifelse(is.na(as.numeric(ref.factors[this.row,"Temperature"])),
                             25,
                             as.numeric(ref.factors[this.row,"Temperature"])),
                 state=ref.factors[this.row,"State"])
}

signif(ref.factors$Factor/ref.factors$httk.factor,2)

print(ref.factors[signif(ref.factors$Factor/ref.factors$httk.factor,2)!=1.0,])
