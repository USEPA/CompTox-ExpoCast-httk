## ----setup_vignette, eval = TRUE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=5, fig.height=4)

## ----install_httk, eval = FALSE-----------------------------------------------
#  install.packages("httk")

## ----write_privileges, eval = FALSE-------------------------------------------
#   Depending on the account you are using and where you want to install the package on that computer, you may need “permission” from your local file system to install the package. See this help here:
#  
#  <https://stackoverflow.com/questions/42807247/installing-package-cannot-open-file-permission-denied>
#  
#  and here:
#  
#  <https://support.microsoft.com/en-us/topic/general-problem-installing-any-r-package-0bf1f9ba-ead2-6335-46ec-190f6af75e44>

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls())

## ----load_httk, eval = TRUE---------------------------------------------------
library(httk)

# Check what version you are using 
packageVersion("httk")

## ----MC_samples, eval = TRUE--------------------------------------------------
NSAMP <- 25

## ----suppress_messages, eval = TRUE-------------------------------------------
css <- calc_analytic_css(chem.name = "Bisphenol A",
                  suppress.messages = FALSE)

css <- calc_analytic_css(chem.name = "Bisphenol A",
                  suppress.messages = TRUE)

## ----cheminfo_ex1, eval = TRUE------------------------------------------------
head(get_cheminfo(suppress.messages = TRUE))

## ----cheminfo_ex1a, eval = FALSE----------------------------------------------
#  get_cheminfo(suppress.messages = TRUE)

## ----cheminfo_ex2, eval = TRUE------------------------------------------------
head(get_cheminfo(info = "all", median.only=TRUE,suppress.messages = TRUE))

## ----cheminfo_ex3, eval = TRUE------------------------------------------------
"80-05-7" %in% get_cheminfo(suppress.messages = TRUE)

## ----cheminfo_ex4, eval = TRUE------------------------------------------------
subset(get_cheminfo(info = "all",suppress.messages = TRUE), 
       Compound %in% c("A","B","C"))

## ----oral_equiv_ex, eval = TRUE-----------------------------------------------
calc_mc_oral_equiv(0.1,
                   chem.cas = "34256-82-1",
                   species = "human",
                   samples = NSAMP,
                   suppress.messages = TRUE)
calc_mc_oral_equiv(0.1,
                   chem.cas = "99-71-8", 
                   species = "human",
                   samples = NSAMP,
                   suppress.messages = TRUE)

## ----tkstats_ex, eval = TRUE--------------------------------------------------
calc_tkstats(chem.cas = "34256-82-1",
             species = "rat",
             suppress.messages = TRUE)
calc_tkstats(chem.cas = "962-58-3", 
             species = "rat",
             suppress.messages = TRUE)

## ----pbtk_ex, eval = TRUE-----------------------------------------------------
head(solve_pbtk(chem.name = "bisphenol a", 
                plots = TRUE,
                days = 1,
                suppress.messages = TRUE))

## ----subset_ex, eval = TRUE---------------------------------------------------
my_data <- subset(get_cheminfo(info = "all",suppress.messages = TRUE), 
                  Compound %in% c("A","B","C"))

## ----writetodisk_ex, eval = TRUE----------------------------------------------
write.csv(my_data, file = "my_data.csv")

## ----fup_stats1, eval = TRUE--------------------------------------------------
library(httk)
fup.tab <- get_cheminfo(info="all",
                        median.only=TRUE,
                        model="schmitt",
                        suppress.messages = TRUE)

## ----fup_stats2, eval = TRUE--------------------------------------------------
median(as.numeric(fup.tab$Human.Funbound.plasma),na.rm=TRUE)

## ----fup_stats3, eval = TRUE--------------------------------------------------
mean(as.numeric(fup.tab$Human.Funbound.plasma),na.rm=TRUE)

## ----fup_stats4, eval = TRUE--------------------------------------------------
sum(!is.na(fup.tab$Human.Funbound.plasma))

## ----help1, eval = FALSE------------------------------------------------------
#  help(httk)

## ----help2, eval = FALSE------------------------------------------------------
#  help(package = httk)

## ----help3, eval = FALSE------------------------------------------------------
#  vignette(package = "httk")

## ----help4, eval = FALSE------------------------------------------------------
#  vignette("1_IntroToHTTK")

