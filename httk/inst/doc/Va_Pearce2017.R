## ----configure_knitr, eval = TRUE---------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')
options(rmarkdown.html_vignette.check_title = FALSE)

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls()) 

## ----runchunks, eval = TRUE---------------------------------------------------
# Set whether or not the following chunks will be executed (run):
execute.vignette <- TRUE

## ----load_packages, eval = execute.vignette-----------------------------------
library(ggplot2)
library(httk)

## ----version_check, eval = execute.vignette-----------------------------------
packageVersion("httk")

## ----chemical_thining, eval = execute.vignette--------------------------------
NUM.CHEMS <- length(get_cheminfo(model="pbtk", suppress.messages = TRUE))
SKIP.CHEMS <- 50

## ----MC_samples, eval = execute.vignette--------------------------------------
NSAMP <- 10

## ----rat_pbtk_parameters, eval = execute.vignette-----------------------------
parameters <- try(parameterize_pbtk(chem.name = "triclosan", 
                                    species = "rat"))

## ----rat_pbtk_parameters2, eval = execute.vignette----------------------------
parameters <- parameterize_pbtk(chem.name = "triclosan", 
                                species = "rat", 
                                default.to.human = TRUE)

## ----use)pbtk_parameters, eval = execute.vignette-----------------------------
parameters["Funbound.plasma"] <- 0.001
out <- solve_pbtk(parameters = parameters, suppress.messages = TRUE)

## ----select_Chemicals, eval = execute.vignette--------------------------------
chem.list <- intersect(get_cheminfo(model = "pbtk", 
                                    suppress.messages = TRUE)[seq(
                                      1, NUM.CHEMS, SKIP.CHEMS)],
                       get_wetmore_cheminfo())

## ----make_Css_table, eval = execute.vignette----------------------------------
css.table <- NULL
for (this.cas in chem.list)
{
    ids <- get_chem_id(chem.cas=this.cas)
    this.row <- data.frame(Compound=ids$chem.name,
                              DTXSID=ids$dtxsid,
                              CAS=this.cas)
    this.row <- cbind(this.row,
                      as.data.frame(calc_analytic_css(chem.cas = this.cas,
                                                      model = "pbtk",
                                                      output.units = "uM",
                                                      suppress.messages=TRUE)))
    this.row <- cbind(this.row,
                      as.data.frame(get_wetmore_css(chem.cas = this.cas,
                                                    which.quantile = 0.50,
                                                    output.units = "uM",
                                                    suppress.messages=TRUE)))
    this.row <- cbind(this.row,
                      as.data.frame(calc_mc_css(chem.cas = this.cas,
                                                which.quantile = 0.50,
                                                output.units = "uM",
                                                samples = NSAMP,
                                                suppress.messages=TRUE)))
    css.table <- rbind(css.table, this.row)
}
colnames(css.table) <- c("Compound","DTXSID","CAS", "PBTK", "Wetmore", "MC")

## ----display_Css_table, eval = execute.vignette-------------------------------
knitr::kable(css.table, caption = "Literature and HTTK Plasma Css", 
             floating.environment="sidewaystable",
             row.names=FALSE,
             digits=3)
litvshttkcss <- ggplot(css.table, aes(Wetmore, MC)) +
    geom_point() + geom_abline() +
    scale_x_log10() + scale_y_log10() +
    xlab(bquote("Literature"~C[ss]~"("*mu*"M)")) +
    ylab(bquote("HTTK"~C[ss]~"("*mu*"M)")) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
print(litvshttkcss)

## ----plot_examples, eval = execute.vignette-----------------------------------
out <- solve_pbtk(chem.name = "Bisphenol A", 
                  days = 50, 
                  doses.per.day = 3,
                  daily.dose=1,
                  output.units = "uM")
plot.data <- as.data.frame(out)

css <- calc_analytic_css(chem.name = "Bisphenol A",
                  output.units = "uM")

c.vs.t <- ggplot(plot.data, aes(time, Cplasma)) +
    geom_line() + geom_hline(yintercept = css) +
    ylab(bquote("Plasma Concentration ("*mu*"M)")) +
    xlab("Day") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 17, hjust=0.5)) +
    ggtitle("Bisphenol A")
print(c.vs.t)

## ----suppress_messages, eval = execute.vignette-------------------------------
out <- solve_pbtk(chem.name = "Bisphenol A", 
                  days = 50, 
                  doses.per.day = 3,
                  daily.dose=1,
                  output.units = "uM",
                  suppress.messages = TRUE)

css <- calc_analytic_css(chem.name = "Bisphenol A",
                  output.units = "uM",
                  suppress.messages = TRUE)

## ----stats_example1, eval = FALSE---------------------------------------------
#  all.peak.stats <- calc_stats(days = 10, doses.per.day = 3, stats = "peak")

## ----stats_example2, eval = execute.vignette----------------------------------
triclosan.stats <- calc_stats(days = 10, chem.name = "triclosan")

## ----css_examples, eval = execute.vignette------------------------------------
get_wetmore_css(chem.cas = "80-05-7", daily.dose = 1,
                which.quantile = 0.5, output.units = "uM")

set.seed(654321)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            censored.params = list(
              Funbound.plasma = list(cv = 0.1, lod = 0.005)),
            vary.params = list(
              BW = 0.15, 
              Vliverc = 0.15, 
              Qgfrc = 0.15,
              Qtotal.liverc = 0.15, 
              million.cells.per.gliver = 0.15,
              Clint = 0.15),
            output.units = "uM", 
            samples = NSAMP,
            httkpop=FALSE,
            invitrouv=FALSE,
            suppress.messages = TRUE)

## ----css_examples2, eval = execute.vignette-----------------------------------
set.seed(654321)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

## ----set_seed_example, eval = execute.vignette--------------------------------
# Random number generator seed 1:
set.seed(654321)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

# Continuing to draw random numbers without resetting the seed:
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

# Random number generator seed 2:
set.seed(123456)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

# Continuing to draw random numbers without resetting the seed:
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

# Random number generator seed 2 gives the same answer as it did above:
set.seed(123456)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

# Random number generator seed 1 gives the same answer as it did above:
set.seed(654321)
calc_mc_css(chem.cas = "80-05-7", 
            daily.dose = 1, 
            which.quantile = 0.5,
            output.units = "uM", 
            samples = NSAMP,
            suppress.messages = TRUE)

## ----literature_oral_equivalent_example, eval = execute.vignette--------------
get_wetmore_oral_equiv(50, chem.cas = "80-05-7")
set.seed(654321)
calc_mc_oral_equiv(50, chem.cas = "80-05-7", samples = NSAMP)

## ----monte_carlo_css_example_1, eval = execute.vignette-----------------------
vary.params <- NULL
params <- parameterize_pbtk(chem.name = "Zoxamide")
for (this.param in names(subset(params, names(params) != "Funbound.plasma")))
  # Only want to vary numerical parameters:
  if (is.numeric(params[[this.param]])) 
      vary.params[this.param] <- 0.2

## ----monte_carlo_css_example_2, eval = execute.vignette-----------------------
censored.params <- list(Funbound.plasma = list(cv = 0.2, lod = 0.01))

## ----monte_carlo_css_example_3, eval = execute.vignette-----------------------
set.seed(1)
out <- calc_mc_tk(parameters=params, 
                  vary.params = vary.params,
                  censored.params = censored.params,
                  return.samples = TRUE, 
                  model = "pbtk",
                  suppress.messages = TRUE,
                  httkpop = FALSE,
                  invitrouv = FALSE,
                  samples = NSAMP,
                  solvemodel.arg.list = list(times = seq(0,0.5,0.025))
                  )

## ----monte_carlo_css_example_4, eval = execute.vignette-----------------------
zoxtable <- cbind(out[["means"]][,"time"],
                  out[["means"]][,"Cplasma"],
                  out[["means"]][,"Cplasma"] - 1.96*out[["sds"]][,"Cplasma"],
                  out[["means"]][,"Cplasma"] + 1.96*out[["sds"]][,"Cplasma"])
colnames(zoxtable) <- c("time","Cplasma","lcl","ucl")
knitr::kable(zoxtable, caption = "Zoxamide plasma concentration vs. time",
             floating.environment="sidewaystable")

## ----monte_carlo_css_example_5, eval = execute.vignette-----------------------
zoxamide1 <- ggplot(as.data.frame(zoxtable), aes(x=time,y=Cplasma)) +
    geom_line(color = "dark blue",linewidth=2) +
      geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha = 0.3,
                  fill = "light blue", color="black", linetype="dotted")+
    ylab("Plasma concentration") +
    xlab("Time (days)") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
print(zoxamide1)

## ----monte_carlo_css_example_6, eval = execute.vignette-----------------------
out <- calc_mc_css(parameters=params, 
                  vary.params = vary.params,
                  censored.params = censored.params,
                  return.samples = TRUE, 
                  model = "pbtk",
                  suppress.messages = TRUE,
                  httkpop = FALSE,
                  samples = NSAMP,
                  invitrouv = FALSE)

zoxamide2 <- ggplot(as.data.frame(out), aes(out)) +
    geom_histogram(fill = "blue", binwidth = 1/6) +
    scale_x_log10() + ylab("Number of Samples") +
    xlab(bquote(C[ss]~"("*mu*"M)")) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
print(zoxamide2)

## ----add_a_tissue, eval = execute.vignette------------------------------------
new.tissue <- subset(tissue.data,Tissue == "adipose" & Species =="Human")
new.tissue[, "Tissue"] <- "mammary"
new.tissue[new.tissue$variable=="Vol (L/kg)","value"] <- 
  320/1000/60 # Itsukage et al. (2017) PMID: 29308107
new.tissue[new.tissue$variable %in% c('Flow (mL/min/kg^(3/4))'),'value'] <-
# We'll arbitrarily set flow to a tenth of total adipose:
  new.tissue[new.tissue$variable %in% c('Flow (mL/min/kg^(3/4))'),'value']/10
tissue.data <- rbind(tissue.data, new.tissue)
knitr::kable(subset(tissue.data,Tissue=="mammary"), 
             caption = "Approximate Mamary Tissue Parameters", 
             floating.environment="sidewaystable",
             row.names=FALSE,
             digits=3)

## ----add_a_tissue2, eval = FALSE----------------------------------------------
#  # If we thought this tissue was included in the rest of the bod
#  tissue.data[tissue.data$Tissue == "rest", 'value'] <-
#    tissue.data[tissue.data$Tissue == "rest", 'value'] -
#    new.tissue[new.tissue$variable %in% c(
#      'Vol (L/kg)',
#      'Flow (mL/min/kg^(3/4))'),
#      'value']

## ----add_a_tissue3, eval = execute.vignette-----------------------------------
compartments <- list(liver = c("liver", "gut"), kidney = "kidney",
                     breast = "mammary", brain = "brain",
                     thyroid = "thyroid")
tissues.to.include <- unique(tissue.data$Tissue)
tissues.to.include <- tissues.to.include[!(tissues.to.include=="placenta")]
Kp <- predict_partitioning_schmitt(
  chem.name = "Nicotine", 
  tissues = tissues.to.include,
  suppress.messages = TRUE)
lumped.params <- lump_tissues(Kp, tissuelist=compartments)
knitr::kable(as.data.frame(lumped.params), 
             caption = "PCs, Volumes, and Flows for Lumped Tissues", 
             floating.environment="sidewaystable",
             row.names=FALSE,
             digits=3)

## ----add_a_species1, eval = execute.vignette----------------------------------
new.species <- physiology.data[,"Rabbit"]
names(new.species) <- physiology.data[,"Parameter"]
rabbit.BW <- new.species["Average BW"] 
new.species["Average BW"] <- 31.2 # Rausch and Pearson (1972) https://doi.org/10.2307/3799057
new.species["Average Body Temperature"] <- 38.5 # Thiel et al. (2019) https://doi.org/10.1186/s12983-019-0319-8

physiology.data <- cbind(physiology.data, new.species)
colnames(physiology.data)[length(colnames(physiology.data))] <- "Wolverine"

knitr::kable(physiology.data[,c("Parameter","Units","Wolverine")], 
             caption = "Approximate Wolverine Physiological Parameters", 
             floating.environment="sidewaystable",
             row.names=FALSE,
             digits=3)

## ----add_a_species2, eval = execute.vignette----------------------------------
new.tissue.data <- subset(tissue.data,Species=="Rabbit")
new.tissue.data$Species <- "Wolverine"

tissue.data <- rbind(tissue.data,new.tissue.data)

new.physiology.data <- physiology.data[,"Rabbit"]
physiology.data <- rbind(physiology.data, new.physiology.data)
colnames(physiology.data)[length(colnames(physiology.data))] <- "Wolverine"

## ----add_a_species3, eval = execute.vignette----------------------------------
calc_mc_css(chem.cas="80-05-7",species="wolverine",
            parameterize.arg.list=list(default.to.human=TRUE),
            suppress.messages=TRUE,
            samples = NSAMP)

## ----export, eval = FALSE-----------------------------------------------------
#  export_pbtk_sbml(chem.name = "Bisphenol A", species = "Rat",
#                   initial.amounts = list(Agutlumen = 1),
#                   filename = "PBTKmodel.xml")

## ----figure3, eval = execute.vignette-----------------------------------------
css.data <- data.frame()
chem.list <- get_cheminfo(model='pbtk', suppress.messages = TRUE)[seq(
  1, NUM.CHEMS, SKIP.CHEMS)]
for (this.cas in chem.list) {
    css.info <- calc_css(chem.cas = this.cas,
                         doses.per.day = 1,
                         model="pbtk",
                         suppress.messages = TRUE)
    css.data[this.cas,"days"] <- css.info[["the.day"]]
    css.data[this.cas,"avg"] <- css.info[["avg"]]
    css.data[this.cas,"max"] <- css.info[["max"]]
}

hist <- ggplot(css.data, aes(days+0.1)) +
    geom_histogram(fill = "blue", bins=5) +
    scale_x_log10() + ylab("Number of Chemicals") + xlab("Days") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
print(hist)

## ----figure4, eval = execute.vignette-----------------------------------------
avg.vs.max <- ggplot(css.data, aes(avg, max)) +
    geom_point() + geom_abline() +
    scale_x_log10() + scale_y_log10() +
    xlab(bquote("Avg. Conc. at Steady State ("*mu*"M)")) +
    ylab(bquote("Max. Conc. at Steady State ("*mu*"M)")) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
print(avg.vs.max)

