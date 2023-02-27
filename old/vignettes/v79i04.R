library("httk")

## To check if the version installed is 1.7:
packageVersion("httk")  >= "1.7"

## To get a list of parameters for the pbtk model of triclosan in a rat:
parameters <- parameterize_pbtk(chem.name = "triclosan", species = "rat")

## To change the fub in the previous parameters list to 0.1 from the
##  default of 0.005 and use it in a simulation of the pbtk model for
##  a single dose of 1 mg/kg BW of triclosan in a rat:
parameters["Funbound.plasma"] <- 0.1
out <- solve_pbtk(parameters = parameters)

## Table making example: In the example below, setting model to "pbtk"
## in get cheminfo removes the chemicals from the list with fub below
## the limit of detection which have been set to zero because the
## model uses partition coefficients that are calculated with
## fub. However, we could include all chemicals that work with the
## models without partition coeffiients by using the default option of
## "3compartmentss" or setting exclude.fub.zero to false, and fub
## would then automatically be set to 0.005.
table <- NULL
for (this.cas in intersect(get_cheminfo(model = "pbtk"), get_wetmore_cheminfo())) {
    this.row <- as.data.frame(this.cas)
    this.row <- cbind(this.row,
                      as.data.frame(calc_analytic_css(chem.cas = this.cas,
                                                      model = "pbtk",
                                                      output.units = "mg/L")))
    this.row <- cbind(this.row,
                      as.data.frame(get_wetmore_css(chem.cas = this.cas,
                                                    which.quantile = 0.50)))
    this.row <- cbind(this.row,
                      as.data.frame(calc_mc_css(chem.cas = this.cas,
                                                which.quantile = 0.50)))
    table <- rbind(table, this.row)
}
colnames(table) <- c("CAS", "PBTK", "Wetmore", "MC")
## Plotting examples use the package ggplot2.
library("ggplot2")

## To see how Css resulting from discrete dosing deviates from the
## average steady state concentration, we can make a plot with ggplot2
## that includes a horizontal line through the y axis at the predicted
## Css for oral infusion dosing (Figure 2).
out <- solve_pbtk(chem.name = "Bisphenol A", days = 50, doses.per.day = 3)
plot.data <- as.data.frame(out)
css <- calc_analytic_css(chem.name = "Bisphenol A")
c.vs.t <- ggplot(plot.data, aes(time, Cplasma)) +
    geom_line() + geom_hline(yintercept = css) +
    ylab("Plasma Concentration (uM)") + xlab("Day") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 17, hjust=0.5)) +
    ggtitle("Bisphenol A")
c.vs.t


## To calculate the peak statistics for all chemicals simulated for 10
## days at 1 mg/kg BW/day with 3 doses per day and a list containing
## the AUC, peak, and mean for a single 1 mg dose of triclosan over 10
## days, we have:
all.peak.stats <- calc_stats(days = 10, doses.per.day = 3, stats = "peak")
triclosan.stats <- calc_stats(days = 10, chem.name = "triclosan")

## Below are examples of these two functions, comparing the medians of
## the Wetmore data in humans for 1 mg/kg BW/day of Bisphenol A with
## the calc_mc_css simulation with probability distributions
## containing a third of the standard deviation, half the limit of
## detection for fub, and double the number of samples of the
## parameters used in Wetmore et al. (2012):
get_wetmore_css(chem.cas = "80-05-7", daily.dose = 1,
                which.quantile = 0.5, output.units = "uM")
calc_mc_css(chem.cas = "80-05-7", daily.dose = 1, which.quantile = 0.5,
            censored.params = list(Funbound.plasma = list(cv = 0.1, lod = 0.005)),
            vary.params = list(BW = 0.15, Vliverc = 0.15, Qgfrc = 0.15,
                               Qtotal.liverc = 0.15, million.cells.per.gliver = 0.15,
                               Clint = 0.15),
            output.units = "uM", samples = 2000,httkpop=FALSE)
get_wetmore_oral_equiv(50, chem.cas = "80-05-7")

## To perform a Monte Carlo simulation on zoxamide (Figure 5) with the
## model pbtk with the same limit of detection and coefficients of
## variation of two thirds the size of those used in calc mc css, we
## have:
vary.params <- NULL
params <- parameterize_pbtk(chem.name = "Zoxamide")
for (this.param in names(subset(params, names(params) != "Funbound.plasma")))
    vary.params[this.param] <- 0.2
censored.params <- list(Funbound.plasma = list(cv = 0.2, lod = 0.01))
set.seed(1)
out <- monte_carlo(params, cv.params = vary.params,
                   censored.params = censored.params,
                   return.samples = TRUE, model = "pbtk",
                   suppress.messages = TRUE)
zoxamide <- ggplot(as.data.frame(out), aes(out)) +
    geom_histogram(fill = "blue", binwidth = 1/6) +
    scale_x_log10() + ylab("Number of Samples") +
    xlab("Steady State Concentration (uM)") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
zoxamide

## We can add thyroid to the tissue data by making a row containing
## its data, subtracting the volumes and flows from the rest, and
## binding the row to tissue.data.

new.tissue <- subset(tissue.data,Tissue == "spleen")
new.tissue[, "Tissue"] <- "thyroid"
new.tissue[new.tissue$variable %in% c('Vol (L/kg)','Flow (mL/min/kg^(3/4))'),'value'] <- 
   new.tissue[new.tissue$variable %in% c('Vol (L/kg)','Flow (mL/min/kg^(3/4))'),'value']/10
tissue.data[tissue.data$Tissue == "rest", 'value'] <-
    tissue.data[tissue.data$Tissue == "rest", 'value'] - new.tissue[new.tissue$variable %in% c('Vol (L/kg)','Flow (mL/min/kg^(3/4))'),'value']
tissue.data <- rbind(tissue.data, new.tissue)

## To generate the parameters for a model with kidneys, thyroid, a
## liver compartment combining the liver and gut, and a rest of body
## compartment:
compartments <- list(liver = c("liver", "gut"), kidney = "kidney",
                     thyroid = "thyroid")
parameterize_pbtk(chem.name = "Nicotine", tissuelist = compartments)

## Below is a call of an export function for a dose of 1 given to a
## rat.
export_pbtk_sbml(chem.name = "Bisphenol A", species = "Rat",
                 initial.amounts = list(Agutlumen = 1),
                 filename = "PBTKmodel.xml")
                                        
## Figures 3 and 4
library("httk")
library("ggplot2")
days <- NULL
avg <- NULL
max <- NULL
for (this.cas in get_cheminfo(model='pbtk')) {
    css.info <- calc_css(chem.cas = this.cas,
                         doses.per.day = 1,
                         suppress.messages = TRUE)
    days[[this.cas]] <- css.info[["the.day"]]
    avg[[this.cas]] <- css.info[["avg"]]
    max[[this.cas]] <- css.info[["max"]]
}
days.data <- as.data.frame(days)
hist <- ggplot(days.data, aes(days)) +
    geom_histogram(fill = "blue", binwidth = 1/4) +
    scale_x_log10() + ylab("Number of Chemicals") + xlab("Days") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
hist

avg.max.data <- data.frame(avg, max)
avg.vs.max <- ggplot(avg.max.data, aes(avg, max)) +
    geom_point() + geom_abline() +
    scale_x_log10() + scale_y_log10() +
    xlab("Average Concentration at Steady State (uM)") +
    ylab("Max Concentration at Steady State (uM)") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
avg.vs.max

