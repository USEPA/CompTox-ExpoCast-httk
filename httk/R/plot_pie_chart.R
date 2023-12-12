#' Create a pie chart for use in visualizing in vitro distribution
#'
#' This function accepts a list of compartments and fractions and generates a pie chart. The chart is designed for use with in vitro distribution models with the goal of visualizing the distribution of a chemical.
#'
#'
#' @param input_table  Object of class data.frame with only the following columns:
#' \tabular{ccc}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' compartment \tab Compartment or location of chemical \cr
#' fraction \tab Fraction of chemical in the given compartment \cr
#'
#' @return A pie chart created in ggplot.
#'
#' @author Meredith Scherer
#'
#' @export
#'
#' @examples
#' #import ggplot2 (required)
#' library("ggplot2")
#' #create an input table with two columns: compartment and fraction
#' input_table <- data.frame(compartment = c("Free in medium", "Bound in plasma", "Associated with cells", "In headspace", "Soaked to well plastic"), fraction=c(0.1, 0.240597245, 0.600, 0.048, 0.01140276))
#' #call the piechart function
#' piechart(input_table)

piechart <- function(input_table){

  #round the inputs to four digits
  input_table$fraction <- round(input_table$fraction, digits = 4)

  #convert fractions to percentages
  input_table$label <- scales::percent(input_table$fraction)

  #plot the rounded fractions as a pie chart, color by compartment
  ggplot(input_table, aes(x="", y=fraction, fill=compartment))+
    geom_bar(stat="identity", width = 1)+
    coord_polar("y", start = 0, direction = -1) +
    theme_void()+
    geom_text(aes(label = label))
}
