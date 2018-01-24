\name{export_pbtk_sbml}
\alias{export_pbtk_sbml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export model to sbml.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function exports the multiple compartment PBTK model to an sbml file.
}
\usage{
export_pbtk_sbml(chem.cas=NULL,chem.name=NULL,species="Human",
                 initial.amounts=list(Agutlumen=0),filename="default.xml",digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{chem.cas}{Either the chemical name or CAS number must be specified.}
\item{chem.name}{Either the chemical name or CAS number must be specified.}  
\item{species}{Species desired (either "Rat", "Rabbit", "Dog", or default "Human").}
\item{initial.amounts}{Must specify initial amounts in units of choice.}
\item{filename}{The name of the jarnac file containing the model.}
\item{digits}{Desired number of decimal places to round the parameters.}

}

\details{
Compartments to enter into the initial.amounts list includes Agutlumen, Aart, Aven, Alung, Agut, Aliver, Akidney, and Arest.

%%When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance.
%%  ~~ If necessary, more details than the description above ~~
}

\author{
Robert Pearce
}

\examples{
export_pbtk_sbml(chem.name='Nicotine',initial.amounts=list(Agutlumen=1),filename='PBTKmodel.xml')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Export}

