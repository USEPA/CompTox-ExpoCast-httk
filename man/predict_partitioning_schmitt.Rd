\name{predict_partitioning_schmitt}
\alias{predict_partitioning_schmitt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict partition coefficients using the method from Schmitt (2008).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function implements the method from Schmitt (2008) in predicting the tissue to unbound plasma partition 
coefficients from for the tissues contained in the tissue.data table.
}
\usage{
predict_partitioning_schmitt(chem.name=NULL,chem.cas=NULL,species="Human",
                             default.to.human=F,parameters=NULL,regression=T,
                             regression.list=c('brain','adipose','gut','heart','kidney',
                                        'liver','lung','muscle','skin','spleen','bone'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
\item{chem.name}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{species}{
    Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).
  }
  \item{parameters}{Chemical parameters from the parameterize_schmitt function, overrides chem.name and chem.cas.}
  \item{regression}{Whether or not to use the regressions.  Regressions are used by default.}
  \item{regression.list}{Tissues to use regressions on.}
}

\value{
Returns tissue to unbound plasma partition coefficients for each tissue.

}
\details{
A regression is used for membrane affinity when not provided.  The regressions for correcting each tissue are performed on 
tissue plasma partition coefficients (Ktissue2pu * Funbound.plasma) calculated with the corrected Funbound.plasma value and divided by this value to get Ktissue2pu.
Thus the regressions should be used with the corrected Funbound.plasma.

The red blood cell regression can be used but is not by default because of the span of the data used, reducing confidence in the regression for higher and lower predicted values.

Human tissue volumes are used for species other than Rat.

}

\author{
Robert Pearce
}

\examples{
predict_partitioning_schmitt(chem.name='ibuprofen',regression=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}

