% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_input_params.R
\name{gen_input_params}
\alias{gen_input_params}
\title{Generate and store population values of time-dependent input variables}
\usage{
gen_input_params(
  model,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  samples = 10000,
  httkpop.dt = NULL,
  httkpop.generate.arg.list = list(nsamp = 10000, method = "direct resampling"),
  seed = NULL,
  input.param.dir = NULL
)
}
\arguments{
\item{model}{A model which incorporates time-dependent parameters as specified
in the \code{model.list[[model]]$input.var.names}.}

\item{chem.cas}{Chemical Abstract Services Registry Number (CAS-RN) -- the 
chemical must be identified by either CAS, name, or DTXISD}

\item{chem.name}{Chemical name (spaces and capitalization ignored) --  the 
chemical must be identified by either CAS, name, or DTXISD}

\item{dtxsid}{EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})
-- the chemical must be identified by either CAS, name, or DTXSIDs}

\item{samples}{Size of the stored population; default 10000. Overridden by
the size of \code{httkpop.dt}, if specified, or by \code{nsamp} in 
\code{httkpop.generate.arg.list}, if specified.}

\item{httkpop.dt}{A data table generated by \code{\link{httkpop_generate}}.
This defaults to NULL, in which case \code{\link{httkpop_generate}} is 
called to generate this table.}

\item{httkpop.generate.arg.list}{Arguments passed to \code{\link{httkpop_generate}}
for generating a population if \code{httkpop.dt} is not specified. Defaults 
to \code{list(nsamp = 10000, method = "direct resampling")}.}

\item{seed}{Sets a seed for repeatable generation of populations. Defaults to
null, in which case no seed is set.}

\item{input.param.dir}{The path to the \code{input_params_data_files} directory,
which is used to store all \code{input_param} data files. If \code{input_params_data_files}
does not exist, this function will create it in the specified path. Default \code{NULL}, 
in which case the present working directory is used as default.}
}
\description{
Generates and stores a population using \code{\link{create_mc_samples}} for 
reference by \code{\link{get_input_param_timeseries}}. Specifically, this 
function generates parameters for a sample population and stores those
parameters which are listed in the specified model's \code{input.var.names}.
}
\details{
This function has no output; it writes (or re-writes) an Rds file in 
\code{httk/data} of the form \code{input_params_<model>_<CAS>.Rds} where <model>
and <CAS> are the model name and chemical CAS, respectively.
}
\seealso{
\code{\link{get_input_param_timeseries}}
}
\author{
Colin Thomson
}
\keyword{lifestage}
