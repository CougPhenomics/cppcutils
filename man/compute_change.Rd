% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype_change.R
\name{compute_change}
\alias{compute_change}
\title{Compute a relative phenotype change}
\usage{
compute_change(
  dF,
  statcol_central,
  statcol_spread,
  refcol,
  refvalue,
  chgmetric = NULL
)
}
\arguments{
\item{dF}{dataframe for which to compute the change}

\item{chgmetric}{string to quantify change. current option is log(x,avg,base=2)}
}
\value{
dataframe
}
\description{
Compute a relative phenotype change
}
\details{
this function is called after \code{summarise_phentypes()} for a single phenotype
}
