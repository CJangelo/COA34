% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_ecdf.R
\name{compute_ecdf}
\alias{compute_ecdf}
\title{Compute eCDF}
\usage{
compute_ecdf(
  dat = NULL,
  anchor.group = NULL,
  time.var = NULL,
  timepoint = NULL,
  change.score = NULL
)
}
\arguments{
\item{dat}{pass the dataframe, must contain the anchor group and the change score}

\item{anchor.group}{indicate the name of the anchor group}

\item{time.var}{variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time}

\item{timepoint}{default here is to just use the final timepoint, e.g., "Time_4". Please be sure to have
ordered your time variable correctly.}

\item{change.score}{indicate the name of the PRO change score}
}
\value{
character vector
}
\description{
Compute cumulative density of change score for eCDF plot. This is the true
empirical density, using the stats::ecdf function: https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/ecdf.R
which uses the stats::approxfun function
}
