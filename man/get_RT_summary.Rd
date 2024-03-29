% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSet.R, R/DataSetList.R
\name{get_RT_summary}
\alias{get_RT_summary}
\alias{get_RT_summary.DataSet}
\alias{get_RT_summary.DataSetList}
\title{Get RunTime Summary}
\usage{
get_RT_summary(ds, ftarget, budget, ...)

\method{get_RT_summary}{DataSet}(ds, ftarget, budget = NULL, ...)

\method{get_RT_summary}{DataSetList}(ds, ftarget, budget = NULL, ...)
}
\arguments{
\item{ds}{A DataSet or DataSetList object}

\item{ftarget}{The function target(s) for which to get the runtime summary}

\item{budget}{Optional; overwrites the budget found in ds for ERT-calculation}

\item{...}{Arguments passed to other methods}
}
\value{
A data.table containing the runtime statistics for each provided target
function value
}
\description{
Get RunTime Summary
}
\examples{
get_RT_summary(dsl, 14)
get_RT_summary(dsl[[1]], 14)
}
