% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDataSetList.R
\name{Plot.RT.Single_Func}
\alias{Plot.RT.Single_Func}
\alias{Plot.RT.Single_Func.DataSetList}
\title{Plot lineplot of the ERTs of a DataSetList}
\usage{
Plot.RT.Single_Func(
  dsList,
  Fstart = NULL,
  Fstop = NULL,
  show.ERT = T,
  show.CI = F,
  show.mean = F,
  show.median = F,
  backend = NULL,
  scale.xlog = F,
  scale.ylog = F,
  scale.reverse = F,
  includeOpts = F,
  p = NULL
)

\method{Plot.RT.Single_Func}{DataSetList}(
  dsList,
  Fstart = NULL,
  Fstop = NULL,
  show.ERT = T,
  show.CI = T,
  show.mean = F,
  show.median = F,
  backend = NULL,
  scale.xlog = F,
  scale.ylog = F,
  scale.reverse = F,
  includeOpts = F,
  p = NULL
)
}
\arguments{
\item{dsList}{A DataSetList (should consist of only one function and dimension).}

\item{Fstart}{The starting function value.}

\item{Fstop}{The final function value.}

\item{show.ERT}{Whether or not to show the ERT-values}

\item{show.CI}{Whether or not to show the standard deviations}

\item{show.mean}{Whether or not to show the mean hitting times}

\item{show.median}{Whether or not to show the median hitting times}

\item{backend}{Which plotting library to use. Can be 'plotly' or 'ggplot2'}

\item{scale.xlog}{Whether or not to scale the x-axis logaritmically}

\item{scale.ylog}{Whether or not to scale the y-axis logaritmically}

\item{scale.reverse}{Wheter or not to reverse the x-axis (when using minimization)}

\item{includeOpts}{Whether or not to include all best points reached by each algorithm}

\item{p}{Existing plot to which to add the current data}
}
\value{
A plot of ERT-values of the DataSetList
}
\description{
Plot lineplot of the ERTs of a DataSetList
}
\examples{
Plot.RT.Single_Func(subset(dsl, funcId == 1))
}
