% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc_ci.R
\name{roc_ci}
\alias{roc_ci}
\title{Add Confidence Interval}
\usage{
roc_ci(data, mapping = aes(), color = "black", fill = NA,
  alpha = 0.5, ...)
}
\arguments{
\item{data}{results of rocit()}

\item{mapping}{mapping}

\item{color}{color for line}

\item{fill}{fill for polygon}

\item{alpha}{alpha, defaulted is 0.5}

\item{...}{passed to params}
}
\description{
Add Confidence Interval
}
\details{
see \code{\link[ROCit]{ciAUC}}
}
