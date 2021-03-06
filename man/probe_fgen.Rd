% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probing.R
\name{probe_fgen}
\alias{probe_fgen}
\title{Probe Force Generator}
\usage{
probe_fgen(
  current_time = 0,
  current_distance = 0,
  current_velocity = 0,
  change_ratio = seq(0.8, 1.2, length.out = 100),
  aggregate = "raw",
  ...
)
}
\arguments{
\item{current_time}{Numeric value. Initial system state whose change is probed}

\item{current_distance}{Numeric value. Initial system state whose change is probed}

\item{current_velocity}{Numeric value. Initial system state whose change is probed}

\item{change_ratio}{Numeric vector indicating probing change ratios}

\item{aggregate}{How should \code{\link{fgen_get_output}} output be aggregated?
Default is "raw". Other options involve "ratio" and "diff" which use initial
output values}

\item{...}{Extra argument forwarded to \code{\link{fgen_get_output}}}
}
\value{
Probing data frame
}
\description{
Probe Force Generator
}
\examples{
require(tidyverse)

fgen_probe_data <- probe_fgen(
  current_time = 0.3,
  current_distance = 0.3,
  current_velocity = 1
)

plot_data <- gather(fgen_probe_data, key = "variable", value = "value", -(1:2))

ggplot(plot_data, aes(x = change_ratio, y = value, color = probing)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  xlab("Normalized parameter change") +
  ylab(NULL)
}
